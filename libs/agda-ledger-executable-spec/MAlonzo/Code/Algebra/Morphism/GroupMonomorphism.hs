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

module MAlonzo.Code.Algebra.Morphism.GroupMonomorphism where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Morphism.Structures
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Morphism.GroupMonomorphism._._⁻¹
d__'8315''185'_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny
d__'8315''185'_50 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7
  = du__'8315''185'_50 v4
du__'8315''185'_50 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  AgdaAny -> AgdaAny
du__'8315''185'_50 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 (coe v0)
-- Algebra.Morphism.GroupMonomorphism._._∙_
d__'8729'__52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__52 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du__'8729'__52 v4
du__'8729'__52 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8729'__52 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 (coe v0)
-- Algebra.Morphism.GroupMonomorphism._._≈_
d__'8776'__54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__54 = erased
-- Algebra.Morphism.GroupMonomorphism._.ε
d_ε_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  AgdaAny
d_ε_64 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du_ε_64 v4
du_ε_64 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 -> AgdaAny
du_ε_64 v0 = coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)
-- Algebra.Morphism.GroupMonomorphism._._⁻¹
d__'8315''185'_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny
d__'8315''185'_68 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7
  = du__'8315''185'_68 v5
du__'8315''185'_68 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  AgdaAny -> AgdaAny
du__'8315''185'_68 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 (coe v0)
-- Algebra.Morphism.GroupMonomorphism._._≈_
d__'8776'__70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__70 = erased
-- Algebra.Morphism.GroupMonomorphism._._∙_
d__'8729'__74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__74 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du__'8729'__74 v5
du__'8729'__74 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8729'__74 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 (coe v0)
-- Algebra.Morphism.GroupMonomorphism._.ε
d_ε_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  AgdaAny
d_ε_82 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du_ε_82 v5
du_ε_82 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 -> AgdaAny
du_ε_82 v0 = coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)
-- Algebra.Morphism.GroupMonomorphism._.assoc
d_assoc_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_86 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_assoc_86 v4 v5 v6 v7
du_assoc_86 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_86 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_assoc_140
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.cancel
d_cancel_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cancel_88 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_cancel_88 v4 v5 v6 v7
du_cancel_88 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_cancel_88 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel_216
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.cancelʳ
d_cancel'691'_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'691'_90 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'691'_90 v4 v5 v6 v7
du_cancel'691'_90 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'691'_90 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'691'_204
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.cancelˡ
d_cancel'737'_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'737'_92 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'737'_92 v4 v5 v6 v7
du_cancel'737'_92 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'737'_92 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'737'_192
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.comm
d_comm_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_94 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_comm_94 v4 v5 v6 v7
du_comm_94 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_94 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.cong
d_cong_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_96 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_cong_96 v4 v5 v6 v7
du_cong_96 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cong_96 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cong_126
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.idem
d_idem_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_idem_98 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_idem_98 v4 v5 v6 v7
du_idem_98 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_idem_98 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_idem_158
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.identity
d_identity_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_100 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_identity_100 v4 v5 v6 v7
du_identity_100 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_identity_100 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_identity_160
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v3))
-- Algebra.Morphism.GroupMonomorphism._.identityʳ
d_identity'691'_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_identity'691'_102 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_identity'691'_102 v4 v5 v6 v7
du_identity'691'_102 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_identity'691'_102 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_identity'691'_154
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v3))
-- Algebra.Morphism.GroupMonomorphism._.identityˡ
d_identity'737'_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_identity'737'_104 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_identity'737'_104 v4 v5 v6 v7
du_identity'737'_104 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_identity'737'_104 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_identity'737'_148
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v3))
-- Algebra.Morphism.GroupMonomorphism._.isBand
d_isBand_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_106 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isBand_106 v4 v5 v6 v7
du_isBand_106 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_106 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isBand_282
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.isCommutativeMonoid
d_isCommutativeMonoid_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_108 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isCommutativeMonoid_108 v4 v5 v6 v7
du_isCommutativeMonoid_108 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_108 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isCommutativeMonoid_220
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v3))
-- Algebra.Morphism.GroupMonomorphism._.isMagma
d_isMagma_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_110 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isMagma_110 v4 v5 v6 v7
du_isMagma_110 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_110 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isMagma_218
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.isMonoid
d_isMonoid_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_112 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isMonoid_112 v4 v5 v6 v7
du_isMonoid_112 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_isMonoid_112 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isMonoid_176
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v3))
-- Algebra.Morphism.GroupMonomorphism._.isSelectiveMagma
d_isSelectiveMagma_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_isSelectiveMagma_114 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isSelectiveMagma_114 v4 v5 v6 v7
du_isSelectiveMagma_114 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
du_isSelectiveMagma_114 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSelectiveMagma_320
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.isSemigroup
d_isSemigroup_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_116 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isSemigroup_116 v4 v5 v6 v7
du_isSemigroup_116 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_116 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSemigroup_248
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.sel
d_sel_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_118 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_sel_118 v4 v5 v6 v7
du_sel_118 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_sel_118 v0 v1 v2 v3
  = let v4
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0) in
    let v5
          = coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1) in
    let v6
          = coe
              MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
              (coe v3) in
    coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_sel_164
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v4))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v5))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v6))
-- Algebra.Morphism.GroupMonomorphism._.zero
d_zero_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_120 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_zero_120 v4 v5 v6 v7
du_zero_120 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zero_120 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_zero_174
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v3))
-- Algebra.Morphism.GroupMonomorphism._.zeroʳ
d_zero'691'_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_zero'691'_122 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_zero'691'_122 v4 v5 v6 v7
du_zero'691'_122 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_zero'691'_122 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_zero'691'_168
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v3))
-- Algebra.Morphism.GroupMonomorphism._.zeroˡ
d_zero'737'_124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_zero'737'_124 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_zero'737'_124 v4 v5 v6 v7
du_zero'737'_124 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_zero'737'_124 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_zero'737'_162
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
         (coe v3))
-- Algebra.Morphism.GroupMonomorphism._.inverseˡ
d_inverse'737'_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_inverse'737'_178 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_inverse'737'_178 v4 v5 v6 v7 v8 v9 v10
du_inverse'737'_178 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_inverse'737'_178 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_590 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6) v6)
      (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6) v6))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
               (coe v2 v6))
            (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
                  (coe v2 v6))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6))
                  (coe v2 v6))
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6))
                     (coe v2 v6))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v1))
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v1))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                        (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_562
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                              (coe v3)))))
                  (coe v5 (coe v2 v6)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6))
                  (coe v2 v6) (coe v2 v6)
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_'8315''185''45'homo_564
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                        (coe v3))
                     v6)
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
                     (coe v2 v6))))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_562
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                        (coe v3))))
               (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
               v6)))
-- Algebra.Morphism.GroupMonomorphism._.inverseʳ
d_inverse'691'_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_inverse'691'_184 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_inverse'691'_184 v4 v5 v6 v7 v8 v9 v10
du_inverse'691'_184 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_inverse'691'_184 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_590 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0 v6
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
      (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0 v6
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1 (coe v2 v6)
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)))
            (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1 (coe v2 v6)
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1 (coe v2 v6)
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6)))
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1 (coe v2 v6)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6)))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v1))
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v1))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                        (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_92 (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_562
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                              (coe v3)))))
                  (coe v5 (coe v2 v6)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4 (coe v2 v6)
                  (coe v2 v6)
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6))
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
                     (coe v2 v6))
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_'8315''185''45'homo_564
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                        (coe v3))
                     v6)))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_562
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                        (coe v3))))
               v6
               (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))))
-- Algebra.Morphism.GroupMonomorphism._.inverse
d_inverse_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_190 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
  = du_inverse_190 v4 v5 v6 v7 v8 v9
du_inverse_190 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_inverse_190 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                du_inverse'737'_178 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                (coe v6))
             (coe
                du_inverse'691'_184 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                (coe v7))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.GroupMonomorphism._.⁻¹-cong
d_'8315''185''45'cong_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_196 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10 v11
                          v12
  = du_'8315''185''45'cong_196 v4 v5 v6 v7 v8 v9 v10 v11 v12
du_'8315''185''45'cong_196 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8315''185''45'cong_196 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_590 v3
      (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
      (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6))
            (coe
               v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v7))
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v7))
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7))
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                     (coe
                        v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_'8315''185''45'homo_564
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                        (coe v3))
                     v7))
               (coe
                  v5 (coe v2 v6) (coe v2 v7)
                  (coe
                     MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_562
                              (coe
                                 MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                                 (coe v3)))))
                     v6 v7 v8)))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_'8315''185''45'homo_564
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                  (coe v3))
               v6)))
-- Algebra.Morphism.GroupMonomorphism._.⁻¹-distrib-∙
d_'8315''185''45'distrib'45''8729'_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'distrib'45''8729'_310 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
                                       v8 v9 v10 v11
  = du_'8315''185''45'distrib'45''8729'_310 v4 v5 v6 v7 v8 v9 v10 v11
du_'8315''185''45'distrib'45''8729'_310 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'8315''185''45'distrib'45''8729'_310 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_590 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0 v6 v7))
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v8 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
             let v9 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v8) in
             let v10
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v9) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0 v6 v7)))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0 v6 v7)))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v8 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                let v9 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v8) in
                let v10
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v9) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0 v6 v7)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1 (coe v2 v6)
                     (coe v2 v7)))
               (coe
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v8 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                   let v9 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v8) in
                   let v10
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v9) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1 (coe v2 v6)
                        (coe v2 v7)))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v7)))
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (let v8 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                      let v9 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v8) in
                      let v10
                            = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v9) in
                      coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v7)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
                        (coe
                           v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
                        (coe
                           v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
                     (coe
                        v2
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        (let v8 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                         let v9 = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v8) in
                         let v10
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v9) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe
                                 MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                 (let v8 = MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4) in
                                  let v9
                                        = MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v8) in
                                  let v10
                                        = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                            (coe v9) in
                                  coe
                                    MAlonzo.Code.Algebra.Structures.du_setoid_164
                                    (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v10)))))
                           (coe
                              v2
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7))))
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                           (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                              (coe
                                 MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                 (coe
                                    MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                    (coe
                                       MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                                       (coe
                                          MAlonzo.Code.Algebra.Structures.d_isGroup_988
                                          (coe v4))))))
                           (coe
                              v2
                              (let v8
                                     = coe
                                         MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
                                         (coe
                                            MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96
                                            (coe v0)) in
                               coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v8
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
                              (coe
                                 v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
                              (coe
                                 v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                              (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                                 (coe
                                    MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_562
                                    (coe
                                       MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                                       (coe v3))))
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6)
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7))))
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                        (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                           (coe
                              MAlonzo.Code.Algebra.Structures.d_isMagma_444
                              (coe
                                 MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                 (coe
                                    MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                                    (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4))))))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7)))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v7)))
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                           (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                              (coe
                                 MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                 (coe
                                    MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                                    (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4)))))
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v6))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v6))
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v0 v7))
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 v1 (coe v2 v7))
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_'8315''185''45'homo_564
                              (MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                                 (coe v3))
                              v6)
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_'8315''185''45'homo_564
                              (MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                                 (coe v3))
                              v7))))
                  (coe v5 (coe v2 v6) (coe v2 v7)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
                  (MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4))
                  (coe
                     v2
                     (let v8
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
                                (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0)) in
                      coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v8 v6 v7))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v1 (coe v2 v6)
                     (coe v2 v7))
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_562
                           (coe
                              MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                              (coe v3))))
                     v6 v7)))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_'8315''185''45'homo_564
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isGroupHomomorphism_588
                  (coe v3))
               (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__90 v0 v6 v7))))
-- Algebra.Morphism.GroupMonomorphism.isGroup
d_isGroup_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
d_isGroup_318 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isGroup_318 v4 v5 v6 v7 v8
du_isGroup_318 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888 ->
  MAlonzo.Code.Algebra.Structures.T_IsGroup_888
du_isGroup_318 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsGroup'46'constructor_22905
      (coe
         MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism.du_isMonoid_176
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0))
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
            (coe v3))
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4)))
      (coe
         du_inverse_190 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4))))
         (coe MAlonzo.Code.Algebra.Structures.d_inverse_904 (coe v4)))
      (coe
         du_'8315''185''45'cong_196 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_902 (coe v4))))
         (coe
            MAlonzo.Code.Algebra.Structures.d_'8315''185''45'cong_906
            (coe v4)))
-- Algebra.Morphism.GroupMonomorphism.isAbelianGroup
d_isAbelianGroup_382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
d_isAbelianGroup_382 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isAbelianGroup_382 v4 v5 v6 v7 v8
du_isAbelianGroup_382 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976 ->
  MAlonzo.Code.Algebra.Structures.T_IsAbelianGroup_976
du_isAbelianGroup_382 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsAbelianGroup'46'constructor_27897
      (coe
         du_isGroup_318 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
            (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v0)))
         (coe
            MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60
            (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMonoid_96 (coe v1)))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.du_isMonoidMonomorphism_608
               (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_902
                  (coe MAlonzo.Code.Algebra.Structures.d_isGroup_988 (coe v4)))))
         (coe MAlonzo.Code.Algebra.Structures.d_comm_990 (coe v4)))
