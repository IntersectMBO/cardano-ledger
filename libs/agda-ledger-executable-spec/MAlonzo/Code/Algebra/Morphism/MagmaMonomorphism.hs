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

module MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Morphism.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Morphism.RelMonomorphism
import qualified MAlonzo.Code.Relation.Binary.Morphism.Structures
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Morphism.MagmaMonomorphism._._∙_
d__'8729'__38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__38 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du__'8729'__38 v4
du__'8729'__38 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8729'__38 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 (coe v0)
-- Algebra.Morphism.MagmaMonomorphism._._≈_
d__'8776'__40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__40 = erased
-- Algebra.Morphism.MagmaMonomorphism._._≈_
d__'8776'__48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__48 = erased
-- Algebra.Morphism.MagmaMonomorphism._._∙_
d__'8729'__52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__52 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du__'8729'__52 v5
du__'8729'__52 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8729'__52 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 (coe v0)
-- Algebra.Morphism.MagmaMonomorphism._.cong
d_cong_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_126 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
  = du_cong_126 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
du_cong_126 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cong_126 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104 v3
      (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v5 v7)
      (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v8)
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v5 v7))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v5)
               (coe v2 v7))
            (coe
               v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v8))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v5)
                  (coe v2 v7))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                  (coe v2 v8))
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v8))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                     (coe v2 v8))
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v8))
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v8))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                     (coe
                        v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v8)))
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                        (coe v3))
                     v6 v8))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4 (coe v2 v5)
                  (coe v2 v6) (coe v2 v7) (coe v2 v8)
                  (coe
                     MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                           (coe v3)))
                     v5 v6 v9)
                  (coe
                     MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                           (coe v3)))
                     v7 v8 v10)))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                  (coe v3))
               v5 v7)))
-- Algebra.Morphism.MagmaMonomorphism._.assoc
d_assoc_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_140 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10 v11 v12
  = du_assoc_140 v4 v5 v6 v7 v8 v9 v10 v11 v12
du_assoc_140 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_140 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7) v8)
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6
         (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7) v8))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7))
               (coe v2 v8))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6
                  (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7))
                  (coe v2 v8))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                     (coe v2 v7))
                  (coe v2 v8))
               (coe
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                        (coe v2 v7))
                     (coe v2 v8))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v7)
                        (coe v2 v8)))
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6
                        (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v7)
                           (coe v2 v8)))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                        (coe
                           v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8)))
                     (coe
                        v2
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6
                           (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                        (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                           (coe
                              v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6
                              (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8)))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                           (coe
                              v2
                              (coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6
                                 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8))))
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                           (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                              (coe v3))
                           v6 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8)))
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4 (coe v2 v6)
                        (coe v2 v6)
                        (coe
                           v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v8))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v7)
                           (coe v2 v8))
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
                           (coe v2 v6))
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                           (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                              (coe v3))
                           v7 v8)))
                  (coe v5 (coe v2 v6) (coe v2 v7) (coe v2 v8)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                     (coe v2 v7))
                  (coe v2 v8) (coe v2 v8)
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                        (coe v3))
                     v6 v7)
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
                     (coe v2 v8))))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                  (coe v3))
               (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7) v8)))
-- Algebra.Morphism.MagmaMonomorphism._.comm
d_comm_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_150 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10 v11
  = du_comm_150 v4 v5 v6 v7 v8 v9 v10 v11
du_comm_150 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_150 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104 v3
      (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7)
      (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v6)
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
               (coe v2 v7))
            (coe
               v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v6))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                  (coe v2 v7))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v7)
                  (coe v2 v6))
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v6))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v7)
                     (coe v2 v6))
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v6))
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v6))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                     (coe
                        v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v6)))
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                        (coe v3))
                     v7 v6))
               (coe v5 (coe v2 v6) (coe v2 v7)))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                  (coe v3))
               v6 v7)))
-- Algebra.Morphism.MagmaMonomorphism._.idem
d_idem_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_idem_158 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_idem_158 v4 v5 v6 v7 v8 v9 v10
du_idem_158 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_idem_158 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104 v3
      (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v6) v6
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v6))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
               (coe v2 v6))
            (coe v2 v6)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                  (coe v2 v6))
               (coe v2 v6) (coe v2 v6)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                  (coe v2 v6))
               (coe v5 (coe v2 v6)))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                  (coe v3))
               v6 v6)))
-- Algebra.Morphism.MagmaMonomorphism._.sel
d_sel_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_164 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10 v11
  = du_sel_164 v4 v5 v6 v7 v8 v9 v10 v11
du_sel_164 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_sel_164 v0 v1 v2 v3 v4 v5 v6 v7
  = let v8 = coe v5 (coe v2 v6) (coe v2 v7) in
    case coe v8 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v9
        -> coe
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
             (coe
                MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104 v3
                (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7) v6
                (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                      (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                      (coe
                         v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7))
                      (coe
                         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                         (coe v2 v7))
                      (coe v2 v6)
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                         (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                         (coe
                            MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                            (coe v2 v7))
                         (coe v2 v6) (coe v2 v6)
                         (coe
                            MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                            (coe
                               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                               (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                            (coe v2 v6))
                         v9)
                      (coe
                         MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                         (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                            (coe v3))
                         v6 v7))))
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v9
        -> coe
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
             (coe
                MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104 v3
                (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7) v7
                (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                      (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                      (coe
                         v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7))
                      (coe
                         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                         (coe v2 v7))
                      (coe v2 v7)
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                         (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                         (coe
                            MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                            (coe v2 v7))
                         (coe v2 v7) (coe v2 v7)
                         (coe
                            MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                            (coe
                               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                               (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                            (coe v2 v7))
                         v9)
                      (coe
                         MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                         (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                            (coe v3))
                         v6 v7))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.MagmaMonomorphism._.cancelˡ
d_cancel'737'_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'737'_192 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
  = du_cancel'737'_192 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
du_cancel'737'_192 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'737'_192 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104 v3 v7 v8
      (coe
         v5 (coe v2 v6) (coe v2 v7) (coe v2 v8)
         (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                  (coe v2 v7))
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                  (coe v2 v8))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7))
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v8))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                     (coe v2 v8))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                     (coe
                        v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v8))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                        (coe v2 v8))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                        (coe v2 v8))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v6)
                           (coe v2 v8)))
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                        (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                           (coe v3))
                        v6 v8))
                  (coe
                     MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                           (coe v3)))
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v7)
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v6 v8) v9))
               (coe
                  MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                  (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                     (coe v3))
                  v6 v7))))
-- Algebra.Morphism.MagmaMonomorphism._.cancelʳ
d_cancel'691'_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'691'_204 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
  = du_cancel'691'_204 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
du_cancel'691'_204 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'691'_204 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104 v3 v7 v8
      (coe
         v5 (coe v2 v6) (coe v2 v7) (coe v2 v8)
         (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v7)
                  (coe v2 v6))
               (coe
                  v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v6))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v8)
                  (coe v2 v6))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v6))
                  (coe
                     v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v8 v6))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v8)
                     (coe v2 v6))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                     (coe
                        v2 (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v8 v6))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v8)
                        (coe v2 v6))
                     (coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v8)
                        (coe v2 v6))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                        (coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v1 (coe v2 v8)
                           (coe v2 v6)))
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                        (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                           (coe v3))
                        v8 v6))
                  (coe
                     MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
                     (MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                           (coe v3)))
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v7 v6)
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 v0 v8 v6) v9))
               (coe
                  MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
                  (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
                     (coe v3))
                  v7 v6))))
-- Algebra.Morphism.MagmaMonomorphism._.cancel
d_cancel_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cancel_216 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_cancel_216 v4 v5 v6 v7 v8
du_cancel_216 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_cancel_216 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Data.Product.Base.du_map_104
      (coe
         du_cancel'737'_192 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
      (coe
         (\ v5 ->
            coe
              du_cancel'691'_204 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)))
-- Algebra.Morphism.MagmaMonomorphism.isMagma
d_isMagma_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_218 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isMagma_218 v4 v5 v6 v7 v8
du_isMagma_218 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_218 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.RelMonomorphism.du_isEquivalence_78
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_isRelMonomorphism_114
            (coe v3))
         (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
      (coe du_cong_126 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
-- Algebra.Morphism.MagmaMonomorphism.isSemigroup
d_isSemigroup_248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_248 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isSemigroup_248 v4 v5 v6 v7 v8
du_isSemigroup_248 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_248 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe
         du_isMagma_218 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4)))
      (coe
         du_assoc_140 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
         (coe MAlonzo.Code.Algebra.Structures.d_assoc_446 (coe v4)))
-- Algebra.Morphism.MagmaMonomorphism.isBand
d_isBand_282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_282 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isBand_282 v4 v5 v6 v7 v8
du_isBand_282 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_282 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsBand'46'constructor_10089
      (coe
         du_isSemigroup_248 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v4)))
      (coe
         du_idem_158 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v4)))
         (coe MAlonzo.Code.Algebra.Structures.d_idem_482 (coe v4)))
-- Algebra.Morphism.MagmaMonomorphism.isSelectiveMagma
d_isSelectiveMagma_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_isSelectiveMagma_320 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isSelectiveMagma_320 v4 v5 v6 v7 v8
du_isSelectiveMagma_320 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
du_isSelectiveMagma_320 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSelectiveMagma'46'constructor_8519
      (coe
         du_isMagma_218 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v4)))
      (coe
         du_sel_164 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v4))
         (coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v4)))
