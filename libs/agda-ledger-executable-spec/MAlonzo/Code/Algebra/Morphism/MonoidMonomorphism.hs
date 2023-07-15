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

module MAlonzo.Code.Algebra.Morphism.MonoidMonomorphism where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Morphism.MonoidMonomorphism._._∙_
d__'8729'__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__44 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du__'8729'__44 v4
du__'8729'__44 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8729'__44 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 (coe v0)
-- Algebra.Morphism.MonoidMonomorphism._._≈_
d__'8776'__46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__46 = erased
-- Algebra.Morphism.MonoidMonomorphism._.ε
d_ε_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  AgdaAny
d_ε_54 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du_ε_54 v4
du_ε_54 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 -> AgdaAny
du_ε_54 v0 = coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)
-- Algebra.Morphism.MonoidMonomorphism._._≈_
d__'8776'__58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__58 = erased
-- Algebra.Morphism.MonoidMonomorphism._._∙_
d__'8729'__62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__62 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du__'8729'__62 v5
du__'8729'__62 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8729'__62 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 (coe v0)
-- Algebra.Morphism.MonoidMonomorphism._.ε
d_ε_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  AgdaAny
d_ε_68 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du_ε_68 v5
du_ε_68 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 -> AgdaAny
du_ε_68 v0 = coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)
-- Algebra.Morphism.MonoidMonomorphism._.assoc
d_assoc_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_72 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_assoc_72 v4 v5 v6 v7
du_assoc_72 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_72 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_assoc_140
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.cancel
d_cancel_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cancel_74 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_cancel_74 v4 v5 v6 v7
du_cancel_74 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_cancel_74 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel_216
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.cancelʳ
d_cancel'691'_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'691'_76 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'691'_76 v4 v5 v6 v7
du_cancel'691'_76 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'691'_76 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'691'_204
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.cancelˡ
d_cancel'737'_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'737'_78 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'737'_78 v4 v5 v6 v7
du_cancel'737'_78 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'737'_78 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'737'_192
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.comm
d_comm_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_80 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_comm_80 v4 v5 v6 v7
du_comm_80 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_80 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.cong
d_cong_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_82 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_cong_82 v4 v5 v6 v7
du_cong_82 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cong_82 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cong_126
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.idem
d_idem_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_idem_84 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_idem_84 v4 v5 v6 v7
du_idem_84 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_idem_84 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_idem_158
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.isBand
d_isBand_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_86 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_isBand_86 v4 v5 v6 v7
du_isBand_86 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_86 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isBand_282
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.isMagma
d_isMagma_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_88 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isMagma_88 v4 v5 v6 v7
du_isMagma_88 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_88 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isMagma_218
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.isSelectiveMagma
d_isSelectiveMagma_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_isSelectiveMagma_90 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isSelectiveMagma_90 v4 v5 v6 v7
du_isSelectiveMagma_90 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
du_isSelectiveMagma_90 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSelectiveMagma_320
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.isSemigroup
d_isSemigroup_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_92 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_isSemigroup_92 v4 v5 v6 v7
du_isSemigroup_92 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_92 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSemigroup_248
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.sel
d_sel_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_94 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_sel_94 v4 v5 v6 v7
du_sel_94 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_sel_94 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_sel_164
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
         (coe v3))
-- Algebra.Morphism.MonoidMonomorphism._.identityˡ
d_identity'737'_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_identity'737'_148 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_identity'737'_148 v4 v5 v6 v7 v8 v9 v10
du_identity'737'_148 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_identity'737'_148 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_298 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v0
         (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)) v6)
      v6
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v0
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)) v6))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
               (coe v2 v6))
            (coe v2 v6)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                  (coe v2 v6))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)) (coe v2 v6))
               (coe v2 v6)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1
                     (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)) (coe v2 v6))
                  (coe v2 v6) (coe v2 v6)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                     (coe v2 v6))
                  (coe v5 (coe v2 v6)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)) (coe v2 v6)
                  (coe v2 v6)
                  (MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_296
                        (coe v3)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
                     (coe v2 v6))))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_296
                     (coe v3)))
               (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)) v6)))
-- Algebra.Morphism.MonoidMonomorphism._.identityʳ
d_identity'691'_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_identity'691'_154 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_identity'691'_154 v4 v5 v6 v7 v8 v9 v10
du_identity'691'_154 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_identity'691'_154 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_298 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v0 v6
         (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
      v6
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v0 v6
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0))))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1 (coe v2 v6)
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0))))
            (coe v2 v6)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1 (coe v2 v6)
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0))))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1 (coe v2 v6)
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)))
               (coe v2 v6)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1 (coe v2 v6)
                     (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)))
                  (coe v2 v6) (coe v2 v6)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                     (coe v2 v6))
                  (coe v5 (coe v2 v6)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4 (coe v2 v6)
                  (coe v2 v6)
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1))
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
                     (coe v2 v6))
                  (MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_296
                        (coe v3)))))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_296
                     (coe v3)))
               v6 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))))
-- Algebra.Morphism.MonoidMonomorphism._.identity
d_identity_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_160 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_identity_160 v4 v5 v6 v7 v8
du_identity_160 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_identity_160 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Data.Product.Base.du_map_104
      (coe
         du_identity'737'_148 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
      (coe
         (\ v5 ->
            coe
              du_identity'691'_154 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)))
-- Algebra.Morphism.MonoidMonomorphism._.zeroˡ
d_zero'737'_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_zero'737'_162 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_zero'737'_162 v4 v5 v6 v7 v8 v9 v10
du_zero'737'_162 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_zero'737'_162 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_298 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v0
         (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)) v6)
      (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v0
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)) v6))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
               (coe v2 v6))
            (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                  (coe v2 v6))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)) (coe v2 v6))
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1
                     (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)) (coe v2 v6))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1))
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                        (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_296
                           (coe v3))))
                  (coe v5 (coe v2 v6)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)) (coe v2 v6)
                  (coe v2 v6)
                  (MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_296
                        (coe v3)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
                     (coe v2 v6))))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_296
                     (coe v3)))
               (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)) v6)))
-- Algebra.Morphism.MonoidMonomorphism._.zeroʳ
d_zero'691'_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_zero'691'_168 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10
  = du_zero'691'_168 v4 v5 v6 v7 v8 v9 v10
du_zero'691'_168 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_zero'691'_168 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_298 v3
      (coe
         MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v0 v6
         (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
      (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v0 v6
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0))))
            (coe
               MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1 (coe v2 v6)
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0))))
            (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1 (coe v2 v6)
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0))))
               (coe
                  MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1 (coe v2 v6)
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)))
               (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1 (coe v2 v6)
                     (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1))
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (coe MAlonzo.Code.Algebra.Structures.du_setoid_164 (coe v4))
                     (coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                     (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                        (coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4)))
                        (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0))))
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                        (coe
                           MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_296
                           (coe v3))))
                  (coe v5 (coe v2 v6)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150 v4 (coe v2 v6)
                  (coe v2 v6)
                  (coe v2 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))
                  (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1))
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
                     (coe v2 v6))
                  (MAlonzo.Code.Algebra.Morphism.Structures.d_ε'45'homo_276
                     (coe
                        MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_296
                        (coe v3)))))
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
               (MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_274
                  (coe
                     MAlonzo.Code.Algebra.Morphism.Structures.d_isMonoidHomomorphism_296
                     (coe v3)))
               v6 (MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)))))
-- Algebra.Morphism.MonoidMonomorphism._.zero
d_zero_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_174 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_zero_174 v4 v5 v6 v7 v8
du_zero_174 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zero_174 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Data.Product.Base.du_map_104
      (coe du_zero'737'_162 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
      (coe
         (\ v5 ->
            coe du_zero'691'_168 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)))
-- Algebra.Morphism.MonoidMonomorphism.isMonoid
d_isMonoid_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_176 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isMonoid_176 v4 v5 v6 v7 v8
du_isMonoid_176 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_isMonoid_176 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_isSemigroup_248
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
            (coe v3))
         (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4)))
      (coe
         du_identity_160 v0 v1 v2 v3
         (MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4)))
         (MAlonzo.Code.Algebra.Structures.d_identity_612 (coe v4)))
-- Algebra.Morphism.MonoidMonomorphism.isCommutativeMonoid
d_isCommutativeMonoid_220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_220 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isCommutativeMonoid_220 v4 v5 v6 v7 v8
du_isCommutativeMonoid_220 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_isCommutativeMonoid_220 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe
         du_isMonoid_176 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v0))
         (coe MAlonzo.Code.Algebra.Bundles.Raw.du_rawMagma_60 (coe v1))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_isMagmaMonomorphism_312
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4))))
         (coe MAlonzo.Code.Algebra.Structures.d_comm_662 (coe v4)))
