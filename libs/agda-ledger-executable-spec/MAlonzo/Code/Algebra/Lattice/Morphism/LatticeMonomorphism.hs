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

module MAlonzo.Code.Algebra.Lattice.Morphism.LatticeMonomorphism where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Lattice.Bundles
import qualified MAlonzo.Code.Algebra.Lattice.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Lattice.Morphism.Structures
import qualified MAlonzo.Code.Algebra.Lattice.Properties.Lattice
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Lattice.Structures.Biased
import qualified MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism
import qualified MAlonzo.Code.Algebra.Morphism.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Morphism.RelMonomorphism
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Lattice.Morphism.LatticeMonomorphism._._∧_
d__'8743'__50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__50 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du__'8743'__50 v4
du__'8743'__50 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8743'__50 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 (coe v0)
-- Algebra.Lattice.Morphism.LatticeMonomorphism._._∨_
d__'8744'__52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__52 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 = du__'8744'__52 v4
du__'8744'__52 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8744'__52 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 (coe v0)
-- Algebra.Lattice.Morphism.LatticeMonomorphism._._≈_
d__'8776'__54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__54 = erased
-- Algebra.Lattice.Morphism.LatticeMonomorphism._._≈_
d__'8776'__66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__66 = erased
-- Algebra.Lattice.Morphism.LatticeMonomorphism._._∧_
d__'8743'__70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__70 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du__'8743'__70 v5
du__'8743'__70 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8743'__70 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 (coe v0)
-- Algebra.Lattice.Morphism.LatticeMonomorphism._._∨_
d__'8744'__72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__72 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du__'8744'__72 v5
du__'8744'__72 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8744'__72 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 (coe v0)
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.assoc
d_assoc_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_82 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_assoc_82 v4 v5 v6 v7
du_assoc_82 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_82 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_assoc_140
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.cancel
d_cancel_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cancel_84 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_cancel_84 v4 v5 v6 v7
du_cancel_84 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_cancel_84 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel_216
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.cancelʳ
d_cancel'691'_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'691'_86 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'691'_86 v4 v5 v6 v7
du_cancel'691'_86 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'691'_86 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'691'_204
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.cancelˡ
d_cancel'737'_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'737'_88 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'737'_88 v4 v5 v6 v7
du_cancel'737'_88 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'737'_88 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'737'_192
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.comm
d_comm_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_90 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_comm_90 v4 v5 v6 v7
du_comm_90 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_90 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.cong
d_cong_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_92 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_cong_92 v4 v5 v6 v7
du_cong_92 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cong_92 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cong_126
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.idem
d_idem_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_idem_94 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_idem_94 v4 v5 v6 v7
du_idem_94 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_idem_94 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_idem_158
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.sel
d_sel_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_96 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_sel_96 v4 v5 v6 v7
du_sel_96 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_sel_96 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_sel_164
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.assoc
d_assoc_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_100 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_assoc_100 v4 v5 v6 v7
du_assoc_100 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_100 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_assoc_140
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.cancel
d_cancel_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cancel_102 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel_102 v4 v5 v6 v7
du_cancel_102 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_cancel_102 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel_216
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.cancelʳ
d_cancel'691'_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'691'_104 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'691'_104 v4 v5 v6 v7
du_cancel'691'_104 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'691'_104 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'691'_204
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.cancelˡ
d_cancel'737'_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cancel'737'_106 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_cancel'737'_106 v4 v5 v6 v7
du_cancel'737'_106 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cancel'737'_106 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cancel'737'_192
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.comm
d_comm_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_108 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_comm_108 v4 v5 v6 v7
du_comm_108 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_108 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.cong
d_cong_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_110 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_cong_110 v4 v5 v6 v7
du_cong_110 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cong_110 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cong_126
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.idem
d_idem_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_idem_112 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_idem_112 v4 v5 v6 v7
du_idem_112 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_idem_112 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_idem_158
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.sel
d_sel_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_114 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_sel_114 v4 v5 v6 v7
du_sel_114 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140 ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_sel_114 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_sel_164
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
         (coe v1))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
         (coe v3))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.setoid
d_setoid_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_138 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_setoid_138 v8
du_setoid_138 ::
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_138 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe v0))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_164 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
                                  v10
  = du_'8744''45'absorbs'45''8743'_164 v4 v5 v6 v7 v8 v9 v10
du_'8744''45'absorbs'45''8743'_164 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_164 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_injective_226 v3
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v5
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v5 v6))
      v5
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe du_setoid_138 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v5
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v5 v6)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
               (coe v2 v5)
               (coe
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v5 v6)))
            (coe v2 v5)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe du_setoid_138 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                  (coe v2 v5)
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v5 v6)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                  (coe v2 v5)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                     (coe v2 v5) (coe v2 v6)))
               (coe v2 v5)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe du_setoid_138 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                     (coe v2 v5)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                        (coe v2 v5) (coe v2 v6)))
                  (coe v2 v5) (coe v2 v5)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                           (coe v4)))
                     (coe v2 v5))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
                     v4 (coe v2 v5) (coe v2 v6)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
                  (coe v4) (coe v2 v5)
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v5 v6))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                     (coe v2 v5) (coe v2 v6))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_'8743''45'homo_202
                     (MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_isLatticeHomomorphism_224
                        (coe v3))
                     v5 v6)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_'8744''45'homo_204
               (MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_isLatticeHomomorphism_224
                  (coe v3))
               v5
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v5 v6))))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_170 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
                                  v10
  = du_'8743''45'absorbs'45''8744'_170 v4 v5 v6 v7 v8 v9 v10
du_'8743''45'absorbs'45''8744'_170 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_170 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_injective_226 v3
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v5
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v5 v6))
      v5
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe du_setoid_138 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v5
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v5 v6)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
               (coe v2 v5)
               (coe
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v5 v6)))
            (coe v2 v5)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe du_setoid_138 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                  (coe v2 v5)
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v5 v6)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                  (coe v2 v5)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                     (coe v2 v5) (coe v2 v6)))
               (coe v2 v5)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe du_setoid_138 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                     (coe v2 v5)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                        (coe v2 v5) (coe v2 v6)))
                  (coe v2 v5) (coe v2 v5)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                           (coe v4)))
                     (coe v2 v5))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
                     v4 (coe v2 v5) (coe v2 v6)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
                  (coe v4) (coe v2 v5)
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v5 v6))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                     (coe v2 v5) (coe v2 v6))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_'8744''45'homo_204
                     (MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_isLatticeHomomorphism_224
                        (coe v3))
                     v5 v6)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_'8743''45'homo_202
               (MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_isLatticeHomomorphism_224
                  (coe v3))
               v5
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v5 v6))))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.absorptive
d_absorptive_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_176 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_absorptive_176 v4 v5 v6 v7 v8
du_absorptive_176 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_absorptive_176 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_'8744''45'absorbs'45''8743'_164 (coe v0) (coe v1) (coe v2)
         (coe v3) (coe v4))
      (coe
         du_'8743''45'absorbs'45''8744'_170 (coe v0) (coe v1) (coe v2)
         (coe v3) (coe v4))
-- Algebra.Lattice.Morphism.LatticeMonomorphism._.distribʳ
d_distrib'691'_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_178 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9 v10 v11 v12
  = du_distrib'691'_178 v4 v5 v6 v7 v8 v9 v10 v11 v12
du_distrib'691'_178 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_178 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_injective_226 v3
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v7 v8)
         v6)
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6)
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (coe du_setoid_138 (coe v4))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v7 v8)
                  v6))
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
               (coe
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v7 v8))
               (coe v2 v6))
            (coe
               v2
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6)
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe du_setoid_138 (coe v4))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v7 v8))
                  (coe v2 v6))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                     (coe v2 v7) (coe v2 v8))
                  (coe v2 v6))
               (coe
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6)
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe du_setoid_138 (coe v4))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                        (coe v2 v7) (coe v2 v8))
                     (coe v2 v6))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                        (coe v2 v7) (coe v2 v6))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                        (coe v2 v8) (coe v2 v6)))
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                     (coe du_setoid_138 (coe v4))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                           (coe v2 v7) (coe v2 v6))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                           (coe v2 v8) (coe v2 v6)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6)))
                     (coe
                        v2
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                        (coe du_setoid_138 (coe v4))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                           (coe
                              v2
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6))
                           (coe
                              v2
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6)))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6)
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6)))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
                                 (coe v4)))
                           (coe
                              v2
                              (coe
                                 MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6)
                                 (coe
                                    MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8
                                    v6))))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_'8743''45'homo_202
                           (MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_isLatticeHomomorphism_224
                              (coe v3))
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6)
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6)))
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778 v4
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v7 v6))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                           (coe v2 v7) (coe v2 v6))
                        (coe
                           v2
                           (coe
                              MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v0 v8 v6))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 v1
                           (coe v2 v8) (coe v2 v6))
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_'8744''45'homo_204
                           (MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_isLatticeHomomorphism_224
                              (coe v3))
                           v7 v6)
                        (coe
                           MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_'8744''45'homo_204
                           (MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_isLatticeHomomorphism_224
                              (coe v3))
                           v8 v6)))
                  (coe v5 (coe v2 v6) (coe v2 v7) (coe v2 v8)))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
                  (coe v4) (coe v2 v6)
                  (coe
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v7 v8))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v1
                     (coe v2 v7) (coe v2 v8))
                  (coe
                     MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_'8743''45'homo_202
                     (MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_isLatticeHomomorphism_224
                        (coe v3))
                     v7 v8)))
            (coe
               MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_'8744''45'homo_204
               (MAlonzo.Code.Algebra.Lattice.Morphism.Structures.d_isLatticeHomomorphism_224
                  (coe v3))
               (coe
                  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 v0 v7 v8)
               v6)))
-- Algebra.Lattice.Morphism.LatticeMonomorphism.isLattice
d_isLattice_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_isLattice_188 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isLattice_188 v4 v5 v6 v7 v8
du_isLattice_188 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
du_isLattice_188 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsLattice'46'constructor_34033
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.RelMonomorphism.du_isEquivalence_78
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.du_isRelMonomorphism_114
            (coe
               MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
               (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
            (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
            (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
            (coe v1))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isMagma_2976
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.C_Lattice'46'constructor_7911
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 (coe v1))
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 (coe v1))
               v4))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
            (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_assoc_140
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
            (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
            (coe v1))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isMagma_2976
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.C_Lattice'46'constructor_7911
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 (coe v1))
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 (coe v1))
               v4))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
            (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cong_126
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
            (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
            (coe v1))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8744''45'isMagmaMonomorphism_242
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isMagma_2976
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.C_Lattice'46'constructor_7911
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 (coe v1))
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 (coe v1))
               v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_comm_150
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
            (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
            (coe v1))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isMagma_2952
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.C_Lattice'46'constructor_7911
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 (coe v1))
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 (coe v1))
               v4))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
            (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_assoc_140
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
            (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
            (coe v1))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isMagma_2952
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.C_Lattice'46'constructor_7911
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 (coe v1))
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 (coe v1))
               v4))
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
            (coe v4)))
      (coe
         MAlonzo.Code.Algebra.Morphism.MagmaMonomorphism.du_cong_126
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
            (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
            (coe v1))
         (coe v2)
         (coe
            MAlonzo.Code.Algebra.Lattice.Morphism.Structures.du_'8743''45'isMagmaMonomorphism_244
            (coe v3))
         (coe
            MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isMagma_2952
            (coe
               MAlonzo.Code.Algebra.Lattice.Bundles.C_Lattice'46'constructor_7911
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 (coe v1))
               (MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 (coe v1))
               v4)))
      (coe
         du_absorptive_176 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
-- Algebra.Lattice.Morphism.LatticeMonomorphism.isDistributiveLattice
d_isDistributiveLattice_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_isDistributiveLattice_288 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_isDistributiveLattice_288 v4 v5 v6 v7 v8
du_isDistributiveLattice_288 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Lattice.Morphism.Structures.T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
du_isDistributiveLattice_288 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.Biased.du_isDistributiveLattice'691''690''7504'_732
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.Biased.C_IsDistributiveLattice'691''690''7504''46'constructor_7017
         (coe
            du_isLattice_188 (coe v0) (coe v1) (coe v2) (coe v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v4)))
         (coe
            du_distrib'691'_178 (coe v0) (coe v1) (coe v2) (coe v3)
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v4))
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'691''45''8743'_2878
               (coe v4))))
