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

module MAlonzo.Code.Algebra.Lattice.Morphism.Structures where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Lattice.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Morphism.Structures
import qualified MAlonzo.Code.Relation.Binary.Morphism.Structures

-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms._._∧_
d__'8743'__32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__32 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'8743'__32 v4
du__'8743'__32 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8743'__32 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8743'__30 (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms._._∨_
d__'8744'__34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__34 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'8744'__34 v4
du__'8744'__34 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8744'__34 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.d__'8744'__32 (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms._._≈_
d__'8776'__36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__36 = erased
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms._.Carrier
d_Carrier_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 -> ()
d_Carrier_40 = erased
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaHomomorphism
d_IsMagmaHomomorphism_64 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaIsomorphism
d_IsMagmaIsomorphism_66 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaMonomorphism
d_IsMagmaMonomorphism_68 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaHomomorphism.homo
d_homo_72 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_homo_72 v0
  = coe MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86 (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_74 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_74 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
      (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaHomomorphism.cong
d_cong_76 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_76 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaIsomorphism.homo
d_homo_80 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_homo_80 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
            (coe v0)))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaIsomorphism.injective
d_injective_82 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_82 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_84 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_84 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_86 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_86 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
      (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_88 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_88 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
            (coe v0)))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_90 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_90
du_isRelIsomorphism_90 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_90 v0 v1
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.du_isRelIsomorphism_144 v1
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_92 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_92 v7
du_isRelMonomorphism_92 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_92 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.du_isRelMonomorphism_114
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaIsomorphism.surjective
d_surjective_94 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_94 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_surjective_128 (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaIsomorphism.cong
d_cong_96 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_96 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
               (coe v0))))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaMonomorphism.homo
d_homo_100 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_homo_100 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaMonomorphism.injective
d_injective_102 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_102 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104 (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_104 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_104 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
      (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_106 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_106 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_108 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_108
du_isRelMonomorphism_108 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_108 v0 v1
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.du_isRelMonomorphism_114
      v1
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∨.IsMagmaMonomorphism.cong
d_cong_110 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_110 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
            (coe v0)))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaHomomorphism
d_IsMagmaHomomorphism_114 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaIsomorphism
d_IsMagmaIsomorphism_116 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaMonomorphism
d_IsMagmaMonomorphism_118 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaHomomorphism.homo
d_homo_122 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_homo_122 v0
  = coe MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86 (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_124 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_124 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
      (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaHomomorphism.cong
d_cong_126 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_126 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaIsomorphism.homo
d_homo_130 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_homo_130 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
            (coe v0)))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaIsomorphism.injective
d_injective_132 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_132 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_134 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_134 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_136 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_136 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
      (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_138 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_138 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
            (coe v0)))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_140 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_140
du_isRelIsomorphism_140 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_140 v0 v1
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.du_isRelIsomorphism_144 v1
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_142 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_142 v7
du_isRelMonomorphism_142 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_142 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.du_isRelMonomorphism_114
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaIsomorphism.surjective
d_surjective_144 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_144 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_surjective_128 (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaIsomorphism.cong
d_cong_146 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_146 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
            (coe
               MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaMonomorphism_126
               (coe v0))))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaMonomorphism.homo
d_homo_150 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_homo_150 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_homo_86
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaMonomorphism.injective
d_injective_152 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_152 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_injective_104 (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_154 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_154 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
      (coe v0)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_156 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_156 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
         (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_158 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_158
du_isRelMonomorphism_158 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_158 v0 v1
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.du_isRelMonomorphism_114
      v1
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.∧.IsMagmaMonomorphism.cong
d_cong_160 ::
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_160 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         MAlonzo.Code.Algebra.Morphism.Structures.d_isRelHomomorphism_84
         (coe
            MAlonzo.Code.Algebra.Morphism.Structures.d_isMagmaHomomorphism_102
            (coe v0)))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms._.Homomorphic₂
d_Homomorphic'8322'_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8322'_168 = erased
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms._.Injective
d_Injective_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_178 = erased
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms._.Surjective
d_Surjective_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_186 = erased
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeHomomorphism
d_IsLatticeHomomorphism_190 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsLatticeHomomorphism_190
  = C_IsLatticeHomomorphism'46'constructor_2097 MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
                                                (AgdaAny -> AgdaAny -> AgdaAny)
                                                (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeHomomorphism.isRelHomomorphism
d_isRelHomomorphism_200 ::
  T_IsLatticeHomomorphism_190 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_200 v0
  = case coe v0 of
      C_IsLatticeHomomorphism'46'constructor_2097 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeHomomorphism.∧-homo
d_'8743''45'homo_202 ::
  T_IsLatticeHomomorphism_190 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'homo_202 v0
  = case coe v0 of
      C_IsLatticeHomomorphism'46'constructor_2097 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeHomomorphism.∨-homo
d_'8744''45'homo_204 ::
  T_IsLatticeHomomorphism_190 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'homo_204 v0
  = case coe v0 of
      C_IsLatticeHomomorphism'46'constructor_2097 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeHomomorphism._.cong
d_cong_208 ::
  T_IsLatticeHomomorphism_190 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_208 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_200 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeHomomorphism.∧-isMagmaHomomorphism
d_'8743''45'isMagmaHomomorphism_210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeHomomorphism_190 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_'8743''45'isMagmaHomomorphism_210 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8743''45'isMagmaHomomorphism_210 v7
du_'8743''45'isMagmaHomomorphism_210 ::
  T_IsLatticeHomomorphism_190 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
du_'8743''45'isMagmaHomomorphism_210 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMagmaHomomorphism'46'constructor_1049
      (coe d_isRelHomomorphism_200 (coe v0))
      (coe d_'8743''45'homo_202 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeHomomorphism.∨-isMagmaHomomorphism
d_'8744''45'isMagmaHomomorphism_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeHomomorphism_190 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_'8744''45'isMagmaHomomorphism_212 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8744''45'isMagmaHomomorphism_212 v7
du_'8744''45'isMagmaHomomorphism_212 ::
  T_IsLatticeHomomorphism_190 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
du_'8744''45'isMagmaHomomorphism_212 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMagmaHomomorphism'46'constructor_1049
      (coe d_isRelHomomorphism_200 (coe v0))
      (coe d_'8744''45'homo_204 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism
d_IsLatticeMonomorphism_216 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsLatticeMonomorphism_216
  = C_IsLatticeMonomorphism'46'constructor_3341 T_IsLatticeHomomorphism_190
                                                (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism.isLatticeHomomorphism
d_isLatticeHomomorphism_224 ::
  T_IsLatticeMonomorphism_216 -> T_IsLatticeHomomorphism_190
d_isLatticeHomomorphism_224 v0
  = case coe v0 of
      C_IsLatticeMonomorphism'46'constructor_3341 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism.injective
d_injective_226 ::
  T_IsLatticeMonomorphism_216 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_226 v0
  = case coe v0 of
      C_IsLatticeMonomorphism'46'constructor_3341 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism._.isRelHomomorphism
d_isRelHomomorphism_230 ::
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_230 v0
  = coe
      d_isRelHomomorphism_200 (coe d_isLatticeHomomorphism_224 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism._.∧-homo
d_'8743''45'homo_232 ::
  T_IsLatticeMonomorphism_216 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'homo_232 v0
  = coe
      d_'8743''45'homo_202 (coe d_isLatticeHomomorphism_224 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism._.∧-isMagmaHomomorphism
d_'8743''45'isMagmaHomomorphism_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_'8743''45'isMagmaHomomorphism_234 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8743''45'isMagmaHomomorphism_234 v7
du_'8743''45'isMagmaHomomorphism_234 ::
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
du_'8743''45'isMagmaHomomorphism_234 v0
  = coe
      du_'8743''45'isMagmaHomomorphism_210
      (coe d_isLatticeHomomorphism_224 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism._.∨-homo
d_'8744''45'homo_236 ::
  T_IsLatticeMonomorphism_216 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'homo_236 v0
  = coe
      d_'8744''45'homo_204 (coe d_isLatticeHomomorphism_224 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism._.∨-isMagmaHomomorphism
d_'8744''45'isMagmaHomomorphism_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_'8744''45'isMagmaHomomorphism_238 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8744''45'isMagmaHomomorphism_238 v7
du_'8744''45'isMagmaHomomorphism_238 ::
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
du_'8744''45'isMagmaHomomorphism_238 v0
  = coe
      du_'8744''45'isMagmaHomomorphism_212
      (coe d_isLatticeHomomorphism_224 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism._.cong
d_cong_240 ::
  T_IsLatticeMonomorphism_216 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_240 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_200 (coe d_isLatticeHomomorphism_224 (coe v0)))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism.∨-isMagmaMonomorphism
d_'8744''45'isMagmaMonomorphism_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94
d_'8744''45'isMagmaMonomorphism_242 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8744''45'isMagmaMonomorphism_242 v7
du_'8744''45'isMagmaMonomorphism_242 ::
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94
du_'8744''45'isMagmaMonomorphism_242 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMagmaMonomorphism'46'constructor_1881
      (coe
         du_'8744''45'isMagmaHomomorphism_212
         (coe d_isLatticeHomomorphism_224 (coe v0)))
      (coe d_injective_226 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism.∧-isMagmaMonomorphism
d_'8743''45'isMagmaMonomorphism_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94
d_'8743''45'isMagmaMonomorphism_244 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8743''45'isMagmaMonomorphism_244 v7
du_'8743''45'isMagmaMonomorphism_244 ::
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94
du_'8743''45'isMagmaMonomorphism_244 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMagmaMonomorphism'46'constructor_1881
      (coe
         du_'8743''45'isMagmaHomomorphism_210
         (coe d_isLatticeHomomorphism_224 (coe v0)))
      (coe d_injective_226 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeMonomorphism._.isRelMonomorphism
d_isRelMonomorphism_248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_248 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_248 v7
du_isRelMonomorphism_248 ::
  T_IsLatticeMonomorphism_216 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_248 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.du_isRelMonomorphism_114
      (coe du_'8743''45'isMagmaMonomorphism_244 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism
d_IsLatticeIsomorphism_252 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsLatticeIsomorphism_252
  = C_IsLatticeIsomorphism'46'constructor_4741 T_IsLatticeMonomorphism_216
                                               (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism.isLatticeMonomorphism
d_isLatticeMonomorphism_260 ::
  T_IsLatticeIsomorphism_252 -> T_IsLatticeMonomorphism_216
d_isLatticeMonomorphism_260 v0
  = case coe v0 of
      C_IsLatticeIsomorphism'46'constructor_4741 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism.surjective
d_surjective_262 ::
  T_IsLatticeIsomorphism_252 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_262 v0
  = case coe v0 of
      C_IsLatticeIsomorphism'46'constructor_4741 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.injective
d_injective_266 ::
  T_IsLatticeIsomorphism_252 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_266 v0
  = coe d_injective_226 (coe d_isLatticeMonomorphism_260 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.isLatticeHomomorphism
d_isLatticeHomomorphism_268 ::
  T_IsLatticeIsomorphism_252 -> T_IsLatticeHomomorphism_190
d_isLatticeHomomorphism_268 v0
  = coe
      d_isLatticeHomomorphism_224
      (coe d_isLatticeMonomorphism_260 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.isRelHomomorphism
d_isRelHomomorphism_270 ::
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_270 v0
  = coe
      d_isRelHomomorphism_200
      (coe
         d_isLatticeHomomorphism_224
         (coe d_isLatticeMonomorphism_260 (coe v0)))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.isRelMonomorphism
d_isRelMonomorphism_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_272 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_272 v7
du_isRelMonomorphism_272 ::
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_272 v0
  = let v1 = d_isLatticeMonomorphism_260 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Morphism.Structures.du_isRelMonomorphism_114
      (coe du_'8743''45'isMagmaMonomorphism_244 (coe v1))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.∧-homo
d_'8743''45'homo_274 ::
  T_IsLatticeIsomorphism_252 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'homo_274 v0
  = coe
      d_'8743''45'homo_202
      (coe
         d_isLatticeHomomorphism_224
         (coe d_isLatticeMonomorphism_260 (coe v0)))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.∧-isMagmaHomomorphism
d_'8743''45'isMagmaHomomorphism_276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_'8743''45'isMagmaHomomorphism_276 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8743''45'isMagmaHomomorphism_276 v7
du_'8743''45'isMagmaHomomorphism_276 ::
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
du_'8743''45'isMagmaHomomorphism_276 v0
  = let v1 = d_isLatticeMonomorphism_260 (coe v0) in
    coe
      du_'8743''45'isMagmaHomomorphism_210
      (coe d_isLatticeHomomorphism_224 (coe v1))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.∧-isMagmaMonomorphism
d_'8743''45'isMagmaMonomorphism_278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94
d_'8743''45'isMagmaMonomorphism_278 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8743''45'isMagmaMonomorphism_278 v7
du_'8743''45'isMagmaMonomorphism_278 ::
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94
du_'8743''45'isMagmaMonomorphism_278 v0
  = coe
      du_'8743''45'isMagmaMonomorphism_244
      (coe d_isLatticeMonomorphism_260 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.∨-homo
d_'8744''45'homo_280 ::
  T_IsLatticeIsomorphism_252 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'homo_280 v0
  = coe
      d_'8744''45'homo_204
      (coe
         d_isLatticeHomomorphism_224
         (coe d_isLatticeMonomorphism_260 (coe v0)))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.∨-isMagmaHomomorphism
d_'8744''45'isMagmaHomomorphism_282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
d_'8744''45'isMagmaHomomorphism_282 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8744''45'isMagmaHomomorphism_282 v7
du_'8744''45'isMagmaHomomorphism_282 ::
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaHomomorphism_76
du_'8744''45'isMagmaHomomorphism_282 v0
  = let v1 = d_isLatticeMonomorphism_260 (coe v0) in
    coe
      du_'8744''45'isMagmaHomomorphism_212
      (coe d_isLatticeHomomorphism_224 (coe v1))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.∨-isMagmaMonomorphism
d_'8744''45'isMagmaMonomorphism_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94
d_'8744''45'isMagmaMonomorphism_284 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8744''45'isMagmaMonomorphism_284 v7
du_'8744''45'isMagmaMonomorphism_284 ::
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaMonomorphism_94
du_'8744''45'isMagmaMonomorphism_284 v0
  = coe
      du_'8744''45'isMagmaMonomorphism_242
      (coe d_isLatticeMonomorphism_260 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.cong
d_cong_286 ::
  T_IsLatticeIsomorphism_252 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_286 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_200
         (coe
            d_isLatticeHomomorphism_224
            (coe d_isLatticeMonomorphism_260 (coe v0))))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism.∨-isMagmaIsomorphism
d_'8744''45'isMagmaIsomorphism_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118
d_'8744''45'isMagmaIsomorphism_288 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8744''45'isMagmaIsomorphism_288 v7
du_'8744''45'isMagmaIsomorphism_288 ::
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118
du_'8744''45'isMagmaIsomorphism_288 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMagmaIsomorphism'46'constructor_3015
      (coe
         du_'8744''45'isMagmaMonomorphism_242
         (coe d_isLatticeMonomorphism_260 (coe v0)))
      (coe d_surjective_262 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism.∧-isMagmaIsomorphism
d_'8743''45'isMagmaIsomorphism_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118
d_'8743''45'isMagmaIsomorphism_290 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8743''45'isMagmaIsomorphism_290 v7
du_'8743''45'isMagmaIsomorphism_290 ::
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Algebra.Morphism.Structures.T_IsMagmaIsomorphism_118
du_'8743''45'isMagmaIsomorphism_290 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.C_IsMagmaIsomorphism'46'constructor_3015
      (coe
         du_'8743''45'isMagmaMonomorphism_244
         (coe d_isLatticeMonomorphism_260 (coe v0)))
      (coe d_surjective_262 (coe v0))
-- Algebra.Lattice.Morphism.Structures.LatticeMorphisms.IsLatticeIsomorphism._.isRelIsomorphism
d_isRelIsomorphism_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_294 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_294 v7
du_isRelIsomorphism_294 ::
  T_IsLatticeIsomorphism_252 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_294 v0
  = coe
      MAlonzo.Code.Algebra.Morphism.Structures.du_isRelIsomorphism_144
      (coe du_'8743''45'isMagmaIsomorphism_290 (coe v0))
