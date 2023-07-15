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

module MAlonzo.Code.Algebra.Morphism.Structures where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Relation.Binary.Morphism.Structures

-- Algebra.Morphism.Structures.MagmaMorphisms._._∙_
d__'8729'__30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__30 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'8729'__30 v4
du__'8729'__30 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8729'__30 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__26 (coe v0)
-- Algebra.Morphism.Structures.MagmaMorphisms._._≈_
d__'8776'__32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__32 = erased
-- Algebra.Morphism.Structures.MagmaMorphisms._.Carrier
d_Carrier_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 -> ()
d_Carrier_36 = erased
-- Algebra.Morphism.Structures.MagmaMorphisms._.Homomorphic₂
d_Homomorphic'8322'_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8322'_54 = erased
-- Algebra.Morphism.Structures.MagmaMorphisms._.Injective
d_Injective_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_64 = erased
-- Algebra.Morphism.Structures.MagmaMorphisms._.Surjective
d_Surjective_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_72 = erased
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaHomomorphism
d_IsMagmaHomomorphism_76 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsMagmaHomomorphism_76
  = C_IsMagmaHomomorphism'46'constructor_1049 MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
                                              (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_84 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_84 v0
  = case coe v0 of
      C_IsMagmaHomomorphism'46'constructor_1049 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaHomomorphism.homo
d_homo_86 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_86 v0
  = case coe v0 of
      C_IsMagmaHomomorphism'46'constructor_1049 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaHomomorphism._.cong
d_cong_90 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_90 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaMonomorphism
d_IsMagmaMonomorphism_94 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsMagmaMonomorphism_94
  = C_IsMagmaMonomorphism'46'constructor_1881 T_IsMagmaHomomorphism_76
                                              (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_102 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_102 v0
  = case coe v0 of
      C_IsMagmaMonomorphism'46'constructor_1881 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaMonomorphism.injective
d_injective_104 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_104 v0
  = case coe v0 of
      C_IsMagmaMonomorphism'46'constructor_1881 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaMonomorphism._.homo
d_homo_108 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_108 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaMonomorphism._.isRelHomomorphism
d_isRelHomomorphism_110 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_110 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaMonomorphism._.cong
d_cong_112 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_112 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_114 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_114 v7
du_isRelMonomorphism_114 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_114 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsRelMonomorphism'46'constructor_1555
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
      (coe d_injective_104 (coe v0))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaIsomorphism
d_IsMagmaIsomorphism_118 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsMagmaIsomorphism_118
  = C_IsMagmaIsomorphism'46'constructor_3015 T_IsMagmaMonomorphism_94
                                             (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_126 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_126 v0
  = case coe v0 of
      C_IsMagmaIsomorphism'46'constructor_3015 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaIsomorphism.surjective
d_surjective_128 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_128 v0
  = case coe v0 of
      C_IsMagmaIsomorphism'46'constructor_3015 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaIsomorphism._.homo
d_homo_132 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_132 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaIsomorphism._.injective
d_injective_134 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_134 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaIsomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_136 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_136 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaIsomorphism._.isRelHomomorphism
d_isRelHomomorphism_138 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_138 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaIsomorphism._.isRelMonomorphism
d_isRelMonomorphism_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_140 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_140 v7
du_isRelMonomorphism_140 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_140 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaIsomorphism._.cong
d_cong_142 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_142 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.MagmaMorphisms.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_144 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_144 v7
du_isRelIsomorphism_144 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_144 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsRelIsomorphism'46'constructor_3001
      (coe
         du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0)))
      (coe d_surjective_128 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms._.Carrier
d_Carrier_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 -> ()
d_Carrier_168 = erased
-- Algebra.Morphism.Structures.MonoidMorphisms._.ε
d_ε_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 -> AgdaAny
d_ε_172 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_ε_172 v4
du_ε_172 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 -> AgdaAny
du_ε_172 v0 = coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)
-- Algebra.Morphism.Structures.MonoidMorphisms._.Homomorphic₀
d_Homomorphic'8320'_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_Homomorphic'8320'_190 = erased
-- Algebra.Morphism.Structures.MonoidMorphisms._.Injective
d_Injective_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_204 = erased
-- Algebra.Morphism.Structures.MonoidMorphisms._.Surjective
d_Surjective_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_212 = erased
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaHomomorphism
d_IsMagmaHomomorphism_216 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaIsomorphism
d_IsMagmaIsomorphism_218 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaMonomorphism
d_IsMagmaMonomorphism_220 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaHomomorphism.homo
d_homo_224 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_224 v0 = coe d_homo_86 (coe v0)
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_226 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_226 v0 = coe d_isRelHomomorphism_84 (coe v0)
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaHomomorphism.cong
d_cong_228 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_228 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaIsomorphism.homo
d_homo_232 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_232 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaIsomorphism.injective
d_injective_234 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_234 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_236 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_236 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_238 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_238 v0
  = coe d_isMagmaMonomorphism_126 (coe v0)
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_240 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_240 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_242 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_242
du_isRelIsomorphism_242 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_242 v0 v1 = coe du_isRelIsomorphism_144 v1
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_244 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_244 v7
du_isRelMonomorphism_244 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_244 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaIsomorphism.surjective
d_surjective_246 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_246 v0 = coe d_surjective_128 (coe v0)
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaIsomorphism.cong
d_cong_248 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_248 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaMonomorphism.homo
d_homo_252 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_252 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaMonomorphism.injective
d_injective_254 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_254 v0 = coe d_injective_104 (coe v0)
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_256 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_256 v0
  = coe d_isMagmaHomomorphism_102 (coe v0)
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_258 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_258 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_260 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_260
du_isRelMonomorphism_260 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_260 v0 v1 = coe du_isRelMonomorphism_114 v1
-- Algebra.Morphism.Structures.MonoidMorphisms._.IsMagmaMonomorphism.cong
d_cong_262 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_262 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidHomomorphism
d_IsMonoidHomomorphism_266 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsMonoidHomomorphism_266
  = C_IsMonoidHomomorphism'46'constructor_5533 T_IsMagmaHomomorphism_76
                                               AgdaAny
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidHomomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_274 ::
  T_IsMonoidHomomorphism_266 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_274 v0
  = case coe v0 of
      C_IsMonoidHomomorphism'46'constructor_5533 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidHomomorphism.ε-homo
d_ε'45'homo_276 :: T_IsMonoidHomomorphism_266 -> AgdaAny
d_ε'45'homo_276 v0
  = case coe v0 of
      C_IsMonoidHomomorphism'46'constructor_5533 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidHomomorphism._.homo
d_homo_280 ::
  T_IsMonoidHomomorphism_266 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_280 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_274 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidHomomorphism._.isRelHomomorphism
d_isRelHomomorphism_282 ::
  T_IsMonoidHomomorphism_266 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_282 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_274 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidHomomorphism._.cong
d_cong_284 ::
  T_IsMonoidHomomorphism_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_284 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_274 (coe v0)))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidMonomorphism
d_IsMonoidMonomorphism_288 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsMonoidMonomorphism_288
  = C_IsMonoidMonomorphism'46'constructor_6057 T_IsMonoidHomomorphism_266
                                               (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidMonomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_296 ::
  T_IsMonoidMonomorphism_288 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_296 v0
  = case coe v0 of
      C_IsMonoidMonomorphism'46'constructor_6057 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidMonomorphism.injective
d_injective_298 ::
  T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_298 v0
  = case coe v0 of
      C_IsMonoidMonomorphism'46'constructor_6057 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidMonomorphism._.homo
d_homo_302 ::
  T_IsMonoidMonomorphism_288 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_302 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidMonomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_304 ::
  T_IsMonoidMonomorphism_288 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_304 v0
  = coe
      d_isMagmaHomomorphism_274 (coe d_isMonoidHomomorphism_296 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidMonomorphism._.isRelHomomorphism
d_isRelHomomorphism_306 ::
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_306 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidMonomorphism._.ε-homo
d_ε'45'homo_308 :: T_IsMonoidMonomorphism_288 -> AgdaAny
d_ε'45'homo_308 v0
  = coe d_ε'45'homo_276 (coe d_isMonoidHomomorphism_296 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidMonomorphism._.cong
d_cong_310 ::
  T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_310 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_isMonoidHomomorphism_296 (coe v0))))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidMonomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_312 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_312 v7
du_isMagmaMonomorphism_312 ::
  T_IsMonoidMonomorphism_288 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_312 v0
  = coe
      C_IsMagmaMonomorphism'46'constructor_1881
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
      (coe d_injective_298 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidMonomorphism._.isRelMonomorphism
d_isRelMonomorphism_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_316 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_316 v7
du_isRelMonomorphism_316 ::
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_316 v0
  = coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism
d_IsMonoidIsomorphism_320 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsMonoidIsomorphism_320
  = C_IsMonoidIsomorphism'46'constructor_7115 T_IsMonoidMonomorphism_288
                                              (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism.isMonoidMonomorphism
d_isMonoidMonomorphism_328 ::
  T_IsMonoidIsomorphism_320 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_328 v0
  = case coe v0 of
      C_IsMonoidIsomorphism'46'constructor_7115 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism.surjective
d_surjective_330 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_330 v0
  = case coe v0 of
      C_IsMonoidIsomorphism'46'constructor_7115 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism._.homo
d_homo_334 ::
  T_IsMonoidIsomorphism_320 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_334 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_296
            (coe d_isMonoidMonomorphism_328 (coe v0))))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism._.injective
d_injective_336 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_336 v0
  = coe d_injective_298 (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_338 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_338 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_296
         (coe d_isMonoidMonomorphism_328 (coe v0)))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism._.isMagmaMonomorphism
d_isMagmaMonomorphism_340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_340 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_340 v7
du_isMagmaMonomorphism_340 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_340 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism._.isMonoidHomomorphism
d_isMonoidHomomorphism_342 ::
  T_IsMonoidIsomorphism_320 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_342 v0
  = coe
      d_isMonoidHomomorphism_296
      (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism._.isRelHomomorphism
d_isRelHomomorphism_344 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_344 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_296
            (coe d_isMonoidMonomorphism_328 (coe v0))))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism._.isRelMonomorphism
d_isRelMonomorphism_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_346 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_346 v7
du_isRelMonomorphism_346 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_346 v0
  = let v1 = d_isMonoidMonomorphism_328 (coe v0) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v1))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism._.ε-homo
d_ε'45'homo_348 :: T_IsMonoidIsomorphism_320 -> AgdaAny
d_ε'45'homo_348 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_296
         (coe d_isMonoidMonomorphism_328 (coe v0)))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism._.cong
d_cong_350 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_350 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_296
               (coe d_isMonoidMonomorphism_328 (coe v0)))))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism.isMagmaIsomorphism
d_isMagmaIsomorphism_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_352 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaIsomorphism_352 v7
du_isMagmaIsomorphism_352 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_352 v0
  = coe
      C_IsMagmaIsomorphism'46'constructor_3015
      (coe
         du_isMagmaMonomorphism_312
         (coe d_isMonoidMonomorphism_328 (coe v0)))
      (coe d_surjective_330 (coe v0))
-- Algebra.Morphism.Structures.MonoidMorphisms.IsMonoidIsomorphism._.isRelIsomorphism
d_isRelIsomorphism_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_356 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_356 v7
du_isRelIsomorphism_356 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_356 v0
  = coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._._⁻¹
d__'8315''185'_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  AgdaAny -> AgdaAny
d__'8315''185'_374 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'8315''185'_374 v4
du__'8315''185'_374 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  AgdaAny -> AgdaAny
du__'8315''185'_374 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8315''185'_94 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.Carrier
d_Carrier_382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 -> ()
d_Carrier_382 = erased
-- Algebra.Morphism.Structures.GroupMorphisms._.Homomorphic₁
d_Homomorphic'8321'_412 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8321'_412 = erased
-- Algebra.Morphism.Structures.GroupMorphisms._.Injective
d_Injective_424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_424 = erased
-- Algebra.Morphism.Structures.GroupMorphisms._.Surjective
d_Surjective_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_432 = erased
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaHomomorphism
d_IsMagmaHomomorphism_436 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaIsomorphism
d_IsMagmaIsomorphism_438 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaMonomorphism
d_IsMagmaMonomorphism_440 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaHomomorphism.homo
d_homo_444 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_444 v0 = coe d_homo_86 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_446 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_446 v0 = coe d_isRelHomomorphism_84 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaHomomorphism.cong
d_cong_448 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_448 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaIsomorphism.homo
d_homo_452 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_452 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaIsomorphism.injective
d_injective_454 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_454 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_456 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_456 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_458 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_458 v0
  = coe d_isMagmaMonomorphism_126 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_460 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_460 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_462 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_462
du_isRelIsomorphism_462 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_462 v0 v1 = coe du_isRelIsomorphism_144 v1
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_464 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_464 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_464 v7
du_isRelMonomorphism_464 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_464 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaIsomorphism.surjective
d_surjective_466 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_466 v0 = coe d_surjective_128 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaIsomorphism.cong
d_cong_468 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_468 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaMonomorphism.homo
d_homo_472 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_472 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaMonomorphism.injective
d_injective_474 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_474 v0 = coe d_injective_104 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_476 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_476 v0
  = coe d_isMagmaHomomorphism_102 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_478 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_478 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_480 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_480
du_isRelMonomorphism_480 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_480 v0 v1 = coe du_isRelMonomorphism_114 v1
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMagmaMonomorphism.cong
d_cong_482 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_482 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidHomomorphism
d_IsMonoidHomomorphism_486 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism
d_IsMonoidIsomorphism_488 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidMonomorphism
d_IsMonoidMonomorphism_490 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidHomomorphism.homo
d_homo_494 ::
  T_IsMonoidHomomorphism_266 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_494 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_274 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidHomomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_496 ::
  T_IsMonoidHomomorphism_266 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_496 v0
  = coe d_isMagmaHomomorphism_274 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidHomomorphism.isRelHomomorphism
d_isRelHomomorphism_498 ::
  T_IsMonoidHomomorphism_266 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_498 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_274 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidHomomorphism.ε-homo
d_ε'45'homo_500 :: T_IsMonoidHomomorphism_266 -> AgdaAny
d_ε'45'homo_500 v0 = coe d_ε'45'homo_276 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidHomomorphism.cong
d_cong_502 ::
  T_IsMonoidHomomorphism_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_502 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_274 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.homo
d_homo_506 ::
  T_IsMonoidIsomorphism_320 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_506 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_296
            (coe d_isMonoidMonomorphism_328 (coe v0))))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.injective
d_injective_508 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_508 v0
  = coe d_injective_298 (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_510 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_510 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_296
         (coe d_isMonoidMonomorphism_328 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.isMagmaIsomorphism
d_isMagmaIsomorphism_512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_512 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMagmaIsomorphism_512
du_isMagmaIsomorphism_512 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_512 v0 v1 = coe du_isMagmaIsomorphism_352 v1
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_514 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_514 v7
du_isMagmaMonomorphism_514 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_514 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_516 ::
  T_IsMonoidIsomorphism_320 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_516 v0
  = coe
      d_isMonoidHomomorphism_296
      (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.isMonoidMonomorphism
d_isMonoidMonomorphism_518 ::
  T_IsMonoidIsomorphism_320 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_518 v0
  = coe d_isMonoidMonomorphism_328 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.isRelHomomorphism
d_isRelHomomorphism_520 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_520 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_296
            (coe d_isMonoidMonomorphism_328 (coe v0))))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.isRelIsomorphism
d_isRelIsomorphism_522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_522 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_522 v7
du_isRelIsomorphism_522 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_522 v0
  = coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.isRelMonomorphism
d_isRelMonomorphism_524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_524 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_524 v7
du_isRelMonomorphism_524 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_524 v0
  = let v1 = d_isMonoidMonomorphism_328 (coe v0) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v1))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.surjective
d_surjective_526 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_526 v0 = coe d_surjective_330 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.ε-homo
d_ε'45'homo_528 :: T_IsMonoidIsomorphism_320 -> AgdaAny
d_ε'45'homo_528 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_296
         (coe d_isMonoidMonomorphism_328 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidIsomorphism.cong
d_cong_530 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_530 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_296
               (coe d_isMonoidMonomorphism_328 (coe v0)))))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidMonomorphism.homo
d_homo_534 ::
  T_IsMonoidMonomorphism_288 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_534 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidMonomorphism.injective
d_injective_536 ::
  T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_536 v0 = coe d_injective_298 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_538 ::
  T_IsMonoidMonomorphism_288 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_538 v0
  = coe
      d_isMagmaHomomorphism_274 (coe d_isMonoidHomomorphism_296 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidMonomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_540 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_540 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMagmaMonomorphism_540
du_isMagmaMonomorphism_540 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_540 v0 v1
  = coe du_isMagmaMonomorphism_312 v1
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidMonomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_542 ::
  T_IsMonoidMonomorphism_288 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_542 v0
  = coe d_isMonoidHomomorphism_296 (coe v0)
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidMonomorphism.isRelHomomorphism
d_isRelHomomorphism_544 ::
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_544 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidMonomorphism.isRelMonomorphism
d_isRelMonomorphism_546 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_546 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_546 v7
du_isRelMonomorphism_546 ::
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_546 v0
  = coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidMonomorphism.ε-homo
d_ε'45'homo_548 :: T_IsMonoidMonomorphism_288 -> AgdaAny
d_ε'45'homo_548 v0
  = coe d_ε'45'homo_276 (coe d_isMonoidHomomorphism_296 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms._.IsMonoidMonomorphism.cong
d_cong_550 ::
  T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_550 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_isMonoidHomomorphism_296 (coe v0))))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupHomomorphism
d_IsGroupHomomorphism_554 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsGroupHomomorphism_554
  = C_IsGroupHomomorphism'46'constructor_10405 T_IsMonoidHomomorphism_266
                                               (AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupHomomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_562 ::
  T_IsGroupHomomorphism_554 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_562 v0
  = case coe v0 of
      C_IsGroupHomomorphism'46'constructor_10405 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupHomomorphism.⁻¹-homo
d_'8315''185''45'homo_564 ::
  T_IsGroupHomomorphism_554 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_564 v0
  = case coe v0 of
      C_IsGroupHomomorphism'46'constructor_10405 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupHomomorphism._.homo
d_homo_568 ::
  T_IsGroupHomomorphism_554 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_568 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_562 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupHomomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_570 ::
  T_IsGroupHomomorphism_554 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_570 v0
  = coe
      d_isMagmaHomomorphism_274 (coe d_isMonoidHomomorphism_562 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupHomomorphism._.isRelHomomorphism
d_isRelHomomorphism_572 ::
  T_IsGroupHomomorphism_554 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_572 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_562 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupHomomorphism._.ε-homo
d_ε'45'homo_574 :: T_IsGroupHomomorphism_554 -> AgdaAny
d_ε'45'homo_574 v0
  = coe d_ε'45'homo_276 (coe d_isMonoidHomomorphism_562 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupHomomorphism._.cong
d_cong_576 ::
  T_IsGroupHomomorphism_554 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_576 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_isMonoidHomomorphism_562 (coe v0))))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism
d_IsGroupMonomorphism_580 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsGroupMonomorphism_580
  = C_IsGroupMonomorphism'46'constructor_11055 T_IsGroupHomomorphism_554
                                               (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism.isGroupHomomorphism
d_isGroupHomomorphism_588 ::
  T_IsGroupMonomorphism_580 -> T_IsGroupHomomorphism_554
d_isGroupHomomorphism_588 v0
  = case coe v0 of
      C_IsGroupMonomorphism'46'constructor_11055 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism.injective
d_injective_590 ::
  T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_590 v0
  = case coe v0 of
      C_IsGroupMonomorphism'46'constructor_11055 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_594 ::
  T_IsGroupMonomorphism_580 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_594 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_562
         (coe d_isGroupHomomorphism_588 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism._.isMonoidHomomorphism
d_isMonoidHomomorphism_596 ::
  T_IsGroupMonomorphism_580 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_596 v0
  = coe
      d_isMonoidHomomorphism_562 (coe d_isGroupHomomorphism_588 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism._.isRelHomomorphism
d_isRelHomomorphism_598 ::
  T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_598 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe d_isGroupHomomorphism_588 (coe v0))))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism._.ε-homo
d_ε'45'homo_600 :: T_IsGroupMonomorphism_580 -> AgdaAny
d_ε'45'homo_600 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_562
         (coe d_isGroupHomomorphism_588 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism._.⁻¹-homo
d_'8315''185''45'homo_602 ::
  T_IsGroupMonomorphism_580 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_602 v0
  = coe
      d_'8315''185''45'homo_564 (coe d_isGroupHomomorphism_588 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism._.homo
d_homo_604 ::
  T_IsGroupMonomorphism_580 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_604 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe d_isGroupHomomorphism_588 (coe v0))))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism._.cong
d_cong_606 ::
  T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_606 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_562
               (coe d_isGroupHomomorphism_588 (coe v0)))))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism.isMonoidMonomorphism
d_isMonoidMonomorphism_608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_608 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMonoidMonomorphism_608 v7
du_isMonoidMonomorphism_608 ::
  T_IsGroupMonomorphism_580 -> T_IsMonoidMonomorphism_288
du_isMonoidMonomorphism_608 v0
  = coe
      C_IsMonoidMonomorphism'46'constructor_6057
      (coe
         d_isMonoidHomomorphism_562
         (coe d_isGroupHomomorphism_588 (coe v0)))
      (coe d_injective_590 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism._.isMagmaMonomorphism
d_isMagmaMonomorphism_612 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_612 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_612 v7
du_isMagmaMonomorphism_612 ::
  T_IsGroupMonomorphism_580 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_612 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe du_isMonoidMonomorphism_608 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupMonomorphism._.isRelMonomorphism
d_isRelMonomorphism_614 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_614 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_614 v7
du_isRelMonomorphism_614 ::
  T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_614 v0
  = let v1 = coe du_isMonoidMonomorphism_608 (coe v0) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v1))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism
d_IsGroupIsomorphism_618 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsGroupIsomorphism_618
  = C_IsGroupIsomorphism'46'constructor_12289 T_IsGroupMonomorphism_580
                                              (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism.isGroupMonomorphism
d_isGroupMonomorphism_626 ::
  T_IsGroupIsomorphism_618 -> T_IsGroupMonomorphism_580
d_isGroupMonomorphism_626 v0
  = case coe v0 of
      C_IsGroupIsomorphism'46'constructor_12289 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism.surjective
d_surjective_628 ::
  T_IsGroupIsomorphism_618 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_628 v0
  = case coe v0 of
      C_IsGroupIsomorphism'46'constructor_12289 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.injective
d_injective_632 ::
  T_IsGroupIsomorphism_618 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_632 v0
  = coe d_injective_590 (coe d_isGroupMonomorphism_626 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.isGroupHomomorphism
d_isGroupHomomorphism_634 ::
  T_IsGroupIsomorphism_618 -> T_IsGroupHomomorphism_554
d_isGroupHomomorphism_634 v0
  = coe
      d_isGroupHomomorphism_588 (coe d_isGroupMonomorphism_626 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_636 ::
  T_IsGroupIsomorphism_618 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_636 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_562
         (coe
            d_isGroupHomomorphism_588
            (coe d_isGroupMonomorphism_626 (coe v0))))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.isMagmaMonomorphism
d_isMagmaMonomorphism_638 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_638 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_638 v7
du_isMagmaMonomorphism_638 ::
  T_IsGroupIsomorphism_618 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_638 v0
  = let v1 = d_isGroupMonomorphism_626 (coe v0) in
    coe
      du_isMagmaMonomorphism_312
      (coe du_isMonoidMonomorphism_608 (coe v1))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.isMonoidHomomorphism
d_isMonoidHomomorphism_640 ::
  T_IsGroupIsomorphism_618 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_640 v0
  = coe
      d_isMonoidHomomorphism_562
      (coe
         d_isGroupHomomorphism_588 (coe d_isGroupMonomorphism_626 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.isMonoidMonomorphism
d_isMonoidMonomorphism_642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_642 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMonoidMonomorphism_642 v7
du_isMonoidMonomorphism_642 ::
  T_IsGroupIsomorphism_618 -> T_IsMonoidMonomorphism_288
du_isMonoidMonomorphism_642 v0
  = coe
      du_isMonoidMonomorphism_608
      (coe d_isGroupMonomorphism_626 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.isRelHomomorphism
d_isRelHomomorphism_644 ::
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_644 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe
               d_isGroupHomomorphism_588
               (coe d_isGroupMonomorphism_626 (coe v0)))))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.isRelMonomorphism
d_isRelMonomorphism_646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_646 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_646 v7
du_isRelMonomorphism_646 ::
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_646 v0
  = let v1 = d_isGroupMonomorphism_626 (coe v0) in
    let v2 = coe du_isMonoidMonomorphism_608 (coe v1) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v2))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.ε-homo
d_ε'45'homo_648 :: T_IsGroupIsomorphism_618 -> AgdaAny
d_ε'45'homo_648 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_562
         (coe
            d_isGroupHomomorphism_588
            (coe d_isGroupMonomorphism_626 (coe v0))))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.⁻¹-homo
d_'8315''185''45'homo_650 ::
  T_IsGroupIsomorphism_618 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_650 v0
  = coe
      d_'8315''185''45'homo_564
      (coe
         d_isGroupHomomorphism_588 (coe d_isGroupMonomorphism_626 (coe v0)))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.homo
d_homo_652 ::
  T_IsGroupIsomorphism_618 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_652 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe
               d_isGroupHomomorphism_588
               (coe d_isGroupMonomorphism_626 (coe v0)))))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.cong
d_cong_654 ::
  T_IsGroupIsomorphism_618 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_654 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_562
               (coe
                  d_isGroupHomomorphism_588
                  (coe d_isGroupMonomorphism_626 (coe v0))))))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism.isMonoidIsomorphism
d_isMonoidIsomorphism_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMonoidIsomorphism_320
d_isMonoidIsomorphism_656 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMonoidIsomorphism_656 v7
du_isMonoidIsomorphism_656 ::
  T_IsGroupIsomorphism_618 -> T_IsMonoidIsomorphism_320
du_isMonoidIsomorphism_656 v0
  = coe
      C_IsMonoidIsomorphism'46'constructor_7115
      (coe
         du_isMonoidMonomorphism_608
         (coe d_isGroupMonomorphism_626 (coe v0)))
      (coe d_surjective_628 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.isMagmaIsomorphism
d_isMagmaIsomorphism_660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_660 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaIsomorphism_660 v7
du_isMagmaIsomorphism_660 ::
  T_IsGroupIsomorphism_618 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_660 v0
  = coe
      du_isMagmaIsomorphism_352 (coe du_isMonoidIsomorphism_656 (coe v0))
-- Algebra.Morphism.Structures.GroupMorphisms.IsGroupIsomorphism._.isRelIsomorphism
d_isRelIsomorphism_662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_662 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_662 v7
du_isRelIsomorphism_662 ::
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_662 v0
  = let v1 = coe du_isMonoidIsomorphism_656 (coe v0) in
    coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v1))
-- Algebra.Morphism.Structures.NearSemiringMorphisms._._*_
d__'42'__680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__680 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'42'__680 v4
du__'42'__680 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'42'__680 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__130 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms._.Carrier
d_Carrier_696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 -> ()
d_Carrier_696 = erased
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidHomomorphism
d_IsMonoidHomomorphism_720 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism
d_IsMonoidIsomorphism_722 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidMonomorphism
d_IsMonoidMonomorphism_724 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidHomomorphism.homo
d_homo_728 ::
  T_IsMonoidHomomorphism_266 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_728 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_274 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidHomomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_730 ::
  T_IsMonoidHomomorphism_266 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_730 v0
  = coe d_isMagmaHomomorphism_274 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidHomomorphism.isRelHomomorphism
d_isRelHomomorphism_732 ::
  T_IsMonoidHomomorphism_266 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_732 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_274 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidHomomorphism.ε-homo
d_ε'45'homo_734 :: T_IsMonoidHomomorphism_266 -> AgdaAny
d_ε'45'homo_734 v0 = coe d_ε'45'homo_276 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidHomomorphism.cong
d_cong_736 ::
  T_IsMonoidHomomorphism_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_736 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_274 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.homo
d_homo_740 ::
  T_IsMonoidIsomorphism_320 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_740 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_296
            (coe d_isMonoidMonomorphism_328 (coe v0))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.injective
d_injective_742 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_742 v0
  = coe d_injective_298 (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_744 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_744 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_296
         (coe d_isMonoidMonomorphism_328 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.isMagmaIsomorphism
d_isMagmaIsomorphism_746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_746 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMagmaIsomorphism_746
du_isMagmaIsomorphism_746 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_746 v0 v1 = coe du_isMagmaIsomorphism_352 v1
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_748 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_748 v7
du_isMagmaMonomorphism_748 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_748 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_750 ::
  T_IsMonoidIsomorphism_320 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_750 v0
  = coe
      d_isMonoidHomomorphism_296
      (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.isMonoidMonomorphism
d_isMonoidMonomorphism_752 ::
  T_IsMonoidIsomorphism_320 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_752 v0
  = coe d_isMonoidMonomorphism_328 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.isRelHomomorphism
d_isRelHomomorphism_754 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_754 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_296
            (coe d_isMonoidMonomorphism_328 (coe v0))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.isRelIsomorphism
d_isRelIsomorphism_756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_756 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_756 v7
du_isRelIsomorphism_756 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_756 v0
  = coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.isRelMonomorphism
d_isRelMonomorphism_758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_758 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_758 v7
du_isRelMonomorphism_758 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_758 v0
  = let v1 = d_isMonoidMonomorphism_328 (coe v0) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v1))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.surjective
d_surjective_760 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_760 v0 = coe d_surjective_330 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.ε-homo
d_ε'45'homo_762 :: T_IsMonoidIsomorphism_320 -> AgdaAny
d_ε'45'homo_762 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_296
         (coe d_isMonoidMonomorphism_328 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidIsomorphism.cong
d_cong_764 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_764 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_296
               (coe d_isMonoidMonomorphism_328 (coe v0)))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidMonomorphism.homo
d_homo_768 ::
  T_IsMonoidMonomorphism_288 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_768 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidMonomorphism.injective
d_injective_770 ::
  T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_770 v0 = coe d_injective_298 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_772 ::
  T_IsMonoidMonomorphism_288 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_772 v0
  = coe
      d_isMagmaHomomorphism_274 (coe d_isMonoidHomomorphism_296 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidMonomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_774 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_774 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMagmaMonomorphism_774
du_isMagmaMonomorphism_774 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_774 v0 v1
  = coe du_isMagmaMonomorphism_312 v1
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidMonomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_776 ::
  T_IsMonoidMonomorphism_288 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_776 v0
  = coe d_isMonoidHomomorphism_296 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidMonomorphism.isRelHomomorphism
d_isRelHomomorphism_778 ::
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_778 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidMonomorphism.isRelMonomorphism
d_isRelMonomorphism_780 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_780 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_780 v7
du_isRelMonomorphism_780 ::
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_780 v0
  = coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidMonomorphism.ε-homo
d_ε'45'homo_782 :: T_IsMonoidMonomorphism_288 -> AgdaAny
d_ε'45'homo_782 v0
  = coe d_ε'45'homo_276 (coe d_isMonoidHomomorphism_296 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.+.IsMonoidMonomorphism.cong
d_cong_784 ::
  T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_784 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_isMonoidHomomorphism_296 (coe v0))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaHomomorphism
d_IsMagmaHomomorphism_788 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaIsomorphism
d_IsMagmaIsomorphism_790 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaMonomorphism
d_IsMagmaMonomorphism_792 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaHomomorphism.homo
d_homo_796 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_796 v0 = coe d_homo_86 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_798 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_798 v0 = coe d_isRelHomomorphism_84 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaHomomorphism.cong
d_cong_800 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_800 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaIsomorphism.homo
d_homo_804 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_804 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaIsomorphism.injective
d_injective_806 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_806 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_808 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_808 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_810 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_810 v0
  = coe d_isMagmaMonomorphism_126 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_812 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_812 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_814 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_814
du_isRelIsomorphism_814 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_814 v0 v1 = coe du_isRelIsomorphism_144 v1
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_816 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_816 v7
du_isRelMonomorphism_816 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_816 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaIsomorphism.surjective
d_surjective_818 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_818 v0 = coe d_surjective_128 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaIsomorphism.cong
d_cong_820 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_820 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaMonomorphism.homo
d_homo_824 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_824 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaMonomorphism.injective
d_injective_826 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_826 v0 = coe d_injective_104 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_828 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_828 v0
  = coe d_isMagmaHomomorphism_102 (coe v0)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_830 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_830 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_832 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_832 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_832
du_isRelMonomorphism_832 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_832 v0 v1 = coe du_isRelMonomorphism_114 v1
-- Algebra.Morphism.Structures.NearSemiringMorphisms.*.IsMagmaMonomorphism.cong
d_cong_834 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_834 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms._.Homomorphic₂
d_Homomorphic'8322'_842 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8322'_842 = erased
-- Algebra.Morphism.Structures.NearSemiringMorphisms._.Injective
d_Injective_852 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_852 = erased
-- Algebra.Morphism.Structures.NearSemiringMorphisms._.Surjective
d_Surjective_860 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_860 = erased
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringHomomorphism
d_IsNearSemiringHomomorphism_864 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsNearSemiringHomomorphism_864
  = C_IsNearSemiringHomomorphism'46'constructor_15507 T_IsMonoidHomomorphism_266
                                                      (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringHomomorphism.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_872 ::
  T_IsNearSemiringHomomorphism_864 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_872 v0
  = case coe v0 of
      C_IsNearSemiringHomomorphism'46'constructor_15507 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringHomomorphism.*-homo
d_'42''45'homo_874 ::
  T_IsNearSemiringHomomorphism_864 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_874 v0
  = case coe v0 of
      C_IsNearSemiringHomomorphism'46'constructor_15507 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringHomomorphism._.homo
d_homo_878 ::
  T_IsNearSemiringHomomorphism_864 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_878 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe d_'43''45'isMonoidHomomorphism_872 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringHomomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_880 ::
  T_IsNearSemiringHomomorphism_864 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_880 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe d_'43''45'isMonoidHomomorphism_872 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringHomomorphism._.ε-homo
d_ε'45'homo_882 :: T_IsNearSemiringHomomorphism_864 -> AgdaAny
d_ε'45'homo_882 v0
  = coe
      d_ε'45'homo_276 (coe d_'43''45'isMonoidHomomorphism_872 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringHomomorphism._.isRelHomomorphism
d_isRelHomomorphism_884 ::
  T_IsNearSemiringHomomorphism_864 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_884 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe d_'43''45'isMonoidHomomorphism_872 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringHomomorphism._.cong
d_cong_886 ::
  T_IsNearSemiringHomomorphism_864 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_886 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_'43''45'isMonoidHomomorphism_872 (coe v0))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringHomomorphism.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_888 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringHomomorphism_864 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_888 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_888 v7
du_'42''45'isMagmaHomomorphism_888 ::
  T_IsNearSemiringHomomorphism_864 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_888 v0
  = coe
      C_IsMagmaHomomorphism'46'constructor_1049
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_'43''45'isMonoidHomomorphism_872 (coe v0))))
      (coe d_'42''45'homo_874 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism
d_IsNearSemiringMonomorphism_892 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsNearSemiringMonomorphism_892
  = C_IsNearSemiringMonomorphism'46'constructor_16335 T_IsNearSemiringHomomorphism_864
                                                      (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_900 ::
  T_IsNearSemiringMonomorphism_892 ->
  T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_900 v0
  = case coe v0 of
      C_IsNearSemiringMonomorphism'46'constructor_16335 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism.injective
d_injective_902 ::
  T_IsNearSemiringMonomorphism_892 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_902 v0
  = case coe v0 of
      C_IsNearSemiringMonomorphism'46'constructor_16335 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism._.*-homo
d_'42''45'homo_906 ::
  T_IsNearSemiringMonomorphism_892 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_906 v0
  = coe
      d_'42''45'homo_874 (coe d_isNearSemiringHomomorphism_900 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism._.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_908 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_908 v7
du_'42''45'isMagmaHomomorphism_908 ::
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_908 v0
  = coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_900 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism._.homo
d_homo_910 ::
  T_IsNearSemiringMonomorphism_892 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_910 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe d_isNearSemiringHomomorphism_900 (coe v0))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_912 ::
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_912 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe d_isNearSemiringHomomorphism_900 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism._.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_914 ::
  T_IsNearSemiringMonomorphism_892 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_914 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe d_isNearSemiringHomomorphism_900 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism._.ε-homo
d_ε'45'homo_916 :: T_IsNearSemiringMonomorphism_892 -> AgdaAny
d_ε'45'homo_916 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe d_isNearSemiringHomomorphism_900 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism._.isRelHomomorphism
d_isRelHomomorphism_918 ::
  T_IsNearSemiringMonomorphism_892 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_918 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe d_isNearSemiringHomomorphism_900 (coe v0))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism._.cong
d_cong_920 ::
  T_IsNearSemiringMonomorphism_892 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_920 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe d_isNearSemiringHomomorphism_900 (coe v0)))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism.+-isMonoidMonomorphism
d_'43''45'isMonoidMonomorphism_922 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 -> T_IsMonoidMonomorphism_288
d_'43''45'isMonoidMonomorphism_922 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isMonoidMonomorphism_922 v7
du_'43''45'isMonoidMonomorphism_922 ::
  T_IsNearSemiringMonomorphism_892 -> T_IsMonoidMonomorphism_288
du_'43''45'isMonoidMonomorphism_922 v0
  = coe
      C_IsMonoidMonomorphism'46'constructor_6057
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe d_isNearSemiringHomomorphism_900 (coe v0)))
      (coe d_injective_902 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism._.isMagmaMonomorphism
d_isMagmaMonomorphism_926 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_926 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_926 v7
du_isMagmaMonomorphism_926 ::
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_926 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe du_'43''45'isMonoidMonomorphism_922 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism._.isRelMonomorphism
d_isRelMonomorphism_928 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_928 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_928 v7
du_isRelMonomorphism_928 ::
  T_IsNearSemiringMonomorphism_892 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_928 v0
  = let v1 = coe du_'43''45'isMonoidMonomorphism_922 (coe v0) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v1))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringMonomorphism.*-isMagmaMonomorphism
d_'42''45'isMagmaMonomorphism_930 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaMonomorphism_94
d_'42''45'isMagmaMonomorphism_930 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaMonomorphism_930 v7
du_'42''45'isMagmaMonomorphism_930 ::
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaMonomorphism_94
du_'42''45'isMagmaMonomorphism_930 v0
  = coe
      C_IsMagmaMonomorphism'46'constructor_1881
      (coe
         du_'42''45'isMagmaHomomorphism_888
         (coe d_isNearSemiringHomomorphism_900 (coe v0)))
      (coe d_injective_902 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism
d_IsNearSemiringIsomorphism_934 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsNearSemiringIsomorphism_934
  = C_IsNearSemiringIsomorphism'46'constructor_17921 T_IsNearSemiringMonomorphism_892
                                                     (AgdaAny ->
                                                      MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism.isNearSemiringMonomorphism
d_isNearSemiringMonomorphism_942 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsNearSemiringMonomorphism_892
d_isNearSemiringMonomorphism_942 v0
  = case coe v0 of
      C_IsNearSemiringIsomorphism'46'constructor_17921 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism.surjective
d_surjective_944 ::
  T_IsNearSemiringIsomorphism_934 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_944 v0
  = case coe v0 of
      C_IsNearSemiringIsomorphism'46'constructor_17921 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.*-homo
d_'42''45'homo_948 ::
  T_IsNearSemiringIsomorphism_934 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_948 v0
  = coe
      d_'42''45'homo_874
      (coe
         d_isNearSemiringHomomorphism_900
         (coe d_isNearSemiringMonomorphism_942 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_950 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_950 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_950 v7
du_'42''45'isMagmaHomomorphism_950 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_950 v0
  = let v1 = d_isNearSemiringMonomorphism_942 (coe v0) in
    coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_900 (coe v1))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.*-isMagmaMonomorphism
d_'42''45'isMagmaMonomorphism_952 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaMonomorphism_94
d_'42''45'isMagmaMonomorphism_952 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaMonomorphism_952 v7
du_'42''45'isMagmaMonomorphism_952 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaMonomorphism_94
du_'42''45'isMagmaMonomorphism_952 v0
  = coe
      du_'42''45'isMagmaMonomorphism_930
      (coe d_isNearSemiringMonomorphism_942 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.homo
d_homo_954 ::
  T_IsNearSemiringIsomorphism_934 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_954 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_900
               (coe d_isNearSemiringMonomorphism_942 (coe v0)))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_956 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_956 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_900
            (coe d_isNearSemiringMonomorphism_942 (coe v0))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.isMagmaMonomorphism
d_isMagmaMonomorphism_958 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_958 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_958 v7
du_isMagmaMonomorphism_958 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_958 v0
  = let v1 = d_isNearSemiringMonomorphism_942 (coe v0) in
    coe
      du_isMagmaMonomorphism_312
      (coe du_'43''45'isMonoidMonomorphism_922 (coe v1))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_960 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_960 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe
         d_isNearSemiringHomomorphism_900
         (coe d_isNearSemiringMonomorphism_942 (coe v0)))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.+-isMonoidMonomorphism
d_'43''45'isMonoidMonomorphism_962 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMonoidMonomorphism_288
d_'43''45'isMonoidMonomorphism_962 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isMonoidMonomorphism_962 v7
du_'43''45'isMonoidMonomorphism_962 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMonoidMonomorphism_288
du_'43''45'isMonoidMonomorphism_962 v0
  = coe
      du_'43''45'isMonoidMonomorphism_922
      (coe d_isNearSemiringMonomorphism_942 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.ε-homo
d_ε'45'homo_964 :: T_IsNearSemiringIsomorphism_934 -> AgdaAny
d_ε'45'homo_964 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_900
            (coe d_isNearSemiringMonomorphism_942 (coe v0))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.injective
d_injective_966 ::
  T_IsNearSemiringIsomorphism_934 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_966 v0
  = coe
      d_injective_902 (coe d_isNearSemiringMonomorphism_942 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_968 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_968 v0
  = coe
      d_isNearSemiringHomomorphism_900
      (coe d_isNearSemiringMonomorphism_942 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.isRelHomomorphism
d_isRelHomomorphism_970 ::
  T_IsNearSemiringIsomorphism_934 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_970 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_900
               (coe d_isNearSemiringMonomorphism_942 (coe v0)))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.isRelMonomorphism
d_isRelMonomorphism_972 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_972 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_972 v7
du_isRelMonomorphism_972 ::
  T_IsNearSemiringIsomorphism_934 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_972 v0
  = let v1 = d_isNearSemiringMonomorphism_942 (coe v0) in
    let v2 = coe du_'43''45'isMonoidMonomorphism_922 (coe v1) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v2))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.cong
d_cong_974 ::
  T_IsNearSemiringIsomorphism_934 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_974 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe
                  d_isNearSemiringHomomorphism_900
                  (coe d_isNearSemiringMonomorphism_942 (coe v0))))))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism.+-isMonoidIsomorphism
d_'43''45'isMonoidIsomorphism_976 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMonoidIsomorphism_320
d_'43''45'isMonoidIsomorphism_976 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isMonoidIsomorphism_976 v7
du_'43''45'isMonoidIsomorphism_976 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMonoidIsomorphism_320
du_'43''45'isMonoidIsomorphism_976 v0
  = coe
      C_IsMonoidIsomorphism'46'constructor_7115
      (coe
         du_'43''45'isMonoidMonomorphism_922
         (coe d_isNearSemiringMonomorphism_942 (coe v0)))
      (coe d_surjective_944 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.isMagmaIsomorphism
d_isMagmaIsomorphism_980 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_980 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaIsomorphism_980 v7
du_isMagmaIsomorphism_980 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_980 v0
  = coe
      du_isMagmaIsomorphism_352
      (coe du_'43''45'isMonoidIsomorphism_976 (coe v0))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism._.isRelIsomorphism
d_isRelIsomorphism_982 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_982 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_982 v7
du_isRelIsomorphism_982 ::
  T_IsNearSemiringIsomorphism_934 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_982 v0
  = let v1 = coe du_'43''45'isMonoidIsomorphism_976 (coe v0) in
    coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v1))
-- Algebra.Morphism.Structures.NearSemiringMorphisms.IsNearSemiringIsomorphism.*-isMagmaIsomorphism
d_'42''45'isMagmaIsomorphism_984 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaIsomorphism_118
d_'42''45'isMagmaIsomorphism_984 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaIsomorphism_984 v7
du_'42''45'isMagmaIsomorphism_984 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaIsomorphism_118
du_'42''45'isMagmaIsomorphism_984 v0
  = coe
      C_IsMagmaIsomorphism'46'constructor_3015
      (coe
         du_'42''45'isMagmaMonomorphism_930
         (coe d_isNearSemiringMonomorphism_942 (coe v0)))
      (coe d_surjective_944 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.1#
d_1'35'_1020 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 -> AgdaAny
d_1'35'_1020 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_1'35'_1020 v4
du_1'35'_1020 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 -> AgdaAny
du_1'35'_1020 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_176 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms._.Carrier
d_Carrier_1022 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 -> ()
d_Carrier_1022 = erased
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidHomomorphism
d_IsMonoidHomomorphism_1054 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism
d_IsMonoidIsomorphism_1056 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidMonomorphism
d_IsMonoidMonomorphism_1058 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidHomomorphism.homo
d_homo_1062 ::
  T_IsMonoidHomomorphism_266 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1062 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_274 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidHomomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1064 ::
  T_IsMonoidHomomorphism_266 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1064 v0
  = coe d_isMagmaHomomorphism_274 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidHomomorphism.isRelHomomorphism
d_isRelHomomorphism_1066 ::
  T_IsMonoidHomomorphism_266 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1066 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_274 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidHomomorphism.ε-homo
d_ε'45'homo_1068 :: T_IsMonoidHomomorphism_266 -> AgdaAny
d_ε'45'homo_1068 v0 = coe d_ε'45'homo_276 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidHomomorphism.cong
d_cong_1070 ::
  T_IsMonoidHomomorphism_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1070 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_274 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.homo
d_homo_1074 ::
  T_IsMonoidIsomorphism_320 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1074 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_296
            (coe d_isMonoidMonomorphism_328 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.injective
d_injective_1076 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1076 v0
  = coe d_injective_298 (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1078 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1078 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_296
         (coe d_isMonoidMonomorphism_328 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.isMagmaIsomorphism
d_isMagmaIsomorphism_1080 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_1080 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMagmaIsomorphism_1080
du_isMagmaIsomorphism_1080 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_1080 v0 v1 = coe du_isMagmaIsomorphism_352 v1
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1082 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1082 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_1082 v7
du_isMagmaMonomorphism_1082 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1082 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_1084 ::
  T_IsMonoidIsomorphism_320 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1084 v0
  = coe
      d_isMonoidHomomorphism_296
      (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.isMonoidMonomorphism
d_isMonoidMonomorphism_1086 ::
  T_IsMonoidIsomorphism_320 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_1086 v0
  = coe d_isMonoidMonomorphism_328 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.isRelHomomorphism
d_isRelHomomorphism_1088 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1088 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_296
            (coe d_isMonoidMonomorphism_328 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.isRelIsomorphism
d_isRelIsomorphism_1090 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_1090 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_1090 v7
du_isRelIsomorphism_1090 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_1090 v0
  = coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.isRelMonomorphism
d_isRelMonomorphism_1092 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1092 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1092 v7
du_isRelMonomorphism_1092 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1092 v0
  = let v1 = d_isMonoidMonomorphism_328 (coe v0) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v1))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.surjective
d_surjective_1094 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_1094 v0 = coe d_surjective_330 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.ε-homo
d_ε'45'homo_1096 :: T_IsMonoidIsomorphism_320 -> AgdaAny
d_ε'45'homo_1096 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_296
         (coe d_isMonoidMonomorphism_328 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidIsomorphism.cong
d_cong_1098 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1098 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_296
               (coe d_isMonoidMonomorphism_328 (coe v0)))))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidMonomorphism.homo
d_homo_1102 ::
  T_IsMonoidMonomorphism_288 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1102 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidMonomorphism.injective
d_injective_1104 ::
  T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1104 v0 = coe d_injective_298 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1106 ::
  T_IsMonoidMonomorphism_288 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1106 v0
  = coe
      d_isMagmaHomomorphism_274 (coe d_isMonoidHomomorphism_296 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidMonomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1108 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMagmaMonomorphism_1108
du_isMagmaMonomorphism_1108 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1108 v0 v1
  = coe du_isMagmaMonomorphism_312 v1
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidMonomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_1110 ::
  T_IsMonoidMonomorphism_288 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1110 v0
  = coe d_isMonoidHomomorphism_296 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidMonomorphism.isRelHomomorphism
d_isRelHomomorphism_1112 ::
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1112 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidMonomorphism.isRelMonomorphism
d_isRelMonomorphism_1114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1114 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1114 v7
du_isRelMonomorphism_1114 ::
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1114 v0
  = coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidMonomorphism.ε-homo
d_ε'45'homo_1116 :: T_IsMonoidMonomorphism_288 -> AgdaAny
d_ε'45'homo_1116 v0
  = coe d_ε'45'homo_276 (coe d_isMonoidHomomorphism_296 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.*.IsMonoidMonomorphism.cong
d_cong_1118 ::
  T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1118 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_isMonoidHomomorphism_296 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms._.Homomorphic₀
d_Homomorphic'8320'_1122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_Homomorphic'8320'_1122 = erased
-- Algebra.Morphism.Structures.SemiringMorphisms._.Injective
d_Injective_1136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_1136 = erased
-- Algebra.Morphism.Structures.SemiringMorphisms._.Surjective
d_Surjective_1144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_1144 = erased
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringHomomorphism
d_IsNearSemiringHomomorphism_1148 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism
d_IsNearSemiringIsomorphism_1150 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism
d_IsNearSemiringMonomorphism_1152 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringHomomorphism.*-homo
d_'42''45'homo_1156 ::
  T_IsNearSemiringHomomorphism_864 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1156 v0 = coe d_'42''45'homo_874 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringHomomorphism.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_1158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringHomomorphism_864 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_1158 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'42''45'isMagmaHomomorphism_1158
du_'42''45'isMagmaHomomorphism_1158 ::
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringHomomorphism_864 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_1158 v0 v1
  = coe du_'42''45'isMagmaHomomorphism_888 v1
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringHomomorphism.homo
d_homo_1160 ::
  T_IsNearSemiringHomomorphism_864 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1160 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe d_'43''45'isMonoidHomomorphism_872 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringHomomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1162 ::
  T_IsNearSemiringHomomorphism_864 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1162 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe d_'43''45'isMonoidHomomorphism_872 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringHomomorphism.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_1164 ::
  T_IsNearSemiringHomomorphism_864 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_1164 v0
  = coe d_'43''45'isMonoidHomomorphism_872 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringHomomorphism.ε-homo
d_ε'45'homo_1166 :: T_IsNearSemiringHomomorphism_864 -> AgdaAny
d_ε'45'homo_1166 v0
  = coe
      d_ε'45'homo_276 (coe d_'43''45'isMonoidHomomorphism_872 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringHomomorphism.isRelHomomorphism
d_isRelHomomorphism_1168 ::
  T_IsNearSemiringHomomorphism_864 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1168 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe d_'43''45'isMonoidHomomorphism_872 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringHomomorphism.cong
d_cong_1170 ::
  T_IsNearSemiringHomomorphism_864 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1170 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_'43''45'isMonoidHomomorphism_872 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.*-homo
d_'42''45'homo_1174 ::
  T_IsNearSemiringIsomorphism_934 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1174 v0
  = coe
      d_'42''45'homo_874
      (coe
         d_isNearSemiringHomomorphism_900
         (coe d_isNearSemiringMonomorphism_942 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_1176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_1176 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_1176 v7
du_'42''45'isMagmaHomomorphism_1176 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_1176 v0
  = let v1 = d_isNearSemiringMonomorphism_942 (coe v0) in
    coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_900 (coe v1))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.*-isMagmaIsomorphism
d_'42''45'isMagmaIsomorphism_1178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaIsomorphism_118
d_'42''45'isMagmaIsomorphism_1178 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'42''45'isMagmaIsomorphism_1178
du_'42''45'isMagmaIsomorphism_1178 ::
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaIsomorphism_118
du_'42''45'isMagmaIsomorphism_1178 v0 v1
  = coe du_'42''45'isMagmaIsomorphism_984 v1
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.*-isMagmaMonomorphism
d_'42''45'isMagmaMonomorphism_1180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaMonomorphism_94
d_'42''45'isMagmaMonomorphism_1180 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaMonomorphism_1180 v7
du_'42''45'isMagmaMonomorphism_1180 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaMonomorphism_94
du_'42''45'isMagmaMonomorphism_1180 v0
  = coe
      du_'42''45'isMagmaMonomorphism_930
      (coe d_isNearSemiringMonomorphism_942 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.homo
d_homo_1182 ::
  T_IsNearSemiringIsomorphism_934 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1182 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_900
               (coe d_isNearSemiringMonomorphism_942 (coe v0)))))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1184 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1184 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_900
            (coe d_isNearSemiringMonomorphism_942 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.isMagmaIsomorphism
d_isMagmaIsomorphism_1186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_1186 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaIsomorphism_1186 v7
du_isMagmaIsomorphism_1186 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_1186 v0
  = coe
      du_isMagmaIsomorphism_352
      (coe du_'43''45'isMonoidIsomorphism_976 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1188 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_1188 v7
du_isMagmaMonomorphism_1188 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1188 v0
  = let v1 = d_isNearSemiringMonomorphism_942 (coe v0) in
    coe
      du_isMagmaMonomorphism_312
      (coe du_'43''45'isMonoidMonomorphism_922 (coe v1))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_1190 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_1190 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe
         d_isNearSemiringHomomorphism_900
         (coe d_isNearSemiringMonomorphism_942 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.+-isMonoidIsomorphism
d_'43''45'isMonoidIsomorphism_1192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMonoidIsomorphism_320
d_'43''45'isMonoidIsomorphism_1192 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'43''45'isMonoidIsomorphism_1192
du_'43''45'isMonoidIsomorphism_1192 ::
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMonoidIsomorphism_320
du_'43''45'isMonoidIsomorphism_1192 v0 v1
  = coe du_'43''45'isMonoidIsomorphism_976 v1
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.+-isMonoidMonomorphism
d_'43''45'isMonoidMonomorphism_1194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 -> T_IsMonoidMonomorphism_288
d_'43''45'isMonoidMonomorphism_1194 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isMonoidMonomorphism_1194 v7
du_'43''45'isMonoidMonomorphism_1194 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsMonoidMonomorphism_288
du_'43''45'isMonoidMonomorphism_1194 v0
  = coe
      du_'43''45'isMonoidMonomorphism_922
      (coe d_isNearSemiringMonomorphism_942 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.ε-homo
d_ε'45'homo_1196 :: T_IsNearSemiringIsomorphism_934 -> AgdaAny
d_ε'45'homo_1196 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_900
            (coe d_isNearSemiringMonomorphism_942 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.injective
d_injective_1198 ::
  T_IsNearSemiringIsomorphism_934 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1198 v0
  = coe
      d_injective_902 (coe d_isNearSemiringMonomorphism_942 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_1200 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_1200 v0
  = coe
      d_isNearSemiringHomomorphism_900
      (coe d_isNearSemiringMonomorphism_942 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.isNearSemiringMonomorphism
d_isNearSemiringMonomorphism_1202 ::
  T_IsNearSemiringIsomorphism_934 -> T_IsNearSemiringMonomorphism_892
d_isNearSemiringMonomorphism_1202 v0
  = coe d_isNearSemiringMonomorphism_942 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.isRelHomomorphism
d_isRelHomomorphism_1204 ::
  T_IsNearSemiringIsomorphism_934 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1204 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_900
               (coe d_isNearSemiringMonomorphism_942 (coe v0)))))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.isRelIsomorphism
d_isRelIsomorphism_1206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_1206 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_1206 v7
du_isRelIsomorphism_1206 ::
  T_IsNearSemiringIsomorphism_934 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_1206 v0
  = let v1 = coe du_'43''45'isMonoidIsomorphism_976 (coe v0) in
    coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v1))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.isRelMonomorphism
d_isRelMonomorphism_1208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringIsomorphism_934 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1208 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1208 v7
du_isRelMonomorphism_1208 ::
  T_IsNearSemiringIsomorphism_934 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1208 v0
  = let v1 = d_isNearSemiringMonomorphism_942 (coe v0) in
    let v2 = coe du_'43''45'isMonoidMonomorphism_922 (coe v1) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v2))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.surjective
d_surjective_1210 ::
  T_IsNearSemiringIsomorphism_934 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_1210 v0 = coe d_surjective_944 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringIsomorphism.cong
d_cong_1212 ::
  T_IsNearSemiringIsomorphism_934 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1212 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe
                  d_isNearSemiringHomomorphism_900
                  (coe d_isNearSemiringMonomorphism_942 (coe v0))))))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.*-homo
d_'42''45'homo_1216 ::
  T_IsNearSemiringMonomorphism_892 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1216 v0
  = coe
      d_'42''45'homo_874 (coe d_isNearSemiringHomomorphism_900 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_1218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_1218 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_1218 v7
du_'42''45'isMagmaHomomorphism_1218 ::
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_1218 v0
  = coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_900 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.*-isMagmaMonomorphism
d_'42''45'isMagmaMonomorphism_1220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaMonomorphism_94
d_'42''45'isMagmaMonomorphism_1220 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'42''45'isMagmaMonomorphism_1220
du_'42''45'isMagmaMonomorphism_1220 ::
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaMonomorphism_94
du_'42''45'isMagmaMonomorphism_1220 v0 v1
  = coe du_'42''45'isMagmaMonomorphism_930 v1
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.homo
d_homo_1222 ::
  T_IsNearSemiringMonomorphism_892 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1222 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe d_isNearSemiringHomomorphism_900 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1224 ::
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1224 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe d_isNearSemiringHomomorphism_900 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1226 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_1226 v7
du_isMagmaMonomorphism_1226 ::
  T_IsNearSemiringMonomorphism_892 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1226 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe du_'43''45'isMonoidMonomorphism_922 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_1228 ::
  T_IsNearSemiringMonomorphism_892 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_1228 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe d_isNearSemiringHomomorphism_900 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.+-isMonoidMonomorphism
d_'43''45'isMonoidMonomorphism_1230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 -> T_IsMonoidMonomorphism_288
d_'43''45'isMonoidMonomorphism_1230 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'43''45'isMonoidMonomorphism_1230
du_'43''45'isMonoidMonomorphism_1230 ::
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 -> T_IsMonoidMonomorphism_288
du_'43''45'isMonoidMonomorphism_1230 v0 v1
  = coe du_'43''45'isMonoidMonomorphism_922 v1
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.ε-homo
d_ε'45'homo_1232 :: T_IsNearSemiringMonomorphism_892 -> AgdaAny
d_ε'45'homo_1232 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe d_isNearSemiringHomomorphism_900 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.injective
d_injective_1234 ::
  T_IsNearSemiringMonomorphism_892 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1234 v0 = coe d_injective_902 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_1236 ::
  T_IsNearSemiringMonomorphism_892 ->
  T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_1236 v0
  = coe d_isNearSemiringHomomorphism_900 (coe v0)
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.isRelHomomorphism
d_isRelHomomorphism_1238 ::
  T_IsNearSemiringMonomorphism_892 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1238 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe d_isNearSemiringHomomorphism_900 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.isRelMonomorphism
d_isRelMonomorphism_1240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearSemiringMonomorphism_892 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1240 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1240 v7
du_isRelMonomorphism_1240 ::
  T_IsNearSemiringMonomorphism_892 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1240 v0
  = let v1 = coe du_'43''45'isMonoidMonomorphism_922 (coe v0) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v1))
-- Algebra.Morphism.Structures.SemiringMorphisms._.IsNearSemiringMonomorphism.cong
d_cong_1242 ::
  T_IsNearSemiringMonomorphism_892 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1242 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe d_isNearSemiringHomomorphism_900 (coe v0)))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism
d_IsSemiringHomomorphism_1246 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsSemiringHomomorphism_1246
  = C_IsSemiringHomomorphism'46'constructor_21773 T_IsNearSemiringHomomorphism_864
                                                  AgdaAny
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_1254 ::
  T_IsSemiringHomomorphism_1246 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_1254 v0
  = case coe v0 of
      C_IsSemiringHomomorphism'46'constructor_21773 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism.1#-homo
d_1'35''45'homo_1256 :: T_IsSemiringHomomorphism_1246 -> AgdaAny
d_1'35''45'homo_1256 v0
  = case coe v0 of
      C_IsSemiringHomomorphism'46'constructor_21773 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism._.*-homo
d_'42''45'homo_1260 ::
  T_IsSemiringHomomorphism_1246 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1260 v0
  = coe
      d_'42''45'homo_874 (coe d_isNearSemiringHomomorphism_1254 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism._.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_1262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringHomomorphism_1246 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_1262 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_1262 v7
du_'42''45'isMagmaHomomorphism_1262 ::
  T_IsSemiringHomomorphism_1246 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_1262 v0
  = coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_1254 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism._.homo
d_homo_1264 ::
  T_IsSemiringHomomorphism_1246 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1264 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe d_isNearSemiringHomomorphism_1254 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_1266 ::
  T_IsSemiringHomomorphism_1246 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1266 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe d_isNearSemiringHomomorphism_1254 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism._.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_1268 ::
  T_IsSemiringHomomorphism_1246 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_1268 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe d_isNearSemiringHomomorphism_1254 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism._.ε-homo
d_ε'45'homo_1270 :: T_IsSemiringHomomorphism_1246 -> AgdaAny
d_ε'45'homo_1270 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe d_isNearSemiringHomomorphism_1254 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism._.isRelHomomorphism
d_isRelHomomorphism_1272 ::
  T_IsSemiringHomomorphism_1246 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1272 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe d_isNearSemiringHomomorphism_1254 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism._.cong
d_cong_1274 ::
  T_IsSemiringHomomorphism_1246 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1274 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe d_isNearSemiringHomomorphism_1254 (coe v0)))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringHomomorphism.*-isMonoidHomomorphism
d_'42''45'isMonoidHomomorphism_1276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringHomomorphism_1246 -> T_IsMonoidHomomorphism_266
d_'42''45'isMonoidHomomorphism_1276 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidHomomorphism_1276 v7
du_'42''45'isMonoidHomomorphism_1276 ::
  T_IsSemiringHomomorphism_1246 -> T_IsMonoidHomomorphism_266
du_'42''45'isMonoidHomomorphism_1276 v0
  = coe
      C_IsMonoidHomomorphism'46'constructor_5533
      (coe
         du_'42''45'isMagmaHomomorphism_888
         (coe d_isNearSemiringHomomorphism_1254 (coe v0)))
      (coe d_1'35''45'homo_1256 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism
d_IsSemiringMonomorphism_1280 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsSemiringMonomorphism_1280
  = C_IsSemiringMonomorphism'46'constructor_22781 T_IsSemiringHomomorphism_1246
                                                  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism.isSemiringHomomorphism
d_isSemiringHomomorphism_1288 ::
  T_IsSemiringMonomorphism_1280 -> T_IsSemiringHomomorphism_1246
d_isSemiringHomomorphism_1288 v0
  = case coe v0 of
      C_IsSemiringMonomorphism'46'constructor_22781 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism.injective
d_injective_1290 ::
  T_IsSemiringMonomorphism_1280 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1290 v0
  = case coe v0 of
      C_IsSemiringMonomorphism'46'constructor_22781 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.*-homo
d_'42''45'homo_1294 ::
  T_IsSemiringMonomorphism_1280 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1294 v0
  = coe
      d_'42''45'homo_874
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe d_isSemiringHomomorphism_1288 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_1296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_1296 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_1296 v7
du_'42''45'isMagmaHomomorphism_1296 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_1296 v0
  = let v1 = d_isSemiringHomomorphism_1288 (coe v0) in
    coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_1254 (coe v1))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.*-isMonoidHomomorphism
d_'42''45'isMonoidHomomorphism_1298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidHomomorphism_266
d_'42''45'isMonoidHomomorphism_1298 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidHomomorphism_1298 v7
du_'42''45'isMonoidHomomorphism_1298 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidHomomorphism_266
du_'42''45'isMonoidHomomorphism_1298 v0
  = coe
      du_'42''45'isMonoidHomomorphism_1276
      (coe d_isSemiringHomomorphism_1288 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.homo
d_homo_1300 ::
  T_IsSemiringMonomorphism_1280 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1300 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe d_isSemiringHomomorphism_1288 (coe v0)))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_1302 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1302 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe d_isSemiringHomomorphism_1288 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_1304 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_1304 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe d_isSemiringHomomorphism_1288 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.ε-homo
d_ε'45'homo_1306 :: T_IsSemiringMonomorphism_1280 -> AgdaAny
d_ε'45'homo_1306 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe d_isSemiringHomomorphism_1288 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.1#-homo
d_1'35''45'homo_1308 :: T_IsSemiringMonomorphism_1280 -> AgdaAny
d_1'35''45'homo_1308 v0
  = coe
      d_1'35''45'homo_1256 (coe d_isSemiringHomomorphism_1288 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_1310 ::
  T_IsSemiringMonomorphism_1280 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_1310 v0
  = coe
      d_isNearSemiringHomomorphism_1254
      (coe d_isSemiringHomomorphism_1288 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.isRelHomomorphism
d_isRelHomomorphism_1312 ::
  T_IsSemiringMonomorphism_1280 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1312 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe d_isSemiringHomomorphism_1288 (coe v0)))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.cong
d_cong_1314 ::
  T_IsSemiringMonomorphism_1280 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1314 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe
                  d_isNearSemiringHomomorphism_1254
                  (coe d_isSemiringHomomorphism_1288 (coe v0))))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism.isNearSemiringMonomorphism
d_isNearSemiringMonomorphism_1316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsNearSemiringMonomorphism_892
d_isNearSemiringMonomorphism_1316 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isNearSemiringMonomorphism_1316 v7
du_isNearSemiringMonomorphism_1316 ::
  T_IsSemiringMonomorphism_1280 -> T_IsNearSemiringMonomorphism_892
du_isNearSemiringMonomorphism_1316 v0
  = coe
      C_IsNearSemiringMonomorphism'46'constructor_16335
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe d_isSemiringHomomorphism_1288 (coe v0)))
      (coe d_injective_1290 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.*-isMagmaMonomorphism
d_'42''45'isMagmaMonomorphism_1320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMagmaMonomorphism_94
d_'42''45'isMagmaMonomorphism_1320 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaMonomorphism_1320 v7
du_'42''45'isMagmaMonomorphism_1320 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMagmaMonomorphism_94
du_'42''45'isMagmaMonomorphism_1320 v0
  = coe
      du_'42''45'isMagmaMonomorphism_930
      (coe du_isNearSemiringMonomorphism_1316 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism._.+-isMonoidMonomorphism
d_'43''45'isMonoidMonomorphism_1322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidMonomorphism_288
d_'43''45'isMonoidMonomorphism_1322 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isMonoidMonomorphism_1322 v7
du_'43''45'isMonoidMonomorphism_1322 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidMonomorphism_288
du_'43''45'isMonoidMonomorphism_1322 v0
  = coe
      du_'43''45'isMonoidMonomorphism_922
      (coe du_isNearSemiringMonomorphism_1316 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringMonomorphism.*-isMonoidMonomorphism
d_'42''45'isMonoidMonomorphism_1324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidMonomorphism_288
d_'42''45'isMonoidMonomorphism_1324 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidMonomorphism_1324 v7
du_'42''45'isMonoidMonomorphism_1324 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidMonomorphism_288
du_'42''45'isMonoidMonomorphism_1324 v0
  = coe
      C_IsMonoidMonomorphism'46'constructor_6057
      (coe
         du_'42''45'isMonoidHomomorphism_1276
         (coe d_isSemiringHomomorphism_1288 (coe v0)))
      (coe d_injective_1290 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism
d_IsSemiringIsomorphism_1328 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsSemiringIsomorphism_1328
  = C_IsSemiringIsomorphism'46'constructor_24539 T_IsSemiringMonomorphism_1280
                                                 (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism.isSemiringMonomorphism
d_isSemiringMonomorphism_1336 ::
  T_IsSemiringIsomorphism_1328 -> T_IsSemiringMonomorphism_1280
d_isSemiringMonomorphism_1336 v0
  = case coe v0 of
      C_IsSemiringIsomorphism'46'constructor_24539 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism.surjective
d_surjective_1338 ::
  T_IsSemiringIsomorphism_1328 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_1338 v0
  = case coe v0 of
      C_IsSemiringIsomorphism'46'constructor_24539 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.*-homo
d_'42''45'homo_1342 ::
  T_IsSemiringIsomorphism_1328 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1342 v0
  = coe
      d_'42''45'homo_874
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe
            d_isSemiringHomomorphism_1288
            (coe d_isSemiringMonomorphism_1336 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_1344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_1344 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_1344 v7
du_'42''45'isMagmaHomomorphism_1344 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_1344 v0
  = let v1 = d_isSemiringMonomorphism_1336 (coe v0) in
    let v2 = d_isSemiringHomomorphism_1288 (coe v1) in
    coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_1254 (coe v2))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.*-isMagmaMonomorphism
d_'42''45'isMagmaMonomorphism_1346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaMonomorphism_94
d_'42''45'isMagmaMonomorphism_1346 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaMonomorphism_1346 v7
du_'42''45'isMagmaMonomorphism_1346 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaMonomorphism_94
du_'42''45'isMagmaMonomorphism_1346 v0
  = let v1 = d_isSemiringMonomorphism_1336 (coe v0) in
    coe
      du_'42''45'isMagmaMonomorphism_930
      (coe du_isNearSemiringMonomorphism_1316 (coe v1))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.*-isMonoidHomomorphism
d_'42''45'isMonoidHomomorphism_1348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidHomomorphism_266
d_'42''45'isMonoidHomomorphism_1348 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidHomomorphism_1348 v7
du_'42''45'isMonoidHomomorphism_1348 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidHomomorphism_266
du_'42''45'isMonoidHomomorphism_1348 v0
  = let v1 = d_isSemiringMonomorphism_1336 (coe v0) in
    coe
      du_'42''45'isMonoidHomomorphism_1276
      (coe d_isSemiringHomomorphism_1288 (coe v1))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.*-isMonoidMonomorphism
d_'42''45'isMonoidMonomorphism_1350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidMonomorphism_288
d_'42''45'isMonoidMonomorphism_1350 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidMonomorphism_1350 v7
du_'42''45'isMonoidMonomorphism_1350 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidMonomorphism_288
du_'42''45'isMonoidMonomorphism_1350 v0
  = coe
      du_'42''45'isMonoidMonomorphism_1324
      (coe d_isSemiringMonomorphism_1336 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.homo
d_homo_1352 ::
  T_IsSemiringIsomorphism_1328 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1352 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe
                  d_isSemiringHomomorphism_1288
                  (coe d_isSemiringMonomorphism_1336 (coe v0))))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_1354 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1354 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe
               d_isSemiringHomomorphism_1288
               (coe d_isSemiringMonomorphism_1336 (coe v0)))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_1356 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_1356 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe
            d_isSemiringHomomorphism_1288
            (coe d_isSemiringMonomorphism_1336 (coe v0))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.+-isMonoidMonomorphism
d_'43''45'isMonoidMonomorphism_1358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidMonomorphism_288
d_'43''45'isMonoidMonomorphism_1358 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isMonoidMonomorphism_1358 v7
du_'43''45'isMonoidMonomorphism_1358 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidMonomorphism_288
du_'43''45'isMonoidMonomorphism_1358 v0
  = let v1 = d_isSemiringMonomorphism_1336 (coe v0) in
    coe
      du_'43''45'isMonoidMonomorphism_922
      (coe du_isNearSemiringMonomorphism_1316 (coe v1))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.ε-homo
d_ε'45'homo_1360 :: T_IsSemiringIsomorphism_1328 -> AgdaAny
d_ε'45'homo_1360 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe
               d_isSemiringHomomorphism_1288
               (coe d_isSemiringMonomorphism_1336 (coe v0)))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.1#-homo
d_1'35''45'homo_1362 :: T_IsSemiringIsomorphism_1328 -> AgdaAny
d_1'35''45'homo_1362 v0
  = coe
      d_1'35''45'homo_1256
      (coe
         d_isSemiringHomomorphism_1288
         (coe d_isSemiringMonomorphism_1336 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.injective
d_injective_1364 ::
  T_IsSemiringIsomorphism_1328 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1364 v0
  = coe d_injective_1290 (coe d_isSemiringMonomorphism_1336 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_1366 ::
  T_IsSemiringIsomorphism_1328 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_1366 v0
  = coe
      d_isNearSemiringHomomorphism_1254
      (coe
         d_isSemiringHomomorphism_1288
         (coe d_isSemiringMonomorphism_1336 (coe v0)))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.isNearSemiringMonomorphism
d_isNearSemiringMonomorphism_1368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsNearSemiringMonomorphism_892
d_isNearSemiringMonomorphism_1368 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isNearSemiringMonomorphism_1368 v7
du_isNearSemiringMonomorphism_1368 ::
  T_IsSemiringIsomorphism_1328 -> T_IsNearSemiringMonomorphism_892
du_isNearSemiringMonomorphism_1368 v0
  = coe
      du_isNearSemiringMonomorphism_1316
      (coe d_isSemiringMonomorphism_1336 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.isRelHomomorphism
d_isRelHomomorphism_1370 ::
  T_IsSemiringIsomorphism_1328 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1370 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe
                  d_isSemiringHomomorphism_1288
                  (coe d_isSemiringMonomorphism_1336 (coe v0))))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.isSemiringHomomorphism
d_isSemiringHomomorphism_1372 ::
  T_IsSemiringIsomorphism_1328 -> T_IsSemiringHomomorphism_1246
d_isSemiringHomomorphism_1372 v0
  = coe
      d_isSemiringHomomorphism_1288
      (coe d_isSemiringMonomorphism_1336 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.cong
d_cong_1374 ::
  T_IsSemiringIsomorphism_1328 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1374 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe
                  d_isNearSemiringHomomorphism_1254
                  (coe
                     d_isSemiringHomomorphism_1288
                     (coe d_isSemiringMonomorphism_1336 (coe v0)))))))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism.isNearSemiringIsomorphism
d_isNearSemiringIsomorphism_1376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsNearSemiringIsomorphism_934
d_isNearSemiringIsomorphism_1376 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isNearSemiringIsomorphism_1376 v7
du_isNearSemiringIsomorphism_1376 ::
  T_IsSemiringIsomorphism_1328 -> T_IsNearSemiringIsomorphism_934
du_isNearSemiringIsomorphism_1376 v0
  = coe
      C_IsNearSemiringIsomorphism'46'constructor_17921
      (coe
         du_isNearSemiringMonomorphism_1316
         (coe d_isSemiringMonomorphism_1336 (coe v0)))
      (coe d_surjective_1338 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.*-isMagmaIsomorphism
d_'42''45'isMagmaIsomorphism_1380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaIsomorphism_118
d_'42''45'isMagmaIsomorphism_1380 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaIsomorphism_1380 v7
du_'42''45'isMagmaIsomorphism_1380 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaIsomorphism_118
du_'42''45'isMagmaIsomorphism_1380 v0
  = coe
      du_'42''45'isMagmaIsomorphism_984
      (coe du_isNearSemiringIsomorphism_1376 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism._.+-isMonoidIsomorphism
d_'43''45'isMonoidIsomorphism_1382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidIsomorphism_320
d_'43''45'isMonoidIsomorphism_1382 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isMonoidIsomorphism_1382 v7
du_'43''45'isMonoidIsomorphism_1382 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidIsomorphism_320
du_'43''45'isMonoidIsomorphism_1382 v0
  = coe
      du_'43''45'isMonoidIsomorphism_976
      (coe du_isNearSemiringIsomorphism_1376 (coe v0))
-- Algebra.Morphism.Structures.SemiringMorphisms.IsSemiringIsomorphism.*-isMonoidIsomorphism
d_'42''45'isMonoidIsomorphism_1384 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidIsomorphism_320
d_'42''45'isMonoidIsomorphism_1384 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidIsomorphism_1384 v7
du_'42''45'isMonoidIsomorphism_1384 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidIsomorphism_320
du_'42''45'isMonoidIsomorphism_1384 v0
  = coe
      C_IsMonoidIsomorphism'46'constructor_7115
      (coe
         du_'42''45'isMonoidMonomorphism_1324
         (coe d_isSemiringMonomorphism_1336 (coe v0)))
      (coe d_surjective_1338 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms._._*_
d__'42'__1402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__1402 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'42'__1402 v4
du__'42'__1402 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'42'__1402 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__220 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms._.Carrier
d_Carrier_1422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 -> ()
d_Carrier_1422 = erased
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupHomomorphism
d_IsGroupHomomorphism_1450 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism
d_IsGroupIsomorphism_1452 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism
d_IsGroupMonomorphism_1454 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupHomomorphism.homo
d_homo_1458 ::
  T_IsGroupHomomorphism_554 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1458 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_562 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupHomomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1460 ::
  T_IsGroupHomomorphism_554 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1460 v0
  = coe
      d_isMagmaHomomorphism_274 (coe d_isMonoidHomomorphism_562 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupHomomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_1462 ::
  T_IsGroupHomomorphism_554 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1462 v0
  = coe d_isMonoidHomomorphism_562 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupHomomorphism.isRelHomomorphism
d_isRelHomomorphism_1464 ::
  T_IsGroupHomomorphism_554 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1464 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_562 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupHomomorphism.ε-homo
d_ε'45'homo_1466 :: T_IsGroupHomomorphism_554 -> AgdaAny
d_ε'45'homo_1466 v0
  = coe d_ε'45'homo_276 (coe d_isMonoidHomomorphism_562 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupHomomorphism.⁻¹-homo
d_'8315''185''45'homo_1468 ::
  T_IsGroupHomomorphism_554 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_1468 v0
  = coe d_'8315''185''45'homo_564 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupHomomorphism.cong
d_cong_1470 ::
  T_IsGroupHomomorphism_554 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1470 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_isMonoidHomomorphism_562 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.injective
d_injective_1474 ::
  T_IsGroupIsomorphism_618 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1474 v0
  = coe d_injective_590 (coe d_isGroupMonomorphism_626 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isGroupHomomorphism
d_isGroupHomomorphism_1476 ::
  T_IsGroupIsomorphism_618 -> T_IsGroupHomomorphism_554
d_isGroupHomomorphism_1476 v0
  = coe
      d_isGroupHomomorphism_588 (coe d_isGroupMonomorphism_626 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isGroupMonomorphism
d_isGroupMonomorphism_1478 ::
  T_IsGroupIsomorphism_618 -> T_IsGroupMonomorphism_580
d_isGroupMonomorphism_1478 v0
  = coe d_isGroupMonomorphism_626 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1480 ::
  T_IsGroupIsomorphism_618 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1480 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_562
         (coe
            d_isGroupHomomorphism_588
            (coe d_isGroupMonomorphism_626 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isMagmaIsomorphism
d_isMagmaIsomorphism_1482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_1482 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaIsomorphism_1482 v7
du_isMagmaIsomorphism_1482 ::
  T_IsGroupIsomorphism_618 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_1482 v0
  = coe
      du_isMagmaIsomorphism_352 (coe du_isMonoidIsomorphism_656 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1484 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_1484 v7
du_isMagmaMonomorphism_1484 ::
  T_IsGroupIsomorphism_618 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1484 v0
  = let v1 = d_isGroupMonomorphism_626 (coe v0) in
    coe
      du_isMagmaMonomorphism_312
      (coe du_isMonoidMonomorphism_608 (coe v1))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_1486 ::
  T_IsGroupIsomorphism_618 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1486 v0
  = coe
      d_isMonoidHomomorphism_562
      (coe
         d_isGroupHomomorphism_588 (coe d_isGroupMonomorphism_626 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isMonoidIsomorphism
d_isMonoidIsomorphism_1488 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMonoidIsomorphism_320
d_isMonoidIsomorphism_1488 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMonoidIsomorphism_1488
du_isMonoidIsomorphism_1488 ::
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMonoidIsomorphism_320
du_isMonoidIsomorphism_1488 v0 v1
  = coe du_isMonoidIsomorphism_656 v1
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isMonoidMonomorphism
d_isMonoidMonomorphism_1490 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_1490 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMonoidMonomorphism_1490 v7
du_isMonoidMonomorphism_1490 ::
  T_IsGroupIsomorphism_618 -> T_IsMonoidMonomorphism_288
du_isMonoidMonomorphism_1490 v0
  = coe
      du_isMonoidMonomorphism_608
      (coe d_isGroupMonomorphism_626 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isRelHomomorphism
d_isRelHomomorphism_1492 ::
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1492 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe
               d_isGroupHomomorphism_588
               (coe d_isGroupMonomorphism_626 (coe v0)))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isRelIsomorphism
d_isRelIsomorphism_1494 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_1494 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_1494 v7
du_isRelIsomorphism_1494 ::
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_1494 v0
  = let v1 = coe du_isMonoidIsomorphism_656 (coe v0) in
    coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v1))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.isRelMonomorphism
d_isRelMonomorphism_1496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1496 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1496 v7
du_isRelMonomorphism_1496 ::
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1496 v0
  = let v1 = d_isGroupMonomorphism_626 (coe v0) in
    let v2 = coe du_isMonoidMonomorphism_608 (coe v1) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v2))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.surjective
d_surjective_1498 ::
  T_IsGroupIsomorphism_618 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_1498 v0 = coe d_surjective_628 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.ε-homo
d_ε'45'homo_1500 :: T_IsGroupIsomorphism_618 -> AgdaAny
d_ε'45'homo_1500 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_562
         (coe
            d_isGroupHomomorphism_588
            (coe d_isGroupMonomorphism_626 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.⁻¹-homo
d_'8315''185''45'homo_1502 ::
  T_IsGroupIsomorphism_618 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_1502 v0
  = coe
      d_'8315''185''45'homo_564
      (coe
         d_isGroupHomomorphism_588 (coe d_isGroupMonomorphism_626 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.homo
d_homo_1504 ::
  T_IsGroupIsomorphism_618 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1504 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe
               d_isGroupHomomorphism_588
               (coe d_isGroupMonomorphism_626 (coe v0)))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupIsomorphism.cong
d_cong_1506 ::
  T_IsGroupIsomorphism_618 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1506 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_562
               (coe
                  d_isGroupHomomorphism_588
                  (coe d_isGroupMonomorphism_626 (coe v0))))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.injective
d_injective_1510 ::
  T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1510 v0 = coe d_injective_590 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.isGroupHomomorphism
d_isGroupHomomorphism_1512 ::
  T_IsGroupMonomorphism_580 -> T_IsGroupHomomorphism_554
d_isGroupHomomorphism_1512 v0
  = coe d_isGroupHomomorphism_588 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1514 ::
  T_IsGroupMonomorphism_580 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1514 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_562
         (coe d_isGroupHomomorphism_588 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1516 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_1516 v7
du_isMagmaMonomorphism_1516 ::
  T_IsGroupMonomorphism_580 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1516 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe du_isMonoidMonomorphism_608 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_1518 ::
  T_IsGroupMonomorphism_580 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1518 v0
  = coe
      d_isMonoidHomomorphism_562 (coe d_isGroupHomomorphism_588 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.isMonoidMonomorphism
d_isMonoidMonomorphism_1520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_1520 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMonoidMonomorphism_1520
du_isMonoidMonomorphism_1520 ::
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 -> T_IsMonoidMonomorphism_288
du_isMonoidMonomorphism_1520 v0 v1
  = coe du_isMonoidMonomorphism_608 v1
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.isRelHomomorphism
d_isRelHomomorphism_1522 ::
  T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1522 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe d_isGroupHomomorphism_588 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.isRelMonomorphism
d_isRelMonomorphism_1524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1524 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1524 v7
du_isRelMonomorphism_1524 ::
  T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1524 v0
  = let v1 = coe du_isMonoidMonomorphism_608 (coe v0) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v1))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.ε-homo
d_ε'45'homo_1526 :: T_IsGroupMonomorphism_580 -> AgdaAny
d_ε'45'homo_1526 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_562
         (coe d_isGroupHomomorphism_588 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.⁻¹-homo
d_'8315''185''45'homo_1528 ::
  T_IsGroupMonomorphism_580 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_1528 v0
  = coe
      d_'8315''185''45'homo_564 (coe d_isGroupHomomorphism_588 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.homo
d_homo_1530 ::
  T_IsGroupMonomorphism_580 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1530 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe d_isGroupHomomorphism_588 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.+.IsGroupMonomorphism.cong
d_cong_1532 ::
  T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1532 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_562
               (coe d_isGroupHomomorphism_588 (coe v0)))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaHomomorphism
d_IsMagmaHomomorphism_1536 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaIsomorphism
d_IsMagmaIsomorphism_1538 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaMonomorphism
d_IsMagmaMonomorphism_1540 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaHomomorphism.homo
d_homo_1544 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1544 v0 = coe d_homo_86 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_1546 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1546 v0 = coe d_isRelHomomorphism_84 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaHomomorphism.cong
d_cong_1548 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1548 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaIsomorphism.homo
d_homo_1552 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1552 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaIsomorphism.injective
d_injective_1554 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1554 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1556 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1556 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1558 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1558 v0
  = coe d_isMagmaMonomorphism_126 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_1560 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1560 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_1562 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_1562 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_1562
du_isRelIsomorphism_1562 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_1562 v0 v1 = coe du_isRelIsomorphism_144 v1
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_1564 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1564 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1564 v7
du_isRelMonomorphism_1564 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1564 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaIsomorphism.surjective
d_surjective_1566 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_1566 v0 = coe d_surjective_128 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaIsomorphism.cong
d_cong_1568 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1568 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaMonomorphism.homo
d_homo_1572 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1572 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaMonomorphism.injective
d_injective_1574 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1574 v0 = coe d_injective_104 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1576 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1576 v0
  = coe d_isMagmaHomomorphism_102 (coe v0)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_1578 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1578 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_1580 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1580 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_1580
du_isRelMonomorphism_1580 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1580 v0 v1 = coe du_isRelMonomorphism_114 v1
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.*.IsMagmaMonomorphism.cong
d_cong_1582 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1582 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms._.Homomorphic₂
d_Homomorphic'8322'_1590 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8322'_1590 = erased
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms._.Injective
d_Injective_1600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_1600 = erased
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms._.Surjective
d_Surjective_1608 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_1608 = erased
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism
d_IsRingWithoutOneHomomorphism_1612 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsRingWithoutOneHomomorphism_1612
  = C_IsRingWithoutOneHomomorphism'46'constructor_28339 T_IsGroupHomomorphism_554
                                                        (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism.+-isGroupHomomorphism
d_'43''45'isGroupHomomorphism_1620 ::
  T_IsRingWithoutOneHomomorphism_1612 -> T_IsGroupHomomorphism_554
d_'43''45'isGroupHomomorphism_1620 v0
  = case coe v0 of
      C_IsRingWithoutOneHomomorphism'46'constructor_28339 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism.*-homo
d_'42''45'homo_1622 ::
  T_IsRingWithoutOneHomomorphism_1612 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1622 v0
  = case coe v0 of
      C_IsRingWithoutOneHomomorphism'46'constructor_28339 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism._.homo
d_homo_1626 ::
  T_IsRingWithoutOneHomomorphism_1612 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_homo_1626 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe d_'43''45'isGroupHomomorphism_1620 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_1628 ::
  T_IsRingWithoutOneHomomorphism_1612 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1628 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_562
         (coe d_'43''45'isGroupHomomorphism_1620 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism._.ε-homo
d_ε'45'homo_1630 :: T_IsRingWithoutOneHomomorphism_1612 -> AgdaAny
d_ε'45'homo_1630 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_562
         (coe d_'43''45'isGroupHomomorphism_1620 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism._.isMonoidHomomorphism
d_isMonoidHomomorphism_1632 ::
  T_IsRingWithoutOneHomomorphism_1612 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1632 v0
  = coe
      d_isMonoidHomomorphism_562
      (coe d_'43''45'isGroupHomomorphism_1620 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism._.isRelHomomorphism
d_isRelHomomorphism_1634 ::
  T_IsRingWithoutOneHomomorphism_1612 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1634 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe d_'43''45'isGroupHomomorphism_1620 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism._.⁻¹-homo
d_'8315''185''45'homo_1636 ::
  T_IsRingWithoutOneHomomorphism_1612 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_1636 v0
  = coe
      d_'8315''185''45'homo_564
      (coe d_'43''45'isGroupHomomorphism_1620 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism._.cong
d_cong_1638 ::
  T_IsRingWithoutOneHomomorphism_1612 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1638 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_562
               (coe d_'43''45'isGroupHomomorphism_1620 (coe v0)))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneHomomorphism.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_1640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneHomomorphism_1612 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_1640 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_1640 v7
du_'42''45'isMagmaHomomorphism_1640 ::
  T_IsRingWithoutOneHomomorphism_1612 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_1640 v0
  = coe
      C_IsMagmaHomomorphism'46'constructor_1049
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_562
               (coe d_'43''45'isGroupHomomorphism_1620 (coe v0)))))
      (coe d_'42''45'homo_1622 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism
d_IsRingWithoutOneMonomorphism_1644 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsRingWithoutOneMonomorphism_1644
  = C_IsRingWithoutOneMonomorphism'46'constructor_29295 T_IsRingWithoutOneHomomorphism_1612
                                                        (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism.isRingWithoutOneHomomorphism
d_isRingWithoutOneHomomorphism_1652 ::
  T_IsRingWithoutOneMonomorphism_1644 ->
  T_IsRingWithoutOneHomomorphism_1612
d_isRingWithoutOneHomomorphism_1652 v0
  = case coe v0 of
      C_IsRingWithoutOneMonomorphism'46'constructor_29295 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism.injective
d_injective_1654 ::
  T_IsRingWithoutOneMonomorphism_1644 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1654 v0
  = case coe v0 of
      C_IsRingWithoutOneMonomorphism'46'constructor_29295 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.*-homo
d_'42''45'homo_1658 ::
  T_IsRingWithoutOneMonomorphism_1644 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1658 v0
  = coe
      d_'42''45'homo_1622
      (coe d_isRingWithoutOneHomomorphism_1652 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_1660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_1660 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_1660 v7
du_'42''45'isMagmaHomomorphism_1660 ::
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_1660 v0
  = coe
      du_'42''45'isMagmaHomomorphism_1640
      (coe d_isRingWithoutOneHomomorphism_1652 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.homo
d_homo_1662 ::
  T_IsRingWithoutOneMonomorphism_1644 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_homo_1662 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe
               d_'43''45'isGroupHomomorphism_1620
               (coe d_isRingWithoutOneHomomorphism_1652 (coe v0)))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.+-isGroupHomomorphism
d_'43''45'isGroupHomomorphism_1664 ::
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsGroupHomomorphism_554
d_'43''45'isGroupHomomorphism_1664 v0
  = coe
      d_'43''45'isGroupHomomorphism_1620
      (coe d_isRingWithoutOneHomomorphism_1652 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_1666 ::
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1666 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_562
         (coe
            d_'43''45'isGroupHomomorphism_1620
            (coe d_isRingWithoutOneHomomorphism_1652 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.ε-homo
d_ε'45'homo_1668 :: T_IsRingWithoutOneMonomorphism_1644 -> AgdaAny
d_ε'45'homo_1668 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_562
         (coe
            d_'43''45'isGroupHomomorphism_1620
            (coe d_isRingWithoutOneHomomorphism_1652 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.isMonoidHomomorphism
d_isMonoidHomomorphism_1670 ::
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1670 v0
  = coe
      d_isMonoidHomomorphism_562
      (coe
         d_'43''45'isGroupHomomorphism_1620
         (coe d_isRingWithoutOneHomomorphism_1652 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.isRelHomomorphism
d_isRelHomomorphism_1672 ::
  T_IsRingWithoutOneMonomorphism_1644 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1672 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe
               d_'43''45'isGroupHomomorphism_1620
               (coe d_isRingWithoutOneHomomorphism_1652 (coe v0)))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.⁻¹-homo
d_'8315''185''45'homo_1674 ::
  T_IsRingWithoutOneMonomorphism_1644 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_1674 v0
  = coe
      d_'8315''185''45'homo_564
      (coe
         d_'43''45'isGroupHomomorphism_1620
         (coe d_isRingWithoutOneHomomorphism_1652 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.cong
d_cong_1676 ::
  T_IsRingWithoutOneMonomorphism_1644 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1676 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_562
               (coe
                  d_'43''45'isGroupHomomorphism_1620
                  (coe d_isRingWithoutOneHomomorphism_1652 (coe v0))))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism.+-isGroupMonomorphism
d_'43''45'isGroupMonomorphism_1678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsGroupMonomorphism_580
d_'43''45'isGroupMonomorphism_1678 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isGroupMonomorphism_1678 v7
du_'43''45'isGroupMonomorphism_1678 ::
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsGroupMonomorphism_580
du_'43''45'isGroupMonomorphism_1678 v0
  = coe
      C_IsGroupMonomorphism'46'constructor_11055
      (coe
         d_'43''45'isGroupHomomorphism_1620
         (coe d_isRingWithoutOneHomomorphism_1652 (coe v0)))
      (coe d_injective_1654 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.isMagmaMonomorphism
d_isMagmaMonomorphism_1682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1682 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_1682 v7
du_isMagmaMonomorphism_1682 ::
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1682 v0
  = let v1 = coe du_'43''45'isGroupMonomorphism_1678 (coe v0) in
    coe
      du_isMagmaMonomorphism_312
      (coe du_isMonoidMonomorphism_608 (coe v1))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.isMonoidMonomorphism
d_isMonoidMonomorphism_1684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_1684 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMonoidMonomorphism_1684 v7
du_isMonoidMonomorphism_1684 ::
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsMonoidMonomorphism_288
du_isMonoidMonomorphism_1684 v0
  = coe
      du_isMonoidMonomorphism_608
      (coe du_'43''45'isGroupMonomorphism_1678 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism._.isRelMonomorphism
d_isRelMonomorphism_1686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneMonomorphism_1644 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1686 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1686 v7
du_isRelMonomorphism_1686 ::
  T_IsRingWithoutOneMonomorphism_1644 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1686 v0
  = let v1 = coe du_'43''45'isGroupMonomorphism_1678 (coe v0) in
    let v2 = coe du_isMonoidMonomorphism_608 (coe v1) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v2))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneMonomorphism.*-isMagmaMonomorphism
d_'42''45'isMagmaMonomorphism_1688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsMagmaMonomorphism_94
d_'42''45'isMagmaMonomorphism_1688 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaMonomorphism_1688 v7
du_'42''45'isMagmaMonomorphism_1688 ::
  T_IsRingWithoutOneMonomorphism_1644 -> T_IsMagmaMonomorphism_94
du_'42''45'isMagmaMonomorphism_1688 v0
  = coe
      C_IsMagmaMonomorphism'46'constructor_1881
      (coe
         du_'42''45'isMagmaHomomorphism_1640
         (coe d_isRingWithoutOneHomomorphism_1652 (coe v0)))
      (coe d_injective_1654 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism
d_IsRingWithoutOneIsoMorphism_1692 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsRingWithoutOneIsoMorphism_1692
  = C_IsRingWithoutOneIsoMorphism'46'constructor_31057 T_IsRingWithoutOneMonomorphism_1644
                                                       (AgdaAny ->
                                                        MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism.isRingWithoutOneMonomorphism
d_isRingWithoutOneMonomorphism_1700 ::
  T_IsRingWithoutOneIsoMorphism_1692 ->
  T_IsRingWithoutOneMonomorphism_1644
d_isRingWithoutOneMonomorphism_1700 v0
  = case coe v0 of
      C_IsRingWithoutOneIsoMorphism'46'constructor_31057 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism.surjective
d_surjective_1702 ::
  T_IsRingWithoutOneIsoMorphism_1692 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_1702 v0
  = case coe v0 of
      C_IsRingWithoutOneIsoMorphism'46'constructor_31057 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.*-homo
d_'42''45'homo_1706 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_1706 v0
  = coe
      d_'42''45'homo_1622
      (coe
         d_isRingWithoutOneHomomorphism_1652
         (coe d_isRingWithoutOneMonomorphism_1700 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_1708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_1708 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_1708 v7
du_'42''45'isMagmaHomomorphism_1708 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_1708 v0
  = let v1 = d_isRingWithoutOneMonomorphism_1700 (coe v0) in
    coe
      du_'42''45'isMagmaHomomorphism_1640
      (coe d_isRingWithoutOneHomomorphism_1652 (coe v1))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.*-isMagmaMonomorphism
d_'42''45'isMagmaMonomorphism_1710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaMonomorphism_94
d_'42''45'isMagmaMonomorphism_1710 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaMonomorphism_1710 v7
du_'42''45'isMagmaMonomorphism_1710 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaMonomorphism_94
du_'42''45'isMagmaMonomorphism_1710 v0
  = coe
      du_'42''45'isMagmaMonomorphism_1688
      (coe d_isRingWithoutOneMonomorphism_1700 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.homo
d_homo_1712 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1712 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe
               d_'43''45'isGroupHomomorphism_1620
               (coe
                  d_isRingWithoutOneHomomorphism_1652
                  (coe d_isRingWithoutOneMonomorphism_1700 (coe v0))))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.+-isGroupHomomorphism
d_'43''45'isGroupHomomorphism_1714 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsGroupHomomorphism_554
d_'43''45'isGroupHomomorphism_1714 v0
  = coe
      d_'43''45'isGroupHomomorphism_1620
      (coe
         d_isRingWithoutOneHomomorphism_1652
         (coe d_isRingWithoutOneMonomorphism_1700 (coe v0)))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.+-isGroupMonomorphism
d_'43''45'isGroupMonomorphism_1716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsGroupMonomorphism_580
d_'43''45'isGroupMonomorphism_1716 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isGroupMonomorphism_1716 v7
du_'43''45'isGroupMonomorphism_1716 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsGroupMonomorphism_580
du_'43''45'isGroupMonomorphism_1716 v0
  = coe
      du_'43''45'isGroupMonomorphism_1678
      (coe d_isRingWithoutOneMonomorphism_1700 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_1718 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1718 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_562
         (coe
            d_'43''45'isGroupHomomorphism_1620
            (coe
               d_isRingWithoutOneHomomorphism_1652
               (coe d_isRingWithoutOneMonomorphism_1700 (coe v0)))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.isMagmaMonomorphism
d_isMagmaMonomorphism_1720 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1720 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_1720 v7
du_isMagmaMonomorphism_1720 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1720 v0
  = let v1 = d_isRingWithoutOneMonomorphism_1700 (coe v0) in
    let v2 = coe du_'43''45'isGroupMonomorphism_1678 (coe v1) in
    coe
      du_isMagmaMonomorphism_312
      (coe du_isMonoidMonomorphism_608 (coe v2))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.isMonoidMonomorphism
d_isMonoidMonomorphism_1722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_1722 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMonoidMonomorphism_1722 v7
du_isMonoidMonomorphism_1722 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMonoidMonomorphism_288
du_isMonoidMonomorphism_1722 v0
  = let v1 = d_isRingWithoutOneMonomorphism_1700 (coe v0) in
    coe
      du_isMonoidMonomorphism_608
      (coe du_'43''45'isGroupMonomorphism_1678 (coe v1))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.ε-homo
d_ε'45'homo_1724 :: T_IsRingWithoutOneIsoMorphism_1692 -> AgdaAny
d_ε'45'homo_1724 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_562
         (coe
            d_'43''45'isGroupHomomorphism_1620
            (coe
               d_isRingWithoutOneHomomorphism_1652
               (coe d_isRingWithoutOneMonomorphism_1700 (coe v0)))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.injective
d_injective_1726 ::
  T_IsRingWithoutOneIsoMorphism_1692 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1726 v0
  = coe
      d_injective_1654 (coe d_isRingWithoutOneMonomorphism_1700 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.isMonoidHomomorphism
d_isMonoidHomomorphism_1728 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1728 v0
  = coe
      d_isMonoidHomomorphism_562
      (coe
         d_'43''45'isGroupHomomorphism_1620
         (coe
            d_isRingWithoutOneHomomorphism_1652
            (coe d_isRingWithoutOneMonomorphism_1700 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.isRelHomomorphism
d_isRelHomomorphism_1730 ::
  T_IsRingWithoutOneIsoMorphism_1692 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1730 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe
               d_'43''45'isGroupHomomorphism_1620
               (coe
                  d_isRingWithoutOneHomomorphism_1652
                  (coe d_isRingWithoutOneMonomorphism_1700 (coe v0))))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.isRelMonomorphism
d_isRelMonomorphism_1732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1732 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1732 v7
du_isRelMonomorphism_1732 ::
  T_IsRingWithoutOneIsoMorphism_1692 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1732 v0
  = let v1 = d_isRingWithoutOneMonomorphism_1700 (coe v0) in
    let v2 = coe du_'43''45'isGroupMonomorphism_1678 (coe v1) in
    let v3 = coe du_isMonoidMonomorphism_608 (coe v2) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v3))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.isRingWithoutOneHomomorphism
d_isRingWithoutOneHomomorphism_1734 ::
  T_IsRingWithoutOneIsoMorphism_1692 ->
  T_IsRingWithoutOneHomomorphism_1612
d_isRingWithoutOneHomomorphism_1734 v0
  = coe
      d_isRingWithoutOneHomomorphism_1652
      (coe d_isRingWithoutOneMonomorphism_1700 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.⁻¹-homo
d_'8315''185''45'homo_1736 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_1736 v0
  = coe
      d_'8315''185''45'homo_564
      (coe
         d_'43''45'isGroupHomomorphism_1620
         (coe
            d_isRingWithoutOneHomomorphism_1652
            (coe d_isRingWithoutOneMonomorphism_1700 (coe v0))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.cong
d_cong_1738 ::
  T_IsRingWithoutOneIsoMorphism_1692 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1738 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_562
               (coe
                  d_'43''45'isGroupHomomorphism_1620
                  (coe
                     d_isRingWithoutOneHomomorphism_1652
                     (coe d_isRingWithoutOneMonomorphism_1700 (coe v0)))))))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism.+-isGroupIsomorphism
d_'43''45'isGroupIsomorphism_1740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsGroupIsomorphism_618
d_'43''45'isGroupIsomorphism_1740 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isGroupIsomorphism_1740 v7
du_'43''45'isGroupIsomorphism_1740 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsGroupIsomorphism_618
du_'43''45'isGroupIsomorphism_1740 v0
  = coe
      C_IsGroupIsomorphism'46'constructor_12289
      (coe
         du_'43''45'isGroupMonomorphism_1678
         (coe d_isRingWithoutOneMonomorphism_1700 (coe v0)))
      (coe d_surjective_1702 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.isMagmaIsomorphism
d_isMagmaIsomorphism_1744 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_1744 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaIsomorphism_1744 v7
du_isMagmaIsomorphism_1744 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_1744 v0
  = let v1 = coe du_'43''45'isGroupIsomorphism_1740 (coe v0) in
    coe
      du_isMagmaIsomorphism_352 (coe du_isMonoidIsomorphism_656 (coe v1))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.isMonoidIsomorphism
d_isMonoidIsomorphism_1746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMonoidIsomorphism_320
d_isMonoidIsomorphism_1746 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMonoidIsomorphism_1746 v7
du_isMonoidIsomorphism_1746 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMonoidIsomorphism_320
du_isMonoidIsomorphism_1746 v0
  = coe
      du_isMonoidIsomorphism_656
      (coe du_'43''45'isGroupIsomorphism_1740 (coe v0))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism._.isRelIsomorphism
d_isRelIsomorphism_1748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_1748 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_1748 v7
du_isRelIsomorphism_1748 ::
  T_IsRingWithoutOneIsoMorphism_1692 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_1748 v0
  = let v1 = coe du_'43''45'isGroupIsomorphism_1740 (coe v0) in
    let v2 = coe du_isMonoidIsomorphism_656 (coe v1) in
    coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v2))
-- Algebra.Morphism.Structures.RingWithoutOneMorphisms.IsRingWithoutOneIsoMorphism.*-isMagmaIsomorphism
d_'42''45'isMagmaIsomorphism_1750 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRingWithoutOne_196 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaIsomorphism_118
d_'42''45'isMagmaIsomorphism_1750 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaIsomorphism_1750 v7
du_'42''45'isMagmaIsomorphism_1750 ::
  T_IsRingWithoutOneIsoMorphism_1692 -> T_IsMagmaIsomorphism_118
du_'42''45'isMagmaIsomorphism_1750 v0
  = coe
      C_IsMagmaIsomorphism'46'constructor_3015
      (coe
         du_'42''45'isMagmaMonomorphism_1688
         (coe d_isRingWithoutOneMonomorphism_1700 (coe v0)))
      (coe d_surjective_1702 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.-_
d_'45'__1786 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny
d_'45'__1786 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_'45'__1786 v4
du_'45'__1786 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  AgdaAny -> AgdaAny
du_'45'__1786 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms._.Carrier
d_Carrier_1792 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 -> ()
d_Carrier_1792 = erased
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupHomomorphism
d_IsGroupHomomorphism_1828 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism
d_IsGroupIsomorphism_1830 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism
d_IsGroupMonomorphism_1832 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupHomomorphism.homo
d_homo_1836 ::
  T_IsGroupHomomorphism_554 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1836 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_562 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupHomomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1838 ::
  T_IsGroupHomomorphism_554 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1838 v0
  = coe
      d_isMagmaHomomorphism_274 (coe d_isMonoidHomomorphism_562 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupHomomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_1840 ::
  T_IsGroupHomomorphism_554 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1840 v0
  = coe d_isMonoidHomomorphism_562 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupHomomorphism.isRelHomomorphism
d_isRelHomomorphism_1842 ::
  T_IsGroupHomomorphism_554 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1842 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_562 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupHomomorphism.ε-homo
d_ε'45'homo_1844 :: T_IsGroupHomomorphism_554 -> AgdaAny
d_ε'45'homo_1844 v0
  = coe d_ε'45'homo_276 (coe d_isMonoidHomomorphism_562 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupHomomorphism.⁻¹-homo
d_'8315''185''45'homo_1846 ::
  T_IsGroupHomomorphism_554 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_1846 v0
  = coe d_'8315''185''45'homo_564 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupHomomorphism.cong
d_cong_1848 ::
  T_IsGroupHomomorphism_554 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1848 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_isMonoidHomomorphism_562 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.injective
d_injective_1852 ::
  T_IsGroupIsomorphism_618 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1852 v0
  = coe d_injective_590 (coe d_isGroupMonomorphism_626 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isGroupHomomorphism
d_isGroupHomomorphism_1854 ::
  T_IsGroupIsomorphism_618 -> T_IsGroupHomomorphism_554
d_isGroupHomomorphism_1854 v0
  = coe
      d_isGroupHomomorphism_588 (coe d_isGroupMonomorphism_626 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isGroupMonomorphism
d_isGroupMonomorphism_1856 ::
  T_IsGroupIsomorphism_618 -> T_IsGroupMonomorphism_580
d_isGroupMonomorphism_1856 v0
  = coe d_isGroupMonomorphism_626 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1858 ::
  T_IsGroupIsomorphism_618 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1858 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_562
         (coe
            d_isGroupHomomorphism_588
            (coe d_isGroupMonomorphism_626 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isMagmaIsomorphism
d_isMagmaIsomorphism_1860 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_1860 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaIsomorphism_1860 v7
du_isMagmaIsomorphism_1860 ::
  T_IsGroupIsomorphism_618 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_1860 v0
  = coe
      du_isMagmaIsomorphism_352 (coe du_isMonoidIsomorphism_656 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1862 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1862 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_1862 v7
du_isMagmaMonomorphism_1862 ::
  T_IsGroupIsomorphism_618 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1862 v0
  = let v1 = d_isGroupMonomorphism_626 (coe v0) in
    coe
      du_isMagmaMonomorphism_312
      (coe du_isMonoidMonomorphism_608 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_1864 ::
  T_IsGroupIsomorphism_618 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1864 v0
  = coe
      d_isMonoidHomomorphism_562
      (coe
         d_isGroupHomomorphism_588 (coe d_isGroupMonomorphism_626 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isMonoidIsomorphism
d_isMonoidIsomorphism_1866 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMonoidIsomorphism_320
d_isMonoidIsomorphism_1866 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMonoidIsomorphism_1866
du_isMonoidIsomorphism_1866 ::
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMonoidIsomorphism_320
du_isMonoidIsomorphism_1866 v0 v1
  = coe du_isMonoidIsomorphism_656 v1
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isMonoidMonomorphism
d_isMonoidMonomorphism_1868 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_1868 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMonoidMonomorphism_1868 v7
du_isMonoidMonomorphism_1868 ::
  T_IsGroupIsomorphism_618 -> T_IsMonoidMonomorphism_288
du_isMonoidMonomorphism_1868 v0
  = coe
      du_isMonoidMonomorphism_608
      (coe d_isGroupMonomorphism_626 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isRelHomomorphism
d_isRelHomomorphism_1870 ::
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1870 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe
               d_isGroupHomomorphism_588
               (coe d_isGroupMonomorphism_626 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isRelIsomorphism
d_isRelIsomorphism_1872 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_1872 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_1872 v7
du_isRelIsomorphism_1872 ::
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_1872 v0
  = let v1 = coe du_isMonoidIsomorphism_656 (coe v0) in
    coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.isRelMonomorphism
d_isRelMonomorphism_1874 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1874 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1874 v7
du_isRelMonomorphism_1874 ::
  T_IsGroupIsomorphism_618 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1874 v0
  = let v1 = d_isGroupMonomorphism_626 (coe v0) in
    let v2 = coe du_isMonoidMonomorphism_608 (coe v1) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v2))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.surjective
d_surjective_1876 ::
  T_IsGroupIsomorphism_618 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_1876 v0 = coe d_surjective_628 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.ε-homo
d_ε'45'homo_1878 :: T_IsGroupIsomorphism_618 -> AgdaAny
d_ε'45'homo_1878 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_562
         (coe
            d_isGroupHomomorphism_588
            (coe d_isGroupMonomorphism_626 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.⁻¹-homo
d_'8315''185''45'homo_1880 ::
  T_IsGroupIsomorphism_618 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_1880 v0
  = coe
      d_'8315''185''45'homo_564
      (coe
         d_isGroupHomomorphism_588 (coe d_isGroupMonomorphism_626 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.homo
d_homo_1882 ::
  T_IsGroupIsomorphism_618 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1882 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe
               d_isGroupHomomorphism_588
               (coe d_isGroupMonomorphism_626 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupIsomorphism.cong
d_cong_1884 ::
  T_IsGroupIsomorphism_618 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1884 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_562
               (coe
                  d_isGroupHomomorphism_588
                  (coe d_isGroupMonomorphism_626 (coe v0))))))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.injective
d_injective_1888 ::
  T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1888 v0 = coe d_injective_590 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.isGroupHomomorphism
d_isGroupHomomorphism_1890 ::
  T_IsGroupMonomorphism_580 -> T_IsGroupHomomorphism_554
d_isGroupHomomorphism_1890 v0
  = coe d_isGroupHomomorphism_588 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1892 ::
  T_IsGroupMonomorphism_580 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1892 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_562
         (coe d_isGroupHomomorphism_588 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1894 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1894 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_1894 v7
du_isMagmaMonomorphism_1894 ::
  T_IsGroupMonomorphism_580 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1894 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe du_isMonoidMonomorphism_608 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_1896 ::
  T_IsGroupMonomorphism_580 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1896 v0
  = coe
      d_isMonoidHomomorphism_562 (coe d_isGroupHomomorphism_588 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.isMonoidMonomorphism
d_isMonoidMonomorphism_1898 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_1898 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMonoidMonomorphism_1898
du_isMonoidMonomorphism_1898 ::
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 -> T_IsMonoidMonomorphism_288
du_isMonoidMonomorphism_1898 v0 v1
  = coe du_isMonoidMonomorphism_608 v1
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.isRelHomomorphism
d_isRelHomomorphism_1900 ::
  T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1900 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe d_isGroupHomomorphism_588 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.isRelMonomorphism
d_isRelMonomorphism_1902 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1902 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1902 v7
du_isRelMonomorphism_1902 ::
  T_IsGroupMonomorphism_580 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1902 v0
  = let v1 = coe du_isMonoidMonomorphism_608 (coe v0) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.ε-homo
d_ε'45'homo_1904 :: T_IsGroupMonomorphism_580 -> AgdaAny
d_ε'45'homo_1904 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_562
         (coe d_isGroupHomomorphism_588 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.⁻¹-homo
d_'8315''185''45'homo_1906 ::
  T_IsGroupMonomorphism_580 -> AgdaAny -> AgdaAny
d_'8315''185''45'homo_1906 v0
  = coe
      d_'8315''185''45'homo_564 (coe d_isGroupHomomorphism_588 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.homo
d_homo_1908 ::
  T_IsGroupMonomorphism_580 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1908 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_562
            (coe d_isGroupHomomorphism_588 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.+.IsGroupMonomorphism.cong
d_cong_1910 ::
  T_IsGroupMonomorphism_580 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1910 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_562
               (coe d_isGroupHomomorphism_588 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidHomomorphism
d_IsMonoidHomomorphism_1914 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism
d_IsMonoidIsomorphism_1916 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidMonomorphism
d_IsMonoidMonomorphism_1918 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidHomomorphism.homo
d_homo_1922 ::
  T_IsMonoidHomomorphism_266 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1922 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_274 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidHomomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1924 ::
  T_IsMonoidHomomorphism_266 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1924 v0
  = coe d_isMagmaHomomorphism_274 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidHomomorphism.isRelHomomorphism
d_isRelHomomorphism_1926 ::
  T_IsMonoidHomomorphism_266 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1926 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_274 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidHomomorphism.ε-homo
d_ε'45'homo_1928 :: T_IsMonoidHomomorphism_266 -> AgdaAny
d_ε'45'homo_1928 v0 = coe d_ε'45'homo_276 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidHomomorphism.cong
d_cong_1930 ::
  T_IsMonoidHomomorphism_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1930 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_274 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.homo
d_homo_1934 ::
  T_IsMonoidIsomorphism_320 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1934 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_296
            (coe d_isMonoidMonomorphism_328 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.injective
d_injective_1936 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1936 v0
  = coe d_injective_298 (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1938 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1938 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_isMonoidHomomorphism_296
         (coe d_isMonoidMonomorphism_328 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.isMagmaIsomorphism
d_isMagmaIsomorphism_1940 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_1940 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMagmaIsomorphism_1940
du_isMagmaIsomorphism_1940 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_1940 v0 v1 = coe du_isMagmaIsomorphism_352 v1
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1942 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1942 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_1942 v7
du_isMagmaMonomorphism_1942 ::
  T_IsMonoidIsomorphism_320 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1942 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_1944 ::
  T_IsMonoidIsomorphism_320 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1944 v0
  = coe
      d_isMonoidHomomorphism_296
      (coe d_isMonoidMonomorphism_328 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.isMonoidMonomorphism
d_isMonoidMonomorphism_1946 ::
  T_IsMonoidIsomorphism_320 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_1946 v0
  = coe d_isMonoidMonomorphism_328 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.isRelHomomorphism
d_isRelHomomorphism_1948 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1948 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_isMonoidHomomorphism_296
            (coe d_isMonoidMonomorphism_328 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.isRelIsomorphism
d_isRelIsomorphism_1950 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_1950 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_1950 v7
du_isRelIsomorphism_1950 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_1950 v0
  = coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.isRelMonomorphism
d_isRelMonomorphism_1952 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1952 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1952 v7
du_isRelMonomorphism_1952 ::
  T_IsMonoidIsomorphism_320 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1952 v0
  = let v1 = d_isMonoidMonomorphism_328 (coe v0) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.surjective
d_surjective_1954 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_1954 v0 = coe d_surjective_330 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.ε-homo
d_ε'45'homo_1956 :: T_IsMonoidIsomorphism_320 -> AgdaAny
d_ε'45'homo_1956 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_isMonoidHomomorphism_296
         (coe d_isMonoidMonomorphism_328 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidIsomorphism.cong
d_cong_1958 ::
  T_IsMonoidIsomorphism_320 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1958 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_isMonoidHomomorphism_296
               (coe d_isMonoidMonomorphism_328 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidMonomorphism.homo
d_homo_1962 ::
  T_IsMonoidMonomorphism_288 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_1962 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidMonomorphism.injective
d_injective_1964 ::
  T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_1964 v0 = coe d_injective_298 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_1966 ::
  T_IsMonoidMonomorphism_288 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_1966 v0
  = coe
      d_isMagmaHomomorphism_274 (coe d_isMonoidHomomorphism_296 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidMonomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_1968 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_1968 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isMagmaMonomorphism_1968
du_isMagmaMonomorphism_1968 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_1968 v0 v1
  = coe du_isMagmaMonomorphism_312 v1
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidMonomorphism.isMonoidHomomorphism
d_isMonoidHomomorphism_1970 ::
  T_IsMonoidMonomorphism_288 -> T_IsMonoidHomomorphism_266
d_isMonoidHomomorphism_1970 v0
  = coe d_isMonoidHomomorphism_296 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidMonomorphism.isRelHomomorphism
d_isRelHomomorphism_1972 ::
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_1972 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe d_isMonoidHomomorphism_296 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidMonomorphism.isRelMonomorphism
d_isRelMonomorphism_1974 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_1974 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_1974 v7
du_isRelMonomorphism_1974 ::
  T_IsMonoidMonomorphism_288 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_1974 v0
  = coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidMonomorphism.ε-homo
d_ε'45'homo_1976 :: T_IsMonoidMonomorphism_288 -> AgdaAny
d_ε'45'homo_1976 v0
  = coe d_ε'45'homo_276 (coe d_isMonoidHomomorphism_296 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.*.IsMonoidMonomorphism.cong
d_cong_1978 ::
  T_IsMonoidMonomorphism_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_1978 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe d_isMonoidHomomorphism_296 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms._.Homomorphic₁
d_Homomorphic'8321'_1984 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8321'_1984 = erased
-- Algebra.Morphism.Structures.RingMorphisms._.Injective
d_Injective_1996 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_1996 = erased
-- Algebra.Morphism.Structures.RingMorphisms._.Surjective
d_Surjective_2004 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_2004 = erased
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism
d_IsSemiringHomomorphism_2008 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism
d_IsSemiringIsomorphism_2010 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism
d_IsSemiringMonomorphism_2012 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.*-homo
d_'42''45'homo_2016 ::
  T_IsSemiringHomomorphism_1246 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_2016 v0
  = coe
      d_'42''45'homo_874 (coe d_isNearSemiringHomomorphism_1254 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_2018 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringHomomorphism_1246 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_2018 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_2018 v7
du_'42''45'isMagmaHomomorphism_2018 ::
  T_IsSemiringHomomorphism_1246 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_2018 v0
  = coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_1254 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.*-isMonoidHomomorphism
d_'42''45'isMonoidHomomorphism_2020 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringHomomorphism_1246 -> T_IsMonoidHomomorphism_266
d_'42''45'isMonoidHomomorphism_2020 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'42''45'isMonoidHomomorphism_2020
du_'42''45'isMonoidHomomorphism_2020 ::
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringHomomorphism_1246 -> T_IsMonoidHomomorphism_266
du_'42''45'isMonoidHomomorphism_2020 v0 v1
  = coe du_'42''45'isMonoidHomomorphism_1276 v1
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.homo
d_homo_2022 ::
  T_IsSemiringHomomorphism_1246 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2022 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe d_isNearSemiringHomomorphism_1254 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2024 ::
  T_IsSemiringHomomorphism_1246 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2024 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe d_isNearSemiringHomomorphism_1254 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_2026 ::
  T_IsSemiringHomomorphism_1246 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_2026 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe d_isNearSemiringHomomorphism_1254 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.ε-homo
d_ε'45'homo_2028 :: T_IsSemiringHomomorphism_1246 -> AgdaAny
d_ε'45'homo_2028 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe d_isNearSemiringHomomorphism_1254 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.1#-homo
d_1'35''45'homo_2030 :: T_IsSemiringHomomorphism_1246 -> AgdaAny
d_1'35''45'homo_2030 v0 = coe d_1'35''45'homo_1256 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_2032 ::
  T_IsSemiringHomomorphism_1246 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_2032 v0
  = coe d_isNearSemiringHomomorphism_1254 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.isRelHomomorphism
d_isRelHomomorphism_2034 ::
  T_IsSemiringHomomorphism_1246 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2034 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe d_isNearSemiringHomomorphism_1254 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringHomomorphism.cong
d_cong_2036 ::
  T_IsSemiringHomomorphism_1246 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2036 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe d_isNearSemiringHomomorphism_1254 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.*-homo
d_'42''45'homo_2040 ::
  T_IsSemiringIsomorphism_1328 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_2040 v0
  = coe
      d_'42''45'homo_874
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe
            d_isSemiringHomomorphism_1288
            (coe d_isSemiringMonomorphism_1336 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_2042 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_2042 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_2042 v7
du_'42''45'isMagmaHomomorphism_2042 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_2042 v0
  = let v1 = d_isSemiringMonomorphism_1336 (coe v0) in
    let v2 = d_isSemiringHomomorphism_1288 (coe v1) in
    coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_1254 (coe v2))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.*-isMagmaIsomorphism
d_'42''45'isMagmaIsomorphism_2044 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaIsomorphism_118
d_'42''45'isMagmaIsomorphism_2044 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaIsomorphism_2044 v7
du_'42''45'isMagmaIsomorphism_2044 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaIsomorphism_118
du_'42''45'isMagmaIsomorphism_2044 v0
  = coe
      du_'42''45'isMagmaIsomorphism_984
      (coe du_isNearSemiringIsomorphism_1376 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.*-isMagmaMonomorphism
d_'42''45'isMagmaMonomorphism_2046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaMonomorphism_94
d_'42''45'isMagmaMonomorphism_2046 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaMonomorphism_2046 v7
du_'42''45'isMagmaMonomorphism_2046 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaMonomorphism_94
du_'42''45'isMagmaMonomorphism_2046 v0
  = let v1 = d_isSemiringMonomorphism_1336 (coe v0) in
    coe
      du_'42''45'isMagmaMonomorphism_930
      (coe du_isNearSemiringMonomorphism_1316 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.*-isMonoidHomomorphism
d_'42''45'isMonoidHomomorphism_2048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidHomomorphism_266
d_'42''45'isMonoidHomomorphism_2048 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidHomomorphism_2048 v7
du_'42''45'isMonoidHomomorphism_2048 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidHomomorphism_266
du_'42''45'isMonoidHomomorphism_2048 v0
  = let v1 = d_isSemiringMonomorphism_1336 (coe v0) in
    coe
      du_'42''45'isMonoidHomomorphism_1276
      (coe d_isSemiringHomomorphism_1288 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.*-isMonoidIsomorphism
d_'42''45'isMonoidIsomorphism_2050 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidIsomorphism_320
d_'42''45'isMonoidIsomorphism_2050 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'42''45'isMonoidIsomorphism_2050
du_'42''45'isMonoidIsomorphism_2050 ::
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidIsomorphism_320
du_'42''45'isMonoidIsomorphism_2050 v0 v1
  = coe du_'42''45'isMonoidIsomorphism_1384 v1
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.*-isMonoidMonomorphism
d_'42''45'isMonoidMonomorphism_2052 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidMonomorphism_288
d_'42''45'isMonoidMonomorphism_2052 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidMonomorphism_2052 v7
du_'42''45'isMonoidMonomorphism_2052 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidMonomorphism_288
du_'42''45'isMonoidMonomorphism_2052 v0
  = coe
      du_'42''45'isMonoidMonomorphism_1324
      (coe d_isSemiringMonomorphism_1336 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.homo
d_homo_2054 ::
  T_IsSemiringIsomorphism_1328 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2054 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe
                  d_isSemiringHomomorphism_1288
                  (coe d_isSemiringMonomorphism_1336 (coe v0))))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2056 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2056 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe
               d_isSemiringHomomorphism_1288
               (coe d_isSemiringMonomorphism_1336 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_2058 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_2058 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe
            d_isSemiringHomomorphism_1288
            (coe d_isSemiringMonomorphism_1336 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.+-isMonoidIsomorphism
d_'43''45'isMonoidIsomorphism_2060 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidIsomorphism_320
d_'43''45'isMonoidIsomorphism_2060 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isMonoidIsomorphism_2060 v7
du_'43''45'isMonoidIsomorphism_2060 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidIsomorphism_320
du_'43''45'isMonoidIsomorphism_2060 v0
  = coe
      du_'43''45'isMonoidIsomorphism_976
      (coe du_isNearSemiringIsomorphism_1376 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.+-isMonoidMonomorphism
d_'43''45'isMonoidMonomorphism_2062 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidMonomorphism_288
d_'43''45'isMonoidMonomorphism_2062 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isMonoidMonomorphism_2062 v7
du_'43''45'isMonoidMonomorphism_2062 ::
  T_IsSemiringIsomorphism_1328 -> T_IsMonoidMonomorphism_288
du_'43''45'isMonoidMonomorphism_2062 v0
  = let v1 = d_isSemiringMonomorphism_1336 (coe v0) in
    coe
      du_'43''45'isMonoidMonomorphism_922
      (coe du_isNearSemiringMonomorphism_1316 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.ε-homo
d_ε'45'homo_2064 :: T_IsSemiringIsomorphism_1328 -> AgdaAny
d_ε'45'homo_2064 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe
               d_isSemiringHomomorphism_1288
               (coe d_isSemiringMonomorphism_1336 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.1#-homo
d_1'35''45'homo_2066 :: T_IsSemiringIsomorphism_1328 -> AgdaAny
d_1'35''45'homo_2066 v0
  = coe
      d_1'35''45'homo_1256
      (coe
         d_isSemiringHomomorphism_1288
         (coe d_isSemiringMonomorphism_1336 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.injective
d_injective_2068 ::
  T_IsSemiringIsomorphism_1328 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2068 v0
  = coe d_injective_1290 (coe d_isSemiringMonomorphism_1336 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_2070 ::
  T_IsSemiringIsomorphism_1328 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_2070 v0
  = coe
      d_isNearSemiringHomomorphism_1254
      (coe
         d_isSemiringHomomorphism_1288
         (coe d_isSemiringMonomorphism_1336 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.isNearSemiringIsomorphism
d_isNearSemiringIsomorphism_2072 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsNearSemiringIsomorphism_934
d_isNearSemiringIsomorphism_2072 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isNearSemiringIsomorphism_2072
du_isNearSemiringIsomorphism_2072 ::
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsNearSemiringIsomorphism_934
du_isNearSemiringIsomorphism_2072 v0 v1
  = coe du_isNearSemiringIsomorphism_1376 v1
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.isNearSemiringMonomorphism
d_isNearSemiringMonomorphism_2074 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringIsomorphism_1328 -> T_IsNearSemiringMonomorphism_892
d_isNearSemiringMonomorphism_2074 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isNearSemiringMonomorphism_2074 v7
du_isNearSemiringMonomorphism_2074 ::
  T_IsSemiringIsomorphism_1328 -> T_IsNearSemiringMonomorphism_892
du_isNearSemiringMonomorphism_2074 v0
  = coe
      du_isNearSemiringMonomorphism_1316
      (coe d_isSemiringMonomorphism_1336 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.isRelHomomorphism
d_isRelHomomorphism_2076 ::
  T_IsSemiringIsomorphism_1328 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2076 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe
                  d_isSemiringHomomorphism_1288
                  (coe d_isSemiringMonomorphism_1336 (coe v0))))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.isSemiringHomomorphism
d_isSemiringHomomorphism_2078 ::
  T_IsSemiringIsomorphism_1328 -> T_IsSemiringHomomorphism_1246
d_isSemiringHomomorphism_2078 v0
  = coe
      d_isSemiringHomomorphism_1288
      (coe d_isSemiringMonomorphism_1336 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.isSemiringMonomorphism
d_isSemiringMonomorphism_2080 ::
  T_IsSemiringIsomorphism_1328 -> T_IsSemiringMonomorphism_1280
d_isSemiringMonomorphism_2080 v0
  = coe d_isSemiringMonomorphism_1336 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.surjective
d_surjective_2082 ::
  T_IsSemiringIsomorphism_1328 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_2082 v0 = coe d_surjective_1338 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringIsomorphism.cong
d_cong_2084 ::
  T_IsSemiringIsomorphism_1328 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2084 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe
                  d_isNearSemiringHomomorphism_1254
                  (coe
                     d_isSemiringHomomorphism_1288
                     (coe d_isSemiringMonomorphism_1336 (coe v0)))))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.*-homo
d_'42''45'homo_2088 ::
  T_IsSemiringMonomorphism_1280 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_2088 v0
  = coe
      d_'42''45'homo_874
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe d_isSemiringHomomorphism_1288 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_2090 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_2090 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_2090 v7
du_'42''45'isMagmaHomomorphism_2090 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_2090 v0
  = let v1 = d_isSemiringHomomorphism_1288 (coe v0) in
    coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_1254 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.*-isMagmaMonomorphism
d_'42''45'isMagmaMonomorphism_2092 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMagmaMonomorphism_94
d_'42''45'isMagmaMonomorphism_2092 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaMonomorphism_2092 v7
du_'42''45'isMagmaMonomorphism_2092 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMagmaMonomorphism_94
du_'42''45'isMagmaMonomorphism_2092 v0
  = coe
      du_'42''45'isMagmaMonomorphism_930
      (coe du_isNearSemiringMonomorphism_1316 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.*-isMonoidHomomorphism
d_'42''45'isMonoidHomomorphism_2094 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidHomomorphism_266
d_'42''45'isMonoidHomomorphism_2094 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidHomomorphism_2094 v7
du_'42''45'isMonoidHomomorphism_2094 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidHomomorphism_266
du_'42''45'isMonoidHomomorphism_2094 v0
  = coe
      du_'42''45'isMonoidHomomorphism_1276
      (coe d_isSemiringHomomorphism_1288 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.*-isMonoidMonomorphism
d_'42''45'isMonoidMonomorphism_2096 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidMonomorphism_288
d_'42''45'isMonoidMonomorphism_2096 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'42''45'isMonoidMonomorphism_2096
du_'42''45'isMonoidMonomorphism_2096 ::
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidMonomorphism_288
du_'42''45'isMonoidMonomorphism_2096 v0 v1
  = coe du_'42''45'isMonoidMonomorphism_1324 v1
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.homo
d_homo_2098 ::
  T_IsSemiringMonomorphism_1280 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2098 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe d_isSemiringHomomorphism_1288 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2100 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2100 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe d_isSemiringHomomorphism_1288 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_2102 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_2102 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe d_isSemiringHomomorphism_1288 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.+-isMonoidMonomorphism
d_'43''45'isMonoidMonomorphism_2104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidMonomorphism_288
d_'43''45'isMonoidMonomorphism_2104 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isMonoidMonomorphism_2104 v7
du_'43''45'isMonoidMonomorphism_2104 ::
  T_IsSemiringMonomorphism_1280 -> T_IsMonoidMonomorphism_288
du_'43''45'isMonoidMonomorphism_2104 v0
  = coe
      du_'43''45'isMonoidMonomorphism_922
      (coe du_isNearSemiringMonomorphism_1316 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.ε-homo
d_ε'45'homo_2106 :: T_IsSemiringMonomorphism_1280 -> AgdaAny
d_ε'45'homo_2106 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe d_isSemiringHomomorphism_1288 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.1#-homo
d_1'35''45'homo_2108 :: T_IsSemiringMonomorphism_1280 -> AgdaAny
d_1'35''45'homo_2108 v0
  = coe
      d_1'35''45'homo_1256 (coe d_isSemiringHomomorphism_1288 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.injective
d_injective_2110 ::
  T_IsSemiringMonomorphism_1280 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2110 v0 = coe d_injective_1290 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_2112 ::
  T_IsSemiringMonomorphism_1280 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_2112 v0
  = coe
      d_isNearSemiringHomomorphism_1254
      (coe d_isSemiringHomomorphism_1288 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.isNearSemiringMonomorphism
d_isNearSemiringMonomorphism_2114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsNearSemiringMonomorphism_892
d_isNearSemiringMonomorphism_2114 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isNearSemiringMonomorphism_2114
du_isNearSemiringMonomorphism_2114 ::
  (AgdaAny -> AgdaAny) ->
  T_IsSemiringMonomorphism_1280 -> T_IsNearSemiringMonomorphism_892
du_isNearSemiringMonomorphism_2114 v0 v1
  = coe du_isNearSemiringMonomorphism_1316 v1
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.isRelHomomorphism
d_isRelHomomorphism_2116 ::
  T_IsSemiringMonomorphism_1280 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2116 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe d_isSemiringHomomorphism_1288 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.isSemiringHomomorphism
d_isSemiringHomomorphism_2118 ::
  T_IsSemiringMonomorphism_1280 -> T_IsSemiringHomomorphism_1246
d_isSemiringHomomorphism_2118 v0
  = coe d_isSemiringHomomorphism_1288 (coe v0)
-- Algebra.Morphism.Structures.RingMorphisms._.IsSemiringMonomorphism.cong
d_cong_2120 ::
  T_IsSemiringMonomorphism_1280 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2120 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe
                  d_isNearSemiringHomomorphism_1254
                  (coe d_isSemiringHomomorphism_1288 (coe v0))))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism
d_IsRingHomomorphism_2124 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsRingHomomorphism_2124
  = C_IsRingHomomorphism'46'constructor_36047 T_IsSemiringHomomorphism_1246
                                              (AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism.isSemiringHomomorphism
d_isSemiringHomomorphism_2132 ::
  T_IsRingHomomorphism_2124 -> T_IsSemiringHomomorphism_1246
d_isSemiringHomomorphism_2132 v0
  = case coe v0 of
      C_IsRingHomomorphism'46'constructor_36047 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism.-‿homo
d_'45''8255'homo_2134 ::
  T_IsRingHomomorphism_2124 -> AgdaAny -> AgdaAny
d_'45''8255'homo_2134 v0
  = case coe v0 of
      C_IsRingHomomorphism'46'constructor_36047 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.*-homo
d_'42''45'homo_2138 ::
  T_IsRingHomomorphism_2124 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_2138 v0
  = coe
      d_'42''45'homo_874
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe d_isSemiringHomomorphism_2132 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_2140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingHomomorphism_2124 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_2140 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_2140 v7
du_'42''45'isMagmaHomomorphism_2140 ::
  T_IsRingHomomorphism_2124 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_2140 v0
  = let v1 = d_isSemiringHomomorphism_2132 (coe v0) in
    coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_1254 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.*-isMonoidHomomorphism
d_'42''45'isMonoidHomomorphism_2142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingHomomorphism_2124 -> T_IsMonoidHomomorphism_266
d_'42''45'isMonoidHomomorphism_2142 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidHomomorphism_2142 v7
du_'42''45'isMonoidHomomorphism_2142 ::
  T_IsRingHomomorphism_2124 -> T_IsMonoidHomomorphism_266
du_'42''45'isMonoidHomomorphism_2142 v0
  = coe
      du_'42''45'isMonoidHomomorphism_1276
      (coe d_isSemiringHomomorphism_2132 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.homo
d_homo_2144 ::
  T_IsRingHomomorphism_2124 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2144 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe d_isSemiringHomomorphism_2132 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_2146 ::
  T_IsRingHomomorphism_2124 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2146 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe d_isSemiringHomomorphism_2132 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_2148 ::
  T_IsRingHomomorphism_2124 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_2148 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe d_isSemiringHomomorphism_2132 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.ε-homo
d_ε'45'homo_2150 :: T_IsRingHomomorphism_2124 -> AgdaAny
d_ε'45'homo_2150 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe d_isSemiringHomomorphism_2132 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.1#-homo
d_1'35''45'homo_2152 :: T_IsRingHomomorphism_2124 -> AgdaAny
d_1'35''45'homo_2152 v0
  = coe
      d_1'35''45'homo_1256 (coe d_isSemiringHomomorphism_2132 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_2154 ::
  T_IsRingHomomorphism_2124 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_2154 v0
  = coe
      d_isNearSemiringHomomorphism_1254
      (coe d_isSemiringHomomorphism_2132 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.isRelHomomorphism
d_isRelHomomorphism_2156 ::
  T_IsRingHomomorphism_2124 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2156 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe d_isSemiringHomomorphism_2132 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism._.cong
d_cong_2158 ::
  T_IsRingHomomorphism_2124 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2158 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe
                  d_isNearSemiringHomomorphism_1254
                  (coe d_isSemiringHomomorphism_2132 (coe v0))))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingHomomorphism.+-isGroupHomomorphism
d_'43''45'isGroupHomomorphism_2160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingHomomorphism_2124 -> T_IsGroupHomomorphism_554
d_'43''45'isGroupHomomorphism_2160 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isGroupHomomorphism_2160 v7
du_'43''45'isGroupHomomorphism_2160 ::
  T_IsRingHomomorphism_2124 -> T_IsGroupHomomorphism_554
du_'43''45'isGroupHomomorphism_2160 v0
  = coe
      C_IsGroupHomomorphism'46'constructor_10405
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe d_isSemiringHomomorphism_2132 (coe v0))))
      (coe d_'45''8255'homo_2134 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism
d_IsRingMonomorphism_2164 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsRingMonomorphism_2164
  = C_IsRingMonomorphism'46'constructor_37231 T_IsRingHomomorphism_2124
                                              (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism.isRingHomomorphism
d_isRingHomomorphism_2172 ::
  T_IsRingMonomorphism_2164 -> T_IsRingHomomorphism_2124
d_isRingHomomorphism_2172 v0
  = case coe v0 of
      C_IsRingMonomorphism'46'constructor_37231 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism.injective
d_injective_2174 ::
  T_IsRingMonomorphism_2164 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2174 v0
  = case coe v0 of
      C_IsRingMonomorphism'46'constructor_37231 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.*-homo
d_'42''45'homo_2178 ::
  T_IsRingMonomorphism_2164 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_2178 v0
  = coe
      d_'42''45'homo_874
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe
            d_isSemiringHomomorphism_2132
            (coe d_isRingHomomorphism_2172 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_2180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingMonomorphism_2164 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_2180 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_2180 v7
du_'42''45'isMagmaHomomorphism_2180 ::
  T_IsRingMonomorphism_2164 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_2180 v0
  = let v1 = d_isRingHomomorphism_2172 (coe v0) in
    let v2 = d_isSemiringHomomorphism_2132 (coe v1) in
    coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_1254 (coe v2))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.*-isMonoidHomomorphism
d_'42''45'isMonoidHomomorphism_2182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingMonomorphism_2164 -> T_IsMonoidHomomorphism_266
d_'42''45'isMonoidHomomorphism_2182 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidHomomorphism_2182 v7
du_'42''45'isMonoidHomomorphism_2182 ::
  T_IsRingMonomorphism_2164 -> T_IsMonoidHomomorphism_266
du_'42''45'isMonoidHomomorphism_2182 v0
  = let v1 = d_isRingHomomorphism_2172 (coe v0) in
    coe
      du_'42''45'isMonoidHomomorphism_1276
      (coe d_isSemiringHomomorphism_2132 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.homo
d_homo_2184 ::
  T_IsRingMonomorphism_2164 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2184 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe
                  d_isSemiringHomomorphism_2132
                  (coe d_isRingHomomorphism_2172 (coe v0))))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.+-isGroupHomomorphism
d_'43''45'isGroupHomomorphism_2186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingMonomorphism_2164 -> T_IsGroupHomomorphism_554
d_'43''45'isGroupHomomorphism_2186 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isGroupHomomorphism_2186 v7
du_'43''45'isGroupHomomorphism_2186 ::
  T_IsRingMonomorphism_2164 -> T_IsGroupHomomorphism_554
du_'43''45'isGroupHomomorphism_2186 v0
  = coe
      du_'43''45'isGroupHomomorphism_2160
      (coe d_isRingHomomorphism_2172 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_2188 ::
  T_IsRingMonomorphism_2164 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2188 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe
               d_isSemiringHomomorphism_2132
               (coe d_isRingHomomorphism_2172 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_2190 ::
  T_IsRingMonomorphism_2164 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_2190 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe
            d_isSemiringHomomorphism_2132
            (coe d_isRingHomomorphism_2172 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.-‿homo
d_'45''8255'homo_2192 ::
  T_IsRingMonomorphism_2164 -> AgdaAny -> AgdaAny
d_'45''8255'homo_2192 v0
  = coe
      d_'45''8255'homo_2134 (coe d_isRingHomomorphism_2172 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.ε-homo
d_ε'45'homo_2194 :: T_IsRingMonomorphism_2164 -> AgdaAny
d_ε'45'homo_2194 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe
               d_isSemiringHomomorphism_2132
               (coe d_isRingHomomorphism_2172 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.1#-homo
d_1'35''45'homo_2196 :: T_IsRingMonomorphism_2164 -> AgdaAny
d_1'35''45'homo_2196 v0
  = coe
      d_1'35''45'homo_1256
      (coe
         d_isSemiringHomomorphism_2132
         (coe d_isRingHomomorphism_2172 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_2198 ::
  T_IsRingMonomorphism_2164 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_2198 v0
  = coe
      d_isNearSemiringHomomorphism_1254
      (coe
         d_isSemiringHomomorphism_2132
         (coe d_isRingHomomorphism_2172 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.isRelHomomorphism
d_isRelHomomorphism_2200 ::
  T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2200 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe
                  d_isSemiringHomomorphism_2132
                  (coe d_isRingHomomorphism_2172 (coe v0))))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.isSemiringHomomorphism
d_isSemiringHomomorphism_2202 ::
  T_IsRingMonomorphism_2164 -> T_IsSemiringHomomorphism_1246
d_isSemiringHomomorphism_2202 v0
  = coe
      d_isSemiringHomomorphism_2132
      (coe d_isRingHomomorphism_2172 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.cong
d_cong_2204 ::
  T_IsRingMonomorphism_2164 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2204 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe
                  d_isNearSemiringHomomorphism_1254
                  (coe
                     d_isSemiringHomomorphism_2132
                     (coe d_isRingHomomorphism_2172 (coe v0)))))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism.isSemiringMonomorphism
d_isSemiringMonomorphism_2206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingMonomorphism_2164 -> T_IsSemiringMonomorphism_1280
d_isSemiringMonomorphism_2206 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isSemiringMonomorphism_2206 v7
du_isSemiringMonomorphism_2206 ::
  T_IsRingMonomorphism_2164 -> T_IsSemiringMonomorphism_1280
du_isSemiringMonomorphism_2206 v0
  = coe
      C_IsSemiringMonomorphism'46'constructor_22781
      (coe
         d_isSemiringHomomorphism_2132
         (coe d_isRingHomomorphism_2172 (coe v0)))
      (coe d_injective_2174 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism.+-isGroupMonomorphism
d_'43''45'isGroupMonomorphism_2208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingMonomorphism_2164 -> T_IsGroupMonomorphism_580
d_'43''45'isGroupMonomorphism_2208 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isGroupMonomorphism_2208 v7
du_'43''45'isGroupMonomorphism_2208 ::
  T_IsRingMonomorphism_2164 -> T_IsGroupMonomorphism_580
du_'43''45'isGroupMonomorphism_2208 v0
  = coe
      C_IsGroupMonomorphism'46'constructor_11055
      (coe
         du_'43''45'isGroupHomomorphism_2160
         (coe d_isRingHomomorphism_2172 (coe v0)))
      (coe d_injective_2174 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.isMagmaMonomorphism
d_isMagmaMonomorphism_2212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingMonomorphism_2164 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_2212 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_2212 v7
du_isMagmaMonomorphism_2212 ::
  T_IsRingMonomorphism_2164 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_2212 v0
  = let v1 = coe du_'43''45'isGroupMonomorphism_2208 (coe v0) in
    coe
      du_isMagmaMonomorphism_312
      (coe du_isMonoidMonomorphism_608 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.isMonoidMonomorphism
d_isMonoidMonomorphism_2214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingMonomorphism_2164 -> T_IsMonoidMonomorphism_288
d_isMonoidMonomorphism_2214 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMonoidMonomorphism_2214 v7
du_isMonoidMonomorphism_2214 ::
  T_IsRingMonomorphism_2164 -> T_IsMonoidMonomorphism_288
du_isMonoidMonomorphism_2214 v0
  = coe
      du_isMonoidMonomorphism_608
      (coe du_'43''45'isGroupMonomorphism_2208 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.isRelMonomorphism
d_isRelMonomorphism_2216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2216 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2216 v7
du_isRelMonomorphism_2216 ::
  T_IsRingMonomorphism_2164 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2216 v0
  = let v1 = coe du_'43''45'isGroupMonomorphism_2208 (coe v0) in
    let v2 = coe du_isMonoidMonomorphism_608 (coe v1) in
    coe
      du_isRelMonomorphism_114 (coe du_isMagmaMonomorphism_312 (coe v2))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism.*-isMonoidMonomorphism
d_'42''45'isMonoidMonomorphism_2218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingMonomorphism_2164 -> T_IsMonoidMonomorphism_288
d_'42''45'isMonoidMonomorphism_2218 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidMonomorphism_2218 v7
du_'42''45'isMonoidMonomorphism_2218 ::
  T_IsRingMonomorphism_2164 -> T_IsMonoidMonomorphism_288
du_'42''45'isMonoidMonomorphism_2218 v0
  = coe
      C_IsMonoidMonomorphism'46'constructor_6057
      (coe
         du_'42''45'isMonoidHomomorphism_1276
         (coe
            d_isSemiringHomomorphism_2132
            (coe d_isRingHomomorphism_2172 (coe v0))))
      (coe d_injective_2174 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingMonomorphism._.isMagmaMonomorphism
d_isMagmaMonomorphism_2222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingMonomorphism_2164 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_2222 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_2222 v7
du_isMagmaMonomorphism_2222 ::
  T_IsRingMonomorphism_2164 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_2222 v0
  = coe
      du_isMagmaMonomorphism_312
      (coe du_'42''45'isMonoidMonomorphism_2218 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism
d_IsRingIsomorphism_2226 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsRingIsomorphism_2226
  = C_IsRingIsomorphism'46'constructor_39613 T_IsRingMonomorphism_2164
                                             (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism.isRingMonomorphism
d_isRingMonomorphism_2234 ::
  T_IsRingIsomorphism_2226 -> T_IsRingMonomorphism_2164
d_isRingMonomorphism_2234 v0
  = case coe v0 of
      C_IsRingIsomorphism'46'constructor_39613 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism.surjective
d_surjective_2236 ::
  T_IsRingIsomorphism_2226 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_2236 v0
  = case coe v0 of
      C_IsRingIsomorphism'46'constructor_39613 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.*-homo
d_'42''45'homo_2240 ::
  T_IsRingIsomorphism_2226 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'homo_2240 v0
  = coe
      d_'42''45'homo_874
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe
            d_isSemiringHomomorphism_2132
            (coe
               d_isRingHomomorphism_2172
               (coe d_isRingMonomorphism_2234 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.*-isMagmaHomomorphism
d_'42''45'isMagmaHomomorphism_2242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsMagmaHomomorphism_76
d_'42''45'isMagmaHomomorphism_2242 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagmaHomomorphism_2242 v7
du_'42''45'isMagmaHomomorphism_2242 ::
  T_IsRingIsomorphism_2226 -> T_IsMagmaHomomorphism_76
du_'42''45'isMagmaHomomorphism_2242 v0
  = let v1 = d_isRingMonomorphism_2234 (coe v0) in
    let v2 = d_isRingHomomorphism_2172 (coe v1) in
    let v3 = d_isSemiringHomomorphism_2132 (coe v2) in
    coe
      du_'42''45'isMagmaHomomorphism_888
      (coe d_isNearSemiringHomomorphism_1254 (coe v3))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isMagmaMonomorphism
d_isMagmaMonomorphism_2244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_2244 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaMonomorphism_2244 v7
du_isMagmaMonomorphism_2244 ::
  T_IsRingIsomorphism_2226 -> T_IsMagmaMonomorphism_94
du_isMagmaMonomorphism_2244 v0
  = let v1 = d_isRingMonomorphism_2234 (coe v0) in
    coe
      du_isMagmaMonomorphism_312
      (coe du_'42''45'isMonoidMonomorphism_2218 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.*-isMonoidHomomorphism
d_'42''45'isMonoidHomomorphism_2246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsMonoidHomomorphism_266
d_'42''45'isMonoidHomomorphism_2246 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidHomomorphism_2246 v7
du_'42''45'isMonoidHomomorphism_2246 ::
  T_IsRingIsomorphism_2226 -> T_IsMonoidHomomorphism_266
du_'42''45'isMonoidHomomorphism_2246 v0
  = let v1 = d_isRingMonomorphism_2234 (coe v0) in
    let v2 = d_isRingHomomorphism_2172 (coe v1) in
    coe
      du_'42''45'isMonoidHomomorphism_1276
      (coe d_isSemiringHomomorphism_2132 (coe v2))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.*-isMonoidMonomorphism
d_'42''45'isMonoidMonomorphism_2248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsMonoidMonomorphism_288
d_'42''45'isMonoidMonomorphism_2248 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidMonomorphism_2248 v7
du_'42''45'isMonoidMonomorphism_2248 ::
  T_IsRingIsomorphism_2226 -> T_IsMonoidMonomorphism_288
du_'42''45'isMonoidMonomorphism_2248 v0
  = coe
      du_'42''45'isMonoidMonomorphism_2218
      (coe d_isRingMonomorphism_2234 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.homo
d_homo_2250 ::
  T_IsRingIsomorphism_2226 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2250 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe
                  d_isSemiringHomomorphism_2132
                  (coe
                     d_isRingHomomorphism_2172
                     (coe d_isRingMonomorphism_2234 (coe v0)))))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.+-isGroupHomomorphism
d_'43''45'isGroupHomomorphism_2252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsGroupHomomorphism_554
d_'43''45'isGroupHomomorphism_2252 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isGroupHomomorphism_2252 v7
du_'43''45'isGroupHomomorphism_2252 ::
  T_IsRingIsomorphism_2226 -> T_IsGroupHomomorphism_554
du_'43''45'isGroupHomomorphism_2252 v0
  = let v1 = d_isRingMonomorphism_2234 (coe v0) in
    coe
      du_'43''45'isGroupHomomorphism_2160
      (coe d_isRingHomomorphism_2172 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.+-isGroupMonomorphism
d_'43''45'isGroupMonomorphism_2254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsGroupMonomorphism_580
d_'43''45'isGroupMonomorphism_2254 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isGroupMonomorphism_2254 v7
du_'43''45'isGroupMonomorphism_2254 ::
  T_IsRingIsomorphism_2226 -> T_IsGroupMonomorphism_580
du_'43''45'isGroupMonomorphism_2254 v0
  = coe
      du_'43''45'isGroupMonomorphism_2208
      (coe d_isRingMonomorphism_2234 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isMagmaHomomorphism
d_isMagmaHomomorphism_2256 ::
  T_IsRingIsomorphism_2226 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2256 v0
  = coe
      d_isMagmaHomomorphism_274
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe
               d_isSemiringHomomorphism_2132
               (coe
                  d_isRingHomomorphism_2172
                  (coe d_isRingMonomorphism_2234 (coe v0))))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.+-isMonoidHomomorphism
d_'43''45'isMonoidHomomorphism_2258 ::
  T_IsRingIsomorphism_2226 -> T_IsMonoidHomomorphism_266
d_'43''45'isMonoidHomomorphism_2258 v0
  = coe
      d_'43''45'isMonoidHomomorphism_872
      (coe
         d_isNearSemiringHomomorphism_1254
         (coe
            d_isSemiringHomomorphism_2132
            (coe
               d_isRingHomomorphism_2172
               (coe d_isRingMonomorphism_2234 (coe v0)))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.-‿homo
d_'45''8255'homo_2260 ::
  T_IsRingIsomorphism_2226 -> AgdaAny -> AgdaAny
d_'45''8255'homo_2260 v0
  = coe
      d_'45''8255'homo_2134
      (coe
         d_isRingHomomorphism_2172 (coe d_isRingMonomorphism_2234 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.ε-homo
d_ε'45'homo_2262 :: T_IsRingIsomorphism_2226 -> AgdaAny
d_ε'45'homo_2262 v0
  = coe
      d_ε'45'homo_276
      (coe
         d_'43''45'isMonoidHomomorphism_872
         (coe
            d_isNearSemiringHomomorphism_1254
            (coe
               d_isSemiringHomomorphism_2132
               (coe
                  d_isRingHomomorphism_2172
                  (coe d_isRingMonomorphism_2234 (coe v0))))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.1#-homo
d_1'35''45'homo_2264 :: T_IsRingIsomorphism_2226 -> AgdaAny
d_1'35''45'homo_2264 v0
  = coe
      d_1'35''45'homo_1256
      (coe
         d_isSemiringHomomorphism_2132
         (coe
            d_isRingHomomorphism_2172
            (coe d_isRingMonomorphism_2234 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.injective
d_injective_2266 ::
  T_IsRingIsomorphism_2226 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2266 v0
  = coe d_injective_2174 (coe d_isRingMonomorphism_2234 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isNearSemiringHomomorphism
d_isNearSemiringHomomorphism_2268 ::
  T_IsRingIsomorphism_2226 -> T_IsNearSemiringHomomorphism_864
d_isNearSemiringHomomorphism_2268 v0
  = coe
      d_isNearSemiringHomomorphism_1254
      (coe
         d_isSemiringHomomorphism_2132
         (coe
            d_isRingHomomorphism_2172
            (coe d_isRingMonomorphism_2234 (coe v0))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isRelHomomorphism
d_isRelHomomorphism_2270 ::
  T_IsRingIsomorphism_2226 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2270 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_274
         (coe
            d_'43''45'isMonoidHomomorphism_872
            (coe
               d_isNearSemiringHomomorphism_1254
               (coe
                  d_isSemiringHomomorphism_2132
                  (coe
                     d_isRingHomomorphism_2172
                     (coe d_isRingMonomorphism_2234 (coe v0)))))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isRingHomomorphism
d_isRingHomomorphism_2272 ::
  T_IsRingIsomorphism_2226 -> T_IsRingHomomorphism_2124
d_isRingHomomorphism_2272 v0
  = coe
      d_isRingHomomorphism_2172 (coe d_isRingMonomorphism_2234 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isSemiringHomomorphism
d_isSemiringHomomorphism_2274 ::
  T_IsRingIsomorphism_2226 -> T_IsSemiringHomomorphism_1246
d_isSemiringHomomorphism_2274 v0
  = coe
      d_isSemiringHomomorphism_2132
      (coe
         d_isRingHomomorphism_2172 (coe d_isRingMonomorphism_2234 (coe v0)))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isSemiringMonomorphism
d_isSemiringMonomorphism_2276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsSemiringMonomorphism_1280
d_isSemiringMonomorphism_2276 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isSemiringMonomorphism_2276 v7
du_isSemiringMonomorphism_2276 ::
  T_IsRingIsomorphism_2226 -> T_IsSemiringMonomorphism_1280
du_isSemiringMonomorphism_2276 v0
  = coe
      du_isSemiringMonomorphism_2206
      (coe d_isRingMonomorphism_2234 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.cong
d_cong_2278 ::
  T_IsRingIsomorphism_2226 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2278 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_274
            (coe
               d_'43''45'isMonoidHomomorphism_872
               (coe
                  d_isNearSemiringHomomorphism_1254
                  (coe
                     d_isSemiringHomomorphism_2132
                     (coe
                        d_isRingHomomorphism_2172
                        (coe d_isRingMonomorphism_2234 (coe v0))))))))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism.isSemiringIsomorphism
d_isSemiringIsomorphism_2280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsSemiringIsomorphism_1328
d_isSemiringIsomorphism_2280 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isSemiringIsomorphism_2280 v7
du_isSemiringIsomorphism_2280 ::
  T_IsRingIsomorphism_2226 -> T_IsSemiringIsomorphism_1328
du_isSemiringIsomorphism_2280 v0
  = coe
      C_IsSemiringIsomorphism'46'constructor_24539
      (coe
         du_isSemiringMonomorphism_2206
         (coe d_isRingMonomorphism_2234 (coe v0)))
      (coe d_surjective_2236 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism.+-isGroupIsomorphism
d_'43''45'isGroupIsomorphism_2282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsGroupIsomorphism_618
d_'43''45'isGroupIsomorphism_2282 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'43''45'isGroupIsomorphism_2282 v7
du_'43''45'isGroupIsomorphism_2282 ::
  T_IsRingIsomorphism_2226 -> T_IsGroupIsomorphism_618
du_'43''45'isGroupIsomorphism_2282 v0
  = coe
      C_IsGroupIsomorphism'46'constructor_12289
      (coe
         du_'43''45'isGroupMonomorphism_2208
         (coe d_isRingMonomorphism_2234 (coe v0)))
      (coe d_surjective_2236 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isMagmaIsomorphism
d_isMagmaIsomorphism_2286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_2286 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaIsomorphism_2286 v7
du_isMagmaIsomorphism_2286 ::
  T_IsRingIsomorphism_2226 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_2286 v0
  = let v1 = coe du_'43''45'isGroupIsomorphism_2282 (coe v0) in
    coe
      du_isMagmaIsomorphism_352 (coe du_isMonoidIsomorphism_656 (coe v1))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isMonoidIsomorphism
d_isMonoidIsomorphism_2288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsMonoidIsomorphism_320
d_isMonoidIsomorphism_2288 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMonoidIsomorphism_2288 v7
du_isMonoidIsomorphism_2288 ::
  T_IsRingIsomorphism_2226 -> T_IsMonoidIsomorphism_320
du_isMonoidIsomorphism_2288 v0
  = coe
      du_isMonoidIsomorphism_656
      (coe du_'43''45'isGroupIsomorphism_2282 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isRelIsomorphism
d_isRelIsomorphism_2290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_2290 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_2290 v7
du_isRelIsomorphism_2290 ::
  T_IsRingIsomorphism_2226 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_2290 v0
  = let v1 = coe du_'43''45'isGroupIsomorphism_2282 (coe v0) in
    let v2 = coe du_isMonoidIsomorphism_656 (coe v1) in
    coe
      du_isRelIsomorphism_144 (coe du_isMagmaIsomorphism_352 (coe v2))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism.*-isMonoidIsomorphism
d_'42''45'isMonoidIsomorphism_2292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsMonoidIsomorphism_320
d_'42''45'isMonoidIsomorphism_2292 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMonoidIsomorphism_2292 v7
du_'42''45'isMonoidIsomorphism_2292 ::
  T_IsRingIsomorphism_2226 -> T_IsMonoidIsomorphism_320
du_'42''45'isMonoidIsomorphism_2292 v0
  = coe
      C_IsMonoidIsomorphism'46'constructor_7115
      (coe
         du_'42''45'isMonoidMonomorphism_2218
         (coe d_isRingMonomorphism_2234 (coe v0)))
      (coe d_surjective_2236 (coe v0))
-- Algebra.Morphism.Structures.RingMorphisms.IsRingIsomorphism._.isMagmaIsomorphism
d_isMagmaIsomorphism_2296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  (AgdaAny -> AgdaAny) ->
  T_IsRingIsomorphism_2226 -> T_IsMagmaIsomorphism_118
d_isMagmaIsomorphism_2296 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isMagmaIsomorphism_2296 v7
du_isMagmaIsomorphism_2296 ::
  T_IsRingIsomorphism_2226 -> T_IsMagmaIsomorphism_118
du_isMagmaIsomorphism_2296 v0
  = coe
      du_isMagmaIsomorphism_352
      (coe du_'42''45'isMonoidIsomorphism_2292 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms._._//_
d__'47''47'__2314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'47''47'__2314 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'47''47'__2314 v4
du__'47''47'__2314 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'47''47'__2314 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'47''47'__320 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms._._\\_
d__'92''92'__2316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'92''92'__2316 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'92''92'__2316 v4
du__'92''92'__2316 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'92''92'__2316 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'92''92'__318 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms._._∙_
d__'8729'__2318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__2318 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du__'8729'__2318 v4
du__'8729'__2318 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8729'__2318 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__316 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms._._≈_
d__'8776'__2320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__2320 = erased
-- Algebra.Morphism.Structures.QuasigroupMorphisms._.Carrier
d_Carrier_2326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 -> ()
d_Carrier_2326 = erased
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaHomomorphism
d_IsMagmaHomomorphism_2354 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaIsomorphism
d_IsMagmaIsomorphism_2356 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaMonomorphism
d_IsMagmaMonomorphism_2358 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaHomomorphism.homo
d_homo_2362 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2362 v0 = coe d_homo_86 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_2364 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2364 v0 = coe d_isRelHomomorphism_84 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaHomomorphism.cong
d_cong_2366 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2366 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaIsomorphism.homo
d_homo_2370 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2370 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaIsomorphism.injective
d_injective_2372 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2372 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2374 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2374 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_2376 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_2376 v0
  = coe d_isMagmaMonomorphism_126 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_2378 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2378 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_2380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_2380 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_2380
du_isRelIsomorphism_2380 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_2380 v0 v1 = coe du_isRelIsomorphism_144 v1
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_2382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2382 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2382 v7
du_isRelMonomorphism_2382 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2382 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaIsomorphism.surjective
d_surjective_2384 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_2384 v0 = coe d_surjective_128 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaIsomorphism.cong
d_cong_2386 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2386 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaMonomorphism.homo
d_homo_2390 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2390 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaMonomorphism.injective
d_injective_2392 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2392 v0 = coe d_injective_104 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2394 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2394 v0
  = coe d_isMagmaHomomorphism_102 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_2396 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2396 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_2398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2398 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_2398
du_isRelMonomorphism_2398 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2398 v0 v1 = coe du_isRelMonomorphism_114 v1
-- Algebra.Morphism.Structures.QuasigroupMorphisms.∙.IsMagmaMonomorphism.cong
d_cong_2400 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2400 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaHomomorphism
d_IsMagmaHomomorphism_2404 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaIsomorphism
d_IsMagmaIsomorphism_2406 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaMonomorphism
d_IsMagmaMonomorphism_2408 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaHomomorphism.homo
d_homo_2412 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2412 v0 = coe d_homo_86 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_2414 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2414 v0 = coe d_isRelHomomorphism_84 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaHomomorphism.cong
d_cong_2416 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2416 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaIsomorphism.homo
d_homo_2420 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2420 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaIsomorphism.injective
d_injective_2422 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2422 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2424 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2424 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_2426 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_2426 v0
  = coe d_isMagmaMonomorphism_126 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_2428 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2428 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_2430 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_2430 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_2430
du_isRelIsomorphism_2430 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_2430 v0 v1 = coe du_isRelIsomorphism_144 v1
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_2432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2432 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2432 v7
du_isRelMonomorphism_2432 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2432 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaIsomorphism.surjective
d_surjective_2434 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_2434 v0 = coe d_surjective_128 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaIsomorphism.cong
d_cong_2436 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2436 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaMonomorphism.homo
d_homo_2440 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2440 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaMonomorphism.injective
d_injective_2442 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2442 v0 = coe d_injective_104 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2444 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2444 v0
  = coe d_isMagmaHomomorphism_102 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_2446 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2446 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_2448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2448 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_2448
du_isRelMonomorphism_2448 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2448 v0 v1 = coe du_isRelMonomorphism_114 v1
-- Algebra.Morphism.Structures.QuasigroupMorphisms.\\.IsMagmaMonomorphism.cong
d_cong_2450 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2450 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaHomomorphism
d_IsMagmaHomomorphism_2454 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaIsomorphism
d_IsMagmaIsomorphism_2456 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaMonomorphism
d_IsMagmaMonomorphism_2458 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaHomomorphism.homo
d_homo_2462 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2462 v0 = coe d_homo_86 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_2464 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2464 v0 = coe d_isRelHomomorphism_84 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaHomomorphism.cong
d_cong_2466 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2466 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaIsomorphism.homo
d_homo_2470 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2470 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaIsomorphism.injective
d_injective_2472 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2472 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2474 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2474 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_2476 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_2476 v0
  = coe d_isMagmaMonomorphism_126 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_2478 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2478 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_2480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_2480 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_2480
du_isRelIsomorphism_2480 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_2480 v0 v1 = coe du_isRelIsomorphism_144 v1
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_2482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2482 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2482 v7
du_isRelMonomorphism_2482 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2482 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaIsomorphism.surjective
d_surjective_2484 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_2484 v0 = coe d_surjective_128 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaIsomorphism.cong
d_cong_2486 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2486 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaMonomorphism.homo
d_homo_2490 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2490 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaMonomorphism.injective
d_injective_2492 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2492 v0 = coe d_injective_104 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2494 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2494 v0
  = coe d_isMagmaHomomorphism_102 (coe v0)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_2496 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2496 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_2498 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2498 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_2498
du_isRelMonomorphism_2498 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2498 v0 v1 = coe du_isRelMonomorphism_114 v1
-- Algebra.Morphism.Structures.QuasigroupMorphisms.//.IsMagmaMonomorphism.cong
d_cong_2500 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2500 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms._.Homomorphic₂
d_Homomorphic'8322'_2508 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Homomorphic'8322'_2508 = erased
-- Algebra.Morphism.Structures.QuasigroupMorphisms._.Injective
d_Injective_2518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_2518 = erased
-- Algebra.Morphism.Structures.QuasigroupMorphisms._.Surjective
d_Surjective_2526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_2526 = erased
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupHomomorphism
d_IsQuasigroupHomomorphism_2530 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsQuasigroupHomomorphism_2530
  = C_IsQuasigroupHomomorphism'46'constructor_44461 MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
                                                    (AgdaAny -> AgdaAny -> AgdaAny)
                                                    (AgdaAny -> AgdaAny -> AgdaAny)
                                                    (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupHomomorphism.isRelHomomorphism
d_isRelHomomorphism_2542 ::
  T_IsQuasigroupHomomorphism_2530 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2542 v0
  = case coe v0 of
      C_IsQuasigroupHomomorphism'46'constructor_44461 v1 v2 v3 v4
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupHomomorphism.∙-homo
d_'8729''45'homo_2544 ::
  T_IsQuasigroupHomomorphism_2530 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'homo_2544 v0
  = case coe v0 of
      C_IsQuasigroupHomomorphism'46'constructor_44461 v1 v2 v3 v4
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupHomomorphism.\\-homo
d_'92''92''45'homo_2546 ::
  T_IsQuasigroupHomomorphism_2530 -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'homo_2546 v0
  = case coe v0 of
      C_IsQuasigroupHomomorphism'46'constructor_44461 v1 v2 v3 v4
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupHomomorphism.//-homo
d_'47''47''45'homo_2548 ::
  T_IsQuasigroupHomomorphism_2530 -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'homo_2548 v0
  = case coe v0 of
      C_IsQuasigroupHomomorphism'46'constructor_44461 v1 v2 v3 v4
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupHomomorphism._.cong
d_cong_2552 ::
  T_IsQuasigroupHomomorphism_2530 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2552 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_2542 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupHomomorphism.∙-isMagmaHomomorphism
d_'8729''45'isMagmaHomomorphism_2554 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
d_'8729''45'isMagmaHomomorphism_2554 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaHomomorphism_2554 v7
du_'8729''45'isMagmaHomomorphism_2554 ::
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
du_'8729''45'isMagmaHomomorphism_2554 v0
  = coe
      C_IsMagmaHomomorphism'46'constructor_1049
      (coe d_isRelHomomorphism_2542 (coe v0))
      (coe d_'8729''45'homo_2544 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupHomomorphism.\\-isMagmaHomomorphism
d_'92''92''45'isMagmaHomomorphism_2556 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
d_'92''92''45'isMagmaHomomorphism_2556 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaHomomorphism_2556 v7
du_'92''92''45'isMagmaHomomorphism_2556 ::
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
du_'92''92''45'isMagmaHomomorphism_2556 v0
  = coe
      C_IsMagmaHomomorphism'46'constructor_1049
      (coe d_isRelHomomorphism_2542 (coe v0))
      (coe d_'92''92''45'homo_2546 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupHomomorphism.//-isMagmaHomomorphism
d_'47''47''45'isMagmaHomomorphism_2558 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
d_'47''47''45'isMagmaHomomorphism_2558 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaHomomorphism_2558 v7
du_'47''47''45'isMagmaHomomorphism_2558 ::
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
du_'47''47''45'isMagmaHomomorphism_2558 v0
  = coe
      C_IsMagmaHomomorphism'46'constructor_1049
      (coe d_isRelHomomorphism_2542 (coe v0))
      (coe d_'47''47''45'homo_2548 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism
d_IsQuasigroupMonomorphism_2562 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsQuasigroupMonomorphism_2562
  = C_IsQuasigroupMonomorphism'46'constructor_45945 T_IsQuasigroupHomomorphism_2530
                                                    (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism.isQuasigroupHomomorphism
d_isQuasigroupHomomorphism_2570 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsQuasigroupHomomorphism_2530
d_isQuasigroupHomomorphism_2570 v0
  = case coe v0 of
      C_IsQuasigroupMonomorphism'46'constructor_45945 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism.injective
d_injective_2572 ::
  T_IsQuasigroupMonomorphism_2562 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2572 v0
  = case coe v0 of
      C_IsQuasigroupMonomorphism'46'constructor_45945 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism._.//-homo
d_'47''47''45'homo_2576 ::
  T_IsQuasigroupMonomorphism_2562 -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'homo_2576 v0
  = coe
      d_'47''47''45'homo_2548
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism._.//-isMagmaHomomorphism
d_'47''47''45'isMagmaHomomorphism_2578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
d_'47''47''45'isMagmaHomomorphism_2578 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaHomomorphism_2578 v7
du_'47''47''45'isMagmaHomomorphism_2578 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
du_'47''47''45'isMagmaHomomorphism_2578 v0
  = coe
      du_'47''47''45'isMagmaHomomorphism_2558
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism._.\\-homo
d_'92''92''45'homo_2580 ::
  T_IsQuasigroupMonomorphism_2562 -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'homo_2580 v0
  = coe
      d_'92''92''45'homo_2546
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism._.\\-isMagmaHomomorphism
d_'92''92''45'isMagmaHomomorphism_2582 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
d_'92''92''45'isMagmaHomomorphism_2582 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaHomomorphism_2582 v7
du_'92''92''45'isMagmaHomomorphism_2582 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
du_'92''92''45'isMagmaHomomorphism_2582 v0
  = coe
      du_'92''92''45'isMagmaHomomorphism_2556
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism._.isRelHomomorphism
d_isRelHomomorphism_2584 ::
  T_IsQuasigroupMonomorphism_2562 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2584 v0
  = coe
      d_isRelHomomorphism_2542
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism._.∙-homo
d_'8729''45'homo_2586 ::
  T_IsQuasigroupMonomorphism_2562 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'homo_2586 v0
  = coe
      d_'8729''45'homo_2544
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism._.∙-isMagmaHomomorphism
d_'8729''45'isMagmaHomomorphism_2588 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
d_'8729''45'isMagmaHomomorphism_2588 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaHomomorphism_2588 v7
du_'8729''45'isMagmaHomomorphism_2588 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
du_'8729''45'isMagmaHomomorphism_2588 v0
  = coe
      du_'8729''45'isMagmaHomomorphism_2554
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism._.cong
d_cong_2590 ::
  T_IsQuasigroupMonomorphism_2562 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2590 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_2542
         (coe d_isQuasigroupHomomorphism_2570 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism.∙-isMagmaMonomorphism
d_'8729''45'isMagmaMonomorphism_2592 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
d_'8729''45'isMagmaMonomorphism_2592 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaMonomorphism_2592 v7
du_'8729''45'isMagmaMonomorphism_2592 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
du_'8729''45'isMagmaMonomorphism_2592 v0
  = coe
      C_IsMagmaMonomorphism'46'constructor_1881
      (coe
         du_'8729''45'isMagmaHomomorphism_2554
         (coe d_isQuasigroupHomomorphism_2570 (coe v0)))
      (coe d_injective_2572 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism.\\-isMagmaMonomorphism
d_'92''92''45'isMagmaMonomorphism_2594 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
d_'92''92''45'isMagmaMonomorphism_2594 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaMonomorphism_2594 v7
du_'92''92''45'isMagmaMonomorphism_2594 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
du_'92''92''45'isMagmaMonomorphism_2594 v0
  = coe
      C_IsMagmaMonomorphism'46'constructor_1881
      (coe
         du_'92''92''45'isMagmaHomomorphism_2556
         (coe d_isQuasigroupHomomorphism_2570 (coe v0)))
      (coe d_injective_2572 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism.//-isMagmaMonomorphism
d_'47''47''45'isMagmaMonomorphism_2596 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
d_'47''47''45'isMagmaMonomorphism_2596 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaMonomorphism_2596 v7
du_'47''47''45'isMagmaMonomorphism_2596 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
du_'47''47''45'isMagmaMonomorphism_2596 v0
  = coe
      C_IsMagmaMonomorphism'46'constructor_1881
      (coe
         du_'47''47''45'isMagmaHomomorphism_2558
         (coe d_isQuasigroupHomomorphism_2570 (coe v0)))
      (coe d_injective_2572 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupMonomorphism._.isRelMonomorphism
d_isRelMonomorphism_2600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2600 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2600 v7
du_isRelMonomorphism_2600 ::
  T_IsQuasigroupMonomorphism_2562 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2600 v0
  = coe
      du_isRelMonomorphism_114
      (coe du_'47''47''45'isMagmaMonomorphism_2596 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism
d_IsQuasigroupIsomorphism_2604 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsQuasigroupIsomorphism_2604
  = C_IsQuasigroupIsomorphism'46'constructor_47763 T_IsQuasigroupMonomorphism_2562
                                                   (AgdaAny ->
                                                    MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism.isQuasigroupMonomorphism
d_isQuasigroupMonomorphism_2612 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsQuasigroupMonomorphism_2562
d_isQuasigroupMonomorphism_2612 v0
  = case coe v0 of
      C_IsQuasigroupIsomorphism'46'constructor_47763 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism.surjective
d_surjective_2614 ::
  T_IsQuasigroupIsomorphism_2604 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_2614 v0
  = case coe v0 of
      C_IsQuasigroupIsomorphism'46'constructor_47763 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.//-homo
d_'47''47''45'homo_2618 ::
  T_IsQuasigroupIsomorphism_2604 -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'homo_2618 v0
  = coe
      d_'47''47''45'homo_2548
      (coe
         d_isQuasigroupHomomorphism_2570
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.//-isMagmaHomomorphism
d_'47''47''45'isMagmaHomomorphism_2620 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
d_'47''47''45'isMagmaHomomorphism_2620 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaHomomorphism_2620 v7
du_'47''47''45'isMagmaHomomorphism_2620 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
du_'47''47''45'isMagmaHomomorphism_2620 v0
  = let v1 = d_isQuasigroupMonomorphism_2612 (coe v0) in
    coe
      du_'47''47''45'isMagmaHomomorphism_2558
      (coe d_isQuasigroupHomomorphism_2570 (coe v1))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.//-isMagmaMonomorphism
d_'47''47''45'isMagmaMonomorphism_2622 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
d_'47''47''45'isMagmaMonomorphism_2622 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaMonomorphism_2622 v7
du_'47''47''45'isMagmaMonomorphism_2622 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
du_'47''47''45'isMagmaMonomorphism_2622 v0
  = coe
      du_'47''47''45'isMagmaMonomorphism_2596
      (coe d_isQuasigroupMonomorphism_2612 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.\\-homo
d_'92''92''45'homo_2624 ::
  T_IsQuasigroupIsomorphism_2604 -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'homo_2624 v0
  = coe
      d_'92''92''45'homo_2546
      (coe
         d_isQuasigroupHomomorphism_2570
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.\\-isMagmaHomomorphism
d_'92''92''45'isMagmaHomomorphism_2626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
d_'92''92''45'isMagmaHomomorphism_2626 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaHomomorphism_2626 v7
du_'92''92''45'isMagmaHomomorphism_2626 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
du_'92''92''45'isMagmaHomomorphism_2626 v0
  = let v1 = d_isQuasigroupMonomorphism_2612 (coe v0) in
    coe
      du_'92''92''45'isMagmaHomomorphism_2556
      (coe d_isQuasigroupHomomorphism_2570 (coe v1))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.\\-isMagmaMonomorphism
d_'92''92''45'isMagmaMonomorphism_2628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
d_'92''92''45'isMagmaMonomorphism_2628 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaMonomorphism_2628 v7
du_'92''92''45'isMagmaMonomorphism_2628 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
du_'92''92''45'isMagmaMonomorphism_2628 v0
  = coe
      du_'92''92''45'isMagmaMonomorphism_2594
      (coe d_isQuasigroupMonomorphism_2612 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.injective
d_injective_2630 ::
  T_IsQuasigroupIsomorphism_2604 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2630 v0
  = coe
      d_injective_2572 (coe d_isQuasigroupMonomorphism_2612 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.isQuasigroupHomomorphism
d_isQuasigroupHomomorphism_2632 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsQuasigroupHomomorphism_2530
d_isQuasigroupHomomorphism_2632 v0
  = coe
      d_isQuasigroupHomomorphism_2570
      (coe d_isQuasigroupMonomorphism_2612 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.isRelHomomorphism
d_isRelHomomorphism_2634 ::
  T_IsQuasigroupIsomorphism_2604 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2634 v0
  = coe
      d_isRelHomomorphism_2542
      (coe
         d_isQuasigroupHomomorphism_2570
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.isRelMonomorphism
d_isRelMonomorphism_2636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2636 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2636 v7
du_isRelMonomorphism_2636 ::
  T_IsQuasigroupIsomorphism_2604 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2636 v0
  = let v1 = d_isQuasigroupMonomorphism_2612 (coe v0) in
    coe
      du_isRelMonomorphism_114
      (coe du_'47''47''45'isMagmaMonomorphism_2596 (coe v1))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.∙-homo
d_'8729''45'homo_2638 ::
  T_IsQuasigroupIsomorphism_2604 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'homo_2638 v0
  = coe
      d_'8729''45'homo_2544
      (coe
         d_isQuasigroupHomomorphism_2570
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.∙-isMagmaHomomorphism
d_'8729''45'isMagmaHomomorphism_2640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
d_'8729''45'isMagmaHomomorphism_2640 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaHomomorphism_2640 v7
du_'8729''45'isMagmaHomomorphism_2640 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
du_'8729''45'isMagmaHomomorphism_2640 v0
  = let v1 = d_isQuasigroupMonomorphism_2612 (coe v0) in
    coe
      du_'8729''45'isMagmaHomomorphism_2554
      (coe d_isQuasigroupHomomorphism_2570 (coe v1))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.∙-isMagmaMonomorphism
d_'8729''45'isMagmaMonomorphism_2642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
d_'8729''45'isMagmaMonomorphism_2642 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaMonomorphism_2642 v7
du_'8729''45'isMagmaMonomorphism_2642 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
du_'8729''45'isMagmaMonomorphism_2642 v0
  = coe
      du_'8729''45'isMagmaMonomorphism_2592
      (coe d_isQuasigroupMonomorphism_2612 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.cong
d_cong_2644 ::
  T_IsQuasigroupIsomorphism_2604 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2644 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_2542
         (coe
            d_isQuasigroupHomomorphism_2570
            (coe d_isQuasigroupMonomorphism_2612 (coe v0))))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism.∙-isMagmaIsomorphism
d_'8729''45'isMagmaIsomorphism_2646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
d_'8729''45'isMagmaIsomorphism_2646 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaIsomorphism_2646 v7
du_'8729''45'isMagmaIsomorphism_2646 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
du_'8729''45'isMagmaIsomorphism_2646 v0
  = coe
      C_IsMagmaIsomorphism'46'constructor_3015
      (coe
         du_'8729''45'isMagmaMonomorphism_2592
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
      (coe d_surjective_2614 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism.\\-isMagmaIsomorphism
d_'92''92''45'isMagmaIsomorphism_2648 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
d_'92''92''45'isMagmaIsomorphism_2648 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                      v7
  = du_'92''92''45'isMagmaIsomorphism_2648 v7
du_'92''92''45'isMagmaIsomorphism_2648 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
du_'92''92''45'isMagmaIsomorphism_2648 v0
  = coe
      C_IsMagmaIsomorphism'46'constructor_3015
      (coe
         du_'92''92''45'isMagmaMonomorphism_2594
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
      (coe d_surjective_2614 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism.//-isMagmaIsomorphism
d_'47''47''45'isMagmaIsomorphism_2650 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
d_'47''47''45'isMagmaIsomorphism_2650 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                      v7
  = du_'47''47''45'isMagmaIsomorphism_2650 v7
du_'47''47''45'isMagmaIsomorphism_2650 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
du_'47''47''45'isMagmaIsomorphism_2650 v0
  = coe
      C_IsMagmaIsomorphism'46'constructor_3015
      (coe
         du_'47''47''45'isMagmaMonomorphism_2596
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
      (coe d_surjective_2614 (coe v0))
-- Algebra.Morphism.Structures.QuasigroupMorphisms.IsQuasigroupIsomorphism._.isRelIsomorphism
d_isRelIsomorphism_2654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawQuasigroup_296 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_2654 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_2654 v7
du_isRelIsomorphism_2654 ::
  T_IsQuasigroupIsomorphism_2604 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_2654 v0
  = coe
      du_isRelIsomorphism_144
      (coe du_'47''47''45'isMagmaIsomorphism_2650 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.Carrier
d_Carrier_2684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 -> ()
d_Carrier_2684 = erased
-- Algebra.Morphism.Structures.LoopMorphisms._.ε
d_ε_2690 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 -> AgdaAny
d_ε_2690 ~v0 ~v1 ~v2 ~v3 v4 ~v5 = du_ε_2690 v4
du_ε_2690 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 -> AgdaAny
du_ε_2690 v0
  = coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_364 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.Homomorphic₀
d_Homomorphic'8320'_2720 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_Homomorphic'8320'_2720 = erased
-- Algebra.Morphism.Structures.LoopMorphisms._.Injective
d_Injective_2734 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) -> ()
d_Injective_2734 = erased
-- Algebra.Morphism.Structures.LoopMorphisms._.Surjective
d_Surjective_2742 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) -> ()
d_Surjective_2742 = erased
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupHomomorphism
d_IsQuasigroupHomomorphism_2746 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism
d_IsQuasigroupIsomorphism_2748 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism
d_IsQuasigroupMonomorphism_2750 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaHomomorphism
d_IsMagmaHomomorphism_2754 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaIsomorphism
d_IsMagmaIsomorphism_2756 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaMonomorphism
d_IsMagmaMonomorphism_2758 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaHomomorphism.homo
d_homo_2762 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2762 v0 = coe d_homo_86 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_2764 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2764 v0 = coe d_isRelHomomorphism_84 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaHomomorphism.cong
d_cong_2766 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2766 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaIsomorphism.homo
d_homo_2770 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2770 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaIsomorphism.injective
d_injective_2772 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2772 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2774 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2774 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_2776 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_2776 v0
  = coe d_isMagmaMonomorphism_126 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_2778 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2778 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_2780 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_2780 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_2780
du_isRelIsomorphism_2780 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_2780 v0 v1 = coe du_isRelIsomorphism_144 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_2782 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2782 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2782 v7
du_isRelMonomorphism_2782 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2782 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaIsomorphism.surjective
d_surjective_2784 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_2784 v0 = coe d_surjective_128 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaIsomorphism.cong
d_cong_2786 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2786 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaMonomorphism.homo
d_homo_2790 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2790 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaMonomorphism.injective
d_injective_2792 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2792 v0 = coe d_injective_104 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2794 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2794 v0
  = coe d_isMagmaHomomorphism_102 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_2796 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2796 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_2798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2798 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_2798
du_isRelMonomorphism_2798 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2798 v0 v1 = coe du_isRelMonomorphism_114 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.//.IsMagmaMonomorphism.cong
d_cong_2800 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2800 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupHomomorphism.//-homo
d_'47''47''45'homo_2804 ::
  T_IsQuasigroupHomomorphism_2530 -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'homo_2804 v0 = coe d_'47''47''45'homo_2548 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupHomomorphism.//-isMagmaHomomorphism
d_'47''47''45'isMagmaHomomorphism_2806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
d_'47''47''45'isMagmaHomomorphism_2806 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'47''47''45'isMagmaHomomorphism_2806
du_'47''47''45'isMagmaHomomorphism_2806 ::
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
du_'47''47''45'isMagmaHomomorphism_2806 v0 v1
  = coe du_'47''47''45'isMagmaHomomorphism_2558 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupHomomorphism.\\-homo
d_'92''92''45'homo_2808 ::
  T_IsQuasigroupHomomorphism_2530 -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'homo_2808 v0 = coe d_'92''92''45'homo_2546 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupHomomorphism.\\-isMagmaHomomorphism
d_'92''92''45'isMagmaHomomorphism_2810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
d_'92''92''45'isMagmaHomomorphism_2810 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'92''92''45'isMagmaHomomorphism_2810
du_'92''92''45'isMagmaHomomorphism_2810 ::
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
du_'92''92''45'isMagmaHomomorphism_2810 v0 v1
  = coe du_'92''92''45'isMagmaHomomorphism_2556 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupHomomorphism.isRelHomomorphism
d_isRelHomomorphism_2812 ::
  T_IsQuasigroupHomomorphism_2530 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2812 v0 = coe d_isRelHomomorphism_2542 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupHomomorphism.∙-homo
d_'8729''45'homo_2814 ::
  T_IsQuasigroupHomomorphism_2530 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'homo_2814 v0 = coe d_'8729''45'homo_2544 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupHomomorphism.∙-isMagmaHomomorphism
d_'8729''45'isMagmaHomomorphism_2816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
d_'8729''45'isMagmaHomomorphism_2816 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'8729''45'isMagmaHomomorphism_2816
du_'8729''45'isMagmaHomomorphism_2816 ::
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupHomomorphism_2530 -> T_IsMagmaHomomorphism_76
du_'8729''45'isMagmaHomomorphism_2816 v0 v1
  = coe du_'8729''45'isMagmaHomomorphism_2554 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupHomomorphism.cong
d_cong_2818 ::
  T_IsQuasigroupHomomorphism_2530 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2818 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_2542 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.//-homo
d_'47''47''45'homo_2822 ::
  T_IsQuasigroupIsomorphism_2604 -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'homo_2822 v0
  = coe
      d_'47''47''45'homo_2548
      (coe
         d_isQuasigroupHomomorphism_2570
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.//-isMagmaHomomorphism
d_'47''47''45'isMagmaHomomorphism_2824 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
d_'47''47''45'isMagmaHomomorphism_2824 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaHomomorphism_2824 v7
du_'47''47''45'isMagmaHomomorphism_2824 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
du_'47''47''45'isMagmaHomomorphism_2824 v0
  = let v1 = d_isQuasigroupMonomorphism_2612 (coe v0) in
    coe
      du_'47''47''45'isMagmaHomomorphism_2558
      (coe d_isQuasigroupHomomorphism_2570 (coe v1))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.//-isMagmaIsomorphism
d_'47''47''45'isMagmaIsomorphism_2826 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
d_'47''47''45'isMagmaIsomorphism_2826 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'47''47''45'isMagmaIsomorphism_2826
du_'47''47''45'isMagmaIsomorphism_2826 ::
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
du_'47''47''45'isMagmaIsomorphism_2826 v0 v1
  = coe du_'47''47''45'isMagmaIsomorphism_2650 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.//-isMagmaMonomorphism
d_'47''47''45'isMagmaMonomorphism_2828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
d_'47''47''45'isMagmaMonomorphism_2828 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaMonomorphism_2828 v7
du_'47''47''45'isMagmaMonomorphism_2828 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
du_'47''47''45'isMagmaMonomorphism_2828 v0
  = coe
      du_'47''47''45'isMagmaMonomorphism_2596
      (coe d_isQuasigroupMonomorphism_2612 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.\\-homo
d_'92''92''45'homo_2830 ::
  T_IsQuasigroupIsomorphism_2604 -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'homo_2830 v0
  = coe
      d_'92''92''45'homo_2546
      (coe
         d_isQuasigroupHomomorphism_2570
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.\\-isMagmaHomomorphism
d_'92''92''45'isMagmaHomomorphism_2832 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
d_'92''92''45'isMagmaHomomorphism_2832 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaHomomorphism_2832 v7
du_'92''92''45'isMagmaHomomorphism_2832 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
du_'92''92''45'isMagmaHomomorphism_2832 v0
  = let v1 = d_isQuasigroupMonomorphism_2612 (coe v0) in
    coe
      du_'92''92''45'isMagmaHomomorphism_2556
      (coe d_isQuasigroupHomomorphism_2570 (coe v1))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.\\-isMagmaIsomorphism
d_'92''92''45'isMagmaIsomorphism_2834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
d_'92''92''45'isMagmaIsomorphism_2834 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'92''92''45'isMagmaIsomorphism_2834
du_'92''92''45'isMagmaIsomorphism_2834 ::
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
du_'92''92''45'isMagmaIsomorphism_2834 v0 v1
  = coe du_'92''92''45'isMagmaIsomorphism_2648 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.\\-isMagmaMonomorphism
d_'92''92''45'isMagmaMonomorphism_2836 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
d_'92''92''45'isMagmaMonomorphism_2836 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaMonomorphism_2836 v7
du_'92''92''45'isMagmaMonomorphism_2836 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
du_'92''92''45'isMagmaMonomorphism_2836 v0
  = coe
      du_'92''92''45'isMagmaMonomorphism_2594
      (coe d_isQuasigroupMonomorphism_2612 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.injective
d_injective_2838 ::
  T_IsQuasigroupIsomorphism_2604 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2838 v0
  = coe
      d_injective_2572 (coe d_isQuasigroupMonomorphism_2612 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.isQuasigroupHomomorphism
d_isQuasigroupHomomorphism_2840 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsQuasigroupHomomorphism_2530
d_isQuasigroupHomomorphism_2840 v0
  = coe
      d_isQuasigroupHomomorphism_2570
      (coe d_isQuasigroupMonomorphism_2612 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.isQuasigroupMonomorphism
d_isQuasigroupMonomorphism_2842 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsQuasigroupMonomorphism_2562
d_isQuasigroupMonomorphism_2842 v0
  = coe d_isQuasigroupMonomorphism_2612 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.isRelHomomorphism
d_isRelHomomorphism_2844 ::
  T_IsQuasigroupIsomorphism_2604 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2844 v0
  = coe
      d_isRelHomomorphism_2542
      (coe
         d_isQuasigroupHomomorphism_2570
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.isRelIsomorphism
d_isRelIsomorphism_2846 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_2846 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelIsomorphism_2846 v7
du_isRelIsomorphism_2846 ::
  T_IsQuasigroupIsomorphism_2604 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_2846 v0
  = coe
      du_isRelIsomorphism_144
      (coe du_'47''47''45'isMagmaIsomorphism_2650 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.isRelMonomorphism
d_isRelMonomorphism_2848 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2848 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2848 v7
du_isRelMonomorphism_2848 ::
  T_IsQuasigroupIsomorphism_2604 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2848 v0
  = let v1 = d_isQuasigroupMonomorphism_2612 (coe v0) in
    coe
      du_isRelMonomorphism_114
      (coe du_'47''47''45'isMagmaMonomorphism_2596 (coe v1))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.surjective
d_surjective_2850 ::
  T_IsQuasigroupIsomorphism_2604 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_2850 v0 = coe d_surjective_2614 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.∙-homo
d_'8729''45'homo_2852 ::
  T_IsQuasigroupIsomorphism_2604 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'homo_2852 v0
  = coe
      d_'8729''45'homo_2544
      (coe
         d_isQuasigroupHomomorphism_2570
         (coe d_isQuasigroupMonomorphism_2612 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.∙-isMagmaHomomorphism
d_'8729''45'isMagmaHomomorphism_2854 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
d_'8729''45'isMagmaHomomorphism_2854 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaHomomorphism_2854 v7
du_'8729''45'isMagmaHomomorphism_2854 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaHomomorphism_76
du_'8729''45'isMagmaHomomorphism_2854 v0
  = let v1 = d_isQuasigroupMonomorphism_2612 (coe v0) in
    coe
      du_'8729''45'isMagmaHomomorphism_2554
      (coe d_isQuasigroupHomomorphism_2570 (coe v1))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.∙-isMagmaIsomorphism
d_'8729''45'isMagmaIsomorphism_2856 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
d_'8729''45'isMagmaIsomorphism_2856 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'8729''45'isMagmaIsomorphism_2856
du_'8729''45'isMagmaIsomorphism_2856 ::
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaIsomorphism_118
du_'8729''45'isMagmaIsomorphism_2856 v0 v1
  = coe du_'8729''45'isMagmaIsomorphism_2646 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.∙-isMagmaMonomorphism
d_'8729''45'isMagmaMonomorphism_2858 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
d_'8729''45'isMagmaMonomorphism_2858 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaMonomorphism_2858 v7
du_'8729''45'isMagmaMonomorphism_2858 ::
  T_IsQuasigroupIsomorphism_2604 -> T_IsMagmaMonomorphism_94
du_'8729''45'isMagmaMonomorphism_2858 v0
  = coe
      du_'8729''45'isMagmaMonomorphism_2592
      (coe d_isQuasigroupMonomorphism_2612 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupIsomorphism.cong
d_cong_2860 ::
  T_IsQuasigroupIsomorphism_2604 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2860 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_2542
         (coe
            d_isQuasigroupHomomorphism_2570
            (coe d_isQuasigroupMonomorphism_2612 (coe v0))))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.//-homo
d_'47''47''45'homo_2864 ::
  T_IsQuasigroupMonomorphism_2562 -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'homo_2864 v0
  = coe
      d_'47''47''45'homo_2548
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.//-isMagmaHomomorphism
d_'47''47''45'isMagmaHomomorphism_2866 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
d_'47''47''45'isMagmaHomomorphism_2866 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaHomomorphism_2866 v7
du_'47''47''45'isMagmaHomomorphism_2866 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
du_'47''47''45'isMagmaHomomorphism_2866 v0
  = coe
      du_'47''47''45'isMagmaHomomorphism_2558
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.//-isMagmaMonomorphism
d_'47''47''45'isMagmaMonomorphism_2868 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
d_'47''47''45'isMagmaMonomorphism_2868 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'47''47''45'isMagmaMonomorphism_2868
du_'47''47''45'isMagmaMonomorphism_2868 ::
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
du_'47''47''45'isMagmaMonomorphism_2868 v0 v1
  = coe du_'47''47''45'isMagmaMonomorphism_2596 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.\\-homo
d_'92''92''45'homo_2870 ::
  T_IsQuasigroupMonomorphism_2562 -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'homo_2870 v0
  = coe
      d_'92''92''45'homo_2546
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.\\-isMagmaHomomorphism
d_'92''92''45'isMagmaHomomorphism_2872 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
d_'92''92''45'isMagmaHomomorphism_2872 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaHomomorphism_2872 v7
du_'92''92''45'isMagmaHomomorphism_2872 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
du_'92''92''45'isMagmaHomomorphism_2872 v0
  = coe
      du_'92''92''45'isMagmaHomomorphism_2556
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.\\-isMagmaMonomorphism
d_'92''92''45'isMagmaMonomorphism_2874 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
d_'92''92''45'isMagmaMonomorphism_2874 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'92''92''45'isMagmaMonomorphism_2874
du_'92''92''45'isMagmaMonomorphism_2874 ::
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
du_'92''92''45'isMagmaMonomorphism_2874 v0 v1
  = coe du_'92''92''45'isMagmaMonomorphism_2594 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.injective
d_injective_2876 ::
  T_IsQuasigroupMonomorphism_2562 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2876 v0 = coe d_injective_2572 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.isQuasigroupHomomorphism
d_isQuasigroupHomomorphism_2878 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsQuasigroupHomomorphism_2530
d_isQuasigroupHomomorphism_2878 v0
  = coe d_isQuasigroupHomomorphism_2570 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.isRelHomomorphism
d_isRelHomomorphism_2880 ::
  T_IsQuasigroupMonomorphism_2562 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2880 v0
  = coe
      d_isRelHomomorphism_2542
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.isRelMonomorphism
d_isRelMonomorphism_2882 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2882 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2882 v7
du_isRelMonomorphism_2882 ::
  T_IsQuasigroupMonomorphism_2562 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2882 v0
  = coe
      du_isRelMonomorphism_114
      (coe du_'47''47''45'isMagmaMonomorphism_2596 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.∙-homo
d_'8729''45'homo_2884 ::
  T_IsQuasigroupMonomorphism_2562 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'homo_2884 v0
  = coe
      d_'8729''45'homo_2544
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.∙-isMagmaHomomorphism
d_'8729''45'isMagmaHomomorphism_2886 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
d_'8729''45'isMagmaHomomorphism_2886 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaHomomorphism_2886 v7
du_'8729''45'isMagmaHomomorphism_2886 ::
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaHomomorphism_76
du_'8729''45'isMagmaHomomorphism_2886 v0
  = coe
      du_'8729''45'isMagmaHomomorphism_2554
      (coe d_isQuasigroupHomomorphism_2570 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.∙-isMagmaMonomorphism
d_'8729''45'isMagmaMonomorphism_2888 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
d_'8729''45'isMagmaMonomorphism_2888 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'8729''45'isMagmaMonomorphism_2888
du_'8729''45'isMagmaMonomorphism_2888 ::
  (AgdaAny -> AgdaAny) ->
  T_IsQuasigroupMonomorphism_2562 -> T_IsMagmaMonomorphism_94
du_'8729''45'isMagmaMonomorphism_2888 v0 v1
  = coe du_'8729''45'isMagmaMonomorphism_2592 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.IsQuasigroupMonomorphism.cong
d_cong_2890 ::
  T_IsQuasigroupMonomorphism_2562 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2890 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_2542
         (coe d_isQuasigroupHomomorphism_2570 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaHomomorphism
d_IsMagmaHomomorphism_2894 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaIsomorphism
d_IsMagmaIsomorphism_2896 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaMonomorphism
d_IsMagmaMonomorphism_2898 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaHomomorphism.homo
d_homo_2902 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2902 v0 = coe d_homo_86 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_2904 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2904 v0 = coe d_isRelHomomorphism_84 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaHomomorphism.cong
d_cong_2906 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2906 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaIsomorphism.homo
d_homo_2910 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2910 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaIsomorphism.injective
d_injective_2912 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2912 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2914 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2914 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_2916 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_2916 v0
  = coe d_isMagmaMonomorphism_126 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_2918 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2918 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_2920 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_2920 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_2920
du_isRelIsomorphism_2920 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_2920 v0 v1 = coe du_isRelIsomorphism_144 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_2922 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2922 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2922 v7
du_isRelMonomorphism_2922 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2922 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaIsomorphism.surjective
d_surjective_2924 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_2924 v0 = coe d_surjective_128 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaIsomorphism.cong
d_cong_2926 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2926 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaMonomorphism.homo
d_homo_2930 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2930 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaMonomorphism.injective
d_injective_2932 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2932 v0 = coe d_injective_104 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2934 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2934 v0
  = coe d_isMagmaHomomorphism_102 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_2936 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2936 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_2938 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2938 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_2938
du_isRelMonomorphism_2938 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2938 v0 v1 = coe du_isRelMonomorphism_114 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.\\.IsMagmaMonomorphism.cong
d_cong_2940 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2940 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaHomomorphism
d_IsMagmaHomomorphism_2944 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaIsomorphism
d_IsMagmaIsomorphism_2946 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaMonomorphism
d_IsMagmaMonomorphism_2948 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaHomomorphism.homo
d_homo_2952 ::
  T_IsMagmaHomomorphism_76 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2952 v0 = coe d_homo_86 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaHomomorphism.isRelHomomorphism
d_isRelHomomorphism_2954 ::
  T_IsMagmaHomomorphism_76 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2954 v0 = coe d_isRelHomomorphism_84 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaHomomorphism.cong
d_cong_2956 ::
  T_IsMagmaHomomorphism_76 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2956 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_84 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaIsomorphism.homo
d_homo_2960 ::
  T_IsMagmaIsomorphism_118 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2960 v0
  = coe
      d_homo_86
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaIsomorphism.injective
d_injective_2962 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2962 v0
  = coe d_injective_104 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaIsomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2964 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2964 v0
  = coe
      d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaIsomorphism.isMagmaMonomorphism
d_isMagmaMonomorphism_2966 ::
  T_IsMagmaIsomorphism_118 -> T_IsMagmaMonomorphism_94
d_isMagmaMonomorphism_2966 v0
  = coe d_isMagmaMonomorphism_126 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaIsomorphism.isRelHomomorphism
d_isRelHomomorphism_2968 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2968 v0
  = coe
      d_isRelHomomorphism_84
      (coe
         d_isMagmaHomomorphism_102 (coe d_isMagmaMonomorphism_126 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaIsomorphism.isRelIsomorphism
d_isRelIsomorphism_2970 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_2970 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelIsomorphism_2970
du_isRelIsomorphism_2970 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
du_isRelIsomorphism_2970 v0 v1 = coe du_isRelIsomorphism_144 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaIsomorphism.isRelMonomorphism
d_isRelMonomorphism_2972 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2972 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isRelMonomorphism_2972 v7
du_isRelMonomorphism_2972 ::
  T_IsMagmaIsomorphism_118 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2972 v0
  = coe
      du_isRelMonomorphism_114 (coe d_isMagmaMonomorphism_126 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaIsomorphism.surjective
d_surjective_2974 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_2974 v0 = coe d_surjective_128 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaIsomorphism.cong
d_cong_2976 ::
  T_IsMagmaIsomorphism_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2976 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84
         (coe
            d_isMagmaHomomorphism_102
            (coe d_isMagmaMonomorphism_126 (coe v0))))
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaMonomorphism.homo
d_homo_2980 ::
  T_IsMagmaMonomorphism_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_homo_2980 v0
  = coe d_homo_86 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaMonomorphism.injective
d_injective_2982 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_2982 v0 = coe d_injective_104 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaMonomorphism.isMagmaHomomorphism
d_isMagmaHomomorphism_2984 ::
  T_IsMagmaMonomorphism_94 -> T_IsMagmaHomomorphism_76
d_isMagmaHomomorphism_2984 v0
  = coe d_isMagmaHomomorphism_102 (coe v0)
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaMonomorphism.isRelHomomorphism
d_isRelHomomorphism_2986 ::
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_2986 v0
  = coe
      d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaMonomorphism.isRelMonomorphism
d_isRelMonomorphism_2988 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_2988 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_isRelMonomorphism_2988
du_isRelMonomorphism_2988 ::
  (AgdaAny -> AgdaAny) ->
  T_IsMagmaMonomorphism_94 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
du_isRelMonomorphism_2988 v0 v1 = coe du_isRelMonomorphism_114 v1
-- Algebra.Morphism.Structures.LoopMorphisms._.∙.IsMagmaMonomorphism.cong
d_cong_2990 ::
  T_IsMagmaMonomorphism_94 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_2990 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_84 (coe d_isMagmaHomomorphism_102 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism
d_IsLoopHomomorphism_2994 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsLoopHomomorphism_2994
  = C_IsLoopHomomorphism'46'constructor_52259 T_IsQuasigroupHomomorphism_2530
                                              AgdaAny
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism.isQuasigroupHomomorphism
d_isQuasigroupHomomorphism_3002 ::
  T_IsLoopHomomorphism_2994 -> T_IsQuasigroupHomomorphism_2530
d_isQuasigroupHomomorphism_3002 v0
  = case coe v0 of
      C_IsLoopHomomorphism'46'constructor_52259 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism.ε-homo
d_ε'45'homo_3004 :: T_IsLoopHomomorphism_2994 -> AgdaAny
d_ε'45'homo_3004 v0
  = case coe v0 of
      C_IsLoopHomomorphism'46'constructor_52259 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism._.//-homo
d_'47''47''45'homo_3008 ::
  T_IsLoopHomomorphism_2994 -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'homo_3008 v0
  = coe
      d_'47''47''45'homo_2548
      (coe d_isQuasigroupHomomorphism_3002 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism._.//-isMagmaHomomorphism
d_'47''47''45'isMagmaHomomorphism_3010 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLoopHomomorphism_2994 -> T_IsMagmaHomomorphism_76
d_'47''47''45'isMagmaHomomorphism_3010 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaHomomorphism_3010 v7
du_'47''47''45'isMagmaHomomorphism_3010 ::
  T_IsLoopHomomorphism_2994 -> T_IsMagmaHomomorphism_76
du_'47''47''45'isMagmaHomomorphism_3010 v0
  = coe
      du_'47''47''45'isMagmaHomomorphism_2558
      (coe d_isQuasigroupHomomorphism_3002 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism._.\\-homo
d_'92''92''45'homo_3012 ::
  T_IsLoopHomomorphism_2994 -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'homo_3012 v0
  = coe
      d_'92''92''45'homo_2546
      (coe d_isQuasigroupHomomorphism_3002 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism._.\\-isMagmaHomomorphism
d_'92''92''45'isMagmaHomomorphism_3014 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLoopHomomorphism_2994 -> T_IsMagmaHomomorphism_76
d_'92''92''45'isMagmaHomomorphism_3014 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaHomomorphism_3014 v7
du_'92''92''45'isMagmaHomomorphism_3014 ::
  T_IsLoopHomomorphism_2994 -> T_IsMagmaHomomorphism_76
du_'92''92''45'isMagmaHomomorphism_3014 v0
  = coe
      du_'92''92''45'isMagmaHomomorphism_2556
      (coe d_isQuasigroupHomomorphism_3002 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism._.isRelHomomorphism
d_isRelHomomorphism_3016 ::
  T_IsLoopHomomorphism_2994 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_3016 v0
  = coe
      d_isRelHomomorphism_2542
      (coe d_isQuasigroupHomomorphism_3002 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism._.∙-homo
d_'8729''45'homo_3018 ::
  T_IsLoopHomomorphism_2994 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'homo_3018 v0
  = coe
      d_'8729''45'homo_2544
      (coe d_isQuasigroupHomomorphism_3002 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism._.∙-isMagmaHomomorphism
d_'8729''45'isMagmaHomomorphism_3020 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLoopHomomorphism_2994 -> T_IsMagmaHomomorphism_76
d_'8729''45'isMagmaHomomorphism_3020 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaHomomorphism_3020 v7
du_'8729''45'isMagmaHomomorphism_3020 ::
  T_IsLoopHomomorphism_2994 -> T_IsMagmaHomomorphism_76
du_'8729''45'isMagmaHomomorphism_3020 v0
  = coe
      du_'8729''45'isMagmaHomomorphism_2554
      (coe d_isQuasigroupHomomorphism_3002 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopHomomorphism._.cong
d_cong_3022 ::
  T_IsLoopHomomorphism_2994 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_3022 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_2542
         (coe d_isQuasigroupHomomorphism_3002 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism
d_IsLoopMonomorphism_3026 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsLoopMonomorphism_3026
  = C_IsLoopMonomorphism'46'constructor_53067 T_IsLoopHomomorphism_2994
                                              (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism.isLoopHomomorphism
d_isLoopHomomorphism_3034 ::
  T_IsLoopMonomorphism_3026 -> T_IsLoopHomomorphism_2994
d_isLoopHomomorphism_3034 v0
  = case coe v0 of
      C_IsLoopMonomorphism'46'constructor_53067 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism.injective
d_injective_3036 ::
  T_IsLoopMonomorphism_3026 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_3036 v0
  = case coe v0 of
      C_IsLoopMonomorphism'46'constructor_53067 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism._.//-homo
d_'47''47''45'homo_3040 ::
  T_IsLoopMonomorphism_3026 -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'homo_3040 v0
  = coe
      d_'47''47''45'homo_2548
      (coe
         d_isQuasigroupHomomorphism_3002
         (coe d_isLoopHomomorphism_3034 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism._.//-isMagmaHomomorphism
d_'47''47''45'isMagmaHomomorphism_3042 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLoopMonomorphism_3026 -> T_IsMagmaHomomorphism_76
d_'47''47''45'isMagmaHomomorphism_3042 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaHomomorphism_3042 v7
du_'47''47''45'isMagmaHomomorphism_3042 ::
  T_IsLoopMonomorphism_3026 -> T_IsMagmaHomomorphism_76
du_'47''47''45'isMagmaHomomorphism_3042 v0
  = let v1 = d_isLoopHomomorphism_3034 (coe v0) in
    coe
      du_'47''47''45'isMagmaHomomorphism_2558
      (coe d_isQuasigroupHomomorphism_3002 (coe v1))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism._.\\-homo
d_'92''92''45'homo_3044 ::
  T_IsLoopMonomorphism_3026 -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'homo_3044 v0
  = coe
      d_'92''92''45'homo_2546
      (coe
         d_isQuasigroupHomomorphism_3002
         (coe d_isLoopHomomorphism_3034 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism._.\\-isMagmaHomomorphism
d_'92''92''45'isMagmaHomomorphism_3046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLoopMonomorphism_3026 -> T_IsMagmaHomomorphism_76
d_'92''92''45'isMagmaHomomorphism_3046 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaHomomorphism_3046 v7
du_'92''92''45'isMagmaHomomorphism_3046 ::
  T_IsLoopMonomorphism_3026 -> T_IsMagmaHomomorphism_76
du_'92''92''45'isMagmaHomomorphism_3046 v0
  = let v1 = d_isLoopHomomorphism_3034 (coe v0) in
    coe
      du_'92''92''45'isMagmaHomomorphism_2556
      (coe d_isQuasigroupHomomorphism_3002 (coe v1))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism._.isQuasigroupHomomorphism
d_isQuasigroupHomomorphism_3048 ::
  T_IsLoopMonomorphism_3026 -> T_IsQuasigroupHomomorphism_2530
d_isQuasigroupHomomorphism_3048 v0
  = coe
      d_isQuasigroupHomomorphism_3002
      (coe d_isLoopHomomorphism_3034 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism._.isRelHomomorphism
d_isRelHomomorphism_3050 ::
  T_IsLoopMonomorphism_3026 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_3050 v0
  = coe
      d_isRelHomomorphism_2542
      (coe
         d_isQuasigroupHomomorphism_3002
         (coe d_isLoopHomomorphism_3034 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism._.ε-homo
d_ε'45'homo_3052 :: T_IsLoopMonomorphism_3026 -> AgdaAny
d_ε'45'homo_3052 v0
  = coe d_ε'45'homo_3004 (coe d_isLoopHomomorphism_3034 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism._.∙-homo
d_'8729''45'homo_3054 ::
  T_IsLoopMonomorphism_3026 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'homo_3054 v0
  = coe
      d_'8729''45'homo_2544
      (coe
         d_isQuasigroupHomomorphism_3002
         (coe d_isLoopHomomorphism_3034 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism._.∙-isMagmaHomomorphism
d_'8729''45'isMagmaHomomorphism_3056 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLoopMonomorphism_3026 -> T_IsMagmaHomomorphism_76
d_'8729''45'isMagmaHomomorphism_3056 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaHomomorphism_3056 v7
du_'8729''45'isMagmaHomomorphism_3056 ::
  T_IsLoopMonomorphism_3026 -> T_IsMagmaHomomorphism_76
du_'8729''45'isMagmaHomomorphism_3056 v0
  = let v1 = d_isLoopHomomorphism_3034 (coe v0) in
    coe
      du_'8729''45'isMagmaHomomorphism_2554
      (coe d_isQuasigroupHomomorphism_3002 (coe v1))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopMonomorphism._.cong
d_cong_3058 ::
  T_IsLoopMonomorphism_3026 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_3058 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_2542
         (coe
            d_isQuasigroupHomomorphism_3002
            (coe d_isLoopHomomorphism_3034 (coe v0))))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism
d_IsLoopIsomorphism_3062 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsLoopIsomorphism_3062
  = C_IsLoopIsomorphism'46'constructor_54009 T_IsLoopMonomorphism_3026
                                             (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism.isLoopMonomorphism
d_isLoopMonomorphism_3070 ::
  T_IsLoopIsomorphism_3062 -> T_IsLoopMonomorphism_3026
d_isLoopMonomorphism_3070 v0
  = case coe v0 of
      C_IsLoopIsomorphism'46'constructor_54009 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism.surjective
d_surjective_3072 ::
  T_IsLoopIsomorphism_3062 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_3072 v0
  = case coe v0 of
      C_IsLoopIsomorphism'46'constructor_54009 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.//-homo
d_'47''47''45'homo_3076 ::
  T_IsLoopIsomorphism_3062 -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'homo_3076 v0
  = coe
      d_'47''47''45'homo_2548
      (coe
         d_isQuasigroupHomomorphism_3002
         (coe
            d_isLoopHomomorphism_3034
            (coe d_isLoopMonomorphism_3070 (coe v0))))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.//-isMagmaHomomorphism
d_'47''47''45'isMagmaHomomorphism_3078 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLoopIsomorphism_3062 -> T_IsMagmaHomomorphism_76
d_'47''47''45'isMagmaHomomorphism_3078 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'47''47''45'isMagmaHomomorphism_3078 v7
du_'47''47''45'isMagmaHomomorphism_3078 ::
  T_IsLoopIsomorphism_3062 -> T_IsMagmaHomomorphism_76
du_'47''47''45'isMagmaHomomorphism_3078 v0
  = let v1 = d_isLoopMonomorphism_3070 (coe v0) in
    let v2 = d_isLoopHomomorphism_3034 (coe v1) in
    coe
      du_'47''47''45'isMagmaHomomorphism_2558
      (coe d_isQuasigroupHomomorphism_3002 (coe v2))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.\\-homo
d_'92''92''45'homo_3080 ::
  T_IsLoopIsomorphism_3062 -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'homo_3080 v0
  = coe
      d_'92''92''45'homo_2546
      (coe
         d_isQuasigroupHomomorphism_3002
         (coe
            d_isLoopHomomorphism_3034
            (coe d_isLoopMonomorphism_3070 (coe v0))))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.\\-isMagmaHomomorphism
d_'92''92''45'isMagmaHomomorphism_3082 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLoopIsomorphism_3062 -> T_IsMagmaHomomorphism_76
d_'92''92''45'isMagmaHomomorphism_3082 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       v7
  = du_'92''92''45'isMagmaHomomorphism_3082 v7
du_'92''92''45'isMagmaHomomorphism_3082 ::
  T_IsLoopIsomorphism_3062 -> T_IsMagmaHomomorphism_76
du_'92''92''45'isMagmaHomomorphism_3082 v0
  = let v1 = d_isLoopMonomorphism_3070 (coe v0) in
    let v2 = d_isLoopHomomorphism_3034 (coe v1) in
    coe
      du_'92''92''45'isMagmaHomomorphism_2556
      (coe d_isQuasigroupHomomorphism_3002 (coe v2))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.injective
d_injective_3084 ::
  T_IsLoopIsomorphism_3062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_3084 v0
  = coe d_injective_3036 (coe d_isLoopMonomorphism_3070 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.isLoopHomomorphism
d_isLoopHomomorphism_3086 ::
  T_IsLoopIsomorphism_3062 -> T_IsLoopHomomorphism_2994
d_isLoopHomomorphism_3086 v0
  = coe
      d_isLoopHomomorphism_3034 (coe d_isLoopMonomorphism_3070 (coe v0))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.isQuasigroupHomomorphism
d_isQuasigroupHomomorphism_3088 ::
  T_IsLoopIsomorphism_3062 -> T_IsQuasigroupHomomorphism_2530
d_isQuasigroupHomomorphism_3088 v0
  = coe
      d_isQuasigroupHomomorphism_3002
      (coe
         d_isLoopHomomorphism_3034 (coe d_isLoopMonomorphism_3070 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.isRelHomomorphism
d_isRelHomomorphism_3090 ::
  T_IsLoopIsomorphism_3062 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_3090 v0
  = coe
      d_isRelHomomorphism_2542
      (coe
         d_isQuasigroupHomomorphism_3002
         (coe
            d_isLoopHomomorphism_3034
            (coe d_isLoopMonomorphism_3070 (coe v0))))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.ε-homo
d_ε'45'homo_3092 :: T_IsLoopIsomorphism_3062 -> AgdaAny
d_ε'45'homo_3092 v0
  = coe
      d_ε'45'homo_3004
      (coe
         d_isLoopHomomorphism_3034 (coe d_isLoopMonomorphism_3070 (coe v0)))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.∙-homo
d_'8729''45'homo_3094 ::
  T_IsLoopIsomorphism_3062 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'homo_3094 v0
  = coe
      d_'8729''45'homo_2544
      (coe
         d_isQuasigroupHomomorphism_3002
         (coe
            d_isLoopHomomorphism_3034
            (coe d_isLoopMonomorphism_3070 (coe v0))))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.∙-isMagmaHomomorphism
d_'8729''45'isMagmaHomomorphism_3096 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawLoop_336 ->
  (AgdaAny -> AgdaAny) ->
  T_IsLoopIsomorphism_3062 -> T_IsMagmaHomomorphism_76
d_'8729''45'isMagmaHomomorphism_3096 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'isMagmaHomomorphism_3096 v7
du_'8729''45'isMagmaHomomorphism_3096 ::
  T_IsLoopIsomorphism_3062 -> T_IsMagmaHomomorphism_76
du_'8729''45'isMagmaHomomorphism_3096 v0
  = let v1 = d_isLoopMonomorphism_3070 (coe v0) in
    let v2 = d_isLoopHomomorphism_3034 (coe v1) in
    coe
      du_'8729''45'isMagmaHomomorphism_2554
      (coe d_isQuasigroupHomomorphism_3002 (coe v2))
-- Algebra.Morphism.Structures.LoopMorphisms.IsLoopIsomorphism._.cong
d_cong_3098 ::
  T_IsLoopIsomorphism_3062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_3098 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         d_isRelHomomorphism_2542
         (coe
            d_isQuasigroupHomomorphism_3002
            (coe
               d_isLoopHomomorphism_3034
               (coe d_isLoopMonomorphism_3070 (coe v0)))))
