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

module MAlonzo.Code.Relation.Binary.Morphism.Bundles where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Consequences
import qualified MAlonzo.Code.Relation.Binary.Morphism.Structures
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Relation.Binary.Morphism.Bundles._.SetoidHomomorphism
d_SetoidHomomorphism_36 a0 a1 a2 a3 a4 a5 = ()
data T_SetoidHomomorphism_36
  = C_SetoidHomomorphism'46'constructor_729 (AgdaAny -> AgdaAny)
                                            MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
-- Relation.Binary.Morphism.Bundles._.SetoidHomomorphism.⟦_⟧
d_'10214'_'10215'_42 ::
  T_SetoidHomomorphism_36 -> AgdaAny -> AgdaAny
d_'10214'_'10215'_42 v0
  = case coe v0 of
      C_SetoidHomomorphism'46'constructor_729 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Morphism.Bundles._.SetoidHomomorphism.isRelHomomorphism
d_isRelHomomorphism_44 ::
  T_SetoidHomomorphism_36 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_44 v0
  = case coe v0 of
      C_SetoidHomomorphism'46'constructor_729 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Morphism.Bundles._.SetoidHomomorphism._.cong
d_cong_48 ::
  T_SetoidHomomorphism_36 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_48 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe d_isRelHomomorphism_44 (coe v0))
-- Relation.Binary.Morphism.Bundles._.SetoidMonomorphism
d_SetoidMonomorphism_50 a0 a1 a2 a3 a4 a5 = ()
data T_SetoidMonomorphism_50
  = C_SetoidMonomorphism'46'constructor_2043 (AgdaAny -> AgdaAny)
                                             MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
-- Relation.Binary.Morphism.Bundles._.SetoidMonomorphism.⟦_⟧
d_'10214'_'10215'_56 ::
  T_SetoidMonomorphism_50 -> AgdaAny -> AgdaAny
d_'10214'_'10215'_56 v0
  = case coe v0 of
      C_SetoidMonomorphism'46'constructor_2043 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Morphism.Bundles._.SetoidMonomorphism.isRelMonomorphism
d_isRelMonomorphism_58 ::
  T_SetoidMonomorphism_50 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isRelMonomorphism_58 v0
  = case coe v0 of
      C_SetoidMonomorphism'46'constructor_2043 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Morphism.Bundles._.SetoidMonomorphism._.cong
d_cong_62 ::
  T_SetoidMonomorphism_50 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_62 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.d_isHomomorphism_76
         (coe d_isRelMonomorphism_58 (coe v0)))
-- Relation.Binary.Morphism.Bundles._.SetoidMonomorphism._.injective
d_injective_64 ::
  T_SetoidMonomorphism_50 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_64 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_injective_78
      (coe d_isRelMonomorphism_58 (coe v0))
-- Relation.Binary.Morphism.Bundles._.SetoidMonomorphism._.isHomomorphism
d_isHomomorphism_66 ::
  T_SetoidMonomorphism_50 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isHomomorphism_66 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_isHomomorphism_76
      (coe d_isRelMonomorphism_58 (coe v0))
-- Relation.Binary.Morphism.Bundles._.SetoidMonomorphism.homomorphism
d_homomorphism_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_SetoidMonomorphism_50 -> T_SetoidHomomorphism_36
d_homomorphism_68 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_homomorphism_68 v6
du_homomorphism_68 ::
  T_SetoidMonomorphism_50 -> T_SetoidHomomorphism_36
du_homomorphism_68 v0
  = coe
      C_SetoidHomomorphism'46'constructor_729
      (coe d_'10214'_'10215'_56 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.d_isHomomorphism_76
         (coe d_isRelMonomorphism_58 (coe v0)))
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism
d_SetoidIsomorphism_70 a0 a1 a2 a3 a4 a5 = ()
data T_SetoidIsomorphism_70
  = C_SetoidIsomorphism'46'constructor_3663 (AgdaAny -> AgdaAny)
                                            MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism.⟦_⟧
d_'10214'_'10215'_76 ::
  T_SetoidIsomorphism_70 -> AgdaAny -> AgdaAny
d_'10214'_'10215'_76 v0
  = case coe v0 of
      C_SetoidIsomorphism'46'constructor_3663 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism.isRelIsomorphism
d_isRelIsomorphism_78 ::
  T_SetoidIsomorphism_70 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelIsomorphism_94
d_isRelIsomorphism_78 v0
  = case coe v0 of
      C_SetoidIsomorphism'46'constructor_3663 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism._.bijective
d_bijective_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_SetoidIsomorphism_70 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_bijective_82 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_bijective_82 v6
du_bijective_82 ::
  T_SetoidIsomorphism_70 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_bijective_82 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.du_bijective_118
      (coe d_isRelIsomorphism_78 (coe v0))
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism._.cong
d_cong_84 ::
  T_SetoidIsomorphism_70 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_84 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_52
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.d_isHomomorphism_76
         (coe
            MAlonzo.Code.Relation.Binary.Morphism.Structures.d_isMonomorphism_106
            (coe d_isRelIsomorphism_78 (coe v0))))
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism._.injective
d_injective_86 ::
  T_SetoidIsomorphism_70 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_86 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_injective_78
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.d_isMonomorphism_106
         (coe d_isRelIsomorphism_78 (coe v0)))
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism._.isHomomorphism
d_isHomomorphism_88 ::
  T_SetoidIsomorphism_70 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isHomomorphism_88 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_isHomomorphism_76
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.d_isMonomorphism_106
         (coe d_isRelIsomorphism_78 (coe v0)))
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism._.isMonomorphism
d_isMonomorphism_90 ::
  T_SetoidIsomorphism_70 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelMonomorphism_64
d_isMonomorphism_90 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_isMonomorphism_106
      (coe d_isRelIsomorphism_78 (coe v0))
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism._.surjective
d_surjective_92 ::
  T_SetoidIsomorphism_70 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_92 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_surjective_108
      (coe d_isRelIsomorphism_78 (coe v0))
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism.monomorphism
d_monomorphism_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_SetoidIsomorphism_70 -> T_SetoidMonomorphism_50
d_monomorphism_94 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_monomorphism_94 v6
du_monomorphism_94 ::
  T_SetoidIsomorphism_70 -> T_SetoidMonomorphism_50
du_monomorphism_94 v0
  = coe
      C_SetoidMonomorphism'46'constructor_2043
      (coe d_'10214'_'10215'_76 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.d_isMonomorphism_106
         (coe d_isRelIsomorphism_78 (coe v0)))
-- Relation.Binary.Morphism.Bundles._.SetoidIsomorphism._.homomorphism
d_homomorphism_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_SetoidIsomorphism_70 -> T_SetoidHomomorphism_36
d_homomorphism_98 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_homomorphism_98 v6
du_homomorphism_98 ::
  T_SetoidIsomorphism_70 -> T_SetoidHomomorphism_36
du_homomorphism_98 v0
  = coe du_homomorphism_68 (coe du_monomorphism_94 (coe v0))
-- Relation.Binary.Morphism.Bundles.PreorderHomomorphism
d_PreorderHomomorphism_116 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_PreorderHomomorphism_116
  = C_PreorderHomomorphism'46'constructor_6379 (AgdaAny -> AgdaAny)
                                               MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderHomomorphism_138
-- Relation.Binary.Morphism.Bundles.PreorderHomomorphism.⟦_⟧
d_'10214'_'10215'_126 ::
  T_PreorderHomomorphism_116 -> AgdaAny -> AgdaAny
d_'10214'_'10215'_126 v0
  = case coe v0 of
      C_PreorderHomomorphism'46'constructor_6379 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Morphism.Bundles.PreorderHomomorphism.isOrderHomomorphism
d_isOrderHomomorphism_128 ::
  T_PreorderHomomorphism_116 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderHomomorphism_138
d_isOrderHomomorphism_128 v0
  = case coe v0 of
      C_PreorderHomomorphism'46'constructor_6379 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Morphism.Bundles.PreorderHomomorphism._.cong
d_cong_132 ::
  T_PreorderHomomorphism_116 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_132 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_154
      (coe d_isOrderHomomorphism_128 (coe v0))
-- Relation.Binary.Morphism.Bundles.PreorderHomomorphism._.isRelHomomorphism
d_isRelHomomorphism_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  T_PreorderHomomorphism_116 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_134 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isRelHomomorphism_134 v8
du_isRelHomomorphism_134 ::
  T_PreorderHomomorphism_116 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
du_isRelHomomorphism_134 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.du_isRelHomomorphism_162
      (coe d_isOrderHomomorphism_128 (coe v0))
-- Relation.Binary.Morphism.Bundles.PreorderHomomorphism._.mono
d_mono_136 ::
  T_PreorderHomomorphism_116 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_mono_136 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_mono_156
      (coe d_isOrderHomomorphism_128 (coe v0))
-- Relation.Binary.Morphism.Bundles.PreorderHomomorphism._.Eq.isRelHomomorphism
d_isRelHomomorphism_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  T_PreorderHomomorphism_116 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_140 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isRelHomomorphism_140 v8
du_isRelHomomorphism_140 ::
  T_PreorderHomomorphism_116 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
du_isRelHomomorphism_140 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.du_isRelHomomorphism_160
      (coe d_isOrderHomomorphism_128 (coe v0))
-- Relation.Binary.Morphism.Bundles._.P._≈_
d__'8776'__164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__164 = erased
-- Relation.Binary.Morphism.Bundles._.P._≤_
d__'8804'__166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  AgdaAny -> AgdaAny -> ()
d__'8804'__166 = erased
-- Relation.Binary.Morphism.Bundles._.P.Carrier
d_Carrier_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 -> ()
d_Carrier_168 = erased
-- Relation.Binary.Morphism.Bundles._.PosetHomomorphism
d_PosetHomomorphism_270 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_PosetHomomorphism_270
  = C_PosetHomomorphism'46'constructor_8671 (AgdaAny -> AgdaAny)
                                            MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderHomomorphism_138
-- Relation.Binary.Morphism.Bundles._.PosetHomomorphism.⟦_⟧
d_'10214'_'10215'_276 ::
  T_PosetHomomorphism_270 -> AgdaAny -> AgdaAny
d_'10214'_'10215'_276 v0
  = case coe v0 of
      C_PosetHomomorphism'46'constructor_8671 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Morphism.Bundles._.PosetHomomorphism.isOrderHomomorphism
d_isOrderHomomorphism_278 ::
  T_PosetHomomorphism_270 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderHomomorphism_138
d_isOrderHomomorphism_278 v0
  = case coe v0 of
      C_PosetHomomorphism'46'constructor_8671 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Morphism.Bundles._.PosetHomomorphism._.cong
d_cong_282 ::
  T_PosetHomomorphism_270 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_282 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_cong_154
      (coe d_isOrderHomomorphism_278 (coe v0))
-- Relation.Binary.Morphism.Bundles._.PosetHomomorphism._.isRelHomomorphism
d_isRelHomomorphism_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  T_PosetHomomorphism_270 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_284 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isRelHomomorphism_284 v8
du_isRelHomomorphism_284 ::
  T_PosetHomomorphism_270 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
du_isRelHomomorphism_284 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.du_isRelHomomorphism_162
      (coe d_isOrderHomomorphism_278 (coe v0))
-- Relation.Binary.Morphism.Bundles._.PosetHomomorphism._.mono
d_mono_286 ::
  T_PosetHomomorphism_270 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_mono_286 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.d_mono_156
      (coe d_isOrderHomomorphism_278 (coe v0))
-- Relation.Binary.Morphism.Bundles._.PosetHomomorphism._.Eq.isRelHomomorphism
d_isRelHomomorphism_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  T_PosetHomomorphism_270 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
d_isRelHomomorphism_290 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isRelHomomorphism_290 v8
du_isRelHomomorphism_290 ::
  T_PosetHomomorphism_270 ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsRelHomomorphism_42
du_isRelHomomorphism_290 v0
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.du_isRelHomomorphism_160
      (coe d_isOrderHomomorphism_278 (coe v0))
-- Relation.Binary.Morphism.Bundles._.mkPosetHomo
d_mkPosetHomo_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  T_PosetHomomorphism_270
d_mkPosetHomo_294 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9
  = du_mkPosetHomo_294 v6 v7 v8 v9
du_mkPosetHomo_294 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  T_PosetHomomorphism_270
du_mkPosetHomo_294 v0 v1 v2 v3
  = coe
      C_PosetHomomorphism'46'constructor_8671 (coe v2)
      (coe
         MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsOrderHomomorphism'46'constructor_5407
         (coe
            MAlonzo.Code.Relation.Binary.Consequences.du_mono'8658'cong_276
            (let v4
                   = coe
                       MAlonzo.Code.Relation.Binary.Bundles.du_preorder_326 (coe v0) in
             coe
               MAlonzo.Code.Relation.Binary.Structures.d_sym_36
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v4))))
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
                     (coe v0))))
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_antisym_172
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
                  (coe v1)))
            (coe v2) (coe v3))
         (coe v3))
