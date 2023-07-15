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

module MAlonzo.Code.Algebra.Structures where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Consequences.Setoid
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Structures._._DistributesOver_
d__DistributesOver__16 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver__16 = erased
-- Algebra.Structures._._DistributesOverʳ_
d__DistributesOver'691'__18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'691'__18 = erased
-- Algebra.Structures._._DistributesOverˡ_
d__DistributesOver'737'__20 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d__DistributesOver'737'__20 = erased
-- Algebra.Structures._.AlmostLeftCancellative
d_AlmostLeftCancellative_28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_AlmostLeftCancellative_28 = erased
-- Algebra.Structures._.Alternative
d_Alternative_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Alternative_32 = erased
-- Algebra.Structures._.Associative
d_Associative_34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Associative_34 = erased
-- Algebra.Structures._.Commutative
d_Commutative_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Commutative_38 = erased
-- Algebra.Structures._.Congruent₁
d_Congruent'8321'_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> ()
d_Congruent'8321'_40 = erased
-- Algebra.Structures._.Congruent₂
d_Congruent'8322'_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Congruent'8322'_42 = erased
-- Algebra.Structures._.Flexible
d_Flexible_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Flexible_46 = erased
-- Algebra.Structures._.Idempotent
d_Idempotent_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Idempotent_48 = erased
-- Algebra.Structures._.Identical
d_Identical_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Identical_52 = erased
-- Algebra.Structures._.Identity
d_Identity_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Identity_54 = erased
-- Algebra.Structures._.Inverse
d_Inverse_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Inverse_58 = erased
-- Algebra.Structures._.LeftAlternative
d_LeftAlternative_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftAlternative_64 = erased
-- Algebra.Structures._.LeftBol
d_LeftBol_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftBol_66 = erased
-- Algebra.Structures._.LeftCongruent
d_LeftCongruent_70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftCongruent_70 = erased
-- Algebra.Structures._.LeftDivides
d_LeftDivides_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftDivides_74 = erased
-- Algebra.Structures._.LeftDividesʳ
d_LeftDivides'691'_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftDivides'691'_76 = erased
-- Algebra.Structures._.LeftDividesˡ
d_LeftDivides'737'_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftDivides'737'_78 = erased
-- Algebra.Structures._.LeftIdentity
d_LeftIdentity_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftIdentity_80 = erased
-- Algebra.Structures._.LeftInverse
d_LeftInverse_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftInverse_82 = erased
-- Algebra.Structures._.LeftSemimedial
d_LeftSemimedial_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftSemimedial_86 = erased
-- Algebra.Structures._.LeftZero
d_LeftZero_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftZero_88 = erased
-- Algebra.Structures._.Medial
d_Medial_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Medial_90 = erased
-- Algebra.Structures._.MiddleBol
d_MiddleBol_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_MiddleBol_92 = erased
-- Algebra.Structures._.RightAlternative
d_RightAlternative_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightAlternative_94 = erased
-- Algebra.Structures._.RightBol
d_RightBol_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightBol_96 = erased
-- Algebra.Structures._.RightCongruent
d_RightCongruent_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightCongruent_100 = erased
-- Algebra.Structures._.RightDivides
d_RightDivides_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightDivides_104 = erased
-- Algebra.Structures._.RightDividesʳ
d_RightDivides'691'_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightDivides'691'_106 = erased
-- Algebra.Structures._.RightDividesˡ
d_RightDivides'737'_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightDivides'737'_108 = erased
-- Algebra.Structures._.RightIdentity
d_RightIdentity_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightIdentity_110 = erased
-- Algebra.Structures._.RightInverse
d_RightInverse_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightInverse_112 = erased
-- Algebra.Structures._.RightSemimedial
d_RightSemimedial_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightSemimedial_116 = erased
-- Algebra.Structures._.RightZero
d_RightZero_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightZero_118 = erased
-- Algebra.Structures._.Selective
d_Selective_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Selective_120 = erased
-- Algebra.Structures._.Semimedial
d_Semimedial_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Semimedial_122 = erased
-- Algebra.Structures._.StarDestructive
d_StarDestructive_124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_StarDestructive_124 = erased
-- Algebra.Structures._.StarExpansive
d_StarExpansive_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_StarExpansive_126 = erased
-- Algebra.Structures._.StarLeftDestructive
d_StarLeftDestructive_128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_StarLeftDestructive_128 = erased
-- Algebra.Structures._.StarLeftExpansive
d_StarLeftExpansive_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_StarLeftExpansive_130 = erased
-- Algebra.Structures._.StarRightDestructive
d_StarRightDestructive_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_StarRightDestructive_132 = erased
-- Algebra.Structures._.StarRightExpansive
d_StarRightExpansive_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_StarRightExpansive_134 = erased
-- Algebra.Structures._.Zero
d_Zero_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Zero_136 = erased
-- Algebra.Structures.IsMagma
d_IsMagma_140 a0 a1 a2 a3 a4 = ()
data T_IsMagma_140
  = C_IsMagma'46'constructor_769 MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
                                 (AgdaAny ->
                                  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsMagma.isEquivalence
d_isEquivalence_148 ::
  T_IsMagma_140 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_148 v0
  = case coe v0 of
      C_IsMagma'46'constructor_769 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMagma.∙-cong
d_'8729''45'cong_150 ::
  T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_150 v0
  = case coe v0 of
      C_IsMagma'46'constructor_769 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMagma._.isPartialEquivalence
d_isPartialEquivalence_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMagma_140 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_154 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_154 v5
du_isPartialEquivalence_154 ::
  T_IsMagma_140 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_154 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v0))
-- Algebra.Structures.IsMagma._.refl
d_refl_156 :: T_IsMagma_140 -> AgdaAny -> AgdaAny
d_refl_156 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe v0))
-- Algebra.Structures.IsMagma._.reflexive
d_reflexive_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_158 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_158 v5
du_reflexive_158 ::
  T_IsMagma_140 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_158 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
      (coe d_isEquivalence_148 (coe v0)) v1
-- Algebra.Structures.IsMagma._.sym
d_sym_160 ::
  T_IsMagma_140 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_160 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe v0))
-- Algebra.Structures.IsMagma._.trans
d_trans_162 ::
  T_IsMagma_140 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_162 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe v0))
-- Algebra.Structures.IsMagma.setoid
d_setoid_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMagma_140 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_164 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_164 v5
du_setoid_164 ::
  T_IsMagma_140 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_164 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (d_isEquivalence_148 (coe v0))
-- Algebra.Structures.IsMagma.∙-congˡ
d_'8729''45'cong'737'_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMagma_140 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_166 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8 v9
  = du_'8729''45'cong'737'_166 v5 v6 v7 v8 v9
du_'8729''45'cong'737'_166 ::
  T_IsMagma_140 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_166 v0 v1 v2 v3 v4
  = coe
      d_'8729''45'cong_150 v0 v1 v1 v2 v3
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (d_isEquivalence_148 (coe v0)) v1)
      v4
-- Algebra.Structures.IsMagma.∙-congʳ
d_'8729''45'cong'691'_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMagma_140 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_170 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8 v9
  = du_'8729''45'cong'691'_170 v5 v6 v7 v8 v9
du_'8729''45'cong'691'_170 ::
  T_IsMagma_140 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_170 v0 v1 v2 v3 v4
  = coe
      d_'8729''45'cong_150 v0 v2 v3 v1 v1 v4
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (d_isEquivalence_148 (coe v0)) v1)
-- Algebra.Structures.IsCommutativeMagma
d_IsCommutativeMagma_176 a0 a1 a2 a3 a4 = ()
data T_IsCommutativeMagma_176
  = C_IsCommutativeMagma'46'constructor_2649 T_IsMagma_140
                                             (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsCommutativeMagma.isMagma
d_isMagma_184 :: T_IsCommutativeMagma_176 -> T_IsMagma_140
d_isMagma_184 v0
  = case coe v0 of
      C_IsCommutativeMagma'46'constructor_2649 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeMagma.comm
d_comm_186 ::
  T_IsCommutativeMagma_176 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_186 v0
  = case coe v0 of
      C_IsCommutativeMagma'46'constructor_2649 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeMagma._.isEquivalence
d_isEquivalence_190 ::
  T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_190 v0
  = coe d_isEquivalence_148 (coe d_isMagma_184 (coe v0))
-- Algebra.Structures.IsCommutativeMagma._.isPartialEquivalence
d_isPartialEquivalence_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_192 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_192 v5
du_isPartialEquivalence_192 ::
  T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_192 v0
  = let v1 = d_isMagma_184 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsCommutativeMagma._.refl
d_refl_194 :: T_IsCommutativeMagma_176 -> AgdaAny -> AgdaAny
d_refl_194 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_184 (coe v0)))
-- Algebra.Structures.IsCommutativeMagma._.reflexive
d_reflexive_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeMagma_176 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_196 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_196 v5
du_reflexive_196 ::
  T_IsCommutativeMagma_176 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_196 v0
  = let v1 = d_isMagma_184 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsCommutativeMagma._.setoid
d_setoid_198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_198 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_198 v5
du_setoid_198 ::
  T_IsCommutativeMagma_176 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_198 v0 = coe du_setoid_164 (coe d_isMagma_184 (coe v0))
-- Algebra.Structures.IsCommutativeMagma._.sym
d_sym_200 ::
  T_IsCommutativeMagma_176 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_200 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_184 (coe v0)))
-- Algebra.Structures.IsCommutativeMagma._.trans
d_trans_202 ::
  T_IsCommutativeMagma_176 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_202 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_184 (coe v0)))
-- Algebra.Structures.IsCommutativeMagma._.∙-cong
d_'8729''45'cong_204 ::
  T_IsCommutativeMagma_176 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_204 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_184 (coe v0))
-- Algebra.Structures.IsCommutativeMagma._.∙-congʳ
d_'8729''45'cong'691'_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeMagma_176 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_206 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_206 v5
du_'8729''45'cong'691'_206 ::
  T_IsCommutativeMagma_176 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_206 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_184 (coe v0))
-- Algebra.Structures.IsCommutativeMagma._.∙-congˡ
d_'8729''45'cong'737'_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeMagma_176 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_208 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_208 v5
du_'8729''45'cong'737'_208 ::
  T_IsCommutativeMagma_176 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_208 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_184 (coe v0))
-- Algebra.Structures.IsIdempotentMagma
d_IsIdempotentMagma_212 a0 a1 a2 a3 a4 = ()
data T_IsIdempotentMagma_212
  = C_IsIdempotentMagma'46'constructor_3433 T_IsMagma_140
                                            (AgdaAny -> AgdaAny)
-- Algebra.Structures.IsIdempotentMagma.isMagma
d_isMagma_220 :: T_IsIdempotentMagma_212 -> T_IsMagma_140
d_isMagma_220 v0
  = case coe v0 of
      C_IsIdempotentMagma'46'constructor_3433 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsIdempotentMagma.idem
d_idem_222 :: T_IsIdempotentMagma_212 -> AgdaAny -> AgdaAny
d_idem_222 v0
  = case coe v0 of
      C_IsIdempotentMagma'46'constructor_3433 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsIdempotentMagma._.isEquivalence
d_isEquivalence_226 ::
  T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_226 v0
  = coe d_isEquivalence_148 (coe d_isMagma_220 (coe v0))
-- Algebra.Structures.IsIdempotentMagma._.isPartialEquivalence
d_isPartialEquivalence_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_228 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_228 v5
du_isPartialEquivalence_228 ::
  T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_228 v0
  = let v1 = d_isMagma_220 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsIdempotentMagma._.refl
d_refl_230 :: T_IsIdempotentMagma_212 -> AgdaAny -> AgdaAny
d_refl_230 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_220 (coe v0)))
-- Algebra.Structures.IsIdempotentMagma._.reflexive
d_reflexive_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsIdempotentMagma_212 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_232 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_232 v5
du_reflexive_232 ::
  T_IsIdempotentMagma_212 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_232 v0
  = let v1 = d_isMagma_220 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsIdempotentMagma._.setoid
d_setoid_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_234 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_234 v5
du_setoid_234 ::
  T_IsIdempotentMagma_212 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_234 v0 = coe du_setoid_164 (coe d_isMagma_220 (coe v0))
-- Algebra.Structures.IsIdempotentMagma._.sym
d_sym_236 ::
  T_IsIdempotentMagma_212 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_236 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_220 (coe v0)))
-- Algebra.Structures.IsIdempotentMagma._.trans
d_trans_238 ::
  T_IsIdempotentMagma_212 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_238 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_220 (coe v0)))
-- Algebra.Structures.IsIdempotentMagma._.∙-cong
d_'8729''45'cong_240 ::
  T_IsIdempotentMagma_212 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_240 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_220 (coe v0))
-- Algebra.Structures.IsIdempotentMagma._.∙-congʳ
d_'8729''45'cong'691'_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsIdempotentMagma_212 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_242 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_242 v5
du_'8729''45'cong'691'_242 ::
  T_IsIdempotentMagma_212 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_242 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_220 (coe v0))
-- Algebra.Structures.IsIdempotentMagma._.∙-congˡ
d_'8729''45'cong'737'_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsIdempotentMagma_212 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_244 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_244 v5
du_'8729''45'cong'737'_244 ::
  T_IsIdempotentMagma_212 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_244 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_220 (coe v0))
-- Algebra.Structures.IsAlternativeMagma
d_IsAlternativeMagma_248 a0 a1 a2 a3 a4 = ()
data T_IsAlternativeMagma_248
  = C_IsAlternativeMagma'46'constructor_4215 T_IsMagma_140
                                             MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsAlternativeMagma.isMagma
d_isMagma_256 :: T_IsAlternativeMagma_248 -> T_IsMagma_140
d_isMagma_256 v0
  = case coe v0 of
      C_IsAlternativeMagma'46'constructor_4215 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsAlternativeMagma.alter
d_alter_258 ::
  T_IsAlternativeMagma_248 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_alter_258 v0
  = case coe v0 of
      C_IsAlternativeMagma'46'constructor_4215 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsAlternativeMagma._.isEquivalence
d_isEquivalence_262 ::
  T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_262 v0
  = coe d_isEquivalence_148 (coe d_isMagma_256 (coe v0))
-- Algebra.Structures.IsAlternativeMagma._.isPartialEquivalence
d_isPartialEquivalence_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_264 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_264 v5
du_isPartialEquivalence_264 ::
  T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_264 v0
  = let v1 = d_isMagma_256 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsAlternativeMagma._.refl
d_refl_266 :: T_IsAlternativeMagma_248 -> AgdaAny -> AgdaAny
d_refl_266 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_256 (coe v0)))
-- Algebra.Structures.IsAlternativeMagma._.reflexive
d_reflexive_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsAlternativeMagma_248 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_268 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_268 v5
du_reflexive_268 ::
  T_IsAlternativeMagma_248 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_268 v0
  = let v1 = d_isMagma_256 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsAlternativeMagma._.setoid
d_setoid_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_270 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_270 v5
du_setoid_270 ::
  T_IsAlternativeMagma_248 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_270 v0 = coe du_setoid_164 (coe d_isMagma_256 (coe v0))
-- Algebra.Structures.IsAlternativeMagma._.sym
d_sym_272 ::
  T_IsAlternativeMagma_248 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_272 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_256 (coe v0)))
-- Algebra.Structures.IsAlternativeMagma._.trans
d_trans_274 ::
  T_IsAlternativeMagma_248 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_274 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_256 (coe v0)))
-- Algebra.Structures.IsAlternativeMagma._.∙-cong
d_'8729''45'cong_276 ::
  T_IsAlternativeMagma_248 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_276 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_256 (coe v0))
-- Algebra.Structures.IsAlternativeMagma._.∙-congʳ
d_'8729''45'cong'691'_278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsAlternativeMagma_248 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_278 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_278 v5
du_'8729''45'cong'691'_278 ::
  T_IsAlternativeMagma_248 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_278 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_256 (coe v0))
-- Algebra.Structures.IsAlternativeMagma._.∙-congˡ
d_'8729''45'cong'737'_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsAlternativeMagma_248 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_280 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_280 v5
du_'8729''45'cong'737'_280 ::
  T_IsAlternativeMagma_248 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_280 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_256 (coe v0))
-- Algebra.Structures.IsAlternativeMagma.alternativeˡ
d_alternative'737'_282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsAlternativeMagma_248 -> AgdaAny -> AgdaAny -> AgdaAny
d_alternative'737'_282 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_alternative'737'_282 v5
du_alternative'737'_282 ::
  T_IsAlternativeMagma_248 -> AgdaAny -> AgdaAny -> AgdaAny
du_alternative'737'_282 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe d_alter_258 (coe v0))
-- Algebra.Structures.IsAlternativeMagma.alternativeʳ
d_alternative'691'_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsAlternativeMagma_248 -> AgdaAny -> AgdaAny -> AgdaAny
d_alternative'691'_284 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_alternative'691'_284 v5
du_alternative'691'_284 ::
  T_IsAlternativeMagma_248 -> AgdaAny -> AgdaAny -> AgdaAny
du_alternative'691'_284 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe d_alter_258 (coe v0))
-- Algebra.Structures.IsFlexibleMagma
d_IsFlexibleMagma_288 a0 a1 a2 a3 a4 = ()
data T_IsFlexibleMagma_288
  = C_IsFlexibleMagma'46'constructor_5575 T_IsMagma_140
                                          (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsFlexibleMagma.isMagma
d_isMagma_296 :: T_IsFlexibleMagma_288 -> T_IsMagma_140
d_isMagma_296 v0
  = case coe v0 of
      C_IsFlexibleMagma'46'constructor_5575 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsFlexibleMagma.flex
d_flex_298 ::
  T_IsFlexibleMagma_288 -> AgdaAny -> AgdaAny -> AgdaAny
d_flex_298 v0
  = case coe v0 of
      C_IsFlexibleMagma'46'constructor_5575 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsFlexibleMagma._.isEquivalence
d_isEquivalence_302 ::
  T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_302 v0
  = coe d_isEquivalence_148 (coe d_isMagma_296 (coe v0))
-- Algebra.Structures.IsFlexibleMagma._.isPartialEquivalence
d_isPartialEquivalence_304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_304 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_304 v5
du_isPartialEquivalence_304 ::
  T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_304 v0
  = let v1 = d_isMagma_296 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsFlexibleMagma._.refl
d_refl_306 :: T_IsFlexibleMagma_288 -> AgdaAny -> AgdaAny
d_refl_306 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_296 (coe v0)))
-- Algebra.Structures.IsFlexibleMagma._.reflexive
d_reflexive_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsFlexibleMagma_288 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_308 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_308 v5
du_reflexive_308 ::
  T_IsFlexibleMagma_288 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_308 v0
  = let v1 = d_isMagma_296 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsFlexibleMagma._.setoid
d_setoid_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_310 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_310 v5
du_setoid_310 ::
  T_IsFlexibleMagma_288 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_310 v0 = coe du_setoid_164 (coe d_isMagma_296 (coe v0))
-- Algebra.Structures.IsFlexibleMagma._.sym
d_sym_312 ::
  T_IsFlexibleMagma_288 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_312 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_296 (coe v0)))
-- Algebra.Structures.IsFlexibleMagma._.trans
d_trans_314 ::
  T_IsFlexibleMagma_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_314 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_296 (coe v0)))
-- Algebra.Structures.IsFlexibleMagma._.∙-cong
d_'8729''45'cong_316 ::
  T_IsFlexibleMagma_288 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_316 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_296 (coe v0))
-- Algebra.Structures.IsFlexibleMagma._.∙-congʳ
d_'8729''45'cong'691'_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsFlexibleMagma_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_318 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_318 v5
du_'8729''45'cong'691'_318 ::
  T_IsFlexibleMagma_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_318 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_296 (coe v0))
-- Algebra.Structures.IsFlexibleMagma._.∙-congˡ
d_'8729''45'cong'737'_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsFlexibleMagma_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_320 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_320 v5
du_'8729''45'cong'737'_320 ::
  T_IsFlexibleMagma_288 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_320 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_296 (coe v0))
-- Algebra.Structures.IsMedialMagma
d_IsMedialMagma_324 a0 a1 a2 a3 a4 = ()
data T_IsMedialMagma_324
  = C_IsMedialMagma'46'constructor_6359 T_IsMagma_140
                                        (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsMedialMagma.isMagma
d_isMagma_332 :: T_IsMedialMagma_324 -> T_IsMagma_140
d_isMagma_332 v0
  = case coe v0 of
      C_IsMedialMagma'46'constructor_6359 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMedialMagma.medial
d_medial_334 ::
  T_IsMedialMagma_324 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_medial_334 v0
  = case coe v0 of
      C_IsMedialMagma'46'constructor_6359 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMedialMagma._.isEquivalence
d_isEquivalence_338 ::
  T_IsMedialMagma_324 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_338 v0
  = coe d_isEquivalence_148 (coe d_isMagma_332 (coe v0))
-- Algebra.Structures.IsMedialMagma._.isPartialEquivalence
d_isPartialEquivalence_340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMedialMagma_324 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_340 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_340 v5
du_isPartialEquivalence_340 ::
  T_IsMedialMagma_324 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_340 v0
  = let v1 = d_isMagma_332 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsMedialMagma._.refl
d_refl_342 :: T_IsMedialMagma_324 -> AgdaAny -> AgdaAny
d_refl_342 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_332 (coe v0)))
-- Algebra.Structures.IsMedialMagma._.reflexive
d_reflexive_344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMedialMagma_324 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_344 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_344 v5
du_reflexive_344 ::
  T_IsMedialMagma_324 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_344 v0
  = let v1 = d_isMagma_332 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsMedialMagma._.setoid
d_setoid_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMedialMagma_324 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_346 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_346 v5
du_setoid_346 ::
  T_IsMedialMagma_324 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_346 v0 = coe du_setoid_164 (coe d_isMagma_332 (coe v0))
-- Algebra.Structures.IsMedialMagma._.sym
d_sym_348 ::
  T_IsMedialMagma_324 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_348 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_332 (coe v0)))
-- Algebra.Structures.IsMedialMagma._.trans
d_trans_350 ::
  T_IsMedialMagma_324 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_350 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_332 (coe v0)))
-- Algebra.Structures.IsMedialMagma._.∙-cong
d_'8729''45'cong_352 ::
  T_IsMedialMagma_324 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_352 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_332 (coe v0))
-- Algebra.Structures.IsMedialMagma._.∙-congʳ
d_'8729''45'cong'691'_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMedialMagma_324 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_354 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_354 v5
du_'8729''45'cong'691'_354 ::
  T_IsMedialMagma_324 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_354 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_332 (coe v0))
-- Algebra.Structures.IsMedialMagma._.∙-congˡ
d_'8729''45'cong'737'_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsMedialMagma_324 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_356 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_356 v5
du_'8729''45'cong'737'_356 ::
  T_IsMedialMagma_324 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_356 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_332 (coe v0))
-- Algebra.Structures.IsSemimedialMagma
d_IsSemimedialMagma_360 a0 a1 a2 a3 a4 = ()
data T_IsSemimedialMagma_360
  = C_IsSemimedialMagma'46'constructor_7147 T_IsMagma_140
                                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsSemimedialMagma.isMagma
d_isMagma_368 :: T_IsSemimedialMagma_360 -> T_IsMagma_140
d_isMagma_368 v0
  = case coe v0 of
      C_IsSemimedialMagma'46'constructor_7147 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemimedialMagma.semiMedial
d_semiMedial_370 ::
  T_IsSemimedialMagma_360 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_semiMedial_370 v0
  = case coe v0 of
      C_IsSemimedialMagma'46'constructor_7147 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemimedialMagma._.isEquivalence
d_isEquivalence_374 ::
  T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_374 v0
  = coe d_isEquivalence_148 (coe d_isMagma_368 (coe v0))
-- Algebra.Structures.IsSemimedialMagma._.isPartialEquivalence
d_isPartialEquivalence_376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_376 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_376 v5
du_isPartialEquivalence_376 ::
  T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_376 v0
  = let v1 = d_isMagma_368 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsSemimedialMagma._.refl
d_refl_378 :: T_IsSemimedialMagma_360 -> AgdaAny -> AgdaAny
d_refl_378 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_368 (coe v0)))
-- Algebra.Structures.IsSemimedialMagma._.reflexive
d_reflexive_380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemimedialMagma_360 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_380 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_380 v5
du_reflexive_380 ::
  T_IsSemimedialMagma_360 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_380 v0
  = let v1 = d_isMagma_368 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsSemimedialMagma._.setoid
d_setoid_382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_382 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_382 v5
du_setoid_382 ::
  T_IsSemimedialMagma_360 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_382 v0 = coe du_setoid_164 (coe d_isMagma_368 (coe v0))
-- Algebra.Structures.IsSemimedialMagma._.sym
d_sym_384 ::
  T_IsSemimedialMagma_360 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_384 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_368 (coe v0)))
-- Algebra.Structures.IsSemimedialMagma._.trans
d_trans_386 ::
  T_IsSemimedialMagma_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_386 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_368 (coe v0)))
-- Algebra.Structures.IsSemimedialMagma._.∙-cong
d_'8729''45'cong_388 ::
  T_IsSemimedialMagma_360 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_388 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_368 (coe v0))
-- Algebra.Structures.IsSemimedialMagma._.∙-congʳ
d_'8729''45'cong'691'_390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemimedialMagma_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_390 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_390 v5
du_'8729''45'cong'691'_390 ::
  T_IsSemimedialMagma_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_390 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_368 (coe v0))
-- Algebra.Structures.IsSemimedialMagma._.∙-congˡ
d_'8729''45'cong'737'_392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemimedialMagma_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_392 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_392 v5
du_'8729''45'cong'737'_392 ::
  T_IsSemimedialMagma_360 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_392 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_368 (coe v0))
-- Algebra.Structures.IsSemimedialMagma.semimedialˡ
d_semimedial'737'_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemimedialMagma_360 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_semimedial'737'_394 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_semimedial'737'_394 v5
du_semimedial'737'_394 ::
  T_IsSemimedialMagma_360 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_semimedial'737'_394 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_semiMedial_370 (coe v0))
-- Algebra.Structures.IsSemimedialMagma.semimedialʳ
d_semimedial'691'_396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemimedialMagma_360 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_semimedial'691'_396 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_semimedial'691'_396 v5
du_semimedial'691'_396 ::
  T_IsSemimedialMagma_360 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_semimedial'691'_396 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_semiMedial_370 (coe v0))
-- Algebra.Structures.IsSelectiveMagma
d_IsSelectiveMagma_400 a0 a1 a2 a3 a4 = ()
data T_IsSelectiveMagma_400
  = C_IsSelectiveMagma'46'constructor_8519 T_IsMagma_140
                                           (AgdaAny ->
                                            AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30)
-- Algebra.Structures.IsSelectiveMagma.isMagma
d_isMagma_408 :: T_IsSelectiveMagma_400 -> T_IsMagma_140
d_isMagma_408 v0
  = case coe v0 of
      C_IsSelectiveMagma'46'constructor_8519 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSelectiveMagma.sel
d_sel_410 ::
  T_IsSelectiveMagma_400 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_410 v0
  = case coe v0 of
      C_IsSelectiveMagma'46'constructor_8519 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSelectiveMagma._.isEquivalence
d_isEquivalence_414 ::
  T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_414 v0
  = coe d_isEquivalence_148 (coe d_isMagma_408 (coe v0))
-- Algebra.Structures.IsSelectiveMagma._.isPartialEquivalence
d_isPartialEquivalence_416 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_416 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_416 v5
du_isPartialEquivalence_416 ::
  T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_416 v0
  = let v1 = d_isMagma_408 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsSelectiveMagma._.refl
d_refl_418 :: T_IsSelectiveMagma_400 -> AgdaAny -> AgdaAny
d_refl_418 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_408 (coe v0)))
-- Algebra.Structures.IsSelectiveMagma._.reflexive
d_reflexive_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSelectiveMagma_400 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_420 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_420 v5
du_reflexive_420 ::
  T_IsSelectiveMagma_400 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_420 v0
  = let v1 = d_isMagma_408 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsSelectiveMagma._.setoid
d_setoid_422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_422 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_422 v5
du_setoid_422 ::
  T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_422 v0 = coe du_setoid_164 (coe d_isMagma_408 (coe v0))
-- Algebra.Structures.IsSelectiveMagma._.sym
d_sym_424 ::
  T_IsSelectiveMagma_400 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_424 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_408 (coe v0)))
-- Algebra.Structures.IsSelectiveMagma._.trans
d_trans_426 ::
  T_IsSelectiveMagma_400 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_426 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_408 (coe v0)))
-- Algebra.Structures.IsSelectiveMagma._.∙-cong
d_'8729''45'cong_428 ::
  T_IsSelectiveMagma_400 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_428 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_408 (coe v0))
-- Algebra.Structures.IsSelectiveMagma._.∙-congʳ
d_'8729''45'cong'691'_430 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSelectiveMagma_400 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_430 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_430 v5
du_'8729''45'cong'691'_430 ::
  T_IsSelectiveMagma_400 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_430 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_408 (coe v0))
-- Algebra.Structures.IsSelectiveMagma._.∙-congˡ
d_'8729''45'cong'737'_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSelectiveMagma_400 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_432 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_432 v5
du_'8729''45'cong'737'_432 ::
  T_IsSelectiveMagma_400 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_432 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_408 (coe v0))
-- Algebra.Structures.IsSemigroup
d_IsSemigroup_436 a0 a1 a2 a3 a4 = ()
data T_IsSemigroup_436
  = C_IsSemigroup'46'constructor_9303 T_IsMagma_140
                                      (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsSemigroup.isMagma
d_isMagma_444 :: T_IsSemigroup_436 -> T_IsMagma_140
d_isMagma_444 v0
  = case coe v0 of
      C_IsSemigroup'46'constructor_9303 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemigroup.assoc
d_assoc_446 ::
  T_IsSemigroup_436 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_446 v0
  = case coe v0 of
      C_IsSemigroup'46'constructor_9303 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemigroup._.isEquivalence
d_isEquivalence_450 ::
  T_IsSemigroup_436 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_450 v0
  = coe d_isEquivalence_148 (coe d_isMagma_444 (coe v0))
-- Algebra.Structures.IsSemigroup._.isPartialEquivalence
d_isPartialEquivalence_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemigroup_436 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_452 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_452 v5
du_isPartialEquivalence_452 ::
  T_IsSemigroup_436 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_452 v0
  = let v1 = d_isMagma_444 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsSemigroup._.refl
d_refl_454 :: T_IsSemigroup_436 -> AgdaAny -> AgdaAny
d_refl_454 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_444 (coe v0)))
-- Algebra.Structures.IsSemigroup._.reflexive
d_reflexive_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemigroup_436 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_456 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_456 v5
du_reflexive_456 ::
  T_IsSemigroup_436 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_456 v0
  = let v1 = d_isMagma_444 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsSemigroup._.setoid
d_setoid_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemigroup_436 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_458 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_458 v5
du_setoid_458 ::
  T_IsSemigroup_436 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_458 v0 = coe du_setoid_164 (coe d_isMagma_444 (coe v0))
-- Algebra.Structures.IsSemigroup._.sym
d_sym_460 ::
  T_IsSemigroup_436 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_460 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_444 (coe v0)))
-- Algebra.Structures.IsSemigroup._.trans
d_trans_462 ::
  T_IsSemigroup_436 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_462 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_444 (coe v0)))
-- Algebra.Structures.IsSemigroup._.∙-cong
d_'8729''45'cong_464 ::
  T_IsSemigroup_436 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_464 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_444 (coe v0))
-- Algebra.Structures.IsSemigroup._.∙-congʳ
d_'8729''45'cong'691'_466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemigroup_436 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_466 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_466 v5
du_'8729''45'cong'691'_466 ::
  T_IsSemigroup_436 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_466 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v0))
-- Algebra.Structures.IsSemigroup._.∙-congˡ
d_'8729''45'cong'737'_468 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsSemigroup_436 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_468 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_468 v5
du_'8729''45'cong'737'_468 ::
  T_IsSemigroup_436 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_468 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v0))
-- Algebra.Structures.IsBand
d_IsBand_472 a0 a1 a2 a3 a4 = ()
data T_IsBand_472
  = C_IsBand'46'constructor_10089 T_IsSemigroup_436
                                  (AgdaAny -> AgdaAny)
-- Algebra.Structures.IsBand.isSemigroup
d_isSemigroup_480 :: T_IsBand_472 -> T_IsSemigroup_436
d_isSemigroup_480 v0
  = case coe v0 of
      C_IsBand'46'constructor_10089 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsBand.idem
d_idem_482 :: T_IsBand_472 -> AgdaAny -> AgdaAny
d_idem_482 v0
  = case coe v0 of
      C_IsBand'46'constructor_10089 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsBand._.assoc
d_assoc_486 ::
  T_IsBand_472 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_486 v0 = coe d_assoc_446 (coe d_isSemigroup_480 (coe v0))
-- Algebra.Structures.IsBand._.isEquivalence
d_isEquivalence_488 ::
  T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_488 v0
  = coe
      d_isEquivalence_148
      (coe d_isMagma_444 (coe d_isSemigroup_480 (coe v0)))
-- Algebra.Structures.IsBand._.isMagma
d_isMagma_490 :: T_IsBand_472 -> T_IsMagma_140
d_isMagma_490 v0
  = coe d_isMagma_444 (coe d_isSemigroup_480 (coe v0))
-- Algebra.Structures.IsBand._.isPartialEquivalence
d_isPartialEquivalence_492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_492 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_492 v5
du_isPartialEquivalence_492 ::
  T_IsBand_472 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_492 v0
  = let v1 = d_isSemigroup_480 (coe v0) in
    let v2 = d_isMagma_444 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v2))
-- Algebra.Structures.IsBand._.refl
d_refl_494 :: T_IsBand_472 -> AgdaAny -> AgdaAny
d_refl_494 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe d_isMagma_444 (coe d_isSemigroup_480 (coe v0))))
-- Algebra.Structures.IsBand._.reflexive
d_reflexive_496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsBand_472 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_496 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_496 v5
du_reflexive_496 ::
  T_IsBand_472 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_496 v0
  = let v1 = d_isSemigroup_480 (coe v0) in
    let v2 = d_isMagma_444 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v2)) v3
-- Algebra.Structures.IsBand._.setoid
d_setoid_498 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsBand_472 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_498 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_498 v5
du_setoid_498 ::
  T_IsBand_472 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_498 v0
  = let v1 = d_isSemigroup_480 (coe v0) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v1))
-- Algebra.Structures.IsBand._.sym
d_sym_500 ::
  T_IsBand_472 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_500 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe d_isMagma_444 (coe d_isSemigroup_480 (coe v0))))
-- Algebra.Structures.IsBand._.trans
d_trans_502 ::
  T_IsBand_472 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_502 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe d_isMagma_444 (coe d_isSemigroup_480 (coe v0))))
-- Algebra.Structures.IsBand._.∙-cong
d_'8729''45'cong_504 ::
  T_IsBand_472 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_504 v0
  = coe
      d_'8729''45'cong_150
      (coe d_isMagma_444 (coe d_isSemigroup_480 (coe v0)))
-- Algebra.Structures.IsBand._.∙-congʳ
d_'8729''45'cong'691'_506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsBand_472 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_506 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_506 v5
du_'8729''45'cong'691'_506 ::
  T_IsBand_472 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_506 v0
  = let v1 = d_isSemigroup_480 (coe v0) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v1))
-- Algebra.Structures.IsBand._.∙-congˡ
d_'8729''45'cong'737'_508 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsBand_472 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_508 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_508 v5
du_'8729''45'cong'737'_508 ::
  T_IsBand_472 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_508 v0
  = let v1 = d_isSemigroup_480 (coe v0) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v1))
-- Algebra.Structures.IsCommutativeSemigroup
d_IsCommutativeSemigroup_512 a0 a1 a2 a3 a4 = ()
data T_IsCommutativeSemigroup_512
  = C_IsCommutativeSemigroup'46'constructor_10975 T_IsSemigroup_436
                                                  (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsCommutativeSemigroup.isSemigroup
d_isSemigroup_520 ::
  T_IsCommutativeSemigroup_512 -> T_IsSemigroup_436
d_isSemigroup_520 v0
  = case coe v0 of
      C_IsCommutativeSemigroup'46'constructor_10975 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeSemigroup.comm
d_comm_522 ::
  T_IsCommutativeSemigroup_512 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_522 v0
  = case coe v0 of
      C_IsCommutativeSemigroup'46'constructor_10975 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeSemigroup._.assoc
d_assoc_526 ::
  T_IsCommutativeSemigroup_512 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_526 v0 = coe d_assoc_446 (coe d_isSemigroup_520 (coe v0))
-- Algebra.Structures.IsCommutativeSemigroup._.isEquivalence
d_isEquivalence_528 ::
  T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_528 v0
  = coe
      d_isEquivalence_148
      (coe d_isMagma_444 (coe d_isSemigroup_520 (coe v0)))
-- Algebra.Structures.IsCommutativeSemigroup._.isMagma
d_isMagma_530 :: T_IsCommutativeSemigroup_512 -> T_IsMagma_140
d_isMagma_530 v0
  = coe d_isMagma_444 (coe d_isSemigroup_520 (coe v0))
-- Algebra.Structures.IsCommutativeSemigroup._.isPartialEquivalence
d_isPartialEquivalence_532 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_532 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isPartialEquivalence_532 v5
du_isPartialEquivalence_532 ::
  T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_532 v0
  = let v1 = d_isSemigroup_520 (coe v0) in
    let v2 = d_isMagma_444 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v2))
-- Algebra.Structures.IsCommutativeSemigroup._.refl
d_refl_534 :: T_IsCommutativeSemigroup_512 -> AgdaAny -> AgdaAny
d_refl_534 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe d_isMagma_444 (coe d_isSemigroup_520 (coe v0))))
-- Algebra.Structures.IsCommutativeSemigroup._.reflexive
d_reflexive_536 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeSemigroup_512 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_536 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_reflexive_536 v5
du_reflexive_536 ::
  T_IsCommutativeSemigroup_512 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_536 v0
  = let v1 = d_isSemigroup_520 (coe v0) in
    let v2 = d_isMagma_444 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v2)) v3
-- Algebra.Structures.IsCommutativeSemigroup._.setoid
d_setoid_538 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_538 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_setoid_538 v5
du_setoid_538 ::
  T_IsCommutativeSemigroup_512 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_538 v0
  = let v1 = d_isSemigroup_520 (coe v0) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v1))
-- Algebra.Structures.IsCommutativeSemigroup._.sym
d_sym_540 ::
  T_IsCommutativeSemigroup_512 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_540 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe d_isMagma_444 (coe d_isSemigroup_520 (coe v0))))
-- Algebra.Structures.IsCommutativeSemigroup._.trans
d_trans_542 ::
  T_IsCommutativeSemigroup_512 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_542 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe d_isMagma_444 (coe d_isSemigroup_520 (coe v0))))
-- Algebra.Structures.IsCommutativeSemigroup._.∙-cong
d_'8729''45'cong_544 ::
  T_IsCommutativeSemigroup_512 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_544 v0
  = coe
      d_'8729''45'cong_150
      (coe d_isMagma_444 (coe d_isSemigroup_520 (coe v0)))
-- Algebra.Structures.IsCommutativeSemigroup._.∙-congʳ
d_'8729''45'cong'691'_546 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeSemigroup_512 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_546 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'691'_546 v5
du_'8729''45'cong'691'_546 ::
  T_IsCommutativeSemigroup_512 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_546 v0
  = let v1 = d_isSemigroup_520 (coe v0) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v1))
-- Algebra.Structures.IsCommutativeSemigroup._.∙-congˡ
d_'8729''45'cong'737'_548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeSemigroup_512 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_548 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8729''45'cong'737'_548 v5
du_'8729''45'cong'737'_548 ::
  T_IsCommutativeSemigroup_512 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_548 v0
  = let v1 = d_isSemigroup_520 (coe v0) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v1))
-- Algebra.Structures.IsCommutativeSemigroup.isCommutativeMagma
d_isCommutativeMagma_550 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsCommutativeSemigroup_512 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_550 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_isCommutativeMagma_550 v5
du_isCommutativeMagma_550 ::
  T_IsCommutativeSemigroup_512 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_550 v0
  = coe
      C_IsCommutativeMagma'46'constructor_2649
      (coe d_isMagma_444 (coe d_isSemigroup_520 (coe v0)))
      (coe d_comm_522 (coe v0))
-- Algebra.Structures.IsUnitalMagma
d_IsUnitalMagma_556 a0 a1 a2 a3 a4 a5 = ()
data T_IsUnitalMagma_556
  = C_IsUnitalMagma'46'constructor_12005 T_IsMagma_140
                                         MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsUnitalMagma.isMagma
d_isMagma_566 :: T_IsUnitalMagma_556 -> T_IsMagma_140
d_isMagma_566 v0
  = case coe v0 of
      C_IsUnitalMagma'46'constructor_12005 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsUnitalMagma.identity
d_identity_568 ::
  T_IsUnitalMagma_556 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_568 v0
  = case coe v0 of
      C_IsUnitalMagma'46'constructor_12005 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsUnitalMagma._.isEquivalence
d_isEquivalence_572 ::
  T_IsUnitalMagma_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_572 v0
  = coe d_isEquivalence_148 (coe d_isMagma_566 (coe v0))
-- Algebra.Structures.IsUnitalMagma._.isPartialEquivalence
d_isPartialEquivalence_574 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsUnitalMagma_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_574 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_574 v6
du_isPartialEquivalence_574 ::
  T_IsUnitalMagma_556 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_574 v0
  = let v1 = d_isMagma_566 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsUnitalMagma._.refl
d_refl_576 :: T_IsUnitalMagma_556 -> AgdaAny -> AgdaAny
d_refl_576 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_566 (coe v0)))
-- Algebra.Structures.IsUnitalMagma._.reflexive
d_reflexive_578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsUnitalMagma_556 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_578 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_578 v6
du_reflexive_578 ::
  T_IsUnitalMagma_556 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_578 v0
  = let v1 = d_isMagma_566 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsUnitalMagma._.setoid
d_setoid_580 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsUnitalMagma_556 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_580 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_580 v6
du_setoid_580 ::
  T_IsUnitalMagma_556 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_580 v0 = coe du_setoid_164 (coe d_isMagma_566 (coe v0))
-- Algebra.Structures.IsUnitalMagma._.sym
d_sym_582 ::
  T_IsUnitalMagma_556 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_582 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_566 (coe v0)))
-- Algebra.Structures.IsUnitalMagma._.trans
d_trans_584 ::
  T_IsUnitalMagma_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_584 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_566 (coe v0)))
-- Algebra.Structures.IsUnitalMagma._.∙-cong
d_'8729''45'cong_586 ::
  T_IsUnitalMagma_556 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_586 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_566 (coe v0))
-- Algebra.Structures.IsUnitalMagma._.∙-congʳ
d_'8729''45'cong'691'_588 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsUnitalMagma_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_588 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_588 v6
du_'8729''45'cong'691'_588 ::
  T_IsUnitalMagma_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_588 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_566 (coe v0))
-- Algebra.Structures.IsUnitalMagma._.∙-congˡ
d_'8729''45'cong'737'_590 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsUnitalMagma_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_590 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_590 v6
du_'8729''45'cong'737'_590 ::
  T_IsUnitalMagma_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_590 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_566 (coe v0))
-- Algebra.Structures.IsUnitalMagma.identityˡ
d_identity'737'_592 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsUnitalMagma_556 -> AgdaAny -> AgdaAny
d_identity'737'_592 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'737'_592 v6
du_identity'737'_592 :: T_IsUnitalMagma_556 -> AgdaAny -> AgdaAny
du_identity'737'_592 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_identity_568 (coe v0))
-- Algebra.Structures.IsUnitalMagma.identityʳ
d_identity'691'_594 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsUnitalMagma_556 -> AgdaAny -> AgdaAny
d_identity'691'_594 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'691'_594 v6
du_identity'691'_594 :: T_IsUnitalMagma_556 -> AgdaAny -> AgdaAny
du_identity'691'_594 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_identity_568 (coe v0))
-- Algebra.Structures.IsMonoid
d_IsMonoid_600 a0 a1 a2 a3 a4 a5 = ()
data T_IsMonoid_600
  = C_IsMonoid'46'constructor_13559 T_IsSemigroup_436
                                    MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsMonoid.isSemigroup
d_isSemigroup_610 :: T_IsMonoid_600 -> T_IsSemigroup_436
d_isSemigroup_610 v0
  = case coe v0 of
      C_IsMonoid'46'constructor_13559 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMonoid.identity
d_identity_612 ::
  T_IsMonoid_600 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_612 v0
  = case coe v0 of
      C_IsMonoid'46'constructor_13559 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMonoid._.assoc
d_assoc_616 ::
  T_IsMonoid_600 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_616 v0 = coe d_assoc_446 (coe d_isSemigroup_610 (coe v0))
-- Algebra.Structures.IsMonoid._.isEquivalence
d_isEquivalence_618 ::
  T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_618 v0
  = coe
      d_isEquivalence_148
      (coe d_isMagma_444 (coe d_isSemigroup_610 (coe v0)))
-- Algebra.Structures.IsMonoid._.isMagma
d_isMagma_620 :: T_IsMonoid_600 -> T_IsMagma_140
d_isMagma_620 v0
  = coe d_isMagma_444 (coe d_isSemigroup_610 (coe v0))
-- Algebra.Structures.IsMonoid._.isPartialEquivalence
d_isPartialEquivalence_622 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_622 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_622 v6
du_isPartialEquivalence_622 ::
  T_IsMonoid_600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_622 v0
  = let v1 = d_isSemigroup_610 (coe v0) in
    let v2 = d_isMagma_444 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v2))
-- Algebra.Structures.IsMonoid._.refl
d_refl_624 :: T_IsMonoid_600 -> AgdaAny -> AgdaAny
d_refl_624 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe d_isMagma_444 (coe d_isSemigroup_610 (coe v0))))
-- Algebra.Structures.IsMonoid._.reflexive
d_reflexive_626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMonoid_600 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_626 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_626 v6
du_reflexive_626 ::
  T_IsMonoid_600 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_626 v0
  = let v1 = d_isSemigroup_610 (coe v0) in
    let v2 = d_isMagma_444 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v2)) v3
-- Algebra.Structures.IsMonoid._.setoid
d_setoid_628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMonoid_600 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_628 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_628 v6
du_setoid_628 ::
  T_IsMonoid_600 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_628 v0
  = let v1 = d_isSemigroup_610 (coe v0) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v1))
-- Algebra.Structures.IsMonoid._.sym
d_sym_630 ::
  T_IsMonoid_600 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_630 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe d_isMagma_444 (coe d_isSemigroup_610 (coe v0))))
-- Algebra.Structures.IsMonoid._.trans
d_trans_632 ::
  T_IsMonoid_600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_632 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe d_isMagma_444 (coe d_isSemigroup_610 (coe v0))))
-- Algebra.Structures.IsMonoid._.∙-cong
d_'8729''45'cong_634 ::
  T_IsMonoid_600 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_634 v0
  = coe
      d_'8729''45'cong_150
      (coe d_isMagma_444 (coe d_isSemigroup_610 (coe v0)))
-- Algebra.Structures.IsMonoid._.∙-congʳ
d_'8729''45'cong'691'_636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMonoid_600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_636 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_636 v6
du_'8729''45'cong'691'_636 ::
  T_IsMonoid_600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_636 v0
  = let v1 = d_isSemigroup_610 (coe v0) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v1))
-- Algebra.Structures.IsMonoid._.∙-congˡ
d_'8729''45'cong'737'_638 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMonoid_600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_638 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_638 v6
du_'8729''45'cong'737'_638 ::
  T_IsMonoid_600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_638 v0
  = let v1 = d_isSemigroup_610 (coe v0) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v1))
-- Algebra.Structures.IsMonoid.identityˡ
d_identity'737'_640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMonoid_600 -> AgdaAny -> AgdaAny
d_identity'737'_640 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'737'_640 v6
du_identity'737'_640 :: T_IsMonoid_600 -> AgdaAny -> AgdaAny
du_identity'737'_640 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_identity_612 (coe v0))
-- Algebra.Structures.IsMonoid.identityʳ
d_identity'691'_642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMonoid_600 -> AgdaAny -> AgdaAny
d_identity'691'_642 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'691'_642 v6
du_identity'691'_642 :: T_IsMonoid_600 -> AgdaAny -> AgdaAny
du_identity'691'_642 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_identity_612 (coe v0))
-- Algebra.Structures.IsMonoid.isUnitalMagma
d_isUnitalMagma_644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMonoid_600 -> T_IsUnitalMagma_556
d_isUnitalMagma_644 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isUnitalMagma_644 v6
du_isUnitalMagma_644 :: T_IsMonoid_600 -> T_IsUnitalMagma_556
du_isUnitalMagma_644 v0
  = coe
      C_IsUnitalMagma'46'constructor_12005
      (coe d_isMagma_444 (coe d_isSemigroup_610 (coe v0)))
      (coe d_identity_612 (coe v0))
-- Algebra.Structures.IsCommutativeMonoid
d_IsCommutativeMonoid_650 a0 a1 a2 a3 a4 a5 = ()
data T_IsCommutativeMonoid_650
  = C_IsCommutativeMonoid'46'constructor_15379 T_IsMonoid_600
                                               (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsCommutativeMonoid.isMonoid
d_isMonoid_660 :: T_IsCommutativeMonoid_650 -> T_IsMonoid_600
d_isMonoid_660 v0
  = case coe v0 of
      C_IsCommutativeMonoid'46'constructor_15379 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeMonoid.comm
d_comm_662 ::
  T_IsCommutativeMonoid_650 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_662 v0
  = case coe v0 of
      C_IsCommutativeMonoid'46'constructor_15379 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeMonoid._.assoc
d_assoc_666 ::
  T_IsCommutativeMonoid_650 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_666 v0
  = coe
      d_assoc_446 (coe d_isSemigroup_610 (coe d_isMonoid_660 (coe v0)))
-- Algebra.Structures.IsCommutativeMonoid._.identity
d_identity_668 ::
  T_IsCommutativeMonoid_650 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_668 v0
  = coe d_identity_612 (coe d_isMonoid_660 (coe v0))
-- Algebra.Structures.IsCommutativeMonoid._.identityʳ
d_identity'691'_670 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsCommutativeMonoid_650 -> AgdaAny -> AgdaAny
d_identity'691'_670 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'691'_670 v6
du_identity'691'_670 ::
  T_IsCommutativeMonoid_650 -> AgdaAny -> AgdaAny
du_identity'691'_670 v0
  = coe du_identity'691'_642 (coe d_isMonoid_660 (coe v0))
-- Algebra.Structures.IsCommutativeMonoid._.identityˡ
d_identity'737'_672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsCommutativeMonoid_650 -> AgdaAny -> AgdaAny
d_identity'737'_672 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'737'_672 v6
du_identity'737'_672 ::
  T_IsCommutativeMonoid_650 -> AgdaAny -> AgdaAny
du_identity'737'_672 v0
  = coe du_identity'737'_640 (coe d_isMonoid_660 (coe v0))
-- Algebra.Structures.IsCommutativeMonoid._.isEquivalence
d_isEquivalence_674 ::
  T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_674 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe d_isSemigroup_610 (coe d_isMonoid_660 (coe v0))))
-- Algebra.Structures.IsCommutativeMonoid._.isMagma
d_isMagma_676 :: T_IsCommutativeMonoid_650 -> T_IsMagma_140
d_isMagma_676 v0
  = coe
      d_isMagma_444 (coe d_isSemigroup_610 (coe d_isMonoid_660 (coe v0)))
-- Algebra.Structures.IsCommutativeMonoid._.isPartialEquivalence
d_isPartialEquivalence_678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_678 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_678 v6
du_isPartialEquivalence_678 ::
  T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_678 v0
  = let v1 = d_isMonoid_660 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    let v3 = d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v3))
-- Algebra.Structures.IsCommutativeMonoid._.isSemigroup
d_isSemigroup_680 :: T_IsCommutativeMonoid_650 -> T_IsSemigroup_436
d_isSemigroup_680 v0
  = coe d_isSemigroup_610 (coe d_isMonoid_660 (coe v0))
-- Algebra.Structures.IsCommutativeMonoid._.isUnitalMagma
d_isUnitalMagma_682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsCommutativeMonoid_650 -> T_IsUnitalMagma_556
d_isUnitalMagma_682 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isUnitalMagma_682 v6
du_isUnitalMagma_682 ::
  T_IsCommutativeMonoid_650 -> T_IsUnitalMagma_556
du_isUnitalMagma_682 v0
  = coe du_isUnitalMagma_644 (coe d_isMonoid_660 (coe v0))
-- Algebra.Structures.IsCommutativeMonoid._.refl
d_refl_684 :: T_IsCommutativeMonoid_650 -> AgdaAny -> AgdaAny
d_refl_684 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_isMonoid_660 (coe v0)))))
-- Algebra.Structures.IsCommutativeMonoid._.reflexive
d_reflexive_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeMonoid_650 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_686 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_686 v6
du_reflexive_686 ::
  T_IsCommutativeMonoid_650 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_686 v0
  = let v1 = d_isMonoid_660 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    let v3 = d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v3)) v4
-- Algebra.Structures.IsCommutativeMonoid._.setoid
d_setoid_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_688 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_688 v6
du_setoid_688 ::
  T_IsCommutativeMonoid_650 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_688 v0
  = let v1 = d_isMonoid_660 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsCommutativeMonoid._.sym
d_sym_690 ::
  T_IsCommutativeMonoid_650 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_690 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_isMonoid_660 (coe v0)))))
-- Algebra.Structures.IsCommutativeMonoid._.trans
d_trans_692 ::
  T_IsCommutativeMonoid_650 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_692 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_isMonoid_660 (coe v0)))))
-- Algebra.Structures.IsCommutativeMonoid._.∙-cong
d_'8729''45'cong_694 ::
  T_IsCommutativeMonoid_650 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_694 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe d_isSemigroup_610 (coe d_isMonoid_660 (coe v0))))
-- Algebra.Structures.IsCommutativeMonoid._.∙-congʳ
d_'8729''45'cong'691'_696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeMonoid_650 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_696 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_696 v6
du_'8729''45'cong'691'_696 ::
  T_IsCommutativeMonoid_650 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_696 v0
  = let v1 = d_isMonoid_660 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsCommutativeMonoid._.∙-congˡ
d_'8729''45'cong'737'_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeMonoid_650 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_698 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_698 v6
du_'8729''45'cong'737'_698 ::
  T_IsCommutativeMonoid_650 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_698 v0
  = let v1 = d_isMonoid_660 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsCommutativeMonoid.isCommutativeSemigroup
d_isCommutativeSemigroup_700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeMonoid_650 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_700 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isCommutativeSemigroup_700 v6
du_isCommutativeSemigroup_700 ::
  T_IsCommutativeMonoid_650 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_700 v0
  = coe
      C_IsCommutativeSemigroup'46'constructor_10975
      (coe d_isSemigroup_610 (coe d_isMonoid_660 (coe v0)))
      (coe d_comm_662 (coe v0))
-- Algebra.Structures.IsCommutativeMonoid._.isCommutativeMagma
d_isCommutativeMagma_704 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsCommutativeMonoid_650 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_704 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isCommutativeMagma_704 v6
du_isCommutativeMagma_704 ::
  T_IsCommutativeMonoid_650 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_704 v0
  = coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v0))
-- Algebra.Structures.IsIdempotentCommutativeMonoid
d_IsIdempotentCommutativeMonoid_710 a0 a1 a2 a3 a4 a5 = ()
data T_IsIdempotentCommutativeMonoid_710
  = C_IsIdempotentCommutativeMonoid'46'constructor_16919 T_IsCommutativeMonoid_650
                                                         (AgdaAny -> AgdaAny)
-- Algebra.Structures.IsIdempotentCommutativeMonoid.isCommutativeMonoid
d_isCommutativeMonoid_720 ::
  T_IsIdempotentCommutativeMonoid_710 -> T_IsCommutativeMonoid_650
d_isCommutativeMonoid_720 v0
  = case coe v0 of
      C_IsIdempotentCommutativeMonoid'46'constructor_16919 v1 v2
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsIdempotentCommutativeMonoid.idem
d_idem_722 ::
  T_IsIdempotentCommutativeMonoid_710 -> AgdaAny -> AgdaAny
d_idem_722 v0
  = case coe v0 of
      C_IsIdempotentCommutativeMonoid'46'constructor_16919 v1 v2
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.assoc
d_assoc_726 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_726 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0))))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.comm
d_comm_728 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_728 v0
  = coe d_comm_662 (coe d_isCommutativeMonoid_720 (coe v0))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.identity
d_identity_730 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_730 v0
  = coe
      d_identity_612
      (coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0)))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.identityʳ
d_identity'691'_732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsIdempotentCommutativeMonoid_710 -> AgdaAny -> AgdaAny
d_identity'691'_732 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'691'_732 v6
du_identity'691'_732 ::
  T_IsIdempotentCommutativeMonoid_710 -> AgdaAny -> AgdaAny
du_identity'691'_732 v0
  = let v1 = d_isCommutativeMonoid_720 (coe v0) in
    coe du_identity'691'_642 (coe d_isMonoid_660 (coe v1))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.identityˡ
d_identity'737'_734 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsIdempotentCommutativeMonoid_710 -> AgdaAny -> AgdaAny
d_identity'737'_734 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_identity'737'_734 v6
du_identity'737'_734 ::
  T_IsIdempotentCommutativeMonoid_710 -> AgdaAny -> AgdaAny
du_identity'737'_734 v0
  = let v1 = d_isCommutativeMonoid_720 (coe v0) in
    coe du_identity'737'_640 (coe d_isMonoid_660 (coe v1))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.isCommutativeMagma
d_isCommutativeMagma_736 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsIdempotentCommutativeMonoid_710 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_736 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isCommutativeMagma_736 v6
du_isCommutativeMagma_736 ::
  T_IsIdempotentCommutativeMonoid_710 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_736 v0
  = let v1 = d_isCommutativeMonoid_720 (coe v0) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v1))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.isCommutativeSemigroup
d_isCommutativeSemigroup_738 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsIdempotentCommutativeMonoid_710 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_738 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isCommutativeSemigroup_738 v6
du_isCommutativeSemigroup_738 ::
  T_IsIdempotentCommutativeMonoid_710 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_738 v0
  = coe
      du_isCommutativeSemigroup_700
      (coe d_isCommutativeMonoid_720 (coe v0))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.isEquivalence
d_isEquivalence_740 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_740 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0)))))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.isMagma
d_isMagma_742 ::
  T_IsIdempotentCommutativeMonoid_710 -> T_IsMagma_140
d_isMagma_742 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0))))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.isMonoid
d_isMonoid_744 ::
  T_IsIdempotentCommutativeMonoid_710 -> T_IsMonoid_600
d_isMonoid_744 v0
  = coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.isPartialEquivalence
d_isPartialEquivalence_746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_746 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_746 v6
du_isPartialEquivalence_746 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_746 v0
  = let v1 = d_isCommutativeMonoid_720 (coe v0) in
    let v2 = d_isMonoid_660 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    let v4 = d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v4))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.isSemigroup
d_isSemigroup_748 ::
  T_IsIdempotentCommutativeMonoid_710 -> T_IsSemigroup_436
d_isSemigroup_748 v0
  = coe
      d_isSemigroup_610
      (coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0)))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.isUnitalMagma
d_isUnitalMagma_750 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsIdempotentCommutativeMonoid_710 -> T_IsUnitalMagma_556
d_isUnitalMagma_750 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isUnitalMagma_750 v6
du_isUnitalMagma_750 ::
  T_IsIdempotentCommutativeMonoid_710 -> T_IsUnitalMagma_556
du_isUnitalMagma_750 v0
  = let v1 = d_isCommutativeMonoid_720 (coe v0) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_660 (coe v1))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.refl
d_refl_752 ::
  T_IsIdempotentCommutativeMonoid_710 -> AgdaAny -> AgdaAny
d_refl_752 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0))))))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.reflexive
d_reflexive_754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_754 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_754 v6
du_reflexive_754 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_754 v0
  = let v1 = d_isCommutativeMonoid_720 (coe v0) in
    let v2 = d_isMonoid_660 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    let v4 = d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v4)) v5
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.setoid
d_setoid_756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_756 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_setoid_756 v6
du_setoid_756 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_756 v0
  = let v1 = d_isCommutativeMonoid_720 (coe v0) in
    let v2 = d_isMonoid_660 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.sym
d_sym_758 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_758 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0))))))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.trans
d_trans_760 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_760 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0))))))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.∙-cong
d_'8729''45'cong_762 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_762 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0)))))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.∙-congʳ
d_'8729''45'cong'691'_764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_764 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'691'_764 v6
du_'8729''45'cong'691'_764 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_764 v0
  = let v1 = d_isCommutativeMonoid_720 (coe v0) in
    let v2 = d_isMonoid_660 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsIdempotentCommutativeMonoid._.∙-congˡ
d_'8729''45'cong'737'_766 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_766 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8729''45'cong'737'_766 v6
du_'8729''45'cong'737'_766 ::
  T_IsIdempotentCommutativeMonoid_710 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_766 v0
  = let v1 = d_isCommutativeMonoid_720 (coe v0) in
    let v2 = d_isMonoid_660 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsIdempotentCommutativeMonoid.isBand
d_isBand_768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsIdempotentCommutativeMonoid_710 -> T_IsBand_472
d_isBand_768 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_isBand_768 v6
du_isBand_768 ::
  T_IsIdempotentCommutativeMonoid_710 -> T_IsBand_472
du_isBand_768 v0
  = coe
      C_IsBand'46'constructor_10089
      (coe
         d_isSemigroup_610
         (coe d_isMonoid_660 (coe d_isCommutativeMonoid_720 (coe v0))))
      (coe d_idem_722 (coe v0))
-- Algebra.Structures.IsInvertibleMagma
d_IsInvertibleMagma_776 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsInvertibleMagma_776
  = C_IsInvertibleMagma'46'constructor_18645 T_IsMagma_140
                                             MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                             (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsInvertibleMagma.isMagma
d_isMagma_790 :: T_IsInvertibleMagma_776 -> T_IsMagma_140
d_isMagma_790 v0
  = case coe v0 of
      C_IsInvertibleMagma'46'constructor_18645 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsInvertibleMagma.inverse
d_inverse_792 ::
  T_IsInvertibleMagma_776 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_792 v0
  = case coe v0 of
      C_IsInvertibleMagma'46'constructor_18645 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsInvertibleMagma.⁻¹-cong
d_'8315''185''45'cong_794 ::
  T_IsInvertibleMagma_776 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_794 v0
  = case coe v0 of
      C_IsInvertibleMagma'46'constructor_18645 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsInvertibleMagma._.isEquivalence
d_isEquivalence_798 ::
  T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_798 v0
  = coe d_isEquivalence_148 (coe d_isMagma_790 (coe v0))
-- Algebra.Structures.IsInvertibleMagma._.isPartialEquivalence
d_isPartialEquivalence_800 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_800 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_800 v7
du_isPartialEquivalence_800 ::
  T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_800 v0
  = let v1 = d_isMagma_790 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsInvertibleMagma._.refl
d_refl_802 :: T_IsInvertibleMagma_776 -> AgdaAny -> AgdaAny
d_refl_802 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_790 (coe v0)))
-- Algebra.Structures.IsInvertibleMagma._.reflexive
d_reflexive_804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleMagma_776 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_804 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_804 v7
du_reflexive_804 ::
  T_IsInvertibleMagma_776 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_804 v0
  = let v1 = d_isMagma_790 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsInvertibleMagma._.setoid
d_setoid_806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_806 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_setoid_806 v7
du_setoid_806 ::
  T_IsInvertibleMagma_776 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_806 v0 = coe du_setoid_164 (coe d_isMagma_790 (coe v0))
-- Algebra.Structures.IsInvertibleMagma._.sym
d_sym_808 ::
  T_IsInvertibleMagma_776 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_808 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_790 (coe v0)))
-- Algebra.Structures.IsInvertibleMagma._.trans
d_trans_810 ::
  T_IsInvertibleMagma_776 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_810 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_790 (coe v0)))
-- Algebra.Structures.IsInvertibleMagma._.∙-cong
d_'8729''45'cong_812 ::
  T_IsInvertibleMagma_776 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_812 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_790 (coe v0))
-- Algebra.Structures.IsInvertibleMagma._.∙-congʳ
d_'8729''45'cong'691'_814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleMagma_776 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_814 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'691'_814 v7
du_'8729''45'cong'691'_814 ::
  T_IsInvertibleMagma_776 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_814 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_790 (coe v0))
-- Algebra.Structures.IsInvertibleMagma._.∙-congˡ
d_'8729''45'cong'737'_816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleMagma_776 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_816 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'737'_816 v7
du_'8729''45'cong'737'_816 ::
  T_IsInvertibleMagma_776 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_816 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_790 (coe v0))
-- Algebra.Structures.IsInvertibleMagma.inverseˡ
d_inverse'737'_818 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleMagma_776 -> AgdaAny -> AgdaAny
d_inverse'737'_818 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_inverse'737'_818 v7
du_inverse'737'_818 ::
  T_IsInvertibleMagma_776 -> AgdaAny -> AgdaAny
du_inverse'737'_818 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_inverse_792 (coe v0))
-- Algebra.Structures.IsInvertibleMagma.inverseʳ
d_inverse'691'_820 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleMagma_776 -> AgdaAny -> AgdaAny
d_inverse'691'_820 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_inverse'691'_820 v7
du_inverse'691'_820 ::
  T_IsInvertibleMagma_776 -> AgdaAny -> AgdaAny
du_inverse'691'_820 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_inverse_792 (coe v0))
-- Algebra.Structures.IsInvertibleUnitalMagma
d_IsInvertibleUnitalMagma_828 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsInvertibleUnitalMagma_828
  = C_IsInvertibleUnitalMagma'46'constructor_20515 T_IsInvertibleMagma_776
                                                   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsInvertibleUnitalMagma.isInvertibleMagma
d_isInvertibleMagma_840 ::
  T_IsInvertibleUnitalMagma_828 -> T_IsInvertibleMagma_776
d_isInvertibleMagma_840 v0
  = case coe v0 of
      C_IsInvertibleUnitalMagma'46'constructor_20515 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsInvertibleUnitalMagma.identity
d_identity_842 ::
  T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_842 v0
  = case coe v0 of
      C_IsInvertibleUnitalMagma'46'constructor_20515 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsInvertibleUnitalMagma._.inverse
d_inverse_846 ::
  T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_846 v0
  = coe d_inverse_792 (coe d_isInvertibleMagma_840 (coe v0))
-- Algebra.Structures.IsInvertibleUnitalMagma._.inverseʳ
d_inverse'691'_848 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleUnitalMagma_828 -> AgdaAny -> AgdaAny
d_inverse'691'_848 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_inverse'691'_848 v7
du_inverse'691'_848 ::
  T_IsInvertibleUnitalMagma_828 -> AgdaAny -> AgdaAny
du_inverse'691'_848 v0
  = coe du_inverse'691'_820 (coe d_isInvertibleMagma_840 (coe v0))
-- Algebra.Structures.IsInvertibleUnitalMagma._.inverseˡ
d_inverse'737'_850 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleUnitalMagma_828 -> AgdaAny -> AgdaAny
d_inverse'737'_850 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_inverse'737'_850 v7
du_inverse'737'_850 ::
  T_IsInvertibleUnitalMagma_828 -> AgdaAny -> AgdaAny
du_inverse'737'_850 v0
  = coe du_inverse'737'_818 (coe d_isInvertibleMagma_840 (coe v0))
-- Algebra.Structures.IsInvertibleUnitalMagma._.isEquivalence
d_isEquivalence_852 ::
  T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_852 v0
  = coe
      d_isEquivalence_148
      (coe d_isMagma_790 (coe d_isInvertibleMagma_840 (coe v0)))
-- Algebra.Structures.IsInvertibleUnitalMagma._.isMagma
d_isMagma_854 :: T_IsInvertibleUnitalMagma_828 -> T_IsMagma_140
d_isMagma_854 v0
  = coe d_isMagma_790 (coe d_isInvertibleMagma_840 (coe v0))
-- Algebra.Structures.IsInvertibleUnitalMagma._.isPartialEquivalence
d_isPartialEquivalence_856 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_856 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_856 v7
du_isPartialEquivalence_856 ::
  T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_856 v0
  = let v1 = d_isInvertibleMagma_840 (coe v0) in
    let v2 = d_isMagma_790 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v2))
-- Algebra.Structures.IsInvertibleUnitalMagma._.refl
d_refl_858 :: T_IsInvertibleUnitalMagma_828 -> AgdaAny -> AgdaAny
d_refl_858 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe d_isMagma_790 (coe d_isInvertibleMagma_840 (coe v0))))
-- Algebra.Structures.IsInvertibleUnitalMagma._.reflexive
d_reflexive_860 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleUnitalMagma_828 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_860 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_860 v7
du_reflexive_860 ::
  T_IsInvertibleUnitalMagma_828 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_860 v0
  = let v1 = d_isInvertibleMagma_840 (coe v0) in
    let v2 = d_isMagma_790 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v2)) v3
-- Algebra.Structures.IsInvertibleUnitalMagma._.setoid
d_setoid_862 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_862 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_setoid_862 v7
du_setoid_862 ::
  T_IsInvertibleUnitalMagma_828 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_862 v0
  = let v1 = d_isInvertibleMagma_840 (coe v0) in
    coe du_setoid_164 (coe d_isMagma_790 (coe v1))
-- Algebra.Structures.IsInvertibleUnitalMagma._.sym
d_sym_864 ::
  T_IsInvertibleUnitalMagma_828 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_864 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe d_isMagma_790 (coe d_isInvertibleMagma_840 (coe v0))))
-- Algebra.Structures.IsInvertibleUnitalMagma._.trans
d_trans_866 ::
  T_IsInvertibleUnitalMagma_828 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_866 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe d_isMagma_790 (coe d_isInvertibleMagma_840 (coe v0))))
-- Algebra.Structures.IsInvertibleUnitalMagma._.⁻¹-cong
d_'8315''185''45'cong_868 ::
  T_IsInvertibleUnitalMagma_828 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_868 v0
  = coe
      d_'8315''185''45'cong_794 (coe d_isInvertibleMagma_840 (coe v0))
-- Algebra.Structures.IsInvertibleUnitalMagma._.∙-cong
d_'8729''45'cong_870 ::
  T_IsInvertibleUnitalMagma_828 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_870 v0
  = coe
      d_'8729''45'cong_150
      (coe d_isMagma_790 (coe d_isInvertibleMagma_840 (coe v0)))
-- Algebra.Structures.IsInvertibleUnitalMagma._.∙-congʳ
d_'8729''45'cong'691'_872 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleUnitalMagma_828 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_872 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'691'_872 v7
du_'8729''45'cong'691'_872 ::
  T_IsInvertibleUnitalMagma_828 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_872 v0
  = let v1 = d_isInvertibleMagma_840 (coe v0) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_790 (coe v1))
-- Algebra.Structures.IsInvertibleUnitalMagma._.∙-congˡ
d_'8729''45'cong'737'_874 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleUnitalMagma_828 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_874 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'737'_874 v7
du_'8729''45'cong'737'_874 ::
  T_IsInvertibleUnitalMagma_828 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_874 v0
  = let v1 = d_isInvertibleMagma_840 (coe v0) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_790 (coe v1))
-- Algebra.Structures.IsInvertibleUnitalMagma.identityˡ
d_identity'737'_876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleUnitalMagma_828 -> AgdaAny -> AgdaAny
d_identity'737'_876 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_identity'737'_876 v7
du_identity'737'_876 ::
  T_IsInvertibleUnitalMagma_828 -> AgdaAny -> AgdaAny
du_identity'737'_876 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_identity_842 (coe v0))
-- Algebra.Structures.IsInvertibleUnitalMagma.identityʳ
d_identity'691'_878 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleUnitalMagma_828 -> AgdaAny -> AgdaAny
d_identity'691'_878 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_identity'691'_878 v7
du_identity'691'_878 ::
  T_IsInvertibleUnitalMagma_828 -> AgdaAny -> AgdaAny
du_identity'691'_878 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_identity_842 (coe v0))
-- Algebra.Structures.IsInvertibleUnitalMagma.isUnitalMagma
d_isUnitalMagma_880 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsInvertibleUnitalMagma_828 -> T_IsUnitalMagma_556
d_isUnitalMagma_880 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isUnitalMagma_880 v7
du_isUnitalMagma_880 ::
  T_IsInvertibleUnitalMagma_828 -> T_IsUnitalMagma_556
du_isUnitalMagma_880 v0
  = coe
      C_IsUnitalMagma'46'constructor_12005
      (coe d_isMagma_790 (coe d_isInvertibleMagma_840 (coe v0)))
      (coe d_identity_842 (coe v0))
-- Algebra.Structures.IsGroup
d_IsGroup_888 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsGroup_888
  = C_IsGroup'46'constructor_22905 T_IsMonoid_600
                                   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                   (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsGroup.isMonoid
d_isMonoid_902 :: T_IsGroup_888 -> T_IsMonoid_600
d_isMonoid_902 v0
  = case coe v0 of
      C_IsGroup'46'constructor_22905 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsGroup.inverse
d_inverse_904 ::
  T_IsGroup_888 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_904 v0
  = case coe v0 of
      C_IsGroup'46'constructor_22905 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsGroup.⁻¹-cong
d_'8315''185''45'cong_906 ::
  T_IsGroup_888 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_906 v0
  = case coe v0 of
      C_IsGroup'46'constructor_22905 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsGroup._.assoc
d_assoc_910 ::
  T_IsGroup_888 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_910 v0
  = coe
      d_assoc_446 (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v0)))
-- Algebra.Structures.IsGroup._.identity
d_identity_912 ::
  T_IsGroup_888 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_912 v0
  = coe d_identity_612 (coe d_isMonoid_902 (coe v0))
-- Algebra.Structures.IsGroup._.identityʳ
d_identity'691'_914 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsGroup_888 -> AgdaAny -> AgdaAny
d_identity'691'_914 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_identity'691'_914 v7
du_identity'691'_914 :: T_IsGroup_888 -> AgdaAny -> AgdaAny
du_identity'691'_914 v0
  = coe du_identity'691'_642 (coe d_isMonoid_902 (coe v0))
-- Algebra.Structures.IsGroup._.identityˡ
d_identity'737'_916 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsGroup_888 -> AgdaAny -> AgdaAny
d_identity'737'_916 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_identity'737'_916 v7
du_identity'737'_916 :: T_IsGroup_888 -> AgdaAny -> AgdaAny
du_identity'737'_916 v0
  = coe du_identity'737'_640 (coe d_isMonoid_902 (coe v0))
-- Algebra.Structures.IsGroup._.isEquivalence
d_isEquivalence_918 ::
  T_IsGroup_888 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_918 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v0))))
-- Algebra.Structures.IsGroup._.isMagma
d_isMagma_920 :: T_IsGroup_888 -> T_IsMagma_140
d_isMagma_920 v0
  = coe
      d_isMagma_444 (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v0)))
-- Algebra.Structures.IsGroup._.isPartialEquivalence
d_isPartialEquivalence_922 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_922 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_922 v7
du_isPartialEquivalence_922 ::
  T_IsGroup_888 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_922 v0
  = let v1 = d_isMonoid_902 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    let v3 = d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v3))
-- Algebra.Structures.IsGroup._.isSemigroup
d_isSemigroup_924 :: T_IsGroup_888 -> T_IsSemigroup_436
d_isSemigroup_924 v0
  = coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v0))
-- Algebra.Structures.IsGroup._.isUnitalMagma
d_isUnitalMagma_926 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsGroup_888 -> T_IsUnitalMagma_556
d_isUnitalMagma_926 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isUnitalMagma_926 v7
du_isUnitalMagma_926 :: T_IsGroup_888 -> T_IsUnitalMagma_556
du_isUnitalMagma_926 v0
  = coe du_isUnitalMagma_644 (coe d_isMonoid_902 (coe v0))
-- Algebra.Structures.IsGroup._.refl
d_refl_928 :: T_IsGroup_888 -> AgdaAny -> AgdaAny
d_refl_928 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v0)))))
-- Algebra.Structures.IsGroup._.reflexive
d_reflexive_930 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_930 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_930 v7
du_reflexive_930 ::
  T_IsGroup_888 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_930 v0
  = let v1 = d_isMonoid_902 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    let v3 = d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v3)) v4
-- Algebra.Structures.IsGroup._.setoid
d_setoid_932 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_932 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_setoid_932 v7
du_setoid_932 ::
  T_IsGroup_888 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_932 v0
  = let v1 = d_isMonoid_902 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsGroup._.sym
d_sym_934 ::
  T_IsGroup_888 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_934 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v0)))))
-- Algebra.Structures.IsGroup._.trans
d_trans_936 ::
  T_IsGroup_888 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_936 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v0)))))
-- Algebra.Structures.IsGroup._.∙-cong
d_'8729''45'cong_938 ::
  T_IsGroup_888 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_938 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v0))))
-- Algebra.Structures.IsGroup._.∙-congʳ
d_'8729''45'cong'691'_940 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_940 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'691'_940 v7
du_'8729''45'cong'691'_940 ::
  T_IsGroup_888 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_940 v0
  = let v1 = d_isMonoid_902 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsGroup._.∙-congˡ
d_'8729''45'cong'737'_942 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_942 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'737'_942 v7
du_'8729''45'cong'737'_942 ::
  T_IsGroup_888 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_942 v0
  = let v1 = d_isMonoid_902 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsGroup._-_
d__'45'__944 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__944 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 ~v7 v8 v9
  = du__'45'__944 v4 v6 v8 v9
du__'45'__944 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__944 v0 v1 v2 v3 = coe v0 v2 (coe v1 v3)
-- Algebra.Structures.IsGroup.inverseˡ
d_inverse'737'_950 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsGroup_888 -> AgdaAny -> AgdaAny
d_inverse'737'_950 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_inverse'737'_950 v7
du_inverse'737'_950 :: T_IsGroup_888 -> AgdaAny -> AgdaAny
du_inverse'737'_950 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_inverse_904 (coe v0))
-- Algebra.Structures.IsGroup.inverseʳ
d_inverse'691'_952 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsGroup_888 -> AgdaAny -> AgdaAny
d_inverse'691'_952 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_inverse'691'_952 v7
du_inverse'691'_952 :: T_IsGroup_888 -> AgdaAny -> AgdaAny
du_inverse'691'_952 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_inverse_904 (coe v0))
-- Algebra.Structures.IsGroup.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_958 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_958 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_unique'737''45''8315''185'_958 v4 v5 v6 v7
du_unique'737''45''8315''185'_958 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_958 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_assoc'43'id'43'inv'691''8658'inv'737''45'unique_342
      (let v4 = d_isSemigroup_610 (coe d_isMonoid_902 (coe v3)) in
       coe du_setoid_164 (coe d_isMagma_444 (coe v4)))
      (coe v0) (coe v2) (coe v1)
      (coe
         d_'8729''45'cong_150
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v3)))))
      (coe
         d_assoc_446 (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v3))))
      (coe d_identity_612 (coe d_isMonoid_902 (coe v3)))
      (coe du_inverse'691'_952 (coe v3))
-- Algebra.Structures.IsGroup.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_964 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_964 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_unique'691''45''8315''185'_964 v4 v5 v6 v7
du_unique'691''45''8315''185'_964 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_964 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Consequences.Setoid.du_assoc'43'id'43'inv'737''8658'inv'691''45'unique_362
      (let v4 = d_isSemigroup_610 (coe d_isMonoid_902 (coe v3)) in
       coe du_setoid_164 (coe d_isMagma_444 (coe v4)))
      (coe v0) (coe v2) (coe v1)
      (coe
         d_'8729''45'cong_150
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v3)))))
      (coe
         d_assoc_446 (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v3))))
      (coe d_identity_612 (coe d_isMonoid_902 (coe v3)))
      (coe du_inverse'737'_950 (coe v3))
-- Algebra.Structures.IsGroup.isInvertibleMagma
d_isInvertibleMagma_966 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsGroup_888 -> T_IsInvertibleMagma_776
d_isInvertibleMagma_966 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isInvertibleMagma_966 v7
du_isInvertibleMagma_966 ::
  T_IsGroup_888 -> T_IsInvertibleMagma_776
du_isInvertibleMagma_966 v0
  = coe
      C_IsInvertibleMagma'46'constructor_18645
      (coe
         d_isMagma_444
         (coe d_isSemigroup_610 (coe d_isMonoid_902 (coe v0))))
      (coe d_inverse_904 (coe v0))
      (coe d_'8315''185''45'cong_906 (coe v0))
-- Algebra.Structures.IsGroup.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_968 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsGroup_888 -> T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_968 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isInvertibleUnitalMagma_968 v7
du_isInvertibleUnitalMagma_968 ::
  T_IsGroup_888 -> T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_968 v0
  = coe
      C_IsInvertibleUnitalMagma'46'constructor_20515
      (coe du_isInvertibleMagma_966 (coe v0))
      (coe d_identity_612 (coe d_isMonoid_902 (coe v0)))
-- Algebra.Structures.IsAbelianGroup
d_IsAbelianGroup_976 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsAbelianGroup_976
  = C_IsAbelianGroup'46'constructor_27897 T_IsGroup_888
                                          (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsAbelianGroup.isGroup
d_isGroup_988 :: T_IsAbelianGroup_976 -> T_IsGroup_888
d_isGroup_988 v0
  = case coe v0 of
      C_IsAbelianGroup'46'constructor_27897 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsAbelianGroup.comm
d_comm_990 :: T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_990 v0
  = case coe v0 of
      C_IsAbelianGroup'46'constructor_27897 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsAbelianGroup._._-_
d__'45'__994 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__994 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 ~v7 = du__'45'__994 v4 v6
du__'45'__994 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__994 v0 v1 = coe du__'45'__944 (coe v0) (coe v1)
-- Algebra.Structures.IsAbelianGroup._.assoc
d_assoc_996 ::
  T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_996 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe d_isMonoid_902 (coe d_isGroup_988 (coe v0))))
-- Algebra.Structures.IsAbelianGroup._.identity
d_identity_998 ::
  T_IsAbelianGroup_976 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_998 v0
  = coe
      d_identity_612 (coe d_isMonoid_902 (coe d_isGroup_988 (coe v0)))
-- Algebra.Structures.IsAbelianGroup._.identityʳ
d_identity'691'_1000 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny
d_identity'691'_1000 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_identity'691'_1000 v7
du_identity'691'_1000 :: T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny
du_identity'691'_1000 v0
  = let v1 = d_isGroup_988 (coe v0) in
    coe du_identity'691'_642 (coe d_isMonoid_902 (coe v1))
-- Algebra.Structures.IsAbelianGroup._.identityˡ
d_identity'737'_1002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny
d_identity'737'_1002 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_identity'737'_1002 v7
du_identity'737'_1002 :: T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny
du_identity'737'_1002 v0
  = let v1 = d_isGroup_988 (coe v0) in
    coe du_identity'737'_640 (coe d_isMonoid_902 (coe v1))
-- Algebra.Structures.IsAbelianGroup._.inverse
d_inverse_1004 ::
  T_IsAbelianGroup_976 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_1004 v0 = coe d_inverse_904 (coe d_isGroup_988 (coe v0))
-- Algebra.Structures.IsAbelianGroup._.inverseʳ
d_inverse'691'_1006 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny
d_inverse'691'_1006 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_inverse'691'_1006 v7
du_inverse'691'_1006 :: T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny
du_inverse'691'_1006 v0
  = coe du_inverse'691'_952 (coe d_isGroup_988 (coe v0))
-- Algebra.Structures.IsAbelianGroup._.inverseˡ
d_inverse'737'_1008 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny
d_inverse'737'_1008 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_inverse'737'_1008 v7
du_inverse'737'_1008 :: T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny
du_inverse'737'_1008 v0
  = coe du_inverse'737'_950 (coe d_isGroup_988 (coe v0))
-- Algebra.Structures.IsAbelianGroup._.isEquivalence
d_isEquivalence_1010 ::
  T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1010 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe d_isMonoid_902 (coe d_isGroup_988 (coe v0)))))
-- Algebra.Structures.IsAbelianGroup._.isInvertibleMagma
d_isInvertibleMagma_1012 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 -> T_IsInvertibleMagma_776
d_isInvertibleMagma_1012 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isInvertibleMagma_1012 v7
du_isInvertibleMagma_1012 ::
  T_IsAbelianGroup_976 -> T_IsInvertibleMagma_776
du_isInvertibleMagma_1012 v0
  = coe du_isInvertibleMagma_966 (coe d_isGroup_988 (coe v0))
-- Algebra.Structures.IsAbelianGroup._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_1014 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 -> T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_1014 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isInvertibleUnitalMagma_1014 v7
du_isInvertibleUnitalMagma_1014 ::
  T_IsAbelianGroup_976 -> T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_1014 v0
  = coe du_isInvertibleUnitalMagma_968 (coe d_isGroup_988 (coe v0))
-- Algebra.Structures.IsAbelianGroup._.isMagma
d_isMagma_1016 :: T_IsAbelianGroup_976 -> T_IsMagma_140
d_isMagma_1016 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe d_isMonoid_902 (coe d_isGroup_988 (coe v0))))
-- Algebra.Structures.IsAbelianGroup._.isMonoid
d_isMonoid_1018 :: T_IsAbelianGroup_976 -> T_IsMonoid_600
d_isMonoid_1018 v0
  = coe d_isMonoid_902 (coe d_isGroup_988 (coe v0))
-- Algebra.Structures.IsAbelianGroup._.isPartialEquivalence
d_isPartialEquivalence_1020 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1020 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_1020 v7
du_isPartialEquivalence_1020 ::
  T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1020 v0
  = let v1 = d_isGroup_988 (coe v0) in
    let v2 = d_isMonoid_902 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    let v4 = d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v4))
-- Algebra.Structures.IsAbelianGroup._.isSemigroup
d_isSemigroup_1022 :: T_IsAbelianGroup_976 -> T_IsSemigroup_436
d_isSemigroup_1022 v0
  = coe
      d_isSemigroup_610 (coe d_isMonoid_902 (coe d_isGroup_988 (coe v0)))
-- Algebra.Structures.IsAbelianGroup._.isUnitalMagma
d_isUnitalMagma_1024 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsAbelianGroup_976 -> T_IsUnitalMagma_556
d_isUnitalMagma_1024 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isUnitalMagma_1024 v7
du_isUnitalMagma_1024 ::
  T_IsAbelianGroup_976 -> T_IsUnitalMagma_556
du_isUnitalMagma_1024 v0
  = let v1 = d_isGroup_988 (coe v0) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_902 (coe v1))
-- Algebra.Structures.IsAbelianGroup._.refl
d_refl_1026 :: T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny
d_refl_1026 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe d_isMonoid_902 (coe d_isGroup_988 (coe v0))))))
-- Algebra.Structures.IsAbelianGroup._.reflexive
d_reflexive_1028 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1028 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_1028 v7
du_reflexive_1028 ::
  T_IsAbelianGroup_976 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1028 v0
  = let v1 = d_isGroup_988 (coe v0) in
    let v2 = d_isMonoid_902 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    let v4 = d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v4)) v5
-- Algebra.Structures.IsAbelianGroup._.setoid
d_setoid_1030 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1030 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_setoid_1030 v7
du_setoid_1030 ::
  T_IsAbelianGroup_976 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1030 v0
  = let v1 = d_isGroup_988 (coe v0) in
    let v2 = d_isMonoid_902 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsAbelianGroup._.sym
d_sym_1032 ::
  T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1032 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe d_isMonoid_902 (coe d_isGroup_988 (coe v0))))))
-- Algebra.Structures.IsAbelianGroup._.trans
d_trans_1034 ::
  T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1034 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe d_isMonoid_902 (coe d_isGroup_988 (coe v0))))))
-- Algebra.Structures.IsAbelianGroup._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_1036 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_1036 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_unique'691''45''8315''185'_1036 v4 v5 v6 v7
du_unique'691''45''8315''185'_1036 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_1036 v0 v1 v2 v3
  = coe
      du_unique'691''45''8315''185'_964 (coe v0) (coe v1) (coe v2)
      (coe d_isGroup_988 (coe v3))
-- Algebra.Structures.IsAbelianGroup._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_1038 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_1038 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_unique'737''45''8315''185'_1038 v4 v5 v6 v7
du_unique'737''45''8315''185'_1038 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_1038 v0 v1 v2 v3
  = coe
      du_unique'737''45''8315''185'_958 (coe v0) (coe v1) (coe v2)
      (coe d_isGroup_988 (coe v3))
-- Algebra.Structures.IsAbelianGroup._.⁻¹-cong
d_'8315''185''45'cong_1040 ::
  T_IsAbelianGroup_976 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_1040 v0
  = coe d_'8315''185''45'cong_906 (coe d_isGroup_988 (coe v0))
-- Algebra.Structures.IsAbelianGroup._.∙-cong
d_'8729''45'cong_1042 ::
  T_IsAbelianGroup_976 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1042 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe d_isMonoid_902 (coe d_isGroup_988 (coe v0)))))
-- Algebra.Structures.IsAbelianGroup._.∙-congʳ
d_'8729''45'cong'691'_1044 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1044 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'691'_1044 v7
du_'8729''45'cong'691'_1044 ::
  T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1044 v0
  = let v1 = d_isGroup_988 (coe v0) in
    let v2 = d_isMonoid_902 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsAbelianGroup._.∙-congˡ
d_'8729''45'cong'737'_1046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1046 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'737'_1046 v7
du_'8729''45'cong'737'_1046 ::
  T_IsAbelianGroup_976 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1046 v0
  = let v1 = d_isGroup_988 (coe v0) in
    let v2 = d_isMonoid_902 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsAbelianGroup.isCommutativeMonoid
d_isCommutativeMonoid_1048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 -> T_IsCommutativeMonoid_650
d_isCommutativeMonoid_1048 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isCommutativeMonoid_1048 v7
du_isCommutativeMonoid_1048 ::
  T_IsAbelianGroup_976 -> T_IsCommutativeMonoid_650
du_isCommutativeMonoid_1048 v0
  = coe
      C_IsCommutativeMonoid'46'constructor_15379
      (coe d_isMonoid_902 (coe d_isGroup_988 (coe v0)))
      (coe d_comm_990 (coe v0))
-- Algebra.Structures.IsAbelianGroup._.isCommutativeMagma
d_isCommutativeMagma_1052 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_1052 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isCommutativeMagma_1052 v7
du_isCommutativeMagma_1052 ::
  T_IsAbelianGroup_976 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_1052 v0
  = let v1 = coe du_isCommutativeMonoid_1048 (coe v0) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v1))
-- Algebra.Structures.IsAbelianGroup._.isCommutativeSemigroup
d_isCommutativeSemigroup_1054 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsAbelianGroup_976 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1054 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isCommutativeSemigroup_1054 v7
du_isCommutativeSemigroup_1054 ::
  T_IsAbelianGroup_976 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1054 v0
  = coe
      du_isCommutativeSemigroup_700
      (coe du_isCommutativeMonoid_1048 (coe v0))
-- Algebra.Structures.IsNearSemiring
d_IsNearSemiring_1062 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsNearSemiring_1062
  = C_IsNearSemiring'46'constructor_30479 T_IsMonoid_600
                                          (AgdaAny ->
                                           AgdaAny ->
                                           AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny)
-- Algebra.Structures.IsNearSemiring.+-isMonoid
d_'43''45'isMonoid_1080 :: T_IsNearSemiring_1062 -> T_IsMonoid_600
d_'43''45'isMonoid_1080 v0
  = case coe v0 of
      C_IsNearSemiring'46'constructor_30479 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNearSemiring.*-cong
d_'42''45'cong_1082 ::
  T_IsNearSemiring_1062 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1082 v0
  = case coe v0 of
      C_IsNearSemiring'46'constructor_30479 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNearSemiring.*-assoc
d_'42''45'assoc_1084 ::
  T_IsNearSemiring_1062 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1084 v0
  = case coe v0 of
      C_IsNearSemiring'46'constructor_30479 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNearSemiring.distribʳ
d_distrib'691'_1086 ::
  T_IsNearSemiring_1062 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1086 v0
  = case coe v0 of
      C_IsNearSemiring'46'constructor_30479 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNearSemiring.zeroˡ
d_zero'737'_1088 :: T_IsNearSemiring_1062 -> AgdaAny -> AgdaAny
d_zero'737'_1088 v0
  = case coe v0 of
      C_IsNearSemiring'46'constructor_30479 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNearSemiring._.assoc
d_assoc_1092 ::
  T_IsNearSemiring_1062 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1092 v0
  = coe
      d_assoc_446
      (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1080 (coe v0)))
-- Algebra.Structures.IsNearSemiring._.∙-cong
d_'8729''45'cong_1094 ::
  T_IsNearSemiring_1062 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1094 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1080 (coe v0))))
-- Algebra.Structures.IsNearSemiring._.∙-congʳ
d_'8729''45'cong'691'_1096 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsNearSemiring_1062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1096 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'691'_1096 v7
du_'8729''45'cong'691'_1096 ::
  T_IsNearSemiring_1062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1096 v0
  = let v1 = d_'43''45'isMonoid_1080 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsNearSemiring._.∙-congˡ
d_'8729''45'cong'737'_1098 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsNearSemiring_1062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1098 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'737'_1098 v7
du_'8729''45'cong'737'_1098 ::
  T_IsNearSemiring_1062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1098 v0
  = let v1 = d_'43''45'isMonoid_1080 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsNearSemiring._.identity
d_identity_1100 ::
  T_IsNearSemiring_1062 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1100 v0
  = coe d_identity_612 (coe d_'43''45'isMonoid_1080 (coe v0))
-- Algebra.Structures.IsNearSemiring._.identityʳ
d_identity'691'_1102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsNearSemiring_1062 -> AgdaAny -> AgdaAny
d_identity'691'_1102 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_identity'691'_1102 v7
du_identity'691'_1102 ::
  T_IsNearSemiring_1062 -> AgdaAny -> AgdaAny
du_identity'691'_1102 v0
  = coe du_identity'691'_642 (coe d_'43''45'isMonoid_1080 (coe v0))
-- Algebra.Structures.IsNearSemiring._.identityˡ
d_identity'737'_1104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsNearSemiring_1062 -> AgdaAny -> AgdaAny
d_identity'737'_1104 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_identity'737'_1104 v7
du_identity'737'_1104 ::
  T_IsNearSemiring_1062 -> AgdaAny -> AgdaAny
du_identity'737'_1104 v0
  = coe du_identity'737'_640 (coe d_'43''45'isMonoid_1080 (coe v0))
-- Algebra.Structures.IsNearSemiring._.isMagma
d_isMagma_1106 :: T_IsNearSemiring_1062 -> T_IsMagma_140
d_isMagma_1106 v0
  = coe
      d_isMagma_444
      (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1080 (coe v0)))
-- Algebra.Structures.IsNearSemiring._.isSemigroup
d_isSemigroup_1108 :: T_IsNearSemiring_1062 -> T_IsSemigroup_436
d_isSemigroup_1108 v0
  = coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1080 (coe v0))
-- Algebra.Structures.IsNearSemiring._.isUnitalMagma
d_isUnitalMagma_1110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsNearSemiring_1062 -> T_IsUnitalMagma_556
d_isUnitalMagma_1110 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isUnitalMagma_1110 v7
du_isUnitalMagma_1110 ::
  T_IsNearSemiring_1062 -> T_IsUnitalMagma_556
du_isUnitalMagma_1110 v0
  = coe du_isUnitalMagma_644 (coe d_'43''45'isMonoid_1080 (coe v0))
-- Algebra.Structures.IsNearSemiring._.isEquivalence
d_isEquivalence_1112 ::
  T_IsNearSemiring_1062 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1112 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1080 (coe v0))))
-- Algebra.Structures.IsNearSemiring._.isPartialEquivalence
d_isPartialEquivalence_1114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsNearSemiring_1062 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1114 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_1114 v7
du_isPartialEquivalence_1114 ::
  T_IsNearSemiring_1062 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1114 v0
  = let v1 = d_'43''45'isMonoid_1080 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    let v3 = d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v3))
-- Algebra.Structures.IsNearSemiring._.refl
d_refl_1116 :: T_IsNearSemiring_1062 -> AgdaAny -> AgdaAny
d_refl_1116 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1080 (coe v0)))))
-- Algebra.Structures.IsNearSemiring._.reflexive
d_reflexive_1118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsNearSemiring_1062 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1118 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_1118 v7
du_reflexive_1118 ::
  T_IsNearSemiring_1062 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1118 v0
  = let v1 = d_'43''45'isMonoid_1080 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    let v3 = d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v3)) v4
-- Algebra.Structures.IsNearSemiring._.setoid
d_setoid_1120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsNearSemiring_1062 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1120 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_setoid_1120 v7
du_setoid_1120 ::
  T_IsNearSemiring_1062 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1120 v0
  = let v1 = d_'43''45'isMonoid_1080 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsNearSemiring._.sym
d_sym_1122 ::
  T_IsNearSemiring_1062 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1122 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1080 (coe v0)))))
-- Algebra.Structures.IsNearSemiring._.trans
d_trans_1124 ::
  T_IsNearSemiring_1062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1124 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1080 (coe v0)))))
-- Algebra.Structures.IsNearSemiring.*-isMagma
d_'42''45'isMagma_1126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsNearSemiring_1062 -> T_IsMagma_140
d_'42''45'isMagma_1126 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagma_1126 v7
du_'42''45'isMagma_1126 :: T_IsNearSemiring_1062 -> T_IsMagma_140
du_'42''45'isMagma_1126 v0
  = coe
      C_IsMagma'46'constructor_769
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1080 (coe v0)))))
      (coe d_'42''45'cong_1082 (coe v0))
-- Algebra.Structures.IsNearSemiring.*-isSemigroup
d_'42''45'isSemigroup_1128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsNearSemiring_1062 -> T_IsSemigroup_436
d_'42''45'isSemigroup_1128 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isSemigroup_1128 v7
du_'42''45'isSemigroup_1128 ::
  T_IsNearSemiring_1062 -> T_IsSemigroup_436
du_'42''45'isSemigroup_1128 v0
  = coe
      C_IsSemigroup'46'constructor_9303
      (coe du_'42''45'isMagma_1126 (coe v0))
      (coe d_'42''45'assoc_1084 (coe v0))
-- Algebra.Structures.IsNearSemiring._.∙-congʳ
d_'8729''45'cong'691'_1132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsNearSemiring_1062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1132 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'691'_1132 v7
du_'8729''45'cong'691'_1132 ::
  T_IsNearSemiring_1062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1132 v0
  = coe
      du_'8729''45'cong'691'_170 (coe du_'42''45'isMagma_1126 (coe v0))
-- Algebra.Structures.IsNearSemiring._.∙-congˡ
d_'8729''45'cong'737'_1134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsNearSemiring_1062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1134 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'737'_1134 v7
du_'8729''45'cong'737'_1134 ::
  T_IsNearSemiring_1062 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1134 v0
  = coe
      du_'8729''45'cong'737'_166 (coe du_'42''45'isMagma_1126 (coe v0))
-- Algebra.Structures.IsSemiringWithoutOne
d_IsSemiringWithoutOne_1142 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsSemiringWithoutOne_1142
  = C_IsSemiringWithoutOne'46'constructor_33063 T_IsCommutativeMonoid_650
                                                (AgdaAny ->
                                                 AgdaAny ->
                                                 AgdaAny ->
                                                 AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                                (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                                MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                                MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsSemiringWithoutOne.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1160 ::
  T_IsSemiringWithoutOne_1142 -> T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1160 v0
  = case coe v0 of
      C_IsSemiringWithoutOne'46'constructor_33063 v1 v2 v3 v4 v5
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiringWithoutOne.*-cong
d_'42''45'cong_1162 ::
  T_IsSemiringWithoutOne_1142 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1162 v0
  = case coe v0 of
      C_IsSemiringWithoutOne'46'constructor_33063 v1 v2 v3 v4 v5
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiringWithoutOne.*-assoc
d_'42''45'assoc_1164 ::
  T_IsSemiringWithoutOne_1142 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1164 v0
  = case coe v0 of
      C_IsSemiringWithoutOne'46'constructor_33063 v1 v2 v3 v4 v5
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiringWithoutOne.distrib
d_distrib_1166 ::
  T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1166 v0
  = case coe v0 of
      C_IsSemiringWithoutOne'46'constructor_33063 v1 v2 v3 v4 v5
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiringWithoutOne.zero
d_zero_1168 ::
  T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1168 v0
  = case coe v0 of
      C_IsSemiringWithoutOne'46'constructor_33063 v1 v2 v3 v4 v5
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiringWithoutOne._.comm
d_comm_1172 ::
  T_IsSemiringWithoutOne_1142 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_1172 v0
  = coe d_comm_662 (coe d_'43''45'isCommutativeMonoid_1160 (coe v0))
-- Algebra.Structures.IsSemiringWithoutOne._.isCommutativeMagma
d_isCommutativeMagma_1174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsSemiringWithoutOne_1142 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_1174 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isCommutativeMagma_1174 v7
du_isCommutativeMagma_1174 ::
  T_IsSemiringWithoutOne_1142 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_1174 v0
  = let v1 = d_'43''45'isCommutativeMonoid_1160 (coe v0) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v1))
-- Algebra.Structures.IsSemiringWithoutOne._.isCommutativeSemigroup
d_isCommutativeSemigroup_1176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsSemiringWithoutOne_1142 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1176 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isCommutativeSemigroup_1176 v7
du_isCommutativeSemigroup_1176 ::
  T_IsSemiringWithoutOne_1142 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1176 v0
  = coe
      du_isCommutativeSemigroup_700
      (coe d_'43''45'isCommutativeMonoid_1160 (coe v0))
-- Algebra.Structures.IsSemiringWithoutOne._.isMonoid
d_isMonoid_1178 :: T_IsSemiringWithoutOne_1142 -> T_IsMonoid_600
d_isMonoid_1178 v0
  = coe
      d_isMonoid_660 (coe d_'43''45'isCommutativeMonoid_1160 (coe v0))
-- Algebra.Structures.IsSemiringWithoutOne._.isEquivalence
d_isEquivalence_1180 ::
  T_IsSemiringWithoutOne_1142 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1180 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660 (coe d_'43''45'isCommutativeMonoid_1160 (coe v0)))))
-- Algebra.Structures.IsSemiringWithoutOne.*-isMagma
d_'42''45'isMagma_1182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsSemiringWithoutOne_1142 -> T_IsMagma_140
d_'42''45'isMagma_1182 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagma_1182 v7
du_'42''45'isMagma_1182 ::
  T_IsSemiringWithoutOne_1142 -> T_IsMagma_140
du_'42''45'isMagma_1182 v0
  = coe
      C_IsMagma'46'constructor_769
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe d_'43''45'isCommutativeMonoid_1160 (coe v0))))))
      (coe d_'42''45'cong_1162 (coe v0))
-- Algebra.Structures.IsSemiringWithoutOne.*-isSemigroup
d_'42''45'isSemigroup_1184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsSemiringWithoutOne_1142 -> T_IsSemigroup_436
d_'42''45'isSemigroup_1184 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isSemigroup_1184 v7
du_'42''45'isSemigroup_1184 ::
  T_IsSemiringWithoutOne_1142 -> T_IsSemigroup_436
du_'42''45'isSemigroup_1184 v0
  = coe
      C_IsSemigroup'46'constructor_9303
      (coe du_'42''45'isMagma_1182 (coe v0))
      (coe d_'42''45'assoc_1164 (coe v0))
-- Algebra.Structures.IsSemiringWithoutOne._.∙-congʳ
d_'8729''45'cong'691'_1188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsSemiringWithoutOne_1142 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1188 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'691'_1188 v7
du_'8729''45'cong'691'_1188 ::
  T_IsSemiringWithoutOne_1142 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1188 v0
  = coe
      du_'8729''45'cong'691'_170 (coe du_'42''45'isMagma_1182 (coe v0))
-- Algebra.Structures.IsSemiringWithoutOne._.∙-congˡ
d_'8729''45'cong'737'_1190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsSemiringWithoutOne_1142 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1190 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'737'_1190 v7
du_'8729''45'cong'737'_1190 ::
  T_IsSemiringWithoutOne_1142 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1190 v0
  = coe
      du_'8729''45'cong'737'_166 (coe du_'42''45'isMagma_1182 (coe v0))
-- Algebra.Structures.IsSemiringWithoutOne.zeroˡ
d_zero'737'_1192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsSemiringWithoutOne_1142 -> AgdaAny -> AgdaAny
d_zero'737'_1192 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_zero'737'_1192 v7
du_zero'737'_1192 ::
  T_IsSemiringWithoutOne_1142 -> AgdaAny -> AgdaAny
du_zero'737'_1192 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe d_zero_1168 (coe v0))
-- Algebra.Structures.IsSemiringWithoutOne.zeroʳ
d_zero'691'_1194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsSemiringWithoutOne_1142 -> AgdaAny -> AgdaAny
d_zero'691'_1194 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_zero'691'_1194 v7
du_zero'691'_1194 ::
  T_IsSemiringWithoutOne_1142 -> AgdaAny -> AgdaAny
du_zero'691'_1194 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe d_zero_1168 (coe v0))
-- Algebra.Structures.IsSemiringWithoutOne.isNearSemiring
d_isNearSemiring_1196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsSemiringWithoutOne_1142 -> T_IsNearSemiring_1062
d_isNearSemiring_1196 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isNearSemiring_1196 v7
du_isNearSemiring_1196 ::
  T_IsSemiringWithoutOne_1142 -> T_IsNearSemiring_1062
du_isNearSemiring_1196 v0
  = coe
      C_IsNearSemiring'46'constructor_30479
      (coe
         d_isMonoid_660 (coe d_'43''45'isCommutativeMonoid_1160 (coe v0)))
      (coe d_'42''45'cong_1162 (coe v0))
      (coe d_'42''45'assoc_1164 (coe v0))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
         (coe d_distrib_1166 (coe v0)))
      (coe du_zero'737'_1192 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne
d_IsCommutativeSemiringWithoutOne_1204 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsCommutativeSemiringWithoutOne_1204
  = C_IsCommutativeSemiringWithoutOne'46'constructor_36241 T_IsSemiringWithoutOne_1142
                                                           (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsCommutativeSemiringWithoutOne.isSemiringWithoutOne
d_isSemiringWithoutOne_1216 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_1216 v0
  = case coe v0 of
      C_IsCommutativeSemiringWithoutOne'46'constructor_36241 v1 v2
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeSemiringWithoutOne.*-comm
d_'42''45'comm_1218 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_1218 v0
  = case coe v0 of
      C_IsCommutativeSemiringWithoutOne'46'constructor_36241 v1 v2
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.*-assoc
d_'42''45'assoc_1222 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1222 v0
  = coe
      d_'42''45'assoc_1164 (coe d_isSemiringWithoutOne_1216 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.*-cong
d_'42''45'cong_1224 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1224 v0
  = coe
      d_'42''45'cong_1162 (coe d_isSemiringWithoutOne_1216 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.∙-congʳ
d_'8729''45'cong'691'_1226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiringWithoutOne_1204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1226 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'691'_1226 v7
du_'8729''45'cong'691'_1226 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1226 v0
  = let v1 = d_isSemiringWithoutOne_1216 (coe v0) in
    coe
      du_'8729''45'cong'691'_170 (coe du_'42''45'isMagma_1182 (coe v1))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.∙-congˡ
d_'8729''45'cong'737'_1228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiringWithoutOne_1204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1228 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'737'_1228 v7
du_'8729''45'cong'737'_1228 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1228 v0
  = let v1 = d_isSemiringWithoutOne_1216 (coe v0) in
    coe
      du_'8729''45'cong'737'_166 (coe du_'42''45'isMagma_1182 (coe v1))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.*-isMagma
d_'42''45'isMagma_1230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsCommutativeSemiringWithoutOne_1204 -> T_IsMagma_140
d_'42''45'isMagma_1230 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isMagma_1230 v7
du_'42''45'isMagma_1230 ::
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsMagma_140
du_'42''45'isMagma_1230 v0
  = coe
      du_'42''45'isMagma_1182 (coe d_isSemiringWithoutOne_1216 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.*-isSemigroup
d_'42''45'isSemigroup_1232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsSemigroup_436
d_'42''45'isSemigroup_1232 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'42''45'isSemigroup_1232 v7
du_'42''45'isSemigroup_1232 ::
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsSemigroup_436
du_'42''45'isSemigroup_1232 v0
  = coe
      du_'42''45'isSemigroup_1184
      (coe d_isSemiringWithoutOne_1216 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.comm
d_comm_1234 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_1234 v0
  = coe
      d_comm_662
      (coe
         d_'43''45'isCommutativeMonoid_1160
         (coe d_isSemiringWithoutOne_1216 (coe v0)))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.isCommutativeMagma
d_isCommutativeMagma_1236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_1236 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isCommutativeMagma_1236 v7
du_isCommutativeMagma_1236 ::
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_1236 v0
  = let v1 = d_isSemiringWithoutOne_1216 (coe v0) in
    let v2 = d_'43''45'isCommutativeMonoid_1160 (coe v1) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v2))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1238 ::
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1238 v0
  = coe
      d_'43''45'isCommutativeMonoid_1160
      (coe d_isSemiringWithoutOne_1216 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.isCommutativeSemigroup
d_isCommutativeSemigroup_1240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiringWithoutOne_1204 ->
  T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1240 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isCommutativeSemigroup_1240 v7
du_isCommutativeSemigroup_1240 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1240 v0
  = let v1 = d_isSemiringWithoutOne_1216 (coe v0) in
    coe
      du_isCommutativeSemigroup_700
      (coe d_'43''45'isCommutativeMonoid_1160 (coe v1))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.isMonoid
d_isMonoid_1242 ::
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsMonoid_600
d_isMonoid_1242 v0
  = coe
      d_isMonoid_660
      (coe
         d_'43''45'isCommutativeMonoid_1160
         (coe d_isSemiringWithoutOne_1216 (coe v0)))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.distrib
d_distrib_1244 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1244 v0
  = coe d_distrib_1166 (coe d_isSemiringWithoutOne_1216 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.isEquivalence
d_isEquivalence_1246 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1246 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1160
                  (coe d_isSemiringWithoutOne_1216 (coe v0))))))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.isNearSemiring
d_isNearSemiring_1248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsNearSemiring_1062
d_isNearSemiring_1248 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isNearSemiring_1248 v7
du_isNearSemiring_1248 ::
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsNearSemiring_1062
du_isNearSemiring_1248 v0
  = coe
      du_isNearSemiring_1196 (coe d_isSemiringWithoutOne_1216 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.zero
d_zero_1250 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1250 v0
  = coe d_zero_1168 (coe d_isSemiringWithoutOne_1216 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.zeroʳ
d_zero'691'_1252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiringWithoutOne_1204 -> AgdaAny -> AgdaAny
d_zero'691'_1252 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_zero'691'_1252 v7
du_zero'691'_1252 ::
  T_IsCommutativeSemiringWithoutOne_1204 -> AgdaAny -> AgdaAny
du_zero'691'_1252 v0
  = coe du_zero'691'_1194 (coe d_isSemiringWithoutOne_1216 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.zeroˡ
d_zero'737'_1254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiringWithoutOne_1204 -> AgdaAny -> AgdaAny
d_zero'737'_1254 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_zero'737'_1254 v7
du_zero'737'_1254 ::
  T_IsCommutativeSemiringWithoutOne_1204 -> AgdaAny -> AgdaAny
du_zero'737'_1254 v0
  = coe du_zero'737'_1192 (coe d_isSemiringWithoutOne_1216 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_1256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiringWithoutOne_1204 ->
  T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_1256 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                      v7
  = du_'42''45'isCommutativeSemigroup_1256 v7
du_'42''45'isCommutativeSemigroup_1256 ::
  T_IsCommutativeSemiringWithoutOne_1204 ->
  T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_1256 v0
  = coe
      C_IsCommutativeSemigroup'46'constructor_10975
      (coe
         du_'42''45'isSemigroup_1184
         (coe d_isSemiringWithoutOne_1216 (coe v0)))
      (coe d_'42''45'comm_1218 (coe v0))
-- Algebra.Structures.IsCommutativeSemiringWithoutOne._.isCommutativeMagma
d_isCommutativeMagma_1260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_1260 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isCommutativeMagma_1260 v7
du_isCommutativeMagma_1260 ::
  T_IsCommutativeSemiringWithoutOne_1204 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_1260 v0
  = coe
      du_isCommutativeMagma_550
      (coe du_'42''45'isCommutativeSemigroup_1256 (coe v0))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero
d_IsSemiringWithoutAnnihilatingZero_1270 a0 a1 a2 a3 a4 a5 a6 a7
  = ()
data T_IsSemiringWithoutAnnihilatingZero_1270
  = C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063 T_IsCommutativeMonoid_650
                                                             (AgdaAny ->
                                                              AgdaAny ->
                                                              AgdaAny ->
                                                              AgdaAny ->
                                                              AgdaAny -> AgdaAny -> AgdaAny)
                                                             (AgdaAny ->
                                                              AgdaAny -> AgdaAny -> AgdaAny)
                                                             MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                                             MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1290 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1290 v0
  = case coe v0 of
      C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063 v1 v2 v3 v4 v5
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero.*-cong
d_'42''45'cong_1292 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1292 v0
  = case coe v0 of
      C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063 v1 v2 v3 v4 v5
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero.*-assoc
d_'42''45'assoc_1294 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1294 v0
  = case coe v0 of
      C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063 v1 v2 v3 v4 v5
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero.*-identity
d_'42''45'identity_1296 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1296 v0
  = case coe v0 of
      C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063 v1 v2 v3 v4 v5
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero.distrib
d_distrib_1298 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1298 v0
  = case coe v0 of
      C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063 v1 v2 v3 v4 v5
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero.distribˡ
d_distrib'737'_1300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_1300 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'737'_1300 v8
du_distrib'737'_1300 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_1300 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_distrib_1298 (coe v0))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero.distribʳ
d_distrib'691'_1302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1302 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'691'_1302 v8
du_distrib'691'_1302 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_1302 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_distrib_1298 (coe v0))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.assoc
d_assoc_1306 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1306 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660 (coe d_'43''45'isCommutativeMonoid_1290 (coe v0))))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.comm
d_comm_1308 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_1308 v0
  = coe d_comm_662 (coe d_'43''45'isCommutativeMonoid_1290 (coe v0))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.∙-cong
d_'8729''45'cong_1310 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1310 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660 (coe d_'43''45'isCommutativeMonoid_1290 (coe v0)))))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.∙-congʳ
d_'8729''45'cong'691'_1312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1312 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_1312 v8
du_'8729''45'cong'691'_1312 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1312 v0
  = let v1 = d_'43''45'isCommutativeMonoid_1290 (coe v0) in
    let v2 = d_isMonoid_660 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.∙-congˡ
d_'8729''45'cong'737'_1314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1314 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_1314 v8
du_'8729''45'cong'737'_1314 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1314 v0
  = let v1 = d_'43''45'isCommutativeMonoid_1290 (coe v0) in
    let v2 = d_isMonoid_660 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.identity
d_identity_1316 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1316 v0
  = coe
      d_identity_612
      (coe
         d_isMonoid_660 (coe d_'43''45'isCommutativeMonoid_1290 (coe v0)))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.identityʳ
d_identity'691'_1318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 -> AgdaAny -> AgdaAny
d_identity'691'_1318 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_1318 v8
du_identity'691'_1318 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> AgdaAny -> AgdaAny
du_identity'691'_1318 v0
  = let v1 = d_'43''45'isCommutativeMonoid_1290 (coe v0) in
    coe du_identity'691'_642 (coe d_isMonoid_660 (coe v1))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.identityˡ
d_identity'737'_1320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 -> AgdaAny -> AgdaAny
d_identity'737'_1320 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_1320 v8
du_identity'737'_1320 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> AgdaAny -> AgdaAny
du_identity'737'_1320 v0
  = let v1 = d_'43''45'isCommutativeMonoid_1290 (coe v0) in
    coe du_identity'737'_640 (coe d_isMonoid_660 (coe v1))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.isCommutativeMagma
d_isCommutativeMagma_1322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  T_IsCommutativeMagma_176
d_isCommutativeMagma_1322 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeMagma_1322 v8
du_isCommutativeMagma_1322 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  T_IsCommutativeMagma_176
du_isCommutativeMagma_1322 v0
  = let v1 = d_'43''45'isCommutativeMonoid_1290 (coe v0) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v1))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.isCommutativeSemigroup
d_isCommutativeSemigroup_1324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1324 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeSemigroup_1324 v8
du_isCommutativeSemigroup_1324 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1324 v0
  = coe
      du_isCommutativeSemigroup_700
      (coe d_'43''45'isCommutativeMonoid_1290 (coe v0))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.isMagma
d_isMagma_1326 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsMagma_140
d_isMagma_1326 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660 (coe d_'43''45'isCommutativeMonoid_1290 (coe v0))))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.isMonoid
d_isMonoid_1328 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsMonoid_600
d_isMonoid_1328 v0
  = coe
      d_isMonoid_660 (coe d_'43''45'isCommutativeMonoid_1290 (coe v0))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.isSemigroup
d_isSemigroup_1330 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsSemigroup_436
d_isSemigroup_1330 v0
  = coe
      d_isSemigroup_610
      (coe
         d_isMonoid_660 (coe d_'43''45'isCommutativeMonoid_1290 (coe v0)))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.isUnitalMagma
d_isUnitalMagma_1332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsUnitalMagma_556
d_isUnitalMagma_1332 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isUnitalMagma_1332 v8
du_isUnitalMagma_1332 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsUnitalMagma_556
du_isUnitalMagma_1332 v0
  = let v1 = d_'43''45'isCommutativeMonoid_1290 (coe v0) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_660 (coe v1))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.isEquivalence
d_isEquivalence_1334 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1334 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660 (coe d_'43''45'isCommutativeMonoid_1290 (coe v0)))))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.isPartialEquivalence
d_isPartialEquivalence_1336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1336 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_1336 v8
du_isPartialEquivalence_1336 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1336 v0
  = let v1 = d_'43''45'isCommutativeMonoid_1290 (coe v0) in
    let v2 = d_isMonoid_660 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    let v4 = d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v4))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.refl
d_refl_1338 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> AgdaAny -> AgdaAny
d_refl_1338 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe d_'43''45'isCommutativeMonoid_1290 (coe v0))))))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.reflexive
d_reflexive_1340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1340 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_1340 v8
du_reflexive_1340 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1340 v0
  = let v1 = d_'43''45'isCommutativeMonoid_1290 (coe v0) in
    let v2 = d_isMonoid_660 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    let v4 = d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v4)) v5
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.setoid
d_setoid_1342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1342 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_1342 v8
du_setoid_1342 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1342 v0
  = let v1 = d_'43''45'isCommutativeMonoid_1290 (coe v0) in
    let v2 = d_isMonoid_660 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.sym
d_sym_1344 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1344 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe d_'43''45'isCommutativeMonoid_1290 (coe v0))))))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.trans
d_trans_1346 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1346 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe d_'43''45'isCommutativeMonoid_1290 (coe v0))))))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero.*-isMagma
d_'42''45'isMagma_1348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsMagma_140
d_'42''45'isMagma_1348 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMagma_1348 v8
du_'42''45'isMagma_1348 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsMagma_140
du_'42''45'isMagma_1348 v0
  = coe
      C_IsMagma'46'constructor_769
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe d_'43''45'isCommutativeMonoid_1290 (coe v0))))))
      (coe d_'42''45'cong_1292 (coe v0))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero.*-isSemigroup
d_'42''45'isSemigroup_1350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsSemigroup_436
d_'42''45'isSemigroup_1350 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isSemigroup_1350 v8
du_'42''45'isSemigroup_1350 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsSemigroup_436
du_'42''45'isSemigroup_1350 v0
  = coe
      C_IsSemigroup'46'constructor_9303
      (coe du_'42''45'isMagma_1348 (coe v0))
      (coe d_'42''45'assoc_1294 (coe v0))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero.*-isMonoid
d_'42''45'isMonoid_1352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsMonoid_600
d_'42''45'isMonoid_1352 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMonoid_1352 v8
du_'42''45'isMonoid_1352 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> T_IsMonoid_600
du_'42''45'isMonoid_1352 v0
  = coe
      C_IsMonoid'46'constructor_13559
      (coe du_'42''45'isSemigroup_1350 (coe v0))
      (coe d_'42''45'identity_1296 (coe v0))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.∙-congʳ
d_'8729''45'cong'691'_1356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1356 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_1356 v8
du_'8729''45'cong'691'_1356 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1356 v0
  = let v1 = coe du_'42''45'isMonoid_1352 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.∙-congˡ
d_'8729''45'cong'737'_1358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1358 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_1358 v8
du_'8729''45'cong'737'_1358 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1358 v0
  = let v1 = coe du_'42''45'isMonoid_1352 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.identityʳ
d_identity'691'_1360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 -> AgdaAny -> AgdaAny
d_identity'691'_1360 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_1360 v8
du_identity'691'_1360 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> AgdaAny -> AgdaAny
du_identity'691'_1360 v0
  = coe du_identity'691'_642 (coe du_'42''45'isMonoid_1352 (coe v0))
-- Algebra.Structures.IsSemiringWithoutAnnihilatingZero._.identityˡ
d_identity'737'_1362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiringWithoutAnnihilatingZero_1270 -> AgdaAny -> AgdaAny
d_identity'737'_1362 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_1362 v8
du_identity'737'_1362 ::
  T_IsSemiringWithoutAnnihilatingZero_1270 -> AgdaAny -> AgdaAny
du_identity'737'_1362 v0
  = coe du_identity'737'_640 (coe du_'42''45'isMonoid_1352 (coe v0))
-- Algebra.Structures.IsSemiring
d_IsSemiring_1372 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsSemiring_1372
  = C_IsSemiring'46'constructor_42303 T_IsSemiringWithoutAnnihilatingZero_1270
                                      MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsSemiring.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1386 ::
  T_IsSemiring_1372 -> T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1386 v0
  = case coe v0 of
      C_IsSemiring'46'constructor_42303 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiring.zero
d_zero_1388 ::
  T_IsSemiring_1372 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1388 v0
  = case coe v0 of
      C_IsSemiring'46'constructor_42303 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsSemiring._.*-assoc
d_'42''45'assoc_1392 ::
  T_IsSemiring_1372 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1392 v0
  = coe
      d_'42''45'assoc_1294
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))
-- Algebra.Structures.IsSemiring._.*-cong
d_'42''45'cong_1394 ::
  T_IsSemiring_1372 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1394 v0
  = coe
      d_'42''45'cong_1292
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))
-- Algebra.Structures.IsSemiring._.∙-congʳ
d_'8729''45'cong'691'_1396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiring_1372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1396 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_1396 v8
du_'8729''45'cong'691'_1396 ::
  T_IsSemiring_1372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1396 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = coe du_'42''45'isMonoid_1352 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsSemiring._.∙-congˡ
d_'8729''45'cong'737'_1398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiring_1372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1398 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_1398 v8
du_'8729''45'cong'737'_1398 ::
  T_IsSemiring_1372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1398 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = coe du_'42''45'isMonoid_1352 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsSemiring._.*-identity
d_'42''45'identity_1400 ::
  T_IsSemiring_1372 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1400 v0
  = coe
      d_'42''45'identity_1296
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))
-- Algebra.Structures.IsSemiring._.identityʳ
d_identity'691'_1402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> AgdaAny -> AgdaAny
d_identity'691'_1402 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_1402 v8
du_identity'691'_1402 :: T_IsSemiring_1372 -> AgdaAny -> AgdaAny
du_identity'691'_1402 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    coe du_identity'691'_642 (coe du_'42''45'isMonoid_1352 (coe v1))
-- Algebra.Structures.IsSemiring._.identityˡ
d_identity'737'_1404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> AgdaAny -> AgdaAny
d_identity'737'_1404 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_1404 v8
du_identity'737'_1404 :: T_IsSemiring_1372 -> AgdaAny -> AgdaAny
du_identity'737'_1404 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    coe du_identity'737'_640 (coe du_'42''45'isMonoid_1352 (coe v1))
-- Algebra.Structures.IsSemiring._.*-isMagma
d_'42''45'isMagma_1406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> T_IsMagma_140
d_'42''45'isMagma_1406 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMagma_1406 v8
du_'42''45'isMagma_1406 :: T_IsSemiring_1372 -> T_IsMagma_140
du_'42''45'isMagma_1406 v0
  = coe
      du_'42''45'isMagma_1348
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))
-- Algebra.Structures.IsSemiring._.*-isMonoid
d_'42''45'isMonoid_1408 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> T_IsMonoid_600
d_'42''45'isMonoid_1408 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMonoid_1408 v8
du_'42''45'isMonoid_1408 :: T_IsSemiring_1372 -> T_IsMonoid_600
du_'42''45'isMonoid_1408 v0
  = coe
      du_'42''45'isMonoid_1352
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))
-- Algebra.Structures.IsSemiring._.*-isSemigroup
d_'42''45'isSemigroup_1410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> T_IsSemigroup_436
d_'42''45'isSemigroup_1410 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isSemigroup_1410 v8
du_'42''45'isSemigroup_1410 ::
  T_IsSemiring_1372 -> T_IsSemigroup_436
du_'42''45'isSemigroup_1410 v0
  = coe
      du_'42''45'isSemigroup_1350
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))
-- Algebra.Structures.IsSemiring._.assoc
d_assoc_1412 ::
  T_IsSemiring_1372 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1412 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660
            (coe
               d_'43''45'isCommutativeMonoid_1290
               (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))))
-- Algebra.Structures.IsSemiring._.comm
d_comm_1414 :: T_IsSemiring_1372 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_1414 v0
  = coe
      d_comm_662
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))
-- Algebra.Structures.IsSemiring._.∙-cong
d_'8729''45'cong_1416 ::
  T_IsSemiring_1372 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1416 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1290
                  (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))))))
-- Algebra.Structures.IsSemiring._.∙-congʳ
d_'8729''45'cong'691'_1418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiring_1372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1418 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_1418 v8
du_'8729''45'cong'691'_1418 ::
  T_IsSemiring_1372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1418 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = d_'43''45'isCommutativeMonoid_1290 (coe v1) in
    let v3 = d_isMonoid_660 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsSemiring._.∙-congˡ
d_'8729''45'cong'737'_1420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiring_1372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1420 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_1420 v8
du_'8729''45'cong'737'_1420 ::
  T_IsSemiring_1372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1420 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = d_'43''45'isCommutativeMonoid_1290 (coe v1) in
    let v3 = d_isMonoid_660 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsSemiring._.identity
d_identity_1422 ::
  T_IsSemiring_1372 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1422 v0
  = coe
      d_identity_612
      (coe
         d_isMonoid_660
         (coe
            d_'43''45'isCommutativeMonoid_1290
            (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))))
-- Algebra.Structures.IsSemiring._.identityʳ
d_identity'691'_1424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> AgdaAny -> AgdaAny
d_identity'691'_1424 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_1424 v8
du_identity'691'_1424 :: T_IsSemiring_1372 -> AgdaAny -> AgdaAny
du_identity'691'_1424 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = d_'43''45'isCommutativeMonoid_1290 (coe v1) in
    coe du_identity'691'_642 (coe d_isMonoid_660 (coe v2))
-- Algebra.Structures.IsSemiring._.identityˡ
d_identity'737'_1426 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> AgdaAny -> AgdaAny
d_identity'737'_1426 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_1426 v8
du_identity'737'_1426 :: T_IsSemiring_1372 -> AgdaAny -> AgdaAny
du_identity'737'_1426 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = d_'43''45'isCommutativeMonoid_1290 (coe v1) in
    coe du_identity'737'_640 (coe d_isMonoid_660 (coe v2))
-- Algebra.Structures.IsSemiring._.isCommutativeMagma
d_isCommutativeMagma_1428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_1428 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeMagma_1428 v8
du_isCommutativeMagma_1428 ::
  T_IsSemiring_1372 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_1428 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = d_'43''45'isCommutativeMonoid_1290 (coe v1) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v2))
-- Algebra.Structures.IsSemiring._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1430 ::
  T_IsSemiring_1372 -> T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1430 v0
  = coe
      d_'43''45'isCommutativeMonoid_1290
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))
-- Algebra.Structures.IsSemiring._.isCommutativeSemigroup
d_isCommutativeSemigroup_1432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsSemiring_1372 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1432 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeSemigroup_1432 v8
du_isCommutativeSemigroup_1432 ::
  T_IsSemiring_1372 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1432 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    coe
      du_isCommutativeSemigroup_700
      (coe d_'43''45'isCommutativeMonoid_1290 (coe v1))
-- Algebra.Structures.IsSemiring._.isMagma
d_isMagma_1434 :: T_IsSemiring_1372 -> T_IsMagma_140
d_isMagma_1434 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660
            (coe
               d_'43''45'isCommutativeMonoid_1290
               (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))))
-- Algebra.Structures.IsSemiring._.isMonoid
d_isMonoid_1436 :: T_IsSemiring_1372 -> T_IsMonoid_600
d_isMonoid_1436 v0
  = coe
      d_isMonoid_660
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))
-- Algebra.Structures.IsSemiring._.isSemigroup
d_isSemigroup_1438 :: T_IsSemiring_1372 -> T_IsSemigroup_436
d_isSemigroup_1438 v0
  = coe
      d_isSemigroup_610
      (coe
         d_isMonoid_660
         (coe
            d_'43''45'isCommutativeMonoid_1290
            (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))))
-- Algebra.Structures.IsSemiring._.isUnitalMagma
d_isUnitalMagma_1440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> T_IsUnitalMagma_556
d_isUnitalMagma_1440 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isUnitalMagma_1440 v8
du_isUnitalMagma_1440 :: T_IsSemiring_1372 -> T_IsUnitalMagma_556
du_isUnitalMagma_1440 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = d_'43''45'isCommutativeMonoid_1290 (coe v1) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_660 (coe v2))
-- Algebra.Structures.IsSemiring._.distrib
d_distrib_1442 ::
  T_IsSemiring_1372 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1442 v0
  = coe
      d_distrib_1298
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))
-- Algebra.Structures.IsSemiring._.distribʳ
d_distrib'691'_1444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiring_1372 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1444 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'691'_1444 v8
du_distrib'691'_1444 ::
  T_IsSemiring_1372 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_1444 v0
  = coe
      du_distrib'691'_1302
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))
-- Algebra.Structures.IsSemiring._.distribˡ
d_distrib'737'_1446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiring_1372 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_1446 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'737'_1446 v8
du_distrib'737'_1446 ::
  T_IsSemiring_1372 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_1446 v0
  = coe
      du_distrib'737'_1300
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))
-- Algebra.Structures.IsSemiring._.isEquivalence
d_isEquivalence_1448 ::
  T_IsSemiring_1372 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1448 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1290
                  (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0))))))
-- Algebra.Structures.IsSemiring._.isPartialEquivalence
d_isPartialEquivalence_1450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiring_1372 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1450 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_1450 v8
du_isPartialEquivalence_1450 ::
  T_IsSemiring_1372 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1450 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = d_'43''45'isCommutativeMonoid_1290 (coe v1) in
    let v3 = d_isMonoid_660 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    let v5 = d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v5))
-- Algebra.Structures.IsSemiring._.refl
d_refl_1452 :: T_IsSemiring_1372 -> AgdaAny -> AgdaAny
d_refl_1452 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))))))
-- Algebra.Structures.IsSemiring._.reflexive
d_reflexive_1454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiring_1372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1454 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_1454 v8
du_reflexive_1454 ::
  T_IsSemiring_1372 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1454 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = d_'43''45'isCommutativeMonoid_1290 (coe v1) in
    let v3 = d_isMonoid_660 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    let v5 = d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v5)) v6
-- Algebra.Structures.IsSemiring._.setoid
d_setoid_1456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsSemiring_1372 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1456 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_1456 v8
du_setoid_1456 ::
  T_IsSemiring_1372 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1456 v0
  = let v1 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v0) in
    let v2 = d_'43''45'isCommutativeMonoid_1290 (coe v1) in
    let v3 = d_isMonoid_660 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsSemiring._.sym
d_sym_1458 ::
  T_IsSemiring_1372 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1458 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))))))
-- Algebra.Structures.IsSemiring._.trans
d_trans_1460 ::
  T_IsSemiring_1372 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1460 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))))))
-- Algebra.Structures.IsSemiring.isSemiringWithoutOne
d_isSemiringWithoutOne_1462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsSemiring_1372 -> T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_1462 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isSemiringWithoutOne_1462 v8
du_isSemiringWithoutOne_1462 ::
  T_IsSemiring_1372 -> T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_1462 v0
  = coe
      C_IsSemiringWithoutOne'46'constructor_33063
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))
      (coe
         d_'42''45'cong_1292
         (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))
      (coe
         d_'42''45'assoc_1294
         (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))
      (coe
         d_distrib_1298
         (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v0)))
      (coe d_zero_1388 (coe v0))
-- Algebra.Structures.IsSemiring._.isNearSemiring
d_isNearSemiring_1466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> T_IsNearSemiring_1062
d_isNearSemiring_1466 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isNearSemiring_1466 v8
du_isNearSemiring_1466 ::
  T_IsSemiring_1372 -> T_IsNearSemiring_1062
du_isNearSemiring_1466 v0
  = coe
      du_isNearSemiring_1196 (coe du_isSemiringWithoutOne_1462 (coe v0))
-- Algebra.Structures.IsSemiring._.zeroʳ
d_zero'691'_1468 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> AgdaAny -> AgdaAny
d_zero'691'_1468 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_zero'691'_1468 v8
du_zero'691'_1468 :: T_IsSemiring_1372 -> AgdaAny -> AgdaAny
du_zero'691'_1468 v0
  = coe du_zero'691'_1194 (coe du_isSemiringWithoutOne_1462 (coe v0))
-- Algebra.Structures.IsSemiring._.zeroˡ
d_zero'737'_1470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsSemiring_1372 -> AgdaAny -> AgdaAny
d_zero'737'_1470 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_zero'737'_1470 v8
du_zero'737'_1470 :: T_IsSemiring_1372 -> AgdaAny -> AgdaAny
du_zero'737'_1470 v0
  = coe du_zero'737'_1192 (coe du_isSemiringWithoutOne_1462 (coe v0))
-- Algebra.Structures.IsCommutativeSemiring
d_IsCommutativeSemiring_1480 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsCommutativeSemiring_1480
  = C_IsCommutativeSemiring'46'constructor_46125 T_IsSemiring_1372
                                                 (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsCommutativeSemiring.isSemiring
d_isSemiring_1494 ::
  T_IsCommutativeSemiring_1480 -> T_IsSemiring_1372
d_isSemiring_1494 v0
  = case coe v0 of
      C_IsCommutativeSemiring'46'constructor_46125 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeSemiring.*-comm
d_'42''45'comm_1496 ::
  T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_1496 v0
  = case coe v0 of
      C_IsCommutativeSemiring'46'constructor_46125 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeSemiring._.*-assoc
d_'42''45'assoc_1500 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1500 v0
  = coe
      d_'42''45'assoc_1294
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1494 (coe v0)))
-- Algebra.Structures.IsCommutativeSemiring._.*-cong
d_'42''45'cong_1502 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1502 v0
  = coe
      d_'42''45'cong_1292
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1494 (coe v0)))
-- Algebra.Structures.IsCommutativeSemiring._.∙-congʳ
d_'8729''45'cong'691'_1504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1504 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_1504 v8
du_'8729''45'cong'691'_1504 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1504 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = coe du_'42''45'isMonoid_1352 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsCommutativeSemiring._.∙-congˡ
d_'8729''45'cong'737'_1506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1506 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_1506 v8
du_'8729''45'cong'737'_1506 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1506 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = coe du_'42''45'isMonoid_1352 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsCommutativeSemiring._.*-identity
d_'42''45'identity_1508 ::
  T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1508 v0
  = coe
      d_'42''45'identity_1296
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1494 (coe v0)))
-- Algebra.Structures.IsCommutativeSemiring._.identityʳ
d_identity'691'_1510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
d_identity'691'_1510 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_1510 v8
du_identity'691'_1510 ::
  T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
du_identity'691'_1510 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    coe du_identity'691'_642 (coe du_'42''45'isMonoid_1352 (coe v2))
-- Algebra.Structures.IsCommutativeSemiring._.identityˡ
d_identity'737'_1512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
d_identity'737'_1512 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_1512 v8
du_identity'737'_1512 ::
  T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
du_identity'737'_1512 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    coe du_identity'737'_640 (coe du_'42''45'isMonoid_1352 (coe v2))
-- Algebra.Structures.IsCommutativeSemiring._.*-isMagma
d_'42''45'isMagma_1514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsCommutativeSemiring_1480 -> T_IsMagma_140
d_'42''45'isMagma_1514 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMagma_1514 v8
du_'42''45'isMagma_1514 ::
  T_IsCommutativeSemiring_1480 -> T_IsMagma_140
du_'42''45'isMagma_1514 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    coe
      du_'42''45'isMagma_1348
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsCommutativeSemiring._.*-isMonoid
d_'42''45'isMonoid_1516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> T_IsMonoid_600
d_'42''45'isMonoid_1516 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMonoid_1516 v8
du_'42''45'isMonoid_1516 ::
  T_IsCommutativeSemiring_1480 -> T_IsMonoid_600
du_'42''45'isMonoid_1516 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    coe
      du_'42''45'isMonoid_1352
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsCommutativeSemiring._.*-isSemigroup
d_'42''45'isSemigroup_1518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> T_IsSemigroup_436
d_'42''45'isSemigroup_1518 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isSemigroup_1518 v8
du_'42''45'isSemigroup_1518 ::
  T_IsCommutativeSemiring_1480 -> T_IsSemigroup_436
du_'42''45'isSemigroup_1518 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    coe
      du_'42''45'isSemigroup_1350
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsCommutativeSemiring._.assoc
d_assoc_1520 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1520 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660
            (coe
               d_'43''45'isCommutativeMonoid_1290
               (coe
                  d_isSemiringWithoutAnnihilatingZero_1386
                  (coe d_isSemiring_1494 (coe v0))))))
-- Algebra.Structures.IsCommutativeSemiring._.comm
d_comm_1522 ::
  T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_1522 v0
  = coe
      d_comm_662
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe
            d_isSemiringWithoutAnnihilatingZero_1386
            (coe d_isSemiring_1494 (coe v0))))
-- Algebra.Structures.IsCommutativeSemiring._.∙-cong
d_'8729''45'cong_1524 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1524 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1290
                  (coe
                     d_isSemiringWithoutAnnihilatingZero_1386
                     (coe d_isSemiring_1494 (coe v0)))))))
-- Algebra.Structures.IsCommutativeSemiring._.∙-congʳ
d_'8729''45'cong'691'_1526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1526 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_1526 v8
du_'8729''45'cong'691'_1526 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1526 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    let v4 = d_isMonoid_660 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsCommutativeSemiring._.∙-congˡ
d_'8729''45'cong'737'_1528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1528 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_1528 v8
du_'8729''45'cong'737'_1528 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1528 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    let v4 = d_isMonoid_660 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsCommutativeSemiring._.identity
d_identity_1530 ::
  T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1530 v0
  = coe
      d_identity_612
      (coe
         d_isMonoid_660
         (coe
            d_'43''45'isCommutativeMonoid_1290
            (coe
               d_isSemiringWithoutAnnihilatingZero_1386
               (coe d_isSemiring_1494 (coe v0)))))
-- Algebra.Structures.IsCommutativeSemiring._.identityʳ
d_identity'691'_1532 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
d_identity'691'_1532 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_1532 v8
du_identity'691'_1532 ::
  T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
du_identity'691'_1532 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    coe du_identity'691'_642 (coe d_isMonoid_660 (coe v3))
-- Algebra.Structures.IsCommutativeSemiring._.identityˡ
d_identity'737'_1534 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
d_identity'737'_1534 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_1534 v8
du_identity'737'_1534 ::
  T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
du_identity'737'_1534 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    coe du_identity'737'_640 (coe d_isMonoid_660 (coe v3))
-- Algebra.Structures.IsCommutativeSemiring._.isCommutativeMagma
d_isCommutativeMagma_1536 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_1536 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeMagma_1536 v8
du_isCommutativeMagma_1536 ::
  T_IsCommutativeSemiring_1480 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_1536 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v3))
-- Algebra.Structures.IsCommutativeSemiring._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1538 ::
  T_IsCommutativeSemiring_1480 -> T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1538 v0
  = coe
      d_'43''45'isCommutativeMonoid_1290
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1494 (coe v0)))
-- Algebra.Structures.IsCommutativeSemiring._.isCommutativeSemigroup
d_isCommutativeSemigroup_1540 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1540 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeSemigroup_1540 v8
du_isCommutativeSemigroup_1540 ::
  T_IsCommutativeSemiring_1480 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1540 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    coe
      du_isCommutativeSemigroup_700
      (coe d_'43''45'isCommutativeMonoid_1290 (coe v2))
-- Algebra.Structures.IsCommutativeSemiring._.isMagma
d_isMagma_1542 :: T_IsCommutativeSemiring_1480 -> T_IsMagma_140
d_isMagma_1542 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660
            (coe
               d_'43''45'isCommutativeMonoid_1290
               (coe
                  d_isSemiringWithoutAnnihilatingZero_1386
                  (coe d_isSemiring_1494 (coe v0))))))
-- Algebra.Structures.IsCommutativeSemiring._.isMonoid
d_isMonoid_1544 :: T_IsCommutativeSemiring_1480 -> T_IsMonoid_600
d_isMonoid_1544 v0
  = coe
      d_isMonoid_660
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe
            d_isSemiringWithoutAnnihilatingZero_1386
            (coe d_isSemiring_1494 (coe v0))))
-- Algebra.Structures.IsCommutativeSemiring._.isSemigroup
d_isSemigroup_1546 ::
  T_IsCommutativeSemiring_1480 -> T_IsSemigroup_436
d_isSemigroup_1546 v0
  = coe
      d_isSemigroup_610
      (coe
         d_isMonoid_660
         (coe
            d_'43''45'isCommutativeMonoid_1290
            (coe
               d_isSemiringWithoutAnnihilatingZero_1386
               (coe d_isSemiring_1494 (coe v0)))))
-- Algebra.Structures.IsCommutativeSemiring._.isUnitalMagma
d_isUnitalMagma_1548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> T_IsUnitalMagma_556
d_isUnitalMagma_1548 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isUnitalMagma_1548 v8
du_isUnitalMagma_1548 ::
  T_IsCommutativeSemiring_1480 -> T_IsUnitalMagma_556
du_isUnitalMagma_1548 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_660 (coe v3))
-- Algebra.Structures.IsCommutativeSemiring._.distrib
d_distrib_1550 ::
  T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1550 v0
  = coe
      d_distrib_1298
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1494 (coe v0)))
-- Algebra.Structures.IsCommutativeSemiring._.distribʳ
d_distrib'691'_1552 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1552 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'691'_1552 v8
du_distrib'691'_1552 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_1552 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    coe
      du_distrib'691'_1302
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsCommutativeSemiring._.distribˡ
d_distrib'737'_1554 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_1554 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'737'_1554 v8
du_distrib'737'_1554 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_1554 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    coe
      du_distrib'737'_1300
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsCommutativeSemiring._.isEquivalence
d_isEquivalence_1556 ::
  T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1556 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1290
                  (coe
                     d_isSemiringWithoutAnnihilatingZero_1386
                     (coe d_isSemiring_1494 (coe v0)))))))
-- Algebra.Structures.IsCommutativeSemiring._.isNearSemiring
d_isNearSemiring_1558 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> T_IsNearSemiring_1062
d_isNearSemiring_1558 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isNearSemiring_1558 v8
du_isNearSemiring_1558 ::
  T_IsCommutativeSemiring_1480 -> T_IsNearSemiring_1062
du_isNearSemiring_1558 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    coe
      du_isNearSemiring_1196 (coe du_isSemiringWithoutOne_1462 (coe v1))
-- Algebra.Structures.IsCommutativeSemiring._.isPartialEquivalence
d_isPartialEquivalence_1560 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1560 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_1560 v8
du_isPartialEquivalence_1560 ::
  T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1560 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    let v4 = d_isMonoid_660 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    let v6 = d_isMagma_444 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v6))
-- Algebra.Structures.IsCommutativeSemiring._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1562 ::
  T_IsCommutativeSemiring_1480 ->
  T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1562 v0
  = coe
      d_isSemiringWithoutAnnihilatingZero_1386
      (coe d_isSemiring_1494 (coe v0))
-- Algebra.Structures.IsCommutativeSemiring._.isSemiringWithoutOne
d_isSemiringWithoutOne_1564 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 -> T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_1564 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isSemiringWithoutOne_1564 v8
du_isSemiringWithoutOne_1564 ::
  T_IsCommutativeSemiring_1480 -> T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_1564 v0
  = coe du_isSemiringWithoutOne_1462 (coe d_isSemiring_1494 (coe v0))
-- Algebra.Structures.IsCommutativeSemiring._.refl
d_refl_1566 :: T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
d_refl_1566 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe d_isSemiring_1494 (coe v0))))))))
-- Algebra.Structures.IsCommutativeSemiring._.reflexive
d_reflexive_1568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1568 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_1568 v8
du_reflexive_1568 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1568 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    let v4 = d_isMonoid_660 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    let v6 = d_isMagma_444 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v6)) v7
-- Algebra.Structures.IsCommutativeSemiring._.setoid
d_setoid_1570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1570 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_1570 v8
du_setoid_1570 ::
  T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1570 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    let v4 = d_isMonoid_660 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsCommutativeSemiring._.sym
d_sym_1572 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1572 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe d_isSemiring_1494 (coe v0))))))))
-- Algebra.Structures.IsCommutativeSemiring._.trans
d_trans_1574 ::
  T_IsCommutativeSemiring_1480 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1574 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe d_isSemiring_1494 (coe v0))))))))
-- Algebra.Structures.IsCommutativeSemiring._.zero
d_zero_1576 ::
  T_IsCommutativeSemiring_1480 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1576 v0 = coe d_zero_1388 (coe d_isSemiring_1494 (coe v0))
-- Algebra.Structures.IsCommutativeSemiring._.zeroʳ
d_zero'691'_1578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
d_zero'691'_1578 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_zero'691'_1578 v8
du_zero'691'_1578 ::
  T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
du_zero'691'_1578 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    coe du_zero'691'_1194 (coe du_isSemiringWithoutOne_1462 (coe v1))
-- Algebra.Structures.IsCommutativeSemiring._.zeroˡ
d_zero'737'_1580 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
d_zero'737'_1580 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_zero'737'_1580 v8
du_zero'737'_1580 ::
  T_IsCommutativeSemiring_1480 -> AgdaAny -> AgdaAny
du_zero'737'_1580 v0
  = let v1 = d_isSemiring_1494 (coe v0) in
    coe du_zero'737'_1192 (coe du_isSemiringWithoutOne_1462 (coe v1))
-- Algebra.Structures.IsCommutativeSemiring.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_1582 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 ->
  T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_1582 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       ~v7 v8
  = du_isCommutativeSemiringWithoutOne_1582 v8
du_isCommutativeSemiringWithoutOne_1582 ::
  T_IsCommutativeSemiring_1480 ->
  T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_1582 v0
  = coe
      C_IsCommutativeSemiringWithoutOne'46'constructor_36241
      (coe du_isSemiringWithoutOne_1462 (coe d_isSemiring_1494 (coe v0)))
      (coe d_'42''45'comm_1496 (coe v0))
-- Algebra.Structures.IsCommutativeSemiring._.isCommutativeMagma
d_isCommutativeMagma_1586 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeSemiring_1480 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_1586 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeMagma_1586 v8
du_isCommutativeMagma_1586 ::
  T_IsCommutativeSemiring_1480 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_1586 v0
  = let v1 = coe du_isCommutativeSemiringWithoutOne_1582 (coe v0) in
    coe
      du_isCommutativeMagma_550
      (coe du_'42''45'isCommutativeSemigroup_1256 (coe v1))
-- Algebra.Structures.IsCommutativeSemiring._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_1588 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 -> T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_1588 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                      ~v7 v8
  = du_'42''45'isCommutativeSemigroup_1588 v8
du_'42''45'isCommutativeSemigroup_1588 ::
  T_IsCommutativeSemiring_1480 -> T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_1588 v0
  = coe
      du_'42''45'isCommutativeSemigroup_1256
      (coe du_isCommutativeSemiringWithoutOne_1582 (coe v0))
-- Algebra.Structures.IsCommutativeSemiring.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_1590 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeSemiring_1480 -> T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_1590 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                   v8
  = du_'42''45'isCommutativeMonoid_1590 v8
du_'42''45'isCommutativeMonoid_1590 ::
  T_IsCommutativeSemiring_1480 -> T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_1590 v0
  = coe
      C_IsCommutativeMonoid'46'constructor_15379
      (coe
         du_'42''45'isMonoid_1352
         (coe
            d_isSemiringWithoutAnnihilatingZero_1386
            (coe d_isSemiring_1494 (coe v0))))
      (coe d_'42''45'comm_1496 (coe v0))
-- Algebra.Structures.IsCancellativeCommutativeSemiring
d_IsCancellativeCommutativeSemiring_1600 a0 a1 a2 a3 a4 a5 a6 a7
  = ()
data T_IsCancellativeCommutativeSemiring_1600
  = C_IsCancellativeCommutativeSemiring'46'constructor_50091 T_IsCommutativeSemiring_1480
                                                             (AgdaAny ->
                                                              AgdaAny ->
                                                              AgdaAny ->
                                                              (AgdaAny ->
                                                               MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
                                                              AgdaAny -> AgdaAny)
-- Algebra.Structures.IsCancellativeCommutativeSemiring.isCommutativeSemiring
d_isCommutativeSemiring_1614 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_1614 v0
  = case coe v0 of
      C_IsCancellativeCommutativeSemiring'46'constructor_50091 v1 v2
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCancellativeCommutativeSemiring.*-cancelˡ-nonZero
d_'42''45'cancel'737''45'nonZero_1616 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> AgdaAny
d_'42''45'cancel'737''45'nonZero_1616 v0
  = case coe v0 of
      C_IsCancellativeCommutativeSemiring'46'constructor_50091 v1 v2
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.*-assoc
d_'42''45'assoc_1620 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1620 v0
  = coe
      d_'42''45'assoc_1294
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.*-comm
d_'42''45'comm_1622 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_1622 v0
  = coe
      d_'42''45'comm_1496 (coe d_isCommutativeSemiring_1614 (coe v0))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.*-cong
d_'42''45'cong_1624 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1624 v0
  = coe
      d_'42''45'cong_1292
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.∙-congʳ
d_'8729''45'cong'691'_1626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1626 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_1626 v8
du_'8729''45'cong'691'_1626 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1626 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = coe du_'42''45'isMonoid_1352 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.∙-congˡ
d_'8729''45'cong'737'_1628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1628 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_1628 v8
du_'8729''45'cong'737'_1628 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1628 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = coe du_'42''45'isMonoid_1352 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.*-identity
d_'42''45'identity_1630 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1630 v0
  = coe
      d_'42''45'identity_1296
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.identityʳ
d_identity'691'_1632 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
d_identity'691'_1632 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_1632 v8
du_identity'691'_1632 ::
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
du_identity'691'_1632 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    coe du_identity'691'_642 (coe du_'42''45'isMonoid_1352 (coe v3))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.identityˡ
d_identity'737'_1634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
d_identity'737'_1634 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_1634 v8
du_identity'737'_1634 ::
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
du_identity'737'_1634 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    coe du_identity'737'_640 (coe du_'42''45'isMonoid_1352 (coe v3))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isCommutativeMagma
d_isCommutativeMagma_1636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeMagma_176
d_isCommutativeMagma_1636 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeMagma_1636 v8
du_isCommutativeMagma_1636 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeMagma_176
du_isCommutativeMagma_1636 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = coe du_isCommutativeSemiringWithoutOne_1582 (coe v1) in
    coe
      du_isCommutativeMagma_550
      (coe du_'42''45'isCommutativeSemigroup_1256 (coe v2))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_1638 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_1638 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                   v8
  = du_'42''45'isCommutativeMonoid_1638 v8
du_'42''45'isCommutativeMonoid_1638 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_1638 v0
  = coe
      du_'42''45'isCommutativeMonoid_1590
      (coe d_isCommutativeSemiring_1614 (coe v0))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_1640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_1640 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                      ~v7 v8
  = du_'42''45'isCommutativeSemigroup_1640 v8
du_'42''45'isCommutativeSemigroup_1640 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_1640 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    coe
      du_'42''45'isCommutativeSemigroup_1256
      (coe du_isCommutativeSemiringWithoutOne_1582 (coe v1))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.*-isMagma
d_'42''45'isMagma_1642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsMagma_140
d_'42''45'isMagma_1642 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMagma_1642 v8
du_'42''45'isMagma_1642 ::
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsMagma_140
du_'42''45'isMagma_1642 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    coe
      du_'42''45'isMagma_1348
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.*-isMonoid
d_'42''45'isMonoid_1644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsMonoid_600
d_'42''45'isMonoid_1644 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMonoid_1644 v8
du_'42''45'isMonoid_1644 ::
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsMonoid_600
du_'42''45'isMonoid_1644 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    coe
      du_'42''45'isMonoid_1352
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.*-isSemigroup
d_'42''45'isSemigroup_1646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsSemigroup_436
d_'42''45'isSemigroup_1646 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isSemigroup_1646 v8
du_'42''45'isSemigroup_1646 ::
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsSemigroup_436
du_'42''45'isSemigroup_1646 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    coe
      du_'42''45'isSemigroup_1350
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.assoc
d_assoc_1648 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1648 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660
            (coe
               d_'43''45'isCommutativeMonoid_1290
               (coe
                  d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0)))))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.comm
d_comm_1650 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_comm_1650 v0
  = coe
      d_comm_662
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe
            d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0)))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.∙-cong
d_'8729''45'cong_1652 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1652 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1290
                  (coe
                     d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0))))))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.∙-congʳ
d_'8729''45'cong'691'_1654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1654 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_1654 v8
du_'8729''45'cong'691'_1654 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1654 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    let v5 = d_isMonoid_660 (coe v4) in
    let v6 = d_isSemigroup_610 (coe v5) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v6))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.∙-congˡ
d_'8729''45'cong'737'_1656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1656 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_1656 v8
du_'8729''45'cong'737'_1656 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1656 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    let v5 = d_isMonoid_660 (coe v4) in
    let v6 = d_isSemigroup_610 (coe v5) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v6))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.identity
d_identity_1658 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1658 v0
  = coe
      d_identity_612
      (coe
         d_isMonoid_660
         (coe
            d_'43''45'isCommutativeMonoid_1290
            (coe
               d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0))))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.identityʳ
d_identity'691'_1660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
d_identity'691'_1660 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_1660 v8
du_identity'691'_1660 ::
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
du_identity'691'_1660 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    coe du_identity'691'_642 (coe d_isMonoid_660 (coe v4))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.identityˡ
d_identity'737'_1662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
d_identity'737'_1662 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_1662 v8
du_identity'737'_1662 ::
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
du_identity'737'_1662 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    coe du_identity'737'_640 (coe d_isMonoid_660 (coe v4))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isCommutativeMagma
d_isCommutativeMagma_1664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeMagma_176
d_isCommutativeMagma_1664 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeMagma_1664 v8
du_isCommutativeMagma_1664 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeMagma_176
du_isCommutativeMagma_1664 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v4))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1666 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1666 v0
  = coe
      d_'43''45'isCommutativeMonoid_1290
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isCommutativeSemigroup
d_isCommutativeSemigroup_1668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1668 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeSemigroup_1668 v8
du_isCommutativeSemigroup_1668 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1668 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    coe
      du_isCommutativeSemigroup_700
      (coe d_'43''45'isCommutativeMonoid_1290 (coe v3))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isMagma
d_isMagma_1670 ::
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsMagma_140
d_isMagma_1670 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660
            (coe
               d_'43''45'isCommutativeMonoid_1290
               (coe
                  d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0)))))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isMonoid
d_isMonoid_1672 ::
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsMonoid_600
d_isMonoid_1672 v0
  = coe
      d_isMonoid_660
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe
            d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0)))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isSemigroup
d_isSemigroup_1674 ::
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsSemigroup_436
d_isSemigroup_1674 v0
  = coe
      d_isSemigroup_610
      (coe
         d_isMonoid_660
         (coe
            d_'43''45'isCommutativeMonoid_1290
            (coe
               d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0))))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isUnitalMagma
d_isUnitalMagma_1676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsUnitalMagma_556
d_isUnitalMagma_1676 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isUnitalMagma_1676 v8
du_isUnitalMagma_1676 ::
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsUnitalMagma_556
du_isUnitalMagma_1676 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_660 (coe v4))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.distrib
d_distrib_1678 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1678 v0
  = coe
      d_distrib_1298
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe
            d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.distribʳ
d_distrib'691'_1680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1680 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'691'_1680 v8
du_distrib'691'_1680 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_1680 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    coe
      du_distrib'691'_1302
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.distribˡ
d_distrib'737'_1682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_1682 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'737'_1682 v8
du_distrib'737'_1682 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_1682 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    coe
      du_distrib'737'_1300
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_1684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_1684 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       ~v7 v8
  = du_isCommutativeSemiringWithoutOne_1684 v8
du_isCommutativeSemiringWithoutOne_1684 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_1684 v0
  = coe
      du_isCommutativeSemiringWithoutOne_1582
      (coe d_isCommutativeSemiring_1614 (coe v0))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isEquivalence
d_isEquivalence_1686 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1686 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1290
                  (coe
                     d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0))))))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isNearSemiring
d_isNearSemiring_1688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsNearSemiring_1062
d_isNearSemiring_1688 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isNearSemiring_1688 v8
du_isNearSemiring_1688 ::
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsNearSemiring_1062
du_isNearSemiring_1688 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    coe
      du_isNearSemiring_1196 (coe du_isSemiringWithoutOne_1462 (coe v2))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isPartialEquivalence
d_isPartialEquivalence_1690 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1690 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_1690 v8
du_isPartialEquivalence_1690 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1690 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    let v5 = d_isMonoid_660 (coe v4) in
    let v6 = d_isSemigroup_610 (coe v5) in
    let v7 = d_isMagma_444 (coe v6) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v7))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isSemiring
d_isSemiring_1692 ::
  T_IsCancellativeCommutativeSemiring_1600 -> T_IsSemiring_1372
d_isSemiring_1692 v0
  = coe d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1694 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1694 v0
  = coe
      d_isSemiringWithoutAnnihilatingZero_1386
      (coe d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0)))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.isSemiringWithoutOne
d_isSemiringWithoutOne_1696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_1696 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isSemiringWithoutOne_1696 v8
du_isSemiringWithoutOne_1696 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_1696 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    coe du_isSemiringWithoutOne_1462 (coe d_isSemiring_1494 (coe v1))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.refl
d_refl_1698 ::
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
d_refl_1698 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           d_isSemiring_1494
                           (coe d_isCommutativeSemiring_1614 (coe v0)))))))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.reflexive
d_reflexive_1700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1700 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_1700 v8
du_reflexive_1700 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1700 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    let v5 = d_isMonoid_660 (coe v4) in
    let v6 = d_isSemigroup_610 (coe v5) in
    let v7 = d_isMagma_444 (coe v6) in
    \ v8 v9 v10 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v7)) v8
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.setoid
d_setoid_1702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1702 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_1702 v8
du_setoid_1702 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1702 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    let v5 = d_isMonoid_660 (coe v4) in
    let v6 = d_isSemigroup_610 (coe v5) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v6))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.sym
d_sym_1704 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1704 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           d_isSemiring_1494
                           (coe d_isCommutativeSemiring_1614 (coe v0)))))))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.trans
d_trans_1706 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1706 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           d_isSemiring_1494
                           (coe d_isCommutativeSemiring_1614 (coe v0)))))))))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.zero
d_zero_1708 ::
  T_IsCancellativeCommutativeSemiring_1600 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1708 v0
  = coe
      d_zero_1388
      (coe d_isSemiring_1494 (coe d_isCommutativeSemiring_1614 (coe v0)))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.zeroʳ
d_zero'691'_1710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
d_zero'691'_1710 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_zero'691'_1710 v8
du_zero'691'_1710 ::
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
du_zero'691'_1710 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    coe du_zero'691'_1194 (coe du_isSemiringWithoutOne_1462 (coe v2))
-- Algebra.Structures.IsCancellativeCommutativeSemiring._.zeroˡ
d_zero'737'_1712 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
d_zero'737'_1712 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_zero'737'_1712 v8
du_zero'737'_1712 ::
  T_IsCancellativeCommutativeSemiring_1600 -> AgdaAny -> AgdaAny
du_zero'737'_1712 v0
  = let v1 = d_isCommutativeSemiring_1614 (coe v0) in
    let v2 = d_isSemiring_1494 (coe v1) in
    coe du_zero'737'_1192 (coe du_isSemiringWithoutOne_1462 (coe v2))
-- Algebra.Structures.IsIdempotentSemiring
d_IsIdempotentSemiring_1722 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsIdempotentSemiring_1722
  = C_IsIdempotentSemiring'46'constructor_53759 T_IsSemiring_1372
                                                (AgdaAny -> AgdaAny)
-- Algebra.Structures.IsIdempotentSemiring.isSemiring
d_isSemiring_1736 ::
  T_IsIdempotentSemiring_1722 -> T_IsSemiring_1372
d_isSemiring_1736 v0
  = case coe v0 of
      C_IsIdempotentSemiring'46'constructor_53759 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsIdempotentSemiring.+-idem
d_'43''45'idem_1738 ::
  T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
d_'43''45'idem_1738 v0
  = case coe v0 of
      C_IsIdempotentSemiring'46'constructor_53759 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsIdempotentSemiring._.*-assoc
d_'42''45'assoc_1742 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1742 v0
  = coe
      d_'42''45'assoc_1294
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1736 (coe v0)))
-- Algebra.Structures.IsIdempotentSemiring._.*-cong
d_'42''45'cong_1744 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1744 v0
  = coe
      d_'42''45'cong_1292
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1736 (coe v0)))
-- Algebra.Structures.IsIdempotentSemiring._.∙-congʳ
d_'8729''45'cong'691'_1746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1746 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_1746 v8
du_'8729''45'cong'691'_1746 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1746 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = coe du_'42''45'isMonoid_1352 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsIdempotentSemiring._.∙-congˡ
d_'8729''45'cong'737'_1748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1748 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_1748 v8
du_'8729''45'cong'737'_1748 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1748 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = coe du_'42''45'isMonoid_1352 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsIdempotentSemiring._.*-identity
d_'42''45'identity_1750 ::
  T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1750 v0
  = coe
      d_'42''45'identity_1296
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1736 (coe v0)))
-- Algebra.Structures.IsIdempotentSemiring._.identityʳ
d_identity'691'_1752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
d_identity'691'_1752 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_1752 v8
du_identity'691'_1752 ::
  T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
du_identity'691'_1752 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    coe du_identity'691'_642 (coe du_'42''45'isMonoid_1352 (coe v2))
-- Algebra.Structures.IsIdempotentSemiring._.identityˡ
d_identity'737'_1754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
d_identity'737'_1754 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_1754 v8
du_identity'737'_1754 ::
  T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
du_identity'737'_1754 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    coe du_identity'737'_640 (coe du_'42''45'isMonoid_1352 (coe v2))
-- Algebra.Structures.IsIdempotentSemiring._.*-isMagma
d_'42''45'isMagma_1756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsIdempotentSemiring_1722 -> T_IsMagma_140
d_'42''45'isMagma_1756 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMagma_1756 v8
du_'42''45'isMagma_1756 ::
  T_IsIdempotentSemiring_1722 -> T_IsMagma_140
du_'42''45'isMagma_1756 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    coe
      du_'42''45'isMagma_1348
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsIdempotentSemiring._.*-isMonoid
d_'42''45'isMonoid_1758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsIdempotentSemiring_1722 -> T_IsMonoid_600
d_'42''45'isMonoid_1758 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMonoid_1758 v8
du_'42''45'isMonoid_1758 ::
  T_IsIdempotentSemiring_1722 -> T_IsMonoid_600
du_'42''45'isMonoid_1758 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    coe
      du_'42''45'isMonoid_1352
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsIdempotentSemiring._.*-isSemigroup
d_'42''45'isSemigroup_1760 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsIdempotentSemiring_1722 -> T_IsSemigroup_436
d_'42''45'isSemigroup_1760 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isSemigroup_1760 v8
du_'42''45'isSemigroup_1760 ::
  T_IsIdempotentSemiring_1722 -> T_IsSemigroup_436
du_'42''45'isSemigroup_1760 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    coe
      du_'42''45'isSemigroup_1350
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsIdempotentSemiring._.assoc
d_assoc_1762 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1762 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660
            (coe
               d_'43''45'isCommutativeMonoid_1290
               (coe
                  d_isSemiringWithoutAnnihilatingZero_1386
                  (coe d_isSemiring_1736 (coe v0))))))
-- Algebra.Structures.IsIdempotentSemiring._.comm
d_comm_1764 ::
  T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_1764 v0
  = coe
      d_comm_662
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe
            d_isSemiringWithoutAnnihilatingZero_1386
            (coe d_isSemiring_1736 (coe v0))))
-- Algebra.Structures.IsIdempotentSemiring._.∙-cong
d_'8729''45'cong_1766 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1766 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1290
                  (coe
                     d_isSemiringWithoutAnnihilatingZero_1386
                     (coe d_isSemiring_1736 (coe v0)))))))
-- Algebra.Structures.IsIdempotentSemiring._.∙-congʳ
d_'8729''45'cong'691'_1768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1768 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_1768 v8
du_'8729''45'cong'691'_1768 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1768 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    let v4 = d_isMonoid_660 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsIdempotentSemiring._.∙-congˡ
d_'8729''45'cong'737'_1770 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1770 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_1770 v8
du_'8729''45'cong'737'_1770 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1770 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    let v4 = d_isMonoid_660 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsIdempotentSemiring._.identity
d_identity_1772 ::
  T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1772 v0
  = coe
      d_identity_612
      (coe
         d_isMonoid_660
         (coe
            d_'43''45'isCommutativeMonoid_1290
            (coe
               d_isSemiringWithoutAnnihilatingZero_1386
               (coe d_isSemiring_1736 (coe v0)))))
-- Algebra.Structures.IsIdempotentSemiring._.identityʳ
d_identity'691'_1774 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
d_identity'691'_1774 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_1774 v8
du_identity'691'_1774 ::
  T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
du_identity'691'_1774 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    coe du_identity'691'_642 (coe d_isMonoid_660 (coe v3))
-- Algebra.Structures.IsIdempotentSemiring._.identityˡ
d_identity'737'_1776 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
d_identity'737'_1776 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_1776 v8
du_identity'737'_1776 ::
  T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
du_identity'737'_1776 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    coe du_identity'737'_640 (coe d_isMonoid_660 (coe v3))
-- Algebra.Structures.IsIdempotentSemiring._.isCommutativeMagma
d_isCommutativeMagma_1778 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsIdempotentSemiring_1722 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_1778 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeMagma_1778 v8
du_isCommutativeMagma_1778 ::
  T_IsIdempotentSemiring_1722 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_1778 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v3))
-- Algebra.Structures.IsIdempotentSemiring._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1780 ::
  T_IsIdempotentSemiring_1722 -> T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1780 v0
  = coe
      d_'43''45'isCommutativeMonoid_1290
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1736 (coe v0)))
-- Algebra.Structures.IsIdempotentSemiring._.isCommutativeSemigroup
d_isCommutativeSemigroup_1782 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1782 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeSemigroup_1782 v8
du_isCommutativeSemigroup_1782 ::
  T_IsIdempotentSemiring_1722 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1782 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    coe
      du_isCommutativeSemigroup_700
      (coe d_'43''45'isCommutativeMonoid_1290 (coe v2))
-- Algebra.Structures.IsIdempotentSemiring._.isMagma
d_isMagma_1784 :: T_IsIdempotentSemiring_1722 -> T_IsMagma_140
d_isMagma_1784 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660
            (coe
               d_'43''45'isCommutativeMonoid_1290
               (coe
                  d_isSemiringWithoutAnnihilatingZero_1386
                  (coe d_isSemiring_1736 (coe v0))))))
-- Algebra.Structures.IsIdempotentSemiring._.isMonoid
d_isMonoid_1786 :: T_IsIdempotentSemiring_1722 -> T_IsMonoid_600
d_isMonoid_1786 v0
  = coe
      d_isMonoid_660
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe
            d_isSemiringWithoutAnnihilatingZero_1386
            (coe d_isSemiring_1736 (coe v0))))
-- Algebra.Structures.IsIdempotentSemiring._.isSemigroup
d_isSemigroup_1788 ::
  T_IsIdempotentSemiring_1722 -> T_IsSemigroup_436
d_isSemigroup_1788 v0
  = coe
      d_isSemigroup_610
      (coe
         d_isMonoid_660
         (coe
            d_'43''45'isCommutativeMonoid_1290
            (coe
               d_isSemiringWithoutAnnihilatingZero_1386
               (coe d_isSemiring_1736 (coe v0)))))
-- Algebra.Structures.IsIdempotentSemiring._.isUnitalMagma
d_isUnitalMagma_1790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsIdempotentSemiring_1722 -> T_IsUnitalMagma_556
d_isUnitalMagma_1790 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isUnitalMagma_1790 v8
du_isUnitalMagma_1790 ::
  T_IsIdempotentSemiring_1722 -> T_IsUnitalMagma_556
du_isUnitalMagma_1790 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_660 (coe v3))
-- Algebra.Structures.IsIdempotentSemiring._.distrib
d_distrib_1792 ::
  T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1792 v0
  = coe
      d_distrib_1298
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1736 (coe v0)))
-- Algebra.Structures.IsIdempotentSemiring._.distribʳ
d_distrib'691'_1794 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1794 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'691'_1794 v8
du_distrib'691'_1794 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_1794 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    coe
      du_distrib'691'_1302
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsIdempotentSemiring._.distribˡ
d_distrib'737'_1796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_1796 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'737'_1796 v8
du_distrib'737'_1796 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_1796 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    coe
      du_distrib'737'_1300
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsIdempotentSemiring._.isEquivalence
d_isEquivalence_1798 ::
  T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1798 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1290
                  (coe
                     d_isSemiringWithoutAnnihilatingZero_1386
                     (coe d_isSemiring_1736 (coe v0)))))))
-- Algebra.Structures.IsIdempotentSemiring._.isNearSemiring
d_isNearSemiring_1800 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsIdempotentSemiring_1722 -> T_IsNearSemiring_1062
d_isNearSemiring_1800 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isNearSemiring_1800 v8
du_isNearSemiring_1800 ::
  T_IsIdempotentSemiring_1722 -> T_IsNearSemiring_1062
du_isNearSemiring_1800 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    coe
      du_isNearSemiring_1196 (coe du_isSemiringWithoutOne_1462 (coe v1))
-- Algebra.Structures.IsIdempotentSemiring._.isPartialEquivalence
d_isPartialEquivalence_1802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1802 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_1802 v8
du_isPartialEquivalence_1802 ::
  T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1802 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    let v4 = d_isMonoid_660 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    let v6 = d_isMagma_444 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v6))
-- Algebra.Structures.IsIdempotentSemiring._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1804 ::
  T_IsIdempotentSemiring_1722 ->
  T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1804 v0
  = coe
      d_isSemiringWithoutAnnihilatingZero_1386
      (coe d_isSemiring_1736 (coe v0))
-- Algebra.Structures.IsIdempotentSemiring._.isSemiringWithoutOne
d_isSemiringWithoutOne_1806 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 -> T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_1806 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isSemiringWithoutOne_1806 v8
du_isSemiringWithoutOne_1806 ::
  T_IsIdempotentSemiring_1722 -> T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_1806 v0
  = coe du_isSemiringWithoutOne_1462 (coe d_isSemiring_1736 (coe v0))
-- Algebra.Structures.IsIdempotentSemiring._.refl
d_refl_1808 :: T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
d_refl_1808 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe d_isSemiring_1736 (coe v0))))))))
-- Algebra.Structures.IsIdempotentSemiring._.reflexive
d_reflexive_1810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1810 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_1810 v8
du_reflexive_1810 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1810 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    let v4 = d_isMonoid_660 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    let v6 = d_isMagma_444 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v6)) v7
-- Algebra.Structures.IsIdempotentSemiring._.setoid
d_setoid_1812 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1812 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_1812 v8
du_setoid_1812 ::
  T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1812 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    let v2 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v1) in
    let v3 = d_'43''45'isCommutativeMonoid_1290 (coe v2) in
    let v4 = d_isMonoid_660 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsIdempotentSemiring._.sym
d_sym_1814 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1814 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe d_isSemiring_1736 (coe v0))))))))
-- Algebra.Structures.IsIdempotentSemiring._.trans
d_trans_1816 ::
  T_IsIdempotentSemiring_1722 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1816 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe d_isSemiring_1736 (coe v0))))))))
-- Algebra.Structures.IsIdempotentSemiring._.zero
d_zero_1818 ::
  T_IsIdempotentSemiring_1722 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1818 v0 = coe d_zero_1388 (coe d_isSemiring_1736 (coe v0))
-- Algebra.Structures.IsIdempotentSemiring._.zeroʳ
d_zero'691'_1820 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
d_zero'691'_1820 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_zero'691'_1820 v8
du_zero'691'_1820 ::
  T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
du_zero'691'_1820 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    coe du_zero'691'_1194 (coe du_isSemiringWithoutOne_1462 (coe v1))
-- Algebra.Structures.IsIdempotentSemiring._.zeroˡ
d_zero'737'_1822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
d_zero'737'_1822 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_zero'737'_1822 v8
du_zero'737'_1822 ::
  T_IsIdempotentSemiring_1722 -> AgdaAny -> AgdaAny
du_zero'737'_1822 v0
  = let v1 = d_isSemiring_1736 (coe v0) in
    coe du_zero'737'_1192 (coe du_isSemiringWithoutOne_1462 (coe v1))
-- Algebra.Structures.IsKleeneAlgebra
d_IsKleeneAlgebra_1834 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsKleeneAlgebra_1834
  = C_IsKleeneAlgebra'46'constructor_57155 T_IsIdempotentSemiring_1722
                                           MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                           MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsKleeneAlgebra.isIdempotentSemiring
d_isIdempotentSemiring_1852 ::
  T_IsKleeneAlgebra_1834 -> T_IsIdempotentSemiring_1722
d_isIdempotentSemiring_1852 v0
  = case coe v0 of
      C_IsKleeneAlgebra'46'constructor_57155 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsKleeneAlgebra.starExpansive
d_starExpansive_1854 ::
  T_IsKleeneAlgebra_1834 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starExpansive_1854 v0
  = case coe v0 of
      C_IsKleeneAlgebra'46'constructor_57155 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsKleeneAlgebra.starDestructive
d_starDestructive_1856 ::
  T_IsKleeneAlgebra_1834 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_starDestructive_1856 v0
  = case coe v0 of
      C_IsKleeneAlgebra'46'constructor_57155 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsKleeneAlgebra._.*-assoc
d_'42''45'assoc_1860 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1860 v0
  = coe
      d_'42''45'assoc_1294
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0))))
-- Algebra.Structures.IsKleeneAlgebra._.*-cong
d_'42''45'cong_1862 ::
  T_IsKleeneAlgebra_1834 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1862 v0
  = coe
      d_'42''45'cong_1292
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0))))
-- Algebra.Structures.IsKleeneAlgebra._.∙-congʳ
d_'8729''45'cong'691'_1864 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1864 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_1864 v9
du_'8729''45'cong'691'_1864 ::
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1864 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = coe du_'42''45'isMonoid_1352 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsKleeneAlgebra._.∙-congˡ
d_'8729''45'cong'737'_1866 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1866 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_1866 v9
du_'8729''45'cong'737'_1866 ::
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1866 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = coe du_'42''45'isMonoid_1352 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsKleeneAlgebra._.*-identity
d_'42''45'identity_1868 ::
  T_IsKleeneAlgebra_1834 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1868 v0
  = coe
      d_'42''45'identity_1296
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0))))
-- Algebra.Structures.IsKleeneAlgebra._.identityʳ
d_identity'691'_1870 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
d_identity'691'_1870 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_1870 v9
du_identity'691'_1870 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
du_identity'691'_1870 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    coe du_identity'691'_642 (coe du_'42''45'isMonoid_1352 (coe v3))
-- Algebra.Structures.IsKleeneAlgebra._.identityˡ
d_identity'737'_1872 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
d_identity'737'_1872 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_1872 v9
du_identity'737'_1872 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
du_identity'737'_1872 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    coe du_identity'737'_640 (coe du_'42''45'isMonoid_1352 (coe v3))
-- Algebra.Structures.IsKleeneAlgebra._.*-isMagma
d_'42''45'isMagma_1874 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> T_IsMagma_140
d_'42''45'isMagma_1874 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMagma_1874 v9
du_'42''45'isMagma_1874 :: T_IsKleeneAlgebra_1834 -> T_IsMagma_140
du_'42''45'isMagma_1874 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    coe
      du_'42''45'isMagma_1348
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsKleeneAlgebra._.*-isMonoid
d_'42''45'isMonoid_1876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> T_IsMonoid_600
d_'42''45'isMonoid_1876 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMonoid_1876 v9
du_'42''45'isMonoid_1876 ::
  T_IsKleeneAlgebra_1834 -> T_IsMonoid_600
du_'42''45'isMonoid_1876 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    coe
      du_'42''45'isMonoid_1352
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsKleeneAlgebra._.*-isSemigroup
d_'42''45'isSemigroup_1878 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> T_IsSemigroup_436
d_'42''45'isSemigroup_1878 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isSemigroup_1878 v9
du_'42''45'isSemigroup_1878 ::
  T_IsKleeneAlgebra_1834 -> T_IsSemigroup_436
du_'42''45'isSemigroup_1878 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    coe
      du_'42''45'isSemigroup_1350
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsKleeneAlgebra._.assoc
d_assoc_1880 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1880 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660
            (coe
               d_'43''45'isCommutativeMonoid_1290
               (coe
                  d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0)))))))
-- Algebra.Structures.IsKleeneAlgebra._.comm
d_comm_1882 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_1882 v0
  = coe
      d_comm_662
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe
            d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0)))))
-- Algebra.Structures.IsKleeneAlgebra._.∙-cong
d_'8729''45'cong_1884 ::
  T_IsKleeneAlgebra_1834 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_1884 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1290
                  (coe
                     d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0))))))))
-- Algebra.Structures.IsKleeneAlgebra._.∙-congʳ
d_'8729''45'cong'691'_1886 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_1886 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_1886 v9
du_'8729''45'cong'691'_1886 ::
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_1886 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    let v5 = d_isMonoid_660 (coe v4) in
    let v6 = d_isSemigroup_610 (coe v5) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v6))
-- Algebra.Structures.IsKleeneAlgebra._.∙-congˡ
d_'8729''45'cong'737'_1888 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_1888 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_1888 v9
du_'8729''45'cong'737'_1888 ::
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_1888 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    let v5 = d_isMonoid_660 (coe v4) in
    let v6 = d_isSemigroup_610 (coe v5) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v6))
-- Algebra.Structures.IsKleeneAlgebra._.+-idem
d_'43''45'idem_1890 :: T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
d_'43''45'idem_1890 v0
  = coe
      d_'43''45'idem_1738 (coe d_isIdempotentSemiring_1852 (coe v0))
-- Algebra.Structures.IsKleeneAlgebra._.identity
d_identity_1892 ::
  T_IsKleeneAlgebra_1834 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_1892 v0
  = coe
      d_identity_612
      (coe
         d_isMonoid_660
         (coe
            d_'43''45'isCommutativeMonoid_1290
            (coe
               d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0))))))
-- Algebra.Structures.IsKleeneAlgebra._.identityʳ
d_identity'691'_1894 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
d_identity'691'_1894 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_1894 v9
du_identity'691'_1894 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
du_identity'691'_1894 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    coe du_identity'691'_642 (coe d_isMonoid_660 (coe v4))
-- Algebra.Structures.IsKleeneAlgebra._.identityˡ
d_identity'737'_1896 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
d_identity'737'_1896 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_1896 v9
du_identity'737'_1896 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
du_identity'737'_1896 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    coe du_identity'737'_640 (coe d_isMonoid_660 (coe v4))
-- Algebra.Structures.IsKleeneAlgebra._.isCommutativeMagma
d_isCommutativeMagma_1898 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsKleeneAlgebra_1834 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_1898 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMagma_1898 v9
du_isCommutativeMagma_1898 ::
  T_IsKleeneAlgebra_1834 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_1898 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v4))
-- Algebra.Structures.IsKleeneAlgebra._.+-isCommutativeMonoid
d_'43''45'isCommutativeMonoid_1900 ::
  T_IsKleeneAlgebra_1834 -> T_IsCommutativeMonoid_650
d_'43''45'isCommutativeMonoid_1900 v0
  = coe
      d_'43''45'isCommutativeMonoid_1290
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0))))
-- Algebra.Structures.IsKleeneAlgebra._.isCommutativeSemigroup
d_isCommutativeSemigroup_1902 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsKleeneAlgebra_1834 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_1902 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                              v9
  = du_isCommutativeSemigroup_1902 v9
du_isCommutativeSemigroup_1902 ::
  T_IsKleeneAlgebra_1834 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_1902 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    coe
      du_isCommutativeSemigroup_700
      (coe d_'43''45'isCommutativeMonoid_1290 (coe v3))
-- Algebra.Structures.IsKleeneAlgebra._.isMagma
d_isMagma_1904 :: T_IsKleeneAlgebra_1834 -> T_IsMagma_140
d_isMagma_1904 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_660
            (coe
               d_'43''45'isCommutativeMonoid_1290
               (coe
                  d_isSemiringWithoutAnnihilatingZero_1386
                  (coe
                     d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0)))))))
-- Algebra.Structures.IsKleeneAlgebra._.isMonoid
d_isMonoid_1906 :: T_IsKleeneAlgebra_1834 -> T_IsMonoid_600
d_isMonoid_1906 v0
  = coe
      d_isMonoid_660
      (coe
         d_'43''45'isCommutativeMonoid_1290
         (coe
            d_isSemiringWithoutAnnihilatingZero_1386
            (coe
               d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0)))))
-- Algebra.Structures.IsKleeneAlgebra._.isSemigroup
d_isSemigroup_1908 :: T_IsKleeneAlgebra_1834 -> T_IsSemigroup_436
d_isSemigroup_1908 v0
  = coe
      d_isSemigroup_610
      (coe
         d_isMonoid_660
         (coe
            d_'43''45'isCommutativeMonoid_1290
            (coe
               d_isSemiringWithoutAnnihilatingZero_1386
               (coe
                  d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0))))))
-- Algebra.Structures.IsKleeneAlgebra._.isUnitalMagma
d_isUnitalMagma_1910 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> T_IsUnitalMagma_556
d_isUnitalMagma_1910 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isUnitalMagma_1910 v9
du_isUnitalMagma_1910 ::
  T_IsKleeneAlgebra_1834 -> T_IsUnitalMagma_556
du_isUnitalMagma_1910 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_660 (coe v4))
-- Algebra.Structures.IsKleeneAlgebra._.distrib
d_distrib_1912 ::
  T_IsKleeneAlgebra_1834 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1912 v0
  = coe
      d_distrib_1298
      (coe
         d_isSemiringWithoutAnnihilatingZero_1386
         (coe d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0))))
-- Algebra.Structures.IsKleeneAlgebra._.distribʳ
d_distrib'691'_1914 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_1914 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_distrib'691'_1914 v9
du_distrib'691'_1914 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_1914 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    coe
      du_distrib'691'_1302
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsKleeneAlgebra._.distribˡ
d_distrib'737'_1916 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_1916 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_distrib'737'_1916 v9
du_distrib'737'_1916 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_1916 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    coe
      du_distrib'737'_1300
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsKleeneAlgebra._.isEquivalence
d_isEquivalence_1918 ::
  T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_1918 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_660
               (coe
                  d_'43''45'isCommutativeMonoid_1290
                  (coe
                     d_isSemiringWithoutAnnihilatingZero_1386
                     (coe
                        d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0))))))))
-- Algebra.Structures.IsKleeneAlgebra._.isNearSemiring
d_isNearSemiring_1920 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsKleeneAlgebra_1834 -> T_IsNearSemiring_1062
d_isNearSemiring_1920 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isNearSemiring_1920 v9
du_isNearSemiring_1920 ::
  T_IsKleeneAlgebra_1834 -> T_IsNearSemiring_1062
du_isNearSemiring_1920 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    coe
      du_isNearSemiring_1196 (coe du_isSemiringWithoutOne_1462 (coe v2))
-- Algebra.Structures.IsKleeneAlgebra._.isPartialEquivalence
d_isPartialEquivalence_1922 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_1922 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_1922 v9
du_isPartialEquivalence_1922 ::
  T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_1922 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    let v5 = d_isMonoid_660 (coe v4) in
    let v6 = d_isSemigroup_610 (coe v5) in
    let v7 = d_isMagma_444 (coe v6) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v7))
-- Algebra.Structures.IsKleeneAlgebra._.isSemiring
d_isSemiring_1924 :: T_IsKleeneAlgebra_1834 -> T_IsSemiring_1372
d_isSemiring_1924 v0
  = coe d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0))
-- Algebra.Structures.IsKleeneAlgebra._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_1926 ::
  T_IsKleeneAlgebra_1834 -> T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_1926 v0
  = coe
      d_isSemiringWithoutAnnihilatingZero_1386
      (coe d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0)))
-- Algebra.Structures.IsKleeneAlgebra._.isSemiringWithoutOne
d_isSemiringWithoutOne_1928 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsKleeneAlgebra_1834 -> T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_1928 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isSemiringWithoutOne_1928 v9
du_isSemiringWithoutOne_1928 ::
  T_IsKleeneAlgebra_1834 -> T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_1928 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    coe du_isSemiringWithoutOne_1462 (coe d_isSemiring_1736 (coe v1))
-- Algebra.Structures.IsKleeneAlgebra._.refl
d_refl_1930 :: T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
d_refl_1930 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0)))))))))
-- Algebra.Structures.IsKleeneAlgebra._.reflexive
d_reflexive_1932 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_1932 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_1932 v9
du_reflexive_1932 ::
  T_IsKleeneAlgebra_1834 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_1932 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    let v5 = d_isMonoid_660 (coe v4) in
    let v6 = d_isSemigroup_610 (coe v5) in
    let v7 = d_isMagma_444 (coe v6) in
    \ v8 v9 v10 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v7)) v8
-- Algebra.Structures.IsKleeneAlgebra._.setoid
d_setoid_1934 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_1934 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_1934 v9
du_setoid_1934 ::
  T_IsKleeneAlgebra_1834 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_1934 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    let v3 = d_isSemiringWithoutAnnihilatingZero_1386 (coe v2) in
    let v4 = d_'43''45'isCommutativeMonoid_1290 (coe v3) in
    let v5 = d_isMonoid_660 (coe v4) in
    let v6 = d_isSemigroup_610 (coe v5) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v6))
-- Algebra.Structures.IsKleeneAlgebra._.sym
d_sym_1936 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_1936 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0)))))))))
-- Algebra.Structures.IsKleeneAlgebra._.trans
d_trans_1938 ::
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_1938 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_660
                  (coe
                     d_'43''45'isCommutativeMonoid_1290
                     (coe
                        d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0)))))))))
-- Algebra.Structures.IsKleeneAlgebra._.zero
d_zero_1940 ::
  T_IsKleeneAlgebra_1834 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1940 v0
  = coe
      d_zero_1388
      (coe d_isSemiring_1736 (coe d_isIdempotentSemiring_1852 (coe v0)))
-- Algebra.Structures.IsKleeneAlgebra._.zeroʳ
d_zero'691'_1942 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
d_zero'691'_1942 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_zero'691'_1942 v9
du_zero'691'_1942 :: T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
du_zero'691'_1942 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    coe du_zero'691'_1194 (coe du_isSemiringWithoutOne_1462 (coe v2))
-- Algebra.Structures.IsKleeneAlgebra._.zeroˡ
d_zero'737'_1944 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
d_zero'737'_1944 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_zero'737'_1944 v9
du_zero'737'_1944 :: T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
du_zero'737'_1944 v0
  = let v1 = d_isIdempotentSemiring_1852 (coe v0) in
    let v2 = d_isSemiring_1736 (coe v1) in
    coe du_zero'737'_1192 (coe du_isSemiringWithoutOne_1462 (coe v2))
-- Algebra.Structures.IsKleeneAlgebra.starExpansiveˡ
d_starExpansive'737'_1946 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
d_starExpansive'737'_1946 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_starExpansive'737'_1946 v9
du_starExpansive'737'_1946 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
du_starExpansive'737'_1946 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_starExpansive_1854 (coe v0))
-- Algebra.Structures.IsKleeneAlgebra.starExpansiveʳ
d_starExpansive'691'_1948 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
d_starExpansive'691'_1948 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_starExpansive'691'_1948 v9
du_starExpansive'691'_1948 ::
  T_IsKleeneAlgebra_1834 -> AgdaAny -> AgdaAny
du_starExpansive'691'_1948 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_starExpansive_1854 (coe v0))
-- Algebra.Structures.IsKleeneAlgebra.starDestructiveˡ
d_starDestructive'737'_1950 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_starDestructive'737'_1950 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_starDestructive'737'_1950 v9
du_starDestructive'737'_1950 ::
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_starDestructive'737'_1950 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_starDestructive_1856 (coe v0))
-- Algebra.Structures.IsKleeneAlgebra.starDestructiveʳ
d_starDestructive'691'_1952 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_starDestructive'691'_1952 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_starDestructive'691'_1952 v9
du_starDestructive'691'_1952 ::
  T_IsKleeneAlgebra_1834 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_starDestructive'691'_1952 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_starDestructive_1856 (coe v0))
-- Algebra.Structures.IsQuasiring
d_IsQuasiring_1962 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsQuasiring_1962
  = C_IsQuasiring'46'constructor_63027 T_IsMonoid_600
                                       (AgdaAny ->
                                        AgdaAny ->
                                        AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                       MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                       MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                       MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsQuasiring.+-isMonoid
d_'43''45'isMonoid_1984 :: T_IsQuasiring_1962 -> T_IsMonoid_600
d_'43''45'isMonoid_1984 v0
  = case coe v0 of
      C_IsQuasiring'46'constructor_63027 v1 v2 v3 v4 v5 v6 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasiring.*-cong
d_'42''45'cong_1986 ::
  T_IsQuasiring_1962 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_1986 v0
  = case coe v0 of
      C_IsQuasiring'46'constructor_63027 v1 v2 v3 v4 v5 v6 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasiring.*-assoc
d_'42''45'assoc_1988 ::
  T_IsQuasiring_1962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_1988 v0
  = case coe v0 of
      C_IsQuasiring'46'constructor_63027 v1 v2 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasiring.*-identity
d_'42''45'identity_1990 ::
  T_IsQuasiring_1962 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_1990 v0
  = case coe v0 of
      C_IsQuasiring'46'constructor_63027 v1 v2 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasiring.distrib
d_distrib_1992 ::
  T_IsQuasiring_1962 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_1992 v0
  = case coe v0 of
      C_IsQuasiring'46'constructor_63027 v1 v2 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasiring.zero
d_zero_1994 ::
  T_IsQuasiring_1962 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_1994 v0
  = case coe v0 of
      C_IsQuasiring'46'constructor_63027 v1 v2 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasiring._.assoc
d_assoc_1998 ::
  T_IsQuasiring_1962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_1998 v0
  = coe
      d_assoc_446
      (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1984 (coe v0)))
-- Algebra.Structures.IsQuasiring._.∙-cong
d_'8729''45'cong_2000 ::
  T_IsQuasiring_1962 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2000 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1984 (coe v0))))
-- Algebra.Structures.IsQuasiring._.∙-congʳ
d_'8729''45'cong'691'_2002 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsQuasiring_1962 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2002 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_2002 v8
du_'8729''45'cong'691'_2002 ::
  T_IsQuasiring_1962 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2002 v0
  = let v1 = d_'43''45'isMonoid_1984 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsQuasiring._.∙-congˡ
d_'8729''45'cong'737'_2004 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsQuasiring_1962 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2004 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_2004 v8
du_'8729''45'cong'737'_2004 ::
  T_IsQuasiring_1962 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2004 v0
  = let v1 = d_'43''45'isMonoid_1984 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsQuasiring._.identity
d_identity_2006 ::
  T_IsQuasiring_1962 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2006 v0
  = coe d_identity_612 (coe d_'43''45'isMonoid_1984 (coe v0))
-- Algebra.Structures.IsQuasiring._.identityʳ
d_identity'691'_2008 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsQuasiring_1962 -> AgdaAny -> AgdaAny
d_identity'691'_2008 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_2008 v8
du_identity'691'_2008 :: T_IsQuasiring_1962 -> AgdaAny -> AgdaAny
du_identity'691'_2008 v0
  = coe du_identity'691'_642 (coe d_'43''45'isMonoid_1984 (coe v0))
-- Algebra.Structures.IsQuasiring._.identityˡ
d_identity'737'_2010 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsQuasiring_1962 -> AgdaAny -> AgdaAny
d_identity'737'_2010 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_2010 v8
du_identity'737'_2010 :: T_IsQuasiring_1962 -> AgdaAny -> AgdaAny
du_identity'737'_2010 v0
  = coe du_identity'737'_640 (coe d_'43''45'isMonoid_1984 (coe v0))
-- Algebra.Structures.IsQuasiring._.isMagma
d_isMagma_2012 :: T_IsQuasiring_1962 -> T_IsMagma_140
d_isMagma_2012 v0
  = coe
      d_isMagma_444
      (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1984 (coe v0)))
-- Algebra.Structures.IsQuasiring._.isSemigroup
d_isSemigroup_2014 :: T_IsQuasiring_1962 -> T_IsSemigroup_436
d_isSemigroup_2014 v0
  = coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1984 (coe v0))
-- Algebra.Structures.IsQuasiring._.isUnitalMagma
d_isUnitalMagma_2016 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsQuasiring_1962 -> T_IsUnitalMagma_556
d_isUnitalMagma_2016 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isUnitalMagma_2016 v8
du_isUnitalMagma_2016 :: T_IsQuasiring_1962 -> T_IsUnitalMagma_556
du_isUnitalMagma_2016 v0
  = coe du_isUnitalMagma_644 (coe d_'43''45'isMonoid_1984 (coe v0))
-- Algebra.Structures.IsQuasiring._.isEquivalence
d_isEquivalence_2018 ::
  T_IsQuasiring_1962 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2018 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1984 (coe v0))))
-- Algebra.Structures.IsQuasiring._.isPartialEquivalence
d_isPartialEquivalence_2020 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsQuasiring_1962 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2020 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_2020 v8
du_isPartialEquivalence_2020 ::
  T_IsQuasiring_1962 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2020 v0
  = let v1 = d_'43''45'isMonoid_1984 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    let v3 = d_isMagma_444 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v3))
-- Algebra.Structures.IsQuasiring._.refl
d_refl_2022 :: T_IsQuasiring_1962 -> AgdaAny -> AgdaAny
d_refl_2022 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1984 (coe v0)))))
-- Algebra.Structures.IsQuasiring._.reflexive
d_reflexive_2024 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsQuasiring_1962 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2024 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_2024 v8
du_reflexive_2024 ::
  T_IsQuasiring_1962 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2024 v0
  = let v1 = d_'43''45'isMonoid_1984 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    let v3 = d_isMagma_444 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v3)) v4
-- Algebra.Structures.IsQuasiring._.setoid
d_setoid_2026 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsQuasiring_1962 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2026 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_2026 v8
du_setoid_2026 ::
  T_IsQuasiring_1962 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2026 v0
  = let v1 = d_'43''45'isMonoid_1984 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsQuasiring._.sym
d_sym_2028 ::
  T_IsQuasiring_1962 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2028 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1984 (coe v0)))))
-- Algebra.Structures.IsQuasiring._.trans
d_trans_2030 ::
  T_IsQuasiring_1962 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2030 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1984 (coe v0)))))
-- Algebra.Structures.IsQuasiring.*-isMagma
d_'42''45'isMagma_2032 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsQuasiring_1962 -> T_IsMagma_140
d_'42''45'isMagma_2032 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMagma_2032 v8
du_'42''45'isMagma_2032 :: T_IsQuasiring_1962 -> T_IsMagma_140
du_'42''45'isMagma_2032 v0
  = coe
      C_IsMagma'46'constructor_769
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe d_isSemigroup_610 (coe d_'43''45'isMonoid_1984 (coe v0)))))
      (coe d_'42''45'cong_1986 (coe v0))
-- Algebra.Structures.IsQuasiring.*-isSemigroup
d_'42''45'isSemigroup_2034 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsQuasiring_1962 -> T_IsSemigroup_436
d_'42''45'isSemigroup_2034 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isSemigroup_2034 v8
du_'42''45'isSemigroup_2034 ::
  T_IsQuasiring_1962 -> T_IsSemigroup_436
du_'42''45'isSemigroup_2034 v0
  = coe
      C_IsSemigroup'46'constructor_9303
      (coe du_'42''45'isMagma_2032 (coe v0))
      (coe d_'42''45'assoc_1988 (coe v0))
-- Algebra.Structures.IsQuasiring.*-isMonoid
d_'42''45'isMonoid_2036 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsQuasiring_1962 -> T_IsMonoid_600
d_'42''45'isMonoid_2036 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMonoid_2036 v8
du_'42''45'isMonoid_2036 :: T_IsQuasiring_1962 -> T_IsMonoid_600
du_'42''45'isMonoid_2036 v0
  = coe
      C_IsMonoid'46'constructor_13559
      (coe du_'42''45'isSemigroup_2034 (coe v0))
      (coe d_'42''45'identity_1990 (coe v0))
-- Algebra.Structures.IsQuasiring._.∙-congʳ
d_'8729''45'cong'691'_2040 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsQuasiring_1962 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2040 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_2040 v8
du_'8729''45'cong'691'_2040 ::
  T_IsQuasiring_1962 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2040 v0
  = let v1 = coe du_'42''45'isMonoid_2036 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsQuasiring._.∙-congˡ
d_'8729''45'cong'737'_2042 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsQuasiring_1962 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2042 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_2042 v8
du_'8729''45'cong'737'_2042 ::
  T_IsQuasiring_1962 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2042 v0
  = let v1 = coe du_'42''45'isMonoid_2036 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsQuasiring._.identityʳ
d_identity'691'_2044 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsQuasiring_1962 -> AgdaAny -> AgdaAny
d_identity'691'_2044 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_2044 v8
du_identity'691'_2044 :: T_IsQuasiring_1962 -> AgdaAny -> AgdaAny
du_identity'691'_2044 v0
  = coe du_identity'691'_642 (coe du_'42''45'isMonoid_2036 (coe v0))
-- Algebra.Structures.IsQuasiring._.identityˡ
d_identity'737'_2046 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsQuasiring_1962 -> AgdaAny -> AgdaAny
d_identity'737'_2046 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_2046 v8
du_identity'737'_2046 :: T_IsQuasiring_1962 -> AgdaAny -> AgdaAny
du_identity'737'_2046 v0
  = coe du_identity'737'_640 (coe du_'42''45'isMonoid_2036 (coe v0))
-- Algebra.Structures.IsRingWithoutOne
d_IsRingWithoutOne_2056 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsRingWithoutOne_2056
  = C_IsRingWithoutOne'46'constructor_66353 T_IsAbelianGroup_976
                                            (AgdaAny ->
                                             AgdaAny ->
                                             AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                            (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsRingWithoutOne.+-isAbelianGroup
d_'43''45'isAbelianGroup_2076 ::
  T_IsRingWithoutOne_2056 -> T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_2076 v0
  = case coe v0 of
      C_IsRingWithoutOne'46'constructor_66353 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRingWithoutOne.*-cong
d_'42''45'cong_2078 ::
  T_IsRingWithoutOne_2056 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2078 v0
  = case coe v0 of
      C_IsRingWithoutOne'46'constructor_66353 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRingWithoutOne.*-assoc
d_'42''45'assoc_2080 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_2080 v0
  = case coe v0 of
      C_IsRingWithoutOne'46'constructor_66353 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRingWithoutOne.distrib
d_distrib_2082 ::
  T_IsRingWithoutOne_2056 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2082 v0
  = case coe v0 of
      C_IsRingWithoutOne'46'constructor_66353 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRingWithoutOne.zero
d_zero_2084 ::
  T_IsRingWithoutOne_2056 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2084 v0
  = case coe v0 of
      C_IsRingWithoutOne'46'constructor_66353 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRingWithoutOne._._-_
d__'45'__2088 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__2088 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 ~v7 ~v8
  = du__'45'__2088 v4 v6
du__'45'__2088 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__2088 v0 v1 = coe du__'45'__944 (coe v0) (coe v1)
-- Algebra.Structures.IsRingWithoutOne._.assoc
d_assoc_2090 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2090 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_902
            (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0)))))
-- Algebra.Structures.IsRingWithoutOne._.comm
d_comm_2092 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2092 v0
  = coe d_comm_990 (coe d_'43''45'isAbelianGroup_2076 (coe v0))
-- Algebra.Structures.IsRingWithoutOne._.∙-cong
d_'8729''45'cong_2094 ::
  T_IsRingWithoutOne_2056 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2094 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_902
               (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0))))))
-- Algebra.Structures.IsRingWithoutOne._.∙-congʳ
d_'8729''45'cong'691'_2096 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2096 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_2096 v8
du_'8729''45'cong'691'_2096 ::
  T_IsRingWithoutOne_2056 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2096 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsRingWithoutOne._.∙-congˡ
d_'8729''45'cong'737'_2098 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2098 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_2098 v8
du_'8729''45'cong'737'_2098 ::
  T_IsRingWithoutOne_2056 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2098 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsRingWithoutOne._.identity
d_identity_2100 ::
  T_IsRingWithoutOne_2056 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2100 v0
  = coe
      d_identity_612
      (coe
         d_isMonoid_902
         (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0))))
-- Algebra.Structures.IsRingWithoutOne._.identityʳ
d_identity'691'_2102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
d_identity'691'_2102 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_2102 v8
du_identity'691'_2102 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
du_identity'691'_2102 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    coe du_identity'691'_642 (coe d_isMonoid_902 (coe v2))
-- Algebra.Structures.IsRingWithoutOne._.identityˡ
d_identity'737'_2104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
d_identity'737'_2104 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_2104 v8
du_identity'737'_2104 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
du_identity'737'_2104 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    coe du_identity'737'_640 (coe d_isMonoid_902 (coe v2))
-- Algebra.Structures.IsRingWithoutOne._.isCommutativeMagma
d_isCommutativeMagma_2106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_2106 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeMagma_2106 v8
du_isCommutativeMagma_2106 ::
  T_IsRingWithoutOne_2056 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_2106 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    let v2 = coe du_isCommutativeMonoid_1048 (coe v1) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v2))
-- Algebra.Structures.IsRingWithoutOne._.isCommutativeMonoid
d_isCommutativeMonoid_2108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> T_IsCommutativeMonoid_650
d_isCommutativeMonoid_2108 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeMonoid_2108 v8
du_isCommutativeMonoid_2108 ::
  T_IsRingWithoutOne_2056 -> T_IsCommutativeMonoid_650
du_isCommutativeMonoid_2108 v0
  = coe
      du_isCommutativeMonoid_1048
      (coe d_'43''45'isAbelianGroup_2076 (coe v0))
-- Algebra.Structures.IsRingWithoutOne._.isCommutativeSemigroup
d_isCommutativeSemigroup_2110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_2110 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isCommutativeSemigroup_2110 v8
du_isCommutativeSemigroup_2110 ::
  T_IsRingWithoutOne_2056 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_2110 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    coe
      du_isCommutativeSemigroup_700
      (coe du_isCommutativeMonoid_1048 (coe v1))
-- Algebra.Structures.IsRingWithoutOne._.isGroup
d_isGroup_2112 :: T_IsRingWithoutOne_2056 -> T_IsGroup_888
d_isGroup_2112 v0
  = coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0))
-- Algebra.Structures.IsRingWithoutOne._.isInvertibleMagma
d_isInvertibleMagma_2114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> T_IsInvertibleMagma_776
d_isInvertibleMagma_2114 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isInvertibleMagma_2114 v8
du_isInvertibleMagma_2114 ::
  T_IsRingWithoutOne_2056 -> T_IsInvertibleMagma_776
du_isInvertibleMagma_2114 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    coe du_isInvertibleMagma_966 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsRingWithoutOne._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_2116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_2116 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isInvertibleUnitalMagma_2116 v8
du_isInvertibleUnitalMagma_2116 ::
  T_IsRingWithoutOne_2056 -> T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_2116 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    coe du_isInvertibleUnitalMagma_968 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsRingWithoutOne._.isMagma
d_isMagma_2118 :: T_IsRingWithoutOne_2056 -> T_IsMagma_140
d_isMagma_2118 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_902
            (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0)))))
-- Algebra.Structures.IsRingWithoutOne._.isMonoid
d_isMonoid_2120 :: T_IsRingWithoutOne_2056 -> T_IsMonoid_600
d_isMonoid_2120 v0
  = coe
      d_isMonoid_902
      (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0)))
-- Algebra.Structures.IsRingWithoutOne._.isSemigroup
d_isSemigroup_2122 :: T_IsRingWithoutOne_2056 -> T_IsSemigroup_436
d_isSemigroup_2122 v0
  = coe
      d_isSemigroup_610
      (coe
         d_isMonoid_902
         (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0))))
-- Algebra.Structures.IsRingWithoutOne._.isUnitalMagma
d_isUnitalMagma_2124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> T_IsUnitalMagma_556
d_isUnitalMagma_2124 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isUnitalMagma_2124 v8
du_isUnitalMagma_2124 ::
  T_IsRingWithoutOne_2056 -> T_IsUnitalMagma_556
du_isUnitalMagma_2124 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_902 (coe v2))
-- Algebra.Structures.IsRingWithoutOne._.⁻¹-cong
d_'8315''185''45'cong_2126 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_2126 v0
  = coe
      d_'8315''185''45'cong_906
      (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0)))
-- Algebra.Structures.IsRingWithoutOne._.inverse
d_inverse_2128 ::
  T_IsRingWithoutOne_2056 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_2128 v0
  = coe
      d_inverse_904
      (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0)))
-- Algebra.Structures.IsRingWithoutOne._.inverseʳ
d_inverse'691'_2130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
d_inverse'691'_2130 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_inverse'691'_2130 v8
du_inverse'691'_2130 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
du_inverse'691'_2130 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    coe du_inverse'691'_952 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsRingWithoutOne._.inverseˡ
d_inverse'737'_2132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
d_inverse'737'_2132 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_inverse'737'_2132 v8
du_inverse'737'_2132 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
du_inverse'737'_2132 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    coe du_inverse'737'_950 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsRingWithoutOne._.isEquivalence
d_isEquivalence_2134 ::
  T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2134 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_902
               (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0))))))
-- Algebra.Structures.IsRingWithoutOne._.isPartialEquivalence
d_isPartialEquivalence_2136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2136 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_2136 v8
du_isPartialEquivalence_2136 ::
  T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2136 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    let v5 = d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v5))
-- Algebra.Structures.IsRingWithoutOne._.refl
d_refl_2138 :: T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
d_refl_2138 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0)))))))
-- Algebra.Structures.IsRingWithoutOne._.reflexive
d_reflexive_2140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2140 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_2140 v8
du_reflexive_2140 ::
  T_IsRingWithoutOne_2056 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2140 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    let v5 = d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v5)) v6
-- Algebra.Structures.IsRingWithoutOne._.setoid
d_setoid_2142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2142 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_2142 v8
du_setoid_2142 ::
  T_IsRingWithoutOne_2056 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2142 v0
  = let v1 = d_'43''45'isAbelianGroup_2076 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsRingWithoutOne._.sym
d_sym_2144 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2144 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0)))))))
-- Algebra.Structures.IsRingWithoutOne._.trans
d_trans_2146 ::
  T_IsRingWithoutOne_2056 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2146 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0)))))))
-- Algebra.Structures.IsRingWithoutOne._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_2148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_2148 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 v8
  = du_unique'691''45''8315''185'_2148 v4 v6 v7 v8
du_unique'691''45''8315''185'_2148 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_2148 v0 v1 v2 v3
  = let v4 = d_'43''45'isAbelianGroup_2076 (coe v3) in
    coe
      du_unique'691''45''8315''185'_964 (coe v0) (coe v2) (coe v1)
      (coe d_isGroup_988 (coe v4))
-- Algebra.Structures.IsRingWithoutOne._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_2150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_2150 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 v8
  = du_unique'737''45''8315''185'_2150 v4 v6 v7 v8
du_unique'737''45''8315''185'_2150 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_2150 v0 v1 v2 v3
  = let v4 = d_'43''45'isAbelianGroup_2076 (coe v3) in
    coe
      du_unique'737''45''8315''185'_958 (coe v0) (coe v2) (coe v1)
      (coe d_isGroup_988 (coe v4))
-- Algebra.Structures.IsRingWithoutOne.*-isMagma
d_'42''45'isMagma_2152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> T_IsMagma_140
d_'42''45'isMagma_2152 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isMagma_2152 v8
du_'42''45'isMagma_2152 :: T_IsRingWithoutOne_2056 -> T_IsMagma_140
du_'42''45'isMagma_2152 v0
  = coe
      C_IsMagma'46'constructor_769
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2076 (coe v0)))))))
      (coe d_'42''45'cong_2078 (coe v0))
-- Algebra.Structures.IsRingWithoutOne.zeroˡ
d_zero'737'_2154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
d_zero'737'_2154 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_zero'737'_2154 v8
du_zero'737'_2154 :: T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
du_zero'737'_2154 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe d_zero_2084 (coe v0))
-- Algebra.Structures.IsRingWithoutOne.zeroʳ
d_zero'691'_2156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
d_zero'691'_2156 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_zero'691'_2156 v8
du_zero'691'_2156 :: T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny
du_zero'691'_2156 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe d_zero_2084 (coe v0))
-- Algebra.Structures.IsRingWithoutOne.distribˡ
d_distrib'737'_2158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_2158 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'737'_2158 v8
du_distrib'737'_2158 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_2158 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_distrib_2082 (coe v0))
-- Algebra.Structures.IsRingWithoutOne.distribʳ
d_distrib'691'_2160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_2160 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_distrib'691'_2160 v8
du_distrib'691'_2160 ::
  T_IsRingWithoutOne_2056 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_2160 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_distrib_2082 (coe v0))
-- Algebra.Structures.IsRingWithoutOne.*-isSemigroup
d_'42''45'isSemigroup_2162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRingWithoutOne_2056 -> T_IsSemigroup_436
d_'42''45'isSemigroup_2162 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'42''45'isSemigroup_2162 v8
du_'42''45'isSemigroup_2162 ::
  T_IsRingWithoutOne_2056 -> T_IsSemigroup_436
du_'42''45'isSemigroup_2162 v0
  = coe
      C_IsSemigroup'46'constructor_9303
      (coe du_'42''45'isMagma_2152 (coe v0))
      (coe d_'42''45'assoc_2080 (coe v0))
-- Algebra.Structures.IsRingWithoutOne._.∙-congʳ
d_'8729''45'cong'691'_2166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2166 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_2166 v8
du_'8729''45'cong'691'_2166 ::
  T_IsRingWithoutOne_2056 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2166 v0
  = coe
      du_'8729''45'cong'691'_170 (coe du_'42''45'isMagma_2152 (coe v0))
-- Algebra.Structures.IsRingWithoutOne._.∙-congˡ
d_'8729''45'cong'737'_2168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRingWithoutOne_2056 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2168 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_2168 v8
du_'8729''45'cong'737'_2168 ::
  T_IsRingWithoutOne_2056 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2168 v0
  = coe
      du_'8729''45'cong'737'_166 (coe du_'42''45'isMagma_2152 (coe v0))
-- Algebra.Structures.IsNonAssociativeRing
d_IsNonAssociativeRing_2180 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsNonAssociativeRing_2180
  = C_IsNonAssociativeRing'46'constructor_72021 T_IsAbelianGroup_976
                                                (AgdaAny ->
                                                 AgdaAny ->
                                                 AgdaAny ->
                                                 AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                                MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                                MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                                MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsNonAssociativeRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_2202 ::
  T_IsNonAssociativeRing_2180 -> T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_2202 v0
  = case coe v0 of
      C_IsNonAssociativeRing'46'constructor_72021 v1 v2 v3 v4 v5
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNonAssociativeRing.*-cong
d_'42''45'cong_2204 ::
  T_IsNonAssociativeRing_2180 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2204 v0
  = case coe v0 of
      C_IsNonAssociativeRing'46'constructor_72021 v1 v2 v3 v4 v5
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNonAssociativeRing.identity
d_identity_2206 ::
  T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2206 v0
  = case coe v0 of
      C_IsNonAssociativeRing'46'constructor_72021 v1 v2 v3 v4 v5
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNonAssociativeRing.distrib
d_distrib_2208 ::
  T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2208 v0
  = case coe v0 of
      C_IsNonAssociativeRing'46'constructor_72021 v1 v2 v3 v4 v5
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNonAssociativeRing.zero
d_zero_2210 ::
  T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2210 v0
  = case coe v0 of
      C_IsNonAssociativeRing'46'constructor_72021 v1 v2 v3 v4 v5
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNonAssociativeRing._._-_
d__'45'__2214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__2214 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 ~v7 ~v8 ~v9
  = du__'45'__2214 v4 v6
du__'45'__2214 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__2214 v0 v1 = coe du__'45'__944 (coe v0) (coe v1)
-- Algebra.Structures.IsNonAssociativeRing._.assoc
d_assoc_2216 ::
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2216 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_902
            (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0)))))
-- Algebra.Structures.IsNonAssociativeRing._.comm
d_comm_2218 ::
  T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2218 v0
  = coe d_comm_990 (coe d_'43''45'isAbelianGroup_2202 (coe v0))
-- Algebra.Structures.IsNonAssociativeRing._.∙-cong
d_'8729''45'cong_2220 ::
  T_IsNonAssociativeRing_2180 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2220 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_902
               (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0))))))
-- Algebra.Structures.IsNonAssociativeRing._.∙-congʳ
d_'8729''45'cong'691'_2222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2222 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_2222 v9
du_'8729''45'cong'691'_2222 ::
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2222 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsNonAssociativeRing._.∙-congˡ
d_'8729''45'cong'737'_2224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2224 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_2224 v9
du_'8729''45'cong'737'_2224 ::
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2224 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsNonAssociativeRing._.identity
d_identity_2226 ::
  T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2226 v0
  = coe
      d_identity_612
      (coe
         d_isMonoid_902
         (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0))))
-- Algebra.Structures.IsNonAssociativeRing._.identityʳ
d_identity'691'_2228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
d_identity'691'_2228 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_2228 v9
du_identity'691'_2228 ::
  T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
du_identity'691'_2228 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    coe du_identity'691'_642 (coe d_isMonoid_902 (coe v2))
-- Algebra.Structures.IsNonAssociativeRing._.identityˡ
d_identity'737'_2230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
d_identity'737'_2230 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_2230 v9
du_identity'737'_2230 ::
  T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
du_identity'737'_2230 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    coe du_identity'737'_640 (coe d_isMonoid_902 (coe v2))
-- Algebra.Structures.IsNonAssociativeRing._.isCommutativeMagma
d_isCommutativeMagma_2232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsNonAssociativeRing_2180 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_2232 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMagma_2232 v9
du_isCommutativeMagma_2232 ::
  T_IsNonAssociativeRing_2180 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_2232 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    let v2 = coe du_isCommutativeMonoid_1048 (coe v1) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v2))
-- Algebra.Structures.IsNonAssociativeRing._.isCommutativeMonoid
d_isCommutativeMonoid_2234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsNonAssociativeRing_2180 -> T_IsCommutativeMonoid_650
d_isCommutativeMonoid_2234 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMonoid_2234 v9
du_isCommutativeMonoid_2234 ::
  T_IsNonAssociativeRing_2180 -> T_IsCommutativeMonoid_650
du_isCommutativeMonoid_2234 v0
  = coe
      du_isCommutativeMonoid_1048
      (coe d_'43''45'isAbelianGroup_2202 (coe v0))
-- Algebra.Structures.IsNonAssociativeRing._.isCommutativeSemigroup
d_isCommutativeSemigroup_2236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_2236 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                              v9
  = du_isCommutativeSemigroup_2236 v9
du_isCommutativeSemigroup_2236 ::
  T_IsNonAssociativeRing_2180 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_2236 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    coe
      du_isCommutativeSemigroup_700
      (coe du_isCommutativeMonoid_1048 (coe v1))
-- Algebra.Structures.IsNonAssociativeRing._.isGroup
d_isGroup_2238 :: T_IsNonAssociativeRing_2180 -> T_IsGroup_888
d_isGroup_2238 v0
  = coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0))
-- Algebra.Structures.IsNonAssociativeRing._.isInvertibleMagma
d_isInvertibleMagma_2240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsNonAssociativeRing_2180 -> T_IsInvertibleMagma_776
d_isInvertibleMagma_2240 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isInvertibleMagma_2240 v9
du_isInvertibleMagma_2240 ::
  T_IsNonAssociativeRing_2180 -> T_IsInvertibleMagma_776
du_isInvertibleMagma_2240 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    coe du_isInvertibleMagma_966 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsNonAssociativeRing._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_2242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 -> T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_2242 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               v9
  = du_isInvertibleUnitalMagma_2242 v9
du_isInvertibleUnitalMagma_2242 ::
  T_IsNonAssociativeRing_2180 -> T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_2242 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    coe du_isInvertibleUnitalMagma_968 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsNonAssociativeRing._.isMagma
d_isMagma_2244 :: T_IsNonAssociativeRing_2180 -> T_IsMagma_140
d_isMagma_2244 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_902
            (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0)))))
-- Algebra.Structures.IsNonAssociativeRing._.isMonoid
d_isMonoid_2246 :: T_IsNonAssociativeRing_2180 -> T_IsMonoid_600
d_isMonoid_2246 v0
  = coe
      d_isMonoid_902
      (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0)))
-- Algebra.Structures.IsNonAssociativeRing._.isSemigroup
d_isSemigroup_2248 ::
  T_IsNonAssociativeRing_2180 -> T_IsSemigroup_436
d_isSemigroup_2248 v0
  = coe
      d_isSemigroup_610
      (coe
         d_isMonoid_902
         (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0))))
-- Algebra.Structures.IsNonAssociativeRing._.isUnitalMagma
d_isUnitalMagma_2250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsNonAssociativeRing_2180 -> T_IsUnitalMagma_556
d_isUnitalMagma_2250 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isUnitalMagma_2250 v9
du_isUnitalMagma_2250 ::
  T_IsNonAssociativeRing_2180 -> T_IsUnitalMagma_556
du_isUnitalMagma_2250 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_902 (coe v2))
-- Algebra.Structures.IsNonAssociativeRing._.⁻¹-cong
d_'8315''185''45'cong_2252 ::
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_2252 v0
  = coe
      d_'8315''185''45'cong_906
      (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0)))
-- Algebra.Structures.IsNonAssociativeRing._.inverse
d_inverse_2254 ::
  T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_2254 v0
  = coe
      d_inverse_904
      (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0)))
-- Algebra.Structures.IsNonAssociativeRing._.inverseʳ
d_inverse'691'_2256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
d_inverse'691'_2256 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_inverse'691'_2256 v9
du_inverse'691'_2256 ::
  T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
du_inverse'691'_2256 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    coe du_inverse'691'_952 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsNonAssociativeRing._.inverseˡ
d_inverse'737'_2258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
d_inverse'737'_2258 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_inverse'737'_2258 v9
du_inverse'737'_2258 ::
  T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
du_inverse'737'_2258 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    coe du_inverse'737'_950 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsNonAssociativeRing._.isEquivalence
d_isEquivalence_2260 ::
  T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2260 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_902
               (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0))))))
-- Algebra.Structures.IsNonAssociativeRing._.isPartialEquivalence
d_isPartialEquivalence_2262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2262 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_2262 v9
du_isPartialEquivalence_2262 ::
  T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2262 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    let v5 = d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v5))
-- Algebra.Structures.IsNonAssociativeRing._.refl
d_refl_2264 :: T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
d_refl_2264 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0)))))))
-- Algebra.Structures.IsNonAssociativeRing._.reflexive
d_reflexive_2266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2266 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_2266 v9
du_reflexive_2266 ::
  T_IsNonAssociativeRing_2180 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2266 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    let v5 = d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v5)) v6
-- Algebra.Structures.IsNonAssociativeRing._.setoid
d_setoid_2268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2268 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_2268 v9
du_setoid_2268 ::
  T_IsNonAssociativeRing_2180 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2268 v0
  = let v1 = d_'43''45'isAbelianGroup_2202 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsNonAssociativeRing._.sym
d_sym_2270 ::
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2270 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0)))))))
-- Algebra.Structures.IsNonAssociativeRing._.trans
d_trans_2272 ::
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2272 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0)))))))
-- Algebra.Structures.IsNonAssociativeRing._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_2274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_2274 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 ~v8
                                  v9
  = du_unique'691''45''8315''185'_2274 v4 v6 v7 v9
du_unique'691''45''8315''185'_2274 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_2274 v0 v1 v2 v3
  = let v4 = d_'43''45'isAbelianGroup_2202 (coe v3) in
    coe
      du_unique'691''45''8315''185'_964 (coe v0) (coe v2) (coe v1)
      (coe d_isGroup_988 (coe v4))
-- Algebra.Structures.IsNonAssociativeRing._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_2276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_2276 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 ~v8
                                  v9
  = du_unique'737''45''8315''185'_2276 v4 v6 v7 v9
du_unique'737''45''8315''185'_2276 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsNonAssociativeRing_2180 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_2276 v0 v1 v2 v3
  = let v4 = d_'43''45'isAbelianGroup_2202 (coe v3) in
    coe
      du_unique'737''45''8315''185'_958 (coe v0) (coe v2) (coe v1)
      (coe d_isGroup_988 (coe v4))
-- Algebra.Structures.IsNonAssociativeRing.*-isMagma
d_'42''45'isMagma_2278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsNonAssociativeRing_2180 -> T_IsMagma_140
d_'42''45'isMagma_2278 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMagma_2278 v9
du_'42''45'isMagma_2278 ::
  T_IsNonAssociativeRing_2180 -> T_IsMagma_140
du_'42''45'isMagma_2278 v0
  = coe
      C_IsMagma'46'constructor_769
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2202 (coe v0)))))))
      (coe d_'42''45'cong_2204 (coe v0))
-- Algebra.Structures.IsNonAssociativeRing.*-identityˡ
d_'42''45'identity'737'_2280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
d_'42''45'identity'737'_2280 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'identity'737'_2280 v9
du_'42''45'identity'737'_2280 ::
  T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
du_'42''45'identity'737'_2280 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_identity_2206 (coe v0))
-- Algebra.Structures.IsNonAssociativeRing.*-identityʳ
d_'42''45'identity'691'_2282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
d_'42''45'identity'691'_2282 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'identity'691'_2282 v9
du_'42''45'identity'691'_2282 ::
  T_IsNonAssociativeRing_2180 -> AgdaAny -> AgdaAny
du_'42''45'identity'691'_2282 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_identity_2206 (coe v0))
-- Algebra.Structures.IsNearring
d_IsNearring_2294 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsNearring_2294
  = C_IsNearring'46'constructor_76801 T_IsQuasiring_1962
                                      MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                      (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsNearring.isQuasiring
d_isQuasiring_2312 :: T_IsNearring_2294 -> T_IsQuasiring_1962
d_isQuasiring_2312 v0
  = case coe v0 of
      C_IsNearring'46'constructor_76801 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNearring.+-inverse
d_'43''45'inverse_2314 ::
  T_IsNearring_2294 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'43''45'inverse_2314 v0
  = case coe v0 of
      C_IsNearring'46'constructor_76801 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNearring.⁻¹-cong
d_'8315''185''45'cong_2316 ::
  T_IsNearring_2294 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_2316 v0
  = case coe v0 of
      C_IsNearring'46'constructor_76801 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsNearring._.*-assoc
d_'42''45'assoc_2320 ::
  T_IsNearring_2294 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_2320 v0
  = coe d_'42''45'assoc_1988 (coe d_isQuasiring_2312 (coe v0))
-- Algebra.Structures.IsNearring._.*-cong
d_'42''45'cong_2322 ::
  T_IsNearring_2294 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2322 v0
  = coe d_'42''45'cong_1986 (coe d_isQuasiring_2312 (coe v0))
-- Algebra.Structures.IsNearring._.∙-congʳ
d_'8729''45'cong'691'_2324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearring_2294 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2324 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_2324 v9
du_'8729''45'cong'691'_2324 ::
  T_IsNearring_2294 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2324 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    let v2 = coe du_'42''45'isMonoid_2036 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsNearring._.∙-congˡ
d_'8729''45'cong'737'_2326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearring_2294 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2326 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_2326 v9
du_'8729''45'cong'737'_2326 ::
  T_IsNearring_2294 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2326 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    let v2 = coe du_'42''45'isMonoid_2036 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsNearring._.*-identity
d_'42''45'identity_2328 ::
  T_IsNearring_2294 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2328 v0
  = coe d_'42''45'identity_1990 (coe d_isQuasiring_2312 (coe v0))
-- Algebra.Structures.IsNearring._.identityʳ
d_identity'691'_2330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsNearring_2294 -> AgdaAny -> AgdaAny
d_identity'691'_2330 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_2330 v9
du_identity'691'_2330 :: T_IsNearring_2294 -> AgdaAny -> AgdaAny
du_identity'691'_2330 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    coe du_identity'691'_642 (coe du_'42''45'isMonoid_2036 (coe v1))
-- Algebra.Structures.IsNearring._.identityˡ
d_identity'737'_2332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsNearring_2294 -> AgdaAny -> AgdaAny
d_identity'737'_2332 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_2332 v9
du_identity'737'_2332 :: T_IsNearring_2294 -> AgdaAny -> AgdaAny
du_identity'737'_2332 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    coe du_identity'737'_640 (coe du_'42''45'isMonoid_2036 (coe v1))
-- Algebra.Structures.IsNearring._.*-isMagma
d_'42''45'isMagma_2334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsNearring_2294 -> T_IsMagma_140
d_'42''45'isMagma_2334 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMagma_2334 v9
du_'42''45'isMagma_2334 :: T_IsNearring_2294 -> T_IsMagma_140
du_'42''45'isMagma_2334 v0
  = coe du_'42''45'isMagma_2032 (coe d_isQuasiring_2312 (coe v0))
-- Algebra.Structures.IsNearring._.*-isMonoid
d_'42''45'isMonoid_2336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsNearring_2294 -> T_IsMonoid_600
d_'42''45'isMonoid_2336 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMonoid_2336 v9
du_'42''45'isMonoid_2336 :: T_IsNearring_2294 -> T_IsMonoid_600
du_'42''45'isMonoid_2336 v0
  = coe du_'42''45'isMonoid_2036 (coe d_isQuasiring_2312 (coe v0))
-- Algebra.Structures.IsNearring._.*-isSemigroup
d_'42''45'isSemigroup_2338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsNearring_2294 -> T_IsSemigroup_436
d_'42''45'isSemigroup_2338 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isSemigroup_2338 v9
du_'42''45'isSemigroup_2338 ::
  T_IsNearring_2294 -> T_IsSemigroup_436
du_'42''45'isSemigroup_2338 v0
  = coe du_'42''45'isSemigroup_2034 (coe d_isQuasiring_2312 (coe v0))
-- Algebra.Structures.IsNearring._.assoc
d_assoc_2340 ::
  T_IsNearring_2294 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2340 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe d_'43''45'isMonoid_1984 (coe d_isQuasiring_2312 (coe v0))))
-- Algebra.Structures.IsNearring._.∙-cong
d_'8729''45'cong_2342 ::
  T_IsNearring_2294 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2342 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe d_'43''45'isMonoid_1984 (coe d_isQuasiring_2312 (coe v0)))))
-- Algebra.Structures.IsNearring._.∙-congʳ
d_'8729''45'cong'691'_2344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearring_2294 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2344 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_2344 v9
du_'8729''45'cong'691'_2344 ::
  T_IsNearring_2294 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2344 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    let v2 = d_'43''45'isMonoid_1984 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsNearring._.∙-congˡ
d_'8729''45'cong'737'_2346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearring_2294 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2346 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_2346 v9
du_'8729''45'cong'737'_2346 ::
  T_IsNearring_2294 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2346 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    let v2 = d_'43''45'isMonoid_1984 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsNearring._.identity
d_identity_2348 ::
  T_IsNearring_2294 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2348 v0
  = coe
      d_identity_612
      (coe d_'43''45'isMonoid_1984 (coe d_isQuasiring_2312 (coe v0)))
-- Algebra.Structures.IsNearring._.identityʳ
d_identity'691'_2350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsNearring_2294 -> AgdaAny -> AgdaAny
d_identity'691'_2350 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_2350 v9
du_identity'691'_2350 :: T_IsNearring_2294 -> AgdaAny -> AgdaAny
du_identity'691'_2350 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    coe du_identity'691'_642 (coe d_'43''45'isMonoid_1984 (coe v1))
-- Algebra.Structures.IsNearring._.identityˡ
d_identity'737'_2352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsNearring_2294 -> AgdaAny -> AgdaAny
d_identity'737'_2352 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_2352 v9
du_identity'737'_2352 :: T_IsNearring_2294 -> AgdaAny -> AgdaAny
du_identity'737'_2352 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    coe du_identity'737'_640 (coe d_'43''45'isMonoid_1984 (coe v1))
-- Algebra.Structures.IsNearring._.isMagma
d_isMagma_2354 :: T_IsNearring_2294 -> T_IsMagma_140
d_isMagma_2354 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe d_'43''45'isMonoid_1984 (coe d_isQuasiring_2312 (coe v0))))
-- Algebra.Structures.IsNearring._.+-isMonoid
d_'43''45'isMonoid_2356 :: T_IsNearring_2294 -> T_IsMonoid_600
d_'43''45'isMonoid_2356 v0
  = coe d_'43''45'isMonoid_1984 (coe d_isQuasiring_2312 (coe v0))
-- Algebra.Structures.IsNearring._.isSemigroup
d_isSemigroup_2358 :: T_IsNearring_2294 -> T_IsSemigroup_436
d_isSemigroup_2358 v0
  = coe
      d_isSemigroup_610
      (coe d_'43''45'isMonoid_1984 (coe d_isQuasiring_2312 (coe v0)))
-- Algebra.Structures.IsNearring._.isUnitalMagma
d_isUnitalMagma_2360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsNearring_2294 -> T_IsUnitalMagma_556
d_isUnitalMagma_2360 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isUnitalMagma_2360 v9
du_isUnitalMagma_2360 :: T_IsNearring_2294 -> T_IsUnitalMagma_556
du_isUnitalMagma_2360 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    coe du_isUnitalMagma_644 (coe d_'43''45'isMonoid_1984 (coe v1))
-- Algebra.Structures.IsNearring._.distrib
d_distrib_2362 ::
  T_IsNearring_2294 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2362 v0
  = coe d_distrib_1992 (coe d_isQuasiring_2312 (coe v0))
-- Algebra.Structures.IsNearring._.isEquivalence
d_isEquivalence_2364 ::
  T_IsNearring_2294 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2364 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe d_'43''45'isMonoid_1984 (coe d_isQuasiring_2312 (coe v0)))))
-- Algebra.Structures.IsNearring._.isPartialEquivalence
d_isPartialEquivalence_2366 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearring_2294 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2366 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_2366 v9
du_isPartialEquivalence_2366 ::
  T_IsNearring_2294 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2366 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    let v2 = d_'43''45'isMonoid_1984 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    let v4 = d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v4))
-- Algebra.Structures.IsNearring._.refl
d_refl_2368 :: T_IsNearring_2294 -> AgdaAny -> AgdaAny
d_refl_2368 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe d_'43''45'isMonoid_1984 (coe d_isQuasiring_2312 (coe v0))))))
-- Algebra.Structures.IsNearring._.reflexive
d_reflexive_2370 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearring_2294 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2370 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_2370 v9
du_reflexive_2370 ::
  T_IsNearring_2294 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2370 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    let v2 = d_'43''45'isMonoid_1984 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    let v4 = d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v4)) v5
-- Algebra.Structures.IsNearring._.setoid
d_setoid_2372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  T_IsNearring_2294 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2372 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_2372 v9
du_setoid_2372 ::
  T_IsNearring_2294 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2372 v0
  = let v1 = d_isQuasiring_2312 (coe v0) in
    let v2 = d_'43''45'isMonoid_1984 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsNearring._.sym
d_sym_2374 ::
  T_IsNearring_2294 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2374 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe d_'43''45'isMonoid_1984 (coe d_isQuasiring_2312 (coe v0))))))
-- Algebra.Structures.IsNearring._.trans
d_trans_2376 ::
  T_IsNearring_2294 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2376 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe d_'43''45'isMonoid_1984 (coe d_isQuasiring_2312 (coe v0))))))
-- Algebra.Structures.IsNearring._.zero
d_zero_2378 ::
  T_IsNearring_2294 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2378 v0 = coe d_zero_1994 (coe d_isQuasiring_2312 (coe v0))
-- Algebra.Structures.IsNearring.+-inverseˡ
d_'43''45'inverse'737'_2380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsNearring_2294 -> AgdaAny -> AgdaAny
d_'43''45'inverse'737'_2380 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'43''45'inverse'737'_2380 v9
du_'43''45'inverse'737'_2380 ::
  T_IsNearring_2294 -> AgdaAny -> AgdaAny
du_'43''45'inverse'737'_2380 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_'43''45'inverse_2314 (coe v0))
-- Algebra.Structures.IsNearring.+-inverseʳ
d_'43''45'inverse'691'_2382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> T_IsNearring_2294 -> AgdaAny -> AgdaAny
d_'43''45'inverse'691'_2382 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'43''45'inverse'691'_2382 v9
du_'43''45'inverse'691'_2382 ::
  T_IsNearring_2294 -> AgdaAny -> AgdaAny
du_'43''45'inverse'691'_2382 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_'43''45'inverse_2314 (coe v0))
-- Algebra.Structures.IsRing
d_IsRing_2394 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsRing_2394
  = C_IsRing'46'constructor_80853 T_IsAbelianGroup_976
                                  (AgdaAny ->
                                   AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsRing.+-isAbelianGroup
d_'43''45'isAbelianGroup_2418 ::
  T_IsRing_2394 -> T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_2418 v0
  = case coe v0 of
      C_IsRing'46'constructor_80853 v1 v2 v3 v4 v5 v6 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRing.*-cong
d_'42''45'cong_2420 ::
  T_IsRing_2394 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2420 v0
  = case coe v0 of
      C_IsRing'46'constructor_80853 v1 v2 v3 v4 v5 v6 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRing.*-assoc
d_'42''45'assoc_2422 ::
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_2422 v0
  = case coe v0 of
      C_IsRing'46'constructor_80853 v1 v2 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRing.*-identity
d_'42''45'identity_2424 ::
  T_IsRing_2394 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2424 v0
  = case coe v0 of
      C_IsRing'46'constructor_80853 v1 v2 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRing.distrib
d_distrib_2426 ::
  T_IsRing_2394 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2426 v0
  = case coe v0 of
      C_IsRing'46'constructor_80853 v1 v2 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRing.zero
d_zero_2428 ::
  T_IsRing_2394 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2428 v0
  = case coe v0 of
      C_IsRing'46'constructor_80853 v1 v2 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRing._._-_
d__'45'__2432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__2432 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 ~v7 ~v8 ~v9
  = du__'45'__2432 v4 v6
du__'45'__2432 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__2432 v0 v1 = coe du__'45'__944 (coe v0) (coe v1)
-- Algebra.Structures.IsRing._.assoc
d_assoc_2434 ::
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2434 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_902
            (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0)))))
-- Algebra.Structures.IsRing._.comm
d_comm_2436 :: T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2436 v0
  = coe d_comm_990 (coe d_'43''45'isAbelianGroup_2418 (coe v0))
-- Algebra.Structures.IsRing._.∙-cong
d_'8729''45'cong_2438 ::
  T_IsRing_2394 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2438 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_902
               (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0))))))
-- Algebra.Structures.IsRing._.∙-congʳ
d_'8729''45'cong'691'_2440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2440 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_2440 v9
du_'8729''45'cong'691'_2440 ::
  T_IsRing_2394 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2440 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsRing._.∙-congˡ
d_'8729''45'cong'737'_2442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2442 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_2442 v9
du_'8729''45'cong'737'_2442 ::
  T_IsRing_2394 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2442 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsRing._.identity
d_identity_2444 ::
  T_IsRing_2394 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2444 v0
  = coe
      d_identity_612
      (coe
         d_isMonoid_902
         (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0))))
-- Algebra.Structures.IsRing._.identityʳ
d_identity'691'_2446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> AgdaAny -> AgdaAny
d_identity'691'_2446 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_2446 v9
du_identity'691'_2446 :: T_IsRing_2394 -> AgdaAny -> AgdaAny
du_identity'691'_2446 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    coe du_identity'691'_642 (coe d_isMonoid_902 (coe v2))
-- Algebra.Structures.IsRing._.identityˡ
d_identity'737'_2448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> AgdaAny -> AgdaAny
d_identity'737'_2448 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_2448 v9
du_identity'737'_2448 :: T_IsRing_2394 -> AgdaAny -> AgdaAny
du_identity'737'_2448 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    coe du_identity'737'_640 (coe d_isMonoid_902 (coe v2))
-- Algebra.Structures.IsRing._.isCommutativeMagma
d_isCommutativeMagma_2450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_2450 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMagma_2450 v9
du_isCommutativeMagma_2450 ::
  T_IsRing_2394 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_2450 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    let v2 = coe du_isCommutativeMonoid_1048 (coe v1) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v2))
-- Algebra.Structures.IsRing._.isCommutativeMonoid
d_isCommutativeMonoid_2452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsCommutativeMonoid_650
d_isCommutativeMonoid_2452 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMonoid_2452 v9
du_isCommutativeMonoid_2452 ::
  T_IsRing_2394 -> T_IsCommutativeMonoid_650
du_isCommutativeMonoid_2452 v0
  = coe
      du_isCommutativeMonoid_1048
      (coe d_'43''45'isAbelianGroup_2418 (coe v0))
-- Algebra.Structures.IsRing._.isCommutativeSemigroup
d_isCommutativeSemigroup_2454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_2454 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                              v9
  = du_isCommutativeSemigroup_2454 v9
du_isCommutativeSemigroup_2454 ::
  T_IsRing_2394 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_2454 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    coe
      du_isCommutativeSemigroup_700
      (coe du_isCommutativeMonoid_1048 (coe v1))
-- Algebra.Structures.IsRing._.isGroup
d_isGroup_2456 :: T_IsRing_2394 -> T_IsGroup_888
d_isGroup_2456 v0
  = coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0))
-- Algebra.Structures.IsRing._.isInvertibleMagma
d_isInvertibleMagma_2458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsInvertibleMagma_776
d_isInvertibleMagma_2458 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isInvertibleMagma_2458 v9
du_isInvertibleMagma_2458 ::
  T_IsRing_2394 -> T_IsInvertibleMagma_776
du_isInvertibleMagma_2458 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    coe du_isInvertibleMagma_966 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsRing._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_2460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsRing_2394 -> T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_2460 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               v9
  = du_isInvertibleUnitalMagma_2460 v9
du_isInvertibleUnitalMagma_2460 ::
  T_IsRing_2394 -> T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_2460 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    coe du_isInvertibleUnitalMagma_968 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsRing._.isMagma
d_isMagma_2462 :: T_IsRing_2394 -> T_IsMagma_140
d_isMagma_2462 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_902
            (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0)))))
-- Algebra.Structures.IsRing._.isMonoid
d_isMonoid_2464 :: T_IsRing_2394 -> T_IsMonoid_600
d_isMonoid_2464 v0
  = coe
      d_isMonoid_902
      (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0)))
-- Algebra.Structures.IsRing._.isSemigroup
d_isSemigroup_2466 :: T_IsRing_2394 -> T_IsSemigroup_436
d_isSemigroup_2466 v0
  = coe
      d_isSemigroup_610
      (coe
         d_isMonoid_902
         (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0))))
-- Algebra.Structures.IsRing._.isUnitalMagma
d_isUnitalMagma_2468 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsUnitalMagma_556
d_isUnitalMagma_2468 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isUnitalMagma_2468 v9
du_isUnitalMagma_2468 :: T_IsRing_2394 -> T_IsUnitalMagma_556
du_isUnitalMagma_2468 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_902 (coe v2))
-- Algebra.Structures.IsRing._.⁻¹-cong
d_'8315''185''45'cong_2470 ::
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_2470 v0
  = coe
      d_'8315''185''45'cong_906
      (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0)))
-- Algebra.Structures.IsRing._.inverse
d_inverse_2472 ::
  T_IsRing_2394 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_2472 v0
  = coe
      d_inverse_904
      (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0)))
-- Algebra.Structures.IsRing._.inverseʳ
d_inverse'691'_2474 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> AgdaAny -> AgdaAny
d_inverse'691'_2474 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_inverse'691'_2474 v9
du_inverse'691'_2474 :: T_IsRing_2394 -> AgdaAny -> AgdaAny
du_inverse'691'_2474 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    coe du_inverse'691'_952 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsRing._.inverseˡ
d_inverse'737'_2476 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> AgdaAny -> AgdaAny
d_inverse'737'_2476 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_inverse'737'_2476 v9
du_inverse'737'_2476 :: T_IsRing_2394 -> AgdaAny -> AgdaAny
du_inverse'737'_2476 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    coe du_inverse'737'_950 (coe d_isGroup_988 (coe v1))
-- Algebra.Structures.IsRing._.isEquivalence
d_isEquivalence_2478 ::
  T_IsRing_2394 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2478 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_902
               (coe d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0))))))
-- Algebra.Structures.IsRing._.isPartialEquivalence
d_isPartialEquivalence_2480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2480 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_2480 v9
du_isPartialEquivalence_2480 ::
  T_IsRing_2394 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2480 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    let v5 = d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v5))
-- Algebra.Structures.IsRing._.refl
d_refl_2482 :: T_IsRing_2394 -> AgdaAny -> AgdaAny
d_refl_2482 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0)))))))
-- Algebra.Structures.IsRing._.reflexive
d_reflexive_2484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2484 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_2484 v9
du_reflexive_2484 ::
  T_IsRing_2394 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2484 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    let v5 = d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v5)) v6
-- Algebra.Structures.IsRing._.setoid
d_setoid_2486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2486 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_2486 v9
du_setoid_2486 ::
  T_IsRing_2394 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2486 v0
  = let v1 = d_'43''45'isAbelianGroup_2418 (coe v0) in
    let v2 = d_isGroup_988 (coe v1) in
    let v3 = d_isMonoid_902 (coe v2) in
    let v4 = d_isSemigroup_610 (coe v3) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v4))
-- Algebra.Structures.IsRing._.sym
d_sym_2488 ::
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2488 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0)))))))
-- Algebra.Structures.IsRing._.trans
d_trans_2490 ::
  T_IsRing_2394 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2490 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0)))))))
-- Algebra.Structures.IsRing._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_2492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_2492 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 ~v8
                                  v9
  = du_unique'691''45''8315''185'_2492 v4 v6 v7 v9
du_unique'691''45''8315''185'_2492 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_2492 v0 v1 v2 v3
  = let v4 = d_'43''45'isAbelianGroup_2418 (coe v3) in
    coe
      du_unique'691''45''8315''185'_964 (coe v0) (coe v2) (coe v1)
      (coe d_isGroup_988 (coe v4))
-- Algebra.Structures.IsRing._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_2494 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_2494 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 ~v8
                                  v9
  = du_unique'737''45''8315''185'_2494 v4 v6 v7 v9
du_unique'737''45''8315''185'_2494 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_2494 v0 v1 v2 v3
  = let v4 = d_'43''45'isAbelianGroup_2418 (coe v3) in
    coe
      du_unique'737''45''8315''185'_958 (coe v0) (coe v2) (coe v1)
      (coe d_isGroup_988 (coe v4))
-- Algebra.Structures.IsRing.*-isMagma
d_'42''45'isMagma_2496 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsMagma_140
d_'42''45'isMagma_2496 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMagma_2496 v9
du_'42''45'isMagma_2496 :: T_IsRing_2394 -> T_IsMagma_140
du_'42''45'isMagma_2496 v0
  = coe
      C_IsMagma'46'constructor_769
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988 (coe d_'43''45'isAbelianGroup_2418 (coe v0)))))))
      (coe d_'42''45'cong_2420 (coe v0))
-- Algebra.Structures.IsRing.*-isSemigroup
d_'42''45'isSemigroup_2498 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsSemigroup_436
d_'42''45'isSemigroup_2498 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isSemigroup_2498 v9
du_'42''45'isSemigroup_2498 :: T_IsRing_2394 -> T_IsSemigroup_436
du_'42''45'isSemigroup_2498 v0
  = coe
      C_IsSemigroup'46'constructor_9303
      (coe du_'42''45'isMagma_2496 (coe v0))
      (coe d_'42''45'assoc_2422 (coe v0))
-- Algebra.Structures.IsRing.*-isMonoid
d_'42''45'isMonoid_2500 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsMonoid_600
d_'42''45'isMonoid_2500 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMonoid_2500 v9
du_'42''45'isMonoid_2500 :: T_IsRing_2394 -> T_IsMonoid_600
du_'42''45'isMonoid_2500 v0
  = coe
      C_IsMonoid'46'constructor_13559
      (coe du_'42''45'isSemigroup_2498 (coe v0))
      (coe d_'42''45'identity_2424 (coe v0))
-- Algebra.Structures.IsRing._.∙-congʳ
d_'8729''45'cong'691'_2504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2504 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_2504 v9
du_'8729''45'cong'691'_2504 ::
  T_IsRing_2394 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2504 v0
  = let v1 = coe du_'42''45'isMonoid_2500 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsRing._.∙-congˡ
d_'8729''45'cong'737'_2506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2506 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_2506 v9
du_'8729''45'cong'737'_2506 ::
  T_IsRing_2394 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2506 v0
  = let v1 = coe du_'42''45'isMonoid_2500 (coe v0) in
    let v2 = d_isSemigroup_610 (coe v1) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v2))
-- Algebra.Structures.IsRing._.identityʳ
d_identity'691'_2508 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> AgdaAny -> AgdaAny
d_identity'691'_2508 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_2508 v9
du_identity'691'_2508 :: T_IsRing_2394 -> AgdaAny -> AgdaAny
du_identity'691'_2508 v0
  = coe du_identity'691'_642 (coe du_'42''45'isMonoid_2500 (coe v0))
-- Algebra.Structures.IsRing._.identityˡ
d_identity'737'_2510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> AgdaAny -> AgdaAny
d_identity'737'_2510 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_2510 v9
du_identity'737'_2510 :: T_IsRing_2394 -> AgdaAny -> AgdaAny
du_identity'737'_2510 v0
  = coe du_identity'737'_640 (coe du_'42''45'isMonoid_2500 (coe v0))
-- Algebra.Structures.IsRing.zeroˡ
d_zero'737'_2512 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> AgdaAny -> AgdaAny
d_zero'737'_2512 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_zero'737'_2512 v9
du_zero'737'_2512 :: T_IsRing_2394 -> AgdaAny -> AgdaAny
du_zero'737'_2512 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe d_zero_2428 (coe v0))
-- Algebra.Structures.IsRing.zeroʳ
d_zero'691'_2514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> AgdaAny -> AgdaAny
d_zero'691'_2514 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_zero'691'_2514 v9
du_zero'691'_2514 :: T_IsRing_2394 -> AgdaAny -> AgdaAny
du_zero'691'_2514 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe d_zero_2428 (coe v0))
-- Algebra.Structures.IsRing.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 -> T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2516 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
                                         ~v6 ~v7 ~v8 v9
  = du_isSemiringWithoutAnnihilatingZero_2516 v9
du_isSemiringWithoutAnnihilatingZero_2516 ::
  T_IsRing_2394 -> T_IsSemiringWithoutAnnihilatingZero_1270
du_isSemiringWithoutAnnihilatingZero_2516 v0
  = coe
      C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
      (coe
         du_isCommutativeMonoid_1048
         (coe d_'43''45'isAbelianGroup_2418 (coe v0)))
      (coe d_'42''45'cong_2420 (coe v0))
      (coe d_'42''45'assoc_2422 (coe v0))
      (coe d_'42''45'identity_2424 (coe v0))
      (coe d_distrib_2426 (coe v0))
-- Algebra.Structures.IsRing.isSemiring
d_isSemiring_2518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsSemiring_1372
d_isSemiring_2518 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isSemiring_2518 v9
du_isSemiring_2518 :: T_IsRing_2394 -> T_IsSemiring_1372
du_isSemiring_2518 v0
  = coe
      C_IsSemiring'46'constructor_42303
      (coe du_isSemiringWithoutAnnihilatingZero_2516 (coe v0))
      (coe d_zero_2428 (coe v0))
-- Algebra.Structures.IsRing._.distribʳ
d_distrib'691'_2522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_2522 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_distrib'691'_2522 v9
du_distrib'691'_2522 ::
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_2522 v0
  = let v1 = coe du_isSemiring_2518 (coe v0) in
    coe
      du_distrib'691'_1302
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsRing._.distribˡ
d_distrib'737'_2524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_2524 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_distrib'737'_2524 v9
du_distrib'737'_2524 ::
  T_IsRing_2394 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_2524 v0
  = let v1 = coe du_isSemiring_2518 (coe v0) in
    coe
      du_distrib'737'_1300
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v1))
-- Algebra.Structures.IsRing._.isNearSemiring
d_isNearSemiring_2526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsNearSemiring_1062
d_isNearSemiring_2526 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isNearSemiring_2526 v9
du_isNearSemiring_2526 :: T_IsRing_2394 -> T_IsNearSemiring_1062
du_isNearSemiring_2526 v0
  = let v1 = coe du_isSemiring_2518 (coe v0) in
    coe
      du_isNearSemiring_1196 (coe du_isSemiringWithoutOne_1462 (coe v1))
-- Algebra.Structures.IsRing._.isSemiringWithoutOne
d_isSemiringWithoutOne_2528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsRing_2394 -> T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_2528 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isSemiringWithoutOne_2528 v9
du_isSemiringWithoutOne_2528 ::
  T_IsRing_2394 -> T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_2528 v0
  = coe
      du_isSemiringWithoutOne_1462 (coe du_isSemiring_2518 (coe v0))
-- Algebra.Structures.IsCommutativeRing
d_IsCommutativeRing_2540 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_IsCommutativeRing_2540
  = C_IsCommutativeRing'46'constructor_87819 T_IsRing_2394
                                             (AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsCommutativeRing.isRing
d_isRing_2556 :: T_IsCommutativeRing_2540 -> T_IsRing_2394
d_isRing_2556 v0
  = case coe v0 of
      C_IsCommutativeRing'46'constructor_87819 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeRing.*-comm
d_'42''45'comm_2558 ::
  T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'comm_2558 v0
  = case coe v0 of
      C_IsCommutativeRing'46'constructor_87819 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsCommutativeRing._._-_
d__'45'__2562 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny -> AgdaAny
d__'45'__2562 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 ~v7 ~v8 ~v9
  = du__'45'__2562 v4 v6
du__'45'__2562 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'45'__2562 v0 v1 = coe du__'45'__944 (coe v0) (coe v1)
-- Algebra.Structures.IsCommutativeRing._.*-assoc
d_'42''45'assoc_2564 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'assoc_2564 v0
  = coe d_'42''45'assoc_2422 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.*-cong
d_'42''45'cong_2566 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'42''45'cong_2566 v0
  = coe d_'42''45'cong_2420 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.∙-congʳ
d_'8729''45'cong'691'_2568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2568 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_2568 v9
du_'8729''45'cong'691'_2568 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2568 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = coe du_'42''45'isMonoid_2500 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsCommutativeRing._.∙-congˡ
d_'8729''45'cong'737'_2570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2570 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_2570 v9
du_'8729''45'cong'737'_2570 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2570 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = coe du_'42''45'isMonoid_2500 (coe v1) in
    let v3 = d_isSemigroup_610 (coe v2) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v3))
-- Algebra.Structures.IsCommutativeRing._.*-identity
d_'42''45'identity_2572 ::
  T_IsCommutativeRing_2540 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'42''45'identity_2572 v0
  = coe d_'42''45'identity_2424 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.identityʳ
d_identity'691'_2574 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
d_identity'691'_2574 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_2574 v9
du_identity'691'_2574 ::
  T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
du_identity'691'_2574 v0
  = let v1 = d_isRing_2556 (coe v0) in
    coe du_identity'691'_642 (coe du_'42''45'isMonoid_2500 (coe v1))
-- Algebra.Structures.IsCommutativeRing._.identityˡ
d_identity'737'_2576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
d_identity'737'_2576 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_2576 v9
du_identity'737'_2576 ::
  T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
du_identity'737'_2576 v0
  = let v1 = d_isRing_2556 (coe v0) in
    coe du_identity'737'_640 (coe du_'42''45'isMonoid_2500 (coe v1))
-- Algebra.Structures.IsCommutativeRing._.*-isMagma
d_'42''45'isMagma_2578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsCommutativeRing_2540 -> T_IsMagma_140
d_'42''45'isMagma_2578 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMagma_2578 v9
du_'42''45'isMagma_2578 ::
  T_IsCommutativeRing_2540 -> T_IsMagma_140
du_'42''45'isMagma_2578 v0
  = coe du_'42''45'isMagma_2496 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.*-isMonoid
d_'42''45'isMonoid_2580 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsCommutativeRing_2540 -> T_IsMonoid_600
d_'42''45'isMonoid_2580 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isMonoid_2580 v9
du_'42''45'isMonoid_2580 ::
  T_IsCommutativeRing_2540 -> T_IsMonoid_600
du_'42''45'isMonoid_2580 v0
  = coe du_'42''45'isMonoid_2500 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.*-isSemigroup
d_'42''45'isSemigroup_2582 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsCommutativeRing_2540 -> T_IsSemigroup_436
d_'42''45'isSemigroup_2582 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'42''45'isSemigroup_2582 v9
du_'42''45'isSemigroup_2582 ::
  T_IsCommutativeRing_2540 -> T_IsSemigroup_436
du_'42''45'isSemigroup_2582 v0
  = coe du_'42''45'isSemigroup_2498 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.assoc
d_assoc_2584 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_2584 v0
  = coe
      d_assoc_446
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_902
            (coe
               d_isGroup_988
               (coe d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0))))))
-- Algebra.Structures.IsCommutativeRing._.comm
d_comm_2586 ::
  T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_2586 v0
  = coe
      d_comm_990
      (coe d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0)))
-- Algebra.Structures.IsCommutativeRing._.∙-cong
d_'8729''45'cong_2588 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2588 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_902
               (coe
                  d_isGroup_988
                  (coe
                     d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0)))))))
-- Algebra.Structures.IsCommutativeRing._.∙-congʳ
d_'8729''45'cong'691'_2590 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2590 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'691'_2590 v9
du_'8729''45'cong'691'_2590 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2590 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    let v3 = d_isGroup_988 (coe v2) in
    let v4 = d_isMonoid_902 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsCommutativeRing._.∙-congˡ
d_'8729''45'cong'737'_2592 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2592 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_'8729''45'cong'737'_2592 v9
du_'8729''45'cong'737'_2592 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2592 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    let v3 = d_isGroup_988 (coe v2) in
    let v4 = d_isMonoid_902 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsCommutativeRing._.identity
d_identity_2594 ::
  T_IsCommutativeRing_2540 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2594 v0
  = coe
      d_identity_612
      (coe
         d_isMonoid_902
         (coe
            d_isGroup_988
            (coe d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0)))))
-- Algebra.Structures.IsCommutativeRing._.identityʳ
d_identity'691'_2596 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
d_identity'691'_2596 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'691'_2596 v9
du_identity'691'_2596 ::
  T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
du_identity'691'_2596 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    let v3 = d_isGroup_988 (coe v2) in
    coe du_identity'691'_642 (coe d_isMonoid_902 (coe v3))
-- Algebra.Structures.IsCommutativeRing._.identityˡ
d_identity'737'_2598 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
d_identity'737'_2598 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_identity'737'_2598 v9
du_identity'737'_2598 ::
  T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
du_identity'737'_2598 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    let v3 = d_isGroup_988 (coe v2) in
    coe du_identity'737'_640 (coe d_isMonoid_902 (coe v3))
-- Algebra.Structures.IsCommutativeRing._.+-isAbelianGroup
d_'43''45'isAbelianGroup_2600 ::
  T_IsCommutativeRing_2540 -> T_IsAbelianGroup_976
d_'43''45'isAbelianGroup_2600 v0
  = coe d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.isCommutativeMagma
d_isCommutativeMagma_2602 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_2602 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMagma_2602 v9
du_isCommutativeMagma_2602 ::
  T_IsCommutativeRing_2540 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_2602 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    let v3 = coe du_isCommutativeMonoid_1048 (coe v2) in
    coe
      du_isCommutativeMagma_550
      (coe du_isCommutativeSemigroup_700 (coe v3))
-- Algebra.Structures.IsCommutativeRing._.isCommutativeMonoid
d_isCommutativeMonoid_2604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsCommutativeMonoid_650
d_isCommutativeMonoid_2604 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMonoid_2604 v9
du_isCommutativeMonoid_2604 ::
  T_IsCommutativeRing_2540 -> T_IsCommutativeMonoid_650
du_isCommutativeMonoid_2604 v0
  = let v1 = d_isRing_2556 (coe v0) in
    coe
      du_isCommutativeMonoid_1048
      (coe d_'43''45'isAbelianGroup_2418 (coe v1))
-- Algebra.Structures.IsCommutativeRing._.isCommutativeSemigroup
d_isCommutativeSemigroup_2606 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_2606 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                              v9
  = du_isCommutativeSemigroup_2606 v9
du_isCommutativeSemigroup_2606 ::
  T_IsCommutativeRing_2540 -> T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_2606 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    coe
      du_isCommutativeSemigroup_700
      (coe du_isCommutativeMonoid_1048 (coe v2))
-- Algebra.Structures.IsCommutativeRing._.isGroup
d_isGroup_2608 :: T_IsCommutativeRing_2540 -> T_IsGroup_888
d_isGroup_2608 v0
  = coe
      d_isGroup_988
      (coe d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0)))
-- Algebra.Structures.IsCommutativeRing._.isInvertibleMagma
d_isInvertibleMagma_2610 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsInvertibleMagma_776
d_isInvertibleMagma_2610 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isInvertibleMagma_2610 v9
du_isInvertibleMagma_2610 ::
  T_IsCommutativeRing_2540 -> T_IsInvertibleMagma_776
du_isInvertibleMagma_2610 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    coe du_isInvertibleMagma_966 (coe d_isGroup_988 (coe v2))
-- Algebra.Structures.IsCommutativeRing._.isInvertibleUnitalMagma
d_isInvertibleUnitalMagma_2612 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 -> T_IsInvertibleUnitalMagma_828
d_isInvertibleUnitalMagma_2612 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                               v9
  = du_isInvertibleUnitalMagma_2612 v9
du_isInvertibleUnitalMagma_2612 ::
  T_IsCommutativeRing_2540 -> T_IsInvertibleUnitalMagma_828
du_isInvertibleUnitalMagma_2612 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    coe du_isInvertibleUnitalMagma_968 (coe d_isGroup_988 (coe v2))
-- Algebra.Structures.IsCommutativeRing._.isMagma
d_isMagma_2614 :: T_IsCommutativeRing_2540 -> T_IsMagma_140
d_isMagma_2614 v0
  = coe
      d_isMagma_444
      (coe
         d_isSemigroup_610
         (coe
            d_isMonoid_902
            (coe
               d_isGroup_988
               (coe d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0))))))
-- Algebra.Structures.IsCommutativeRing._.isMonoid
d_isMonoid_2616 :: T_IsCommutativeRing_2540 -> T_IsMonoid_600
d_isMonoid_2616 v0
  = coe
      d_isMonoid_902
      (coe
         d_isGroup_988
         (coe d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0))))
-- Algebra.Structures.IsCommutativeRing._.isSemigroup
d_isSemigroup_2618 :: T_IsCommutativeRing_2540 -> T_IsSemigroup_436
d_isSemigroup_2618 v0
  = coe
      d_isSemigroup_610
      (coe
         d_isMonoid_902
         (coe
            d_isGroup_988
            (coe d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0)))))
-- Algebra.Structures.IsCommutativeRing._.isUnitalMagma
d_isUnitalMagma_2620 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsUnitalMagma_556
d_isUnitalMagma_2620 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isUnitalMagma_2620 v9
du_isUnitalMagma_2620 ::
  T_IsCommutativeRing_2540 -> T_IsUnitalMagma_556
du_isUnitalMagma_2620 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    let v3 = d_isGroup_988 (coe v2) in
    coe du_isUnitalMagma_644 (coe d_isMonoid_902 (coe v3))
-- Algebra.Structures.IsCommutativeRing._.⁻¹-cong
d_'8315''185''45'cong_2622 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8315''185''45'cong_2622 v0
  = coe
      d_'8315''185''45'cong_906
      (coe
         d_isGroup_988
         (coe d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0))))
-- Algebra.Structures.IsCommutativeRing._.inverse
d_inverse_2624 ::
  T_IsCommutativeRing_2540 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse_2624 v0
  = coe
      d_inverse_904
      (coe
         d_isGroup_988
         (coe d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0))))
-- Algebra.Structures.IsCommutativeRing._.inverseʳ
d_inverse'691'_2626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
d_inverse'691'_2626 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_inverse'691'_2626 v9
du_inverse'691'_2626 ::
  T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
du_inverse'691'_2626 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    coe du_inverse'691'_952 (coe d_isGroup_988 (coe v2))
-- Algebra.Structures.IsCommutativeRing._.inverseˡ
d_inverse'737'_2628 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
d_inverse'737'_2628 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_inverse'737'_2628 v9
du_inverse'737'_2628 ::
  T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
du_inverse'737'_2628 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    coe du_inverse'737'_950 (coe d_isGroup_988 (coe v2))
-- Algebra.Structures.IsCommutativeRing._.distrib
d_distrib_2630 ::
  T_IsCommutativeRing_2540 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_distrib_2630 v0 = coe d_distrib_2426 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.distribʳ
d_distrib'691'_2632 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'691'_2632 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_distrib'691'_2632 v9
du_distrib'691'_2632 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'691'_2632 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = coe du_isSemiring_2518 (coe v1) in
    coe
      du_distrib'691'_1302
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsCommutativeRing._.distribˡ
d_distrib'737'_2634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib'737'_2634 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_distrib'737'_2634 v9
du_distrib'737'_2634 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib'737'_2634 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = coe du_isSemiring_2518 (coe v1) in
    coe
      du_distrib'737'_1300
      (coe d_isSemiringWithoutAnnihilatingZero_1386 (coe v2))
-- Algebra.Structures.IsCommutativeRing._.isEquivalence
d_isEquivalence_2636 ::
  T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2636 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_444
         (coe
            d_isSemigroup_610
            (coe
               d_isMonoid_902
               (coe
                  d_isGroup_988
                  (coe
                     d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0)))))))
-- Algebra.Structures.IsCommutativeRing._.isNearSemiring
d_isNearSemiring_2638 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsNearSemiring_1062
d_isNearSemiring_2638 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isNearSemiring_2638 v9
du_isNearSemiring_2638 ::
  T_IsCommutativeRing_2540 -> T_IsNearSemiring_1062
du_isNearSemiring_2638 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = coe du_isSemiring_2518 (coe v1) in
    coe
      du_isNearSemiring_1196 (coe du_isSemiringWithoutOne_1462 (coe v2))
-- Algebra.Structures.IsCommutativeRing._.isPartialEquivalence
d_isPartialEquivalence_2640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2640 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isPartialEquivalence_2640 v9
du_isPartialEquivalence_2640 ::
  T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2640 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    let v3 = d_isGroup_988 (coe v2) in
    let v4 = d_isMonoid_902 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    let v6 = d_isMagma_444 (coe v5) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v6))
-- Algebra.Structures.IsCommutativeRing._.isSemiring
d_isSemiring_2642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T_IsCommutativeRing_2540 -> T_IsSemiring_1372
d_isSemiring_2642 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isSemiring_2642 v9
du_isSemiring_2642 :: T_IsCommutativeRing_2540 -> T_IsSemiring_1372
du_isSemiring_2642 v0
  = coe du_isSemiring_2518 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.isSemiringWithoutAnnihilatingZero
d_isSemiringWithoutAnnihilatingZero_2644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  T_IsSemiringWithoutAnnihilatingZero_1270
d_isSemiringWithoutAnnihilatingZero_2644 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
                                         ~v6 ~v7 ~v8 v9
  = du_isSemiringWithoutAnnihilatingZero_2644 v9
du_isSemiringWithoutAnnihilatingZero_2644 ::
  T_IsCommutativeRing_2540 ->
  T_IsSemiringWithoutAnnihilatingZero_1270
du_isSemiringWithoutAnnihilatingZero_2644 v0
  = coe
      du_isSemiringWithoutAnnihilatingZero_2516
      (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.isSemiringWithoutOne
d_isSemiringWithoutOne_2646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsSemiringWithoutOne_1142
d_isSemiringWithoutOne_2646 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isSemiringWithoutOne_2646 v9
du_isSemiringWithoutOne_2646 ::
  T_IsCommutativeRing_2540 -> T_IsSemiringWithoutOne_1142
du_isSemiringWithoutOne_2646 v0
  = let v1 = d_isRing_2556 (coe v0) in
    coe du_isSemiringWithoutOne_1462 (coe du_isSemiring_2518 (coe v1))
-- Algebra.Structures.IsCommutativeRing._.refl
d_refl_2648 :: T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
d_refl_2648 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988
                     (coe
                        d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0))))))))
-- Algebra.Structures.IsCommutativeRing._.reflexive
d_reflexive_2650 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2650 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_reflexive_2650 v9
du_reflexive_2650 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2650 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    let v3 = d_isGroup_988 (coe v2) in
    let v4 = d_isMonoid_902 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    let v6 = d_isMagma_444 (coe v5) in
    \ v7 v8 v9 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v6)) v7
-- Algebra.Structures.IsCommutativeRing._.setoid
d_setoid_2652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2652 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_setoid_2652 v9
du_setoid_2652 ::
  T_IsCommutativeRing_2540 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2652 v0
  = let v1 = d_isRing_2556 (coe v0) in
    let v2 = d_'43''45'isAbelianGroup_2418 (coe v1) in
    let v3 = d_isGroup_988 (coe v2) in
    let v4 = d_isMonoid_902 (coe v3) in
    let v5 = d_isSemigroup_610 (coe v4) in
    coe du_setoid_164 (coe d_isMagma_444 (coe v5))
-- Algebra.Structures.IsCommutativeRing._.sym
d_sym_2654 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2654 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988
                     (coe
                        d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0))))))))
-- Algebra.Structures.IsCommutativeRing._.trans
d_trans_2656 ::
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2656 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_444
            (coe
               d_isSemigroup_610
               (coe
                  d_isMonoid_902
                  (coe
                     d_isGroup_988
                     (coe
                        d_'43''45'isAbelianGroup_2418 (coe d_isRing_2556 (coe v0))))))))
-- Algebra.Structures.IsCommutativeRing._.uniqueʳ-⁻¹
d_unique'691''45''8315''185'_2658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'691''45''8315''185'_2658 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 ~v8
                                  v9
  = du_unique'691''45''8315''185'_2658 v4 v6 v7 v9
du_unique'691''45''8315''185'_2658 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'691''45''8315''185'_2658 v0 v1 v2 v3
  = let v4 = d_isRing_2556 (coe v3) in
    let v5 = d_'43''45'isAbelianGroup_2418 (coe v4) in
    coe
      du_unique'691''45''8315''185'_964 (coe v0) (coe v2) (coe v1)
      (coe d_isGroup_988 (coe v5))
-- Algebra.Structures.IsCommutativeRing._.uniqueˡ-⁻¹
d_unique'737''45''8315''185'_2660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_unique'737''45''8315''185'_2660 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 ~v8
                                  v9
  = du_unique'737''45''8315''185'_2660 v4 v6 v7 v9
du_unique'737''45''8315''185'_2660 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsCommutativeRing_2540 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_unique'737''45''8315''185'_2660 v0 v1 v2 v3
  = let v4 = d_isRing_2556 (coe v3) in
    let v5 = d_'43''45'isAbelianGroup_2418 (coe v4) in
    coe
      du_unique'737''45''8315''185'_958 (coe v0) (coe v2) (coe v1)
      (coe d_isGroup_988 (coe v5))
-- Algebra.Structures.IsCommutativeRing._.zero
d_zero_2662 ::
  T_IsCommutativeRing_2540 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zero_2662 v0 = coe d_zero_2428 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.zeroʳ
d_zero'691'_2664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
d_zero'691'_2664 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_zero'691'_2664 v9
du_zero'691'_2664 :: T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
du_zero'691'_2664 v0
  = coe du_zero'691'_2514 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.zeroˡ
d_zero'737'_2666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
d_zero'737'_2666 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_zero'737'_2666 v9
du_zero'737'_2666 :: T_IsCommutativeRing_2540 -> AgdaAny -> AgdaAny
du_zero'737'_2666 v0
  = coe du_zero'737'_2512 (coe d_isRing_2556 (coe v0))
-- Algebra.Structures.IsCommutativeRing.isCommutativeSemiring
d_isCommutativeSemiring_2668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsCommutativeSemiring_1480
d_isCommutativeSemiring_2668 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeSemiring_2668 v9
du_isCommutativeSemiring_2668 ::
  T_IsCommutativeRing_2540 -> T_IsCommutativeSemiring_1480
du_isCommutativeSemiring_2668 v0
  = coe
      C_IsCommutativeSemiring'46'constructor_46125
      (coe du_isSemiring_2518 (coe d_isRing_2556 (coe v0)))
      (coe d_'42''45'comm_2558 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.isCommutativeMagma
d_isCommutativeMagma_2672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsCommutativeMagma_176
d_isCommutativeMagma_2672 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_isCommutativeMagma_2672 v9
du_isCommutativeMagma_2672 ::
  T_IsCommutativeRing_2540 -> T_IsCommutativeMagma_176
du_isCommutativeMagma_2672 v0
  = let v1 = coe du_isCommutativeSemiring_2668 (coe v0) in
    let v2 = coe du_isCommutativeSemiringWithoutOne_1582 (coe v1) in
    coe
      du_isCommutativeMagma_550
      (coe du_'42''45'isCommutativeSemigroup_1256 (coe v2))
-- Algebra.Structures.IsCommutativeRing._.*-isCommutativeMonoid
d_'42''45'isCommutativeMonoid_2674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsCommutativeMonoid_650
d_'42''45'isCommutativeMonoid_2674 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                   ~v8 v9
  = du_'42''45'isCommutativeMonoid_2674 v9
du_'42''45'isCommutativeMonoid_2674 ::
  T_IsCommutativeRing_2540 -> T_IsCommutativeMonoid_650
du_'42''45'isCommutativeMonoid_2674 v0
  = coe
      du_'42''45'isCommutativeMonoid_1590
      (coe du_isCommutativeSemiring_2668 (coe v0))
-- Algebra.Structures.IsCommutativeRing._.*-isCommutativeSemigroup
d_'42''45'isCommutativeSemigroup_2676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T_IsCommutativeRing_2540 -> T_IsCommutativeSemigroup_512
d_'42''45'isCommutativeSemigroup_2676 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                      ~v7 ~v8 v9
  = du_'42''45'isCommutativeSemigroup_2676 v9
du_'42''45'isCommutativeSemigroup_2676 ::
  T_IsCommutativeRing_2540 -> T_IsCommutativeSemigroup_512
du_'42''45'isCommutativeSemigroup_2676 v0
  = let v1 = coe du_isCommutativeSemiring_2668 (coe v0) in
    coe
      du_'42''45'isCommutativeSemigroup_1256
      (coe du_isCommutativeSemiringWithoutOne_1582 (coe v1))
-- Algebra.Structures.IsCommutativeRing._.isCommutativeSemiringWithoutOne
d_isCommutativeSemiringWithoutOne_2678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T_IsCommutativeRing_2540 -> T_IsCommutativeSemiringWithoutOne_1204
d_isCommutativeSemiringWithoutOne_2678 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
                                       ~v7 ~v8 v9
  = du_isCommutativeSemiringWithoutOne_2678 v9
du_isCommutativeSemiringWithoutOne_2678 ::
  T_IsCommutativeRing_2540 -> T_IsCommutativeSemiringWithoutOne_1204
du_isCommutativeSemiringWithoutOne_2678 v0
  = coe
      du_isCommutativeSemiringWithoutOne_1582
      (coe du_isCommutativeSemiring_2668 (coe v0))
-- Algebra.Structures.IsQuasigroup
d_IsQuasigroup_2686 a0 a1 a2 a3 a4 a5 a6 = ()
data T_IsQuasigroup_2686
  = C_IsQuasigroup'46'constructor_93139 T_IsMagma_140
                                        (AgdaAny ->
                                         AgdaAny ->
                                         AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                        (AgdaAny ->
                                         AgdaAny ->
                                         AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                        MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                        MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsQuasigroup.isMagma
d_isMagma_2704 :: T_IsQuasigroup_2686 -> T_IsMagma_140
d_isMagma_2704 v0
  = case coe v0 of
      C_IsQuasigroup'46'constructor_93139 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasigroup.\\-cong
d_'92''92''45'cong_2706 ::
  T_IsQuasigroup_2686 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_2706 v0
  = case coe v0 of
      C_IsQuasigroup'46'constructor_93139 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasigroup.//-cong
d_'47''47''45'cong_2708 ::
  T_IsQuasigroup_2686 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_2708 v0
  = case coe v0 of
      C_IsQuasigroup'46'constructor_93139 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasigroup.leftDivides
d_leftDivides_2710 ::
  T_IsQuasigroup_2686 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_2710 v0
  = case coe v0 of
      C_IsQuasigroup'46'constructor_93139 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasigroup.rightDivides
d_rightDivides_2712 ::
  T_IsQuasigroup_2686 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_2712 v0
  = case coe v0 of
      C_IsQuasigroup'46'constructor_93139 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsQuasigroup._.isEquivalence
d_isEquivalence_2716 ::
  T_IsQuasigroup_2686 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2716 v0
  = coe d_isEquivalence_148 (coe d_isMagma_2704 (coe v0))
-- Algebra.Structures.IsQuasigroup._.isPartialEquivalence
d_isPartialEquivalence_2718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2718 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isPartialEquivalence_2718 v7
du_isPartialEquivalence_2718 ::
  T_IsQuasigroup_2686 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2718 v0
  = let v1 = d_isMagma_2704 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v1))
-- Algebra.Structures.IsQuasigroup._.refl
d_refl_2720 :: T_IsQuasigroup_2686 -> AgdaAny -> AgdaAny
d_refl_2720 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe d_isEquivalence_148 (coe d_isMagma_2704 (coe v0)))
-- Algebra.Structures.IsQuasigroup._.reflexive
d_reflexive_2722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2722 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_reflexive_2722 v7
du_reflexive_2722 ::
  T_IsQuasigroup_2686 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2722 v0
  = let v1 = d_isMagma_2704 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v1)) v2
-- Algebra.Structures.IsQuasigroup._.setoid
d_setoid_2724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2724 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_setoid_2724 v7
du_setoid_2724 ::
  T_IsQuasigroup_2686 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2724 v0 = coe du_setoid_164 (coe d_isMagma_2704 (coe v0))
-- Algebra.Structures.IsQuasigroup._.sym
d_sym_2726 ::
  T_IsQuasigroup_2686 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2726 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe d_isEquivalence_148 (coe d_isMagma_2704 (coe v0)))
-- Algebra.Structures.IsQuasigroup._.trans
d_trans_2728 ::
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2728 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe d_isEquivalence_148 (coe d_isMagma_2704 (coe v0)))
-- Algebra.Structures.IsQuasigroup._.∙-cong
d_'8729''45'cong_2730 ::
  T_IsQuasigroup_2686 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2730 v0
  = coe d_'8729''45'cong_150 (coe d_isMagma_2704 (coe v0))
-- Algebra.Structures.IsQuasigroup._.∙-congʳ
d_'8729''45'cong'691'_2732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2732 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'691'_2732 v7
du_'8729''45'cong'691'_2732 ::
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2732 v0
  = coe du_'8729''45'cong'691'_170 (coe d_isMagma_2704 (coe v0))
-- Algebra.Structures.IsQuasigroup._.∙-congˡ
d_'8729''45'cong'737'_2734 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2734 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_'8729''45'cong'737'_2734 v7
du_'8729''45'cong'737'_2734 ::
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2734 v0
  = coe du_'8729''45'cong'737'_166 (coe d_isMagma_2704 (coe v0))
-- Algebra.Structures.IsQuasigroup.\\-congˡ
d_'92''92''45'cong'737'_2736 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_2736 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9
                             v10 v11
  = du_'92''92''45'cong'737'_2736 v7 v8 v9 v10 v11
du_'92''92''45'cong'737'_2736 ::
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_2736 v0 v1 v2 v3 v4
  = coe
      d_'92''92''45'cong_2706 v0 v1 v1 v2 v3
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (d_isEquivalence_148 (coe d_isMagma_2704 (coe v0))) v1)
      v4
-- Algebra.Structures.IsQuasigroup.\\-congʳ
d_'92''92''45'cong'691'_2740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_2740 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9
                             v10 v11
  = du_'92''92''45'cong'691'_2740 v7 v8 v9 v10 v11
du_'92''92''45'cong'691'_2740 ::
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_2740 v0 v1 v2 v3 v4
  = coe
      d_'92''92''45'cong_2706 v0 v2 v3 v1 v1 v4
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (d_isEquivalence_148 (coe d_isMagma_2704 (coe v0))) v1)
-- Algebra.Structures.IsQuasigroup.//-congˡ
d_'47''47''45'cong'737'_2744 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_2744 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9
                             v10 v11
  = du_'47''47''45'cong'737'_2744 v7 v8 v9 v10 v11
du_'47''47''45'cong'737'_2744 ::
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_2744 v0 v1 v2 v3 v4
  = coe
      d_'47''47''45'cong_2708 v0 v1 v1 v2 v3
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (d_isEquivalence_148 (coe d_isMagma_2704 (coe v0))) v1)
      v4
-- Algebra.Structures.IsQuasigroup.//-congʳ
d_'47''47''45'cong'691'_2748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_2748 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9
                             v10 v11
  = du_'47''47''45'cong'691'_2748 v7 v8 v9 v10 v11
du_'47''47''45'cong'691'_2748 ::
  T_IsQuasigroup_2686 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_2748 v0 v1 v2 v3 v4
  = coe
      d_'47''47''45'cong_2708 v0 v2 v3 v1 v1 v4
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (d_isEquivalence_148 (coe d_isMagma_2704 (coe v0))) v1)
-- Algebra.Structures.IsQuasigroup.leftDividesˡ
d_leftDivides'737'_2752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_2752 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_leftDivides'737'_2752 v7
du_leftDivides'737'_2752 ::
  T_IsQuasigroup_2686 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_2752 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_leftDivides_2710 (coe v0))
-- Algebra.Structures.IsQuasigroup.leftDividesʳ
d_leftDivides'691'_2754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_2754 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_leftDivides'691'_2754 v7
du_leftDivides'691'_2754 ::
  T_IsQuasigroup_2686 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_2754 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_leftDivides_2710 (coe v0))
-- Algebra.Structures.IsQuasigroup.rightDividesˡ
d_rightDivides'737'_2756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_2756 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_rightDivides'737'_2756 v7
du_rightDivides'737'_2756 ::
  T_IsQuasigroup_2686 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_2756 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_rightDivides_2712 (coe v0))
-- Algebra.Structures.IsQuasigroup.rightDividesʳ
d_rightDivides'691'_2758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_IsQuasigroup_2686 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_2758 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_rightDivides'691'_2758 v7
du_rightDivides'691'_2758 ::
  T_IsQuasigroup_2686 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_2758 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_rightDivides_2712 (coe v0))
-- Algebra.Structures.IsLoop
d_IsLoop_2768 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsLoop_2768
  = C_IsLoop'46'constructor_98347 T_IsQuasigroup_2686
                                  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Algebra.Structures.IsLoop.isQuasigroup
d_isQuasigroup_2782 :: T_IsLoop_2768 -> T_IsQuasigroup_2686
d_isQuasigroup_2782 v0
  = case coe v0 of
      C_IsLoop'46'constructor_98347 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsLoop.identity
d_identity_2784 ::
  T_IsLoop_2768 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2784 v0
  = case coe v0 of
      C_IsLoop'46'constructor_98347 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsLoop._.//-cong
d_'47''47''45'cong_2788 ::
  T_IsLoop_2768 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_2788 v0
  = coe d_'47''47''45'cong_2708 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.//-congʳ
d_'47''47''45'cong'691'_2790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_2790 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'47''47''45'cong'691'_2790 v8
du_'47''47''45'cong'691'_2790 ::
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_2790 v0
  = coe
      du_'47''47''45'cong'691'_2748 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.//-congˡ
d_'47''47''45'cong'737'_2792 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_2792 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'47''47''45'cong'737'_2792 v8
du_'47''47''45'cong'737'_2792 ::
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_2792 v0
  = coe
      du_'47''47''45'cong'737'_2744 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.\\-cong
d_'92''92''45'cong_2794 ::
  T_IsLoop_2768 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_2794 v0
  = coe d_'92''92''45'cong_2706 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.\\-congʳ
d_'92''92''45'cong'691'_2796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_2796 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'92''92''45'cong'691'_2796 v8
du_'92''92''45'cong'691'_2796 ::
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_2796 v0
  = coe
      du_'92''92''45'cong'691'_2740 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.\\-congˡ
d_'92''92''45'cong'737'_2798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_2798 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'92''92''45'cong'737'_2798 v8
du_'92''92''45'cong'737'_2798 ::
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_2798 v0
  = coe
      du_'92''92''45'cong'737'_2736 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.isEquivalence
d_isEquivalence_2800 ::
  T_IsLoop_2768 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2800 v0
  = coe
      d_isEquivalence_148
      (coe d_isMagma_2704 (coe d_isQuasigroup_2782 (coe v0)))
-- Algebra.Structures.IsLoop._.isMagma
d_isMagma_2802 :: T_IsLoop_2768 -> T_IsMagma_140
d_isMagma_2802 v0
  = coe d_isMagma_2704 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.isPartialEquivalence
d_isPartialEquivalence_2804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLoop_2768 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2804 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_2804 v8
du_isPartialEquivalence_2804 ::
  T_IsLoop_2768 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2804 v0
  = let v1 = d_isQuasigroup_2782 (coe v0) in
    let v2 = d_isMagma_2704 (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v2))
-- Algebra.Structures.IsLoop._.leftDivides
d_leftDivides_2806 ::
  T_IsLoop_2768 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_2806 v0
  = coe d_leftDivides_2710 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.leftDividesʳ
d_leftDivides'691'_2808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLoop_2768 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_2808 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_leftDivides'691'_2808 v8
du_leftDivides'691'_2808 ::
  T_IsLoop_2768 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_2808 v0
  = coe du_leftDivides'691'_2754 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.leftDividesˡ
d_leftDivides'737'_2810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLoop_2768 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_2810 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_leftDivides'737'_2810 v8
du_leftDivides'737'_2810 ::
  T_IsLoop_2768 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_2810 v0
  = coe du_leftDivides'737'_2752 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.refl
d_refl_2812 :: T_IsLoop_2768 -> AgdaAny -> AgdaAny
d_refl_2812 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe d_isMagma_2704 (coe d_isQuasigroup_2782 (coe v0))))
-- Algebra.Structures.IsLoop._.reflexive
d_reflexive_2814 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLoop_2768 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2814 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_2814 v8
du_reflexive_2814 ::
  T_IsLoop_2768 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2814 v0
  = let v1 = d_isQuasigroup_2782 (coe v0) in
    let v2 = d_isMagma_2704 (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v2)) v3
-- Algebra.Structures.IsLoop._.rightDivides
d_rightDivides_2816 ::
  T_IsLoop_2768 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_2816 v0
  = coe d_rightDivides_2712 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.rightDividesʳ
d_rightDivides'691'_2818 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLoop_2768 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_2818 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rightDivides'691'_2818 v8
du_rightDivides'691'_2818 ::
  T_IsLoop_2768 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_2818 v0
  = coe du_rightDivides'691'_2758 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.rightDividesˡ
d_rightDivides'737'_2820 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLoop_2768 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_2820 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rightDivides'737'_2820 v8
du_rightDivides'737'_2820 ::
  T_IsLoop_2768 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_2820 v0
  = coe du_rightDivides'737'_2756 (coe d_isQuasigroup_2782 (coe v0))
-- Algebra.Structures.IsLoop._.setoid
d_setoid_2822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLoop_2768 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2822 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_2822 v8
du_setoid_2822 ::
  T_IsLoop_2768 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2822 v0
  = let v1 = d_isQuasigroup_2782 (coe v0) in
    coe du_setoid_164 (coe d_isMagma_2704 (coe v1))
-- Algebra.Structures.IsLoop._.sym
d_sym_2824 ::
  T_IsLoop_2768 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2824 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe d_isMagma_2704 (coe d_isQuasigroup_2782 (coe v0))))
-- Algebra.Structures.IsLoop._.trans
d_trans_2826 ::
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2826 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe d_isMagma_2704 (coe d_isQuasigroup_2782 (coe v0))))
-- Algebra.Structures.IsLoop._.∙-cong
d_'8729''45'cong_2828 ::
  T_IsLoop_2768 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2828 v0
  = coe
      d_'8729''45'cong_150
      (coe d_isMagma_2704 (coe d_isQuasigroup_2782 (coe v0)))
-- Algebra.Structures.IsLoop._.∙-congʳ
d_'8729''45'cong'691'_2830 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2830 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_2830 v8
du_'8729''45'cong'691'_2830 ::
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2830 v0
  = let v1 = d_isQuasigroup_2782 (coe v0) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_2704 (coe v1))
-- Algebra.Structures.IsLoop._.∙-congˡ
d_'8729''45'cong'737'_2832 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2832 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_2832 v8
du_'8729''45'cong'737'_2832 ::
  T_IsLoop_2768 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2832 v0
  = let v1 = d_isQuasigroup_2782 (coe v0) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_2704 (coe v1))
-- Algebra.Structures.IsLoop.identityˡ
d_identity'737'_2834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLoop_2768 -> AgdaAny -> AgdaAny
d_identity'737'_2834 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_2834 v8
du_identity'737'_2834 :: T_IsLoop_2768 -> AgdaAny -> AgdaAny
du_identity'737'_2834 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_identity_2784 (coe v0))
-- Algebra.Structures.IsLoop.identityʳ
d_identity'691'_2836 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLoop_2768 -> AgdaAny -> AgdaAny
d_identity'691'_2836 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_2836 v8
du_identity'691'_2836 :: T_IsLoop_2768 -> AgdaAny -> AgdaAny
du_identity'691'_2836 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_identity_2784 (coe v0))
-- Algebra.Structures.IsLeftBolLoop
d_IsLeftBolLoop_2846 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsLeftBolLoop_2846
  = C_IsLeftBolLoop'46'constructor_101343 T_IsLoop_2768
                                          (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsLeftBolLoop.isLoop
d_isLoop_2860 :: T_IsLeftBolLoop_2846 -> T_IsLoop_2768
d_isLoop_2860 v0
  = case coe v0 of
      C_IsLeftBolLoop'46'constructor_101343 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsLeftBolLoop.leftBol
d_leftBol_2862 ::
  T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_leftBol_2862 v0
  = case coe v0 of
      C_IsLeftBolLoop'46'constructor_101343 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsLeftBolLoop._.//-cong
d_'47''47''45'cong_2866 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_2866 v0
  = coe
      d_'47''47''45'cong_2708
      (coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0)))
-- Algebra.Structures.IsLeftBolLoop._.//-congʳ
d_'47''47''45'cong'691'_2868 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_2868 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'47''47''45'cong'691'_2868 v8
du_'47''47''45'cong'691'_2868 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_2868 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    coe
      du_'47''47''45'cong'691'_2748 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsLeftBolLoop._.//-congˡ
d_'47''47''45'cong'737'_2870 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_2870 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'47''47''45'cong'737'_2870 v8
du_'47''47''45'cong'737'_2870 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_2870 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    coe
      du_'47''47''45'cong'737'_2744 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsLeftBolLoop._.\\-cong
d_'92''92''45'cong_2872 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_2872 v0
  = coe
      d_'92''92''45'cong_2706
      (coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0)))
-- Algebra.Structures.IsLeftBolLoop._.\\-congʳ
d_'92''92''45'cong'691'_2874 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_2874 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'92''92''45'cong'691'_2874 v8
du_'92''92''45'cong'691'_2874 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_2874 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    coe
      du_'92''92''45'cong'691'_2740 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsLeftBolLoop._.\\-congˡ
d_'92''92''45'cong'737'_2876 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_2876 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'92''92''45'cong'737'_2876 v8
du_'92''92''45'cong'737'_2876 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_2876 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    coe
      du_'92''92''45'cong'737'_2736 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsLeftBolLoop._.identity
d_identity_2878 ::
  T_IsLeftBolLoop_2846 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2878 v0
  = coe d_identity_2784 (coe d_isLoop_2860 (coe v0))
-- Algebra.Structures.IsLeftBolLoop._.identityʳ
d_identity'691'_2880 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny
d_identity'691'_2880 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_2880 v8
du_identity'691'_2880 :: T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny
du_identity'691'_2880 v0
  = coe du_identity'691'_2836 (coe d_isLoop_2860 (coe v0))
-- Algebra.Structures.IsLeftBolLoop._.identityˡ
d_identity'737'_2882 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny
d_identity'737'_2882 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_2882 v8
du_identity'737'_2882 :: T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny
du_identity'737'_2882 v0
  = coe du_identity'737'_2834 (coe d_isLoop_2860 (coe v0))
-- Algebra.Structures.IsLeftBolLoop._.isEquivalence
d_isEquivalence_2884 ::
  T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2884 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_2704
         (coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0))))
-- Algebra.Structures.IsLeftBolLoop._.isMagma
d_isMagma_2886 :: T_IsLeftBolLoop_2846 -> T_IsMagma_140
d_isMagma_2886 v0
  = coe
      d_isMagma_2704
      (coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0)))
-- Algebra.Structures.IsLeftBolLoop._.isPartialEquivalence
d_isPartialEquivalence_2888 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2888 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_2888 v8
du_isPartialEquivalence_2888 ::
  T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2888 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    let v3 = d_isMagma_2704 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v3))
-- Algebra.Structures.IsLeftBolLoop._.isQuasigroup
d_isQuasigroup_2890 :: T_IsLeftBolLoop_2846 -> T_IsQuasigroup_2686
d_isQuasigroup_2890 v0
  = coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0))
-- Algebra.Structures.IsLeftBolLoop._.leftDivides
d_leftDivides_2892 ::
  T_IsLeftBolLoop_2846 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_2892 v0
  = coe
      d_leftDivides_2710
      (coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0)))
-- Algebra.Structures.IsLeftBolLoop._.leftDividesʳ
d_leftDivides'691'_2894 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_2894 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_leftDivides'691'_2894 v8
du_leftDivides'691'_2894 ::
  T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_2894 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    coe du_leftDivides'691'_2754 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsLeftBolLoop._.leftDividesˡ
d_leftDivides'737'_2896 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_2896 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_leftDivides'737'_2896 v8
du_leftDivides'737'_2896 ::
  T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_2896 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    coe du_leftDivides'737'_2752 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsLeftBolLoop._.refl
d_refl_2898 :: T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny
d_refl_2898 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0)))))
-- Algebra.Structures.IsLeftBolLoop._.reflexive
d_reflexive_2900 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLeftBolLoop_2846 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2900 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_2900 v8
du_reflexive_2900 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2900 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    let v3 = d_isMagma_2704 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v3)) v4
-- Algebra.Structures.IsLeftBolLoop._.rightDivides
d_rightDivides_2902 ::
  T_IsLeftBolLoop_2846 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_2902 v0
  = coe
      d_rightDivides_2712
      (coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0)))
-- Algebra.Structures.IsLeftBolLoop._.rightDividesʳ
d_rightDivides'691'_2904 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_2904 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rightDivides'691'_2904 v8
du_rightDivides'691'_2904 ::
  T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_2904 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    coe du_rightDivides'691'_2758 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsLeftBolLoop._.rightDividesˡ
d_rightDivides'737'_2906 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_2906 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rightDivides'737'_2906 v8
du_rightDivides'737'_2906 ::
  T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_2906 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    coe du_rightDivides'737'_2756 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsLeftBolLoop._.setoid
d_setoid_2908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2908 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_2908 v8
du_setoid_2908 ::
  T_IsLeftBolLoop_2846 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2908 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    coe du_setoid_164 (coe d_isMagma_2704 (coe v2))
-- Algebra.Structures.IsLeftBolLoop._.sym
d_sym_2910 ::
  T_IsLeftBolLoop_2846 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2910 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0)))))
-- Algebra.Structures.IsLeftBolLoop._.trans
d_trans_2912 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2912 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0)))))
-- Algebra.Structures.IsLeftBolLoop._.∙-cong
d_'8729''45'cong_2914 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2914 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_2704
         (coe d_isQuasigroup_2782 (coe d_isLoop_2860 (coe v0))))
-- Algebra.Structures.IsLeftBolLoop._.∙-congʳ
d_'8729''45'cong'691'_2916 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2916 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_2916 v8
du_'8729''45'cong'691'_2916 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2916 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_2704 (coe v2))
-- Algebra.Structures.IsLeftBolLoop._.∙-congˡ
d_'8729''45'cong'737'_2918 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_2918 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_2918 v8
du_'8729''45'cong'737'_2918 ::
  T_IsLeftBolLoop_2846 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_2918 v0
  = let v1 = d_isLoop_2860 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_2704 (coe v2))
-- Algebra.Structures.IsRightBolLoop
d_IsRightBolLoop_2928 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsRightBolLoop_2928
  = C_IsRightBolLoop'46'constructor_103819 T_IsLoop_2768
                                           (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsRightBolLoop.isLoop
d_isLoop_2942 :: T_IsRightBolLoop_2928 -> T_IsLoop_2768
d_isLoop_2942 v0
  = case coe v0 of
      C_IsRightBolLoop'46'constructor_103819 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRightBolLoop.rightBol
d_rightBol_2944 ::
  T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_rightBol_2944 v0
  = case coe v0 of
      C_IsRightBolLoop'46'constructor_103819 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsRightBolLoop._.//-cong
d_'47''47''45'cong_2948 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_2948 v0
  = coe
      d_'47''47''45'cong_2708
      (coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0)))
-- Algebra.Structures.IsRightBolLoop._.//-congʳ
d_'47''47''45'cong'691'_2950 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_2950 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'47''47''45'cong'691'_2950 v8
du_'47''47''45'cong'691'_2950 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_2950 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    coe
      du_'47''47''45'cong'691'_2748 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsRightBolLoop._.//-congˡ
d_'47''47''45'cong'737'_2952 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_2952 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'47''47''45'cong'737'_2952 v8
du_'47''47''45'cong'737'_2952 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_2952 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    coe
      du_'47''47''45'cong'737'_2744 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsRightBolLoop._.\\-cong
d_'92''92''45'cong_2954 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_2954 v0
  = coe
      d_'92''92''45'cong_2706
      (coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0)))
-- Algebra.Structures.IsRightBolLoop._.\\-congʳ
d_'92''92''45'cong'691'_2956 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_2956 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'92''92''45'cong'691'_2956 v8
du_'92''92''45'cong'691'_2956 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_2956 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    coe
      du_'92''92''45'cong'691'_2740 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsRightBolLoop._.\\-congˡ
d_'92''92''45'cong'737'_2958 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_2958 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'92''92''45'cong'737'_2958 v8
du_'92''92''45'cong'737'_2958 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_2958 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    coe
      du_'92''92''45'cong'737'_2736 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsRightBolLoop._.identity
d_identity_2960 ::
  T_IsRightBolLoop_2928 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_2960 v0
  = coe d_identity_2784 (coe d_isLoop_2942 (coe v0))
-- Algebra.Structures.IsRightBolLoop._.identityʳ
d_identity'691'_2962 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny
d_identity'691'_2962 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_2962 v8
du_identity'691'_2962 ::
  T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny
du_identity'691'_2962 v0
  = coe du_identity'691'_2836 (coe d_isLoop_2942 (coe v0))
-- Algebra.Structures.IsRightBolLoop._.identityˡ
d_identity'737'_2964 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny
d_identity'737'_2964 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_2964 v8
du_identity'737'_2964 ::
  T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny
du_identity'737'_2964 v0
  = coe du_identity'737'_2834 (coe d_isLoop_2942 (coe v0))
-- Algebra.Structures.IsRightBolLoop._.isEquivalence
d_isEquivalence_2966 ::
  T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_2966 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_2704
         (coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0))))
-- Algebra.Structures.IsRightBolLoop._.isMagma
d_isMagma_2968 :: T_IsRightBolLoop_2928 -> T_IsMagma_140
d_isMagma_2968 v0
  = coe
      d_isMagma_2704
      (coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0)))
-- Algebra.Structures.IsRightBolLoop._.isPartialEquivalence
d_isPartialEquivalence_2970 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_2970 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_2970 v8
du_isPartialEquivalence_2970 ::
  T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_2970 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    let v3 = d_isMagma_2704 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v3))
-- Algebra.Structures.IsRightBolLoop._.isQuasigroup
d_isQuasigroup_2972 :: T_IsRightBolLoop_2928 -> T_IsQuasigroup_2686
d_isQuasigroup_2972 v0
  = coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0))
-- Algebra.Structures.IsRightBolLoop._.leftDivides
d_leftDivides_2974 ::
  T_IsRightBolLoop_2928 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_2974 v0
  = coe
      d_leftDivides_2710
      (coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0)))
-- Algebra.Structures.IsRightBolLoop._.leftDividesʳ
d_leftDivides'691'_2976 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_2976 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_leftDivides'691'_2976 v8
du_leftDivides'691'_2976 ::
  T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_2976 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    coe du_leftDivides'691'_2754 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsRightBolLoop._.leftDividesˡ
d_leftDivides'737'_2978 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_2978 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_leftDivides'737'_2978 v8
du_leftDivides'737'_2978 ::
  T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_2978 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    coe du_leftDivides'737'_2752 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsRightBolLoop._.refl
d_refl_2980 :: T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny
d_refl_2980 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0)))))
-- Algebra.Structures.IsRightBolLoop._.reflexive
d_reflexive_2982 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRightBolLoop_2928 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_2982 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_2982 v8
du_reflexive_2982 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_2982 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    let v3 = d_isMagma_2704 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v3)) v4
-- Algebra.Structures.IsRightBolLoop._.rightDivides
d_rightDivides_2984 ::
  T_IsRightBolLoop_2928 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_2984 v0
  = coe
      d_rightDivides_2712
      (coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0)))
-- Algebra.Structures.IsRightBolLoop._.rightDividesʳ
d_rightDivides'691'_2986 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_2986 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rightDivides'691'_2986 v8
du_rightDivides'691'_2986 ::
  T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_2986 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    coe du_rightDivides'691'_2758 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsRightBolLoop._.rightDividesˡ
d_rightDivides'737'_2988 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_2988 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rightDivides'737'_2988 v8
du_rightDivides'737'_2988 ::
  T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_2988 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    coe du_rightDivides'737'_2756 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsRightBolLoop._.setoid
d_setoid_2990 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_2990 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_2990 v8
du_setoid_2990 ::
  T_IsRightBolLoop_2928 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_2990 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    coe du_setoid_164 (coe d_isMagma_2704 (coe v2))
-- Algebra.Structures.IsRightBolLoop._.sym
d_sym_2992 ::
  T_IsRightBolLoop_2928 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_2992 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0)))))
-- Algebra.Structures.IsRightBolLoop._.trans
d_trans_2994 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_2994 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0)))))
-- Algebra.Structures.IsRightBolLoop._.∙-cong
d_'8729''45'cong_2996 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_2996 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_2704
         (coe d_isQuasigroup_2782 (coe d_isLoop_2942 (coe v0))))
-- Algebra.Structures.IsRightBolLoop._.∙-congʳ
d_'8729''45'cong'691'_2998 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_2998 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_2998 v8
du_'8729''45'cong'691'_2998 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_2998 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_2704 (coe v2))
-- Algebra.Structures.IsRightBolLoop._.∙-congˡ
d_'8729''45'cong'737'_3000 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3000 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_3000 v8
du_'8729''45'cong'737'_3000 ::
  T_IsRightBolLoop_2928 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3000 v0
  = let v1 = d_isLoop_2942 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_2704 (coe v2))
-- Algebra.Structures.IsMoufangLoop
d_IsMoufangLoop_3010 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsMoufangLoop_3010
  = C_IsMoufangLoop'46'constructor_106319 T_IsLeftBolLoop_2846
                                          (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsMoufangLoop.isLeftBolLoop
d_isLeftBolLoop_3026 ::
  T_IsMoufangLoop_3010 -> T_IsLeftBolLoop_2846
d_isLeftBolLoop_3026 v0
  = case coe v0 of
      C_IsMoufangLoop'46'constructor_106319 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMoufangLoop.rightBol
d_rightBol_3028 ::
  T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_rightBol_3028 v0
  = case coe v0 of
      C_IsMoufangLoop'46'constructor_106319 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMoufangLoop.identical
d_identical_3030 ::
  T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_identical_3030 v0
  = case coe v0 of
      C_IsMoufangLoop'46'constructor_106319 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMoufangLoop._.//-cong
d_'47''47''45'cong_3034 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_3034 v0
  = coe
      d_'47''47''45'cong_2708
      (coe
         d_isQuasigroup_2782
         (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0))))
-- Algebra.Structures.IsMoufangLoop._.//-congʳ
d_'47''47''45'cong'691'_3036 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_3036 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'47''47''45'cong'691'_3036 v8
du_'47''47''45'cong'691'_3036 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_3036 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    coe
      du_'47''47''45'cong'691'_2748 (coe d_isQuasigroup_2782 (coe v2))
-- Algebra.Structures.IsMoufangLoop._.//-congˡ
d_'47''47''45'cong'737'_3038 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_3038 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'47''47''45'cong'737'_3038 v8
du_'47''47''45'cong'737'_3038 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_3038 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    coe
      du_'47''47''45'cong'737'_2744 (coe d_isQuasigroup_2782 (coe v2))
-- Algebra.Structures.IsMoufangLoop._.\\-cong
d_'92''92''45'cong_3040 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_3040 v0
  = coe
      d_'92''92''45'cong_2706
      (coe
         d_isQuasigroup_2782
         (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0))))
-- Algebra.Structures.IsMoufangLoop._.\\-congʳ
d_'92''92''45'cong'691'_3042 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_3042 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'92''92''45'cong'691'_3042 v8
du_'92''92''45'cong'691'_3042 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_3042 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    coe
      du_'92''92''45'cong'691'_2740 (coe d_isQuasigroup_2782 (coe v2))
-- Algebra.Structures.IsMoufangLoop._.\\-congˡ
d_'92''92''45'cong'737'_3044 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_3044 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'92''92''45'cong'737'_3044 v8
du_'92''92''45'cong'737'_3044 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_3044 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    coe
      du_'92''92''45'cong'737'_2736 (coe d_isQuasigroup_2782 (coe v2))
-- Algebra.Structures.IsMoufangLoop._.identity
d_identity_3046 ::
  T_IsMoufangLoop_3010 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_3046 v0
  = coe
      d_identity_2784
      (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0)))
-- Algebra.Structures.IsMoufangLoop._.identityʳ
d_identity'691'_3048 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny
d_identity'691'_3048 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_3048 v8
du_identity'691'_3048 :: T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny
du_identity'691'_3048 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    coe du_identity'691'_2836 (coe d_isLoop_2860 (coe v1))
-- Algebra.Structures.IsMoufangLoop._.identityˡ
d_identity'737'_3050 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny
d_identity'737'_3050 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_3050 v8
du_identity'737'_3050 :: T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny
du_identity'737'_3050 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    coe du_identity'737'_2834 (coe d_isLoop_2860 (coe v1))
-- Algebra.Structures.IsMoufangLoop._.isEquivalence
d_isEquivalence_3052 ::
  T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_3052 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_2704
         (coe
            d_isQuasigroup_2782
            (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0)))))
-- Algebra.Structures.IsMoufangLoop._.isLoop
d_isLoop_3054 :: T_IsMoufangLoop_3010 -> T_IsLoop_2768
d_isLoop_3054 v0
  = coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0))
-- Algebra.Structures.IsMoufangLoop._.isMagma
d_isMagma_3056 :: T_IsMoufangLoop_3010 -> T_IsMagma_140
d_isMagma_3056 v0
  = coe
      d_isMagma_2704
      (coe
         d_isQuasigroup_2782
         (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0))))
-- Algebra.Structures.IsMoufangLoop._.isPartialEquivalence
d_isPartialEquivalence_3058 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3058 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_3058 v8
du_isPartialEquivalence_3058 ::
  T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3058 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    let v3 = d_isQuasigroup_2782 (coe v2) in
    let v4 = d_isMagma_2704 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v4))
-- Algebra.Structures.IsMoufangLoop._.isQuasigroup
d_isQuasigroup_3060 :: T_IsMoufangLoop_3010 -> T_IsQuasigroup_2686
d_isQuasigroup_3060 v0
  = coe
      d_isQuasigroup_2782
      (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0)))
-- Algebra.Structures.IsMoufangLoop._.leftBol
d_leftBol_3062 ::
  T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_leftBol_3062 v0
  = coe d_leftBol_2862 (coe d_isLeftBolLoop_3026 (coe v0))
-- Algebra.Structures.IsMoufangLoop._.leftDivides
d_leftDivides_3064 ::
  T_IsMoufangLoop_3010 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_3064 v0
  = coe
      d_leftDivides_2710
      (coe
         d_isQuasigroup_2782
         (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0))))
-- Algebra.Structures.IsMoufangLoop._.leftDividesʳ
d_leftDivides'691'_3066 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_3066 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_leftDivides'691'_3066 v8
du_leftDivides'691'_3066 ::
  T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_3066 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    coe du_leftDivides'691'_2754 (coe d_isQuasigroup_2782 (coe v2))
-- Algebra.Structures.IsMoufangLoop._.leftDividesˡ
d_leftDivides'737'_3068 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_3068 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_leftDivides'737'_3068 v8
du_leftDivides'737'_3068 ::
  T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_3068 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    coe du_leftDivides'737'_2752 (coe d_isQuasigroup_2782 (coe v2))
-- Algebra.Structures.IsMoufangLoop._.refl
d_refl_3070 :: T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny
d_refl_3070 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe
               d_isQuasigroup_2782
               (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0))))))
-- Algebra.Structures.IsMoufangLoop._.reflexive
d_reflexive_3072 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMoufangLoop_3010 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3072 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_3072 v8
du_reflexive_3072 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3072 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    let v3 = d_isQuasigroup_2782 (coe v2) in
    let v4 = d_isMagma_2704 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v4)) v5
-- Algebra.Structures.IsMoufangLoop._.rightDivides
d_rightDivides_3074 ::
  T_IsMoufangLoop_3010 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_3074 v0
  = coe
      d_rightDivides_2712
      (coe
         d_isQuasigroup_2782
         (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0))))
-- Algebra.Structures.IsMoufangLoop._.rightDividesʳ
d_rightDivides'691'_3076 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_3076 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rightDivides'691'_3076 v8
du_rightDivides'691'_3076 ::
  T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_3076 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    coe du_rightDivides'691'_2758 (coe d_isQuasigroup_2782 (coe v2))
-- Algebra.Structures.IsMoufangLoop._.rightDividesˡ
d_rightDivides'737'_3078 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_3078 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rightDivides'737'_3078 v8
du_rightDivides'737'_3078 ::
  T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_3078 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    coe du_rightDivides'737'_2756 (coe d_isQuasigroup_2782 (coe v2))
-- Algebra.Structures.IsMoufangLoop._.setoid
d_setoid_3080 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3080 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_3080 v8
du_setoid_3080 ::
  T_IsMoufangLoop_3010 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3080 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    let v3 = d_isQuasigroup_2782 (coe v2) in
    coe du_setoid_164 (coe d_isMagma_2704 (coe v3))
-- Algebra.Structures.IsMoufangLoop._.sym
d_sym_3082 ::
  T_IsMoufangLoop_3010 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_3082 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe
               d_isQuasigroup_2782
               (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0))))))
-- Algebra.Structures.IsMoufangLoop._.trans
d_trans_3084 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3084 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe
               d_isQuasigroup_2782
               (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0))))))
-- Algebra.Structures.IsMoufangLoop._.∙-cong
d_'8729''45'cong_3086 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_3086 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_2704
         (coe
            d_isQuasigroup_2782
            (coe d_isLoop_2860 (coe d_isLeftBolLoop_3026 (coe v0)))))
-- Algebra.Structures.IsMoufangLoop._.∙-congʳ
d_'8729''45'cong'691'_3088 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3088 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_3088 v8
du_'8729''45'cong'691'_3088 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3088 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    let v3 = d_isQuasigroup_2782 (coe v2) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_2704 (coe v3))
-- Algebra.Structures.IsMoufangLoop._.∙-congˡ
d_'8729''45'cong'737'_3090 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3090 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_3090 v8
du_'8729''45'cong'737'_3090 ::
  T_IsMoufangLoop_3010 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3090 v0
  = let v1 = d_isLeftBolLoop_3026 (coe v0) in
    let v2 = d_isLoop_2860 (coe v1) in
    let v3 = d_isQuasigroup_2782 (coe v2) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_2704 (coe v3))
-- Algebra.Structures.IsMiddleBolLoop
d_IsMiddleBolLoop_3100 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_IsMiddleBolLoop_3100
  = C_IsMiddleBolLoop'46'constructor_109023 T_IsLoop_2768
                                            (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Algebra.Structures.IsMiddleBolLoop.isLoop
d_isLoop_3114 :: T_IsMiddleBolLoop_3100 -> T_IsLoop_2768
d_isLoop_3114 v0
  = case coe v0 of
      C_IsMiddleBolLoop'46'constructor_109023 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMiddleBolLoop.middleBol
d_middleBol_3116 ::
  T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_middleBol_3116 v0
  = case coe v0 of
      C_IsMiddleBolLoop'46'constructor_109023 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Structures.IsMiddleBolLoop._.//-cong
d_'47''47''45'cong_3120 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong_3120 v0
  = coe
      d_'47''47''45'cong_2708
      (coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0)))
-- Algebra.Structures.IsMiddleBolLoop._.//-congʳ
d_'47''47''45'cong'691'_3122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'691'_3122 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'47''47''45'cong'691'_3122 v8
du_'47''47''45'cong'691'_3122 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'691'_3122 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    coe
      du_'47''47''45'cong'691'_2748 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsMiddleBolLoop._.//-congˡ
d_'47''47''45'cong'737'_3124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'47''47''45'cong'737'_3124 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'47''47''45'cong'737'_3124 v8
du_'47''47''45'cong'737'_3124 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'47''47''45'cong'737'_3124 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    coe
      du_'47''47''45'cong'737'_2744 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsMiddleBolLoop._.\\-cong
d_'92''92''45'cong_3126 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong_3126 v0
  = coe
      d_'92''92''45'cong_2706
      (coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0)))
-- Algebra.Structures.IsMiddleBolLoop._.\\-congʳ
d_'92''92''45'cong'691'_3128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'691'_3128 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'92''92''45'cong'691'_3128 v8
du_'92''92''45'cong'691'_3128 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'691'_3128 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    coe
      du_'92''92''45'cong'691'_2740 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsMiddleBolLoop._.\\-congˡ
d_'92''92''45'cong'737'_3130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'92''92''45'cong'737'_3130 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'92''92''45'cong'737'_3130 v8
du_'92''92''45'cong'737'_3130 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'92''92''45'cong'737'_3130 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    coe
      du_'92''92''45'cong'737'_2736 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsMiddleBolLoop._.identity
d_identity_3132 ::
  T_IsMiddleBolLoop_3100 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_3132 v0
  = coe d_identity_2784 (coe d_isLoop_3114 (coe v0))
-- Algebra.Structures.IsMiddleBolLoop._.identityʳ
d_identity'691'_3134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny
d_identity'691'_3134 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'691'_3134 v8
du_identity'691'_3134 ::
  T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny
du_identity'691'_3134 v0
  = coe du_identity'691'_2836 (coe d_isLoop_3114 (coe v0))
-- Algebra.Structures.IsMiddleBolLoop._.identityˡ
d_identity'737'_3136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny
d_identity'737'_3136 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_identity'737'_3136 v8
du_identity'737'_3136 ::
  T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny
du_identity'737'_3136 v0
  = coe du_identity'737'_2834 (coe d_isLoop_3114 (coe v0))
-- Algebra.Structures.IsMiddleBolLoop._.isEquivalence
d_isEquivalence_3138 ::
  T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_3138 v0
  = coe
      d_isEquivalence_148
      (coe
         d_isMagma_2704
         (coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0))))
-- Algebra.Structures.IsMiddleBolLoop._.isMagma
d_isMagma_3140 :: T_IsMiddleBolLoop_3100 -> T_IsMagma_140
d_isMagma_3140 v0
  = coe
      d_isMagma_2704
      (coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0)))
-- Algebra.Structures.IsMiddleBolLoop._.isPartialEquivalence
d_isPartialEquivalence_3142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_3142 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_isPartialEquivalence_3142 v8
du_isPartialEquivalence_3142 ::
  T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_3142 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    let v3 = d_isMagma_2704 (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe d_isEquivalence_148 (coe v3))
-- Algebra.Structures.IsMiddleBolLoop._.isQuasigroup
d_isQuasigroup_3144 ::
  T_IsMiddleBolLoop_3100 -> T_IsQuasigroup_2686
d_isQuasigroup_3144 v0
  = coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0))
-- Algebra.Structures.IsMiddleBolLoop._.leftDivides
d_leftDivides_3146 ::
  T_IsMiddleBolLoop_3100 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_leftDivides_3146 v0
  = coe
      d_leftDivides_2710
      (coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0)))
-- Algebra.Structures.IsMiddleBolLoop._.leftDividesʳ
d_leftDivides'691'_3148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'691'_3148 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_leftDivides'691'_3148 v8
du_leftDivides'691'_3148 ::
  T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'691'_3148 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    coe du_leftDivides'691'_2754 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsMiddleBolLoop._.leftDividesˡ
d_leftDivides'737'_3150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny -> AgdaAny
d_leftDivides'737'_3150 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_leftDivides'737'_3150 v8
du_leftDivides'737'_3150 ::
  T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny -> AgdaAny
du_leftDivides'737'_3150 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    coe du_leftDivides'737'_2752 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsMiddleBolLoop._.refl
d_refl_3152 :: T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny
d_refl_3152 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0)))))
-- Algebra.Structures.IsMiddleBolLoop._.reflexive
d_reflexive_3154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMiddleBolLoop_3100 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_3154 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_reflexive_3154 v8
du_reflexive_3154 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_3154 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    let v3 = d_isMagma_2704 (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe d_isEquivalence_148 (coe v3)) v4
-- Algebra.Structures.IsMiddleBolLoop._.rightDivides
d_rightDivides_3156 ::
  T_IsMiddleBolLoop_3100 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rightDivides_3156 v0
  = coe
      d_rightDivides_2712
      (coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0)))
-- Algebra.Structures.IsMiddleBolLoop._.rightDividesʳ
d_rightDivides'691'_3158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'691'_3158 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rightDivides'691'_3158 v8
du_rightDivides'691'_3158 ::
  T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'691'_3158 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    coe du_rightDivides'691'_2758 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsMiddleBolLoop._.rightDividesˡ
d_rightDivides'737'_3160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny -> AgdaAny
d_rightDivides'737'_3160 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rightDivides'737'_3160 v8
du_rightDivides'737'_3160 ::
  T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny -> AgdaAny
du_rightDivides'737'_3160 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    coe du_rightDivides'737'_2756 (coe d_isQuasigroup_2782 (coe v1))
-- Algebra.Structures.IsMiddleBolLoop._.setoid
d_setoid_3162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_3162 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_setoid_3162 v8
du_setoid_3162 ::
  T_IsMiddleBolLoop_3100 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_3162 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    coe du_setoid_164 (coe d_isMagma_2704 (coe v2))
-- Algebra.Structures.IsMiddleBolLoop._.sym
d_sym_3164 ::
  T_IsMiddleBolLoop_3100 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_3164 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0)))))
-- Algebra.Structures.IsMiddleBolLoop._.trans
d_trans_3166 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_3166 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         d_isEquivalence_148
         (coe
            d_isMagma_2704
            (coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0)))))
-- Algebra.Structures.IsMiddleBolLoop._.∙-cong
d_'8729''45'cong_3168 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_3168 v0
  = coe
      d_'8729''45'cong_150
      (coe
         d_isMagma_2704
         (coe d_isQuasigroup_2782 (coe d_isLoop_3114 (coe v0))))
-- Algebra.Structures.IsMiddleBolLoop._.∙-congʳ
d_'8729''45'cong'691'_3170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_3170 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'691'_3170 v8
du_'8729''45'cong'691'_3170 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_3170 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    coe du_'8729''45'cong'691'_170 (coe d_isMagma_2704 (coe v2))
-- Algebra.Structures.IsMiddleBolLoop._.∙-congˡ
d_'8729''45'cong'737'_3172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_3172 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_'8729''45'cong'737'_3172 v8
du_'8729''45'cong'737'_3172 ::
  T_IsMiddleBolLoop_3100 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_3172 v0
  = let v1 = d_isLoop_3114 (coe v0) in
    let v2 = d_isQuasigroup_2782 (coe v1) in
    coe du_'8729''45'cong'737'_166 (coe d_isMagma_2704 (coe v2))
