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

module MAlonzo.Code.Algebra.Construct.NaturalChoice.MinOp where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Construct.NaturalChoice.Base
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Double
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Construct.NaturalChoice.MinOp._._≈_
d__'8776'__20 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> ()
d__'8776'__20 = erased
-- Algebra.Construct.NaturalChoice.MinOp._._≲_
d__'8818'__22 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> ()
d__'8818'__22 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.Associative
d_Associative_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Associative_102 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.Commutative
d_Commutative_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Commutative_106 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.Congruent₁
d_Congruent'8321'_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  (AgdaAny -> AgdaAny) -> ()
d_Congruent'8321'_108 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.Congruent₂
d_Congruent'8322'_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Congruent'8322'_110 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.Idempotent
d_Idempotent_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Idempotent_116 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.Identity
d_Identity_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Identity_122 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.LeftIdentity
d_LeftIdentity_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftIdentity_148 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.LeftZero
d_LeftZero_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_LeftZero_156 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.RightIdentity
d_RightIdentity_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightIdentity_178 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.RightZero
d_RightZero_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_RightZero_186 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.Selective
d_Selective_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Selective_188 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.Zero
d_Zero_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Zero_204 = erased
-- Algebra.Construct.NaturalChoice.MinOp._.IsBand
d_IsBand_212 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Construct.NaturalChoice.MinOp._.IsCommutativeSemigroup
d_IsCommutativeSemigroup_222 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Construct.NaturalChoice.MinOp._.IsMagma
d_IsMagma_248 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Construct.NaturalChoice.MinOp._.IsMonoid
d_IsMonoid_254 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Construct.NaturalChoice.MinOp._.IsSelectiveMagma
d_IsSelectiveMagma_274 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Construct.NaturalChoice.MinOp._.IsSemigroup
d_IsSemigroup_276 a0 a1 a2 a3 a4 a5 = ()
-- Algebra.Construct.NaturalChoice.MinOp.x⊓y≤x
d_x'8851'y'8804'x_2556 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8804'x_2556 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_x'8851'y'8804'x_2556 v3 v4 v5 v6
du_x'8851'y'8804'x_2556 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8804'x_2556 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_128
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0))
              v2 v3 in
    case coe v4 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5
        -> let v6
                 = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                     (coe v0) in
           coe
             MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v6))
             (coe v2) (coe v2)
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v2 v3)
             (let v7
                    = coe
                        MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v7)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v3)
                v2
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                   v1 v2 v3 v5))
             (let v7
                    = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                        (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.du_refl_98
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v7))
                (coe v2))
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5
        -> let v6
                 = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                     (coe v0) in
           coe
             MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v6))
             (coe v2) (coe v3)
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v2 v3)
             (let v7
                    = coe
                        MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v7)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v3)
                v3
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                   v1 v2 v3 v5))
             (coe v5)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.MinOp.x⊓y≤y
d_x'8851'y'8804'y_2582 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8804'y_2582 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_x'8851'y'8804'y_2582 v3 v4 v5 v6
du_x'8851'y'8804'y_2582 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8804'y_2582 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_128
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0))
              v2 v3 in
    case coe v4 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5
        -> let v6
                 = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                     (coe v0) in
           coe
             MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v6))
             (coe v3) (coe v2)
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v2 v3)
             (let v7
                    = coe
                        MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v7)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v3)
                v2
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                   v1 v2 v3 v5))
             (coe v5)
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5
        -> let v6
                 = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                     (coe v0) in
           coe
             MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v6))
             (coe v3) (coe v3)
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v2 v3)
             (let v7
                    = coe
                        MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v7)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v3)
                v3
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                   v1 v2 v3 v5))
             (let v7
                    = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                        (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.du_refl_98
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v7))
                (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.MinOp.⊓-comm
d_'8851''45'comm_2604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'comm_2604 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_'8851''45'comm_2604 v3 v4 v5 v6
du_'8851''45'comm_2604 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'comm_2604 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_128
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0))
              v2 v3 in
    case coe v4 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5
        -> let v6
                 = coe
                     MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
           coe
             MAlonzo.Code.Relation.Binary.Structures.d_trans_38
             (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                (coe
                   MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v6)))
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v2 v3)
             v2
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v3 v2)
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                v1 v2 v3 v5)
             (let v7
                    = coe
                        MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v7)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v3 v2)
                v2
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                   v1 v3 v2 v5))
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5
        -> let v6
                 = coe
                     MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
           coe
             MAlonzo.Code.Relation.Binary.Structures.d_trans_38
             (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                (coe
                   MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v6)))
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v2 v3)
             v3
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v3 v2)
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                v1 v2 v3 v5)
             (let v7
                    = coe
                        MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v7)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v3 v2)
                v3
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                   v1 v3 v2 v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.MinOp.⊓-congˡ
d_'8851''45'cong'737'_2630 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'cong'737'_2630 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8
  = du_'8851''45'cong'737'_2630 v3 v4 v5 v6 v7 v8
du_'8851''45'cong'737'_2630 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'cong'737'_2630 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_128
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0))
              v2 v3 in
    case coe v6 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                      (coe v0)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v3)
                (coe v2)
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v4)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                      (coe
                         MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                         (coe v0)))
                   (coe v2)
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v2 v4)
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v2 v4)
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                            (coe v0)))
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                         v2 v4))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                      v1 v2 v4
                      (let v8
                             = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                                 (coe v0) in
                       coe
                         MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
                         (coe
                            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v8))
                         (coe v2) (coe v3) (coe v4) (coe v5) (coe v7))))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                   v1 v2 v3 v7))
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v7
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                      (coe v0)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v3)
                (coe v3)
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v4)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                      (coe
                         MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                         (coe v0)))
                   (coe v3) (coe v4)
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v2 v4)
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                            (coe v0)))
                      (coe v4)
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                         v2 v4)
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                         v2 v4)
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
                         (coe
                            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                            (coe
                               MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                               (coe v0)))
                         (coe
                            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                            v2 v4))
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                         v1 v2 v4
                         (let v8
                                = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                                    (coe v0) in
                          coe
                            MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
                            (coe
                               MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v8))
                            (coe v2) (coe v3) (coe v4) (coe v5) (coe v7))))
                   (coe v5))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                   v1 v2 v3 v7))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.MinOp.⊓-congʳ
d_'8851''45'cong'691'_2668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'cong'691'_2668 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8
  = du_'8851''45'cong'691'_2668 v3 v4 v5 v6 v7 v8
du_'8851''45'cong'691'_2668 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'cong'691'_2668 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
               (coe v0)))
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
            v3 v2)
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
            v2 v3)
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
            v4 v2)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                  (coe v0)))
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               v2 v3)
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               v2 v4)
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               v4 v2)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                     (coe v0)))
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v2 v4)
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v4 v2)
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v4 v2)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                        (coe v0)))
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     v4 v2))
               (coe du_'8851''45'comm_2604 (coe v0) (coe v1) (coe v2) (coe v4)))
            (coe
               du_'8851''45'cong'737'_2630 (coe v0) (coe v1) (coe v2) (coe v3)
               (coe v4) (coe v5)))
         (coe du_'8851''45'comm_2604 (coe v0) (coe v1) (coe v2) (coe v3)))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-cong
d_'8851''45'cong_2678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'cong_2678 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8 v9 v10
  = du_'8851''45'cong_2678 v3 v4 v5 v6 v7 v8 v9 v10
du_'8851''45'cong_2678 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'cong_2678 v0 v1 v2 v3 v4 v5 v6 v7
  = let v8
          = coe
              MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v8)))
      (coe
         MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
            v2)
         (\ v9 v10 -> v9) v4 v5)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v9 v10 -> v10)
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
            v2)
         v4 v5)
      (coe
         MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
         (\ v9 v10 -> v10)
         (\ v9 ->
            coe
              MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
              v9 v5)
         v2 v3)
      (coe
         du_'8851''45'cong'737'_2630 (coe v0) (coe v1) (coe v2) (coe v4)
         (coe v5) (coe v7))
      (coe
         du_'8851''45'cong'691'_2668 (coe v0) (coe v1) (coe v5) (coe v2)
         (coe v3) (coe v6))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-assoc
d_'8851''45'assoc_2692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'assoc_2692 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7
  = du_'8851''45'assoc_2692 v3 v4 v5 v6 v7
du_'8851''45'assoc_2692 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'assoc_2692 v0 v1 v2 v3 v4
  = let v5
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_128
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0))
              v2 v3 in
    let v6
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_128
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0))
              v3 v4 in
    case coe v5 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7
        -> case coe v6 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v8
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
                       (coe
                          MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                          (coe
                             MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                             (coe v0)))
                       (coe
                          MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v2 v3)
                          v4)
                       (coe
                          MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                          v2 v4)
                       (coe
                          MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                          v2
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v3 v4))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
                          (coe
                             MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                             (coe
                                MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                                (coe v0)))
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v2 v4)
                          (coe v2)
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v2
                             (coe
                                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                                v3 v4))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                                   (coe v0)))
                             (coe v2)
                             (coe
                                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                                v2 v3)
                             (coe
                                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                                v2
                                (coe
                                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
                                   v1 v3 v4))
                             (coe
                                MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
                                (coe
                                   MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                                   (coe
                                      MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                                      (coe v0)))
                                (coe
                                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
                                   v1 v2 v3)
                                (coe
                                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
                                   v1 v2
                                   (coe
                                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
                                      v1 v3 v4))
                                (coe
                                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
                                   v1 v2
                                   (coe
                                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
                                      v1 v3 v4))
                                (coe
                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
                                   (coe
                                      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                                      (coe
                                         MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                                         (coe v0)))
                                   (coe
                                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
                                      v1 v2
                                      (coe
                                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
                                         v1 v3 v4)))
                                (coe
                                   du_'8851''45'cong'737'_2630 (coe v0) (coe v1) (coe v2)
                                   (coe
                                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
                                      v1 v3 v4)
                                   (coe v3)
                                   (coe
                                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                                      v1 v3 v4 v8)))
                             (coe
                                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                                v1 v2 v3 v7))
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                             v1 v2 v4
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_trans_84
                                (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                                   (coe
                                      MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                                      (coe v0)))
                                v2 v3 v4 v7 v8)))
                       (coe
                          du_'8851''45'cong'691'_2668 (coe v0) (coe v1) (coe v4)
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v2 v3)
                          (coe v2)
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                             v1 v2 v3 v7)))
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v8
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
                       (coe
                          MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                          (coe
                             MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                             (coe v0)))
                       (coe
                          MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v2 v3)
                          v4)
                       (coe
                          MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                          v2 v4)
                       (coe
                          MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                          v2
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v3 v4))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
                          (coe
                             MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                             (coe
                                MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                                (coe v0)))
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v2 v4)
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v2
                             (coe
                                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                                v3 v4))
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v2
                             (coe
                                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                                v3 v4))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                                   (coe v0)))
                             (coe
                                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                                v2
                                (coe
                                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
                                   v1 v3 v4)))
                          (coe
                             du_'8851''45'cong'737'_2630 (coe v0) (coe v1) (coe v2)
                             (coe
                                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                                v3 v4)
                             (coe v4)
                             (coe
                                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                                v1 v3 v4 v8)))
                       (coe
                          du_'8851''45'cong'691'_2668 (coe v0) (coe v1) (coe v4)
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                             v2 v3)
                          (coe v2)
                          (coe
                             MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                             v1 v2 v3 v7)))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v7
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                      (coe v0)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v2 v3)
                   v4)
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v3 v4)
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v3 v4))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                      (coe
                         MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                         (coe v0)))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v3 v4)
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v2
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                         v3 v4))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v2
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                         v3 v4))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                            (coe v0)))
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                         v2
                         (coe
                            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                            v3 v4)))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                      v1 v2
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                         v3 v4)
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_trans_84
                         (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                            (coe
                               MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                               (coe v0)))
                         (coe
                            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                            v3 v4)
                         v3 v2
                         (coe du_x'8851'y'8804'x_2556 (coe v0) (coe v1) (coe v3) (coe v4))
                         v7)))
                (coe
                   du_'8851''45'cong'691'_2668 (coe v0) (coe v1) (coe v4)
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v2 v3)
                   (coe v3)
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                      v1 v2 v3 v7)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.MinOp.⊓-idem
d_'8851''45'idem_2732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny
d_'8851''45'idem_2732 ~v0 ~v1 ~v2 v3 v4 v5
  = du_'8851''45'idem_2732 v3 v4 v5
du_'8851''45'idem_2732 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny
du_'8851''45'idem_2732 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
      v1 v2 v2
      (let v3
             = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0) in
       coe
         MAlonzo.Code.Relation.Binary.Structures.du_refl_98
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v3))
         (coe v2))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-sel
d_'8851''45'sel_2736 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8851''45'sel_2736 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_'8851''45'sel_2736 v3 v4 v5 v6
du_'8851''45'sel_2736 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_'8851''45'sel_2736 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Sum.Base.du_map_84
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
         v1 v2 v3)
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
         v1 v2 v3)
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_total_128
         (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
            (coe v0))
         v2 v3)
-- Algebra.Construct.NaturalChoice.MinOp.⊓-identityˡ
d_'8851''45'identity'737'_2744 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'identity'737'_2744 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
      v0 v1 v3 (coe v2 v3)
-- Algebra.Construct.NaturalChoice.MinOp.⊓-identityʳ
d_'8851''45'identity'691'_2750 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'identity'691'_2750 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
      v0 v3 v1 (coe v2 v3)
-- Algebra.Construct.NaturalChoice.MinOp.⊓-identity
d_'8851''45'identity_2756 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'identity_2756 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
              v0 v1 v3 (coe v2 v3)))
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
              v0 v3 v1 (coe v2 v3)))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-zeroˡ
d_'8851''45'zero'737'_2762 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'zero'737'_2762 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
      v0 v1 v3 (coe v2 v3)
-- Algebra.Construct.NaturalChoice.MinOp.⊓-zeroʳ
d_'8851''45'zero'691'_2768 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_'8851''45'zero'691'_2768 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
      v0 v3 v1 (coe v2 v3)
-- Algebra.Construct.NaturalChoice.MinOp.⊓-zero
d_'8851''45'zero_2774 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8851''45'zero_2774 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
              v0 v1 v3 (coe v2 v3)))
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
              v0 v3 v1 (coe v2 v3)))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-isMagma
d_'8851''45'isMagma_2778 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8851''45'isMagma_2778 ~v0 ~v1 ~v2 v3 v4
  = du_'8851''45'isMagma_2778 v3 v4
du_'8851''45'isMagma_2778 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8851''45'isMagma_2778 v0 v1
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
               (coe v0))))
      (coe du_'8851''45'cong_2678 (coe v0) (coe v1))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-isSemigroup
d_'8851''45'isSemigroup_2780 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8851''45'isSemigroup_2780 ~v0 ~v1 ~v2 v3 v4
  = du_'8851''45'isSemigroup_2780 v3 v4
du_'8851''45'isSemigroup_2780 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8851''45'isSemigroup_2780 v0 v1
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'8851''45'isMagma_2778 (coe v0) (coe v1))
      (coe du_'8851''45'assoc_2692 (coe v0) (coe v1))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-isBand
d_'8851''45'isBand_2782 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8851''45'isBand_2782 ~v0 ~v1 ~v2 v3 v4
  = du_'8851''45'isBand_2782 v3 v4
du_'8851''45'isBand_2782 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_'8851''45'isBand_2782 v0 v1
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsBand'46'constructor_10089
      (coe du_'8851''45'isSemigroup_2780 (coe v0) (coe v1))
      (coe du_'8851''45'idem_2732 (coe v0) (coe v1))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-isCommutativeSemigroup
d_'8851''45'isCommutativeSemigroup_2784 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_'8851''45'isCommutativeSemigroup_2784 ~v0 ~v1 ~v2 v3 v4
  = du_'8851''45'isCommutativeSemigroup_2784 v3 v4
du_'8851''45'isCommutativeSemigroup_2784 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_'8851''45'isCommutativeSemigroup_2784 v0 v1
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemigroup'46'constructor_10975
      (coe du_'8851''45'isSemigroup_2780 (coe v0) (coe v1))
      (coe du_'8851''45'comm_2604 (coe v0) (coe v1))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-isSelectiveMagma
d_'8851''45'isSelectiveMagma_2786 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_'8851''45'isSelectiveMagma_2786 ~v0 ~v1 ~v2 v3 v4
  = du_'8851''45'isSelectiveMagma_2786 v3 v4
du_'8851''45'isSelectiveMagma_2786 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
du_'8851''45'isSelectiveMagma_2786 v0 v1
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSelectiveMagma'46'constructor_8519
      (coe du_'8851''45'isMagma_2778 (coe v0) (coe v1))
      (coe du_'8851''45'sel_2736 (coe v0) (coe v1))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-isMonoid
d_'8851''45'isMonoid_2790 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8851''45'isMonoid_2790 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_'8851''45'isMonoid_2790 v3 v4 v5 v6
du_'8851''45'isMonoid_2790 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'8851''45'isMonoid_2790 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe du_'8851''45'isSemigroup_2780 (coe v0) (coe v1))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            (\ v4 ->
               coe
                 MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                 v1 v2 v4 (coe v3 v4)))
         (coe
            (\ v4 ->
               coe
                 MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                 v1 v4 v2 (coe v3 v4))))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-rawMagma
d_'8851''45'rawMagma_2794 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8851''45'rawMagma_2794 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8851''45'rawMagma_2794 v4
du_'8851''45'rawMagma_2794 ::
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'8851''45'rawMagma_2794 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMagma'46'constructor_77
      (MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
         (coe v0))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-magma
d_'8851''45'magma_2796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8851''45'magma_2796 ~v0 ~v1 ~v2 v3 v4
  = du_'8851''45'magma_2796 v3 v4
du_'8851''45'magma_2796 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_'8851''45'magma_2796 v0 v1
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187
      (MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
         (coe v1))
      (coe du_'8851''45'isMagma_2778 (coe v0) (coe v1))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-semigroup
d_'8851''45'semigroup_2798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8851''45'semigroup_2798 ~v0 ~v1 ~v2 v3 v4
  = du_'8851''45'semigroup_2798 v3 v4
du_'8851''45'semigroup_2798 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'8851''45'semigroup_2798 v0 v1
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557
      (MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
         (coe v1))
      (coe du_'8851''45'isSemigroup_2780 (coe v0) (coe v1))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-band
d_'8851''45'band_2800 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.T_Band_536
d_'8851''45'band_2800 ~v0 ~v1 ~v2 v3 v4
  = du_'8851''45'band_2800 v3 v4
du_'8851''45'band_2800 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.T_Band_536
du_'8851''45'band_2800 v0 v1
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Band'46'constructor_9627
      (MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
         (coe v1))
      (coe du_'8851''45'isBand_2782 (coe v0) (coe v1))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-commutativeSemigroup
d_'8851''45'commutativeSemigroup_2802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
d_'8851''45'commutativeSemigroup_2802 ~v0 ~v1 ~v2 v3 v4
  = du_'8851''45'commutativeSemigroup_2802 v3 v4
du_'8851''45'commutativeSemigroup_2802 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemigroup_602
du_'8851''45'commutativeSemigroup_2802 v0 v1
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemigroup'46'constructor_10763
      (MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
         (coe v1))
      (coe du_'8851''45'isCommutativeSemigroup_2784 (coe v0) (coe v1))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-selectiveMagma
d_'8851''45'selectiveMagma_2804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
d_'8851''45'selectiveMagma_2804 ~v0 ~v1 ~v2 v3 v4
  = du_'8851''45'selectiveMagma_2804 v3 v4
du_'8851''45'selectiveMagma_2804 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  MAlonzo.Code.Algebra.Bundles.T_SelectiveMagma_62
du_'8851''45'selectiveMagma_2804 v0 v1
  = coe
      MAlonzo.Code.Algebra.Bundles.C_SelectiveMagma'46'constructor_1177
      (MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
         (coe v1))
      (coe du_'8851''45'isSelectiveMagma_2786 (coe v0) (coe v1))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-monoid
d_'8851''45'monoid_2808 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'8851''45'monoid_2808 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_'8851''45'monoid_2808 v3 v4 v5 v6
du_'8851''45'monoid_2808 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'8851''45'monoid_2808 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309
      (MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100
         (coe v1))
      v2
      (coe
         du_'8851''45'isMonoid_2790 (coe v0) (coe v1) (coe v2) (coe v3))
-- Algebra.Construct.NaturalChoice.MinOp.x⊓y≈x⇒x≤y
d_x'8851'y'8776'x'8658'x'8804'y_2816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8776'x'8658'x'8804'y_2816 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7
  = du_x'8851'y'8776'x'8658'x'8804'y_2816 v3 v4 v5 v6 v7
du_x'8851'y'8776'x'8658'x'8804'y_2816 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8776'x'8658'x'8804'y_2816 v0 v1 v2 v3 v4
  = let v5
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_128
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0))
              v2 v3 in
    case coe v5 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v6 -> coe v6
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v6
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82
             (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                (coe
                   MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                   (coe v0)))
             v2 v3
             (let v7
                    = coe
                        MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v7)))
                v2
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v3)
                v3
                (let v8
                       = coe
                           MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
                 coe
                   MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                   (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                      (coe
                         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v8)))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v2 v3)
                   v2 v4)
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                   v1 v2 v3 v6))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.MinOp.x⊓y≈y⇒y≤x
d_x'8851'y'8776'y'8658'y'8804'x_2848 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8851'y'8776'y'8658'y'8804'x_2848 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7
  = du_x'8851'y'8776'y'8658'y'8804'x_2848 v3 v4 v5 v6 v7
du_x'8851'y'8776'y'8658'y'8804'x_2848 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8851'y'8776'y'8658'y'8804'x_2848 v0 v1 v2 v3 v4
  = coe
      du_x'8851'y'8776'x'8658'x'8804'y_2816 (coe v0) (coe v1) (coe v3)
      (coe v2)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                  (coe v0)))
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               v3 v2)
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               v2 v3)
            (coe v3)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                     (coe v0)))
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v2 v3)
               (coe v3) (coe v3)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                        (coe v0)))
                  (coe v3))
               (coe v4))
            (coe du_'8851''45'comm_2604 (coe v0) (coe v1) (coe v3) (coe v2))))
-- Algebra.Construct.NaturalChoice.MinOp.mono-≤-distrib-⊓
d_mono'45''8804''45'distrib'45''8851'_2862 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
d_mono'45''8804''45'distrib'45''8851'_2862 ~v0 ~v1 ~v2 v3 v4 v5 v6
                                           v7 v8 v9
  = du_mono'45''8804''45'distrib'45''8851'_2862 v3 v4 v5 v6 v7 v8 v9
du_mono'45''8804''45'distrib'45''8851'_2862 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
du_mono'45''8804''45'distrib'45''8851'_2862 v0 v1 v2 v3 v4 v5 v6
  = let v7
          = coe
              MAlonzo.Code.Relation.Binary.Structures.d_total_128
              (MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0))
              v5 v6 in
    case coe v7 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v8
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                      (coe v0)))
                (coe
                   v2
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v5 v6))
                (coe v2 v5)
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   (coe v2 v5) (coe v2 v6))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                      (coe
                         MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                         (coe v0)))
                   (coe v2 v5)
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      (coe v2 v5) (coe v2 v6))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      (coe v2 v5) (coe v2 v6))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                            (coe v0)))
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                         (coe v2 v5) (coe v2 v6)))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                      v1 (coe v2 v5) (coe v2 v6) (coe v4 v5 v6 v8)))
                (coe
                   v3
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v5 v6)
                   v5
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8804'y'8658'x'8851'y'8776'x_106
                      v1 v5 v6 v8)))
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v8
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                      (coe v0)))
                (coe
                   v2
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v5 v6))
                (coe v2 v6)
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   (coe v2 v5) (coe v2 v6))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                      (coe
                         MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                         (coe v0)))
                   (coe v2 v6)
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      (coe v2 v5) (coe v2 v6))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      (coe v2 v5) (coe v2 v6))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                            (coe v0)))
                      (coe
                         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                         (coe v2 v5) (coe v2 v6)))
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                      v1 (coe v2 v5) (coe v2 v6) (coe v4 v6 v5 v8)))
                (coe
                   v3
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                      v5 v6)
                   v6
                   (coe
                      MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d_x'8805'y'8658'x'8851'y'8776'y_112
                      v1 v5 v6 v8)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.MinOp.x≤y⇒x⊓z≤y
d_x'8804'y'8658'x'8851'z'8804'y_2908 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'x'8851'z'8804'y_2908 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8
  = du_x'8804'y'8658'x'8851'z'8804'y_2908 v3 v4 v5 v6 v7 v8
du_x'8804'y'8658'x'8851'z'8804'y_2908 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8658'x'8851'z'8804'y_2908 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
            (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
         v2 v4)
      v2 v3
      (coe du_x'8851'y'8804'x_2556 (coe v0) (coe v1) (coe v2) (coe v4))
      v5
-- Algebra.Construct.NaturalChoice.MinOp.x≤y⇒z⊓x≤y
d_x'8804'y'8658'z'8851'x'8804'y_2920 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8658'z'8851'x'8804'y_2920 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8
  = du_x'8804'y'8658'z'8851'x'8804'y_2920 v3 v4 v5 v6 v7 v8
du_x'8804'y'8658'z'8851'x'8804'y_2920 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8658'z'8851'x'8804'y_2920 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
            (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
         v4 v2)
      v2 v3
      (coe du_x'8851'y'8804'y_2582 (coe v0) (coe v1) (coe v4) (coe v2))
      v5
-- Algebra.Construct.NaturalChoice.MinOp.x≤y⊓z⇒x≤y
d_x'8804'y'8851'z'8658'x'8804'y_2932 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8851'z'8658'x'8804'y_2932 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8
  = du_x'8804'y'8851'z'8658'x'8804'y_2932 v3 v4 v5 v6 v7 v8
du_x'8804'y'8851'z'8658'x'8804'y_2932 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8851'z'8658'x'8804'y_2932 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
            (coe v0)))
      v2
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
         v3 v4)
      v3 v5
      (coe du_x'8851'y'8804'x_2556 (coe v0) (coe v1) (coe v3) (coe v4))
-- Algebra.Construct.NaturalChoice.MinOp.x≤y⊓z⇒x≤z
d_x'8804'y'8851'z'8658'x'8804'z_2946 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_x'8804'y'8851'z'8658'x'8804'z_2946 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8
  = du_x'8804'y'8851'z'8658'x'8804'z_2946 v3 v4 v5 v6 v7 v8
du_x'8804'y'8851'z'8658'x'8804'z_2946 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_x'8804'y'8851'z'8658'x'8804'z_2946 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_84
      (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
            (coe v0)))
      v2
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
         v3 v4)
      v4 v5
      (coe du_x'8851'y'8804'y_2582 (coe v0) (coe v1) (coe v3) (coe v4))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-mono-≤
d_'8851''45'mono'45''8804'_2954 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'mono'45''8804'_2954 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8 v9
                                v10
  = du_'8851''45'mono'45''8804'_2954 v3 v4 v5 v6 v7 v8 v9 v10
du_'8851''45'mono'45''8804'_2954 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'mono'45''8804'_2954 v0 v1 v2 v3 v4 v5 v6 v7
  = let v8
          = coe du_'8851''45'sel_2736 (coe v0) (coe v1) (coe v3) (coe v5) in
    case coe v8 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v9
        -> let v10
                 = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                     (coe v0) in
           coe
             MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v10))
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v2 v4)
             (coe v3)
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v3 v5)
             (let v11
                    = coe
                        MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v11)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v3 v5)
                v3 v9)
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_trans_84
                (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                      (coe v0)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v4)
                v2 v3
                (coe du_x'8851'y'8804'x_2556 (coe v0) (coe v1) (coe v2) (coe v4))
                v6)
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v9
        -> let v10
                 = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                     (coe v0) in
           coe
             MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'691''45''8776'_106
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v10))
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v2 v4)
             (coe v5)
             (coe
                MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                v3 v5)
             (let v11
                    = coe
                        MAlonzo.Code.Relation.Binary.Bundles.du_preorder_248 (coe v0) in
              coe
                MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v11)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v3 v5)
                v5 v9)
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_trans_84
                (MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                   (coe
                      MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                      (coe v0)))
                (coe
                   MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                   v2 v4)
                v4 v5
                (coe du_x'8851'y'8804'y_2582 (coe v0) (coe v1) (coe v2) (coe v4))
                v7)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.NaturalChoice.MinOp.⊓-monoˡ-≤
d_'8851''45'mono'737''45''8804'_3004 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'mono'737''45''8804'_3004 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8
  = du_'8851''45'mono'737''45''8804'_3004 v3 v4 v5 v6 v7 v8
du_'8851''45'mono'737''45''8804'_3004 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'mono'737''45''8804'_3004 v0 v1 v2 v3 v4 v5
  = coe
      du_'8851''45'mono'45''8804'_2954 (coe v0) (coe v1) (coe v3)
      (coe v4) (coe v2) (coe v2) (coe v5)
      (let v6
             = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0) in
       coe
         MAlonzo.Code.Relation.Binary.Structures.du_refl_98
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v6))
         (coe v2))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-monoʳ-≤
d_'8851''45'mono'691''45''8804'_3014 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'mono'691''45''8804'_3014 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8
  = du_'8851''45'mono'691''45''8804'_3014 v3 v4 v5 v6 v7 v8
du_'8851''45'mono'691''45''8804'_3014 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'mono'691''45''8804'_3014 v0 v1 v2 v3 v4 v5
  = coe
      du_'8851''45'mono'45''8804'_2954 (coe v0) (coe v1) (coe v2)
      (coe v2) (coe v3) (coe v4)
      (let v6
             = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                 (coe v0) in
       coe
         MAlonzo.Code.Relation.Binary.Structures.du_refl_98
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v6))
         (coe v2))
      (coe v5)
-- Algebra.Construct.NaturalChoice.MinOp.⊓-glb
d_'8851''45'glb_3026 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'glb_3026 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 v8 v9
  = du_'8851''45'glb_3026 v3 v4 v5 v6 v7 v8 v9
du_'8851''45'glb_3026 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'glb_3026 v0 v1 v2 v3 v4 v5 v6
  = let v7
          = MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
              (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'737''45''8776'_100
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v7))
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
         v3 v4)
      (coe
         MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
         v2 v2)
      (coe v2) (coe du_'8851''45'idem_2732 (coe v0) (coe v1) (coe v2))
      (coe
         du_'8851''45'mono'45''8804'_2954 (coe v0) (coe v1) (coe v2)
         (coe v3) (coe v2) (coe v4) (coe v5) (coe v6))
-- Algebra.Construct.NaturalChoice.MinOp.⊓-triangulate
d_'8851''45'triangulate_3040 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8851''45'triangulate_3040 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7
  = du_'8851''45'triangulate_3040 v3 v4 v5 v6 v7
du_'8851''45'triangulate_3040 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.T_MinOperator_84 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8851''45'triangulate_3040 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
               (coe v0)))
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               v2 v3)
            v4)
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               v2
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v3 v3))
            v4)
         (coe
            MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               v2 v3)
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               v3 v4))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                  (coe v0)))
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     v3 v3))
               v4)
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               v2
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     v3 v3)
                  v4))
            (coe
               MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v2 v3)
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v3 v4))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                     (coe v0)))
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        v3 v3)
                     v4))
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v2
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     v3
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        v3 v4)))
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     v2 v3)
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     v3 v4))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                        (coe v0)))
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     v2
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        v3
                        (coe
                           MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                           v3 v4)))
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        v2 v3)
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        v3 v4))
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        v2 v3)
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        v3 v4))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
                           (coe v0)))
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        (coe
                           MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                           v2 v3)
                        (coe
                           MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                           v3 v4)))
                  (coe
                     du_'8851''45'assoc_2692 (coe v0) (coe v1) (coe v2) (coe v3)
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        v3 v4)))
               (coe
                  du_'8851''45'cong'737'_2630 (coe v0) (coe v1) (coe v2)
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        v3 v3)
                     v4)
                  (coe
                     MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                     v3
                     (coe
                        MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                        v3 v4))
                  (coe
                     du_'8851''45'assoc_2692 (coe v0) (coe v1) (coe v3) (coe v3)
                     (coe v4))))
            (coe
               du_'8851''45'assoc_2692 (coe v0) (coe v1) (coe v2)
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v3 v3)
               (coe v4)))
         (coe
            du_'8851''45'cong'691'_2668 (coe v0) (coe v1) (coe v4)
            (coe
               MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v2)
               (\ v5 v6 -> v5)
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v3 v3)
               v3)
            (coe
               MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
               (\ v5 v6 -> v6)
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v2)
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v3 v3)
               v3)
            (coe
               du_'8851''45'cong'737'_2630 (coe v0) (coe v1) (coe v2)
               (coe
                  MAlonzo.Code.Algebra.Construct.NaturalChoice.Base.d__'8851'__100 v1
                  v3 v3)
               (coe v3) (coe du_'8851''45'idem_2732 (coe v0) (coe v1) (coe v3)))))
