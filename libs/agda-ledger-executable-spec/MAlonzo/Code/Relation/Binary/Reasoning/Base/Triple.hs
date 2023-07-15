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

module MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Relation.Binary.Reasoning.Base.Triple._IsRelatedTo_
d__IsRelatedTo__70 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
                   a14 a15
  = ()
data T__IsRelatedTo__70
  = C_strict_78 AgdaAny | C_nonstrict_82 AgdaAny |
    C_equals_86 AgdaAny
-- Relation.Binary.Reasoning.Base.Triple.IsStrict
d_IsStrict_92 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
              a16
  = ()
data T_IsStrict_92 = C_isStrict_100
-- Relation.Binary.Reasoning.Base.Triple.IsStrict?
d_IsStrict'63'_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsStrict'63'_108 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                   ~v11 ~v12 ~v13 ~v14 ~v15 v16
  = du_IsStrict'63'_108 v16
du_IsStrict'63'_108 ::
  T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_IsStrict'63'_108 v0
  = case coe v0 of
      C_strict_78 v1
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe
                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                (coe C_isStrict_100))
      C_nonstrict_82 v1
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      C_equals_86 v1
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Triple.extractStrict
d_extractStrict_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__70 -> T_IsStrict_92 -> AgdaAny
d_extractStrict_118 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                    ~v11 ~v12 ~v13 ~v14 ~v15 v16 v17
  = du_extractStrict_118 v16 v17
du_extractStrict_118 ::
  T__IsRelatedTo__70 -> T_IsStrict_92 -> AgdaAny
du_extractStrict_118 v0 v1
  = coe
      seq (coe v1)
      (case coe v0 of
         C_strict_78 v2 -> coe v2
         _ -> MAlonzo.RTE.mazUnreachableError)
-- Relation.Binary.Reasoning.Base.Triple.IsEquality
d_IsEquality_126 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
                 a15 a16
  = ()
data T_IsEquality_126 = C_isEquality_134
-- Relation.Binary.Reasoning.Base.Triple.IsEquality?
d_IsEquality'63'_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsEquality'63'_142 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                     ~v11 ~v12 ~v13 ~v14 ~v15 v16
  = du_IsEquality'63'_142 v16
du_IsEquality'63'_142 ::
  T__IsRelatedTo__70 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_IsEquality'63'_142 v0
  = case coe v0 of
      C_strict_78 v1
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      C_nonstrict_82 v1
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      C_equals_86 v1
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe
                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                (coe C_isEquality_134))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Triple.extractEquality
d_extractEquality_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__70 -> T_IsEquality_126 -> AgdaAny
d_extractEquality_152 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                      ~v11 ~v12 ~v13 ~v14 ~v15 v16 v17
  = du_extractEquality_152 v16 v17
du_extractEquality_152 ::
  T__IsRelatedTo__70 -> T_IsEquality_126 -> AgdaAny
du_extractEquality_152 v0 v1
  = coe
      seq (coe v1)
      (case coe v0 of
         C_equals_86 v2 -> coe v2
         _ -> MAlonzo.RTE.mazUnreachableError)
-- Relation.Binary.Reasoning.Base.Triple.begin_
d_begin__160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T__IsRelatedTo__70 -> AgdaAny
d_begin__160 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9 ~v10 v11 ~v12
             ~v13 v14 v15 v16
  = du_begin__160 v8 v11 v14 v15 v16
du_begin__160 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T__IsRelatedTo__70 -> AgdaAny
du_begin__160 v0 v1 v2 v3 v4
  = case coe v4 of
      C_strict_78 v5 -> coe v1 v2 v3 v5
      C_nonstrict_82 v5 -> coe v5
      C_equals_86 v5
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82 v0 v2 v3 v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Triple.begin-strict_
d_begin'45'strict__176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T__IsRelatedTo__70 -> AgdaAny -> AgdaAny
d_begin'45'strict__176 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                       ~v11 ~v12 ~v13 ~v14 ~v15 v16 ~v17
  = du_begin'45'strict__176 v16
du_begin'45'strict__176 :: T__IsRelatedTo__70 -> AgdaAny
du_begin'45'strict__176 v0
  = coe
      du_extractStrict_118 (coe v0)
      (coe
         MAlonzo.Code.Relation.Nullary.Decidable.Core.du_toWitness_108
         (coe du_IsStrict'63'_108 (coe v0)))
-- Relation.Binary.Reasoning.Base.Triple.begin-equality_
d_begin'45'equality__190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> T__IsRelatedTo__70 -> AgdaAny -> AgdaAny
d_begin'45'equality__190 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                         ~v10 ~v11 ~v12 ~v13 ~v14 ~v15 v16 ~v17
  = du_begin'45'equality__190 v16
du_begin'45'equality__190 :: T__IsRelatedTo__70 -> AgdaAny
du_begin'45'equality__190 v0
  = coe
      du_extractEquality_152 (coe v0)
      (coe
         MAlonzo.Code.Relation.Nullary.Decidable.Core.du_toWitness_108
         (coe du_IsEquality'63'_142 (coe v0)))
-- Relation.Binary.Reasoning.Base.Triple.step-<
d_step'45''60'_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__70 -> AgdaAny -> T__IsRelatedTo__70
d_step'45''60'_202 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10 ~v11
                   v12 ~v13 v14 v15 v16 v17 v18
  = du_step'45''60'_202 v9 v10 v12 v14 v15 v16 v17 v18
du_step'45''60'_202 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__70 -> AgdaAny -> T__IsRelatedTo__70
du_step'45''60'_202 v0 v1 v2 v3 v4 v5 v6 v7
  = case coe v6 of
      C_strict_78 v8 -> coe C_strict_78 (coe v0 v3 v4 v5 v7 v8)
      C_nonstrict_82 v8 -> coe C_strict_78 (coe v2 v3 v4 v5 v7 v8)
      C_equals_86 v8
        -> coe
             C_strict_78
             (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 v1 v3 v4 v5 v8 v7)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Triple.step-≤
d_step'45''8804'_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__70 -> AgdaAny -> T__IsRelatedTo__70
d_step'45''8804'_228 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9 ~v10
                     ~v11 ~v12 v13 v14 v15 v16 v17 v18
  = du_step'45''8804'_228 v8 v13 v14 v15 v16 v17 v18
du_step'45''8804'_228 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__70 -> AgdaAny -> T__IsRelatedTo__70
du_step'45''8804'_228 v0 v1 v2 v3 v4 v5 v6
  = case coe v5 of
      C_strict_78 v7 -> coe C_strict_78 (coe v1 v2 v3 v4 v6 v7)
      C_nonstrict_82 v7
        -> coe
             C_nonstrict_82
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_trans_84 v0 v2 v3 v4 v6
                v7)
      C_equals_86 v7
        -> coe
             C_nonstrict_82
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
                   (coe v0))
                v2 v3 v4 v7 v6)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Triple.step-≈
d_step'45''8776'_254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__70 -> AgdaAny -> T__IsRelatedTo__70
d_step'45''8776'_254 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9 v10
                     ~v11 ~v12 ~v13 v14 v15 v16 v17 v18
  = du_step'45''8776'_254 v8 v10 v14 v15 v16 v17 v18
du_step'45''8776'_254 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__70 -> AgdaAny -> T__IsRelatedTo__70
du_step'45''8776'_254 v0 v1 v2 v3 v4 v5 v6
  = case coe v5 of
      C_strict_78 v7
        -> coe
             C_strict_78
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v1 v4 v3 v2
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                   (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                      (coe v0))
                   v2 v3 v6)
                v7)
      C_nonstrict_82 v7
        -> coe
             C_nonstrict_82
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.du_'8764''45'resp'45''8776'_112
                   (coe v0))
                v4 v3 v2
                (coe
                   MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                   (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                      (coe v0))
                   v2 v3 v6)
                v7)
      C_equals_86 v7
        -> coe
             C_equals_86
             (coe
                MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
                   (coe v0))
                v2 v3 v4 v6 v7)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Triple.step-≈˘
d_step'45''8776''728'_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__70 -> AgdaAny -> T__IsRelatedTo__70
d_step'45''8776''728'_280 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9
                          v10 ~v11 ~v12 ~v13 v14 v15 v16 v17 v18
  = du_step'45''8776''728'_280 v8 v10 v14 v15 v16 v17 v18
du_step'45''8776''728'_280 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__70 -> AgdaAny -> T__IsRelatedTo__70
du_step'45''8776''728'_280 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_step'45''8776'_254 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
      (coe v5)
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_sym_36
         (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
            (coe v0))
         v3 v2 v6)
-- Relation.Binary.Reasoning.Base.Triple.step-≡
d_step'45''8801'_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T__IsRelatedTo__70
d_step'45''8801'_294 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10
                     ~v11 ~v12 ~v13 ~v14 ~v15 ~v16 v17 ~v18
  = du_step'45''8801'_294 v17
du_step'45''8801'_294 :: T__IsRelatedTo__70 -> T__IsRelatedTo__70
du_step'45''8801'_294 v0 = coe v0
-- Relation.Binary.Reasoning.Base.Triple.step-≡˘
d_step'45''8801''728'_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  T__IsRelatedTo__70 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T__IsRelatedTo__70
d_step'45''8801''728'_326 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                          ~v10 ~v11 ~v12 ~v13 ~v14 ~v15 ~v16 v17 ~v18
  = du_step'45''8801''728'_326 v17
du_step'45''8801''728'_326 ::
  T__IsRelatedTo__70 -> T__IsRelatedTo__70
du_step'45''8801''728'_326 v0 = coe v0
-- Relation.Binary.Reasoning.Base.Triple._≡⟨⟩_
d__'8801''10216''10217'__338 ::
  T__IsRelatedTo__70 -> T__IsRelatedTo__70
d__'8801''10216''10217'__338 v0 = coe v0
-- Relation.Binary.Reasoning.Base.Triple._∎
d__'8718'_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T__IsRelatedTo__70
d__'8718'_346 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9 ~v10 ~v11 ~v12
              ~v13 v14
  = du__'8718'_346 v8 v14
du__'8718'_346 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  AgdaAny -> T__IsRelatedTo__70
du__'8718'_346 v0 v1
  = coe
      C_equals_86
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
            (coe v0))
         v1)
