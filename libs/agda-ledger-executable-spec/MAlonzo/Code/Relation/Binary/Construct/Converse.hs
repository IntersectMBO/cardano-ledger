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

module MAlonzo.Code.Relation.Binary.Construct.Converse where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Relation.Binary.Construct.Converse._.refl
d_refl_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_refl_42 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_refl_42 v4 v5
du_refl_42 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_refl_42 v0 v1 = coe v0 v1
-- Relation.Binary.Construct.Converse._.sym
d_sym_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_46 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_sym_46 v4 v5 v6
du_sym_46 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_46 v0 v1 v2 = coe v0 v2 v1
-- Relation.Binary.Construct.Converse._.trans
d_trans_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_50 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
  = du_trans_50 v4 v5 v6 v7 v8 v9
du_trans_50 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_50 v0 v1 v2 v3 v4 v5 = coe v0 v3 v2 v1 v5 v4
-- Relation.Binary.Construct.Converse._.asym
d_asym_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asym_54 = erased
-- Relation.Binary.Construct.Converse._.total
d_total_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_58 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_total_58 v4 v5 v6
du_total_58 ::
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_total_58 v0 v1 v2 = coe v0 v2 v1
-- Relation.Binary.Construct.Converse._.resp
d_resp_70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_resp_70 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10
  = du_resp_70 v6 v7 v8 v9 v10
du_resp_70 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_resp_70 v0 v1 v2 v3 v4 = coe v1 v2 v3 (coe v0 v3 v2 v4)
-- Relation.Binary.Construct.Converse._.max
d_max_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_max_80 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_max_80 v5
du_max_80 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_max_80 v0 = coe v0
-- Relation.Binary.Construct.Converse._.min
d_min_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_min_86 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_min_86 v5
du_min_86 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_min_86 v0 = coe v0
-- Relation.Binary.Construct.Converse._.reflexive
d_reflexive_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_104 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10
  = du_reflexive_104 v6 v7 v8 v9 v10
du_reflexive_104 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_reflexive_104 v0 v1 v2 v3 v4 = coe v1 v3 v2 (coe v0 v2 v3 v4)
-- Relation.Binary.Construct.Converse._.irrefl
d_irrefl_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_110 = erased
-- Relation.Binary.Construct.Converse._.antisym
d_antisym_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_120 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10
  = du_antisym_120 v6 v7 v8 v9 v10
du_antisym_120 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_antisym_120 v0 v1 v2 v3 v4 = coe v0 v1 v2 v4 v3
-- Relation.Binary.Construct.Converse._.compare
d_compare_124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_compare_124 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_compare_124 v6 v7 v8
du_compare_124 ::
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
du_compare_124 v0 v1 v2
  = let v3 = coe v0 v1 v2 in
    case coe v3 of
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v4
        -> coe MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v4
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v5
        -> coe MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v5
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v6
        -> coe MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Construct.Converse._.resp₂
d_resp'8322'_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_resp'8322'_186 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_resp'8322'_186 v6
du_resp'8322'_186 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_resp'8322'_186 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Construct.Converse._.dec
d_dec_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_dec_204 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 = du_dec_204 v6 v7 v8
du_dec_204 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_dec_204 v0 v1 v2 = coe v0 v2 v1
-- Relation.Binary.Construct.Converse.isEquivalence
d_isEquivalence_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_208 ~v0 ~v1 ~v2 ~v3 v4 = du_isEquivalence_208 v4
du_isEquivalence_208 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_208 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (coe
         du_refl_42
         (coe MAlonzo.Code.Relation.Binary.Structures.d_refl_34 (coe v0)))
      (coe
         du_sym_46
         (coe MAlonzo.Code.Relation.Binary.Structures.d_sym_36 (coe v0)))
      (coe
         du_trans_50
         (coe MAlonzo.Code.Relation.Binary.Structures.d_trans_38 (coe v0)))
-- Relation.Binary.Construct.Converse.isDecEquivalence
d_isDecEquivalence_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_isDecEquivalence_230 ~v0 ~v1 ~v2 ~v3 v4
  = du_isDecEquivalence_230 v4
du_isDecEquivalence_230 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
du_isDecEquivalence_230 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecEquivalence'46'constructor_2293
      (coe
         du_isEquivalence_208
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_50
            (coe v0)))
      (coe
         du_dec_204
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d__'8799'__52 (coe v0)))
-- Relation.Binary.Construct.Converse.isPreorder
d_isPreorder_256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_isPreorder_256 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_isPreorder_256 v6
du_isPreorder_256 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_isPreorder_256 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
         (coe v0))
      (coe
         du_reflexive_104
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_80
               (coe v0)))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_reflexive_82 (coe v0)))
      (coe
         du_trans_50
         (coe MAlonzo.Code.Relation.Binary.Structures.d_trans_84 (coe v0)))
-- Relation.Binary.Construct.Converse.isTotalPreorder
d_isTotalPreorder_296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
d_isTotalPreorder_296 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isTotalPreorder_296 v6
du_isTotalPreorder_296 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalPreorder_118
du_isTotalPreorder_296 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalPreorder'46'constructor_7157
      (coe
         du_isPreorder_256
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_126 (coe v0)))
      (coe
         du_total_58
         (coe MAlonzo.Code.Relation.Binary.Structures.d_total_128 (coe v0)))
-- Relation.Binary.Construct.Converse.isPartialOrder
d_isPartialOrder_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_isPartialOrder_336 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialOrder_336 v6
du_isPartialOrder_336 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_isPartialOrder_336 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe
         du_isPreorder_256
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170 (coe v0)))
      (coe
         du_antisym_120
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_antisym_172 (coe v0)))
-- Relation.Binary.Construct.Converse.isTotalOrder
d_isTotalOrder_378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
d_isTotalOrder_378 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isTotalOrder_378 v6
du_isTotalOrder_378 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsTotalOrder_380
du_isTotalOrder_378 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsTotalOrder'46'constructor_18851
      (coe
         du_isPartialOrder_336
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
            (coe v0)))
      (coe
         du_total_58
         (coe MAlonzo.Code.Relation.Binary.Structures.d_total_390 (coe v0)))
-- Relation.Binary.Construct.Converse.isDecTotalOrder
d_isDecTotalOrder_424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
d_isDecTotalOrder_424 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isDecTotalOrder_424 v6
du_isDecTotalOrder_424 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecTotalOrder_430
du_isDecTotalOrder_424 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecTotalOrder'46'constructor_20821
      (coe
         du_isTotalOrder_378
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isTotalOrder_440
            (coe v0)))
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d__'8799'__442 (coe v0))
      (coe
         du_dec_204
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
            (coe v0)))
-- Relation.Binary.Construct.Converse.isStrictPartialOrder
d_isStrictPartialOrder_484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_isStrictPartialOrder_484 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isStrictPartialOrder_484 v6
du_isStrictPartialOrder_484 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
du_isStrictPartialOrder_484 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictPartialOrder'46'constructor_12363
      (MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_278
         (coe v0))
      (coe
         du_trans_50
         (coe MAlonzo.Code.Relation.Binary.Structures.d_trans_282 (coe v0)))
      (coe
         du_resp'8322'_186
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_'60''45'resp'45''8776'_284
            (coe v0)))
-- Relation.Binary.Construct.Converse.isStrictTotalOrder
d_isStrictTotalOrder_522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_isStrictTotalOrder_522 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isStrictTotalOrder_522 v6
du_isStrictTotalOrder_522 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
du_isStrictTotalOrder_522 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictTotalOrder'46'constructor_23035
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_isEquivalence_508
         (coe v0))
      (coe
         du_trans_50
         (coe MAlonzo.Code.Relation.Binary.Structures.d_trans_510 (coe v0)))
      (coe
         du_compare_124
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_compare_512 (coe v0)))
-- Relation.Binary.Construct.Converse.setoid
d_setoid_576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_576 ~v0 ~v1 v2 = du_setoid_576 v2
du_setoid_576 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_576 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         du_isEquivalence_208
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
-- Relation.Binary.Construct.Converse.decSetoid
d_decSetoid_606 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_decSetoid_606 ~v0 ~v1 v2 = du_decSetoid_606 v2
du_decSetoid_606 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
du_decSetoid_606 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecSetoid'46'constructor_1373
      (coe
         du_isDecEquivalence_230
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isDecEquivalence_100
            (coe v0)))
-- Relation.Binary.Construct.Converse.preorder
d_preorder_642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_preorder_642 ~v0 ~v1 ~v2 v3 = du_preorder_642 v3
du_preorder_642 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_preorder_642 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe
         du_isPreorder_256
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0)))
-- Relation.Binary.Construct.Converse.totalPreorder
d_totalPreorder_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204
d_totalPreorder_698 ~v0 ~v1 ~v2 v3 = du_totalPreorder_698 v3
du_totalPreorder_698 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalPreorder_204
du_totalPreorder_698 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalPreorder'46'constructor_3645
      (coe
         du_isTotalPreorder_296
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalPreorder_226
            (coe v0)))
-- Relation.Binary.Construct.Converse.poset
d_poset_760 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_760 ~v0 ~v1 ~v2 v3 = du_poset_760 v3
du_poset_760 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_760 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      (coe
         du_isPartialOrder_336
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
            (coe v0)))
-- Relation.Binary.Construct.Converse.totalOrder
d_totalOrder_822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_totalOrder_822 ~v0 ~v1 ~v2 v3 = du_totalOrder_822 v3
du_totalOrder_822 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
du_totalOrder_822 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_TotalOrder'46'constructor_12355
      (coe
         du_isTotalOrder_378
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670 (coe v0)))
-- Relation.Binary.Construct.Converse.decTotalOrder
d_decTotalOrder_894 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
d_decTotalOrder_894 ~v0 ~v1 ~v2 v3 = du_decTotalOrder_894 v3
du_decTotalOrder_894 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736
du_decTotalOrder_894 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecTotalOrder'46'constructor_14197
      (coe
         du_isDecTotalOrder_424
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
            (coe v0)))
-- Relation.Binary.Construct.Converse.strictPartialOrder
d_strictPartialOrder_982 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_strictPartialOrder_982 ~v0 ~v1 ~v2 v3
  = du_strictPartialOrder_982 v3
du_strictPartialOrder_982 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
du_strictPartialOrder_982 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      (coe
         du_isStrictPartialOrder_484
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isStrictPartialOrder_494
            (coe v0)))
-- Relation.Binary.Construct.Converse.strictTotalOrder
d_strictTotalOrder_1038 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_strictTotalOrder_1038 ~v0 ~v1 ~v2 v3
  = du_strictTotalOrder_1038 v3
du_strictTotalOrder_1038 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
du_strictTotalOrder_1038 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictTotalOrder'46'constructor_16593
      (coe
         du_isStrictTotalOrder_522
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isStrictTotalOrder_882
            (coe v0)))
