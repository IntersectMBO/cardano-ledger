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

module MAlonzo.Code.Relation.Nullary.Negation.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Empty.Irrelevant
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sum.Base

-- Relation.Nullary.Negation.Core.¬_
d_'172'__24 :: MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()
d_'172'__24 = erased
-- Relation.Nullary.Negation.Core.DoubleNegation
d_DoubleNegation_28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()
d_DoubleNegation_28 = erased
-- Relation.Nullary.Negation.Core.Stable
d_Stable_32 :: MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()
d_Stable_32 = erased
-- Relation.Nullary.Negation.Core._¬-⊎_
d__'172''45''8846'__36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d__'172''45''8846'__36 = erased
-- Relation.Nullary.Negation.Core.contradiction
d_contradiction_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny
d_contradiction_38 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 = du_contradiction_38
du_contradiction_38 :: AgdaAny
du_contradiction_38
  = coe MAlonzo.Code.Data.Empty.Irrelevant.du_'8869''45'elim_10
-- Relation.Nullary.Negation.Core.contradiction₂
d_contradiction'8322'_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny
d_contradiction'8322'_44 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8
  = du_contradiction'8322'_44 v6
du_contradiction'8322'_44 ::
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_contradiction'8322'_44 v0
  = coe seq (coe v0) (coe du_contradiction_38)
-- Relation.Nullary.Negation.Core.contraposition
d_contraposition_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_contraposition_58 = erased
-- Relation.Nullary.Negation.Core.stable
d_stable_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  ((((AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
     MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
    AgdaAny) ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_stable_68 = erased
-- Relation.Nullary.Negation.Core.negated-stable
d_negated'45'stable_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (((AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
    MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_negated'45'stable_74 = erased
-- Relation.Nullary.Negation.Core.¬¬-map
d_'172''172''45'map_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  ((AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'172''172''45'map_82 = erased
