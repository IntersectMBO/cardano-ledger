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

module MAlonzo.Code.Data.List.Relation.Unary.Unique.Propositional.Properties.WithK where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Function.Equivalence
import qualified MAlonzo.Code.Function.Inverse

-- Data.List.Relation.Unary.Unique.Propositional.Properties.WithK.unique⇒∈-prop
d_unique'8658''8712''45'prop_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unique'8658''8712''45'prop_26 = erased
-- Data.List.Relation.Unary.Unique.Propositional.Properties.WithK.unique∧set⇒bag
d_unique'8743'set'8658'bag_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  (AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16) ->
  AgdaAny -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_unique'8743'set'8658'bag_64 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_unique'8743'set'8658'bag_64 v6 v7
du_unique'8743'set'8658'bag_64 ::
  (AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16) ->
  AgdaAny -> MAlonzo.Code.Function.Inverse.T_Inverse_58
du_unique'8743'set'8658'bag_64 v0 v1
  = coe
      MAlonzo.Code.Function.Inverse.C_Inverse'46'constructor_3553
      (coe MAlonzo.Code.Function.Equivalence.d_to_34 (coe v0 v1))
      (coe MAlonzo.Code.Function.Equivalence.d_from_36 (coe v0 v1))
      (coe
         MAlonzo.Code.Function.Inverse.C__InverseOf_'46'constructor_2103
         erased erased)
