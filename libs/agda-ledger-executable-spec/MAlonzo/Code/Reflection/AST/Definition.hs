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

module MAlonzo.Code.Reflection.AST.Definition where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Data.List.Properties
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Reflection.AST.Argument
import qualified MAlonzo.Code.Reflection.AST.Name
import qualified MAlonzo.Code.Reflection.AST.Term
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Reflection.AST.Definition.function-injective
d_function'45'injective_10 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_function'45'injective_10 = erased
-- Reflection.AST.Definition.data-type-injective₁
d_data'45'type'45'injective'8321'_20 ::
  Integer ->
  Integer ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_data'45'type'45'injective'8321'_20 = erased
-- Reflection.AST.Definition.data-type-injective₂
d_data'45'type'45'injective'8322'_30 ::
  Integer ->
  Integer ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_data'45'type'45'injective'8322'_30 = erased
-- Reflection.AST.Definition.data-type-injective
d_data'45'type'45'injective_40 ::
  Integer ->
  Integer ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_data'45'type'45'injective_40 ~v0 ~v1 ~v2 ~v3
  = du_data'45'type'45'injective_40
du_data'45'type'45'injective_40 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_data'45'type'45'injective_40
  = coe
      MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88 erased erased
-- Reflection.AST.Definition.record′-injective₁
d_record'8242''45'injective'8321'_50 ::
  AgdaAny ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_record'8242''45'injective'8321'_50 = erased
-- Reflection.AST.Definition.record′-injective₂
d_record'8242''45'injective'8322'_60 ::
  AgdaAny ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_record'8242''45'injective'8322'_60 = erased
-- Reflection.AST.Definition.record′-injective
d_record'8242''45'injective_70 ::
  AgdaAny ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_record'8242''45'injective_70 ~v0 ~v1 ~v2 ~v3
  = du_record'8242''45'injective_70
du_record'8242''45'injective_70 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_record'8242''45'injective_70
  = coe
      MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88 erased erased
-- Reflection.AST.Definition.constructor′-injective
d_constructor'8242''45'injective_76 ::
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_constructor'8242''45'injective_76 = erased
-- Reflection.AST.Definition._≟_
d__'8799'__78 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Definition_272 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Definition_272 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__78 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_function_276 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_function_276 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    erased
                    (coe
                       MAlonzo.Code.Reflection.AST.Term.d__'8799''45'Clauses__222 (coe v2)
                       (coe v3))
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v3 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v3 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'cons_292 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_axiom_294
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_prim'45'fun_296
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_function_276 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v4 v5
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    (coe MAlonzo.Code.Data.Product.Base.du_uncurry_220 erased)
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                       (coe
                          MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464 (coe v2) (coe v4))
                       (coe
                          MAlonzo.Code.Data.List.Properties.du_'8801''45'dec_54
                          (coe MAlonzo.Code.Reflection.AST.Name.d__'8799'__12) (coe v3)
                          (coe v5)))
             MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v4 v5
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'cons_292 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_axiom_294
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_prim'45'fun_296
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_function_276 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v4 v5
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v4 v5
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    (coe MAlonzo.Code.Data.Product.Base.du_uncurry_220 erased)
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                       (coe
                          MAlonzo.Code.Reflection.AST.Name.d__'8799'__12 (coe v2) (coe v4))
                       (coe
                          MAlonzo.Code.Data.List.Properties.du_'8801''45'dec_54
                          (coe
                             MAlonzo.Code.Reflection.AST.Argument.du_'8801''45'dec_96
                             (coe MAlonzo.Code.Reflection.AST.Name.d__'8799'__12))
                          (coe v3) (coe v5)))
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'cons_292 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_axiom_294
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_prim'45'fun_296
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'cons_292 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_function_276 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v3 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v3 v4
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'cons_292 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    erased
                    (coe
                       MAlonzo.Code.Reflection.AST.Name.d__'8799'__12 (coe v2) (coe v3))
             MAlonzo.Code.Agda.Builtin.Reflection.C_axiom_294
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_prim'45'fun_296
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_axiom_294
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_function_276 v2
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v2 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v2 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'cons_292 v2
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_axiom_294
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
             MAlonzo.Code.Agda.Builtin.Reflection.C_prim'45'fun_296
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_prim'45'fun_296
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_function_276 v2
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v2 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v2 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'cons_292 v2
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_axiom_294
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             MAlonzo.Code.Agda.Builtin.Reflection.C_prim'45'fun_296
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
