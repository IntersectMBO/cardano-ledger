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

module MAlonzo.Code.Data.Product.Properties.Ext where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Product.Function.Dependent.Propositional
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Inverse
import qualified MAlonzo.Code.Function.Related
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties

-- Data.Product.Properties.Ext.∃-cong
d_'8707''45'cong_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  () ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  (AgdaAny -> AgdaAny) -> AgdaAny
d_'8707''45'cong_32 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du_'8707''45'cong_32 v4 v9 v10
du_'8707''45'cong_32 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  (AgdaAny -> AgdaAny) -> AgdaAny
du_'8707''45'cong_32 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Related.du_fromRelated_138 (coe v0)
      (coe
         MAlonzo.Code.Data.Product.Function.Dependent.Propositional.du_cong_380
         v0 v1
         (\ v3 ->
            coe
              MAlonzo.Code.Function.Related.du_toRelated_100 (coe v0)
              (coe v2 v3)))
-- Data.Product.Properties.Ext.∃-cong′
d_'8707''45'cong'8242'_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> AgdaAny
d_'8707''45'cong'8242'_46 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7
  = du_'8707''45'cong'8242'_46 v3 v7
du_'8707''45'cong'8242'_46 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  (AgdaAny -> AgdaAny) -> AgdaAny
du_'8707''45'cong'8242'_46 v0 v1
  = coe
      du_'8707''45'cong_32 (coe v0)
      (coe
         MAlonzo.Code.Function.Inverse.du_id_186
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402))
      (coe v1)
-- Data.Product.Properties.Ext.∃-≡
d_'8707''45''8801'_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8707''45''8801'_58 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8707''45''8801'_58 v4
du_'8707''45''8801'_58 ::
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8707''45''8801'_58 v0
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
      (coe
         (\ v1 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
              (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased (coe v1))))
      (coe
         (\ v1 ->
            case coe v1 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
                -> case coe v3 of
                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5 -> coe v5
                     _ -> MAlonzo.RTE.mazUnreachableError
              _ -> MAlonzo.RTE.mazUnreachableError))
