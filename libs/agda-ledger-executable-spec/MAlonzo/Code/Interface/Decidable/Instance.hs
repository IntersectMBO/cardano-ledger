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

module MAlonzo.Code.Interface.Decidable.Instance where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Interface.Decidable.Instance.Dec₁
d_Dec'8321'_24 a0 a1 a2 a3 = ()
newtype T_Dec'8321'_24
  = C_Dec'8321''46'constructor_167 (AgdaAny ->
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
-- Interface.Decidable.Instance.Dec₁.P?
d_P'63'_34 ::
  T_Dec'8321'_24 ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_P'63'_34 v0
  = case coe v0 of
      C_Dec'8321''46'constructor_167 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.Decidable.Instance.¿_¿
d_'191'_'191'_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_'191'_'191'_38 ~v0 ~v1 v2 = du_'191'_'191'_38 v2
du_'191'_'191'_38 ::
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_'191'_'191'_38 v0 = coe v0
-- Interface.Decidable.Instance.ifᵈ_then_else_
d_if'7496'_then_else__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_if'7496'_then_else__44 v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_if'7496'_then_else__44 v0 v4 v5 v6
du_if'7496'_then_else__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_if'7496'_then_else__44 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
      (coe
         MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 v0
         erased v1)
      (coe v2) (coe v3)
-- Interface.Decidable.Instance.Decidable²⇒Dec
d_Decidable'178''8658'Dec_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_Decidable'178''8658'Dec_58 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_Decidable'178''8658'Dec_58 v4 v5 v6
du_Decidable'178''8658'Dec_58 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_Decidable'178''8658'Dec_58 v0 v1 v2 = coe v0 v1 v2
-- Interface.Decidable.Instance.Dec-⊥
d_Dec'45''8869'_66 ::
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_Dec'45''8869'_66
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
      (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
-- Interface.Decidable.Instance.Dec-⊤
d_Dec'45''8868'_70 ::
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_Dec'45''8868'_70
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
      (coe
         MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Interface.Decidable.Instance.Dec-→
d_Dec'45''8594'_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_Dec'45''8594'_72 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_Dec'45''8594'_72 v4 v5
du_Dec'45''8594'_72 ::
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_Dec'45''8594'_72 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'8594''45'dec__82
      (coe v0) (coe v1)
-- Interface.Decidable.Instance.Dec-×
d_Dec'45''215'_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_Dec'45''215'_78 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_Dec'45''215'_78 v4 v5
du_Dec'45''215'_78 ::
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_Dec'45''215'_78 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
      (coe v0) (coe v1)
-- Interface.Decidable.Instance.DecEq⇒Dec
d_DecEq'8658'Dec_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_DecEq'8658'Dec_88 ~v0 ~v1 v2 v3 v4
  = du_DecEq'8658'Dec_88 v2 v3 v4
du_DecEq'8658'Dec_88 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_DecEq'8658'Dec_88 v0 v1 v2
  = case coe v0 of
      MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63 v3
        -> coe v3 v1 v2
      _ -> MAlonzo.RTE.mazUnreachableError
