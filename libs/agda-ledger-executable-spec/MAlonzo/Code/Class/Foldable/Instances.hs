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

module MAlonzo.Code.Class.Foldable.Instances where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Foldable.Core
import qualified MAlonzo.Code.Class.Monoid.Core
import qualified MAlonzo.Code.Class.Semigroup.Core
import qualified MAlonzo.Code.Data.List.NonEmpty.Base

-- Class.Foldable.Instances.Foldable-Maybe
d_Foldable'45'Maybe_10 ::
  MAlonzo.Code.Class.Foldable.Core.T_Foldable_12
d_Foldable'45'Maybe_10
  = coe
      MAlonzo.Code.Class.Foldable.Core.C_Foldable'46'constructor_141
      (coe
         (\ v0 v1 v2 v3 v4 v5 v6 ->
            case coe v6 of
              MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v7 -> coe v5 v7
              MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                -> coe MAlonzo.Code.Class.Monoid.Core.d_ε_24 (coe v4)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Class.Foldable.Instances.Foldable-List
d_Foldable'45'List_18 ::
  MAlonzo.Code.Class.Foldable.Core.T_Foldable_12
d_Foldable'45'List_18
  = coe
      MAlonzo.Code.Class.Foldable.Core.C_Foldable'46'constructor_141
      (coe
         (\ v0 v1 v2 v3 v4 v5 ->
            coe du_go_30 (coe v0) (coe v1) (coe v4) (coe v5)))
-- Class.Foldable.Instances._.go
d_go_30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
d_go_30 v0 v1 ~v2 ~v3 v4 v5 = du_go_30 v0 v1 v4 v5
du_go_30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
du_go_30 v0 v1 v2 v3
  = coe du_'46'extendedlambda0_32 (coe v0) (coe v1) (coe v2) (coe v3)
-- Class.Foldable.Instances._..extendedlambda0
d_'46'extendedlambda0_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
d_'46'extendedlambda0_32 v0 v1 ~v2 ~v3 v4 v5 v6
  = du_'46'extendedlambda0_32 v0 v1 v4 v5 v6
du_'46'extendedlambda0_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
du_'46'extendedlambda0_32 v0 v1 v2 v3 v4
  = case coe v4 of
      [] -> coe MAlonzo.Code.Class.Monoid.Core.d_ε_24 (coe v2)
      (:) v5 v6
        -> coe
             MAlonzo.Code.Class.Semigroup.Core.d__'9671'__20
             (MAlonzo.Code.Class.Monoid.Core.d_sm_22 (coe v2)) (coe v3 v5)
             (coe du_go_30 v0 v1 v2 v3 v6)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Foldable.Instances.Foldable-List⁺
d_Foldable'45'List'8314'_38 ::
  MAlonzo.Code.Class.Foldable.Core.T_Foldable_12
d_Foldable'45'List'8314'_38
  = coe
      MAlonzo.Code.Class.Foldable.Core.C_Foldable'46'constructor_141
      (coe
         (\ v0 v1 v2 v3 v4 v5 v6 ->
            case coe v6 of
              MAlonzo.Code.Data.List.NonEmpty.Base.C__'8759'__34 v7 v8
                -> coe
                     MAlonzo.Code.Class.Semigroup.Core.d__'9671'__20
                     (MAlonzo.Code.Class.Monoid.Core.d_sm_22 (coe v4)) (coe v5 v7)
                     (coe du_go_30 v0 v1 v4 v5 v8)
              _ -> MAlonzo.RTE.mazUnreachableError))
