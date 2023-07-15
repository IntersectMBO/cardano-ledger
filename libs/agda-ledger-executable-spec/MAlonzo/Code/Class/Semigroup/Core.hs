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

module MAlonzo.Code.Class.Semigroup.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive

-- Class.Semigroup.Core.Semigroup
d_Semigroup_14 a0 a1 = ()
newtype T_Semigroup_14
  = C_Semigroup'46'constructor_41 (AgdaAny -> AgdaAny -> AgdaAny)
-- Class.Semigroup.Core.Semigroup._◇_
d__'9671'__20 :: T_Semigroup_14 -> AgdaAny -> AgdaAny -> AgdaAny
d__'9671'__20 v0
  = case coe v0 of
      C_Semigroup'46'constructor_41 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Semigroup.Core.Semigroup._<>_
d__'60''62'__22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Semigroup_14 -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''62'__22 ~v0 ~v1 v2 = du__'60''62'__22 v2
du__'60''62'__22 :: T_Semigroup_14 -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''62'__22 v0 = coe d__'9671'__20 (coe v0)
-- Class.Semigroup.Core._._<>_
d__'60''62'__26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Semigroup_14 -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''62'__26 ~v0 ~v1 v2 = du__'60''62'__26 v2
du__'60''62'__26 :: T_Semigroup_14 -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''62'__26 v0 = coe du__'60''62'__22 (coe v0)
-- Class.Semigroup.Core._._◇_
d__'9671'__28 :: T_Semigroup_14 -> AgdaAny -> AgdaAny -> AgdaAny
d__'9671'__28 v0 = coe d__'9671'__20 (coe v0)
-- Class.Semigroup.Core.SemigroupLaws
d_SemigroupLaws_40 a0 a1 a2 a3 a4 = ()
data T_SemigroupLaws_40
  = C_SemigroupLaws'46'constructor_1291 (AgdaAny ->
                                         AgdaAny -> AgdaAny)
                                        (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Class.Semigroup.Core._.Associative
d_Associative_70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Semigroup_14 ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Associative_70 = erased
-- Class.Semigroup.Core._.Commutative
d_Commutative_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Semigroup_14 ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny -> AgdaAny) -> ()
d_Commutative_74 = erased
-- Class.Semigroup.Core.SemigroupLaws.◇-comm
d_'9671''45'comm_304 ::
  T_SemigroupLaws_40 -> AgdaAny -> AgdaAny -> AgdaAny
d_'9671''45'comm_304 v0
  = case coe v0 of
      C_SemigroupLaws'46'constructor_1291 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Semigroup.Core.SemigroupLaws.◇-assocʳ
d_'9671''45'assoc'691'_306 ::
  T_SemigroupLaws_40 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'9671''45'assoc'691'_306 v0
  = case coe v0 of
      C_SemigroupLaws'46'constructor_1291 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Semigroup.Core._.◇-assocʳ
d_'9671''45'assoc'691'_310 ::
  T_SemigroupLaws_40 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'9671''45'assoc'691'_310 v0
  = coe d_'9671''45'assoc'691'_306 (coe v0)
-- Class.Semigroup.Core._.◇-comm
d_'9671''45'comm_312 ::
  T_SemigroupLaws_40 -> AgdaAny -> AgdaAny -> AgdaAny
d_'9671''45'comm_312 v0 = coe d_'9671''45'comm_304 (coe v0)
-- Class.Semigroup.Core.SemigroupLaws≡
d_SemigroupLaws'8801'_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Semigroup_14 -> ()
d_SemigroupLaws'8801'_316 = erased
-- Class.Semigroup.Core._.◇-assocˡ
d_'9671''45'assoc'737'_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Semigroup_14 ->
  T_SemigroupLaws_40 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'9671''45'assoc'737'_338 = erased
