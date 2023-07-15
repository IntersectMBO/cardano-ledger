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

module MAlonzo.Code.Class.Semigroup.Instances where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Semigroup.Core
import qualified MAlonzo.Code.Data.Integer.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.NonEmpty.Base
import qualified MAlonzo.Code.Data.String.Base
import qualified MAlonzo.Code.Data.Vec.Base

-- Class.Semigroup.Instances.Semigroup-List
d_Semigroup'45'List_10 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
d_Semigroup'45'List_10 ~v0 ~v1 = du_Semigroup'45'List_10
du_Semigroup'45'List_10 ::
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
du_Semigroup'45'List_10
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
      (coe MAlonzo.Code.Data.List.Base.du__'43''43'__62)
-- Class.Semigroup.Instances.Semigroup-List⁺
d_Semigroup'45'List'8314'_12 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
d_Semigroup'45'List'8314'_12 ~v0 ~v1
  = du_Semigroup'45'List'8314'_12
du_Semigroup'45'List'8314'_12 ::
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
du_Semigroup'45'List'8314'_12
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
      (coe
         MAlonzo.Code.Data.List.NonEmpty.Base.du__'8314''43''43''8314'__178)
-- Class.Semigroup.Instances.Semigroup-∃Vec
d_Semigroup'45''8707'Vec_14 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
d_Semigroup'45''8707'Vec_14 ~v0 ~v1 = du_Semigroup'45''8707'Vec_14
du_Semigroup'45''8707'Vec_14 ::
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
du_Semigroup'45''8707'Vec_14
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
      (coe
         (\ v0 ->
            case coe v0 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
                -> coe
                     (\ v3 ->
                        case coe v3 of
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                            -> coe
                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                 (coe addInt (coe v1) (coe v4))
                                 (coe
                                    MAlonzo.Code.Data.Vec.Base.du__'43''43'__188 (coe v2) (coe v5))
                          _ -> MAlonzo.RTE.mazUnreachableError)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Class.Semigroup.Instances.Semigroup-String
d_Semigroup'45'String_20 ::
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
d_Semigroup'45'String_20
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
      (coe MAlonzo.Code.Data.String.Base.d__'43''43'__20)
-- Class.Semigroup.Instances.Semigroup-Maybe
d_Semigroup'45'Maybe_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14 ->
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
d_Semigroup'45'Maybe_22 ~v0 ~v1 v2 = du_Semigroup'45'Maybe_22 v2
du_Semigroup'45'Maybe_22 ::
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14 ->
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
du_Semigroup'45'Maybe_22 v0
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
      (coe
         (\ v1 v2 ->
            case coe v1 of
              MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v3
                -> case coe v2 of
                     MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v4
                       -> coe
                            MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                            (coe MAlonzo.Code.Class.Semigroup.Core.d__'9671'__20 v0 v3 v4)
                     MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v1
                     _ -> MAlonzo.RTE.mazUnreachableError
              MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v2
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Class.Semigroup.Instances.SemigroupLaws-Maybe
d_SemigroupLaws'45'Maybe_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14 ->
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40 ->
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40
d_SemigroupLaws'45'Maybe_36 ~v0 ~v1 ~v2 ~v3
  = du_SemigroupLaws'45'Maybe_36
du_SemigroupLaws'45'Maybe_36 ::
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40
du_SemigroupLaws'45'Maybe_36
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_SemigroupLaws'46'constructor_1291
      erased erased
-- Class.Semigroup.Instances._._.Associative
d_Associative_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14 ->
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40 ->
  (Maybe AgdaAny -> Maybe AgdaAny -> Maybe AgdaAny) -> ()
d_Associative_66 = erased
-- Class.Semigroup.Instances._._.Commutative
d_Commutative_70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14 ->
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40 ->
  (Maybe AgdaAny -> Maybe AgdaAny -> Maybe AgdaAny) -> ()
d_Commutative_70 = erased
-- Class.Semigroup.Instances._.p
d_p_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14 ->
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40 ->
  Maybe AgdaAny ->
  Maybe AgdaAny ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_p_170 = erased
-- Class.Semigroup.Instances._.q
d_q_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14 ->
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40 ->
  Maybe AgdaAny ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_q_172 = erased
-- Class.Semigroup.Instances._.Semigroup-ℕ-+
d_Semigroup'45'ℕ'45''43'_182 ::
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
d_Semigroup'45'ℕ'45''43'_182
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
      (coe addInt)
-- Class.Semigroup.Instances._.SemigroupLaws-ℕ-+
d_SemigroupLaws'45'ℕ'45''43'_186 ::
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40
d_SemigroupLaws'45'ℕ'45''43'_186
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_SemigroupLaws'46'constructor_1291
      erased erased
-- Class.Semigroup.Instances._.Semigroup-ℕ-*
d_Semigroup'45'ℕ'45''42'_194 ::
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
d_Semigroup'45'ℕ'45''42'_194
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
      (coe mulInt)
-- Class.Semigroup.Instances._.SemigroupLaws-ℕ-*
d_SemigroupLaws'45'ℕ'45''42'_198 ::
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40
d_SemigroupLaws'45'ℕ'45''42'_198
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_SemigroupLaws'46'constructor_1291
      erased erased
-- Class.Semigroup.Instances._.Semigroup-ℤ-+
d_Semigroup'45'ℤ'45''43'_210 ::
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
d_Semigroup'45'ℤ'45''43'_210
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
      (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276)
-- Class.Semigroup.Instances._.SemigroupLaws-ℤ-+
d_SemigroupLaws'45'ℤ'45''43'_214 ::
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40
d_SemigroupLaws'45'ℤ'45''43'_214
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_SemigroupLaws'46'constructor_1291
      erased erased
-- Class.Semigroup.Instances._.Semigroup-ℤ-*
d_Semigroup'45'ℤ'45''42'_222 ::
  MAlonzo.Code.Class.Semigroup.Core.T_Semigroup_14
d_Semigroup'45'ℤ'45''42'_222
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
      (coe MAlonzo.Code.Data.Integer.Base.d__'42'__308)
-- Class.Semigroup.Instances._.SemigroupLaws-ℤ-*
d_SemigroupLaws'45'ℤ'45''42'_226 ::
  MAlonzo.Code.Class.Semigroup.Core.T_SemigroupLaws_40
d_SemigroupLaws'45'ℤ'45''42'_226
  = coe
      MAlonzo.Code.Class.Semigroup.Core.C_SemigroupLaws'46'constructor_1291
      erased erased
