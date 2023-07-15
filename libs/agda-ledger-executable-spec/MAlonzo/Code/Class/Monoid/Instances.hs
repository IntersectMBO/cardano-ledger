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

module MAlonzo.Code.Class.Monoid.Instances where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Monoid.Core
import qualified MAlonzo.Code.Class.Semigroup.Core
import qualified MAlonzo.Code.Class.Semigroup.Instances
import qualified MAlonzo.Code.Data.Integer.Base
import qualified MAlonzo.Code.Data.Vec.Base

-- Class.Monoid.Instances.Monoid-List
d_Monoid'45'List_10 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
d_Monoid'45'List_10 ~v0 ~v1 = du_Monoid'45'List_10
du_Monoid'45'List_10 :: MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
du_Monoid'45'List_10
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_Monoid'46'constructor_53
      (coe
         MAlonzo.Code.Class.Semigroup.Instances.du_Semigroup'45'List_10)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Class.Monoid.Instances.MonoidLaws-List
d_MonoidLaws'45'List_12 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40
d_MonoidLaws'45'List_12 ~v0 ~v1 = du_MonoidLaws'45'List_12
du_MonoidLaws'45'List_12 ::
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40
du_MonoidLaws'45'List_12
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_MonoidLaws'46'constructor_1215
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased)
-- Class.Monoid.Instances.Monoid-Vec
d_Monoid'45'Vec_14 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
d_Monoid'45'Vec_14 ~v0 ~v1 = du_Monoid'45'Vec_14
du_Monoid'45'Vec_14 :: MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
du_Monoid'45'Vec_14
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_Monoid'46'constructor_53
      (coe
         MAlonzo.Code.Class.Semigroup.Instances.du_Semigroup'45''8707'Vec_14)
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe (0 :: Integer))
         (coe MAlonzo.Code.Data.Vec.Base.C_'91''93'_32))
-- Class.Monoid.Instances.Monoid-String
d_Monoid'45'String_16 :: MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
d_Monoid'45'String_16
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_Monoid'46'constructor_53
      (coe
         MAlonzo.Code.Class.Semigroup.Instances.d_Semigroup'45'String_20)
      (coe ("" :: Data.Text.Text))
-- Class.Monoid.Instances.Monoid-Maybe
d_Monoid'45'Maybe_18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
d_Monoid'45'Maybe_18 ~v0 ~v1 v2 = du_Monoid'45'Maybe_18 v2
du_Monoid'45'Maybe_18 ::
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
du_Monoid'45'Maybe_18 v0
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_Monoid'46'constructor_53
      (coe
         MAlonzo.Code.Class.Semigroup.Instances.du_Semigroup'45'Maybe_22
         (coe MAlonzo.Code.Class.Monoid.Core.d_sm_22 (coe v0)))
      (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
-- Class.Monoid.Instances.MonoidLaws-Maybe
d_MonoidLaws'45'Maybe_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40 ->
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40
d_MonoidLaws'45'Maybe_22 ~v0 ~v1 ~v2 ~v3
  = du_MonoidLaws'45'Maybe_22
du_MonoidLaws'45'Maybe_22 ::
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40
du_MonoidLaws'45'Maybe_22
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_MonoidLaws'46'constructor_1215
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased)
-- Class.Monoid.Instances._.p
d_p_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40 ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_p_156 = erased
-- Class.Monoid.Instances._.q
d_q_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40 ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_q_160 = erased
-- Class.Monoid.Instances._.Monoid-ℕ-+
d_Monoid'45'ℕ'45''43'_168 ::
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
d_Monoid'45'ℕ'45''43'_168
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_Monoid'46'constructor_53
      (coe
         MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
         (coe addInt))
      (coe (0 :: Integer))
-- Class.Monoid.Instances._.MonoidLaws-ℕ-+
d_MonoidLaws'45'ℕ'45''43'_178 ::
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40
d_MonoidLaws'45'ℕ'45''43'_178
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_MonoidLaws'46'constructor_1215
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased)
-- Class.Monoid.Instances._.Monoid-ℕ-*
d_Monoid'45'ℕ'45''42'_186 ::
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
d_Monoid'45'ℕ'45''42'_186
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_Monoid'46'constructor_53
      (coe
         MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
         (coe mulInt))
      (coe (1 :: Integer))
-- Class.Monoid.Instances._.MonoidLaws-ℕ-*
d_MonoidLaws'45'ℕ'45''42'_196 ::
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40
d_MonoidLaws'45'ℕ'45''42'_196
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_MonoidLaws'46'constructor_1215
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased)
-- Class.Monoid.Instances._.Monoid-ℤ-+
d_Monoid'45'ℤ'45''43'_208 ::
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
d_Monoid'45'ℤ'45''43'_208
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_Monoid'46'constructor_53
      (coe
         MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
         (coe MAlonzo.Code.Data.Integer.Base.d__'43'__276))
      (coe MAlonzo.Code.Data.Integer.Base.d_0ℤ_12)
-- Class.Monoid.Instances._.MonoidLaws-ℤ-+
d_MonoidLaws'45'ℤ'45''43'_218 ::
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40
d_MonoidLaws'45'ℤ'45''43'_218
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_MonoidLaws'46'constructor_1215
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased)
-- Class.Monoid.Instances._.Monoid-ℤ-*
d_Monoid'45'ℤ'45''42'_226 ::
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14
d_Monoid'45'ℤ'45''42'_226
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_Monoid'46'constructor_53
      (coe
         MAlonzo.Code.Class.Semigroup.Core.C_Semigroup'46'constructor_41
         (coe MAlonzo.Code.Data.Integer.Base.d__'42'__308))
      (coe MAlonzo.Code.Data.Integer.Base.d_1ℤ_16)
-- Class.Monoid.Instances._.MonoidLaws-ℤ-*
d_MonoidLaws'45'ℤ'45''42'_236 ::
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40
d_MonoidLaws'45'ℤ'45''42'_236
  = coe
      MAlonzo.Code.Class.Monoid.Core.C_MonoidLaws'46'constructor_1215
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased)
-- Class.Monoid.Instances._.just-◇ˡ
d_just'45''9671''737'_258 ::
  () ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40 ->
  AgdaAny ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_just'45''9671''737'_258 = erased
-- Class.Monoid.Instances._.just-◇ʳ
d_just'45''9671''691'_274 ::
  () ->
  MAlonzo.Code.Class.Monoid.Core.T_Monoid_14 ->
  MAlonzo.Code.Class.Monoid.Core.T_MonoidLaws_40 ->
  AgdaAny ->
  Maybe AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_just'45''9671''691'_274 = erased
