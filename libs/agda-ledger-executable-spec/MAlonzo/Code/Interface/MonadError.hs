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

module MAlonzo.Code.Interface.MonadError where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Interface.Monad

-- _._>>=_
d__'62''62''61'__18 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61'__18 v0
  = coe MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 (coe v0)
-- _.return
d_return_22 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_return_22 v0
  = coe MAlonzo.Code.Interface.Monad.d_return_28 (coe v0)
-- Interface.MonadError.MonadError
d_MonadError_46 a0 a1 a2 = ()
data T_MonadError_46
  = C_MonadError'46'constructor_643 (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                     () -> AgdaAny -> AgdaAny)
                                    (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                     () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny)
-- Interface.MonadError.MonadError.error
d_error_56 ::
  T_MonadError_46 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_error_56 v0
  = case coe v0 of
      C_MonadError'46'constructor_643 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadError.MonadError.catch
d_catch_58 ::
  T_MonadError_46 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_catch_58 v0
  = case coe v0 of
      C_MonadError'46'constructor_643 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadError.MonadError-TC
d_MonadError'45'TC_60 :: T_MonadError_46
d_MonadError'45'TC_60
  = coe
      C_MonadError'46'constructor_643
      (coe
         (\ v0 v1 ->
            coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334 v0 erased))
      (coe
         (\ v0 v1 v2 v3 ->
            coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_catchTC_348 v0 erased v2
              (coe
                 v3
                 (coe
                    MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
                       (coe
                          ("TC doesn't provide which error to catch" :: Data.Text.Text)))))))
-- Interface.MonadError.ErrorT
d_ErrorT_74 ::
  () ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()
d_ErrorT_74 = erased
-- Interface.MonadError._.Monad-ErrorT
d_Monad'45'ErrorT_94 ::
  () ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.Monad.T_Monad_20
d_Monad'45'ErrorT_94 ~v0 ~v1 v2 = du_Monad'45'ErrorT_94 v2
du_Monad'45'ErrorT_94 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.Monad.T_Monad_20
du_Monad'45'ErrorT_94 v0
  = coe
      MAlonzo.Code.Interface.Monad.C_Monad'46'constructor_273
      (coe
         (\ v1 v2 v3 ->
            coe
              MAlonzo.Code.Interface.Monad.d_return_28 v0 v1 erased
              (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 (coe v3))))
      (coe
         (\ v1 v2 v3 v4 v5 v6 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 v1 erased v3
              erased v5
              (\ v7 ->
                 case coe v7 of
                   MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v8
                     -> coe MAlonzo.Code.Interface.Monad.d_return_28 v0 v3 erased v7
                   MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v8 -> coe v6 v8
                   _ -> MAlonzo.RTE.mazUnreachableError)))
-- Interface.MonadError._.MonadError-ErrorT
d_MonadError'45'ErrorT_108 ::
  () ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 -> T_MonadError_46
d_MonadError'45'ErrorT_108 ~v0 ~v1 v2
  = du_MonadError'45'ErrorT_108 v2
du_MonadError'45'ErrorT_108 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 -> T_MonadError_46
du_MonadError'45'ErrorT_108 v0
  = coe
      C_MonadError'46'constructor_643
      (coe
         (\ v1 v2 v3 ->
            coe
              MAlonzo.Code.Interface.Monad.d_return_28 v0 v1 erased
              (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v3))))
      (coe
         (\ v1 v2 v3 v4 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 v1 erased v1
              erased v3
              (\ v5 ->
                 case coe v5 of
                   MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v6 -> coe v4 v6
                   MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v6
                     -> coe MAlonzo.Code.Interface.Monad.d_return_28 v0 v1 erased v5
                   _ -> MAlonzo.RTE.mazUnreachableError)))
