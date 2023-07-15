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

module MAlonzo.Code.Interface.MonadReader where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError

-- Interface.MonadReader._._>>=_
d__'62''62''61'__20 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61'__20 v0
  = coe MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 (coe v0)
-- Interface.MonadReader._.return
d_return_24 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_return_24 v0
  = coe MAlonzo.Code.Interface.Monad.d_return_28 (coe v0)
-- Interface.MonadReader._.catch
d_catch_36 ::
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_catch_36 v0
  = coe MAlonzo.Code.Interface.MonadError.d_catch_58 (coe v0)
-- Interface.MonadReader._.error
d_error_38 ::
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_error_38 v0
  = coe MAlonzo.Code.Interface.MonadError.d_error_56 (coe v0)
-- Interface.MonadReader.MonadReader
d_MonadReader_50 a0 a1 a2 a3 = ()
data T_MonadReader_50
  = C_MonadReader'46'constructor_595 AgdaAny
                                     (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                      () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny)
-- Interface.MonadReader.MonadReader.ask
d_ask_66 :: T_MonadReader_50 -> AgdaAny
d_ask_66 v0
  = case coe v0 of
      C_MonadReader'46'constructor_595 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadReader.MonadReader.local
d_local_72 ::
  T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_local_72 v0
  = case coe v0 of
      C_MonadReader'46'constructor_595 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadReader.MonadReader.reader
d_reader_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny
d_reader_78 v0 ~v1 ~v2 v3 v4 v5 ~v6 v7
  = du_reader_78 v0 v3 v4 v5 v7
du_reader_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> AgdaAny
du_reader_78 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v1) (coe v0)
      (coe v3) (coe v4) (coe d_ask_66 (coe v2))
-- Interface.MonadReader._.ask
d_ask_84 :: T_MonadReader_50 -> AgdaAny
d_ask_84 v0 = coe d_ask_66 (coe v0)
-- Interface.MonadReader._.local
d_local_86 ::
  T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_local_86 v0 = coe d_local_72 (coe v0)
-- Interface.MonadReader.ReaderT
d_ReaderT_98 ::
  () ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()
d_ReaderT_98 = erased
-- Interface.MonadReader._.Monad-ReaderT
d_Monad'45'ReaderT_118 ::
  () ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.Monad.T_Monad_20
d_Monad'45'ReaderT_118 ~v0 ~v1 v2 = du_Monad'45'ReaderT_118 v2
du_Monad'45'ReaderT_118 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.Monad.T_Monad_20
du_Monad'45'ReaderT_118 v0
  = coe
      MAlonzo.Code.Interface.Monad.C_Monad'46'constructor_273
      (coe
         (\ v1 v2 v3 v4 ->
            coe MAlonzo.Code.Interface.Monad.d_return_28 v0 v1 erased v3))
      (coe
         (\ v1 v2 v3 v4 v5 v6 v7 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 v1 erased v3
              erased (coe v5 v7) (\ v8 -> coe v6 v8 v7)))
-- Interface.MonadReader._.MonadReader-ReaderT
d_MonadReader'45'ReaderT_132 ::
  () ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 -> T_MonadReader_50
d_MonadReader'45'ReaderT_132 ~v0 ~v1 v2
  = du_MonadReader'45'ReaderT_132 v2
du_MonadReader'45'ReaderT_132 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 -> T_MonadReader_50
du_MonadReader'45'ReaderT_132 v0
  = coe
      C_MonadReader'46'constructor_595
      (coe MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased)
      (coe (\ v1 v2 v3 v4 v5 -> coe v4 (coe v3 v5)))
-- Interface.MonadReader._.MonadError-ReaderT
d_MonadError'45'ReaderT_144 ::
  () ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46
d_MonadError'45'ReaderT_144 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_MonadError'45'ReaderT_144 v5
du_MonadError'45'ReaderT_144 ::
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46
du_MonadError'45'ReaderT_144 v0
  = coe
      MAlonzo.Code.Interface.MonadError.C_MonadError'46'constructor_643
      (coe
         (\ v1 v2 v3 v4 ->
            coe MAlonzo.Code.Interface.MonadError.d_error_56 v0 v1 erased v3))
      (coe
         (\ v1 v2 v3 v4 v5 ->
            coe
              MAlonzo.Code.Interface.MonadError.d_catch_58 v0 v1 erased
              (coe v3 v5) (\ v6 -> coe v4 v6 v5)))
