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

module MAlonzo.Code.Interface.MonadReader.Instance where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadReader

-- Interface.MonadReader.Instance._.ask
d_ask_8 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
d_ask_8 v0
  = coe MAlonzo.Code.Interface.MonadReader.d_ask_66 (coe v0)
-- Interface.MonadReader.Instance._.local
d_local_10 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_local_10 v0
  = coe MAlonzo.Code.Interface.MonadReader.d_local_72 (coe v0)
-- Interface.MonadReader.Instance._.reader
d_reader_12 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny
d_reader_12 v0 ~v1 ~v2 v3 v4 = du_reader_12 v0 v3 v4
du_reader_12 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny
du_reader_12 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Interface.MonadReader.du_reader_78 (coe v0) (coe v1)
      (coe v2) v3 v5
