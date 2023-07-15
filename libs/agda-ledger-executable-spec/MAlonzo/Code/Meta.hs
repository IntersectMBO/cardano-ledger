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

module MAlonzo.Code.Meta where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Interface.MonadTC
import qualified MAlonzo.Code.Reflection.TCI

-- Meta.iMonad-TC
d_iMonad'45'TC_2 :: MAlonzo.Code.Interface.Monad.T_Monad_20
d_iMonad'45'TC_2 = coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
-- Meta.iMonadTC-TCI
d_iMonadTC'45'TCI_4 :: MAlonzo.Code.Interface.MonadTC.T_MonadTC_104
d_iMonadTC'45'TCI_4
  = coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
-- Meta.iMonadReader-TC
d_iMonadReader'45'TC_6 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50
d_iMonadReader'45'TC_6
  = coe MAlonzo.Code.Reflection.TCI.d_MonadReader'45'TC_20
-- Meta.iMonadError-TC
d_iMonadError'45'TC_8 ::
  MAlonzo.Code.Interface.MonadError.T_MonadError_46
d_iMonadError'45'TC_8
  = coe MAlonzo.Code.Reflection.TCI.d_MonadError'45'TC_22
