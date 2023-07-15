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

module MAlonzo.Code.IO.Primitive where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.IO
import qualified MAlonzo.Code.Agda.Primitive

-- IO.Primitive.pure
d_pure_8 ::
  forall xa.
    forall xA.
      MAlonzo.Code.Agda.Primitive.T_Level_14 ->
      () -> xA -> MAlonzo.Code.Agda.Builtin.IO.T_IO_8 xa xA
d_pure_8 = \_ _ -> return
-- IO.Primitive._>>=_
d__'62''62''61'__18 ::
  forall xa.
    forall xb.
      forall xA.
        forall xB.
          MAlonzo.Code.Agda.Primitive.T_Level_14 ->
          MAlonzo.Code.Agda.Primitive.T_Level_14 ->
          () ->
          () ->
          MAlonzo.Code.Agda.Builtin.IO.T_IO_8 xa xA ->
          (xA -> MAlonzo.Code.Agda.Builtin.IO.T_IO_8 xb xB) ->
          MAlonzo.Code.Agda.Builtin.IO.T_IO_8 xb xB
d__'62''62''61'__18 = \_ _ _ _ -> (>>=)
