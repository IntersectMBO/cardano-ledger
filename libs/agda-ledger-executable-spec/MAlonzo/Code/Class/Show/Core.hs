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

module MAlonzo.Code.Class.Show.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive

-- Class.Show.Core.Show
d_Show_10 a0 a1 = ()
newtype T_Show_10
  = C_mkShow_20 (AgdaAny ->
                 MAlonzo.Code.Agda.Builtin.String.T_String_6)
-- Class.Show.Core.Show.show
d_show_18 ::
  T_Show_10 -> AgdaAny -> MAlonzo.Code.Agda.Builtin.String.T_String_6
d_show_18 v0
  = case coe v0 of
      C_mkShow_20 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Show.Core._.show
d_show_24 ::
  T_Show_10 -> AgdaAny -> MAlonzo.Code.Agda.Builtin.String.T_String_6
d_show_24 v0 = coe d_show_18 (coe v0)
