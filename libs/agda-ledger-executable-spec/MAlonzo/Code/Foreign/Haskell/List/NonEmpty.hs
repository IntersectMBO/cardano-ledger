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

module MAlonzo.Code.Foreign.Haskell.List.NonEmpty where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List

import qualified Data.List.NonEmpty as NE
type AgdaNonEmpty l a = NE.NonEmpty a
-- Foreign.Haskell.List.NonEmpty.NonEmpty
d_NonEmpty_16 a0 a1 = ()
type T_NonEmpty_16 a0 a1 = AgdaNonEmpty a0 a1
pattern C__'8759'__20 a0 a1 = (NE.:|) a0 a1
check__'8759'__20 ::
  forall xa.
    forall xA.
      xA ->
      MAlonzo.Code.Agda.Builtin.List.T_List_10 xa xA ->
      T_NonEmpty_16 xa xA
check__'8759'__20 = (NE.:|)
cover_NonEmpty_16 :: AgdaNonEmpty a1 a2 -> ()
cover_NonEmpty_16 x
  = case x of
      (NE.:|) _ _ -> ()
