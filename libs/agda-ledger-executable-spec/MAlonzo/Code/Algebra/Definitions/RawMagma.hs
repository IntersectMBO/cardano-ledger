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

module MAlonzo.Code.Algebra.Definitions.RawMagma where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw

-- Algebra.Definitions.RawMagma._∣ˡ_
d__'8739''737'__22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8739''737'__22 = erased
-- Algebra.Definitions.RawMagma._∤ˡ_
d__'8740''737'__30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8740''737'__30 = erased
-- Algebra.Definitions.RawMagma._∣ʳ_
d__'8739''691'__36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8739''691'__36 = erased
-- Algebra.Definitions.RawMagma._∤ʳ_
d__'8740''691'__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8740''691'__44 = erased
-- Algebra.Definitions.RawMagma._∣_
d__'8739'__50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8739'__50 = erased
-- Algebra.Definitions.RawMagma._∤_
d__'8740'__52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8740'__52 = erased
-- Algebra.Definitions.RawMagma._∣∣_
d__'8739''8739'__58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8739''8739'__58 = erased
-- Algebra.Definitions.RawMagma._∤∤_
d__'8740''8740'__64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8740''8740'__64 = erased
