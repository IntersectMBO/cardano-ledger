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

module MAlonzo.Code.Foreign.Haskell.Either where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Sum.Base

type AgdaEither l1 l2 a b = Either a b
-- Foreign.Haskell.Either.Either
d_Either_22 a0 a1 a2 a3 = ()
type T_Either_22 a0 a1 a2 a3 =
  MAlonzo.Code.Foreign.Haskell.Either.AgdaEither a0 a1 a2 a3
pattern C_left_28 a0 = Left a0
pattern C_right_30 a0 = Right a0
check_left_28 ::
  forall xa.
    forall xb. forall xA. forall xB. xA -> T_Either_22 xa xb xA xB
check_left_28 = Left
check_right_30 ::
  forall xa.
    forall xb. forall xA. forall xB. xB -> T_Either_22 xa xb xA xB
check_right_30 = Right
cover_Either_22 ::
  MAlonzo.Code.Foreign.Haskell.Either.AgdaEither a1 a2 a3 a4 -> ()
cover_Either_22 x
  = case x of
      Left _ -> ()
      Right _ -> ()
-- Foreign.Haskell.Either.toForeign
d_toForeign_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  T_Either_22 AgdaAny AgdaAny AgdaAny AgdaAny
d_toForeign_32 ~v0 ~v1 ~v2 ~v3 v4 = du_toForeign_32 v4
du_toForeign_32 ::
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  T_Either_22 AgdaAny AgdaAny AgdaAny AgdaAny
du_toForeign_32 v0
  = case coe v0 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v1
        -> coe C_left_28 (coe v1)
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v1
        -> coe C_right_30 (coe v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Foreign.Haskell.Either.fromForeign
d_fromForeign_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Either_22 AgdaAny AgdaAny AgdaAny AgdaAny ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_fromForeign_38 ~v0 ~v1 ~v2 ~v3 v4 = du_fromForeign_38 v4
du_fromForeign_38 ::
  T_Either_22 AgdaAny AgdaAny AgdaAny AgdaAny ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_fromForeign_38 v0
  = case coe v0 of
      C_left_28 v1
        -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v1)
      C_right_30 v1
        -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 (coe v1)
      _ -> MAlonzo.RTE.mazUnreachableError
