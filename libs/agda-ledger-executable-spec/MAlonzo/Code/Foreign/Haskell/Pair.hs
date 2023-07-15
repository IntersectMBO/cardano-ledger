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

module MAlonzo.Code.Foreign.Haskell.Pair where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive

type AgdaPair l1 l2 a b = (a , b)
-- Foreign.Haskell.Pair.Pair
d_Pair_22 a0 a1 a2 a3 = ()
type T_Pair_22 a0 a1 a2 a3 =
  MAlonzo.Code.Foreign.Haskell.Pair.AgdaPair a0 a1 a2 a3
pattern C__'44'__36 a0 a1 = (,) a0 a1
check__'44'__36 ::
  forall xa.
    forall xb. forall xA. forall xB. xA -> xB -> T_Pair_22 xa xb xA xB
check__'44'__36 = (,)
cover_Pair_22 ::
  MAlonzo.Code.Foreign.Haskell.Pair.AgdaPair a1 a2 a3 a4 -> ()
cover_Pair_22 x
  = case x of
      (,) _ _ -> ()
-- Foreign.Haskell.Pair.Pair.fst
d_fst_32 :: T_Pair_22 AgdaAny AgdaAny AgdaAny AgdaAny -> AgdaAny
d_fst_32 v0
  = case coe v0 of
      C__'44'__36 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Foreign.Haskell.Pair.Pair.snd
d_snd_34 :: T_Pair_22 AgdaAny AgdaAny AgdaAny AgdaAny -> AgdaAny
d_snd_34 v0
  = case coe v0 of
      C__'44'__36 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Foreign.Haskell.Pair.toForeign
d_toForeign_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_Pair_22 AgdaAny AgdaAny AgdaAny AgdaAny
d_toForeign_38 ~v0 ~v1 ~v2 ~v3 v4 = du_toForeign_38 v4
du_toForeign_38 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_Pair_22 AgdaAny AgdaAny AgdaAny AgdaAny
du_toForeign_38 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> coe C__'44'__36 (coe v1) (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Foreign.Haskell.Pair.fromForeign
d_fromForeign_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Pair_22 AgdaAny AgdaAny AgdaAny AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_fromForeign_44 ~v0 ~v1 ~v2 ~v3 v4 = du_fromForeign_44 v4
du_fromForeign_44 ::
  T_Pair_22 AgdaAny AgdaAny AgdaAny AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_fromForeign_44 v0
  = case coe v0 of
      C__'44'__36 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
