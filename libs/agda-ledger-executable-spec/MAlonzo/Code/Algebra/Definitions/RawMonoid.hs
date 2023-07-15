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

module MAlonzo.Code.Algebra.Definitions.RawMonoid where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Vec.Functional

-- Algebra.Definitions.RawMonoid._._∣_
d__'8739'__28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  AgdaAny -> AgdaAny -> ()
d__'8739'__28 = erased
-- Algebra.Definitions.RawMonoid._._∣ʳ_
d__'8739''691'__30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  AgdaAny -> AgdaAny -> ()
d__'8739''691'__30 = erased
-- Algebra.Definitions.RawMonoid._._∣ˡ_
d__'8739''737'__32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  AgdaAny -> AgdaAny -> ()
d__'8739''737'__32 = erased
-- Algebra.Definitions.RawMonoid._._∣∣_
d__'8739''8739'__34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  AgdaAny -> AgdaAny -> ()
d__'8739''8739'__34 = erased
-- Algebra.Definitions.RawMonoid._._∤_
d__'8740'__36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  AgdaAny -> AgdaAny -> ()
d__'8740'__36 = erased
-- Algebra.Definitions.RawMonoid._._∤ʳ_
d__'8740''691'__38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  AgdaAny -> AgdaAny -> ()
d__'8740''691'__38 = erased
-- Algebra.Definitions.RawMonoid._._∤ˡ_
d__'8740''737'__40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  AgdaAny -> AgdaAny -> ()
d__'8740''737'__40 = erased
-- Algebra.Definitions.RawMonoid._._∤∤_
d__'8740''8740'__42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  AgdaAny -> AgdaAny -> ()
d__'8740''8740'__42 = erased
-- Algebra.Definitions.RawMonoid._×_
d__'215'__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  Integer -> AgdaAny -> AgdaAny
d__'215'__44 ~v0 ~v1 v2 v3 v4 = du__'215'__44 v2 v3 v4
du__'215'__44 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  Integer -> AgdaAny -> AgdaAny
du__'215'__44 v0 v1 v2
  = case coe v1 of
      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)
      _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v0 v2
             (coe du__'215'__44 (coe v0) (coe v3) (coe v2))
-- Algebra.Definitions.RawMonoid._×′_
d__'215''8242'__52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  Integer -> AgdaAny -> AgdaAny
d__'215''8242'__52 ~v0 ~v1 v2 v3 v4 = du__'215''8242'__52 v2 v3 v4
du__'215''8242'__52 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  Integer -> AgdaAny -> AgdaAny
du__'215''8242'__52 v0 v1 v2
  = let v3 = subInt (coe v1) (coe (1 :: Integer)) in
    let v4
          = coe
              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v0
              (coe du__'215''8242'__52 (coe v0) (coe v3) (coe v2)) v2 in
    case coe v1 of
      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)
      1 -> coe v2
      _ -> coe v4
-- Algebra.Definitions.RawMonoid.sum
d_sum_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
d_sum_64 ~v0 ~v1 v2 v3 = du_sum_64 v2 v3
du_sum_64 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38 ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
du_sum_64 v0 v1
  = coe
      MAlonzo.Code.Data.Vec.Functional.du_foldr_200
      (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 (coe v0))
      (coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v0)) (coe v1)
