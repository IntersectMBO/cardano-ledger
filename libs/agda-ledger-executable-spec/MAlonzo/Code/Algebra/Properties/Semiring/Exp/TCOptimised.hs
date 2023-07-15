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

module MAlonzo.Code.Algebra.Properties.Semiring.Exp.TCOptimised where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Definitions.RawMonoid
import qualified MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised

-- Algebra.Properties.Semiring.Exp.TCOptimised._._^′_
d__'94''8242'__208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986 ->
  AgdaAny -> Integer -> AgdaAny
d__'94''8242'__208 ~v0 ~v1 v2 = du__'94''8242'__208 v2
du__'94''8242'__208 ::
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986 ->
  AgdaAny -> Integer -> AgdaAny
du__'94''8242'__208 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
              (coe
                 MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                 (coe v0)) in
    coe
      (\ v2 v3 ->
         let v4
               = coe
                   MAlonzo.Code.Algebra.Bundles.Raw.du_'42''45'rawMonoid_190
                   (coe v1) in
         let v5 = subInt (coe v3) (coe (1 :: Integer)) in
         let v6
               = coe
                   MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                      (coe v4) (coe v5) (coe v2))
                   v2 in
         case coe v3 of
           0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
           1 -> coe v2
           _ -> coe v6)
-- Algebra.Properties.Semiring.Exp.TCOptimised.^-congˡ
d_'94''45'cong'737'_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'94''45'cong'737'_214 ~v0 ~v1 v2 = du_'94''45'cong'737'_214 v2
du_'94''45'cong'737'_214 ::
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'94''45'cong'737'_214 v0
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''45'cong'691'_160
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
         (coe
            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
            (coe v0)))
-- Algebra.Properties.Semiring.Exp.TCOptimised.^-cong
d_'94''45'cong_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986 ->
  AgdaAny ->
  AgdaAny ->
  Integer ->
  Integer ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_'94''45'cong_216 ~v0 ~v1 v2 v3 v4 v5 ~v6 v7 ~v8
  = du_'94''45'cong_216 v2 v3 v4 v5 v7
du_'94''45'cong_216 ::
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986 ->
  AgdaAny -> AgdaAny -> Integer -> AgdaAny -> AgdaAny
du_'94''45'cong_216 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''45'cong_170
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
         (coe
            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
            (coe v0)))
      (coe v3) (coe v1) (coe v2) (coe v4)
-- Algebra.Properties.Semiring.Exp.TCOptimised.^-homo-*
d_'94''45'homo'45''42'_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
d_'94''45'homo'45''42'_228 ~v0 ~v1 v2
  = du_'94''45'homo'45''42'_228 v2
du_'94''45'homo'45''42'_228 ::
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
du_'94''45'homo'45''42'_228 v0
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''45'homo'45''43'_148
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
         (coe
            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
            (coe v0)))
-- Algebra.Properties.Semiring.Exp.TCOptimised.^-assocʳ
d_'94''45'assoc'691'_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
d_'94''45'assoc'691'_236 ~v0 ~v1 v2 v3 v4 v5
  = du_'94''45'assoc'691'_236 v2 v3 v4 v5
du_'94''45'assoc'691'_236 ::
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
du_'94''45'assoc'691'_236 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''45'assoc'737'_182
      (coe
         MAlonzo.Code.Algebra.Bundles.du_'42''45'monoid_1970
         (coe
            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
            (coe v0)))
      (coe v1) (coe v3) (coe v2)
