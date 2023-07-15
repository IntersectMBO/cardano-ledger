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

module MAlonzo.Code.Data.Nat.GCD.Lemmas where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties

-- Data.Nat.GCD.Lemmas.distrib-comm
d_distrib'45'comm_14 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'45'comm_14 = erased
-- Data.Nat.GCD.Lemmas.distrib-comm₂
d_distrib'45'comm'8322'_30 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_distrib'45'comm'8322'_30 = erased
-- Data.Nat.GCD.Lemmas.lem₀
d_lem'8320'_48 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8320'_48 = erased
-- Data.Nat.GCD.Lemmas.lem₁
d_lem'8321'_68 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804''8242'__272
d_lem'8321'_68 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.du_'8804''8658''8804''8242'_5930
      (coe addInt (coe addInt (coe (2 :: Integer)) (coe v0)) (coe v1))
      (coe
         MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
         (coe
            MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
            (coe
               MAlonzo.Code.Data.Nat.Properties.du_m'8804'n'43'm_3374 (coe v0))))
-- Data.Nat.GCD.Lemmas.lem₂
d_lem'8322'_82 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8322'_82 = erased
-- Data.Nat.GCD.Lemmas.lem₃
d_lem'8323'_110 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8323'_110 = erased
-- Data.Nat.GCD.Lemmas._.y
d_y_128 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_y_128 ~v0 v1 v2 ~v3 ~v4 ~v5 = du_y_128 v1 v2
du_y_128 :: Integer -> Integer -> Integer
du_y_128 v0 v1
  = coe addInt (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1)
-- Data.Nat.GCD.Lemmas.lem₄
d_lem'8324'_146 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8324'_146 = erased
-- Data.Nat.GCD.Lemmas.lem₅
d_lem'8325'_174 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8325'_174 = erased
-- Data.Nat.GCD.Lemmas.lem₆
d_lem'8326'_202 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8326'_202 = erased
-- Data.Nat.GCD.Lemmas._.y
d_y_220 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_y_220 ~v0 v1 ~v2 v3 ~v4 ~v5 = du_y_220 v1 v3
du_y_220 :: Integer -> Integer -> Integer
du_y_220 v0 v1
  = coe addInt (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1)
-- Data.Nat.GCD.Lemmas.lem₇
d_lem'8327'_240 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8327'_240 = erased
-- Data.Nat.GCD.Lemmas.lem₈
d_lem'8328'_274 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8328'_274 = erased
-- Data.Nat.GCD.Lemmas._.lemma
d_lemma_296 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lemma_296 = erased
-- Data.Nat.GCD.Lemmas.lem₉
d_lem'8329'_334 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8329'_334 = erased
-- Data.Nat.GCD.Lemmas._.lem
d_lem_356 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem_356 = erased
-- Data.Nat.GCD.Lemmas._.lemma
d_lemma_364 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lemma_364 = erased
-- Data.Nat.GCD.Lemmas.lem₁₀
d_lem'8321''8320'_390 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8321''8320'_390 = erased
-- Data.Nat.GCD.Lemmas._.a
d_a_410 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> Integer
d_a_410 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 = du_a_410 v0
du_a_410 :: Integer -> Integer
du_a_410 v0 = coe addInt (coe (1 :: Integer)) (coe v0)
-- Data.Nat.GCD.Lemmas.lem₁₁
d_lem'8321''8321'_444 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8321''8321'_444 = erased
