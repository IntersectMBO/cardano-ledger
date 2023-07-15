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

module MAlonzo.Code.Data.Nat.Divisibility.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality

-- Data.Nat.Divisibility.Core._∣_
d__'8739'__12 a0 a1 = ()
newtype T__'8739'__12 = C_divides_26 Integer
-- Data.Nat.Divisibility.Core._∣_.quotient
d_quotient_22 :: T__'8739'__12 -> Integer
d_quotient_22 v0
  = case coe v0 of
      C_divides_26 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Divisibility.Core._∣_.equality
d_equality_24 ::
  T__'8739'__12 -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_equality_24 = erased
-- Data.Nat.Divisibility.Core._∤_
d__'8740'__28 :: Integer -> Integer -> ()
d__'8740'__28 = erased
-- Data.Nat.Divisibility.Core.*-pres-∣
d_'42''45'pres'45''8739'_42 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> T__'8739'__12 -> T__'8739'__12 -> T__'8739'__12
d_'42''45'pres'45''8739'_42 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_'42''45'pres'45''8739'_42 v4 v5
du_'42''45'pres'45''8739'_42 ::
  T__'8739'__12 -> T__'8739'__12 -> T__'8739'__12
du_'42''45'pres'45''8739'_42 v0 v1
  = case coe v0 of
      C_divides_26 v2
        -> case coe v1 of
             C_divides_26 v4 -> coe C_divides_26 (mulInt (coe v2) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
