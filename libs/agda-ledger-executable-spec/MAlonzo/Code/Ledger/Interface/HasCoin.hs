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

module MAlonzo.Code.Ledger.Interface.HasCoin where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive

-- Ledger.Interface.HasCoin.HasCoin
d_HasCoin_10 a0 a1 = ()
newtype T_HasCoin_10
  = C_HasCoin'46'constructor_9 (AgdaAny -> Integer)
-- Ledger.Interface.HasCoin.HasCoin.getCoin
d_getCoin_18 :: T_HasCoin_10 -> AgdaAny -> Integer
d_getCoin_18 v0
  = case coe v0 of
      C_HasCoin'46'constructor_9 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Interface.HasCoin._.getCoin
d_getCoin_22 :: T_HasCoin_10 -> AgdaAny -> Integer
d_getCoin_22 v0 = coe d_getCoin_18 (coe v0)
