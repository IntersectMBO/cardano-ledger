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

module MAlonzo.Code.Data.Integer.Ext where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Data.Integer.Base
import qualified MAlonzo.Code.Data.Sign.Base

-- Data.Integer.Ext.ℤtoSignedℕ
d_ℤtoSignedℕ_6 :: Integer -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_ℤtoSignedℕ_6 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe MAlonzo.Code.Data.Integer.Base.d_sign_24 (coe v0))
      (coe MAlonzo.Code.Data.Integer.Base.d_'8739'_'8739'_18 (coe v0))
-- Data.Integer.Ext.posPart
d_posPart_10 :: Integer -> Integer
d_posPart_10 v0
  = let v1 = d_ℤtoSignedℕ_6 (coe v0) in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> case coe v2 of
             MAlonzo.Code.Data.Sign.Base.C_'43'_10 -> coe v3
             _ -> coe (0 :: Integer)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Ext.negPart
d_negPart_24 :: Integer -> Integer
d_negPart_24 v0
  = let v1 = d_ℤtoSignedℕ_6 (coe v0) in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> case coe v2 of
             MAlonzo.Code.Data.Sign.Base.C_'45'_8 -> coe v3
             _ -> coe (0 :: Integer)
      _ -> MAlonzo.RTE.mazUnreachableError
