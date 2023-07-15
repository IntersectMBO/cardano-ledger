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

module MAlonzo.Code.Generics.Deriving where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive

-- Generics.Deriving.Derivation
d_Derivation_4 :: ()
d_Derivation_4 = erased
-- Generics.Deriving.Derivable
d_Derivable_8 a0 = ()
newtype T_Derivable_8
  = C_Derivable'46'constructor_11 ([MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
                                   AgdaAny)
-- Generics.Deriving.Derivable.DERIVE'
d_DERIVE''_14 ::
  T_Derivable_8 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_DERIVE''_14 v0
  = case coe v0 of
      C_Derivable'46'constructor_11 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Deriving._.DERIVE'
d_DERIVE''_18 ::
  T_Derivable_8 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_DERIVE''_18 v0 = coe d_DERIVE''_14 (coe v0)
-- Generics.Deriving.DERIVE
d_DERIVE_22 ::
  T_Derivable_8 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_DERIVE_22 v0 = coe d_DERIVE''_14 (coe v0)
-- Generics.Deriving.Derivable¹
d_Derivable'185'_28 a0 = ()
newtype T_Derivable'185'_28
  = C_Derivable'185''46'constructor_199 ([MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
                                         AgdaAny)
-- Generics.Deriving.Derivable¹.DERIVE¹'
d_DERIVE'185'''_34 ::
  T_Derivable'185'_28 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_DERIVE'185'''_34 v0
  = case coe v0 of
      C_Derivable'185''46'constructor_199 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Deriving._.DERIVE¹'
d_DERIVE'185'''_38 ::
  T_Derivable'185'_28 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_DERIVE'185'''_38 v0 = coe d_DERIVE'185'''_34 (coe v0)
-- Generics.Deriving.DERIVE¹
d_DERIVE'185'_42 ::
  T_Derivable'185'_28 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_DERIVE'185'_42 v0 = coe d_DERIVE'185'''_34 (coe v0)
-- Generics.Deriving.Derivableω
d_Derivableω_48 a0 = ()
newtype T_Derivableω_48
  = C_Derivableω'46'constructor_323 ([MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
                                     AgdaAny)
-- Generics.Deriving.Derivableω.DERIVEω'
d_DERIVEω''_54 ::
  T_Derivableω_48 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_DERIVEω''_54 v0
  = case coe v0 of
      C_Derivableω'46'constructor_323 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Deriving._.DERIVEω'
d_DERIVEω''_58 ::
  T_Derivableω_48 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_DERIVEω''_58 v0 = coe d_DERIVEω''_54 (coe v0)
-- Generics.Deriving.DERIVEω
d_DERIVEω_62 ::
  T_Derivableω_48 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_DERIVEω_62 v0 = coe d_DERIVEω''_54 (coe v0)
