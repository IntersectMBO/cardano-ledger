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

module MAlonzo.Code.Data.Nat.Tactic.RingSolver where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Tactic.RingSolver
import qualified MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing

-- Data.Nat.Tactic.RingSolver.ring
d_ring_8 ::
  MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.T_AlmostCommutativeRing_178
d_ring_8
  = coe
      MAlonzo.Code.Tactic.RingSolver.Core.AlmostCommutativeRing.du_fromCommutativeSemiring_1794
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'43''45''42''45'commutativeSemiring_3650)
      (coe
         (\ v0 ->
            let v1 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
            case coe v0 of
              0 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 erased
              _ -> coe v1))
-- Data.Nat.Tactic.RingSolver.solve-∀
d_solve'45''8704'_12 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_solve'45''8704'_12
  = coe
      MAlonzo.Code.Tactic.RingSolver.d_solve'45''8704''45'macro_322
      (coe
         (MAlonzo.RTE.QName
            (8 :: Integer) (16219765639759126300 :: Integer)
            "Data.Nat.Tactic.RingSolver.ring"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
-- Data.Nat.Tactic.RingSolver.solve
d_solve_14 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_solve_14 v0
  = coe
      MAlonzo.Code.Tactic.RingSolver.d_solve'45'macro_424 (coe v0)
      (coe
         (MAlonzo.RTE.QName
            (8 :: Integer) (16219765639759126300 :: Integer)
            "Data.Nat.Tactic.RingSolver.ring"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
