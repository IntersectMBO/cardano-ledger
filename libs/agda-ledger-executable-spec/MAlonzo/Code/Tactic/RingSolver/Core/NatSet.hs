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

module MAlonzo.Code.Tactic.RingSolver.Core.NatSet where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Nat.Base

-- Tactic.RingSolver.Core.NatSet.para
d_para_16 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> [AgdaAny] -> AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> AgdaAny
d_para_16 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_para_16 v4 v5 v6
du_para_16 ::
  (AgdaAny -> [AgdaAny] -> AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> AgdaAny
du_para_16 v0 v1 v2
  = case coe v2 of
      [] -> coe v1
      (:) v3 v4
        -> coe v0 v3 v4 (coe du_para_16 (coe v0) (coe v1) (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.NatSet.NatSet
d_NatSet_30 :: ()
d_NatSet_30 = erased
-- Tactic.RingSolver.Core.NatSet.insert
d_insert_32 :: Integer -> [Integer] -> [Integer]
d_insert_32 v0 v1
  = coe
      du_para_16 (coe du_f_42)
      (\ v2 ->
         coe
           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
      v1 v0
-- Tactic.RingSolver.Core.NatSet._.f
d_f_42 ::
  Integer ->
  [Integer] ->
  Integer ->
  [Integer] -> (Integer -> [Integer]) -> Integer -> [Integer]
d_f_42 ~v0 ~v1 v2 v3 v4 v5 = du_f_42 v2 v3 v4 v5
du_f_42 ::
  Integer ->
  [Integer] -> (Integer -> [Integer]) -> Integer -> [Integer]
du_f_42 v0 v1 v2 v3
  = let v4
          = MAlonzo.Code.Data.Nat.Base.d_compare_400 (coe v3) (coe v0) in
    case coe v4 of
      MAlonzo.Code.Data.Nat.Base.C_less_384 v6
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v6) (coe v1))
      MAlonzo.Code.Data.Nat.Base.C_equal_388
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v1)
      MAlonzo.Code.Data.Nat.Base.C_greater_394 v6
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v2 v6)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.NatSet.delete
d_delete_92 :: Integer -> [Integer] -> [Integer]
d_delete_92 v0 v1
  = coe
      du_para_16 (coe du_f_102)
      (let v2 = coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16 in
       \ v3 -> v2)
      v1 v0
-- Tactic.RingSolver.Core.NatSet._.f
d_f_102 ::
  Integer ->
  [Integer] ->
  Integer ->
  [Integer] -> (Integer -> [Integer]) -> Integer -> [Integer]
d_f_102 ~v0 ~v1 v2 v3 v4 v5 = du_f_102 v2 v3 v4 v5
du_f_102 ::
  Integer ->
  [Integer] -> (Integer -> [Integer]) -> Integer -> [Integer]
du_f_102 v0 v1 v2 v3
  = let v4
          = MAlonzo.Code.Data.Nat.Base.d_compare_400 (coe v3) (coe v0) in
    case coe v4 of
      MAlonzo.Code.Data.Nat.Base.C_less_384 v6
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
             (coe addInt (coe addInt (coe (1 :: Integer)) (coe v3)) (coe v6))
             (coe v1)
      MAlonzo.Code.Data.Nat.Base.C_equal_388
        -> case coe v1 of
             [] -> coe v1
             (:) v6 v7
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe addInt (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v6))
                    (coe v7)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Nat.Base.C_greater_394 v6
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v2 v6)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.NatSet.lookup
d_lookup_152 :: [Integer] -> Integer -> Maybe Integer
d_lookup_152 v0 v1
  = coe
      MAlonzo.Code.Data.List.Base.du_foldr_242 (coe du_f_162)
      (let v2
             = let v2 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
               \ v3 -> v2 in
       \ v3 -> v2)
      v0 v1 (0 :: Integer)
-- Tactic.RingSolver.Core.NatSet._.f
d_f_162 ::
  [Integer] ->
  Integer ->
  Integer ->
  (Integer -> Integer -> Maybe Integer) ->
  Integer -> Integer -> Maybe Integer
d_f_162 ~v0 ~v1 v2 v3 v4 v5 = du_f_162 v2 v3 v4 v5
du_f_162 ::
  Integer ->
  (Integer -> Integer -> Maybe Integer) ->
  Integer -> Integer -> Maybe Integer
du_f_162 v0 v1 v2 v3
  = let v4
          = MAlonzo.Code.Data.Nat.Base.d_compare_400 (coe v2) (coe v0) in
    case coe v4 of
      MAlonzo.Code.Data.Nat.Base.C_less_384 v6
        -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      MAlonzo.Code.Data.Nat.Base.C_equal_388
        -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v3)
      MAlonzo.Code.Data.Nat.Base.C_greater_394 v6
        -> coe v1 v6 (addInt (coe (1 :: Integer)) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.Core.NatSet.member
d_member_210 :: Integer -> [Integer] -> Bool
d_member_210 v0 v1
  = coe
      MAlonzo.Code.Data.Maybe.Base.du_is'45'just_20
      (coe d_lookup_152 (coe v1) (coe v0))
-- Tactic.RingSolver.Core.NatSet.fromList
d_fromList_216 :: [Integer] -> [Integer]
d_fromList_216
  = coe
      MAlonzo.Code.Data.List.Base.du_foldr_242 (coe d_insert_32)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Tactic.RingSolver.Core.NatSet.toList
d_toList_218 :: [Integer] -> [Integer]
d_toList_218 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_drop_588 (coe (1 :: Integer))
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe MAlonzo.Code.Data.Nat.Base.d_pred_126)
         (coe
            MAlonzo.Code.Data.List.Base.du_scanl_374
            (coe
               (\ v1 v2 ->
                  addInt (coe addInt (coe (1 :: Integer)) (coe v1)) (coe v2)))
            (coe (0 :: Integer)) (coe v0)))
