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

module MAlonzo.Code.Data.Vec.Bounded.Base where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Extrema
import qualified MAlonzo.Code.Data.List.Membership.Setoid
import qualified MAlonzo.Code.Data.List.Relation.Unary.All
import qualified MAlonzo.Code.Data.List.Relation.Unary.All.Properties
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.These.Base
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.Vec.Bounded.Base._.max
d_max_50 :: Integer -> [Integer] -> Integer
d_max_50
  = coe
      MAlonzo.Code.Data.List.Extrema.du_max_128
      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'totalOrder_2636)
-- Data.Vec.Bounded.Base._.xs≤max
d_xs'8804'max_98 ::
  Integer ->
  [Integer] -> MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_xs'8804'max_98
  = coe
      MAlonzo.Code.Data.List.Extrema.du_xs'8804'max_706
      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'totalOrder_2636)
-- Data.Vec.Bounded.Base.Vec≤
d_Vec'8804'_122 a0 a1 a2 = ()
data T_Vec'8804'_122
  = C__'44'__140 Integer MAlonzo.Code.Data.Vec.Base.T_Vec_28
-- Data.Vec.Bounded.Base.Vec≤.length
d_length_134 :: T_Vec'8804'_122 -> Integer
d_length_134 v0
  = case coe v0 of
      C__'44'__140 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.Vec≤.vec
d_vec_136 :: T_Vec'8804'_122 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_vec_136 v0
  = case coe v0 of
      C__'44'__140 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.fromVec
d_fromVec_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> T_Vec'8804'_122
d_fromVec_144 ~v0 ~v1 v2 v3 = du_fromVec_144 v2 v3
du_fromVec_144 ::
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> T_Vec'8804'_122
du_fromVec_144 v0 v1 = coe C__'44'__140 v0 v1
-- Data.Vec.Bounded.Base.padRight
d_padRight_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  AgdaAny -> T_Vec'8804'_122 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_padRight_150 ~v0 ~v1 v2 v3 v4 = du_padRight_150 v2 v3 v4
du_padRight_150 ::
  Integer ->
  AgdaAny -> T_Vec'8804'_122 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28
du_padRight_150 v0 v1 v2
  = case coe v2 of
      C__'44'__140 v3 v4
        -> let v6
                 = coe
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.du_recompute_48
                     (coe
                        MAlonzo.Code.Data.Nat.Properties.d__'8804''8243''63'__6094 (coe v3)
                        (coe v0)) in
           case coe v6 of
             MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328 v7
               -> coe
                    MAlonzo.Code.Data.Vec.Base.du__'43''43'__188 (coe v4)
                    (coe MAlonzo.Code.Data.Vec.Base.du_replicate_446 (coe v7) (coe v1))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.padLeft
d_padLeft_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  AgdaAny -> T_Vec'8804'_122 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_padLeft_170 ~v0 ~v1 v2 v3 v4 = du_padLeft_170 v2 v3 v4
du_padLeft_170 ::
  Integer ->
  AgdaAny -> T_Vec'8804'_122 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28
du_padLeft_170 v0 v1 v2
  = case coe v2 of
      C__'44'__140 v3 v4
        -> let v6
                 = coe
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.du_recompute_48
                     (coe
                        MAlonzo.Code.Data.Nat.Properties.d__'8804''8243''63'__6094 (coe v3)
                        (coe v0)) in
           case coe v6 of
             MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328 v7
               -> coe
                    MAlonzo.Code.Data.Vec.Base.du__'43''43'__188
                    (coe MAlonzo.Code.Data.Vec.Base.du_replicate_446 (coe v7) (coe v1))
                    (coe v4)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.split
d_split_218 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_split_218 = erased
-- Data.Vec.Bounded.Base.padBoth
d_padBoth_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  AgdaAny ->
  AgdaAny -> T_Vec'8804'_122 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_padBoth_238 ~v0 ~v1 v2 v3 v4 v5 = du_padBoth_238 v2 v3 v4 v5
du_padBoth_238 ::
  Integer ->
  AgdaAny ->
  AgdaAny -> T_Vec'8804'_122 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28
du_padBoth_238 v0 v1 v2 v3
  = case coe v3 of
      C__'44'__140 v4 v5
        -> let v7
                 = coe
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.du_recompute_48
                     (coe
                        MAlonzo.Code.Data.Nat.Properties.d__'8804''8243''63'__6094 (coe v4)
                        (coe v0)) in
           case coe v7 of
             MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328 v8
               -> coe
                    MAlonzo.Code.Data.Vec.Base.du__'43''43'__188
                    (coe
                       MAlonzo.Code.Data.Vec.Base.du_replicate_446
                       (coe MAlonzo.Code.Data.Nat.Base.d_'8970'_'47'2'8971'_198 (coe v8))
                       (coe v1))
                    (coe
                       MAlonzo.Code.Data.Vec.Base.du__'43''43'__188 (coe v5)
                       (coe
                          MAlonzo.Code.Data.Vec.Base.du_replicate_446
                          (coe MAlonzo.Code.Data.Nat.Base.d_'8968'_'47'2'8969'_202 (coe v8))
                          (coe v2)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.fromList
d_fromList_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> T_Vec'8804'_122
d_fromList_288 ~v0 ~v1 v2 = du_fromList_288 v2
du_fromList_288 :: [AgdaAny] -> T_Vec'8804'_122
du_fromList_288 v0
  = coe
      du_fromVec_144 (coe MAlonzo.Code.Data.List.Base.du_length_304 v0)
      (coe MAlonzo.Code.Data.Vec.Base.du_fromList_638 (coe v0))
-- Data.Vec.Bounded.Base.toList
d_toList_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> T_Vec'8804'_122 -> [AgdaAny]
d_toList_292 ~v0 ~v1 ~v2 v3 = du_toList_292 v3
du_toList_292 :: T_Vec'8804'_122 -> [AgdaAny]
du_toList_292 v0
  = coe
      MAlonzo.Code.Data.Vec.Base.du_toList_630 (coe d_vec_136 (coe v0))
-- Data.Vec.Bounded.Base.replicate
d_replicate_300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  AgdaAny -> T_Vec'8804'_122
d_replicate_300 ~v0 ~v1 v2 ~v3 ~v4 v5 = du_replicate_300 v2 v5
du_replicate_300 :: Integer -> AgdaAny -> T_Vec'8804'_122
du_replicate_300 v0 v1
  = coe
      C__'44'__140 v0
      (coe MAlonzo.Code.Data.Vec.Base.du_replicate_446 (coe v0) (coe v1))
-- Data.Vec.Bounded.Base.[]
d_'91''93'_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> T_Vec'8804'_122
d_'91''93'_308 ~v0 ~v1 ~v2 = du_'91''93'_308
du_'91''93'_308 :: T_Vec'8804'_122
du_'91''93'_308
  = coe
      C__'44'__140 (0 :: Integer)
      (coe MAlonzo.Code.Data.Vec.Base.C_'91''93'_32)
-- Data.Vec.Bounded.Base._∷_
d__'8759'__312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> AgdaAny -> T_Vec'8804'_122 -> T_Vec'8804'_122
d__'8759'__312 ~v0 ~v1 ~v2 v3 v4 = du__'8759'__312 v3 v4
du__'8759'__312 :: AgdaAny -> T_Vec'8804'_122 -> T_Vec'8804'_122
du__'8759'__312 v0 v1
  = case coe v1 of
      C__'44'__140 v2 v3
        -> coe
             C__'44'__140 (addInt (coe (1 :: Integer)) (coe v2))
             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v0 v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.≤-cast
d_'8804''45'cast_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  T_Vec'8804'_122 -> T_Vec'8804'_122
d_'8804''45'cast_326 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8804''45'cast_326 v5
du_'8804''45'cast_326 :: T_Vec'8804'_122 -> T_Vec'8804'_122
du_'8804''45'cast_326 v0 = coe v0
-- Data.Vec.Bounded.Base.≡-cast
d_'8801''45'cast_340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T_Vec'8804'_122 -> T_Vec'8804'_122
d_'8801''45'cast_340 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8801''45'cast_340 v5
du_'8801''45'cast_340 :: T_Vec'8804'_122 -> T_Vec'8804'_122
du_'8801''45'cast_340 v0 = coe v0
-- Data.Vec.Bounded.Base.map
d_map_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  Integer -> T_Vec'8804'_122 -> T_Vec'8804'_122
d_map_346 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 = du_map_346 v4 v6
du_map_346 ::
  (AgdaAny -> AgdaAny) -> T_Vec'8804'_122 -> T_Vec'8804'_122
du_map_346 v0 v1
  = case coe v1 of
      C__'44'__140 v2 v3
        -> coe
             C__'44'__140 v2
             (coe MAlonzo.Code.Data.Vec.Base.du_map_178 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.reverse
d_reverse_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> T_Vec'8804'_122 -> T_Vec'8804'_122
d_reverse_356 ~v0 ~v1 ~v2 v3 = du_reverse_356 v3
du_reverse_356 :: T_Vec'8804'_122 -> T_Vec'8804'_122
du_reverse_356 v0
  = case coe v0 of
      C__'44'__140 v1 v2
        -> coe
             C__'44'__140 v1 (coe MAlonzo.Code.Data.Vec.Base.du_reverse_654 v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.alignWith
d_alignWith_364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  Integer -> T_Vec'8804'_122 -> T_Vec'8804'_122 -> T_Vec'8804'_122
d_alignWith_364 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 v8 v9
  = du_alignWith_364 v6 v8 v9
du_alignWith_364 ::
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  T_Vec'8804'_122 -> T_Vec'8804'_122 -> T_Vec'8804'_122
du_alignWith_364 v0 v1 v2
  = case coe v1 of
      C__'44'__140 v3 v4
        -> case coe v2 of
             C__'44'__140 v6 v7
               -> coe
                    C__'44'__140
                    (MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v3) (coe v6))
                    (coe
                       MAlonzo.Code.Data.Vec.Base.du_alignWith_204 (coe v0) (coe v4)
                       (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.zipWith
d_zipWith_378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  Integer -> T_Vec'8804'_122 -> T_Vec'8804'_122 -> T_Vec'8804'_122
d_zipWith_378 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 v8 v9
  = du_zipWith_378 v6 v8 v9
du_zipWith_378 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_Vec'8804'_122 -> T_Vec'8804'_122 -> T_Vec'8804'_122
du_zipWith_378 v0 v1 v2
  = case coe v1 of
      C__'44'__140 v3 v4
        -> case coe v2 of
             C__'44'__140 v6 v7
               -> coe
                    C__'44'__140
                    (MAlonzo.Code.Data.Nat.Base.d__'8851'__166 (coe v3) (coe v6))
                    (coe
                       MAlonzo.Code.Data.Vec.Base.du_restrictWith_224 (coe v0) (coe v4)
                       (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.zip
d_zip_392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer -> T_Vec'8804'_122 -> T_Vec'8804'_122 -> T_Vec'8804'_122
d_zip_392 ~v0 ~v1 ~v2 ~v3 ~v4 = du_zip_392
du_zip_392 :: T_Vec'8804'_122 -> T_Vec'8804'_122 -> T_Vec'8804'_122
du_zip_392
  = coe
      du_zipWith_378 (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32)
-- Data.Vec.Bounded.Base.align
d_align_396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer -> T_Vec'8804'_122 -> T_Vec'8804'_122 -> T_Vec'8804'_122
d_align_396 ~v0 ~v1 ~v2 ~v3 ~v4 = du_align_396
du_align_396 ::
  T_Vec'8804'_122 -> T_Vec'8804'_122 -> T_Vec'8804'_122
du_align_396 = coe du_alignWith_364 (coe (\ v0 -> v0))
-- Data.Vec.Bounded.Base.take
d_take_402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> Integer -> T_Vec'8804'_122 -> T_Vec'8804'_122
d_take_402 ~v0 ~v1 ~v2 v3 v4 = du_take_402 v3 v4
du_take_402 :: Integer -> T_Vec'8804'_122 -> T_Vec'8804'_122
du_take_402 v0 v1
  = case coe v0 of
      0 -> coe du_'91''93'_308
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             C__'44'__140 v3 v4
               -> case coe v4 of
                    MAlonzo.Code.Data.Vec.Base.C_'91''93'_32 -> coe du_'91''93'_308
                    MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v7 v8
                      -> let v9 = subInt (coe v3) (coe (1 :: Integer)) in
                         coe
                           du__'8759'__312 (coe v7)
                           (coe du_take_402 (coe v2) (coe C__'44'__140 v9 v8))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.drop
d_drop_422 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> Integer -> T_Vec'8804'_122 -> T_Vec'8804'_122
d_drop_422 ~v0 ~v1 ~v2 v3 v4 = du_drop_422 v3 v4
du_drop_422 :: Integer -> T_Vec'8804'_122 -> T_Vec'8804'_122
du_drop_422 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             C__'44'__140 v3 v4
               -> case coe v4 of
                    MAlonzo.Code.Data.Vec.Base.C_'91''93'_32 -> coe du_'91''93'_308
                    MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v7 v8
                      -> let v9 = subInt (coe v3) (coe (1 :: Integer)) in
                         coe du_drop_422 (coe v2) (coe C__'44'__140 v9 v8)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Bounded.Base.rectangle
d_rectangle_440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rectangle_440 ~v0 ~v1 v2 = du_rectangle_440 v2
du_rectangle_440 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_rectangle_440 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_width_452 (coe v0)) (coe du_padded_458 (coe v0))
-- Data.Vec.Bounded.Base._.sizes
d_sizes_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> [Integer]
d_sizes_450 ~v0 ~v1 v2 = du_sizes_450 v2
du_sizes_450 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> [Integer]
du_sizes_450 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_map_22
      (coe (\ v1 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v1)))
      (coe v0)
-- Data.Vec.Bounded.Base._.width
d_width_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> Integer
d_width_452 ~v0 ~v1 v2 = du_width_452 v2
du_width_452 :: [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> Integer
du_width_452 v0
  = coe
      MAlonzo.Code.Data.List.Extrema.du_max_128
      MAlonzo.Code.Data.Nat.Properties.d_'8804''45'totalOrder_2636
      (0 :: Integer) (coe du_sizes_450 (coe v0))
-- Data.Vec.Bounded.Base._.all≤
d_all'8804'_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_all'8804'_456 ~v0 ~v1 v2 = du_all'8804'_456 v2
du_all'8804'_456 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
du_all'8804'_456 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.All.Properties.du_map'8315'_680
      (coe v0)
      (coe
         MAlonzo.Code.Data.List.Extrema.du_xs'8804'max_706
         MAlonzo.Code.Data.Nat.Properties.d_'8804''45'totalOrder_2636
         (0 :: Integer) (coe du_sizes_450 (coe v0)))
-- Data.Vec.Bounded.Base._.padded
d_padded_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> [T_Vec'8804'_122]
d_padded_458 ~v0 ~v1 v2 = du_padded_458 v2
du_padded_458 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> [T_Vec'8804'_122]
du_padded_458 v0
  = coe
      MAlonzo.Code.Data.List.Membership.Setoid.du_mapWith'8712'_58
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      (coe v0)
      (coe
         (\ v1 v2 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v1)))
