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

module MAlonzo.Code.Data.String where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Char
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Data.Char.Properties
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Extrema
import qualified MAlonzo.Code.Data.List.Membership.DecSetoid
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.String.Base
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.String._.max
d_max_46 :: Integer -> [Integer] -> Integer
d_max_46
  = coe
      MAlonzo.Code.Data.List.Extrema.du_max_128
      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'totalOrder_2636)
-- Data.String._._∈?_
d__'8712''63'__102 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  [MAlonzo.Code.Agda.Builtin.Char.T_Char_6] ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8712''63'__102
  = let v0 = MAlonzo.Code.Data.Char.Properties.d__'8799'__14 in
    coe
      MAlonzo.Code.Data.List.Membership.DecSetoid.du__'8712''63'__60
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_decSetoid_406
         (coe v0))
-- Data.String.toVec
d_toVec_122 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_toVec_122 v0
  = coe
      MAlonzo.Code.Data.Vec.Base.du_fromList_638
      (coe MAlonzo.Code.Agda.Builtin.String.d_primStringToList_12 v0)
-- Data.String.fromVec
d_fromVec_128 ::
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_fromVec_128 ~v0 v1 = du_fromVec_128 v1
du_fromVec_128 ::
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
du_fromVec_128 v0
  = coe
      MAlonzo.Code.Agda.Builtin.String.d_primStringFromList_14
      (coe MAlonzo.Code.Data.Vec.Base.du_toList_630 (coe v0))
-- Data.String.parensIfSpace
d_parensIfSpace_130 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_parensIfSpace_130 v0
  = let v1
          = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe
                 MAlonzo.Code.Data.List.Membership.DecSetoid.du__'8712''63'__60
                 (coe
                    MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_decSetoid_406
                    (coe MAlonzo.Code.Data.Char.Properties.d__'8799'__14))
                 (coe ' ')
                 (coe MAlonzo.Code.Agda.Builtin.String.d_primStringToList_12 v0)) in
    if coe v1
      then coe MAlonzo.Code.Data.String.Base.d_parens_38 (coe v0)
      else coe v0
-- Data.String.rectangle
d_rectangle_144 ::
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_rectangle_144 ~v0 v1 v2 = du_rectangle_144 v1 v2
du_rectangle_144 ::
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
du_rectangle_144 v0 v1
  = coe
      MAlonzo.Code.Data.Vec.Base.du_zipWith_242
      (coe (\ v2 -> coe v2 (coe du_width_156 (coe v1)))) (coe v0)
      (coe v1)
-- Data.String._.sizes
d_sizes_154 ::
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> [Integer]
d_sizes_154 ~v0 ~v1 v2 = du_sizes_154 v2
du_sizes_154 :: MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> [Integer]
du_sizes_154 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_map_22
      (coe MAlonzo.Code.Data.String.Base.d_length_22)
      (coe MAlonzo.Code.Data.Vec.Base.du_toList_630 (coe v0))
-- Data.String._.width
d_width_156 ::
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> Integer
d_width_156 ~v0 ~v1 v2 = du_width_156 v2
du_width_156 :: MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> Integer
du_width_156 v0
  = coe
      MAlonzo.Code.Data.List.Extrema.du_max_128
      MAlonzo.Code.Data.Nat.Properties.d_'8804''45'totalOrder_2636
      (0 :: Integer) (coe du_sizes_154 (coe v0))
-- Data.String.rectangleˡ
d_rectangle'737'_164 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_rectangle'737'_164 v0 v1
  = coe
      du_rectangle_144
      (coe
         MAlonzo.Code.Data.Vec.Base.du_replicate_446 (coe v0)
         (coe MAlonzo.Code.Data.String.Base.d_padLeft_56 (coe v1)))
-- Data.String.rectangleʳ
d_rectangle'691'_170 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_rectangle'691'_170 v0 v1
  = coe
      du_rectangle_144
      (coe
         MAlonzo.Code.Data.Vec.Base.du_replicate_446 (coe v0)
         (coe MAlonzo.Code.Data.String.Base.d_padRight_82 (coe v1)))
-- Data.String.rectangleᶜ
d_rectangle'7580'_176 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_rectangle'7580'_176 v0 v1 v2
  = coe
      du_rectangle_144
      (coe
         MAlonzo.Code.Data.Vec.Base.du_replicate_446 (coe v0)
         (coe
            MAlonzo.Code.Data.String.Base.d_padBoth_108 (coe v1) (coe v2)))
