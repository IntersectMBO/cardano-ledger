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

module MAlonzo.Code.Algebra.Lattice.Properties.Semilattice where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Lattice.Bundles
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Construct.NaturalOrder.Left
import qualified MAlonzo.Code.Relation.Binary.Lattice.Bundles
import qualified MAlonzo.Code.Relation.Binary.Lattice.Structures
import qualified MAlonzo.Code.Relation.Binary.Properties.Poset

-- Algebra.Lattice.Properties.Semilattice.poset
d_poset_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_146 ~v0 ~v1 v2 = du_poset_146 v2
du_poset_146 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_146 v0
  = coe
      MAlonzo.Code.Relation.Binary.Construct.NaturalOrder.Left.du_poset_3500
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8729'__28 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Bundles.d_isSemilattice_30 (coe v0))
-- Algebra.Lattice.Properties.Semilattice._._≤_
d__'8804'__150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8804'__150 = erased
-- Algebra.Lattice.Properties.Semilattice._._≥_
d__'8805'__156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  AgdaAny -> AgdaAny -> ()
d__'8805'__156 = erased
-- Algebra.Lattice.Properties.Semilattice.∧-isOrderTheoreticMeetSemilattice
d_'8743''45'isOrderTheoreticMeetSemilattice_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_'8743''45'isOrderTheoreticMeetSemilattice_160 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticMeetSemilattice_160 v2
du_'8743''45'isOrderTheoreticMeetSemilattice_160 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_'8743''45'isOrderTheoreticMeetSemilattice_160 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.C_IsMeetSemilattice'46'constructor_7327
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPartialOrder_304
         (coe du_poset_146 (coe v0)))
      (coe
         MAlonzo.Code.Relation.Binary.Construct.NaturalOrder.Left.du_infimum_3356
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8729'__28 (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isSemilattice_30 (coe v0)))
-- Algebra.Lattice.Properties.Semilattice.∧-isOrderTheoreticJoinSemilattice
d_'8743''45'isOrderTheoreticJoinSemilattice_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_'8743''45'isOrderTheoreticJoinSemilattice_162 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticJoinSemilattice_162 v2
du_'8743''45'isOrderTheoreticJoinSemilattice_162 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_'8743''45'isOrderTheoreticJoinSemilattice_162 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.C_IsJoinSemilattice'46'constructor_527
      (coe
         MAlonzo.Code.Relation.Binary.Properties.Poset.du_'8805''45'isPartialOrder_134
         (coe du_poset_146 (coe v0)))
      (coe
         MAlonzo.Code.Relation.Binary.Construct.NaturalOrder.Left.du_infimum_3356
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.d__'8729'__28 (coe v0))
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isSemilattice_30 (coe v0)))
-- Algebra.Lattice.Properties.Semilattice.∧-orderTheoreticMeetSemilattice
d_'8743''45'orderTheoreticMeetSemilattice_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
d_'8743''45'orderTheoreticMeetSemilattice_164 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticMeetSemilattice_164 v2
du_'8743''45'orderTheoreticMeetSemilattice_164 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
du_'8743''45'orderTheoreticMeetSemilattice_164 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Bundles.C_MeetSemilattice'46'constructor_4329
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8729'__28 (coe v0))
      (coe du_'8743''45'isOrderTheoreticMeetSemilattice_160 (coe v0))
-- Algebra.Lattice.Properties.Semilattice.∧-orderTheoreticJoinSemilattice
d_'8743''45'orderTheoreticJoinSemilattice_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
d_'8743''45'orderTheoreticJoinSemilattice_166 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticJoinSemilattice_166 v2
du_'8743''45'orderTheoreticJoinSemilattice_166 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
du_'8743''45'orderTheoreticJoinSemilattice_166 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Bundles.C_JoinSemilattice'46'constructor_363
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8729'__28 (coe v0))
      (coe du_'8743''45'isOrderTheoreticJoinSemilattice_162 (coe v0))
