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

module MAlonzo.Code.Algebra.Lattice.Properties.DistributiveLattice where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Lattice.Bundles
import qualified MAlonzo.Code.Algebra.Lattice.Properties.Lattice
import qualified MAlonzo.Code.Algebra.Lattice.Properties.Semilattice
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Lattice.Bundles
import qualified MAlonzo.Code.Relation.Binary.Lattice.Structures

-- Algebra.Lattice.Properties.DistributiveLattice._.IsDistributiveLattice
d_IsDistributiveLattice_226 a0 a1 a2 a3 a4 = ()
-- Algebra.Lattice.Properties.DistributiveLattice._.poset
d_poset_664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_poset_664 ~v0 ~v1 v2 = du_poset_664 v2
du_poset_664 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_poset_664 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_poset_146
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
         (coe v1))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-idem
d_'8743''45'idem_666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny
d_'8743''45'idem_666 ~v0 ~v1 v2 = du_'8743''45'idem_666 v2
du_'8743''45'idem_666 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny
du_'8743''45'idem_666 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'idem_2948
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-isBand
d_'8743''45'isBand_668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8743''45'isBand_668 ~v0 ~v1 v2 = du_'8743''45'isBand_668 v2
du_'8743''45'isBand_668 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_'8743''45'isBand_668 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isBand_2956
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-isMagma
d_'8743''45'isMagma_670 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8743''45'isMagma_670 ~v0 ~v1 v2 = du_'8743''45'isMagma_670 v2
du_'8743''45'isMagma_670 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8743''45'isMagma_670 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isMagma_2952
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-isOrderTheoreticJoinSemilattice
d_'8743''45'isOrderTheoreticJoinSemilattice_672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_'8743''45'isOrderTheoreticJoinSemilattice_672 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticJoinSemilattice_672 v2
du_'8743''45'isOrderTheoreticJoinSemilattice_672 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_'8743''45'isOrderTheoreticJoinSemilattice_672 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticJoinSemilattice_162
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
         (coe v1))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-isOrderTheoreticMeetSemilattice
d_'8743''45'isOrderTheoreticMeetSemilattice_674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_'8743''45'isOrderTheoreticMeetSemilattice_674 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticMeetSemilattice_674 v2
du_'8743''45'isOrderTheoreticMeetSemilattice_674 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_'8743''45'isOrderTheoreticMeetSemilattice_674 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticMeetSemilattice_160
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
         (coe v1))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-isSemigroup
d_'8743''45'isSemigroup_676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8743''45'isSemigroup_676 ~v0 ~v1 v2
  = du_'8743''45'isSemigroup_676 v2
du_'8743''45'isSemigroup_676 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8743''45'isSemigroup_676 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isSemigroup_2954
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-isSemilattice
d_'8743''45'isSemilattice_678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8743''45'isSemilattice_678 ~v0 ~v1 v2
  = du_'8743''45'isSemilattice_678 v2
du_'8743''45'isSemilattice_678 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_'8743''45'isSemilattice_678 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'isSemilattice_2958
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-orderTheoreticJoinSemilattice
d_'8743''45'orderTheoreticJoinSemilattice_680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
d_'8743''45'orderTheoreticJoinSemilattice_680 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticJoinSemilattice_680 v2
du_'8743''45'orderTheoreticJoinSemilattice_680 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
du_'8743''45'orderTheoreticJoinSemilattice_680 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticJoinSemilattice_166
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
         (coe v1))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-orderTheoreticMeetSemilattice
d_'8743''45'orderTheoreticMeetSemilattice_682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
d_'8743''45'orderTheoreticMeetSemilattice_682 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticMeetSemilattice_682 v2
du_'8743''45'orderTheoreticMeetSemilattice_682 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
du_'8743''45'orderTheoreticMeetSemilattice_682 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticMeetSemilattice_164
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
         (coe v1))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-semilattice
d_'8743''45'semilattice_684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8743''45'semilattice_684 ~v0 ~v1 v2
  = du_'8743''45'semilattice_684 v2
du_'8743''45'semilattice_684 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
du_'8743''45'semilattice_684 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45'semilattice_2960
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-∨-isLattice
d_'8743''45''8744''45'isLattice_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_'8743''45''8744''45'isLattice_686 ~v0 ~v1 v2
  = du_'8743''45''8744''45'isLattice_686 v2
du_'8743''45''8744''45'isLattice_686 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
du_'8743''45''8744''45'isLattice_686 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45''8744''45'isLattice_2996
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-∨-lattice
d_'8743''45''8744''45'lattice_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
d_'8743''45''8744''45'lattice_688 ~v0 ~v1 v2
  = du_'8743''45''8744''45'lattice_688 v2
du_'8743''45''8744''45'lattice_688 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Lattice_498
du_'8743''45''8744''45'lattice_688 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45''8744''45'lattice_2998
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∨-idem
d_'8744''45'idem_690 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny
d_'8744''45'idem_690 ~v0 ~v1 v2 = du_'8744''45'idem_690 v2
du_'8744''45'idem_690 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny
du_'8744''45'idem_690 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'idem_2972
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∨-isBand
d_'8744''45'isBand_692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_'8744''45'isBand_692 ~v0 ~v1 v2 = du_'8744''45'isBand_692 v2
du_'8744''45'isBand_692 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_'8744''45'isBand_692 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isBand_2980
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∨-isMagma
d_'8744''45'isMagma_694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8744''45'isMagma_694 ~v0 ~v1 v2 = du_'8744''45'isMagma_694 v2
du_'8744''45'isMagma_694 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8744''45'isMagma_694 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isMagma_2976
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-isOrderTheoreticJoinSemilattice
d_'8743''45'isOrderTheoreticJoinSemilattice_696 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_'8743''45'isOrderTheoreticJoinSemilattice_696 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticJoinSemilattice_696 v2
du_'8743''45'isOrderTheoreticJoinSemilattice_696 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_'8743''45'isOrderTheoreticJoinSemilattice_696 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticJoinSemilattice_162
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'semilattice_2984
         (coe v1))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-isOrderTheoreticMeetSemilattice
d_'8743''45'isOrderTheoreticMeetSemilattice_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
d_'8743''45'isOrderTheoreticMeetSemilattice_698 ~v0 ~v1 v2
  = du_'8743''45'isOrderTheoreticMeetSemilattice_698 v2
du_'8743''45'isOrderTheoreticMeetSemilattice_698 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsMeetSemilattice_168
du_'8743''45'isOrderTheoreticMeetSemilattice_698 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'isOrderTheoreticMeetSemilattice_160
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'semilattice_2984
         (coe v1))
-- Algebra.Lattice.Properties.DistributiveLattice._.∨-isSemigroup
d_'8744''45'isSemigroup_700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8744''45'isSemigroup_700 ~v0 ~v1 v2
  = du_'8744''45'isSemigroup_700 v2
du_'8744''45'isSemigroup_700 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8744''45'isSemigroup_700 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isSemigroup_2978
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∨-isSemilattice
d_'8744''45'isSemilattice_702 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_'8744''45'isSemilattice_702 ~v0 ~v1 v2
  = du_'8744''45'isSemilattice_702 v2
du_'8744''45'isSemilattice_702 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_'8744''45'isSemilattice_702 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'isSemilattice_2982
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-orderTheoreticJoinSemilattice
d_'8743''45'orderTheoreticJoinSemilattice_704 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
d_'8743''45'orderTheoreticJoinSemilattice_704 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticJoinSemilattice_704 v2
du_'8743''45'orderTheoreticJoinSemilattice_704 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_JoinSemilattice_14
du_'8743''45'orderTheoreticJoinSemilattice_704 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticJoinSemilattice_166
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'semilattice_2984
         (coe v1))
-- Algebra.Lattice.Properties.DistributiveLattice._.∧-orderTheoreticMeetSemilattice
d_'8743''45'orderTheoreticMeetSemilattice_706 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
d_'8743''45'orderTheoreticMeetSemilattice_706 ~v0 ~v1 v2
  = du_'8743''45'orderTheoreticMeetSemilattice_706 v2
du_'8743''45'orderTheoreticMeetSemilattice_706 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_MeetSemilattice_188
du_'8743''45'orderTheoreticMeetSemilattice_706 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Properties.Semilattice.du_'8743''45'orderTheoreticMeetSemilattice_164
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'semilattice_2984
         (coe v1))
-- Algebra.Lattice.Properties.DistributiveLattice._.∨-semilattice
d_'8744''45'semilattice_708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
d_'8744''45'semilattice_708 ~v0 ~v1 v2
  = du_'8744''45'semilattice_708 v2
du_'8744''45'semilattice_708 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_Semilattice_10
du_'8744''45'semilattice_708 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45'semilattice_2984
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∨-∧-isOrderTheoreticLattice
d_'8744''45''8743''45'isOrderTheoreticLattice_710 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
d_'8744''45''8743''45'isOrderTheoreticLattice_710 ~v0 ~v1 v2
  = du_'8744''45''8743''45'isOrderTheoreticLattice_710 v2
du_'8744''45''8743''45'isOrderTheoreticLattice_710 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
du_'8744''45''8743''45'isOrderTheoreticLattice_710 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45''8743''45'isOrderTheoreticLattice_3010
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice._.∨-∧-orderTheoreticLattice
d_'8744''45''8743''45'orderTheoreticLattice_712 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_Lattice_362
d_'8744''45''8743''45'orderTheoreticLattice_712 ~v0 ~v1 v2
  = du_'8744''45''8743''45'orderTheoreticLattice_712 v2
du_'8744''45''8743''45'orderTheoreticLattice_712 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Lattice.Bundles.T_Lattice_362
du_'8744''45''8743''45'orderTheoreticLattice_712 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8744''45''8743''45'orderTheoreticLattice_3066
      (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0))
-- Algebra.Lattice.Properties.DistributiveLattice.∧-∨-isDistributiveLattice
d_'8743''45''8744''45'isDistributiveLattice_714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_'8743''45''8744''45'isDistributiveLattice_714 ~v0 ~v1 v2
  = du_'8743''45''8744''45'isDistributiveLattice_714 v2
du_'8743''45''8744''45'isDistributiveLattice_714 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
du_'8743''45''8744''45'isDistributiveLattice_714 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.C_IsDistributiveLattice'46'constructor_38127
      (coe
         MAlonzo.Code.Algebra.Lattice.Properties.Lattice.du_'8743''45''8744''45'isLattice_2996
         (coe MAlonzo.Code.Algebra.Lattice.Bundles.du_lattice_662 (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'distrib'45''8744'_2834
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isDistributiveLattice_606
            (coe v0)))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'distrib'45''8743'_2832
         (coe
            MAlonzo.Code.Algebra.Lattice.Bundles.d_isDistributiveLattice_606
            (coe v0)))
-- Algebra.Lattice.Properties.DistributiveLattice.∧-∨-distributiveLattice
d_'8743''45''8744''45'distributiveLattice_716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
d_'8743''45''8744''45'distributiveLattice_716 ~v0 ~v1 v2
  = du_'8743''45''8744''45'distributiveLattice_716 v2
du_'8743''45''8744''45'distributiveLattice_716 ::
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.T_DistributiveLattice_582
du_'8743''45''8744''45'distributiveLattice_716 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.C_DistributiveLattice'46'constructor_9473
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8743'__604 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Bundles.d__'8744'__602 (coe v0))
      (coe du_'8743''45''8744''45'isDistributiveLattice_714 (coe v0))
