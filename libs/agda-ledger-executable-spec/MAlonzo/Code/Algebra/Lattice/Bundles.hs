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

module MAlonzo.Code.Algebra.Lattice.Bundles where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Lattice.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Lattice.Structures
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Lattice.Bundles.Semilattice
d_Semilattice_10 a0 a1 = ()
data T_Semilattice_10
  = C_Semilattice'46'constructor_187 (AgdaAny -> AgdaAny -> AgdaAny)
                                     MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
-- Algebra.Lattice.Bundles.Semilattice.Carrier
d_Carrier_24 :: T_Semilattice_10 -> ()
d_Carrier_24 = erased
-- Algebra.Lattice.Bundles.Semilattice._≈_
d__'8776'__26 :: T_Semilattice_10 -> AgdaAny -> AgdaAny -> ()
d__'8776'__26 = erased
-- Algebra.Lattice.Bundles.Semilattice._∙_
d__'8729'__28 :: T_Semilattice_10 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__28 v0
  = case coe v0 of
      C_Semilattice'46'constructor_187 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.Semilattice.isSemilattice
d_isSemilattice_30 ::
  T_Semilattice_10 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isSemilattice_30 v0
  = case coe v0 of
      C_Semilattice'46'constructor_187 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.Semilattice._.assoc
d_assoc_34 ::
  T_Semilattice_10 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_34 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isSemilattice_30 (coe v0))))
-- Algebra.Lattice.Bundles.Semilattice._.comm
d_comm_36 :: T_Semilattice_10 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_36 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_comm_2454
      (coe d_isSemilattice_30 (coe v0))
-- Algebra.Lattice.Bundles.Semilattice._.idem
d_idem_38 :: T_Semilattice_10 -> AgdaAny -> AgdaAny
d_idem_38 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_482
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isSemilattice_30 (coe v0)))
-- Algebra.Lattice.Bundles.Semilattice._.isBand
d_isBand_40 ::
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_40 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
      (coe d_isSemilattice_30 (coe v0))
-- Algebra.Lattice.Bundles.Semilattice._.isEquivalence
d_isEquivalence_42 ::
  T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_42 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isSemilattice_30 (coe v0)))))
-- Algebra.Lattice.Bundles.Semilattice._.isMagma
d_isMagma_44 ::
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_44 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isSemilattice_30 (coe v0))))
-- Algebra.Lattice.Bundles.Semilattice._.isPartialEquivalence
d_isPartialEquivalence_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_46 ~v0 ~v1 v2
  = du_isPartialEquivalence_46 v2
du_isPartialEquivalence_46 ::
  T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_46 v0
  = let v1 = d_isSemilattice_30 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Lattice.Bundles.Semilattice._.isSemigroup
d_isSemigroup_48 ::
  T_Semilattice_10 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_48 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isSemilattice_30 (coe v0)))
-- Algebra.Lattice.Bundles.Semilattice._.refl
d_refl_50 :: T_Semilattice_10 -> AgdaAny -> AgdaAny
d_refl_50 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isSemilattice_30 (coe v0))))))
-- Algebra.Lattice.Bundles.Semilattice._.reflexive
d_reflexive_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_52 ~v0 ~v1 v2 = du_reflexive_52 v2
du_reflexive_52 ::
  T_Semilattice_10 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_52 v0
  = let v1 = d_isSemilattice_30 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Lattice.Bundles.Semilattice._.setoid
d_setoid_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_54 ~v0 ~v1 v2 = du_setoid_54 v2
du_setoid_54 ::
  T_Semilattice_10 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_54 v0
  = let v1 = d_isSemilattice_30 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Bundles.Semilattice._.sym
d_sym_56 ::
  T_Semilattice_10 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_56 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isSemilattice_30 (coe v0))))))
-- Algebra.Lattice.Bundles.Semilattice._.trans
d_trans_58 ::
  T_Semilattice_10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_58 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isSemilattice_30 (coe v0))))))
-- Algebra.Lattice.Bundles.Semilattice._.∙-cong
d_'8729''45'cong_60 ::
  T_Semilattice_10 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_60 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isSemilattice_30 (coe v0)))))
-- Algebra.Lattice.Bundles.Semilattice._.∙-congʳ
d_'8729''45'cong'691'_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_62 ~v0 ~v1 v2 = du_'8729''45'cong'691'_62 v2
du_'8729''45'cong'691'_62 ::
  T_Semilattice_10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_62 v0
  = let v1 = d_isSemilattice_30 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Bundles.Semilattice._.∙-congˡ
d_'8729''45'cong'737'_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_64 ~v0 ~v1 v2 = du_'8729''45'cong'737'_64 v2
du_'8729''45'cong'737'_64 ::
  T_Semilattice_10 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_64 v0
  = let v1 = d_isSemilattice_30 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Bundles.Semilattice.band
d_band_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Bundles.T_Band_536
d_band_66 ~v0 ~v1 v2 = du_band_66 v2
du_band_66 ::
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Bundles.T_Band_536
du_band_66 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Band'46'constructor_9627
      (d__'8729'__28 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isSemilattice_30 (coe v0)))
-- Algebra.Lattice.Bundles.Semilattice._._≉_
d__'8777'__70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 -> AgdaAny -> AgdaAny -> ()
d__'8777'__70 = erased
-- Algebra.Lattice.Bundles.Semilattice._.isBand
d_isBand_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_72 ~v0 ~v1 v2 = du_isBand_72 v2
du_isBand_72 ::
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_72 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
      (coe d_isSemilattice_30 (coe v0))
-- Algebra.Lattice.Bundles.Semilattice._.isMagma
d_isMagma_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_74 ~v0 ~v1 v2 = du_isMagma_74 v2
du_isMagma_74 ::
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_74 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isSemilattice_30 (coe v0))))
-- Algebra.Lattice.Bundles.Semilattice._.isSemigroup
d_isSemigroup_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_76 ~v0 ~v1 v2 = du_isSemigroup_76 v2
du_isSemigroup_76 ::
  T_Semilattice_10 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_76 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isSemilattice_30 (coe v0)))
-- Algebra.Lattice.Bundles.Semilattice._.magma
d_magma_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_78 ~v0 ~v1 v2 = du_magma_78 v2
du_magma_78 ::
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_78 v0
  = let v1 = coe du_band_66 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v1))
-- Algebra.Lattice.Bundles.Semilattice._.rawMagma
d_rawMagma_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_80 ~v0 ~v1 v2 = du_rawMagma_80 v2
du_rawMagma_80 ::
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_80 v0
  = let v1 = coe du_band_66 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawMagma_52
      (coe MAlonzo.Code.Algebra.Bundles.du_magma_524 (coe v2))
-- Algebra.Lattice.Bundles.Semilattice._.semigroup
d_semigroup_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_82 ~v0 ~v1 v2 = du_semigroup_82 v2
du_semigroup_82 ::
  T_Semilattice_10 -> MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_82 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_588
      (coe du_band_66 (coe v0))
-- Algebra.Lattice.Bundles.MeetSemilattice
d_MeetSemilattice_88 a0 a1 = ()
data T_MeetSemilattice_88
  = C_MeetSemilattice'46'constructor_1447 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
-- Algebra.Lattice.Bundles.MeetSemilattice.Carrier
d_Carrier_102 :: T_MeetSemilattice_88 -> ()
d_Carrier_102 = erased
-- Algebra.Lattice.Bundles.MeetSemilattice._≈_
d__'8776'__104 :: T_MeetSemilattice_88 -> AgdaAny -> AgdaAny -> ()
d__'8776'__104 = erased
-- Algebra.Lattice.Bundles.MeetSemilattice._∧_
d__'8743'__106 ::
  T_MeetSemilattice_88 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__106 v0
  = case coe v0 of
      C_MeetSemilattice'46'constructor_1447 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.MeetSemilattice.isMeetSemilattice
d_isMeetSemilattice_108 ::
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isMeetSemilattice_108 v0
  = case coe v0 of
      C_MeetSemilattice'46'constructor_1447 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.MeetSemilattice._.assoc
d_assoc_112 ::
  T_MeetSemilattice_88 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_112 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isMeetSemilattice_108 (coe v0))))
-- Algebra.Lattice.Bundles.MeetSemilattice._.comm
d_comm_114 :: T_MeetSemilattice_88 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_114 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_comm_2454
      (coe d_isMeetSemilattice_108 (coe v0))
-- Algebra.Lattice.Bundles.MeetSemilattice._.idem
d_idem_116 :: T_MeetSemilattice_88 -> AgdaAny -> AgdaAny
d_idem_116 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_482
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isMeetSemilattice_108 (coe v0)))
-- Algebra.Lattice.Bundles.MeetSemilattice._.isBand
d_isBand_118 ::
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_118 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
      (coe d_isMeetSemilattice_108 (coe v0))
-- Algebra.Lattice.Bundles.MeetSemilattice._.isEquivalence
d_isEquivalence_120 ::
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_120 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isMeetSemilattice_108 (coe v0)))))
-- Algebra.Lattice.Bundles.MeetSemilattice._.isMagma
d_isMagma_122 ::
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_122 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isMeetSemilattice_108 (coe v0))))
-- Algebra.Lattice.Bundles.MeetSemilattice._.isPartialEquivalence
d_isPartialEquivalence_124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_124 ~v0 ~v1 v2
  = du_isPartialEquivalence_124 v2
du_isPartialEquivalence_124 ::
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_124 v0
  = let v1 = d_isMeetSemilattice_108 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Lattice.Bundles.MeetSemilattice._.isSemigroup
d_isSemigroup_126 ::
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_126 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isMeetSemilattice_108 (coe v0)))
-- Algebra.Lattice.Bundles.MeetSemilattice._.refl
d_refl_128 :: T_MeetSemilattice_88 -> AgdaAny -> AgdaAny
d_refl_128 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isMeetSemilattice_108 (coe v0))))))
-- Algebra.Lattice.Bundles.MeetSemilattice._.reflexive
d_reflexive_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_88 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_130 ~v0 ~v1 v2 = du_reflexive_130 v2
du_reflexive_130 ::
  T_MeetSemilattice_88 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_130 v0
  = let v1 = d_isMeetSemilattice_108 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Lattice.Bundles.MeetSemilattice._.setoid
d_setoid_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_132 ~v0 ~v1 v2 = du_setoid_132 v2
du_setoid_132 ::
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_132 v0
  = let v1 = d_isMeetSemilattice_108 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Bundles.MeetSemilattice._.sym
d_sym_134 ::
  T_MeetSemilattice_88 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_134 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isMeetSemilattice_108 (coe v0))))))
-- Algebra.Lattice.Bundles.MeetSemilattice._.trans
d_trans_136 ::
  T_MeetSemilattice_88 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_136 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isMeetSemilattice_108 (coe v0))))))
-- Algebra.Lattice.Bundles.MeetSemilattice._.∙-cong
d_'8729''45'cong_138 ::
  T_MeetSemilattice_88 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_138 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isMeetSemilattice_108 (coe v0)))))
-- Algebra.Lattice.Bundles.MeetSemilattice._.∙-congʳ
d_'8729''45'cong'691'_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_88 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_140 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_140 v2
du_'8729''45'cong'691'_140 ::
  T_MeetSemilattice_88 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_140 v0
  = let v1 = d_isMeetSemilattice_108 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Bundles.MeetSemilattice._.∙-congˡ
d_'8729''45'cong'737'_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_88 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_142 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_142 v2
du_'8729''45'cong'737'_142 ::
  T_MeetSemilattice_88 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_142 v0
  = let v1 = d_isMeetSemilattice_108 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Bundles.MeetSemilattice.semilattice
d_semilattice_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_88 -> T_Semilattice_10
d_semilattice_144 ~v0 ~v1 v2 = du_semilattice_144 v2
du_semilattice_144 :: T_MeetSemilattice_88 -> T_Semilattice_10
du_semilattice_144 v0
  = coe
      C_Semilattice'46'constructor_187 (d__'8743'__106 (coe v0))
      (d_isMeetSemilattice_108 (coe v0))
-- Algebra.Lattice.Bundles.MeetSemilattice._.band
d_band_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_88 -> MAlonzo.Code.Algebra.Bundles.T_Band_536
d_band_148 ~v0 ~v1 v2 = du_band_148 v2
du_band_148 ::
  T_MeetSemilattice_88 -> MAlonzo.Code.Algebra.Bundles.T_Band_536
du_band_148 v0 = coe du_band_66 (coe du_semilattice_144 (coe v0))
-- Algebra.Lattice.Bundles.MeetSemilattice._.magma
d_magma_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_88 -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_150 ~v0 ~v1 v2 = du_magma_150 v2
du_magma_150 ::
  T_MeetSemilattice_88 -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_150 v0
  = let v1 = coe du_semilattice_144 (coe v0) in
    let v2 = coe du_band_66 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v2))
-- Algebra.Lattice.Bundles.MeetSemilattice._.rawMagma
d_rawMagma_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_152 ~v0 ~v1 v2 = du_rawMagma_152 v2
du_rawMagma_152 ::
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_152 v0
  = let v1 = coe du_semilattice_144 (coe v0) in
    let v2 = coe du_band_66 (coe v1) in
    let v3
          = coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawMagma_52
      (coe MAlonzo.Code.Algebra.Bundles.du_magma_524 (coe v3))
-- Algebra.Lattice.Bundles.MeetSemilattice._.semigroup
d_semigroup_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_154 ~v0 ~v1 v2 = du_semigroup_154 v2
du_semigroup_154 ::
  T_MeetSemilattice_88 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_154 v0
  = let v1 = coe du_semilattice_144 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_588
      (coe du_band_66 (coe v1))
-- Algebra.Lattice.Bundles.JoinSemilattice
d_JoinSemilattice_160 a0 a1 = ()
data T_JoinSemilattice_160
  = C_JoinSemilattice'46'constructor_2603 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
-- Algebra.Lattice.Bundles.JoinSemilattice.Carrier
d_Carrier_174 :: T_JoinSemilattice_160 -> ()
d_Carrier_174 = erased
-- Algebra.Lattice.Bundles.JoinSemilattice._≈_
d__'8776'__176 :: T_JoinSemilattice_160 -> AgdaAny -> AgdaAny -> ()
d__'8776'__176 = erased
-- Algebra.Lattice.Bundles.JoinSemilattice._∨_
d__'8744'__178 ::
  T_JoinSemilattice_160 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__178 v0
  = case coe v0 of
      C_JoinSemilattice'46'constructor_2603 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.JoinSemilattice.isJoinSemilattice
d_isJoinSemilattice_180 ::
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isJoinSemilattice_180 v0
  = case coe v0 of
      C_JoinSemilattice'46'constructor_2603 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.JoinSemilattice._.assoc
d_assoc_184 ::
  T_JoinSemilattice_160 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_184 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isJoinSemilattice_180 (coe v0))))
-- Algebra.Lattice.Bundles.JoinSemilattice._.comm
d_comm_186 ::
  T_JoinSemilattice_160 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_186 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_comm_2454
      (coe d_isJoinSemilattice_180 (coe v0))
-- Algebra.Lattice.Bundles.JoinSemilattice._.idem
d_idem_188 :: T_JoinSemilattice_160 -> AgdaAny -> AgdaAny
d_idem_188 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_482
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isJoinSemilattice_180 (coe v0)))
-- Algebra.Lattice.Bundles.JoinSemilattice._.isBand
d_isBand_190 ::
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_190 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
      (coe d_isJoinSemilattice_180 (coe v0))
-- Algebra.Lattice.Bundles.JoinSemilattice._.isEquivalence
d_isEquivalence_192 ::
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_192 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isJoinSemilattice_180 (coe v0)))))
-- Algebra.Lattice.Bundles.JoinSemilattice._.isMagma
d_isMagma_194 ::
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_194 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
            (coe d_isJoinSemilattice_180 (coe v0))))
-- Algebra.Lattice.Bundles.JoinSemilattice._.isPartialEquivalence
d_isPartialEquivalence_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_196 ~v0 ~v1 v2
  = du_isPartialEquivalence_196 v2
du_isPartialEquivalence_196 ::
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_196 v0
  = let v1 = d_isJoinSemilattice_180 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
-- Algebra.Lattice.Bundles.JoinSemilattice._.isSemigroup
d_isSemigroup_198 ::
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_198 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
         (coe d_isJoinSemilattice_180 (coe v0)))
-- Algebra.Lattice.Bundles.JoinSemilattice._.refl
d_refl_200 :: T_JoinSemilattice_160 -> AgdaAny -> AgdaAny
d_refl_200 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isJoinSemilattice_180 (coe v0))))))
-- Algebra.Lattice.Bundles.JoinSemilattice._.reflexive
d_reflexive_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_160 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_202 ~v0 ~v1 v2 = du_reflexive_202 v2
du_reflexive_202 ::
  T_JoinSemilattice_160 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_202 v0
  = let v1 = d_isJoinSemilattice_180 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    let v4 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3) in
    \ v5 v6 v7 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v4))
        v5
-- Algebra.Lattice.Bundles.JoinSemilattice._.setoid
d_setoid_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_204 ~v0 ~v1 v2 = du_setoid_204 v2
du_setoid_204 ::
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_204 v0
  = let v1 = d_isJoinSemilattice_180 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Bundles.JoinSemilattice._.sym
d_sym_206 ::
  T_JoinSemilattice_160 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_206 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isJoinSemilattice_180 (coe v0))))))
-- Algebra.Lattice.Bundles.JoinSemilattice._.trans
d_trans_208 ::
  T_JoinSemilattice_160 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_208 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
               (coe
                  MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
                  (coe d_isJoinSemilattice_180 (coe v0))))))
-- Algebra.Lattice.Bundles.JoinSemilattice._.∙-cong
d_'8729''45'cong_210 ::
  T_JoinSemilattice_160 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_210 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_480
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452
               (coe d_isJoinSemilattice_180 (coe v0)))))
-- Algebra.Lattice.Bundles.JoinSemilattice._.∙-congʳ
d_'8729''45'cong'691'_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_160 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_212 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_212 v2
du_'8729''45'cong'691'_212 ::
  T_JoinSemilattice_160 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_212 v0
  = let v1 = d_isJoinSemilattice_180 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Bundles.JoinSemilattice._.∙-congˡ
d_'8729''45'cong'737'_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_160 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_214 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_214 v2
du_'8729''45'cong'737'_214 ::
  T_JoinSemilattice_160 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_214 v0
  = let v1 = d_isJoinSemilattice_180 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v3))
-- Algebra.Lattice.Bundles.JoinSemilattice.semilattice
d_semilattice_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_160 -> T_Semilattice_10
d_semilattice_216 ~v0 ~v1 v2 = du_semilattice_216 v2
du_semilattice_216 :: T_JoinSemilattice_160 -> T_Semilattice_10
du_semilattice_216 v0
  = coe
      C_Semilattice'46'constructor_187 (d__'8744'__178 (coe v0))
      (d_isJoinSemilattice_180 (coe v0))
-- Algebra.Lattice.Bundles.JoinSemilattice._.band
d_band_220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_160 -> MAlonzo.Code.Algebra.Bundles.T_Band_536
d_band_220 ~v0 ~v1 v2 = du_band_220 v2
du_band_220 ::
  T_JoinSemilattice_160 -> MAlonzo.Code.Algebra.Bundles.T_Band_536
du_band_220 v0 = coe du_band_66 (coe du_semilattice_216 (coe v0))
-- Algebra.Lattice.Bundles.JoinSemilattice._.magma
d_magma_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_160 -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_222 ~v0 ~v1 v2 = du_magma_222 v2
du_magma_222 ::
  T_JoinSemilattice_160 -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_222 v0
  = let v1 = coe du_semilattice_216 (coe v0) in
    let v2 = coe du_band_66 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v2))
-- Algebra.Lattice.Bundles.JoinSemilattice._.rawMagma
d_rawMagma_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_224 ~v0 ~v1 v2 = du_rawMagma_224 v2
du_rawMagma_224 ::
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_224 v0
  = let v1 = coe du_semilattice_216 (coe v0) in
    let v2 = coe du_band_66 (coe v1) in
    let v3
          = coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawMagma_52
      (coe MAlonzo.Code.Algebra.Bundles.du_magma_524 (coe v3))
-- Algebra.Lattice.Bundles.JoinSemilattice._.semigroup
d_semigroup_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_226 ~v0 ~v1 v2 = du_semigroup_226 v2
du_semigroup_226 ::
  T_JoinSemilattice_160 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_226 v0
  = let v1 = coe du_semilattice_216 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_588
      (coe du_band_66 (coe v1))
-- Algebra.Lattice.Bundles.BoundedSemilattice
d_BoundedSemilattice_232 a0 a1 = ()
data T_BoundedSemilattice_232
  = C_BoundedSemilattice'46'constructor_3791 (AgdaAny ->
                                              AgdaAny -> AgdaAny)
                                             AgdaAny
                                             MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
-- Algebra.Lattice.Bundles.BoundedSemilattice.Carrier
d_Carrier_248 :: T_BoundedSemilattice_232 -> ()
d_Carrier_248 = erased
-- Algebra.Lattice.Bundles.BoundedSemilattice._≈_
d__'8776'__250 ::
  T_BoundedSemilattice_232 -> AgdaAny -> AgdaAny -> ()
d__'8776'__250 = erased
-- Algebra.Lattice.Bundles.BoundedSemilattice._∙_
d__'8729'__252 ::
  T_BoundedSemilattice_232 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__252 v0
  = case coe v0 of
      C_BoundedSemilattice'46'constructor_3791 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BoundedSemilattice.ε
d_ε_254 :: T_BoundedSemilattice_232 -> AgdaAny
d_ε_254 v0
  = case coe v0 of
      C_BoundedSemilattice'46'constructor_3791 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BoundedSemilattice.isBoundedSemilattice
d_isBoundedSemilattice_256 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
d_isBoundedSemilattice_256 v0
  = case coe v0 of
      C_BoundedSemilattice'46'constructor_3791 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BoundedSemilattice._.assoc
d_assoc_260 ::
  T_BoundedSemilattice_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_260 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe d_isBoundedSemilattice_256 (coe v0)))))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.comm
d_comm_262 ::
  T_BoundedSemilattice_232 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_262 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
         (coe d_isBoundedSemilattice_256 (coe v0)))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.idem
d_idem_264 :: T_BoundedSemilattice_232 -> AgdaAny -> AgdaAny
d_idem_264 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_idem_722
      (coe d_isBoundedSemilattice_256 (coe v0))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.identity
d_identity_266 ::
  T_BoundedSemilattice_232 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_266 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe d_isBoundedSemilattice_256 (coe v0))))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.identityʳ
d_identity'691'_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 -> AgdaAny -> AgdaAny
d_identity'691'_268 ~v0 ~v1 v2 = du_identity'691'_268 v2
du_identity'691'_268 ::
  T_BoundedSemilattice_232 -> AgdaAny -> AgdaAny
du_identity'691'_268 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.identityˡ
d_identity'737'_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 -> AgdaAny -> AgdaAny
d_identity'737'_270 ~v0 ~v1 v2 = du_identity'737'_270 v2
du_identity'737'_270 ::
  T_BoundedSemilattice_232 -> AgdaAny -> AgdaAny
du_identity'737'_270 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isBand
d_isBand_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_272 ~v0 ~v1 v2 = du_isBand_272 v2
du_isBand_272 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_272 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.du_isBand_768 (coe v1)
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isCommutativeMagma
d_isCommutativeMagma_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
d_isCommutativeMagma_274 ~v0 ~v1 v2 = du_isCommutativeMagma_274 v2
du_isCommutativeMagma_274 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMagma_176
du_isCommutativeMagma_274 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeMagma_550
      (coe
         MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
         (coe v2))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isCommutativeMonoid
d_isCommutativeMonoid_276 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_isCommutativeMonoid_276 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
      (coe d_isBoundedSemilattice_256 (coe v0))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isCommutativeSemigroup
d_isCommutativeSemigroup_278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
d_isCommutativeSemigroup_278 ~v0 ~v1 v2
  = du_isCommutativeSemigroup_278 v2
du_isCommutativeSemigroup_278 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemigroup_512
du_isCommutativeSemigroup_278 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isCommutativeSemigroup_700
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v1))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isEquivalence
d_isEquivalence_280 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_280 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe d_isBoundedSemilattice_256 (coe v0))))))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isMagma
d_isMagma_282 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_282 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe d_isBoundedSemilattice_256 (coe v0)))))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isMonoid
d_isMonoid_284 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_isMonoid_284 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
         (coe d_isBoundedSemilattice_256 (coe v0)))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isPartialEquivalence
d_isPartialEquivalence_286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_286 ~v0 ~v1 v2
  = du_isPartialEquivalence_286 v2
du_isPartialEquivalence_286 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_286 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isSemigroup
d_isSemigroup_288 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_288 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe d_isBoundedSemilattice_256 (coe v0))))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isSemilattice
d_isSemilattice_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isSemilattice_290 ~v0 ~v1 v2 = du_isSemilattice_290 v2
du_isSemilattice_290 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_isSemilattice_290 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
      (coe d_isBoundedSemilattice_256 (coe v0))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.isUnitalMagma
d_isUnitalMagma_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
d_isUnitalMagma_292 ~v0 ~v1 v2 = du_isUnitalMagma_292 v2
du_isUnitalMagma_292 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Structures.T_IsUnitalMagma_556
du_isUnitalMagma_292 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_isUnitalMagma_644
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.refl
d_refl_294 :: T_BoundedSemilattice_232 -> AgdaAny -> AgdaAny
d_refl_294 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe d_isBoundedSemilattice_256 (coe v0)))))))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.reflexive
d_reflexive_296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_296 ~v0 ~v1 v2 = du_reflexive_296 v2
du_reflexive_296 ::
  T_BoundedSemilattice_232 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_296 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
        v6
-- Algebra.Lattice.Bundles.BoundedSemilattice._.setoid
d_setoid_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_298 ~v0 ~v1 v2 = du_setoid_298 v2
du_setoid_298 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_298 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.sym
d_sym_300 ::
  T_BoundedSemilattice_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_300 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe d_isBoundedSemilattice_256 (coe v0)))))))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.trans
d_trans_302 ::
  T_BoundedSemilattice_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_302 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe d_isBoundedSemilattice_256 (coe v0)))))))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.∙-cong
d_'8729''45'cong_304 ::
  T_BoundedSemilattice_232 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_304 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe d_isBoundedSemilattice_256 (coe v0))))))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.∙-congʳ
d_'8729''45'cong'691'_306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_306 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_306 v2
du_'8729''45'cong'691'_306 ::
  T_BoundedSemilattice_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_306 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.∙-congˡ
d_'8729''45'cong'737'_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_308 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_308 v2
du_'8729''45'cong'737'_308 ::
  T_BoundedSemilattice_232 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_308 v0
  = let v1 = d_isBoundedSemilattice_256 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    let v3 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Lattice.Bundles.BoundedSemilattice.semilattice
d_semilattice_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 -> T_Semilattice_10
d_semilattice_310 ~v0 ~v1 v2 = du_semilattice_310 v2
du_semilattice_310 :: T_BoundedSemilattice_232 -> T_Semilattice_10
du_semilattice_310 v0
  = coe
      C_Semilattice'46'constructor_187 (d__'8729'__252 (coe v0))
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
         (coe d_isBoundedSemilattice_256 (coe v0)))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.band
d_band_314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 -> MAlonzo.Code.Algebra.Bundles.T_Band_536
d_band_314 ~v0 ~v1 v2 = du_band_314 v2
du_band_314 ::
  T_BoundedSemilattice_232 -> MAlonzo.Code.Algebra.Bundles.T_Band_536
du_band_314 v0 = coe du_band_66 (coe du_semilattice_310 (coe v0))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.magma
d_magma_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_316 ~v0 ~v1 v2 = du_magma_316 v2
du_magma_316 ::
  T_BoundedSemilattice_232 -> MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_316 v0
  = let v1 = coe du_semilattice_310 (coe v0) in
    let v2 = coe du_band_66 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v2))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.rawMagma
d_rawMagma_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_318 ~v0 ~v1 v2 = du_rawMagma_318 v2
du_rawMagma_318 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_318 v0
  = let v1 = coe du_semilattice_310 (coe v0) in
    let v2 = coe du_band_66 (coe v1) in
    let v3
          = coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawMagma_52
      (coe MAlonzo.Code.Algebra.Bundles.du_magma_524 (coe v3))
-- Algebra.Lattice.Bundles.BoundedSemilattice._.semigroup
d_semigroup_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_320 ~v0 ~v1 v2 = du_semigroup_320 v2
du_semigroup_320 ::
  T_BoundedSemilattice_232 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_320 v0
  = let v1 = coe du_semilattice_310 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_588
      (coe du_band_66 (coe v1))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice
d_BoundedMeetSemilattice_326 a0 a1 = ()
data T_BoundedMeetSemilattice_326
  = C_BoundedMeetSemilattice'46'constructor_5213 (AgdaAny ->
                                                  AgdaAny -> AgdaAny)
                                                 AgdaAny
                                                 MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice.Carrier
d_Carrier_342 :: T_BoundedMeetSemilattice_326 -> ()
d_Carrier_342 = erased
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._≈_
d__'8776'__344 ::
  T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny -> ()
d__'8776'__344 = erased
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._∧_
d__'8743'__346 ::
  T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__346 v0
  = case coe v0 of
      C_BoundedMeetSemilattice'46'constructor_5213 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice.⊤
d_'8868'_348 :: T_BoundedMeetSemilattice_326 -> AgdaAny
d_'8868'_348 v0
  = case coe v0 of
      C_BoundedMeetSemilattice'46'constructor_5213 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice.isBoundedMeetSemilattice
d_isBoundedMeetSemilattice_350 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
d_isBoundedMeetSemilattice_350 v0
  = case coe v0 of
      C_BoundedMeetSemilattice'46'constructor_5213 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.assoc
d_assoc_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_354 ~v0 ~v1 v2 = du_assoc_354 v2
du_assoc_354 ::
  T_BoundedMeetSemilattice_326 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_354 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v1))))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.comm
d_comm_356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_356 ~v0 ~v1 v2 = du_comm_356 v2
du_comm_356 ::
  T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_356 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v1))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.idem
d_idem_358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny
d_idem_358 ~v0 ~v1 v2 = du_idem_358 v2
du_idem_358 :: T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny
du_idem_358 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.d_idem_722 (coe v1)
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.identity
d_identity_360 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_360 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe d_isBoundedMeetSemilattice_350 (coe v0))))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.identityʳ
d_identity'691'_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny
d_identity'691'_362 ~v0 ~v1 v2 = du_identity'691'_362 v2
du_identity'691'_362 ::
  T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny
du_identity'691'_362 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.identityˡ
d_identity'737'_364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny
d_identity'737'_364 ~v0 ~v1 v2 = du_identity'737'_364 v2
du_identity'737'_364 ::
  T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny
du_identity'737'_364 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.isBand
d_isBand_366 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_366 ~v0 ~v1 v2 = du_isBand_366 v2
du_isBand_366 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_366 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.du_isBand_768 (coe v1)
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.isEquivalence
d_isEquivalence_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_368 ~v0 ~v1 v2 = du_isEquivalence_368 v2
du_isEquivalence_368 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_368 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe v1)))))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.isMagma
d_isMagma_370 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_370 ~v0 ~v1 v2 = du_isMagma_370 v2
du_isMagma_370 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_370 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v1))))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.isSemilattice
d_isSemilattice_372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isSemilattice_372 ~v0 ~v1 v2 = du_isSemilattice_372 v2
du_isSemilattice_372 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_isSemilattice_372 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
      (coe v1)
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.isPartialEquivalence
d_isPartialEquivalence_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_374 ~v0 ~v1 v2
  = du_isPartialEquivalence_374 v2
du_isPartialEquivalence_374 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_374 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.isSemigroup
d_isSemigroup_376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_376 ~v0 ~v1 v2 = du_isSemigroup_376 v2
du_isSemigroup_376 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_376 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v1)))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.refl
d_refl_378 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny
d_refl_378 ~v0 ~v1 v2 = du_refl_378 v2
du_refl_378 :: T_BoundedMeetSemilattice_326 -> AgdaAny -> AgdaAny
du_refl_378 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe v1))))))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.reflexive
d_reflexive_380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_380 ~v0 ~v1 v2 = du_reflexive_380 v2
du_reflexive_380 ::
  T_BoundedMeetSemilattice_326 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_380 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
        v6
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.setoid
d_setoid_382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_382 ~v0 ~v1 v2 = du_setoid_382 v2
du_setoid_382 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_382 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.sym
d_sym_384 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_384 ~v0 ~v1 v2 = du_sym_384 v2
du_sym_384 ::
  T_BoundedMeetSemilattice_326 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_384 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe v1))))))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.trans
d_trans_386 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_386 ~v0 ~v1 v2 = du_trans_386 v2
du_trans_386 ::
  T_BoundedMeetSemilattice_326 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_386 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe v1))))))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.∙-cong
d_'8729''45'cong_388 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_388 ~v0 ~v1 v2 = du_'8729''45'cong_388 v2
du_'8729''45'cong_388 ::
  T_BoundedMeetSemilattice_326 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong_388 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe v1)))))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.∙-congʳ
d_'8729''45'cong'691'_390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_390 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_390 v2
du_'8729''45'cong'691'_390 ::
  T_BoundedMeetSemilattice_326 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_390 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.∙-congˡ
d_'8729''45'cong'737'_392 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_392 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_392 v2
du_'8729''45'cong'737'_392 ::
  T_BoundedMeetSemilattice_326 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_392 v0
  = let v1 = d_isBoundedMeetSemilattice_350 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice.boundedSemilattice
d_boundedSemilattice_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 -> T_BoundedSemilattice_232
d_boundedSemilattice_394 ~v0 ~v1 v2 = du_boundedSemilattice_394 v2
du_boundedSemilattice_394 ::
  T_BoundedMeetSemilattice_326 -> T_BoundedSemilattice_232
du_boundedSemilattice_394 v0
  = coe
      C_BoundedSemilattice'46'constructor_3791 (d__'8743'__346 (coe v0))
      (d_'8868'_348 (coe v0)) (d_isBoundedMeetSemilattice_350 (coe v0))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.band
d_band_398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Bundles.T_Band_536
d_band_398 ~v0 ~v1 v2 = du_band_398 v2
du_band_398 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Bundles.T_Band_536
du_band_398 v0
  = let v1 = coe du_boundedSemilattice_394 (coe v0) in
    coe du_band_66 (coe du_semilattice_310 (coe v1))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.magma
d_magma_400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_400 ~v0 ~v1 v2 = du_magma_400 v2
du_magma_400 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_400 v0
  = let v1 = coe du_boundedSemilattice_394 (coe v0) in
    let v2 = coe du_semilattice_310 (coe v1) in
    let v3 = coe du_band_66 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v3))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.rawMagma
d_rawMagma_402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_402 ~v0 ~v1 v2 = du_rawMagma_402 v2
du_rawMagma_402 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_402 v0
  = let v1 = coe du_boundedSemilattice_394 (coe v0) in
    let v2 = coe du_semilattice_310 (coe v1) in
    let v3 = coe du_band_66 (coe v2) in
    let v4
          = coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawMagma_52
      (coe MAlonzo.Code.Algebra.Bundles.du_magma_524 (coe v4))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.semigroup
d_semigroup_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_404 ~v0 ~v1 v2 = du_semigroup_404 v2
du_semigroup_404 ::
  T_BoundedMeetSemilattice_326 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_404 v0
  = let v1 = coe du_boundedSemilattice_394 (coe v0) in
    let v2 = coe du_semilattice_310 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_588
      (coe du_band_66 (coe v2))
-- Algebra.Lattice.Bundles.BoundedMeetSemilattice._.semilattice
d_semilattice_406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedMeetSemilattice_326 -> T_Semilattice_10
d_semilattice_406 ~v0 ~v1 v2 = du_semilattice_406 v2
du_semilattice_406 ::
  T_BoundedMeetSemilattice_326 -> T_Semilattice_10
du_semilattice_406 v0
  = coe du_semilattice_310 (coe du_boundedSemilattice_394 (coe v0))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice
d_BoundedJoinSemilattice_412 a0 a1 = ()
data T_BoundedJoinSemilattice_412
  = C_BoundedJoinSemilattice'46'constructor_6545 (AgdaAny ->
                                                  AgdaAny -> AgdaAny)
                                                 AgdaAny
                                                 MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice.Carrier
d_Carrier_428 :: T_BoundedJoinSemilattice_412 -> ()
d_Carrier_428 = erased
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._≈_
d__'8776'__430 ::
  T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny -> ()
d__'8776'__430 = erased
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._∨_
d__'8744'__432 ::
  T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__432 v0
  = case coe v0 of
      C_BoundedJoinSemilattice'46'constructor_6545 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice.⊥
d_'8869'_434 :: T_BoundedJoinSemilattice_412 -> AgdaAny
d_'8869'_434 v0
  = case coe v0 of
      C_BoundedJoinSemilattice'46'constructor_6545 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice.isBoundedJoinSemilattice
d_isBoundedJoinSemilattice_436 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Structures.T_IsIdempotentCommutativeMonoid_710
d_isBoundedJoinSemilattice_436 v0
  = case coe v0 of
      C_BoundedJoinSemilattice'46'constructor_6545 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.assoc
d_assoc_440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_440 ~v0 ~v1 v2 = du_assoc_440 v2
du_assoc_440 ::
  T_BoundedJoinSemilattice_412 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_440 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_assoc_446
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v1))))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.comm
d_comm_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_442 ~v0 ~v1 v2 = du_comm_442 v2
du_comm_442 ::
  T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_442 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_comm_662
      (coe
         MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720 (coe v1))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.idem
d_idem_444 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny
d_idem_444 ~v0 ~v1 v2 = du_idem_444 v2
du_idem_444 :: T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny
du_idem_444 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.d_idem_722 (coe v1)
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.identity
d_identity_446 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_identity_446 v0
  = coe
      MAlonzo.Code.Algebra.Structures.d_identity_612
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe d_isBoundedJoinSemilattice_436 (coe v0))))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.identityʳ
d_identity'691'_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny
d_identity'691'_448 ~v0 ~v1 v2 = du_identity'691'_448 v2
du_identity'691'_448 ::
  T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny
du_identity'691'_448 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'691'_642
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.identityˡ
d_identity'737'_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny
d_identity'737'_450 ~v0 ~v1 v2 = du_identity'737'_450 v2
du_identity'737'_450 ::
  T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny
du_identity'737'_450 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Structures.du_identity'737'_640
      (coe MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v2))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.isBand
d_isBand_452 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_452 ~v0 ~v1 v2 = du_isBand_452 v2
du_isBand_452 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_452 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe MAlonzo.Code.Algebra.Structures.du_isBand_768 (coe v1)
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.isEquivalence
d_isEquivalence_454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_454 ~v0 ~v1 v2 = du_isEquivalence_454 v2
du_isEquivalence_454 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_454 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe v1)))))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.isSemilattice
d_isSemilattice_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
d_isSemilattice_456 ~v0 ~v1 v2 = du_isSemilattice_456 v2
du_isSemilattice_456 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsSemilattice_2444
du_isSemilattice_456 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
      (coe v1)
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.isMagma
d_isMagma_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_458 ~v0 ~v1 v2 = du_isMagma_458 v2
du_isMagma_458 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_458 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_isMagma_444
      (coe
         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
            (coe
               MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
               (coe v1))))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.isPartialEquivalence
d_isPartialEquivalence_460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_460 ~v0 ~v1 v2
  = du_isPartialEquivalence_460 v2
du_isPartialEquivalence_460 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_460 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.isSemigroup
d_isSemigroup_462 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_462 ~v0 ~v1 v2 = du_isSemigroup_462 v2
du_isSemigroup_462 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_462 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
         (coe
            MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
            (coe v1)))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.refl
d_refl_464 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny
d_refl_464 ~v0 ~v1 v2 = du_refl_464 v2
du_refl_464 :: T_BoundedJoinSemilattice_412 -> AgdaAny -> AgdaAny
du_refl_464 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe v1))))))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.reflexive
d_reflexive_466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_466 ~v0 ~v1 v2 = du_reflexive_466 v2
du_reflexive_466 ::
  T_BoundedJoinSemilattice_412 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_466 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v3) in
    let v5 = MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4) in
    \ v6 v7 v8 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe MAlonzo.Code.Algebra.Structures.d_isEquivalence_148 (coe v5))
        v6
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.setoid
d_setoid_468 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_468 ~v0 ~v1 v2 = du_setoid_468 v2
du_setoid_468 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_468 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_setoid_164
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.sym
d_sym_470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_470 ~v0 ~v1 v2 = du_sym_470 v2
du_sym_470 ::
  T_BoundedJoinSemilattice_412 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_470 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe v1))))))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.trans
d_trans_472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_472 ~v0 ~v1 v2 = du_trans_472 v2
du_trans_472 ::
  T_BoundedJoinSemilattice_412 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_472 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                     (coe v1))))))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.∙-cong
d_'8729''45'cong_474 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong_474 ~v0 ~v1 v2 = du_'8729''45'cong_474 v2
du_'8729''45'cong_474 ::
  T_BoundedJoinSemilattice_412 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong_474 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
      (coe
         MAlonzo.Code.Algebra.Structures.d_isMagma_444
         (coe
            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
            (coe
               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isCommutativeMonoid_720
                  (coe v1)))))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.∙-congʳ
d_'8729''45'cong'691'_476 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'691'_476 ~v0 ~v1 v2
  = du_'8729''45'cong'691'_476 v2
du_'8729''45'cong'691'_476 ::
  T_BoundedJoinSemilattice_412 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'691'_476 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.∙-congˡ
d_'8729''45'cong'737'_478 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8729''45'cong'737'_478 ~v0 ~v1 v2
  = du_'8729''45'cong'737'_478 v2
du_'8729''45'cong'737'_478 ::
  T_BoundedJoinSemilattice_412 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8729''45'cong'737'_478 v0
  = let v1 = d_isBoundedJoinSemilattice_436 (coe v0) in
    let v2
          = coe
              MAlonzo.Code.Algebra.Lattice.Structures.du_isSemilattice_2630
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isBand_2452 (coe v2) in
    let v4
          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_480 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice.boundedSemilattice
d_boundedSemilattice_480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 -> T_BoundedSemilattice_232
d_boundedSemilattice_480 ~v0 ~v1 v2 = du_boundedSemilattice_480 v2
du_boundedSemilattice_480 ::
  T_BoundedJoinSemilattice_412 -> T_BoundedSemilattice_232
du_boundedSemilattice_480 v0
  = coe
      C_BoundedSemilattice'46'constructor_3791 (d__'8744'__432 (coe v0))
      (d_'8869'_434 (coe v0)) (d_isBoundedJoinSemilattice_436 (coe v0))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.band
d_band_484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Bundles.T_Band_536
d_band_484 ~v0 ~v1 v2 = du_band_484 v2
du_band_484 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Bundles.T_Band_536
du_band_484 v0
  = let v1 = coe du_boundedSemilattice_480 (coe v0) in
    coe du_band_66 (coe du_semilattice_310 (coe v1))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.magma
d_magma_486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_magma_486 ~v0 ~v1 v2 = du_magma_486 v2
du_magma_486 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_magma_486 v0
  = let v1 = coe du_boundedSemilattice_480 (coe v0) in
    let v2 = coe du_semilattice_310 (coe v1) in
    let v3 = coe du_band_66 (coe v2) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_magma_524
      (coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v3))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.rawMagma
d_rawMagma_488 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_rawMagma_488 ~v0 ~v1 v2 = du_rawMagma_488 v2
du_rawMagma_488 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_rawMagma_488 v0
  = let v1 = coe du_boundedSemilattice_480 (coe v0) in
    let v2 = coe du_semilattice_310 (coe v1) in
    let v3 = coe du_band_66 (coe v2) in
    let v4
          = coe MAlonzo.Code.Algebra.Bundles.du_semigroup_588 (coe v3) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_rawMagma_52
      (coe MAlonzo.Code.Algebra.Bundles.du_magma_524 (coe v4))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.semigroup
d_semigroup_490 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_semigroup_490 ~v0 ~v1 v2 = du_semigroup_490 v2
du_semigroup_490 ::
  T_BoundedJoinSemilattice_412 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_semigroup_490 v0
  = let v1 = coe du_boundedSemilattice_480 (coe v0) in
    let v2 = coe du_semilattice_310 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Bundles.du_semigroup_588
      (coe du_band_66 (coe v2))
-- Algebra.Lattice.Bundles.BoundedJoinSemilattice._.semilattice
d_semilattice_492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BoundedJoinSemilattice_412 -> T_Semilattice_10
d_semilattice_492 ~v0 ~v1 v2 = du_semilattice_492 v2
du_semilattice_492 ::
  T_BoundedJoinSemilattice_412 -> T_Semilattice_10
du_semilattice_492 v0
  = coe du_semilattice_310 (coe du_boundedSemilattice_480 (coe v0))
-- Algebra.Lattice.Bundles.Lattice
d_Lattice_498 a0 a1 = ()
data T_Lattice_498
  = C_Lattice'46'constructor_7911 (AgdaAny -> AgdaAny -> AgdaAny)
                                  (AgdaAny -> AgdaAny -> AgdaAny)
                                  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
-- Algebra.Lattice.Bundles.Lattice.Carrier
d_Carrier_514 :: T_Lattice_498 -> ()
d_Carrier_514 = erased
-- Algebra.Lattice.Bundles.Lattice._≈_
d__'8776'__516 :: T_Lattice_498 -> AgdaAny -> AgdaAny -> ()
d__'8776'__516 = erased
-- Algebra.Lattice.Bundles.Lattice._∨_
d__'8744'__518 :: T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__518 v0
  = case coe v0 of
      C_Lattice'46'constructor_7911 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.Lattice._∧_
d__'8743'__520 :: T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__520 v0
  = case coe v0 of
      C_Lattice'46'constructor_7911 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.Lattice.isLattice
d_isLattice_522 ::
  T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_isLattice_522 v0
  = case coe v0 of
      C_Lattice'46'constructor_7911 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.Lattice._.absorptive
d_absorptive_526 ::
  T_Lattice_498 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_526 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_absorptive_2780
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.isEquivalence
d_isEquivalence_528 ::
  T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_528 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.isPartialEquivalence
d_isPartialEquivalence_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_530 ~v0 ~v1 v2
  = du_isPartialEquivalence_530 v2
du_isPartialEquivalence_530 ::
  T_Lattice_498 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_530 v0
  = let v1 = d_isLattice_522 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe v1))
-- Algebra.Lattice.Bundles.Lattice._.refl
d_refl_532 :: T_Lattice_498 -> AgdaAny -> AgdaAny
d_refl_532 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe d_isLattice_522 (coe v0)))
-- Algebra.Lattice.Bundles.Lattice._.reflexive
d_reflexive_534 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_534 ~v0 ~v1 v2 = du_reflexive_534 v2
du_reflexive_534 ::
  T_Lattice_498 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_534 v0
  = let v1 = d_isLattice_522 (coe v0) in
    \ v2 v3 v4 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
           (coe v1))
        v2
-- Algebra.Lattice.Bundles.Lattice._.sym
d_sym_536 ::
  T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_536 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe d_isLattice_522 (coe v0)))
-- Algebra.Lattice.Bundles.Lattice._.trans
d_trans_538 ::
  T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_538 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe d_isLattice_522 (coe v0)))
-- Algebra.Lattice.Bundles.Lattice._.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_540 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_540 ~v0 ~v1 v2
  = du_'8743''45'absorbs'45''8744'_540 v2
du_'8743''45'absorbs'45''8744'_540 ::
  T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_540 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∧-assoc
d_'8743''45'assoc_542 ::
  T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'assoc_542 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∧-comm
d_'8743''45'comm_544 ::
  T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'comm_544 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∧-cong
d_'8743''45'cong_546 ::
  T_Lattice_498 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong_546 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∧-congʳ
d_'8743''45'cong'691'_548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'691'_548 ~v0 ~v1 v2
  = du_'8743''45'cong'691'_548 v2
du_'8743''45'cong'691'_548 ::
  T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'691'_548 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∧-congˡ
d_'8743''45'cong'737'_550 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'737'_550 ~v0 ~v1 v2
  = du_'8743''45'cong'737'_550 v2
du_'8743''45'cong'737'_550 ::
  T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'737'_550 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_552 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_552 ~v0 ~v1 v2
  = du_'8744''45'absorbs'45''8743'_552 v2
du_'8744''45'absorbs'45''8743'_552 ::
  T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_552 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∨-assoc
d_'8744''45'assoc_554 ::
  T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'assoc_554 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∨-comm
d_'8744''45'comm_556 ::
  T_Lattice_498 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'comm_556 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∨-cong
d_'8744''45'cong_558 ::
  T_Lattice_498 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong_558 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∨-congʳ
d_'8744''45'cong'691'_560 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'691'_560 ~v0 ~v1 v2
  = du_'8744''45'cong'691'_560 v2
du_'8744''45'cong'691'_560 ::
  T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'691'_560 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∨-congˡ
d_'8744''45'cong'737'_562 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'737'_562 ~v0 ~v1 v2
  = du_'8744''45'cong'737'_562 v2
du_'8744''45'cong'737'_562 ::
  T_Lattice_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'737'_562 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
      (coe d_isLattice_522 (coe v0))
-- Algebra.Lattice.Bundles.Lattice.rawLattice
d_rawLattice_564 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12
d_rawLattice_564 ~v0 ~v1 v2 = du_rawLattice_564 v2
du_rawLattice_564 ::
  T_Lattice_498 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12
du_rawLattice_564 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.C_RawLattice'46'constructor_119
      (d__'8743'__520 (coe v0)) (d__'8744'__518 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∧-rawMagma
d_'8743''45'rawMagma_568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8743''45'rawMagma_568 ~v0 ~v1 v2 = du_'8743''45'rawMagma_568 v2
du_'8743''45'rawMagma_568 ::
  T_Lattice_498 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'8743''45'rawMagma_568 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
      (coe du_rawLattice_564 (coe v0))
-- Algebra.Lattice.Bundles.Lattice._.∨-rawMagma
d_'8744''45'rawMagma_570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8744''45'rawMagma_570 ~v0 ~v1 v2 = du_'8744''45'rawMagma_570 v2
du_'8744''45'rawMagma_570 ::
  T_Lattice_498 -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'8744''45'rawMagma_570 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
      (coe du_rawLattice_564 (coe v0))
-- Algebra.Lattice.Bundles.Lattice.setoid
d_setoid_572 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_572 ~v0 ~v1 v2 = du_setoid_572 v2
du_setoid_572 ::
  T_Lattice_498 -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_572 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe d_isLattice_522 (coe v0)))
-- Algebra.Lattice.Bundles.Lattice._._≉_
d__'8777'__576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Lattice_498 -> AgdaAny -> AgdaAny -> ()
d__'8777'__576 = erased
-- Algebra.Lattice.Bundles.DistributiveLattice
d_DistributiveLattice_582 a0 a1 = ()
data T_DistributiveLattice_582
  = C_DistributiveLattice'46'constructor_9473 (AgdaAny ->
                                               AgdaAny -> AgdaAny)
                                              (AgdaAny -> AgdaAny -> AgdaAny)
                                              MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
-- Algebra.Lattice.Bundles.DistributiveLattice.Carrier
d_Carrier_598 :: T_DistributiveLattice_582 -> ()
d_Carrier_598 = erased
-- Algebra.Lattice.Bundles.DistributiveLattice._≈_
d__'8776'__600 ::
  T_DistributiveLattice_582 -> AgdaAny -> AgdaAny -> ()
d__'8776'__600 = erased
-- Algebra.Lattice.Bundles.DistributiveLattice._∨_
d__'8744'__602 ::
  T_DistributiveLattice_582 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__602 v0
  = case coe v0 of
      C_DistributiveLattice'46'constructor_9473 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.DistributiveLattice._∧_
d__'8743'__604 ::
  T_DistributiveLattice_582 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__604 v0
  = case coe v0 of
      C_DistributiveLattice'46'constructor_9473 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.DistributiveLattice.isDistributiveLattice
d_isDistributiveLattice_606 ::
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_isDistributiveLattice_606 v0
  = case coe v0 of
      C_DistributiveLattice'46'constructor_9473 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.DistributiveLattice._.absorptive
d_absorptive_610 ::
  T_DistributiveLattice_582 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_610 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_absorptive_2780
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_606 (coe v0)))
-- Algebra.Lattice.Bundles.DistributiveLattice._.isEquivalence
d_isEquivalence_612 ::
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_612 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_606 (coe v0)))
-- Algebra.Lattice.Bundles.DistributiveLattice._.isLattice
d_isLattice_614 ::
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_isLattice_614 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
      (coe d_isDistributiveLattice_606 (coe v0))
-- Algebra.Lattice.Bundles.DistributiveLattice._.isPartialEquivalence
d_isPartialEquivalence_616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_616 ~v0 ~v1 v2
  = du_isPartialEquivalence_616 v2
du_isPartialEquivalence_616 ::
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_616 v0
  = let v1 = d_isDistributiveLattice_606 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
              (coe v1) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe v2))
-- Algebra.Lattice.Bundles.DistributiveLattice._.refl
d_refl_618 :: T_DistributiveLattice_582 -> AgdaAny -> AgdaAny
d_refl_618 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe d_isDistributiveLattice_606 (coe v0))))
-- Algebra.Lattice.Bundles.DistributiveLattice._.reflexive
d_reflexive_620 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_620 ~v0 ~v1 v2 = du_reflexive_620 v2
du_reflexive_620 ::
  T_DistributiveLattice_582 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_620 v0
  = let v1 = d_isDistributiveLattice_606 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
              (coe v1) in
    \ v3 v4 v5 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
           (coe v2))
        v3
-- Algebra.Lattice.Bundles.DistributiveLattice._.sym
d_sym_622 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_622 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe d_isDistributiveLattice_606 (coe v0))))
-- Algebra.Lattice.Bundles.DistributiveLattice._.trans
d_trans_624 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_624 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe d_isDistributiveLattice_606 (coe v0))))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_626 ~v0 ~v1 v2
  = du_'8743''45'absorbs'45''8744'_626 v2
du_'8743''45'absorbs'45''8744'_626 ::
  T_DistributiveLattice_582 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_626 v0
  = let v1 = d_isDistributiveLattice_606 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∧-assoc
d_'8743''45'assoc_628 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'assoc_628 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_606 (coe v0)))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∧-comm
d_'8743''45'comm_630 ::
  T_DistributiveLattice_582 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'comm_630 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_606 (coe v0)))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∧-cong
d_'8743''45'cong_632 ::
  T_DistributiveLattice_582 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong_632 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_606 (coe v0)))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∧-congʳ
d_'8743''45'cong'691'_634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'691'_634 ~v0 ~v1 v2
  = du_'8743''45'cong'691'_634 v2
du_'8743''45'cong'691'_634 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'691'_634 v0
  = let v1 = d_isDistributiveLattice_606 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∧-congˡ
d_'8743''45'cong'737'_636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'737'_636 ~v0 ~v1 v2
  = du_'8743''45'cong'737'_636 v2
du_'8743''45'cong'737'_636 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'737'_636 v0
  = let v1 = d_isDistributiveLattice_606 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∧-distrib-∨
d_'8743''45'distrib'45''8744'_638 ::
  T_DistributiveLattice_582 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8744'_638 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'distrib'45''8744'_2834
      (coe d_isDistributiveLattice_606 (coe v0))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∧-distribʳ-∨
d_'8743''45'distrib'691''45''8744'_640 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'691''45''8744'_640 ~v0 ~v1 v2
  = du_'8743''45'distrib'691''45''8744'_640 v2
du_'8743''45'distrib'691''45''8744'_640 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'691''45''8744'_640 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'691''45''8744'_2882
      (coe d_isDistributiveLattice_606 (coe v0))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∧-distribˡ-∨
d_'8743''45'distrib'737''45''8744'_642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8744'_642 ~v0 ~v1 v2
  = du_'8743''45'distrib'737''45''8744'_642 v2
du_'8743''45'distrib'737''45''8744'_642 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'737''45''8744'_642 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'737''45''8744'_2880
      (coe d_isDistributiveLattice_606 (coe v0))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_644 ~v0 ~v1 v2
  = du_'8744''45'absorbs'45''8743'_644 v2
du_'8744''45'absorbs'45''8743'_644 ::
  T_DistributiveLattice_582 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_644 v0
  = let v1 = d_isDistributiveLattice_606 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∨-assoc
d_'8744''45'assoc_646 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'assoc_646 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_606 (coe v0)))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∨-comm
d_'8744''45'comm_648 ::
  T_DistributiveLattice_582 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'comm_648 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_606 (coe v0)))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∨-cong
d_'8744''45'cong_650 ::
  T_DistributiveLattice_582 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong_650 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_606 (coe v0)))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∨-congʳ
d_'8744''45'cong'691'_652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'691'_652 ~v0 ~v1 v2
  = du_'8744''45'cong'691'_652 v2
du_'8744''45'cong'691'_652 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'691'_652 v0
  = let v1 = d_isDistributiveLattice_606 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∨-congˡ
d_'8744''45'cong'737'_654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'737'_654 ~v0 ~v1 v2
  = du_'8744''45'cong'737'_654 v2
du_'8744''45'cong'737'_654 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'737'_654 v0
  = let v1 = d_isDistributiveLattice_606 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v1))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∨-distrib-∧
d_'8744''45'distrib'45''8743'_656 ::
  T_DistributiveLattice_582 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'distrib'45''8743'_656 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'distrib'45''8743'_2832
      (coe d_isDistributiveLattice_606 (coe v0))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∨-distribʳ-∧
d_'8744''45'distrib'691''45''8743'_658 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'691''45''8743'_658 ~v0 ~v1 v2
  = du_'8744''45'distrib'691''45''8743'_658 v2
du_'8744''45'distrib'691''45''8743'_658 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'691''45''8743'_658 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'691''45''8743'_2878
      (coe d_isDistributiveLattice_606 (coe v0))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∨-distribˡ-∧
d_'8744''45'distrib'737''45''8743'_660 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'737''45''8743'_660 ~v0 ~v1 v2
  = du_'8744''45'distrib'737''45''8743'_660 v2
du_'8744''45'distrib'737''45''8743'_660 ::
  T_DistributiveLattice_582 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'737''45''8743'_660 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'737''45''8743'_2876
      (coe d_isDistributiveLattice_606 (coe v0))
-- Algebra.Lattice.Bundles.DistributiveLattice.lattice
d_lattice_662 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 -> T_Lattice_498
d_lattice_662 ~v0 ~v1 v2 = du_lattice_662 v2
du_lattice_662 :: T_DistributiveLattice_582 -> T_Lattice_498
du_lattice_662 v0
  = coe
      C_Lattice'46'constructor_7911 (d__'8744'__602 (coe v0))
      (d__'8743'__604 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe d_isDistributiveLattice_606 (coe v0)))
-- Algebra.Lattice.Bundles.DistributiveLattice._._≉_
d__'8777'__666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 -> AgdaAny -> AgdaAny -> ()
d__'8777'__666 = erased
-- Algebra.Lattice.Bundles.DistributiveLattice._.rawLattice
d_rawLattice_668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12
d_rawLattice_668 ~v0 ~v1 v2 = du_rawLattice_668 v2
du_rawLattice_668 ::
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12
du_rawLattice_668 v0
  = coe du_rawLattice_564 (coe du_lattice_662 (coe v0))
-- Algebra.Lattice.Bundles.DistributiveLattice._.setoid
d_setoid_670 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_670 ~v0 ~v1 v2 = du_setoid_670 v2
du_setoid_670 ::
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_670 v0 = coe du_setoid_572 (coe du_lattice_662 (coe v0))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∧-rawMagma
d_'8743''45'rawMagma_672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8743''45'rawMagma_672 ~v0 ~v1 v2 = du_'8743''45'rawMagma_672 v2
du_'8743''45'rawMagma_672 ::
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'8743''45'rawMagma_672 v0
  = let v1 = coe du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
      (coe du_rawLattice_564 (coe v1))
-- Algebra.Lattice.Bundles.DistributiveLattice._.∨-rawMagma
d_'8744''45'rawMagma_674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8744''45'rawMagma_674 ~v0 ~v1 v2 = du_'8744''45'rawMagma_674 v2
du_'8744''45'rawMagma_674 ::
  T_DistributiveLattice_582 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'8744''45'rawMagma_674 v0
  = let v1 = coe du_lattice_662 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
      (coe du_rawLattice_564 (coe v1))
-- Algebra.Lattice.Bundles.BooleanAlgebra
d_BooleanAlgebra_680 a0 a1 = ()
data T_BooleanAlgebra_680
  = C_BooleanAlgebra'46'constructor_11433 (AgdaAny ->
                                           AgdaAny -> AgdaAny)
                                          (AgdaAny -> AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                          AgdaAny AgdaAny
                                          MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894
-- Algebra.Lattice.Bundles.BooleanAlgebra.Carrier
d_Carrier_702 :: T_BooleanAlgebra_680 -> ()
d_Carrier_702 = erased
-- Algebra.Lattice.Bundles.BooleanAlgebra._≈_
d__'8776'__704 :: T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> ()
d__'8776'__704 = erased
-- Algebra.Lattice.Bundles.BooleanAlgebra._∨_
d__'8744'__706 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8744'__706 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_11433 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BooleanAlgebra._∧_
d__'8743'__708 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8743'__708 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_11433 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BooleanAlgebra.¬_
d_'172'__710 :: T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny
d_'172'__710 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_11433 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BooleanAlgebra.⊤
d_'8868'_712 :: T_BooleanAlgebra_680 -> AgdaAny
d_'8868'_712 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_11433 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BooleanAlgebra.⊥
d_'8869'_714 :: T_BooleanAlgebra_680 -> AgdaAny
d_'8869'_714 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_11433 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BooleanAlgebra.isBooleanAlgebra
d_isBooleanAlgebra_716 ::
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsBooleanAlgebra_2894
d_isBooleanAlgebra_716 v0
  = case coe v0 of
      C_BooleanAlgebra'46'constructor_11433 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Lattice.Bundles.BooleanAlgebra._.absorptive
d_absorptive_720 ::
  T_BooleanAlgebra_680 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_absorptive_720 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_absorptive_2780
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
            (coe d_isBooleanAlgebra_716 (coe v0))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.isDistributiveLattice
d_isDistributiveLattice_722 ::
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsDistributiveLattice_2818
d_isDistributiveLattice_722 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
      (coe d_isBooleanAlgebra_716 (coe v0))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.isEquivalence
d_isEquivalence_724 ::
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_724 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
            (coe d_isBooleanAlgebra_716 (coe v0))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.isLattice
d_isLattice_726 ::
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Structures.T_IsLattice_2744
d_isLattice_726 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe d_isBooleanAlgebra_716 (coe v0)))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.isPartialEquivalence
d_isPartialEquivalence_728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_728 ~v0 ~v1 v2
  = du_isPartialEquivalence_728 v2
du_isPartialEquivalence_728 ::
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_728 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
              (coe v2) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe v3))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.refl
d_refl_730 :: T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny
d_refl_730 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe d_isBooleanAlgebra_716 (coe v0)))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.reflexive
d_reflexive_732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_732 ~v0 ~v1 v2 = du_reflexive_732 v2
du_reflexive_732 ::
  T_BooleanAlgebra_680 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_732 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
              (coe v1) in
    let v3
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
              (coe v2) in
    \ v4 v5 v6 ->
      coe
        MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
        (coe
           MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
           (coe v3))
        v4
-- Algebra.Lattice.Bundles.BooleanAlgebra._.sym
d_sym_734 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_734 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe d_isBooleanAlgebra_716 (coe v0)))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.trans
d_trans_736 ::
  T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_736 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isEquivalence_2766
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
            (coe
               MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
               (coe d_isBooleanAlgebra_716 (coe v0)))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.¬-cong
d_'172''45'cong_738 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'172''45'cong_738 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'172''45'cong_2920
      (coe d_isBooleanAlgebra_716 (coe v0))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-absorbs-∨
d_'8743''45'absorbs'45''8744'_740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'absorbs'45''8744'_740 ~v0 ~v1 v2
  = du_'8743''45'absorbs'45''8744'_740 v2
du_'8743''45'absorbs'45''8744'_740 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'absorbs'45''8744'_740 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'absorbs'45''8744'_2796
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v2))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-assoc
d_'8743''45'assoc_742 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'assoc_742 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'assoc_2776
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
            (coe d_isBooleanAlgebra_716 (coe v0))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-comm
d_'8743''45'comm_744 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'comm_744 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'comm_2774
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
            (coe d_isBooleanAlgebra_716 (coe v0))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-complement
d_'8743''45'complement_746 ::
  T_BooleanAlgebra_680 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'complement_746 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'complement_2918
      (coe d_isBooleanAlgebra_716 (coe v0))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-complementʳ
d_'8743''45'complement'691'_748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny
d_'8743''45'complement'691'_748 ~v0 ~v1 v2
  = du_'8743''45'complement'691'_748 v2
du_'8743''45'complement'691'_748 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny
du_'8743''45'complement'691'_748 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'691'_2982
      (coe d_isBooleanAlgebra_716 (coe v0))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-complementˡ
d_'8743''45'complement'737'_750 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny
d_'8743''45'complement'737'_750 ~v0 ~v1 v2
  = du_'8743''45'complement'737'_750 v2
du_'8743''45'complement'737'_750 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny
du_'8743''45'complement'737'_750 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'complement'737'_2980
      (coe d_isBooleanAlgebra_716 (coe v0))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-cong
d_'8743''45'cong_752 ::
  T_BooleanAlgebra_680 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong_752 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'cong_2778
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
            (coe d_isBooleanAlgebra_716 (coe v0))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-congʳ
d_'8743''45'cong'691'_754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'691'_754 ~v0 ~v1 v2
  = du_'8743''45'cong'691'_754 v2
du_'8743''45'cong'691'_754 ::
  T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'691'_754 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'691'_2802
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v2))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-congˡ
d_'8743''45'cong'737'_756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'cong'737'_756 ~v0 ~v1 v2
  = du_'8743''45'cong'737'_756 v2
du_'8743''45'cong'737'_756 ::
  T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'cong'737'_756 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'cong'737'_2798
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v2))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-distrib-∨
d_'8743''45'distrib'45''8744'_758 ::
  T_BooleanAlgebra_680 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8743''45'distrib'45''8744'_758 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8743''45'distrib'45''8744'_2834
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe d_isBooleanAlgebra_716 (coe v0)))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-distribʳ-∨
d_'8743''45'distrib'691''45''8744'_760 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'691''45''8744'_760 ~v0 ~v1 v2
  = du_'8743''45'distrib'691''45''8744'_760 v2
du_'8743''45'distrib'691''45''8744'_760 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'691''45''8744'_760 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'691''45''8744'_2882
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe v1))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-distribˡ-∨
d_'8743''45'distrib'737''45''8744'_762 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8743''45'distrib'737''45''8744'_762 ~v0 ~v1 v2
  = du_'8743''45'distrib'737''45''8744'_762 v2
du_'8743''45'distrib'737''45''8744'_762 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8743''45'distrib'737''45''8744'_762 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8743''45'distrib'737''45''8744'_2880
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe v1))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-absorbs-∧
d_'8744''45'absorbs'45''8743'_764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'absorbs'45''8743'_764 ~v0 ~v1 v2
  = du_'8744''45'absorbs'45''8743'_764 v2
du_'8744''45'absorbs'45''8743'_764 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'absorbs'45''8743'_764 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'absorbs'45''8743'_2794
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v2))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-assoc
d_'8744''45'assoc_766 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'assoc_766 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'assoc_2770
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
            (coe d_isBooleanAlgebra_716 (coe v0))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-comm
d_'8744''45'comm_768 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'comm_768 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'comm_2768
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
            (coe d_isBooleanAlgebra_716 (coe v0))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-complement
d_'8744''45'complement_770 ::
  T_BooleanAlgebra_680 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'complement_770 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'complement_2916
      (coe d_isBooleanAlgebra_716 (coe v0))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-complementʳ
d_'8744''45'complement'691'_772 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny
d_'8744''45'complement'691'_772 ~v0 ~v1 v2
  = du_'8744''45'complement'691'_772 v2
du_'8744''45'complement'691'_772 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny
du_'8744''45'complement'691'_772 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'691'_2978
      (coe d_isBooleanAlgebra_716 (coe v0))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-complementˡ
d_'8744''45'complement'737'_774 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny
d_'8744''45'complement'737'_774 ~v0 ~v1 v2
  = du_'8744''45'complement'737'_774 v2
du_'8744''45'complement'737'_774 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny
du_'8744''45'complement'737'_774 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'complement'737'_2976
      (coe d_isBooleanAlgebra_716 (coe v0))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-cong
d_'8744''45'cong_776 ::
  T_BooleanAlgebra_680 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong_776 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'cong_2772
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830
         (coe
            MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
            (coe d_isBooleanAlgebra_716 (coe v0))))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-congʳ
d_'8744''45'cong'691'_778 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'691'_778 ~v0 ~v1 v2
  = du_'8744''45'cong'691'_778 v2
du_'8744''45'cong'691'_778 ::
  T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'691'_778 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'691'_2810
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v2))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-congˡ
d_'8744''45'cong'737'_780 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'cong'737'_780 ~v0 ~v1 v2
  = du_'8744''45'cong'737'_780 v2
du_'8744''45'cong'737'_780 ::
  T_BooleanAlgebra_680 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'cong'737'_780 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    let v2
          = MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
              (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'cong'737'_2806
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isLattice_2830 (coe v2))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-distrib-∧
d_'8744''45'distrib'45''8743'_782 ::
  T_BooleanAlgebra_680 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8744''45'distrib'45''8743'_782 v0
  = coe
      MAlonzo.Code.Algebra.Lattice.Structures.d_'8744''45'distrib'45''8743'_2832
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe d_isBooleanAlgebra_716 (coe v0)))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-distribʳ-∧
d_'8744''45'distrib'691''45''8743'_784 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'691''45''8743'_784 ~v0 ~v1 v2
  = du_'8744''45'distrib'691''45''8743'_784 v2
du_'8744''45'distrib'691''45''8743'_784 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'691''45''8743'_784 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'691''45''8743'_2878
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe v1))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-distribˡ-∧
d_'8744''45'distrib'737''45''8743'_786 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8744''45'distrib'737''45''8743'_786 ~v0 ~v1 v2
  = du_'8744''45'distrib'737''45''8743'_786 v2
du_'8744''45'distrib'737''45''8743'_786 ::
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8744''45'distrib'737''45''8743'_786 v0
  = let v1 = d_isBooleanAlgebra_716 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Lattice.Structures.du_'8744''45'distrib'737''45''8743'_2876
      (coe
         MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe v1))
-- Algebra.Lattice.Bundles.BooleanAlgebra.distributiveLattice
d_distributiveLattice_788 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> T_DistributiveLattice_582
d_distributiveLattice_788 ~v0 ~v1 v2
  = du_distributiveLattice_788 v2
du_distributiveLattice_788 ::
  T_BooleanAlgebra_680 -> T_DistributiveLattice_582
du_distributiveLattice_788 v0
  = coe
      C_DistributiveLattice'46'constructor_9473 (d__'8744'__706 (coe v0))
      (d__'8743'__708 (coe v0))
      (MAlonzo.Code.Algebra.Lattice.Structures.d_isDistributiveLattice_2914
         (coe d_isBooleanAlgebra_716 (coe v0)))
-- Algebra.Lattice.Bundles.BooleanAlgebra._._≉_
d__'8777'__792 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> AgdaAny -> AgdaAny -> ()
d__'8777'__792 = erased
-- Algebra.Lattice.Bundles.BooleanAlgebra._.lattice
d_lattice_794 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 -> T_Lattice_498
d_lattice_794 ~v0 ~v1 v2 = du_lattice_794 v2
du_lattice_794 :: T_BooleanAlgebra_680 -> T_Lattice_498
du_lattice_794 v0
  = coe du_lattice_662 (coe du_distributiveLattice_788 (coe v0))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.rawLattice
d_rawLattice_796 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12
d_rawLattice_796 ~v0 ~v1 v2 = du_rawLattice_796 v2
du_rawLattice_796 ::
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Lattice.Bundles.Raw.T_RawLattice_12
du_rawLattice_796 v0
  = let v1 = coe du_distributiveLattice_788 (coe v0) in
    coe du_rawLattice_564 (coe du_lattice_662 (coe v1))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.setoid
d_setoid_798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_798 ~v0 ~v1 v2 = du_setoid_798 v2
du_setoid_798 ::
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_798 v0
  = let v1 = coe du_distributiveLattice_788 (coe v0) in
    coe du_setoid_572 (coe du_lattice_662 (coe v1))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∧-rawMagma
d_'8743''45'rawMagma_800 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8743''45'rawMagma_800 ~v0 ~v1 v2 = du_'8743''45'rawMagma_800 v2
du_'8743''45'rawMagma_800 ::
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'8743''45'rawMagma_800 v0
  = let v1 = coe du_distributiveLattice_788 (coe v0) in
    let v2 = coe du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8743''45'rawMagma_36
      (coe du_rawLattice_564 (coe v2))
-- Algebra.Lattice.Bundles.BooleanAlgebra._.∨-rawMagma
d_'8744''45'rawMagma_802 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'8744''45'rawMagma_802 ~v0 ~v1 v2 = du_'8744''45'rawMagma_802 v2
du_'8744''45'rawMagma_802 ::
  T_BooleanAlgebra_680 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'8744''45'rawMagma_802 v0
  = let v1 = coe du_distributiveLattice_788 (coe v0) in
    let v2 = coe du_lattice_662 (coe v1) in
    coe
      MAlonzo.Code.Algebra.Lattice.Bundles.Raw.du_'8744''45'rawMagma_34
      (coe du_rawLattice_564 (coe v2))
