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

module MAlonzo.Code.Algebra.Properties.CommutativeMonoid.Mult where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Definitions.RawMonoid
import qualified MAlonzo.Code.Algebra.Properties.CommutativeSemigroup
import qualified MAlonzo.Code.Algebra.Properties.Monoid.Mult
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Properties.CommutativeMonoid.Mult._._×_
d__'215'__176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny
d__'215'__176 ~v0 ~v1 v2 = du__'215'__176 v2
du__'215'__176 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny
du__'215'__176 v0
  = let v1
          = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
      (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v1))
-- Algebra.Properties.CommutativeMonoid.Mult._.×-assocˡ
d_'215''45'assoc'737'_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
d_'215''45'assoc'737'_178 ~v0 ~v1 v2
  = du_'215''45'assoc'737'_178 v2
du_'215''45'assoc'737'_178 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
du_'215''45'assoc'737'_178 v0
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.du_'215''45'assoc'737'_286
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0))
-- Algebra.Properties.CommutativeMonoid.Mult._.×-cong
d_'215''45'cong_180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer ->
  Integer ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  AgdaAny -> AgdaAny
d_'215''45'cong_180 ~v0 ~v1 v2 = du_'215''45'cong_180 v2
du_'215''45'cong_180 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer ->
  Integer ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  AgdaAny -> AgdaAny
du_'215''45'cong_180 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.du_'215''45'cong_238
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) v1 v3 v4
      v6
-- Algebra.Properties.CommutativeMonoid.Mult._.×-congʳ
d_'215''45'cong'691'_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'215''45'cong'691'_182 ~v0 ~v1 v2 = du_'215''45'cong'691'_182 v2
du_'215''45'cong'691'_182 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'215''45'cong'691'_182 v0
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.du_'215''45'cong'691'_230
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0))
-- Algebra.Properties.CommutativeMonoid.Mult._.×-homo-+
d_'215''45'homo'45''43'_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
d_'215''45'homo'45''43'_184 ~v0 ~v1 v2
  = du_'215''45'homo'45''43'_184 v2
du_'215''45'homo'45''43'_184 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
du_'215''45'homo'45''43'_184 v0
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.du_'215''45'homo'45''43'_250
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0))
-- Algebra.Properties.CommutativeMonoid.Mult._.×-idem
d_'215''45'idem_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny ->
  AgdaAny ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> AgdaAny
d_'215''45'idem_186 ~v0 ~v1 v2 = du_'215''45'idem_186 v2
du_'215''45'idem_186 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny ->
  AgdaAny ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> AgdaAny
du_'215''45'idem_186 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.du_'215''45'idem_268
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) v1 v2 v3
-- Algebra.Properties.CommutativeMonoid.Mult.×-distrib-+
d_'215''45'distrib'45''43'_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> AgdaAny -> Integer -> AgdaAny
d_'215''45'distrib'45''43'_194 ~v0 ~v1 v2 v3 v4 v5
  = du_'215''45'distrib'45''43'_194 v2 v3 v4 v5
du_'215''45'distrib'45''43'_194 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> AgdaAny -> Integer -> AgdaAny
du_'215''45'distrib'45''43'_194 v0 v1 v2 v3
  = case coe v3 of
      0 -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_sym_36
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                         (coe
                            MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                            (coe v0))))))
             (coe
                MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                (MAlonzo.Code.Algebra.Bundles.d_ε_842 (coe v0))
                (MAlonzo.Code.Algebra.Bundles.d_ε_842 (coe v0)))
             (MAlonzo.Code.Algebra.Bundles.d_ε_842 (coe v0))
             (let v4
                    = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                        (coe v0) in
              coe
                MAlonzo.Code.Algebra.Structures.du_identity'737'_640
                (MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4))
                (MAlonzo.Code.Algebra.Bundles.d_ε_842 (coe v0)))
      _ -> let v4 = subInt (coe v3) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v5
                       = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                           (coe v0) in
                 let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
                 let v7
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                (coe
                   MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                   (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1 v2)
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                      (coe
                         MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                         (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                      (coe v4)
                      (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1 v2)))
                (coe
                   MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                   (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1 v2)
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                      (coe
                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                            (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                         (coe v4) (coe v1))
                      (coe
                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                            (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                         (coe v4) (coe v2))))
                (coe
                   MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                      (coe
                         MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                         (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                      (coe v3) (coe v1))
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                      (coe
                         MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                         (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                      (coe v3) (coe v2)))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   (let v5
                          = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                              (coe v0) in
                    let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
                    let v7
                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                    coe
                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                      (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1 v2)
                      (coe
                         MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                         (coe
                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                               (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                            (coe v4) (coe v1))
                         (coe
                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                               (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                            (coe v4) (coe v2))))
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1
                      (coe
                         MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v2
                         (coe
                            MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v4) (coe v1))
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v4) (coe v2)))))
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                      (coe
                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                            (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                         (coe v3) (coe v1))
                      (coe
                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                            (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                         (coe v3) (coe v2)))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                      (let v5
                             = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                                 (coe v0) in
                       let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
                       let v7
                             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                       coe
                         MAlonzo.Code.Algebra.Structures.du_setoid_164
                         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                      (coe
                         MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1
                         (coe
                            MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v2
                            (coe
                               MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v4) (coe v1))
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v4) (coe v2)))))
                      (coe
                         MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1
                         (coe
                            MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v4) (coe v1))
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v3) (coe v2))))
                      (coe
                         MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                         (coe
                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                               (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                            (coe v3) (coe v1))
                         (coe
                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                               (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                            (coe v3) (coe v2)))
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                         (let v5
                                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                                    (coe v0) in
                          let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
                          let v7
                                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                          coe
                            MAlonzo.Code.Algebra.Structures.du_setoid_164
                            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))
                         (coe
                            MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1
                            (coe
                               MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v4) (coe v1))
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v3) (coe v2))))
                         (coe
                            MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v3) (coe v1))
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v3) (coe v2)))
                         (coe
                            MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v3) (coe v1))
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v3) (coe v2)))
                         (coe
                            MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                            (coe
                               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                               (coe
                                  MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                  (let v5
                                         = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                                             (coe v0) in
                                   let v6
                                         = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                             (coe v5) in
                                   let v7
                                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                             (coe v6) in
                                   coe
                                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7)))))
                            (coe
                               MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v3) (coe v1))
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v3) (coe v2))))
                         (coe
                            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                            (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                  (coe
                                     MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                     (coe
                                        MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                        (coe
                                           MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                                           (coe v0))))))
                            (coe
                               MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1
                                  (coe
                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                     (coe
                                        MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                        (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                     (coe v4) (coe v1)))
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v3) (coe v2)))
                            (coe
                               MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                                  (coe
                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                     (coe
                                        MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                        (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                     (coe v4) (coe v1))
                                  (coe
                                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                     (coe
                                        MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                        (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                     (coe v3) (coe v2))))
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_assoc_446
                               (MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                  (coe
                                     MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                     (coe
                                        MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                                        (coe v0))))
                               v1
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v4) (coe v1))
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v3) (coe v2)))))
                      (let v5
                             = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                                 (coe v0) in
                       let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
                       let v7
                             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                       coe
                         MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
                         (coe v1)
                         (coe
                            MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v2
                            (coe
                               MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v4) (coe v1))
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v4) (coe v2))))
                         (coe
                            MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v4) (coe v1))
                            (coe
                               MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v2
                               (coe
                                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                                  (coe v4) (coe v2))))
                         (coe
                            MAlonzo.Code.Algebra.Properties.CommutativeSemigroup.du_x'8729'yz'8776'y'8729'xz_240
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_commutativeSemigroup_906 (coe v0))
                            (coe v2)
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v4) (coe v1))
                            (coe
                               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                  (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                               (coe v4) (coe v2)))))
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_assoc_446
                      (MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                            (coe
                               MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844 (coe v0))))
                      v1 v2
                      (coe
                         MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                         (coe
                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                               (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                            (coe v4) (coe v1))
                         (coe
                            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                               (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)))
                            (coe v4) (coe v2)))))
                (let v5
                       = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                           (coe v0) in
                 let v6 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v5) in
                 let v7
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v6) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v7))
                   (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1 v2)
                   (let v8
                          = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                    coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                      (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v8))
                      (coe v4)
                      (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1 v2))
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                      (let v8
                             = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                       coe
                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                         (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v8))
                         (coe v4) (coe v1))
                      (let v8
                             = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                       coe
                         MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                         (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v8))
                         (coe v4) (coe v2)))
                   (coe
                      du_'215''45'distrib'45''43'_194 (coe v0) (coe v1) (coe v2)
                      (coe v4))))
