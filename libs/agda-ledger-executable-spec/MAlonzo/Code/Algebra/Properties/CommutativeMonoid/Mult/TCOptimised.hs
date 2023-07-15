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

module MAlonzo.Code.Algebra.Properties.CommutativeMonoid.Mult.TCOptimised where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Definitions.RawMonoid
import qualified MAlonzo.Code.Algebra.Properties.CommutativeMonoid.Mult
import qualified MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Properties.CommutativeMonoid.Mult.TCOptimised._._×′_
d__'215''8242'__130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny
d__'215''8242'__130 ~v0 ~v1 v2 = du__'215''8242'__130 v2
du__'215''8242'__130 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny
du__'215''8242'__130 v0
  = let v1
          = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
    let v2
          = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v1) in
    coe
      (\ v3 v4 ->
         let v5 = subInt (coe v3) (coe (1 :: Integer)) in
         let v6
               = coe
                   MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v2
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                      (coe v2) (coe v5) (coe v4))
                   v4 in
         case coe v3 of
           0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v2)
           1 -> coe v4
           _ -> coe v6)
-- Algebra.Properties.CommutativeMonoid.Mult.TCOptimised._.1+×
d_1'43''215'_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny
d_1'43''215'_132 ~v0 ~v1 v2 = du_1'43''215'_132 v2
du_1'43''215'_132 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny
du_1'43''215'_132 v0
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_1'43''215'_120
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0))
-- Algebra.Properties.CommutativeMonoid.Mult.TCOptimised._.×-assocˡ
d_'215''45'assoc'737'_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
d_'215''45'assoc'737'_134 ~v0 ~v1 v2
  = du_'215''45'assoc'737'_134 v2
du_'215''45'assoc'737'_134 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
du_'215''45'assoc'737'_134 v0
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''45'assoc'737'_182
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0))
-- Algebra.Properties.CommutativeMonoid.Mult.TCOptimised._.×-cong
d_'215''45'cong_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer ->
  Integer ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  AgdaAny -> AgdaAny
d_'215''45'cong_136 ~v0 ~v1 v2 = du_'215''45'cong_136 v2
du_'215''45'cong_136 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer ->
  Integer ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  AgdaAny -> AgdaAny
du_'215''45'cong_136 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''45'cong_170
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) v1 v3 v4
      v6
-- Algebra.Properties.CommutativeMonoid.Mult.TCOptimised._.×-congʳ
d_'215''45'cong'691'_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'215''45'cong'691'_138 ~v0 ~v1 v2 = du_'215''45'cong'691'_138 v2
du_'215''45'cong'691'_138 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'215''45'cong'691'_138 v0
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''45'cong'691'_160
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0))
-- Algebra.Properties.CommutativeMonoid.Mult.TCOptimised._.×-homo-+
d_'215''45'homo'45''43'_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
d_'215''45'homo'45''43'_140 ~v0 ~v1 v2
  = du_'215''45'homo'45''43'_140 v2
du_'215''45'homo'45''43'_140 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
du_'215''45'homo'45''43'_140 v0
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''45'homo'45''43'_148
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0))
-- Algebra.Properties.CommutativeMonoid.Mult.TCOptimised._.×ᵤ≈×
d_'215''7524''8776''215'_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny
d_'215''7524''8776''215'_142 ~v0 ~v1 v2
  = du_'215''7524''8776''215'_142 v2
du_'215''7524''8776''215'_142 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  Integer -> AgdaAny -> AgdaAny
du_'215''7524''8776''215'_142 v0
  = coe
      MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''7524''8776''215'_134
      (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0))
-- Algebra.Properties.CommutativeMonoid.Mult.TCOptimised.×-distrib-+
d_'215''45'distrib'45''43'_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> AgdaAny -> Integer -> AgdaAny
d_'215''45'distrib'45''43'_150 ~v0 ~v1 v2 v3 v4 v5
  = du_'215''45'distrib'45''43'_150 v2 v3 v4 v5
du_'215''45'distrib'45''43'_150 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820 ->
  AgdaAny -> AgdaAny -> Integer -> AgdaAny
du_'215''45'distrib'45''43'_150 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v4
                = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                    (coe v0) in
          let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
          let v6
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
         (let v4
                = coe
                    MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                    (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
          let v5
                = coe MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1 v2 in
          let v6 = subInt (coe v3) (coe (1 :: Integer)) in
          let v7
                = coe
                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                    (coe
                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                       (coe v4) (coe v6) (coe v5))
                    v5 in
          case coe v3 of
            0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
            1 -> coe v5
            _ -> coe v7)
         (let v4
                = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
          coe
            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
            (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v4))
            (coe v3)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1 v2))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                       (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
             let v5 = subInt (coe v3) (coe (1 :: Integer)) in
             let v6
                   = coe
                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                       (coe
                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                          (coe v4) (coe v5) (coe v1))
                       v1 in
             case coe v3 of
               0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
               1 -> coe v1
               _ -> coe v6)
            (let v4
                   = coe
                       MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                       (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
             let v5 = subInt (coe v3) (coe (1 :: Integer)) in
             let v6
                   = coe
                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                       (coe
                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                          (coe v4) (coe v5) (coe v2))
                       v2 in
             case coe v3 of
               0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
               1 -> coe v2
               _ -> coe v6))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4
                   = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                       (coe v0) in
             let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
             let v6
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
            (let v4
                   = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
             coe
               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
               (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v4))
               (coe v3)
               (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1 v2))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
               (let v4
                      = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                  (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v4))
                  (coe v3) (coe v1))
               (let v4
                      = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                coe
                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                  (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v4))
                  (coe v3) (coe v2)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                          (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
                let v5 = subInt (coe v3) (coe (1 :: Integer)) in
                let v6
                      = coe
                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                          (coe
                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                             (coe v4) (coe v5) (coe v1))
                          v1 in
                case coe v3 of
                  0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                  1 -> coe v1
                  _ -> coe v6)
               (let v4
                      = coe
                          MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                          (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
                let v5 = subInt (coe v3) (coe (1 :: Integer)) in
                let v6
                      = coe
                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                          (coe
                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                             (coe v4) (coe v5) (coe v2))
                          v2 in
                case coe v3 of
                  0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                  1 -> coe v2
                  _ -> coe v6))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4
                      = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                          (coe v0) in
                let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
                let v6
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                     (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v4))
                     (coe v3) (coe v1))
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                     (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v4))
                     (coe v3) (coe v2)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                  (let v4
                         = coe
                             MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                             (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
                   let v5 = subInt (coe v3) (coe (1 :: Integer)) in
                   let v6
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v4) (coe v5) (coe v1))
                             v1 in
                   case coe v3 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                     1 -> coe v1
                     _ -> coe v6)
                  (let v4
                         = coe
                             MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                             (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
                   let v5 = subInt (coe v3) (coe (1 :: Integer)) in
                   let v6
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v4) (coe v5) (coe v2))
                             v2 in
                   case coe v3 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                     1 -> coe v2
                     _ -> coe v6))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                  (let v4
                         = coe
                             MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                             (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
                   let v5 = subInt (coe v3) (coe (1 :: Integer)) in
                   let v6
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v4) (coe v5) (coe v1))
                             v1 in
                   case coe v3 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                     1 -> coe v1
                     _ -> coe v6)
                  (let v4
                         = coe
                             MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                             (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
                   let v5 = subInt (coe v3) (coe (1 :: Integer)) in
                   let v6
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v4) (coe v5) (coe v2))
                             v2 in
                   case coe v3 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                     1 -> coe v2
                     _ -> coe v6))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v4
                               = MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                                   (coe v0) in
                         let v5 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v4) in
                         let v6
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v5) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v6)))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0
                     (let v4
                            = coe
                                MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
                      let v5 = subInt (coe v3) (coe (1 :: Integer)) in
                      let v6
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                                (coe
                                   MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                   (coe v4) (coe v5) (coe v1))
                                v1 in
                      case coe v3 of
                        0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                        1 -> coe v1
                        _ -> coe v6)
                     (let v4
                            = coe
                                MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812
                                (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) in
                      let v5 = subInt (coe v3) (coe (1 :: Integer)) in
                      let v6
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                                (coe
                                   MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                   (coe v4) (coe v5) (coe v2))
                                v2 in
                      case coe v3 of
                        0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                        1 -> coe v2
                        _ -> coe v6)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                  (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                           (coe
                              MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844 (coe v0)))))
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                     (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v4))
                     (coe v3) (coe v1))
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                   let v5
                         = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v4) in
                   let v6 = subInt (coe v3) (coe (1 :: Integer)) in
                   let v7
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v5) (coe v6) (coe v1))
                             v1 in
                   case coe v3 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                     1 -> coe v1
                     _ -> coe v7)
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                   coe
                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                     (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v4))
                     (coe v3) (coe v2))
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0) in
                   let v5
                         = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v4) in
                   let v6 = subInt (coe v3) (coe (1 :: Integer)) in
                   let v7
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v5) (coe v6) (coe v2))
                             v2 in
                   case coe v3 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                     1 -> coe v2
                     _ -> coe v7)
                  (coe
                     MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''7524''8776''215'_134
                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) (coe v3)
                     (coe v1))
                  (coe
                     MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''7524''8776''215'_134
                     (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) (coe v3)
                     (coe v2))))
            (coe
               MAlonzo.Code.Algebra.Properties.CommutativeMonoid.Mult.du_'215''45'distrib'45''43'_194
               (coe v0) (coe v1) (coe v2) (coe v3)))
         (coe
            MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised.du_'215''7524''8776''215'_134
            (coe MAlonzo.Code.Algebra.Bundles.du_monoid_890 (coe v0)) (coe v3)
            (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__840 v0 v1 v2)))
