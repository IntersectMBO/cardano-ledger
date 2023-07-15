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

module MAlonzo.Code.Algebra.Properties.Monoid.Mult.TCOptimised where

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
import qualified MAlonzo.Code.Algebra.Properties.Monoid.Mult
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Properties.Monoid.Mult.TCOptimised.U._×_
d__'215'__76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny
d__'215'__76 ~v0 ~v1 v2 = du__'215'__76 v2
du__'215'__76 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny
du__'215'__76 v0
  = coe
      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
      (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
-- Algebra.Properties.Monoid.Mult.TCOptimised._._×′_
d__'215''8242'__114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny
d__'215''8242'__114 ~v0 ~v1 v2 = du__'215''8242'__114 v2
du__'215''8242'__114 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny
du__'215''8242'__114 v0
  = let v1
          = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
    coe
      (\ v2 v3 ->
         let v4 = subInt (coe v2) (coe (1 :: Integer)) in
         let v5
               = coe
                   MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v1
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                      (coe v1) (coe v4) (coe v3))
                   v3 in
         case coe v2 of
           0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v1)
           1 -> coe v3
           _ -> coe v5)
-- Algebra.Properties.Monoid.Mult.TCOptimised.1+×
d_1'43''215'_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny
d_1'43''215'_120 ~v0 ~v1 v2 v3 v4 = du_1'43''215'_120 v2 v3 v4
du_1'43''215'_120 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny
du_1'43''215'_120 v0 v1 v2
  = case coe v1 of
      0 -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_sym_36
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0)))))
             (coe
                MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                (MAlonzo.Code.Algebra.Bundles.d_ε_762 (coe v0)))
             v2
             (coe
                MAlonzo.Code.Algebra.Structures.du_identity'691'_642
                (MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0)) v2)
      1 -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0)))))
             (let v3
                    = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
              let v4 = 2 :: Integer in
              let v5 = 1 :: Integer in
              let v6
                    = coe
                        MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v3
                        (coe
                           MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                           (coe v3) (coe v5) (coe v2))
                        v2 in
              case coe v4 of
                0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v3)
                1 -> coe v2
                _ -> coe v6)
      _ -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v3 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                 let v4
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4)))
                (coe
                   MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                   (let v3
                          = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                    let v4 = subInt (coe v1) (coe (1 :: Integer)) in
                    let v5
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v3
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v3) (coe v4) (coe v2))
                              v2 in
                    case coe v1 of
                      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v3)
                      1 -> coe v2
                      _ -> coe v5)
                   v2)
                (coe
                   MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                      (let v3
                             = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                       let v4 = subInt (coe v1) (coe (2 :: Integer)) in
                       let v5
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v3
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v3) (coe v4) (coe v2))
                                 v2 in
                       case coe v1 of
                         1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v3)
                         2 -> coe v2
                         _ -> coe v5))
                   v2)
                (coe
                   MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                      (let v3
                             = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                       let v4 = subInt (coe v1) (coe (2 :: Integer)) in
                       let v5
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v3
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v3) (coe v4) (coe v2))
                                 v2 in
                       case coe v1 of
                         1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v3)
                         2 -> coe v2
                         _ -> coe v5)
                      v2))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   (let v3 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                    let v4
                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
                    coe
                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4)))
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                      (coe
                         MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                         (let v3
                                = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                          let v4 = subInt (coe v1) (coe (2 :: Integer)) in
                          let v5
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v3
                                    (coe
                                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                       (coe v3) (coe v4) (coe v2))
                                    v2 in
                          case coe v1 of
                            1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v3)
                            2 -> coe v2
                            _ -> coe v5))
                      v2)
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                      (coe
                         MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                         (let v3
                                = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                          let v4 = subInt (coe v1) (coe (2 :: Integer)) in
                          let v5
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v3
                                    (coe
                                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                       (coe v3) (coe v4) (coe v2))
                                    v2 in
                          case coe v1 of
                            1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v3)
                            2 -> coe v2
                            _ -> coe v5)
                         v2))
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                      (coe
                         MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                         (let v3
                                = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                          let v4 = subInt (coe v1) (coe (2 :: Integer)) in
                          let v5
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v3
                                    (coe
                                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                       (coe v3) (coe v4) (coe v2))
                                    v2 in
                          case coe v1 of
                            1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v3)
                            2 -> coe v2
                            _ -> coe v5)
                         v2))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                            (let v3 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                             let v4
                                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
                             coe
                               MAlonzo.Code.Algebra.Structures.du_setoid_164
                               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4)))))
                      (coe
                         MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                         (coe
                            MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                            (let v3
                                   = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                             let v4 = subInt (coe v1) (coe (2 :: Integer)) in
                             let v5
                                   = coe
                                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v3
                                       (coe
                                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                          (coe v3) (coe v4) (coe v2))
                                       v2 in
                             case coe v1 of
                               1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v3)
                               2 -> coe v2
                               _ -> coe v5)
                            v2)))
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_assoc_446
                      (MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                         (coe MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0)))
                      v2
                      (let v3
                             = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                       let v4 = subInt (coe v1) (coe (2 :: Integer)) in
                       let v5
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v3
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v3) (coe v4) (coe v2))
                                 v2 in
                       case coe v1 of
                         1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v3)
                         2 -> coe v2
                         _ -> coe v5)
                      v2))
                (let v3 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                 let v4
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v3) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v4))
                   (coe v2)
                   (let v5
                          = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                    let v6 = subInt (coe v1) (coe (1 :: Integer)) in
                    let v7
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v5) (coe v6) (coe v2))
                              v2 in
                    case coe v1 of
                      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                      1 -> coe v2
                      _ -> coe v7)
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                      (let v5
                             = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                       let v6 = subInt (coe v1) (coe (2 :: Integer)) in
                       let v7
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v5) (coe v6) (coe v2))
                                 v2 in
                       case coe v1 of
                         1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                         2 -> coe v2
                         _ -> coe v7))
                   (coe
                      du_1'43''215'_120 (coe v0)
                      (coe subInt (coe v1) (coe (1 :: Integer))) (coe v2))))
-- Algebra.Properties.Monoid.Mult.TCOptimised.×ᵤ≈×
d_'215''7524''8776''215'_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny
d_'215''7524''8776''215'_134 ~v0 ~v1 v2 v3 v4
  = du_'215''7524''8776''215'_134 v2 v3 v4
du_'215''7524''8776''215'_134 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny
du_'215''7524''8776''215'_134 v0 v1 v2
  = case coe v1 of
      0 -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0)))))
             (coe
                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                (coe (0 :: Integer)) (coe v2))
      _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                 let v5
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
                (coe
                   MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                      (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                      (coe v3) (coe v2)))
                (coe
                   MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                   (let v4
                          = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                    let v5 = subInt (coe v1) (coe (2 :: Integer)) in
                    let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v4) (coe v5) (coe v2))
                              v2 in
                    case coe v1 of
                      1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                      2 -> coe v2
                      _ -> coe v6))
                (let v4
                       = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                 let v5 = subInt (coe v1) (coe (1 :: Integer)) in
                 let v6
                       = coe
                           MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                           (coe
                              MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                              (coe v4) (coe v5) (coe v2))
                           v2 in
                 case coe v1 of
                   0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                   1 -> coe v2
                   _ -> coe v6)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
                   (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                    let v5
                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
                    coe
                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
                   (coe
                      MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0 v2
                      (let v4
                             = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                       let v5 = subInt (coe v1) (coe (2 :: Integer)) in
                       let v6
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v4) (coe v5) (coe v2))
                                 v2 in
                       case coe v1 of
                         1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                         2 -> coe v2
                         _ -> coe v6))
                   (let v4
                          = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                    let v5 = subInt (coe v1) (coe (1 :: Integer)) in
                    let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v4) (coe v5) (coe v2))
                              v2 in
                    case coe v1 of
                      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                      1 -> coe v2
                      _ -> coe v6)
                   (let v4
                          = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                    let v5 = subInt (coe v1) (coe (1 :: Integer)) in
                    let v6
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v4) (coe v5) (coe v2))
                              v2 in
                    case coe v1 of
                      0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                      1 -> coe v2
                      _ -> coe v6)
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                            (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                             let v5
                                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
                             coe
                               MAlonzo.Code.Algebra.Structures.du_setoid_164
                               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
                      (let v4
                             = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                       let v5 = subInt (coe v1) (coe (1 :: Integer)) in
                       let v6
                             = coe
                                 MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                                 (coe
                                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                    (coe v4) (coe v5) (coe v2))
                                 v2 in
                       case coe v1 of
                         0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                         1 -> coe v2
                         _ -> coe v6))
                   (coe du_1'43''215'_120 (coe v0) (coe v3) (coe v2)))
                (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                 let v5
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5))
                   (coe v2)
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                      (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                      (coe v3) (coe v2))
                   (let v6
                          = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                    let v7 = subInt (coe v1) (coe (2 :: Integer)) in
                    let v8
                          = coe
                              MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v6
                              (coe
                                 MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                 (coe v6) (coe v7) (coe v2))
                              v2 in
                    case coe v1 of
                      1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v6)
                      2 -> coe v2
                      _ -> coe v8)
                   (coe du_'215''7524''8776''215'_134 (coe v0) (coe v3) (coe v2))))
-- Algebra.Properties.Monoid.Mult.TCOptimised.×-homo-+
d_'215''45'homo'45''43'_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
d_'215''45'homo'45''43'_148 ~v0 ~v1 v2 v3 v4 v5
  = du_'215''45'homo'45''43'_148 v2 v3 v4 v5
du_'215''45'homo'45''43'_148 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
du_'215''45'homo'45''43'_148 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (let v4
                = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
          let v5 = addInt (coe v2) (coe v3) in
          let v6 = subInt (coe v5) (coe (1 :: Integer)) in
          let v7
                = coe
                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                    (coe
                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                       (coe v4) (coe v6) (coe v1))
                    v1 in
          case coe v5 of
            0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
            1 -> coe v1
            _ -> coe v7)
         (coe
            MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
            (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
            (coe addInt (coe v2) (coe v3)) (coe v1))
         (coe
            MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
            (let v4
                   = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
             let v5 = subInt (coe v2) (coe (1 :: Integer)) in
             let v6
                   = coe
                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                       (coe
                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                          (coe v4) (coe v5) (coe v1))
                       v1 in
             case coe v2 of
               0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
               1 -> coe v1
               _ -> coe v6)
            (let v4
                   = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
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
               _ -> coe v6))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (coe
               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
               (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
               (coe addInt (coe v2) (coe v3)) (coe v1))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
               (coe
                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                  (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                  (coe v2) (coe v1))
               (coe
                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                  (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                  (coe v3) (coe v1)))
            (coe
               MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
               (let v4
                      = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                let v5 = subInt (coe v2) (coe (1 :: Integer)) in
                let v6
                      = coe
                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                          (coe
                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                             (coe v4) (coe v5) (coe v1))
                          v1 in
                case coe v2 of
                  0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                  1 -> coe v1
                  _ -> coe v6)
               (let v4
                      = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
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
                  _ -> coe v6))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                  (coe
                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                     (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                     (coe v2) (coe v1))
                  (coe
                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                     (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                     (coe v3) (coe v1)))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                   let v5 = subInt (coe v2) (coe (1 :: Integer)) in
                   let v6
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v4) (coe v5) (coe v1))
                             v1 in
                   case coe v2 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                     1 -> coe v1
                     _ -> coe v6)
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
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
                     _ -> coe v6))
               (coe
                  MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                   let v5 = subInt (coe v2) (coe (1 :: Integer)) in
                   let v6
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v4) (coe v5) (coe v1))
                             v1 in
                   case coe v2 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                     1 -> coe v1
                     _ -> coe v6)
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
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
                     _ -> coe v6))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                         let v5
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d__'8729'__760 v0
                     (let v4
                            = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                      let v5 = subInt (coe v2) (coe (1 :: Integer)) in
                      let v6
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                                (coe
                                   MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                   (coe v4) (coe v5) (coe v1))
                                v1 in
                      case coe v2 of
                        0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                        1 -> coe v1
                        _ -> coe v6)
                     (let v4
                            = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
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
                        _ -> coe v6)))
               (coe
                  MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                  (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                        (coe MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0))))
                  (coe
                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                     (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                     (coe v2) (coe v1))
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                   let v5 = subInt (coe v2) (coe (1 :: Integer)) in
                   let v6
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v4) (coe v5) (coe v1))
                             v1 in
                   case coe v2 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                     1 -> coe v1
                     _ -> coe v6)
                  (coe
                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                     (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                     (coe v3) (coe v1))
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
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
                  (coe du_'215''7524''8776''215'_134 (coe v0) (coe v2) (coe v1))
                  (coe du_'215''7524''8776''215'_134 (coe v0) (coe v3) (coe v1))))
            (coe
               MAlonzo.Code.Algebra.Properties.Monoid.Mult.du_'215''45'homo'45''43'_250
               (coe v0) (coe v1) (coe v2) (coe v3)))
         (coe
            du_'215''7524''8776''215'_134 (coe v0)
            (coe addInt (coe v2) (coe v3)) (coe v1)))
-- Algebra.Properties.Monoid.Mult.TCOptimised.×-congʳ
d_'215''45'cong'691'_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'215''45'cong'691'_160 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_'215''45'cong'691'_160 v2 v3 v4 v5 v6
du_'215''45'cong'691'_160 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'215''45'cong'691'_160 v0 v1 v2 v3 v4
  = case coe v1 of
      0 -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0)))))
             (coe
                MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                (let v5
                       = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                 let v6 = 0 :: Integer in
                 \ v7 ->
                   let v8 = -1 :: Integer in
                   let v9
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v5) (coe v8) (coe v7))
                             v7 in
                   case coe v6 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                     1 -> coe v7
                     _ -> coe v9)
                (\ v5 v6 -> v5) v2 v3)
      1 -> coe v4
      _ -> coe
             MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
             (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                   (coe MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0))))
             (coe
                MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                (let v5
                       = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                 \ v6 ->
                   let v7 = subInt (coe v1) (coe (2 :: Integer)) in
                   let v8
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v5) (coe v7) (coe v6))
                             v6 in
                   case coe v1 of
                     1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                     2 -> coe v6
                     _ -> coe v8)
                (\ v5 v6 -> v5) v2 v3)
             (coe
                MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                (\ v5 v6 -> v6)
                (let v5
                       = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                 \ v6 ->
                   let v7 = subInt (coe v1) (coe (2 :: Integer)) in
                   let v8
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v5
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v5) (coe v7) (coe v6))
                             v6 in
                   case coe v1 of
                     1 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v5)
                     2 -> coe v6
                     _ -> coe v8)
                v2 v3)
             v2 v3
             (coe
                du_'215''45'cong'691'_160 (coe v0)
                (coe subInt (coe v1) (coe (1 :: Integer))) (coe v2) (coe v3)
                (coe v4))
             v4
-- Algebra.Properties.Monoid.Mult.TCOptimised.×-cong
d_'215''45'cong_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer ->
  Integer ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  AgdaAny -> AgdaAny
d_'215''45'cong_170 ~v0 ~v1 v2 v3 ~v4 v5 v6 ~v7 v8
  = du_'215''45'cong_170 v2 v3 v5 v6 v8
du_'215''45'cong_170 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'215''45'cong_170 v0 v1 v2 v3 v4
  = coe
      du_'215''45'cong'691'_160 (coe v0) (coe v1) (coe v2) (coe v3)
      (coe v4)
-- Algebra.Properties.Monoid.Mult.TCOptimised.×-assocˡ
d_'215''45'assoc'737'_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
d_'215''45'assoc'737'_182 ~v0 ~v1 v2 v3 v4 v5
  = du_'215''45'assoc'737'_182 v2 v3 v4 v5
du_'215''45'assoc'737'_182 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  AgdaAny -> Integer -> Integer -> AgdaAny
du_'215''45'assoc'737'_182 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
         (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
          let v5
                = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
          coe
            MAlonzo.Code.Algebra.Structures.du_setoid_164
            (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
         (let v4
                = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
          let v5
                = let v5
                        = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
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
                    _ -> coe v7 in
          let v6 = subInt (coe v2) (coe (1 :: Integer)) in
          let v7
                = coe
                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                    (coe
                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                       (coe v4) (coe v6) (coe v5))
                    v5 in
          case coe v2 of
            0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
            1 -> coe v5
            _ -> coe v7)
         (let v4
                = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
          let v5
                = coe
                    MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                    (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                    (coe v3) (coe v1) in
          let v6 = subInt (coe v2) (coe (1 :: Integer)) in
          let v7
                = coe
                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                    (coe
                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                       (coe v4) (coe v6) (coe v5))
                    v5 in
          case coe v2 of
            0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
            1 -> coe v5
            _ -> coe v7)
         (let v4
                = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
          let v5 = mulInt (coe v2) (coe v3) in
          let v6 = subInt (coe v5) (coe (1 :: Integer)) in
          let v7
                = coe
                    MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                    (coe
                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                       (coe v4) (coe v6) (coe v1))
                    v1 in
          case coe v5 of
            0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
            1 -> coe v1
            _ -> coe v7)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
             let v5
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
            (let v4
                   = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
             let v5
                   = coe
                       MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                       (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                       (coe v3) (coe v1) in
             let v6 = subInt (coe v2) (coe (1 :: Integer)) in
             let v7
                   = coe
                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                       (coe
                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                          (coe v4) (coe v6) (coe v5))
                       v5 in
             case coe v2 of
               0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
               1 -> coe v5
               _ -> coe v7)
            (coe
               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
               (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
               (coe v2)
               (coe
                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                  (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                  (coe v3) (coe v1)))
            (let v4
                   = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
             let v5 = mulInt (coe v2) (coe v3) in
             let v6 = subInt (coe v5) (coe (1 :: Integer)) in
             let v7
                   = coe
                       MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                       (coe
                          MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                          (coe v4) (coe v6) (coe v1))
                       v1 in
             case coe v5 of
               0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
               1 -> coe v1
               _ -> coe v7)
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                let v5
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
               (coe
                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                  (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                  (coe v2)
                  (coe
                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                     (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                     (coe v3) (coe v1)))
               (coe
                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                  (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                  (coe mulInt (coe v2) (coe v3)) (coe v1))
               (let v4
                      = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                let v5 = mulInt (coe v2) (coe v3) in
                let v6 = subInt (coe v5) (coe (1 :: Integer)) in
                let v7
                      = coe
                          MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                          (coe
                             MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                             (coe v4) (coe v6) (coe v1))
                          v1 in
                case coe v5 of
                  0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                  1 -> coe v1
                  _ -> coe v7)
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                   let v5
                         = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
                   coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))
                  (coe
                     MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                     (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                     (coe mulInt (coe v2) (coe v3)) (coe v1))
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                   let v5 = mulInt (coe v2) (coe v3) in
                   let v6 = subInt (coe v5) (coe (1 :: Integer)) in
                   let v7
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v4) (coe v6) (coe v1))
                             v1 in
                   case coe v5 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                     1 -> coe v1
                     _ -> coe v7)
                  (let v4
                         = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                   let v5 = mulInt (coe v2) (coe v3) in
                   let v6 = subInt (coe v5) (coe (1 :: Integer)) in
                   let v7
                         = coe
                             MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                             (coe
                                MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                (coe v4) (coe v6) (coe v1))
                             v1 in
                   case coe v5 of
                     0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                     1 -> coe v1
                     _ -> coe v7)
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                           (let v4 = MAlonzo.Code.Algebra.Bundles.d_isMonoid_764 (coe v0) in
                            let v5
                                  = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v4) in
                            coe
                              MAlonzo.Code.Algebra.Structures.du_setoid_164
                              (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v5)))))
                     (let v4
                            = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
                      let v5 = mulInt (coe v2) (coe v3) in
                      let v6 = subInt (coe v5) (coe (1 :: Integer)) in
                      let v7
                            = coe
                                MAlonzo.Code.Algebra.Bundles.Raw.d__'8729'__56 v4
                                (coe
                                   MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215''8242'__52
                                   (coe v4) (coe v6) (coe v1))
                                v1 in
                      case coe v5 of
                        0 -> coe MAlonzo.Code.Algebra.Bundles.Raw.d_ε_58 (coe v4)
                        1 -> coe v1
                        _ -> coe v7))
                  (coe
                     du_'215''7524''8776''215'_134 (coe v0)
                     (coe mulInt (coe v2) (coe v3)) (coe v1)))
               (coe
                  MAlonzo.Code.Algebra.Properties.Monoid.Mult.du_'215''45'assoc'737'_286
                  (coe v0) (coe v1) (coe v2) (coe v3)))
            (coe
               du_'215''7524''8776''215'_134 (coe v0) (coe v2)
               (coe
                  MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
                  (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
                  (coe v3) (coe v1))))
         (coe
            du_'215''45'cong'691'_160 (coe v0) (coe v2)
            (coe
               MAlonzo.Code.Algebra.Definitions.RawMonoid.du__'215'__44
               (coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0))
               (coe v3) (coe v1))
            (let v4
                   = coe MAlonzo.Code.Algebra.Bundles.du_rawMonoid_812 (coe v0) in
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
            (coe du_'215''7524''8776''215'_134 (coe v0) (coe v3) (coe v1))))
