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

module MAlonzo.Code.Data.Integer.Base where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Nat
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Sign.Base
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core

-- Data.Integer.Base.0ℤ
d_0ℤ_12 :: Integer
d_0ℤ_12 = coe (0 :: Integer)
-- Data.Integer.Base.-1ℤ
d_'45'1ℤ_14 :: Integer
d_'45'1ℤ_14 = coe (-1 :: Integer)
-- Data.Integer.Base.1ℤ
d_1ℤ_16 :: Integer
d_1ℤ_16 = coe (1 :: Integer)
-- Data.Integer.Base.∣_∣
d_'8739'_'8739'_18 :: Integer -> Integer
d_'8739'_'8739'_18 v0
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) -> coe v0
      _ -> coe subInt (coe (0 :: Integer)) (coe v0)
-- Data.Integer.Base.sign
d_sign_24 :: Integer -> MAlonzo.Code.Data.Sign.Base.T_Sign_6
d_sign_24 v0
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          coe MAlonzo.Code.Data.Sign.Base.C_'43'_10
      _ -> coe MAlonzo.Code.Data.Sign.Base.C_'45'_8
-- Data.Integer.Base._≤_
d__'8804'__26 a0 a1 = ()
data T__'8804'__26
  = C_'45''8804''45'_34 MAlonzo.Code.Data.Nat.Base.T__'8804'__18 |
    C_'45''8804''43'_40 |
    C_'43''8804''43'_48 MAlonzo.Code.Data.Nat.Base.T__'8804'__18
-- Data.Integer.Base._<_
d__'60'__50 a0 a1 = ()
data T__'60'__50
  = C_'45''60''45'_58 MAlonzo.Code.Data.Nat.Base.T__'8804'__18 |
    C_'45''60''43'_64 |
    C_'43''60''43'_72 MAlonzo.Code.Data.Nat.Base.T__'8804'__18
-- Data.Integer.Base._≥_
d__'8805'__74 :: Integer -> Integer -> ()
d__'8805'__74 = erased
-- Data.Integer.Base._>_
d__'62'__80 :: Integer -> Integer -> ()
d__'62'__80 = erased
-- Data.Integer.Base._≰_
d__'8816'__86 :: Integer -> Integer -> ()
d__'8816'__86 = erased
-- Data.Integer.Base._≱_
d__'8817'__92 :: Integer -> Integer -> ()
d__'8817'__92 = erased
-- Data.Integer.Base._≮_
d__'8814'__98 :: Integer -> Integer -> ()
d__'8814'__98 = erased
-- Data.Integer.Base._≯_
d__'8815'__104 :: Integer -> Integer -> ()
d__'8815'__104 = erased
-- Data.Integer.Base._≤ᵇ_
d__'8804''7495'__110 :: Integer -> Integer -> Bool
d__'8804''7495'__110 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe
                  MAlonzo.Code.Data.Nat.Base.d__'8804''7495'__10 (coe v0) (coe v1)
            _ -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
             _ -> let v3 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    MAlonzo.Code.Data.Nat.Base.d__'8804''7495'__10 (coe v3) (coe v2)
-- Data.Integer.Base.NonZero
d_NonZero_128 :: Integer -> ()
d_NonZero_128 = erased
-- Data.Integer.Base.Positive
d_Positive_134 a0 = ()
newtype T_Positive_134 = C_Positive'46'constructor_1295 AgdaAny
-- Data.Integer.Base.Positive.pos
d_pos_140 :: T_Positive_134 -> AgdaAny
d_pos_140 v0
  = case coe v0 of
      C_Positive'46'constructor_1295 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Base.NonNegative
d_NonNegative_144 a0 = ()
newtype T_NonNegative_144
  = C_NonNegative'46'constructor_1353 AgdaAny
-- Data.Integer.Base.NonNegative.nonNeg
d_nonNeg_150 :: T_NonNegative_144 -> AgdaAny
d_nonNeg_150 v0
  = case coe v0 of
      C_NonNegative'46'constructor_1353 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Base.NonPositive
d_NonPositive_154 a0 = ()
newtype T_NonPositive_154
  = C_NonPositive'46'constructor_1411 AgdaAny
-- Data.Integer.Base.NonPositive.nonPos
d_nonPos_160 :: T_NonPositive_154 -> AgdaAny
d_nonPos_160 v0
  = case coe v0 of
      C_NonPositive'46'constructor_1411 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Base.Negative
d_Negative_164 a0 = ()
newtype T_Negative_164 = C_Negative'46'constructor_1469 AgdaAny
-- Data.Integer.Base.Negative.neg
d_neg_170 :: T_Negative_164 -> AgdaAny
d_neg_170 v0
  = case coe v0 of
      C_Negative'46'constructor_1469 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Base.pos
d_pos_174 :: Integer -> T_Positive_134
d_pos_174 ~v0 = du_pos_174
du_pos_174 :: T_Positive_134
du_pos_174
  = coe
      C_Positive'46'constructor_1295
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Integer.Base.nonNeg
d_nonNeg_178 :: Integer -> T_NonNegative_144
d_nonNeg_178 ~v0 = du_nonNeg_178
du_nonNeg_178 :: T_NonNegative_144
du_nonNeg_178
  = coe
      C_NonNegative'46'constructor_1353
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Integer.Base.nonPos0
d_nonPos0_180 :: T_NonPositive_154
d_nonPos0_180
  = coe
      C_NonPositive'46'constructor_1411
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Integer.Base.nonPos
d_nonPos_184 :: Integer -> T_NonPositive_154
d_nonPos_184 ~v0 = du_nonPos_184
du_nonPos_184 :: T_NonPositive_154
du_nonPos_184
  = coe
      C_NonPositive'46'constructor_1411
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Integer.Base.neg
d_neg_188 :: Integer -> T_Negative_164
d_neg_188 ~v0 = du_neg_188
du_neg_188 :: T_Negative_164
du_neg_188
  = coe
      C_Negative'46'constructor_1469
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Integer.Base.≢-nonZero
d_'8802''45'nonZero_192 ::
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_'8802''45'nonZero_192 v0 ~v1 = du_'8802''45'nonZero_192 v0
du_'8802''45'nonZero_192 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_'8802''45'nonZero_192 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
      _ | coe geqInt (coe v0) (coe (1 :: Integer)) ->
          coe
            MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
            (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      _ -> coe
             MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
             (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Integer.Base.>-nonZero
d_'62''45'nonZero_202 ::
  Integer -> T__'60'__50 -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_'62''45'nonZero_202 ~v0 v1 = du_'62''45'nonZero_202 v1
du_'62''45'nonZero_202 ::
  T__'60'__50 -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_'62''45'nonZero_202 v0
  = case coe v0 of
      C_'43''60''43'_72 v3
        -> coe
             seq (coe v3)
             (coe
                MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Base.<-nonZero
d_'60''45'nonZero_208 ::
  Integer -> T__'60'__50 -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_'60''45'nonZero_208 ~v0 v1 = du_'60''45'nonZero_208 v1
du_'60''45'nonZero_208 ::
  T__'60'__50 -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
du_'60''45'nonZero_208 v0
  = coe
      seq (coe v0)
      (coe
         MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Integer.Base.positive
d_positive_212 :: Integer -> T__'60'__50 -> T_Positive_134
d_positive_212 ~v0 v1 = du_positive_212 v1
du_positive_212 :: T__'60'__50 -> T_Positive_134
du_positive_212 v0
  = case coe v0 of
      C_'43''60''43'_72 v3
        -> coe
             seq (coe v3)
             (coe
                C_Positive'46'constructor_1295
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Base.negative
d_negative_218 :: Integer -> T__'60'__50 -> T_Negative_164
d_negative_218 ~v0 v1 = du_negative_218 v1
du_negative_218 :: T__'60'__50 -> T_Negative_164
du_negative_218 v0
  = coe
      seq (coe v0)
      (coe
         C_Negative'46'constructor_1469
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Integer.Base.nonPositive
d_nonPositive_222 :: Integer -> T__'8804'__26 -> T_NonPositive_154
d_nonPositive_222 ~v0 v1 = du_nonPositive_222 v1
du_nonPositive_222 :: T__'8804'__26 -> T_NonPositive_154
du_nonPositive_222 v0
  = case coe v0 of
      C_'45''8804''43'_40
        -> coe
             C_NonPositive'46'constructor_1411
             (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      C_'43''8804''43'_48 v3
        -> coe
             seq (coe v3)
             (coe
                C_NonPositive'46'constructor_1411
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Base.nonNegative
d_nonNegative_226 :: Integer -> T__'8804'__26 -> T_NonNegative_144
d_nonNegative_226 v0 ~v1 = du_nonNegative_226 v0
du_nonNegative_226 :: Integer -> T_NonNegative_144
du_nonNegative_226 v0
  = coe
      seq (coe v0)
      (coe
         C_NonNegative'46'constructor_1353
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Data.Integer.Base._◃_
d__'9667'__230 ::
  MAlonzo.Code.Data.Sign.Base.T_Sign_6 -> Integer -> Integer
d__'9667'__230 v0 v1
  = case coe v1 of
      0 -> coe (0 :: Integer)
      _ -> case coe v0 of
             MAlonzo.Code.Data.Sign.Base.C_'45'_8
               -> coe subInt (coe (0 :: Integer)) (coe v1)
             MAlonzo.Code.Data.Sign.Base.C_'43'_10 -> coe v1
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Integer.Base.SignAbs
d_SignAbs_236 a0 = ()
data T_SignAbs_236
  = C__'9666'__242 MAlonzo.Code.Data.Sign.Base.T_Sign_6 Integer
-- Data.Integer.Base.signAbs
d_signAbs_246 :: Integer -> T_SignAbs_236
d_signAbs_246 v0
  = case coe v0 of
      0 -> coe
             C__'9666'__242 (coe MAlonzo.Code.Data.Sign.Base.C_'43'_10)
             (coe (0 :: Integer))
      _ | coe geqInt (coe v0) (coe (1 :: Integer)) ->
          coe
            C__'9666'__242 (coe MAlonzo.Code.Data.Sign.Base.C_'43'_10) (coe v0)
      _ -> coe
             C__'9666'__242 (coe MAlonzo.Code.Data.Sign.Base.C_'45'_8)
             (coe subInt (coe (0 :: Integer)) (coe v0))
-- Data.Integer.Base.-_
d_'45'__252 :: Integer -> Integer
d_'45'__252 v0 = coe subInt (coe (0 :: Integer)) (coe v0)
-- Data.Integer.Base._⊖_
d__'8854'__258 :: Integer -> Integer -> Integer
d__'8854'__258 v0 v1
  = let v2 = ltInt (coe v0) (coe v1) in
    if coe v2
      then coe d_'45'__252 (coe subInt (coe v1) (coe v0))
      else coe subInt (coe v0) (coe v1)
-- Data.Integer.Base._+_
d__'43'__276 :: Integer -> Integer -> Integer
d__'43'__276 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe addInt (coe v0) (coe v1)
            _ -> coe
                   d__'8854'__258 (coe v0) (coe subInt (coe (0 :: Integer)) (coe v1))
      _ -> case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                 coe
                   d__'8854'__258 (coe v1) (coe subInt (coe (0 :: Integer)) (coe v0))
             _ -> coe addInt (coe v0) (coe v1)
-- Data.Integer.Base._-_
d__'45'__294 :: Integer -> Integer -> Integer
d__'45'__294 v0 v1
  = coe d__'43'__276 (coe v0) (coe d_'45'__252 (coe v1))
-- Data.Integer.Base.suc
d_suc_300 :: Integer -> Integer
d_suc_300 v0 = coe d__'43'__276 (coe d_1ℤ_16) (coe v0)
-- Data.Integer.Base.pred
d_pred_304 :: Integer -> Integer
d_pred_304 v0 = coe d__'43'__276 (coe d_'45'1ℤ_14) (coe v0)
-- Data.Integer.Base._*_
d__'42'__308 :: Integer -> Integer -> Integer
d__'42'__308 v0 v1
  = coe
      d__'9667'__230
      (coe
         MAlonzo.Code.Data.Sign.Base.d__'42'__14 (coe d_sign_24 (coe v0))
         (coe d_sign_24 (coe v1)))
      (coe
         mulInt (coe d_'8739'_'8739'_18 (coe v0))
         (coe d_'8739'_'8739'_18 (coe v1)))
-- Data.Integer.Base._^_
d__'94'__314 :: Integer -> Integer -> Integer
d__'94'__314 v0 v1
  = case coe v1 of
      0 -> coe d_1ℤ_16
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           coe d__'42'__308 (coe v0) (coe d__'94'__314 (coe v0) (coe v2))
-- Data.Integer.Base._⊔_
d__'8852'__322 :: Integer -> Integer -> Integer
d__'8852'__322 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v0) (coe v1)
            _ -> coe v0
      _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) -> coe v1
             _ -> let v3 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    subInt (coe (-1 :: Integer))
                    (coe MAlonzo.Code.Data.Nat.Base.d__'8851'__166 (coe v2) (coe v3))
-- Data.Integer.Base._⊓_
d__'8851'__340 :: Integer -> Integer -> Integer
d__'8851'__340 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          case coe v1 of
            _ | coe geqInt (coe v1) (coe (0 :: Integer)) ->
                coe MAlonzo.Code.Data.Nat.Base.d__'8851'__166 (coe v0) (coe v1)
            _ -> coe v1
      _ -> let v2 = subInt (coe (-1 :: Integer)) (coe v0) in
           case coe v1 of
             _ | coe geqInt (coe v1) (coe (0 :: Integer)) -> coe v0
             _ -> let v3 = subInt (coe (-1 :: Integer)) (coe v1) in
                  coe
                    subInt (coe (-1 :: Integer))
                    (coe MAlonzo.Code.Data.Nat.Base.d__'8852'__138 (coe v2) (coe v3))
-- Data.Integer.Base._/ℕ_
d__'47'ℕ__364 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d__'47'ℕ__364 v0 v1 ~v2 = du__'47'ℕ__364 v0 v1
du__'47'ℕ__364 :: Integer -> Integer -> Integer
du__'47'ℕ__364 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1)
      _ -> let v2
                 = coe
                     MAlonzo.Code.Data.Nat.Base.du__'37'__260
                     (coe subInt (coe (0 :: Integer)) (coe v0)) (coe v1) in
           case coe v2 of
             0 -> coe
                    d_'45'__252
                    (coe
                       MAlonzo.Code.Data.Nat.Base.du__'47'__248
                       (coe subInt (coe (0 :: Integer)) (coe v0)) (coe v1))
             _ -> coe
                    subInt (coe (-1 :: Integer))
                    (coe
                       MAlonzo.Code.Data.Nat.Base.du__'47'__248
                       (coe subInt (coe (0 :: Integer)) (coe v0)) (coe v1))
-- Data.Integer.Base._/_
d__'47'__394 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d__'47'__394 v0 v1 ~v2 = du__'47'__394 v0 v1
du__'47'__394 :: Integer -> Integer -> Integer
du__'47'__394 v0 v1
  = coe
      d__'42'__308
      (coe d__'9667'__230 (coe d_sign_24 (coe v1)) (coe (1 :: Integer)))
      (coe du__'47'ℕ__364 (coe v0) (coe d_'8739'_'8739'_18 (coe v1)))
-- Data.Integer.Base._%ℕ_
d__'37'ℕ__406 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d__'37'ℕ__406 v0 v1 ~v2 = du__'37'ℕ__406 v0 v1
du__'37'ℕ__406 :: Integer -> Integer -> Integer
du__'37'ℕ__406 v0 v1
  = case coe v0 of
      _ | coe geqInt (coe v0) (coe (0 :: Integer)) ->
          coe MAlonzo.Code.Data.Nat.Base.du__'37'__260 (coe v0) (coe v1)
      _ -> let v2
                 = coe
                     MAlonzo.Code.Data.Nat.Base.du__'37'__260
                     (coe subInt (coe (0 :: Integer)) (coe v0)) (coe v1) in
           case coe v2 of
             0 -> coe (0 :: Integer)
             _ -> coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v1 v2
-- Data.Integer.Base._%_
d__'37'__436 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d__'37'__436 v0 v1 ~v2 = du__'37'__436 v0 v1
du__'37'__436 :: Integer -> Integer -> Integer
du__'37'__436 v0 v1
  = coe du__'37'ℕ__406 (coe v0) (coe d_'8739'_'8739'_18 (coe v1))
-- Data.Integer.Base.+-rawMagma
d_'43''45'rawMagma_442 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'43''45'rawMagma_442
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMagma'46'constructor_77
      d__'43'__276
-- Data.Integer.Base.+-0-rawMonoid
d_'43''45'0'45'rawMonoid_444 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'43''45'0'45'rawMonoid_444
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMonoid'46'constructor_473
      d__'43'__276 d_0ℤ_12
-- Data.Integer.Base.+-0-rawGroup
d_'43''45'0'45'rawGroup_446 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawGroup_70
d_'43''45'0'45'rawGroup_446
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawGroup'46'constructor_921
      d__'43'__276 d_0ℤ_12 d_'45'__252
-- Data.Integer.Base.*-rawMagma
d_'42''45'rawMagma_448 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'42''45'rawMagma_448
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMagma'46'constructor_77
      d__'42'__308
-- Data.Integer.Base.*-1-rawMonoid
d_'42''45'1'45'rawMonoid_450 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'42''45'1'45'rawMonoid_450
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMonoid'46'constructor_473
      d__'42'__308 d_1ℤ_16
-- Data.Integer.Base.+-*-rawNearSemiring
d_'43''45''42''45'rawNearSemiring_452 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
d_'43''45''42''45'rawNearSemiring_452
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawNearSemiring'46'constructor_1421
      d__'43'__276 d__'42'__308 d_0ℤ_12
-- Data.Integer.Base.+-*-rawSemiring
d_'43''45''42''45'rawSemiring_454 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_'43''45''42''45'rawSemiring_454
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawSemiring'46'constructor_2023
      d__'43'__276 d__'42'__308 d_0ℤ_12 d_1ℤ_16
-- Data.Integer.Base.+-*-rawRing
d_'43''45''42''45'rawRing_456 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242
d_'43''45''42''45'rawRing_456
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawRing'46'constructor_3463
      d__'43'__276 d__'42'__308 d_'45'__252 d_0ℤ_12 d_1ℤ_16
