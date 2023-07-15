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

module MAlonzo.Code.Data.Nat.Base where

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
import qualified MAlonzo.Code.Data.Parity.Base
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core

-- Data.Nat.Base._≤ᵇ_
d__'8804''7495'__10 :: Integer -> Integer -> Bool
d__'8804''7495'__10 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe ltInt (coe v2) (coe v1)
-- Data.Nat.Base._≤_
d__'8804'__18 a0 a1 = ()
data T__'8804'__18 = C_z'8804'n_22 | C_s'8804's_30 T__'8804'__18
-- Data.Nat.Base._<_
d__'60'__32 :: Integer -> Integer -> ()
d__'60'__32 = erased
-- Data.Nat.Base._≥_
d__'8805'__50 :: Integer -> Integer -> ()
d__'8805'__50 = erased
-- Data.Nat.Base._>_
d__'62'__56 :: Integer -> Integer -> ()
d__'62'__56 = erased
-- Data.Nat.Base._≰_
d__'8816'__62 :: Integer -> Integer -> ()
d__'8816'__62 = erased
-- Data.Nat.Base._≮_
d__'8814'__68 :: Integer -> Integer -> ()
d__'8814'__68 = erased
-- Data.Nat.Base._≱_
d__'8817'__74 :: Integer -> Integer -> ()
d__'8817'__74 = erased
-- Data.Nat.Base._≯_
d__'8815'__80 :: Integer -> Integer -> ()
d__'8815'__80 = erased
-- Data.Nat.Base.NonZero
d_NonZero_88 a0 = ()
newtype T_NonZero_88 = C_NonZero'46'constructor_563 AgdaAny
-- Data.Nat.Base.NonZero.nonZero
d_nonZero_94 :: T_NonZero_88 -> AgdaAny
d_nonZero_94 v0
  = case coe v0 of
      C_NonZero'46'constructor_563 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Base.nonZero
d_nonZero_98 :: Integer -> T_NonZero_88
d_nonZero_98 ~v0 = du_nonZero_98
du_nonZero_98 :: T_NonZero_88
du_nonZero_98
  = coe
      C_NonZero'46'constructor_563
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Nat.Base.≢-nonZero
d_'8802''45'nonZero_102 ::
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  T_NonZero_88
d_'8802''45'nonZero_102 v0 ~v1 = du_'8802''45'nonZero_102 v0
du_'8802''45'nonZero_102 :: Integer -> T_NonZero_88
du_'8802''45'nonZero_102 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38
      _ -> coe
             C_NonZero'46'constructor_563
             (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Data.Nat.Base.>-nonZero
d_'62''45'nonZero_112 :: Integer -> T__'8804'__18 -> T_NonZero_88
d_'62''45'nonZero_112 ~v0 v1 = du_'62''45'nonZero_112 v1
du_'62''45'nonZero_112 :: T__'8804'__18 -> T_NonZero_88
du_'62''45'nonZero_112 v0
  = case coe v0 of
      C_s'8804's_30 v3
        -> coe
             seq (coe v3)
             (coe
                C_NonZero'46'constructor_563
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Base.≢-nonZero⁻¹
d_'8802''45'nonZero'8315''185'_116 ::
  Integer ->
  T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8802''45'nonZero'8315''185'_116 = erased
-- Data.Nat.Base.>-nonZero⁻¹
d_'62''45'nonZero'8315''185'_122 ::
  Integer -> T_NonZero_88 -> T__'8804'__18
d_'62''45'nonZero'8315''185'_122 ~v0 ~v1
  = du_'62''45'nonZero'8315''185'_122
du_'62''45'nonZero'8315''185'_122 :: T__'8804'__18
du_'62''45'nonZero'8315''185'_122
  = coe C_s'8804's_30 (coe C_z'8804'n_22)
-- Data.Nat.Base.pred
d_pred_126 :: Integer -> Integer
d_pred_126 v0
  = coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v0 (1 :: Integer)
-- Data.Nat.Base._+⋎_
d__'43''8910'__130 :: Integer -> Integer -> Integer
d__'43''8910'__130 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             addInt (coe (1 :: Integer))
             (coe d__'43''8910'__130 (coe v1) (coe v2))
-- Data.Nat.Base._⊔_
d__'8852'__138 :: Integer -> Integer -> Integer
d__'8852'__138 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe v0
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    addInt (coe (1 :: Integer)) (coe d__'8852'__138 (coe v2) (coe v3))
-- Data.Nat.Base._⊔′_
d__'8852''8242'__148 :: Integer -> Integer -> Integer
d__'8852''8242'__148 v0 v1
  = let v2 = ltInt (coe v0) (coe v1) in
    if coe v2 then coe v1 else coe v0
-- Data.Nat.Base._⊓_
d__'8851'__166 :: Integer -> Integer -> Integer
d__'8851'__166 v0 v1
  = case coe v0 of
      0 -> coe (0 :: Integer)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe (0 :: Integer)
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe
                    addInt (coe (1 :: Integer)) (coe d__'8851'__166 (coe v2) (coe v3))
-- Data.Nat.Base._⊓′_
d__'8851''8242'__176 :: Integer -> Integer -> Integer
d__'8851''8242'__176 v0 v1
  = let v2 = ltInt (coe v0) (coe v1) in
    if coe v2 then coe v0 else coe v1
-- Data.Nat.Base.parity
d_parity_194 :: Integer -> MAlonzo.Code.Data.Parity.Base.T_Parity_6
d_parity_194 v0
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Parity.Base.C_0ℙ_8
      1 -> coe MAlonzo.Code.Data.Parity.Base.C_1ℙ_10
      _ -> let v1 = subInt (coe v0) (coe (2 :: Integer)) in
           coe d_parity_194 (coe v1)
-- Data.Nat.Base.⌊_/2⌋
d_'8970'_'47'2'8971'_198 :: Integer -> Integer
d_'8970'_'47'2'8971'_198 v0
  = case coe v0 of
      0 -> coe (0 :: Integer)
      1 -> coe (0 :: Integer)
      _ -> let v1 = subInt (coe v0) (coe (2 :: Integer)) in
           coe
             addInt (coe (1 :: Integer)) (coe d_'8970'_'47'2'8971'_198 (coe v1))
-- Data.Nat.Base.⌈_/2⌉
d_'8968'_'47'2'8969'_202 :: Integer -> Integer
d_'8968'_'47'2'8969'_202 v0
  = coe
      d_'8970'_'47'2'8971'_198 (coe addInt (coe (1 :: Integer)) (coe v0))
-- Data.Nat.Base._^_
d__'94'__206 :: Integer -> Integer -> Integer
d__'94'__206 v0 v1
  = case coe v1 of
      0 -> coe (1 :: Integer)
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           coe mulInt (coe v0) (coe d__'94'__206 (coe v0) (coe v2))
-- Data.Nat.Base.∣_-_∣
d_'8739'_'45'_'8739'_214 :: Integer -> Integer -> Integer
d_'8739'_'45'_'8739'_214 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe v0
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe d_'8739'_'45'_'8739'_214 (coe v2) (coe v3)
-- Data.Nat.Base.∣_-_∣′
d_'8739'_'45'_'8739''8242'_224 :: Integer -> Integer -> Integer
d_'8739'_'45'_'8739''8242'_224 v0 v1
  = let v2 = ltInt (coe v0) (coe v1) in
    if coe v2
      then coe subInt (coe v1) (coe v0)
      else coe subInt (coe v0) (coe v1)
-- Data.Nat.Base._/_
d__'47'__248 :: Integer -> Integer -> T_NonZero_88 -> Integer
d__'47'__248 v0 v1 ~v2 = du__'47'__248 v0 v1
du__'47'__248 :: Integer -> Integer -> Integer
du__'47'__248 v0 v1 = coe quotInt (coe v0) (coe v1)
-- Data.Nat.Base._%_
d__'37'__260 :: Integer -> Integer -> T_NonZero_88 -> Integer
d__'37'__260 v0 v1 ~v2 = du__'37'__260 v0 v1
du__'37'__260 :: Integer -> Integer -> Integer
du__'37'__260 v0 v1 = coe remInt (coe v0) (coe v1)
-- Data.Nat.Base._!
d__'33'_266 :: Integer -> Integer
d__'33'_266 v0
  = case coe v0 of
      0 -> coe (1 :: Integer)
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe mulInt (coe v0) (coe d__'33'_266 (coe v1))
-- Data.Nat.Base._≤′_
d__'8804''8242'__272 a0 a1 = ()
data T__'8804''8242'__272
  = C_'8804''8242''45'refl_276 |
    C_'8804''8242''45'step_282 T__'8804''8242'__272
-- Data.Nat.Base._<′_
d__'60''8242'__284 :: Integer -> Integer -> ()
d__'60''8242'__284 = erased
-- Data.Nat.Base._≥′_
d__'8805''8242'__298 :: Integer -> Integer -> ()
d__'8805''8242'__298 = erased
-- Data.Nat.Base._>′_
d__'62''8242'__304 :: Integer -> Integer -> ()
d__'62''8242'__304 = erased
-- Data.Nat.Base._≤″_
d__'8804''8243'__314 a0 a1 = ()
newtype T__'8804''8243'__314
  = C_less'45'than'45'or'45'equal_328 Integer
-- Data.Nat.Base._≤″_.k
d_k_324 :: T__'8804''8243'__314 -> Integer
d_k_324 v0
  = case coe v0 of
      C_less'45'than'45'or'45'equal_328 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Base._≤″_.proof
d_proof_326 ::
  T__'8804''8243'__314 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_proof_326 = erased
-- Data.Nat.Base._<″_
d__'60''8243'__330 :: Integer -> Integer -> ()
d__'60''8243'__330 = erased
-- Data.Nat.Base._≥″_
d__'8805''8243'__336 :: Integer -> Integer -> ()
d__'8805''8243'__336 = erased
-- Data.Nat.Base._>″_
d__'62''8243'__342 :: Integer -> Integer -> ()
d__'62''8243'__342 = erased
-- Data.Nat.Base._≤‴_
d__'8804''8244'__348 a0 a1 = ()
data T__'8804''8244'__348
  = C_'8804''8244''45'refl_352 |
    C_'8804''8244''45'step_358 T__'8804''8244'__348
-- Data.Nat.Base._<‴_
d__'60''8244'__360 :: Integer -> Integer -> ()
d__'60''8244'__360 = erased
-- Data.Nat.Base._≥‴_
d__'8805''8244'__366 :: Integer -> Integer -> ()
d__'8805''8244'__366 = erased
-- Data.Nat.Base._>‴_
d__'62''8244'__372 :: Integer -> Integer -> ()
d__'62''8244'__372 = erased
-- Data.Nat.Base.Ordering
d_Ordering_378 a0 a1 = ()
data T_Ordering_378
  = C_less_384 Integer | C_equal_388 | C_greater_394 Integer
-- Data.Nat.Base.compare
d_compare_400 :: Integer -> Integer -> T_Ordering_378
d_compare_400 v0 v1
  = case coe v0 of
      0 -> case coe v1 of
             0 -> coe C_equal_388
             _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe C_less_384 v2
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             0 -> coe C_greater_394 v2
             _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                  coe d_compare_400 (coe v2) (coe v3)
-- Data.Nat.Base.+-rawMagma
d_'43''45'rawMagma_436 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'43''45'rawMagma_436
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMagma'46'constructor_77
      addInt
-- Data.Nat.Base.+-0-rawMonoid
d_'43''45'0'45'rawMonoid_438 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'43''45'0'45'rawMonoid_438
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMonoid'46'constructor_473
      addInt (0 :: Integer)
-- Data.Nat.Base.*-rawMagma
d_'42''45'rawMagma_440 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'42''45'rawMagma_440
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMagma'46'constructor_77
      mulInt
-- Data.Nat.Base.*-1-rawMonoid
d_'42''45'1'45'rawMonoid_442 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'42''45'1'45'rawMonoid_442
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMonoid'46'constructor_473
      mulInt (1 :: Integer)
-- Data.Nat.Base.+-*-rawNearSemiring
d_'43''45''42''45'rawNearSemiring_444 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawNearSemiring_108
d_'43''45''42''45'rawNearSemiring_444
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawNearSemiring'46'constructor_1421
      addInt mulInt (0 :: Integer)
-- Data.Nat.Base.+-*-rawSemiring
d_'43''45''42''45'rawSemiring_446 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawSemiring_148
d_'43''45''42''45'rawSemiring_446
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawSemiring'46'constructor_2023
      addInt mulInt (0 :: Integer) (1 :: Integer)
