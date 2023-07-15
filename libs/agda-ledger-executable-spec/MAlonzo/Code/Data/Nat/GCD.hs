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

module MAlonzo.Code.Data.Nat.GCD where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Divisibility
import qualified MAlonzo.Code.Data.Nat.Divisibility.Core
import qualified MAlonzo.Code.Data.Nat.GCD.Lemmas
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Induction
import qualified MAlonzo.Code.Induction.Lexicographic
import qualified MAlonzo.Code.Induction.WellFounded
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Double
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.Nat.GCD.Algebra.Associative
d_Associative_28 :: (Integer -> Integer -> Integer) -> ()
d_Associative_28 = erased
-- Data.Nat.GCD.Algebra.Commutative
d_Commutative_32 :: (Integer -> Integer -> Integer) -> ()
d_Commutative_32 = erased
-- Data.Nat.GCD.Algebra.Identity
d_Identity_48 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_Identity_48 = erased
-- Data.Nat.GCD.Algebra.LeftIdentity
d_LeftIdentity_74 ::
  Integer -> (Integer -> Integer -> Integer) -> ()
d_LeftIdentity_74 = erased
-- Data.Nat.GCD.Algebra.LeftZero
d_LeftZero_82 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_LeftZero_82 = erased
-- Data.Nat.GCD.Algebra.RightIdentity
d_RightIdentity_104 ::
  Integer -> (Integer -> Integer -> Integer) -> ()
d_RightIdentity_104 = erased
-- Data.Nat.GCD.Algebra.RightZero
d_RightZero_112 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_RightZero_112 = erased
-- Data.Nat.GCD.Algebra.Zero
d_Zero_130 :: Integer -> (Integer -> Integer -> Integer) -> ()
d_Zero_130 = erased
-- Data.Nat.GCD.gcd′
d_gcd'8242'_136 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> Integer
d_gcd'8242'_136 v0 v1 ~v2 ~v3 = du_gcd'8242'_136 v0 v1
du_gcd'8242'_136 :: Integer -> Integer -> Integer
du_gcd'8242'_136 v0 v1
  = case coe v1 of
      0 -> coe v0
      _ -> coe
             du_gcd'8242'_136 (coe v1)
             (coe MAlonzo.Code.Data.Nat.Base.du__'37'__260 (coe v0) (coe v1))
-- Data.Nat.GCD.gcd
d_gcd_148 :: Integer -> Integer -> Integer
d_gcd_148 v0 v1
  = let v2
          = MAlonzo.Code.Data.Nat.Properties.d_'60''45'cmp_2828
              (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v3
        -> coe du_gcd'8242'_136 (coe v1) (coe v0)
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v4
        -> coe v0
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v5
        -> coe du_gcd'8242'_136 (coe v0) (coe v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.gcd′[m,n]∣m,n
d_gcd'8242''91'm'44'n'93''8739'm'44'n_182 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_gcd'8242''91'm'44'n'93''8739'm'44'n_182 v0 v1 ~v2 ~v3
  = du_gcd'8242''91'm'44'n'93''8739'm'44'n_182 v0 v1
du_gcd'8242''91'm'44'n'93''8739'm'44'n_182 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_gcd'8242''91'm'44'n'93''8739'm'44'n_182 v0 v1
  = case coe v1 of
      0 -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe MAlonzo.Code.Data.Nat.Divisibility.du_'8739''45'refl_94)
             (coe MAlonzo.Code.Data.Nat.Divisibility.du__'8739'0_194)
      _ -> let v2
                 = coe
                     du_gcd'8242''91'm'44'n'93''8739'm'44'n_182 (coe v1)
                     (coe remInt (coe v0) (coe v1)) in
           case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                    (coe
                       MAlonzo.Code.Data.Nat.Divisibility.du_'8739'n'8739'm'37'n'8658''8739'm_630
                       (coe v0) (coe v1) (coe v3) (coe v4))
                    (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.gcd′-greatest
d_gcd'8242''45'greatest_224 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'8242''45'greatest_224 v0 v1 v2 ~v3 ~v4 v5 v6
  = du_gcd'8242''45'greatest_224 v0 v1 v2 v5 v6
du_gcd'8242''45'greatest_224 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_gcd'8242''45'greatest_224 v0 v1 v2 v3 v4
  = case coe v1 of
      0 -> coe v3
      _ -> coe
             du_gcd'8242''45'greatest_224 (coe v1)
             (coe remInt (coe v0) (coe v1)) (coe v2) (coe v4)
             (coe
                MAlonzo.Code.Data.Nat.Divisibility.du_'37''45'pres'737''45''8739'_662
                (coe v1) (coe v2) (coe v3) (coe v4))
-- Data.Nat.GCD.gcd[m,n]∣m
d_gcd'91'm'44'n'93''8739'm_252 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'm'44'n'93''8739'm_252 v0 v1
  = let v2
          = MAlonzo.Code.Data.Nat.Properties.d_'60''45'cmp_2828
              (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
             (coe du_gcd'8242''91'm'44'n'93''8739'm'44'n_182 (coe v1) (coe v0))
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v4
        -> coe MAlonzo.Code.Data.Nat.Divisibility.du_'8739''45'refl_94
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v5
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
             (coe du_gcd'8242''91'm'44'n'93''8739'm'44'n_182 (coe v0) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.gcd[m,n]∣n
d_gcd'91'm'44'n'93''8739'n_282 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'm'44'n'93''8739'n_282 v0 v1
  = let v2
          = MAlonzo.Code.Data.Nat.Properties.d_'60''45'cmp_2828
              (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
             (coe du_gcd'8242''91'm'44'n'93''8739'm'44'n_182 (coe v1) (coe v0))
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v4
        -> coe MAlonzo.Code.Data.Nat.Divisibility.du_'8739''45'refl_94
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v5
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
             (coe du_gcd'8242''91'm'44'n'93''8739'm'44'n_182 (coe v0) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.gcd-greatest
d_gcd'45'greatest_314 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'45'greatest_314 v0 v1 v2 v3 v4
  = let v5
          = MAlonzo.Code.Data.Nat.Properties.d_'60''45'cmp_2828
              (coe v0) (coe v1) in
    case coe v5 of
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'60'_150 v6
        -> coe
             du_gcd'8242''45'greatest_224 (coe v1) (coe v0) (coe v2) (coe v4)
             (coe v3)
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'8776'_158 v7
        -> coe v3
      MAlonzo.Code.Relation.Binary.Definitions.C_tri'62'_166 v8
        -> coe
             du_gcd'8242''45'greatest_224 (coe v0) (coe v1) (coe v2) (coe v3)
             (coe v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.gcd[0,0]≡0
d_gcd'91'0'44'0'93''8801'0_356 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'91'0'44'0'93''8801'0_356 = erased
-- Data.Nat.GCD.gcd[m,n]≢0
d_gcd'91'm'44'n'93''8802'0_362 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_gcd'91'm'44'n'93''8802'0_362 = erased
-- Data.Nat.GCD.gcd[m,n]≡0⇒m≡0
d_gcd'91'm'44'n'93''8801'0'8658'm'8801'0_388 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'91'm'44'n'93''8801'0'8658'm'8801'0_388 = erased
-- Data.Nat.GCD.gcd[m,n]≡0⇒n≡0
d_gcd'91'm'44'n'93''8801'0'8658'n'8801'0_404 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'91'm'44'n'93''8801'0'8658'n'8801'0_404 = erased
-- Data.Nat.GCD.gcd-comm
d_gcd'45'comm_416 ::
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'45'comm_416 = erased
-- Data.Nat.GCD.gcd-assoc
d_gcd'45'assoc_422 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'45'assoc_422 = erased
-- Data.Nat.GCD._.gcd[gcd[m,n],p]|m
d_gcd'91'gcd'91'm'44'n'93''44'p'93''124'm_434 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'gcd'91'm'44'n'93''44'p'93''124'm_434 v0 v1 v2
  = let v3
          = MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134 in
    coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin__110
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v3))
      (coe d_gcd_148 (coe d_gcd_148 (coe v0) (coe v1)) (coe v2)) (coe v0)
      (coe
         MAlonzo.Code.Data.Nat.Divisibility.d_step'45''8739'_184
         (d_gcd_148 (coe d_gcd_148 (coe v0) (coe v1)) (coe v2))
         (d_gcd_148 (coe v0) (coe v1)) v0
         (coe
            MAlonzo.Code.Data.Nat.Divisibility.d_step'45''8739'_184
            (d_gcd_148 (coe v0) (coe v1)) v0 v0
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154
                  (coe MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134))
               (coe v0))
            (d_gcd'91'm'44'n'93''8739'm_252 (coe v0) (coe v1)))
         (d_gcd'91'm'44'n'93''8739'm_252
            (coe d_gcd_148 (coe v0) (coe v1)) (coe v2)))
-- Data.Nat.GCD._.gcd[gcd[m,n],p]∣n
d_gcd'91'gcd'91'm'44'n'93''44'p'93''8739'n_436 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'gcd'91'm'44'n'93''44'p'93''8739'n_436 v0 v1 v2
  = let v3
          = MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134 in
    coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin__110
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v3))
      (coe d_gcd_148 (coe d_gcd_148 (coe v0) (coe v1)) (coe v2)) (coe v1)
      (coe
         MAlonzo.Code.Data.Nat.Divisibility.d_step'45''8739'_184
         (d_gcd_148 (coe d_gcd_148 (coe v0) (coe v1)) (coe v2))
         (d_gcd_148 (coe v0) (coe v1)) v1
         (coe
            MAlonzo.Code.Data.Nat.Divisibility.d_step'45''8739'_184
            (d_gcd_148 (coe v0) (coe v1)) v1 v1
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154
                  (coe MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134))
               (coe v1))
            (d_gcd'91'm'44'n'93''8739'n_282 (coe v0) (coe v1)))
         (d_gcd'91'm'44'n'93''8739'm_252
            (coe d_gcd_148 (coe v0) (coe v1)) (coe v2)))
-- Data.Nat.GCD._.gcd[gcd[m,n],p]∣p
d_gcd'91'gcd'91'm'44'n'93''44'p'93''8739'p_438 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'gcd'91'm'44'n'93''44'p'93''8739'p_438 v0 v1 v2
  = coe
      d_gcd'91'm'44'n'93''8739'n_282 (coe d_gcd_148 (coe v0) (coe v1))
      (coe v2)
-- Data.Nat.GCD._.gcd[m,gcd[n,p]]∣m
d_gcd'91'm'44'gcd'91'n'44'p'93''93''8739'm_440 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'm'44'gcd'91'n'44'p'93''93''8739'm_440 v0 v1 v2
  = coe
      d_gcd'91'm'44'n'93''8739'm_252 (coe v0)
      (coe d_gcd_148 (coe v1) (coe v2))
-- Data.Nat.GCD._.gcd[m,gcd[n,p]]∣n
d_gcd'91'm'44'gcd'91'n'44'p'93''93''8739'n_442 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'm'44'gcd'91'n'44'p'93''93''8739'n_442 v0 v1 v2
  = let v3
          = MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134 in
    coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin__110
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v3))
      (coe d_gcd_148 (coe v0) (coe d_gcd_148 (coe v1) (coe v2))) (coe v1)
      (coe
         MAlonzo.Code.Data.Nat.Divisibility.d_step'45''8739'_184
         (d_gcd_148 (coe v0) (coe d_gcd_148 (coe v1) (coe v2)))
         (d_gcd_148 (coe v1) (coe v2)) v1
         (coe
            MAlonzo.Code.Data.Nat.Divisibility.d_step'45''8739'_184
            (d_gcd_148 (coe v1) (coe v2)) v1 v1
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154
                  (coe MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134))
               (coe v1))
            (d_gcd'91'm'44'n'93''8739'm_252 (coe v1) (coe v2)))
         (d_gcd'91'm'44'n'93''8739'n_282
            (coe v0) (coe d_gcd_148 (coe v1) (coe v2))))
-- Data.Nat.GCD._.gcd[m,gcd[n,p]]∣p
d_gcd'91'm'44'gcd'91'n'44'p'93''93''8739'p_444 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'm'44'gcd'91'n'44'p'93''93''8739'p_444 v0 v1 v2
  = let v3
          = MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134 in
    coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin__110
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v3))
      (coe d_gcd_148 (coe v0) (coe d_gcd_148 (coe v1) (coe v2))) (coe v2)
      (coe
         MAlonzo.Code.Data.Nat.Divisibility.d_step'45''8739'_184
         (d_gcd_148 (coe v0) (coe d_gcd_148 (coe v1) (coe v2)))
         (d_gcd_148 (coe v1) (coe v2)) v2
         (coe
            MAlonzo.Code.Data.Nat.Divisibility.d_step'45''8739'_184
            (d_gcd_148 (coe v1) (coe v2)) v2 v2
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154
                  (coe MAlonzo.Code.Data.Nat.Divisibility.d_'8739''45'preorder_134))
               (coe v2))
            (d_gcd'91'm'44'n'93''8739'n_282 (coe v1) (coe v2)))
         (d_gcd'91'm'44'n'93''8739'n_282
            (coe v0) (coe d_gcd_148 (coe v1) (coe v2))))
-- Data.Nat.GCD.gcd-identityˡ
d_gcd'45'identity'737'_446 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'45'identity'737'_446 = erased
-- Data.Nat.GCD.gcd-identityʳ
d_gcd'45'identity'691'_448 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'45'identity'691'_448 = erased
-- Data.Nat.GCD.gcd-identity
d_gcd'45'identity_450 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_gcd'45'identity_450
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.GCD.gcd-zeroˡ
d_gcd'45'zero'737'_452 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'45'zero'737'_452 = erased
-- Data.Nat.GCD._.gcd[1,n]∣1
d_gcd'91'1'44'n'93''8739'1_460 ::
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'1'44'n'93''8739'1_460 v0
  = coe d_gcd'91'm'44'n'93''8739'm_252 (coe (1 :: Integer)) (coe v0)
-- Data.Nat.GCD._.1∣gcd[1,n]
d_1'8739'gcd'91'1'44'n'93'_462 ::
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_1'8739'gcd'91'1'44'n'93'_462 v0
  = coe
      MAlonzo.Code.Data.Nat.Divisibility.d_1'8739'__188
      (coe d_gcd_148 (coe (1 :: Integer)) (coe v0))
-- Data.Nat.GCD.gcd-zeroʳ
d_gcd'45'zero'691'_464 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'45'zero'691'_464 = erased
-- Data.Nat.GCD._.gcd[n,1]∣1
d_gcd'91'n'44'1'93''8739'1_472 ::
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'91'n'44'1'93''8739'1_472 v0
  = coe d_gcd'91'm'44'n'93''8739'n_282 (coe v0) (coe (1 :: Integer))
-- Data.Nat.GCD._.1∣gcd[n,1]
d_1'8739'gcd'91'n'44'1'93'_474 ::
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_1'8739'gcd'91'n'44'1'93'_474 v0
  = coe
      MAlonzo.Code.Data.Nat.Divisibility.d_1'8739'__188
      (coe d_gcd_148 (coe v0) (coe (1 :: Integer)))
-- Data.Nat.GCD.gcd-zero
d_gcd'45'zero_476 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_gcd'45'zero_476
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Data.Nat.GCD.gcd-universality
d_gcd'45'universality_488 ::
  Integer ->
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12) ->
  (Integer ->
   MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'45'universality_488 = erased
-- Data.Nat.GCD.gcd[cm,cn]/c≡gcd[m,n]
d_gcd'91'cm'44'cn'93''47'c'8801'gcd'91'm'44'n'93'_522 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'91'cm'44'cn'93''47'c'8801'gcd'91'm'44'n'93'_522 = erased
-- Data.Nat.GCD._.forwards
d_forwards_536 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_forwards_536 v0 v1 v2 ~v3 v4 v5 = du_forwards_536 v0 v1 v2 v4 v5
du_forwards_536 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_forwards_536 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
        -> coe
             MAlonzo.Code.Data.Nat.Divisibility.du_m'42'n'8739'o'8658'n'8739'o'47'm_522
             v0 v3
             (d_gcd'45'greatest_314
                (coe mulInt (coe v0) (coe v1)) (coe mulInt (coe v0) (coe v2))
                (coe mulInt (coe v0) (coe v3)) (coe v5) (coe v6))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD._.backwards
d_backwards_546 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_backwards_546 v0 v1 v2 ~v3 v4 v5
  = du_backwards_546 v0 v1 v2 v4 v5
du_backwards_546 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_backwards_546 v0 v1 v2 v3 v4
  = let v5
          = coe
              MAlonzo.Code.Data.Nat.Divisibility.du_m'8739'n'47'o'8658'o'42'm'8739'n_566
              v3 v0
              (d_gcd'45'greatest_314
                 (coe mulInt (coe v0) (coe v1)) (coe mulInt (coe v0) (coe v2))
                 (coe v0)
                 (coe
                    MAlonzo.Code.Data.Nat.Divisibility.du_m'8739'm'42'n_272 (coe v1))
                 (coe
                    MAlonzo.Code.Data.Nat.Divisibility.du_m'8739'm'42'n_272 (coe v2)))
              v4 in
    coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         MAlonzo.Code.Data.Nat.Divisibility.du_'8739''45'trans_96 (coe v5)
         (coe
            d_gcd'91'm'44'n'93''8739'm_252 (coe mulInt (coe v0) (coe v1))
            (coe mulInt (coe v0) (coe v2))))
      (coe
         MAlonzo.Code.Data.Nat.Divisibility.du_'8739''45'trans_96 (coe v5)
         (coe
            d_gcd'91'm'44'n'93''8739'n_282 (coe mulInt (coe v0) (coe v1))
            (coe mulInt (coe v0) (coe v2))))
-- Data.Nat.GCD.c*gcd[m,n]≡gcd[cm,cn]
d_c'42'gcd'91'm'44'n'93''8801'gcd'91'cm'44'cn'93'_568 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_c'42'gcd'91'm'44'n'93''8801'gcd'91'cm'44'cn'93'_568 = erased
-- Data.Nat.GCD.gcd[m,n]≤n
d_gcd'91'm'44'n'93''8804'n_592 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_gcd'91'm'44'n'93''8804'n_592 v0 v1 ~v2
  = du_gcd'91'm'44'n'93''8804'n_592 v0 v1
du_gcd'91'm'44'n'93''8804'n_592 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_gcd'91'm'44'n'93''8804'n_592 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Divisibility.du_'8739''8658''8804'_64
      (coe d_gcd_148 (coe v0) (coe v1)) (coe v1)
      (coe d_gcd'91'm'44'n'93''8739'n_282 (coe v0) (coe v1))
-- Data.Nat.GCD.n/gcd[m,n]≢0
d_n'47'gcd'91'm'44'n'93''8802'0_606 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_n'47'gcd'91'm'44'n'93''8802'0_606 = erased
-- Data.Nat.GCD.m/gcd[m,n]≢0
d_m'47'gcd'91'm'44'n'93''8802'0_620 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_m'47'gcd'91'm'44'n'93''8802'0_620 = erased
-- Data.Nat.GCD.GCD.GCD
d_GCD_638 a0 a1 a2 = ()
data T_GCD_638
  = C_is_662 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
             (Integer ->
              MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
              MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12)
-- Data.Nat.GCD.GCD.GCD.commonDivisor
d_commonDivisor_652 ::
  T_GCD_638 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_commonDivisor_652 v0
  = case coe v0 of
      C_is_662 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.GCD.GCD.greatest
d_greatest_656 ::
  T_GCD_638 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_greatest_656 v0
  = case coe v0 of
      C_is_662 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.GCD.GCD.gcd∣m
d_gcd'8739'm_658 ::
  Integer ->
  Integer ->
  Integer ->
  T_GCD_638 -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'8739'm_658 ~v0 ~v1 ~v2 v3 = du_gcd'8739'm_658 v3
du_gcd'8739'm_658 ::
  T_GCD_638 -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_gcd'8739'm_658 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_commonDivisor_652 (coe v0))
-- Data.Nat.GCD.GCD.GCD.gcd∣n
d_gcd'8739'n_660 ::
  Integer ->
  Integer ->
  Integer ->
  T_GCD_638 -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_gcd'8739'n_660 ~v0 ~v1 ~v2 v3 = du_gcd'8739'n_660 v3
du_gcd'8739'n_660 ::
  T_GCD_638 -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_gcd'8739'n_660 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_commonDivisor_652 (coe v0))
-- Data.Nat.GCD.GCD.unique
d_unique_672 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  T_GCD_638 ->
  T_GCD_638 -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unique_672 = erased
-- Data.Nat.GCD.GCD.sym
d_sym_684 ::
  Integer -> Integer -> Integer -> T_GCD_638 -> T_GCD_638
d_sym_684 ~v0 ~v1 ~v2 v3 = du_sym_684 v3
du_sym_684 :: T_GCD_638 -> T_GCD_638
du_sym_684 v0
  = coe
      C_is_662
      (coe
         MAlonzo.Code.Data.Product.Base.du_swap_346
         (coe d_commonDivisor_652 (coe v0)))
      (coe
         (\ v1 v2 ->
            coe
              d_greatest_656 v0 v1
              (coe MAlonzo.Code.Data.Product.Base.du_swap_346 (coe v2))))
-- Data.Nat.GCD.GCD.refl
d_refl_690 :: Integer -> T_GCD_638
d_refl_690 ~v0 = du_refl_690
du_refl_690 :: T_GCD_638
du_refl_690
  = coe
      C_is_662
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe MAlonzo.Code.Data.Nat.Divisibility.du_'8739''45'refl_94)
         (coe MAlonzo.Code.Data.Nat.Divisibility.du_'8739''45'refl_94))
      (coe
         (\ v0 v1 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v1)))
-- Data.Nat.GCD.GCD.base
d_base_694 :: Integer -> T_GCD_638
d_base_694 ~v0 = du_base_694
du_base_694 :: T_GCD_638
du_base_694
  = coe
      C_is_662
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe MAlonzo.Code.Data.Nat.Divisibility.du__'8739'0_194)
         (coe MAlonzo.Code.Data.Nat.Divisibility.du_'8739''45'refl_94))
      (coe
         (\ v0 v1 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v1)))
-- Data.Nat.GCD.GCD.step
d_step_704 ::
  Integer -> Integer -> Integer -> T_GCD_638 -> T_GCD_638
d_step_704 ~v0 ~v1 ~v2 v3 = du_step_704 v3
du_step_704 :: T_GCD_638 -> T_GCD_638
du_step_704 v0
  = let v1 = d_commonDivisor_652 (coe v0) in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe
             C_is_662
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
                (coe
                   MAlonzo.Code.Data.Nat.Divisibility.du_'8739'm'8739'n'8658''8739'm'43'n_226
                   (coe v2) (coe v3)))
             (coe du_greatest'8242'_730 (coe v0))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.GCD._.greatest′
d_greatest'8242'_730 ::
  Integer ->
  Integer ->
  Integer ->
  T_GCD_638 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_greatest'8242'_730 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7
  = du_greatest'8242'_730 v3 v6 v7
du_greatest'8242'_730 ::
  T_GCD_638 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_greatest'8242'_730 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> coe
             d_greatest_656 v0 v1
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                (coe
                   MAlonzo.Code.Data.Nat.Divisibility.du_'8739'm'43'n'8739'm'8658''8739'n_238
                   (coe v4) (coe v3)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.gcd-GCD
d_gcd'45'GCD_740 :: Integer -> Integer -> T_GCD_638
d_gcd'45'GCD_740 v0 v1
  = coe
      C_is_662
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe d_gcd'91'm'44'n'93''8739'm_252 (coe v0) (coe v1))
         (coe d_gcd'91'm'44'n'93''8739'n_282 (coe v0) (coe v1)))
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Data.Product.Base.du_uncurry'8242'_296
              (d_gcd'45'greatest_314 (coe v0) (coe v1) (coe v2))))
-- Data.Nat.GCD.mkGCD
d_mkGCD_752 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_mkGCD_752 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe d_gcd_148 (coe v0) (coe v1))
      (coe d_gcd'45'GCD_740 (coe v0) (coe v1))
-- Data.Nat.GCD.gcd?
d_gcd'63'_764 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_gcd'63'_764 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      (coe (\ v3 -> d_gcd'45'GCD_740 (coe v0) (coe v1)))
      (coe
         MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
         (coe d_gcd_148 (coe v0) (coe v1)) (coe v2))
-- Data.Nat.GCD.GCD-*
d_GCD'45''42'_784 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> T_GCD_638 -> T_GCD_638
d_GCD'45''42'_784 ~v0 ~v1 ~v2 v3 ~v4 v5 = du_GCD'45''42'_784 v3 v5
du_GCD'45''42'_784 :: Integer -> T_GCD_638 -> T_GCD_638
du_GCD'45''42'_784 v0 v1
  = case coe v1 of
      C_is_662 v2 v3
        -> coe
             seq (coe v2)
             (coe
                C_is_662 (coe v2)
                (coe
                   (\ v4 v5 ->
                      coe
                        v3 (mulInt (coe v4) (coe v0))
                        (coe
                           MAlonzo.Code.Data.Product.Base.du_map_104 (coe (\ v6 -> v6))
                           (coe (\ v6 v7 -> v7)) (coe v5)))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.GCD-/
d_GCD'45''47'_804 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  T_GCD_638 -> T_GCD_638
d_GCD'45''47'_804 ~v0 ~v1 ~v2 v3 ~v4 v5 v6 v7 v8
  = du_GCD'45''47'_804 v3 v5 v6 v7 v8
du_GCD'45''47'_804 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  T_GCD_638 -> T_GCD_638
du_GCD'45''47'_804 v0 v1 v2 v3 v4
  = coe
      seq (coe v1)
      (coe
         seq (coe v2)
         (coe seq (coe v3) (coe du_GCD'45''42'_784 (coe v0) (coe v4))))
-- Data.Nat.GCD.GCD-/gcd
d_GCD'45''47'gcd_842 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> T_GCD_638
d_GCD'45''47'gcd_842 v0 v1 ~v2 = du_GCD'45''47'gcd_842 v0 v1
du_GCD'45''47'gcd_842 :: Integer -> Integer -> T_GCD_638
du_GCD'45''47'gcd_842 v0 v1
  = coe
      du_GCD'45''47'_804 (coe d_gcd_148 (coe v0) (coe v1))
      (coe d_gcd'91'm'44'n'93''8739'm_252 (coe v0) (coe v1))
      (coe d_gcd'91'm'44'n'93''8739'n_282 (coe v0) (coe v1))
      (coe MAlonzo.Code.Data.Nat.Divisibility.du_'8739''45'refl_94)
      (coe d_gcd'45'GCD_740 (coe v0) (coe v1))
-- Data.Nat.GCD.Bézout.Identity.Identity
d_Identity_862 a0 a1 a2 = ()
data T_Identity_862
  = C_'43''45'_876 Integer Integer | C_'45''43'_884 Integer Integer
-- Data.Nat.GCD.Bézout.Identity.sym
d_sym_888 ::
  Integer -> Integer -> Integer -> T_Identity_862 -> T_Identity_862
d_sym_888 ~v0 ~v1 ~v2 v3 = du_sym_888 v3
du_sym_888 :: T_Identity_862 -> T_Identity_862
du_sym_888 v0
  = case coe v0 of
      C_'43''45'_876 v1 v2 -> coe C_'45''43'_884 v2 v1
      C_'45''43'_884 v1 v2 -> coe C_'43''45'_876 v2 v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.Bézout.Identity.refl
d_refl_904 :: Integer -> T_Identity_862
d_refl_904 ~v0 = du_refl_904
du_refl_904 :: T_Identity_862
du_refl_904 = coe C_'45''43'_884 (0 :: Integer) (1 :: Integer)
-- Data.Nat.GCD.Bézout.Identity.base
d_base_908 :: Integer -> T_Identity_862
d_base_908 ~v0 = du_base_908
du_base_908 :: T_Identity_862
du_base_908 = coe C_'45''43'_884 (0 :: Integer) (1 :: Integer)
-- Data.Nat.GCD.Bézout.Identity._⊕_
d__'8853'__910 :: Integer -> Integer -> Integer
d__'8853'__910 v0 v1
  = coe addInt (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1)
-- Data.Nat.GCD.Bézout.Identity.step
d_step_922 ::
  Integer -> Integer -> Integer -> T_Identity_862 -> T_Identity_862
d_step_922 ~v0 ~v1 ~v2 v3 = du_step_922 v3
du_step_922 :: T_Identity_862 -> T_Identity_862
du_step_922 v0
  = case coe v0 of
      C_'43''45'_876 v1 v2
        -> let v4
                 = MAlonzo.Code.Data.Nat.Base.d_compare_400 (coe v1) (coe v2) in
           case coe v4 of
             MAlonzo.Code.Data.Nat.Base.C_less_384 v6
               -> coe
                    C_'43''45'_876
                    (d__'8853'__910
                       (coe mulInt (coe (2 :: Integer)) (coe v1)) (coe v6))
                    (d__'8853'__910 (coe v1) (coe v6))
             MAlonzo.Code.Data.Nat.Base.C_equal_388
               -> coe C_'43''45'_876 (mulInt (coe (2 :: Integer)) (coe v1)) v1
             MAlonzo.Code.Data.Nat.Base.C_greater_394 v6
               -> coe
                    C_'43''45'_876
                    (d__'8853'__910
                       (coe mulInt (coe (2 :: Integer)) (coe v2)) (coe v6))
                    v2
             _ -> MAlonzo.RTE.mazUnreachableError
      C_'45''43'_884 v1 v2
        -> let v4
                 = MAlonzo.Code.Data.Nat.Base.d_compare_400 (coe v1) (coe v2) in
           case coe v4 of
             MAlonzo.Code.Data.Nat.Base.C_less_384 v6
               -> coe
                    C_'45''43'_884
                    (d__'8853'__910
                       (coe mulInt (coe (2 :: Integer)) (coe v1)) (coe v6))
                    (d__'8853'__910 (coe v1) (coe v6))
             MAlonzo.Code.Data.Nat.Base.C_equal_388
               -> coe C_'45''43'_884 (mulInt (coe (2 :: Integer)) (coe v1)) v1
             MAlonzo.Code.Data.Nat.Base.C_greater_394 v6
               -> coe
                    C_'45''43'_884
                    (d__'8853'__910
                       (coe mulInt (coe (2 :: Integer)) (coe v2)) (coe v6))
                    v2
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.Bézout.Lemma.Lemma
d_Lemma_1038 a0 a1 = ()
data T_Lemma_1038 = C_result_1050 Integer T_GCD_638 T_Identity_862
-- Data.Nat.GCD.Bézout.Lemma.sym
d_sym_1052 :: Integer -> Integer -> T_Lemma_1038 -> T_Lemma_1038
d_sym_1052 ~v0 ~v1 v2 = du_sym_1052 v2
du_sym_1052 :: T_Lemma_1038 -> T_Lemma_1038
du_sym_1052 v0
  = case coe v0 of
      C_result_1050 v1 v2 v3
        -> coe
             C_result_1050 (coe v1) (coe du_sym_684 (coe v2))
             (coe du_sym_888 (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.Bézout.Lemma.base
d_base_1062 :: Integer -> T_Lemma_1038
d_base_1062 v0
  = coe C_result_1050 (coe v0) (coe du_base_694) (coe du_base_908)
-- Data.Nat.GCD.Bézout.Lemma.refl
d_refl_1068 :: Integer -> T_Lemma_1038
d_refl_1068 v0
  = coe C_result_1050 (coe v0) (coe du_refl_690) (coe du_refl_904)
-- Data.Nat.GCD.Bézout.Lemma.stepˡ
d_step'737'_1076 ::
  Integer -> Integer -> T_Lemma_1038 -> T_Lemma_1038
d_step'737'_1076 ~v0 ~v1 v2 = du_step'737'_1076 v2
du_step'737'_1076 :: T_Lemma_1038 -> T_Lemma_1038
du_step'737'_1076 v0
  = case coe v0 of
      C_result_1050 v1 v2 v3
        -> coe
             C_result_1050 (coe v1) (coe du_step_704 (coe v2))
             (coe du_step_922 (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.Bézout.Lemma.stepʳ
d_step'691'_1092 ::
  Integer -> Integer -> T_Lemma_1038 -> T_Lemma_1038
d_step'691'_1092 ~v0 ~v1 v2 = du_step'691'_1092 v2
du_step'691'_1092 :: T_Lemma_1038 -> T_Lemma_1038
du_step'691'_1092 v0
  = coe
      du_sym_1052 (coe du_step'737'_1076 (coe du_sym_1052 (coe v0)))
-- Data.Nat.GCD.Bézout.lemma
d_lemma_1098 :: Integer -> Integer -> T_Lemma_1038
d_lemma_1098 v0 v1
  = coe
      MAlonzo.Code.Induction.du_build_54
      (coe
         MAlonzo.Code.Induction.Lexicographic.du_'91'_'8855'_'93'_166
         (\ v2 v3 v4 ->
            coe MAlonzo.Code.Induction.WellFounded.du_wfRecBuilder_152 v3)
         (\ v2 v3 v4 ->
            coe MAlonzo.Code.Induction.WellFounded.du_wfRecBuilder_152 v3))
      erased (coe du_gcd'8243'_1116)
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0) (coe v1))
-- Data.Nat.GCD.Bézout._.P
d_P_1108 ::
  Integer -> Integer -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d_P_1108 = erased
-- Data.Nat.GCD.Bézout._.gcd″
d_gcd'8243'_1116 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Lemma_1038
d_gcd'8243'_1116 ~v0 ~v1 v2 v3 = du_gcd'8243'_1116 v2 v3
du_gcd'8243'_1116 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Lemma_1038
du_gcd'8243'_1116 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> case coe v2 of
             0 -> coe d_base_1062 (coe v3)
             _ -> let v4 = subInt (coe v2) (coe (1 :: Integer)) in
                  case coe v3 of
                    0 -> coe du_sym_1052 (coe d_base_1062 (coe v2))
                    _ -> let v5 = subInt (coe v3) (coe (1 :: Integer)) in
                         let v6
                               = MAlonzo.Code.Data.Nat.Base.d_compare_400 (coe v4) (coe v5) in
                         case coe v6 of
                           MAlonzo.Code.Data.Nat.Base.C_less_384 v8
                             -> coe
                                  du_step'737'_1076
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 v1
                                     (addInt (coe (1 :: Integer)) (coe v8))
                                     (MAlonzo.Code.Data.Nat.GCD.Lemmas.d_lem'8321'_68
                                        (coe v8) (coe v4)))
                           MAlonzo.Code.Data.Nat.Base.C_equal_388 -> coe d_refl_1068 (coe v2)
                           MAlonzo.Code.Data.Nat.Base.C_greater_394 v8
                             -> coe
                                  du_step'691'_1092
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v1
                                     (addInt (coe (1 :: Integer)) (coe v8))
                                     (MAlonzo.Code.Data.Nat.GCD.Lemmas.d_lem'8321'_68
                                        (coe v8) (coe v5))
                                     v3)
                           _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.GCD.Bézout.identity
d_identity_1170 ::
  Integer -> Integer -> Integer -> T_GCD_638 -> T_Identity_862
d_identity_1170 v0 v1 ~v2 ~v3 = du_identity_1170 v0 v1
du_identity_1170 :: Integer -> Integer -> T_Identity_862
du_identity_1170 v0 v1
  = let v2 = d_lemma_1098 (coe v0) (coe v1) in
    case coe v2 of
      C_result_1050 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
