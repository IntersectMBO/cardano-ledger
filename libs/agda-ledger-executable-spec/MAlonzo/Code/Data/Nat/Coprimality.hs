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

module MAlonzo.Code.Data.Nat.Coprimality where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Nat
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Divisibility
import qualified MAlonzo.Code.Data.Nat.Divisibility.Core
import qualified MAlonzo.Code.Data.Nat.GCD
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Data.Nat.Coprimality.Coprime
d_Coprime_6 :: Integer -> Integer -> ()
d_Coprime_6 = erased
-- Data.Nat.Coprimality.coprime⇒GCD≡1
d_coprime'8658'GCD'8801'1_18 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Data.Nat.GCD.T_GCD_638
d_coprime'8658'GCD'8801'1_18 v0 v1 ~v2
  = du_coprime'8658'GCD'8801'1_18 v0 v1
du_coprime'8658'GCD'8801'1_18 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.GCD.T_GCD_638
du_coprime'8658'GCD'8801'1_18 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.GCD.C_is_662
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe MAlonzo.Code.Data.Nat.Divisibility.d_1'8739'__188 (coe v0))
         (coe MAlonzo.Code.Data.Nat.Divisibility.d_1'8739'__188 (coe v1)))
      (coe
         (\ v2 v3 ->
            coe MAlonzo.Code.Data.Nat.Divisibility.du_'8739''45'reflexive_90))
-- Data.Nat.Coprimality.GCD≡1⇒coprime
d_GCD'8801'1'8658'coprime_30 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.GCD.T_GCD_638 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_GCD'8801'1'8658'coprime_30 = erased
-- Data.Nat.Coprimality.coprime⇒gcd≡1
d_coprime'8658'gcd'8801'1_52 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_coprime'8658'gcd'8801'1_52 = erased
-- Data.Nat.Coprimality.gcd≡1⇒coprime
d_gcd'8801'1'8658'coprime_60 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_gcd'8801'1'8658'coprime_60 = erased
-- Data.Nat.Coprimality.coprime-/gcd
d_coprime'45''47'gcd_70 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_coprime'45''47'gcd_70 = erased
-- Data.Nat.Coprimality.sym
d_sym_76 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sym_76 = erased
-- Data.Nat.Coprimality.0≢1
d_0'8802'1_80 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_0'8802'1_80 = erased
-- Data.Nat.Coprimality.2+≢1
d_2'43''8802'1_84 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_2'43''8802'1_84 = erased
-- Data.Nat.Coprimality.coprime?
d_coprime'63'_86 ::
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_coprime'63'_86 v0 v1
  = let v2
          = MAlonzo.Code.Data.Nat.GCD.d_mkGCD_752 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> case coe v3 of
             0 -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
             1 -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
             _ -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                    (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Coprimality.1-coprimeTo
d_1'45'coprimeTo_118 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_1'45'coprimeTo_118 = erased
-- Data.Nat.Coprimality.0-coprimeTo-m⇒m≡1
d_0'45'coprimeTo'45'm'8658'm'8801'1_124 ::
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_0'45'coprimeTo'45'm'8658'm'8801'1_124 = erased
-- Data.Nat.Coprimality.¬0-coprimeTo-2+
d_'172'0'45'coprimeTo'45'2'43'_132 ::
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'172'0'45'coprimeTo'45'2'43'_132 = erased
-- Data.Nat.Coprimality.coprime-+
d_coprime'45''43'_140 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_coprime'45''43'_140 = erased
-- Data.Nat.Coprimality.recompute
d_recompute_152 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_recompute_152 = erased
-- Data.Nat.Coprimality.Bézout-coprime
d_Bézout'45'coprime_168 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.GCD.T_Identity_862 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_Bézout'45'coprime_168 = erased
-- Data.Nat.Coprimality.coprime-Bézout
d_coprime'45'Bézout_194 ::
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Data.Nat.GCD.T_Identity_862
d_coprime'45'Bézout_194 v0 v1 ~v2 = du_coprime'45'Bézout_194 v0 v1
du_coprime'45'Bézout_194 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.GCD.T_Identity_862
du_coprime'45'Bézout_194 v0 v1
  = coe MAlonzo.Code.Data.Nat.GCD.du_identity_1170 (coe v0) (coe v1)
-- Data.Nat.Coprimality.coprime-divisor
d_coprime'45'divisor_202 ::
  Integer ->
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_coprime'45'divisor_202 v0 v1 v2 ~v3 v4
  = du_coprime'45'divisor_202 v0 v1 v2 v4
du_coprime'45'divisor_202 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_coprime'45'divisor_202 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26 v4
        -> let v6 = coe du_coprime'45'Bézout_194 (coe v1) (coe v2) in
           case coe v6 of
             MAlonzo.Code.Data.Nat.GCD.C_'43''45'_876 v7 v8
               -> coe
                    MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26
                    (coe
                       MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                       (mulInt (coe v7) (coe v0)) (mulInt (coe v8) (coe v4)))
             MAlonzo.Code.Data.Nat.GCD.C_'45''43'_884 v7 v8
               -> coe
                    MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26
                    (coe
                       MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                       (mulInt (coe v8) (coe v4)) (mulInt (coe v7) (coe v0)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Coprimality.coprime-factors
d_coprime'45'factors_252 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  (Integer ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_coprime'45'factors_252 ~v0 v1 v2 ~v3 ~v4 v5
  = du_coprime'45'factors_252 v1 v2 v5
du_coprime'45'factors_252 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_coprime'45'factors_252 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> case coe v3 of
             MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26 v5
               -> case coe v4 of
                    MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26 v7
                      -> let v9 = coe du_coprime'45'Bézout_194 (coe v0) (coe v1) in
                         case coe v9 of
                           MAlonzo.Code.Data.Nat.GCD.C_'43''45'_876 v10 v11
                             -> coe
                                  MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                                     (mulInt (coe v10) (coe v5)) (mulInt (coe v11) (coe v7)))
                           MAlonzo.Code.Data.Nat.GCD.C_'45''43'_884 v10 v11
                             -> coe
                                  MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                                     (mulInt (coe v11) (coe v7)) (mulInt (coe v10) (coe v5)))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.Coprimality.prime⇒coprime
d_prime'8658'coprime_304 ::
  Integer ->
  AgdaAny ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_prime'8658'coprime_304 = erased
-- Data.Nat.Coprimality._.2≤d
d_2'8804'd_322 ::
  Integer ->
  (Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_2'8804'd_322 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 = du_2'8804'd_322
du_2'8804'd_322 :: MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_2'8804'd_322
  = coe
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
      (coe
         MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
         (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
-- Data.Nat.Coprimality._.d<m
d_d'60'm_324 ::
  Integer ->
  (Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_d'60'm_324 ~v0 ~v1 v2 ~v3 v4 v5 ~v6 v7
  = du_d'60'm_324 v2 v4 v5 v7
du_d'60'm_324 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  Integer ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_d'60'm_324 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'737'_2822
      (coe
         MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
         (coe
            MAlonzo.Code.Data.Nat.Divisibility.du_'8739''8658''8804'_64
            (coe addInt (coe (2 :: Integer)) (coe v2))
            (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v3)))
      (coe v1)
