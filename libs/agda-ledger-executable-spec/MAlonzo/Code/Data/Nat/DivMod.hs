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

module MAlonzo.Code.Data.Nat.DivMod where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Nat
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.DivMod.Core
import qualified MAlonzo.Code.Data.Nat.Divisibility.Core
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Induction.WellFounded
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Core
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.Nat.DivMod.m≡m%n+[m/n]*n
d_m'8801'm'37'n'43''91'm'47'n'93''42'n_76 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8801'm'37'n'43''91'm'47'n'93''42'n_76 = erased
-- Data.Nat.DivMod.m%n≡m∸m/n*n
d_m'37'n'8801'm'8760'm'47'n'42'n_88 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'37'n'8801'm'8760'm'47'n'42'n_88 = erased
-- Data.Nat.DivMod._.m/n*n
d_m'47'n'42'n_98 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_m'47'n'42'n_98 v0 v1 ~v2 = du_m'47'n'42'n_98 v0 v1
du_m'47'n'42'n_98 :: Integer -> Integer -> Integer
du_m'47'n'42'n_98 v0 v1
  = coe
      mulInt
      (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
      (coe v1)
-- Data.Nat.DivMod.%-congˡ
d_'37''45'cong'737'_110 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'37''45'cong'737'_110 = erased
-- Data.Nat.DivMod.%-congʳ
d_'37''45'cong'691'_122 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'37''45'cong'691'_122 = erased
-- Data.Nat.DivMod.n%1≡0
d_n'37'1'8801'0_126 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_n'37'1'8801'0_126 = erased
-- Data.Nat.DivMod.n%n≡0
d_n'37'n'8801'0_132 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_n'37'n'8801'0_132 = erased
-- Data.Nat.DivMod.m%n%n≡m%n
d_m'37'n'37'n'8801'm'37'n_142 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'37'n'37'n'8801'm'37'n_142 = erased
-- Data.Nat.DivMod.[m+n]%n≡m%n
d_'91'm'43'n'93''37'n'8801'm'37'n_154 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'91'm'43'n'93''37'n'8801'm'37'n_154 = erased
-- Data.Nat.DivMod.[m+kn]%n≡m%n
d_'91'm'43'kn'93''37'n'8801'm'37'n_168 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'91'm'43'kn'93''37'n'8801'm'37'n_168 = erased
-- Data.Nat.DivMod.m≤n⇒[n∸m]%m≡n%m
d_m'8804'n'8658''91'n'8760'm'93''37'm'8801'n'37'm_190 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658''91'n'8760'm'93''37'm'8801'n'37'm_190 = erased
-- Data.Nat.DivMod.m*n≤o⇒[o∸m*n]%n≡o%n
d_m'42'n'8804'o'8658''91'o'8760'm'42'n'93''37'n'8801'o'37'n_208 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'42'n'8804'o'8658''91'o'8760'm'42'n'93''37'n'8801'o'37'n_208
  = erased
-- Data.Nat.DivMod.m∣n⇒o%n%m≡o%m
d_m'8739'n'8658'o'37'n'37'm'8801'o'37'm_230 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8739'n'8658'o'37'n'37'm'8801'o'37'm_230 = erased
-- Data.Nat.DivMod._.pm
d_pm_244 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_pm_244 v0 ~v1 ~v2 v3 ~v4 = du_pm_244 v0 v3
du_pm_244 :: Integer -> Integer -> Integer
du_pm_244 v0 v1 = coe mulInt (coe v1) (coe v0)
-- Data.Nat.DivMod._.lem
d_lem_246 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_lem_246 v0 v1 ~v2 v3 ~v4 = du_lem_246 v0 v1 v3
du_lem_246 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_lem_246 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
      (\ v3 v4 v5 ->
         coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v5)
      (coe
         mulInt
         (coe
            mulInt
            (coe
               MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v1)
               (coe du_pm_244 (coe v0) (coe v2)))
            (coe v2))
         (coe v0))
      (coe v1)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
         (\ v3 v4 v5 v6 v7 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v6 v7)
         (coe
            mulInt
            (coe
               MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v1)
               (coe du_pm_244 (coe v0) (coe v2)))
            (coe du_pm_244 (coe v0) (coe v2)))
         (coe
            addInt
            (coe
               MAlonzo.Code.Data.Nat.Base.du__'37'__260 (coe v1)
               (coe du_pm_244 (coe v0) (coe v2)))
            (coe
               mulInt
               (coe
                  MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v1)
                  (coe du_pm_244 (coe v0) (coe v2)))
               (coe du_pm_244 (coe v0) (coe v2))))
         (coe v1)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
            (coe v1))
         (coe
            MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362
            (coe
               mulInt
               (coe
                  MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v1)
                  (coe du_pm_244 (coe v0) (coe v2)))
               (coe du_pm_244 (coe v0) (coe v2)))))
-- Data.Nat.DivMod.m*n%n≡0
d_m'42'n'37'n'8801'0_256 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'42'n'37'n'8801'0_256 = erased
-- Data.Nat.DivMod.m%n<n
d_m'37'n'60'n_268 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'37'n'60'n_268 v0 v1 ~v2 = du_m'37'n'60'n_268 v0 v1
du_m'37'n'60'n_268 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'37'n'60'n_268 v0 v1
  = let v2 = subInt (coe v1) (coe (1 :: Integer)) in
    coe
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
      (MAlonzo.Code.Data.Nat.DivMod.Core.d_a'91'mod'8341''93'n'60'n_70
         (coe (0 :: Integer)) (coe v0) (coe v2))
-- Data.Nat.DivMod.m%n≤n
d_m'37'n'8804'n_280 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'37'n'8804'n_280 v0 v1 ~v2 = du_m'37'n'8804'n_280 v0 v1
du_m'37'n'8804'n_280 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'37'n'8804'n_280 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684
      (coe du_m'37'n'60'n_268 (coe v0) (coe v1))
-- Data.Nat.DivMod.m%n≤m
d_m'37'n'8804'm_292 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'37'n'8804'm_292 v0 v1 ~v2 = du_m'37'n'8804'm_292 v0 v1
du_m'37'n'8804'm_292 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'37'n'8804'm_292 v0 v1
  = let v2 = subInt (coe v1) (coe (1 :: Integer)) in
    coe
      MAlonzo.Code.Data.Nat.DivMod.Core.d_a'91'mod'8341''93'n'8804'a_96
      (coe (0 :: Integer)) (coe v0) (coe v2)
-- Data.Nat.DivMod.m≤n⇒m%n≡m
d_m'8804'n'8658'm'37'n'8801'm_302 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'8804'n'8658'm'37'n'8801'm_302 = erased
-- Data.Nat.DivMod.m<n⇒m%n≡m
d_m'60'n'8658'm'37'n'8801'm_328 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'60'n'8658'm'37'n'8801'm_328 = erased
-- Data.Nat.DivMod.%-pred-≡0
d_'37''45'pred'45''8801'0_342 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'37''45'pred'45''8801'0_342 = erased
-- Data.Nat.DivMod.m<[1+n%d]⇒m≤[n%d]
d_m'60''91'1'43'n'37'd'93''8658'm'8804''91'n'37'd'93'_358 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60''91'1'43'n'37'd'93''8658'm'8804''91'n'37'd'93'_358 ~v0 v1 v2
                                                          ~v3
  = du_m'60''91'1'43'n'37'd'93''8658'm'8804''91'n'37'd'93'_358 v1 v2
du_m'60''91'1'43'n'37'd'93''8658'm'8804''91'n'37'd'93'_358 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60''91'1'43'n'37'd'93''8658'm'8804''91'n'37'd'93'_358 v0 v1
  = case coe v1 of
      0 -> coe (\ v2 -> MAlonzo.RTE.mazUnreachableError)
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.DivMod.Core.du_k'60'1'43'a'91'mod'8341''93'n'8658'k'8804'a'91'mod'8341''93'n_216
             (coe (0 :: Integer)) (coe v0) (coe v2)
-- Data.Nat.DivMod.[1+m%d]≤1+n⇒[m%d]≤n
d_'91'1'43'm'37'd'93''8804'1'43'n'8658''91'm'37'd'93''8804'n_374 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'91'1'43'm'37'd'93''8804'1'43'n'8658''91'm'37'd'93''8804'n_374 v0
                                                                 ~v1 v2 ~v3 ~v4
  = du_'91'1'43'm'37'd'93''8804'1'43'n'8658''91'm'37'd'93''8804'n_374
      v0 v2
du_'91'1'43'm'37'd'93''8804'1'43'n'8658''91'm'37'd'93''8804'n_374 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'91'1'43'm'37'd'93''8804'1'43'n'8658''91'm'37'd'93''8804'n_374 v0
                                                                  v1
  = case coe v1 of
      0 -> coe (\ v2 -> MAlonzo.RTE.mazUnreachableError)
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.DivMod.Core.du_1'43'a'91'mod'8341''93'n'8804'1'43'k'8658'a'91'mod'8341''93'n'8804'k_260
             (coe (0 :: Integer)) (coe v0) (coe v2)
-- Data.Nat.DivMod.%-distribˡ-+
d_'37''45'distrib'737''45''43'_392 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'37''45'distrib'737''45''43'_392 = erased
-- Data.Nat.DivMod.%-distribˡ-*
d_'37''45'distrib'737''45''42'_422 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'37''45'distrib'737''45''42'_422 = erased
-- Data.Nat.DivMod._.m′
d_m'8242'_436 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_m'8242'_436 v0 ~v1 v2 ~v3 = du_m'8242'_436 v0 v2
du_m'8242'_436 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
du_m'8242'_436 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Nat.Base.du__'37'__260 (coe v0)
      (coe addInt (coe (1 :: Integer)) (coe v1))
-- Data.Nat.DivMod._.n′
d_n'8242'_438 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_n'8242'_438 ~v0 v1 v2 ~v3 = du_n'8242'_438 v1 v2
du_n'8242'_438 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
du_n'8242'_438 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Nat.Base.du__'37'__260 (coe v0)
      (coe addInt (coe (1 :: Integer)) (coe v1))
-- Data.Nat.DivMod._.k
d_k_440 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_k_440 v0 ~v1 v2 ~v3 = du_k_440 v0 v2
du_k_440 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
du_k_440 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0)
      (coe addInt (coe (1 :: Integer)) (coe v1))
-- Data.Nat.DivMod._.j
d_j_442 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_j_442 ~v0 v1 v2 ~v3 = du_j_442 v1 v2
du_j_442 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
du_j_442 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0)
      (coe addInt (coe (1 :: Integer)) (coe v1))
-- Data.Nat.DivMod._.lemma
d_lemma_444 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lemma_444 = erased
-- Data.Nat.DivMod.%-remove-+ˡ
d_'37''45'remove'45''43''737'_464 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'37''45'remove'45''43''737'_464 = erased
-- Data.Nat.DivMod.%-remove-+ʳ
d_'37''45'remove'45''43''691'_486 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'37''45'remove'45''43''691'_486 = erased
-- Data.Nat.DivMod./-congˡ
d_'47''45'cong'737'_506 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''45'cong'737'_506 = erased
-- Data.Nat.DivMod./-congʳ
d_'47''45'cong'691'_518 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''45'cong'691'_518 = erased
-- Data.Nat.DivMod.0/n≡0
d_0'47'n'8801'0_524 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_0'47'n'8801'0_524 = erased
-- Data.Nat.DivMod.n/1≡n
d_n'47'1'8801'n_530 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_n'47'1'8801'n_530 = erased
-- Data.Nat.DivMod.n/n≡1
d_n'47'n'8801'1_538 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_n'47'n'8801'1_538 = erased
-- Data.Nat.DivMod.m*n/n≡m
d_m'42'n'47'n'8801'm_548 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'42'n'47'n'8801'm_548 = erased
-- Data.Nat.DivMod.m/n*n≡m
d_m'47'n'42'n'8801'm_560 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'47'n'42'n'8801'm_560 = erased
-- Data.Nat.DivMod.m*[n/m]≡n
d_m'42''91'n'47'm'93''8801'n_576 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'42''91'n'47'm'93''8801'n_576 = erased
-- Data.Nat.DivMod.m/n*n≤m
d_m'47'n'42'n'8804'm_588 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'47'n'42'n'8804'm_588 v0 v1 ~v2
  = du_m'47'n'42'n'8804'm_588 v0 v1
du_m'47'n'42'n'8804'm_588 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'47'n'42'n'8804'm_588 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
      (\ v2 v3 v4 ->
         coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v4)
      (coe
         mulInt
         (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
         (coe v1))
      (coe v0)
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
         (\ v2 v3 v4 v5 v6 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v5 v6)
         (coe
            mulInt
            (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
            (coe v1))
         (coe
            addInt
            (coe MAlonzo.Code.Data.Nat.Base.du__'37'__260 (coe v0) (coe v1))
            (coe
               mulInt
               (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
               (coe v1)))
         (coe v0)
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
            (coe v0))
         (coe
            MAlonzo.Code.Data.Nat.Properties.du_m'8804'm'43'n_3362
            (coe
               mulInt
               (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
               (coe v1))))
-- Data.Nat.DivMod.m/n≤m
d_m'47'n'8804'm_602 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'47'n'8804'm_602 v0 v1 ~v2 = du_m'47'n'8804'm_602 v0 v1
du_m'47'n'8804'm_602 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'47'n'8804'm_602 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.du_'42''45'cancel'691''45''8804'_3858
      (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
-- Data.Nat.DivMod.m/n<m
d_m'47'n'60'm_616 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'47'n'60'm_616 v0 v1 ~v2 ~v3 ~v4 = du_m'47'n'60'm_616 v0 v1
du_m'47'n'60'm_616 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'47'n'60'm_616 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.du_'42''45'cancel'691''45''60'_4032
      (coe v1)
      (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
      (coe v0)
-- Data.Nat.DivMod./-mono-≤
d_'47''45'mono'45''8804'_636 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'47''45'mono'45''8804'_636 v0 v1 v2 v3 ~v4 ~v5 v6 v7
  = du_'47''45'mono'45''8804'_636 v0 v1 v2 v3 v6 v7
du_'47''45'mono'45''8804'_636 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'47''45'mono'45''8804'_636 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8
        -> let v9 = subInt (coe v2) (coe (1 :: Integer)) in
           let v10 = subInt (coe v3) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Nat.DivMod.Core.d_div'8341''45'mono'45''8804'_886
             (coe (0 :: Integer)) (coe (0 :: Integer)) (coe v0) (coe v1)
             (coe v9) (coe v10) (coe v4) (coe v8)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.DivMod./-monoˡ-≤
d_'47''45'mono'737''45''8804'_650 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'47''45'mono'737''45''8804'_650 v0 v1 v2 ~v3 v4
  = du_'47''45'mono'737''45''8804'_650 v0 v1 v2 v4
du_'47''45'mono'737''45''8804'_650 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'47''45'mono'737''45''8804'_650 v0 v1 v2 v3
  = coe
      du_'47''45'mono'45''8804'_636 (coe v0) (coe v1) (coe v2) (coe v2)
      (coe v3)
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570 (coe v2))
-- Data.Nat.DivMod./-monoʳ-≤
d_'47''45'mono'691''45''8804'_666 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'47''45'mono'691''45''8804'_666 v0 v1 v2 ~v3 ~v4 v5
  = du_'47''45'mono'691''45''8804'_666 v0 v1 v2 v5
du_'47''45'mono'691''45''8804'_666 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_'47''45'mono'691''45''8804'_666 v0 v1 v2 v3
  = coe
      du_'47''45'mono'45''8804'_636 (coe v0) (coe v0) (coe v1) (coe v2)
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'8804''45'refl_2570 (coe v0))
      (coe v3)
-- Data.Nat.DivMod./-cancelʳ-≡
d_'47''45'cancel'691''45''8801'_680 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''45'cancel'691''45''8801'_680 = erased
-- Data.Nat.DivMod.m<n⇒m/n≡0
d_m'60'n'8658'm'47'n'8801'0_702 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'60'n'8658'm'47'n'8801'0_702 = erased
-- Data.Nat.DivMod.m≥n⇒m/n>0
d_m'8805'n'8658'm'47'n'62'0_716 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'8805'n'8658'm'47'n'62'0_716 v0 v1 ~v2 v3
  = du_m'8805'n'8658'm'47'n'62'0_716 v0 v1 v3
du_m'8805'n'8658'm'47'n'62'0_716 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'8805'n'8658'm'47'n'62'0_716 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
      (\ v3 v4 v5 ->
         coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v5)
      (coe (1 :: Integer))
      (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
         (\ v3 v4 v5 v6 v7 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v6 v7)
         (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v0))
         (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
         (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
            (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1)))
         (coe
            du_'47''45'mono'691''45''8804'_666 (coe v0) (coe v0) (coe v1)
            (coe v2)))
-- Data.Nat.DivMod.+-distrib-/
d_'43''45'distrib'45''47'_732 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'distrib'45''47'_732 = erased
-- Data.Nat.DivMod.+-distrib-/-∣ˡ
d_'43''45'distrib'45''47''45''8739''737'_750 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'distrib'45''47''45''8739''737'_750 = erased
-- Data.Nat.DivMod.+-distrib-/-∣ʳ
d_'43''45'distrib'45''47''45''8739''691'_770 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'43''45'distrib'45''47''45''8739''691'_770 = erased
-- Data.Nat.DivMod.m/n≡1+[m∸n]/n
d_m'47'n'8801'1'43''91'm'8760'n'93''47'n_788 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'47'n'8801'1'43''91'm'8760'n'93''47'n_788 = erased
-- Data.Nat.DivMod.m*n/m*o≡n/o
d_m'42'n'47'm'42'o'8801'n'47'o_810 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'42'n'47'm'42'o'8801'n'47'o_810 = erased
-- Data.Nat.DivMod._.helper
d_helper_826 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  Integer ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_helper_826 = erased
-- Data.Nat.DivMod._._.n∸o<n
d_n'8760'o'60'n_852 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  (Integer ->
   MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Induction.WellFounded.T_Acc_42) ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_n'8760'o'60'n_852 v0 v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
  = du_n'8760'o'60'n_852 v0 v1
du_n'8760'o'60'n_852 ::
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_n'8760'o'60'n_852 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.d_'8760''45'mono'691''45''60'_4960
      (coe v1) (coe v0) (coe (0 :: Integer))
      (coe
         MAlonzo.Code.Data.Nat.Properties.du_n'8802'0'8658'n'62'0_2944
         (coe v0))
      (coe
         MAlonzo.Code.Data.Nat.Properties.du_'8814''8658''8805'_2732
         (coe v1) (coe v0))
-- Data.Nat.DivMod.m*n/o*n≡m/o
d_m'42'n'47'o'42'n'8801'm'47'o_868 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'42'n'47'o'42'n'8801'm'47'o_868 = erased
-- Data.Nat.DivMod.m<n*o⇒m/o<n
d_m'60'n'42'o'8658'm'47'o'60'n_892 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_m'60'n'42'o'8658'm'47'o'60'n_892 v0 v1 v2 ~v3 ~v4
  = du_m'60'n'42'o'8658'm'47'o'60'n_892 v0 v1 v2
du_m'60'n'42'o'8658'm'47'o'60'n_892 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_m'60'n'42'o'8658'm'47'o'60'n_892 v0 v1 v2
  = let v3 = subInt (coe v1) (coe (1 :: Integer)) in
    let v4
          = MAlonzo.Code.Data.Nat.Properties.d__'60''63'__2860
              (coe v0) (coe v2) in
    case coe v4 of
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v5 v6
        -> if coe v5
             then coe
                    seq (coe v6)
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                          (\ v7 v8 v9 v10 v11 ->
                             coe
                               MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans_2810 v8 v10 v11)
                          (coe
                             MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                          (\ v7 v8 v9 v10 v11 ->
                             coe
                               MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'737'_2822 v10
                               v11)
                          (coe (0 :: Integer)) (coe v1) (coe v1)
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                             (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                             (coe v1))
                          (coe
                             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                             (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))))
             else coe
                    seq (coe v6)
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
                          (\ v7 v8 v9 v10 v11 ->
                             coe
                               MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans_2810 v8 v10 v11)
                          (coe
                             MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
                          (\ v7 v8 v9 v10 v11 ->
                             coe
                               MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'737'_2822 v10
                               v11)
                          (coe
                             addInt (coe (1 :: Integer))
                             (coe
                                MAlonzo.Code.Data.Nat.Base.du__'47'__248
                                (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v0 v2) (coe v2)))
                          (coe v1) (coe v1)
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
                             (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
                             (coe v1))
                          (coe
                             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                             (coe
                                du_m'60'n'42'o'8658'm'47'o'60'n_892
                                (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v0 v2) (coe v3)
                                (coe v2)))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.DivMod._.lem
d_lem_930 ::
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_lem_930 v0 v1 ~v2 v3 ~v4 v5 = du_lem_930 v0 v1 v3 v5
du_lem_930 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_lem_930 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
         (\ v4 v5 v6 v7 v8 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans_2810 v5 v7 v8)
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
         (\ v4 v5 v6 v7 v8 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'737'_2822 v7 v8)
         (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v0 v1)
         (coe
            MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
            (addInt (coe mulInt (coe v2) (coe v1)) (coe v1)) v1)
         (coe mulInt (coe v2) (coe v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
            (coe mulInt (coe v2) (coe v1)))
         (coe
            MAlonzo.Code.Data.Nat.Properties.du_'8760''45'mono'737''45''60'_4934
            (coe v1) (coe v3)
            (coe
               MAlonzo.Code.Data.Nat.Properties.du_'8814''8658''8805'_2732
               (coe v0) (coe v1))))
-- Data.Nat.DivMod.[m∸n]/n≡m/n∸1
d_'91'm'8760'n'93''47'n'8801'm'47'n'8760'1_938 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'91'm'8760'n'93''47'n'8801'm'47'n'8760'1_938 = erased
-- Data.Nat.DivMod.[m∸n*o]/o≡m/o∸n
d_'91'm'8760'n'42'o'93''47'o'8801'm'47'o'8760'n_972 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'91'm'8760'n'42'o'93''47'o'8801'm'47'o'8760'n_972 = erased
-- Data.Nat.DivMod.m/n/o≡m/[n*o]
d_m'47'n'47'o'8801'm'47''91'n'42'o'93'_998 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'47'n'47'o'8801'm'47''91'n'42'o'93'_998 = erased
-- Data.Nat.DivMod._.n*o
d_n'42'o_1010 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_n'42'o_1010 ~v0 v1 v2 ~v3 ~v4 ~v5 = du_n'42'o_1010 v1 v2
du_n'42'o_1010 :: Integer -> Integer -> Integer
du_n'42'o_1010 v0 v1 = coe mulInt (coe v0) (coe v1)
-- Data.Nat.DivMod._.o*n
d_o'42'n_1012 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_o'42'n_1012 ~v0 v1 v2 ~v3 ~v4 ~v5 = du_o'42'n_1012 v1 v2
du_o'42'n_1012 :: Integer -> Integer -> Integer
du_o'42'n_1012 v0 v1 = coe mulInt (coe v1) (coe v0)
-- Data.Nat.DivMod._.lem₁
d_lem'8321'_1014 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
d_lem'8321'_1014 v0 v1 v2 ~v3 ~v4 ~v5 = du_lem'8321'_1014 v0 v1 v2
du_lem'8321'_1014 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12
du_lem'8321'_1014 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Nat.Divisibility.Core.C_divides_26
      (mulInt
         (coe
            MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0)
            (coe du_n'42'o_1010 (coe v1) (coe v2)))
         (coe v2))
-- Data.Nat.DivMod._.lem₂
d_lem'8322'_1018 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_lem'8322'_1018 = erased
-- Data.Nat.DivMod._.lem₃
d_lem'8323'_1022 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_lem'8323'_1022 v0 v1 v2 ~v3 ~v4 ~v5 = du_lem'8323'_1022 v0 v1 v2
du_lem'8323'_1022 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_lem'8323'_1022 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
         (\ v3 v4 v5 v6 v7 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans_2810 v4 v6 v7)
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
         (\ v3 v4 v5 v6 v7 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'737'_2822 v6 v7)
         (coe
            MAlonzo.Code.Data.Nat.Base.du__'37'__260 (coe v0)
            (coe du_n'42'o_1010 (coe v1) (coe v2)))
         (coe du_n'42'o_1010 (coe v1) (coe v2))
         (coe du_o'42'n_1012 (coe v1) (coe v2))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
            (coe du_o'42'n_1012 (coe v1) (coe v2)))
         (coe
            du_m'37'n'60'n_268 (coe v0)
            (coe du_n'42'o_1010 (coe v1) (coe v2))))
-- Data.Nat.DivMod.*-/-assoc
d_'42''45''47''45'assoc_1038 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'42''45''47''45'assoc_1038 = erased
-- Data.Nat.DivMod./-*-interchange
d_'47''45''42''45'interchange_1068 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Data.Nat.Divisibility.Core.T__'8739'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'47''45''42''45'interchange_1068 = erased
-- Data.Nat.DivMod.m*n/m!≡n/[m∸1]!
d_m'42'n'47'm'33''8801'n'47''91'm'8760'1'93''33'_1088 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'42'n'47'm'33''8801'n'47''91'm'8760'1'93''33'_1088 = erased
-- Data.Nat.DivMod.m%[n*o]/o≡m/o%n
d_m'37''91'n'42'o'93''47'o'8801'm'47'o'37'n_1106 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'37''91'n'42'o'93''47'o'8801'm'47'o'37'n_1106 = erased
-- Data.Nat.DivMod.m%n*o≡m*o%[n*o]
d_m'37'n'42'o'8801'm'42'o'37''91'n'42'o'93'_1138 ::
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_m'37'n'42'o'8801'm'42'o'37''91'n'42'o'93'_1138 = erased
-- Data.Nat.DivMod.[m*n+o]%[p*n]≡[m*n]%[p*n]+o
d_'91'm'42'n'43'o'93''37''91'p'42'n'93''8801''91'm'42'n'93''37''91'p'42'n'93''43'o_1164 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'91'm'42'n'43'o'93''37''91'p'42'n'93''8801''91'm'42'n'93''37''91'p'42'n'93''43'o_1164
  = erased
-- Data.Nat.DivMod._.mn
d_mn_1184 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> Integer
d_mn_1184 v0 v1 ~v2 ~v3 ~v4 ~v5 = du_mn_1184 v0 v1
du_mn_1184 :: Integer -> Integer -> Integer
du_mn_1184 v0 v1 = coe mulInt (coe v0) (coe v1)
-- Data.Nat.DivMod._.pn
d_pn_1186 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> Integer
d_pn_1186 ~v0 v1 ~v2 v3 ~v4 ~v5 = du_pn_1186 v1 v3
du_pn_1186 :: Integer -> Integer -> Integer
du_pn_1186 v0 v1
  = coe mulInt (coe addInt (coe (1 :: Integer)) (coe v1)) (coe v0)
-- Data.Nat.DivMod._.lem₁
d_lem'8321'_1188 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_lem'8321'_1188 v0 v1 ~v2 v3 ~v4 ~v5 = du_lem'8321'_1188 v0 v1 v3
du_lem'8321'_1188 ::
  Integer ->
  Integer -> Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_lem'8321'_1188 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin__160
      (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
      (\ v3 v4 v5 ->
         coe MAlonzo.Code.Data.Nat.Properties.du_'60''8658''8804'_2684 v5)
      (coe
         MAlonzo.Code.Data.Nat.Base.du__'37'__260
         (coe du_mn_1184 (coe v0) (coe v1))
         (coe du_pn_1186 (coe v1) (coe v2)))
      (coe mulInt (coe v2) (coe v1))
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''8804'_228
         (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
         (\ v3 v4 v5 v6 v7 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'691'_2816 v6 v7)
         (coe
            mulInt
            (coe
               MAlonzo.Code.Data.Nat.Base.du__'37'__260 (coe v0)
               (coe addInt (coe (1 :: Integer)) (coe v2)))
            (coe v1))
         (coe mulInt (coe v2) (coe v1)) (coe mulInt (coe v2) (coe v1))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
            (coe mulInt (coe v2) (coe v1)))
         (coe
            MAlonzo.Code.Data.Nat.Properties.du_'42''45'mono'737''45''8804'_3904
            (coe v1) (coe v2)
            (coe
               MAlonzo.Code.Data.Nat.Properties.du_m'60'1'43'n'8658'm'8804'n_3016
               (coe
                  du_m'37'n'60'n_268 (coe v0)
                  (coe addInt (coe (1 :: Integer)) (coe v2))))))
-- Data.Nat.DivMod._.lem₂
d_lem'8322'_1190 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_lem'8322'_1190 v0 v1 v2 v3 ~v4 v5
  = du_lem'8322'_1190 v0 v1 v2 v3 v5
du_lem'8322'_1190 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_lem'8322'_1190 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_begin'45'strict__176
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du_step'45''60'_202
         (\ v5 v6 v7 v8 v9 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans_2810 v6 v8 v9)
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Core.du_resp'8322'_144)
         (\ v5 v6 v7 v8 v9 ->
            coe
              MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans'737'_2822 v8 v9)
         (coe
            addInt
            (coe
               MAlonzo.Code.Data.Nat.Base.du__'37'__260
               (coe du_mn_1184 (coe v0) (coe v1))
               (coe du_pn_1186 (coe v1) (coe v3)))
            (coe v2))
         (coe addInt (coe mulInt (coe v3) (coe v1)) (coe v1))
         (coe du_pn_1186 (coe v1) (coe v3))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Triple.du__'8718'_346
            (coe MAlonzo.Code.Data.Nat.Properties.d_'8804''45'isPreorder_2620)
            (coe du_pn_1186 (coe v1) (coe v3)))
         (coe
            MAlonzo.Code.Data.Nat.Properties.du_'43''45'mono'45''8804''45''60'_3450
            (coe v1) (coe du_lem'8321'_1188 (coe v0) (coe v1) (coe v3))
            (coe v4)))
-- Data.Nat.DivMod.DivMod
d_DivMod_1198 a0 a1 = ()
data T_DivMod_1198
  = C_result_1216 Integer MAlonzo.Code.Data.Fin.Base.T_Fin_10
-- Data.Nat.DivMod.DivMod.quotient
d_quotient_1210 :: T_DivMod_1198 -> Integer
d_quotient_1210 v0
  = case coe v0 of
      C_result_1216 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.DivMod.DivMod.remainder
d_remainder_1212 ::
  T_DivMod_1198 -> MAlonzo.Code.Data.Fin.Base.T_Fin_10
d_remainder_1212 v0
  = case coe v0 of
      C_result_1216 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Nat.DivMod.DivMod.property
d_property_1214 ::
  T_DivMod_1198 -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_property_1214 = erased
-- Data.Nat.DivMod._div_
d__div__1224 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d__div__1224 v0 v1 v2
  = coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 v0 v1
-- Data.Nat.DivMod._mod_
d__mod__1232 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10
d__mod__1232 v0 v1 ~v2 = du__mod__1232 v0 v1
du__mod__1232 ::
  Integer -> Integer -> MAlonzo.Code.Data.Fin.Base.T_Fin_10
du__mod__1232 v0 v1
  = coe
      MAlonzo.Code.Data.Fin.Base.du_fromℕ'60'_52
      (coe remInt (coe v0) (coe v1))
      (coe du_m'37'n'60'n_268 (coe v0) (coe v1))
-- Data.Nat.DivMod._divMod_
d__divMod__1244 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> T_DivMod_1198
d__divMod__1244 v0 v1 ~v2 = du__divMod__1244 v0 v1
du__divMod__1244 :: Integer -> Integer -> T_DivMod_1198
du__divMod__1244 v0 v1
  = coe
      C_result_1216
      (coe MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0) (coe v1))
      (coe du__mod__1232 (coe v0) (coe v1))
-- Data.Nat.DivMod._.[m/n]*n
d_'91'm'47'n'93''42'n_1256 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88 -> Integer
d_'91'm'47'n'93''42'n_1256 v0 v1 ~v2
  = du_'91'm'47'n'93''42'n_1256 v0 v1
du_'91'm'47'n'93''42'n_1256 :: Integer -> Integer -> Integer
du_'91'm'47'n'93''42'n_1256 v0 v1
  = coe
      mulInt
      (coe
         MAlonzo.Code.Data.Nat.Base.du__'47'__248 (coe v0)
         (coe addInt (coe (1 :: Integer)) (coe v1)))
      (coe addInt (coe (1 :: Integer)) (coe v1))
