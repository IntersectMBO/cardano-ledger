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

module MAlonzo.Code.Data.Fin.Base where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Nat
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Empty
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base

-- Data.Fin.Base.Fin
d_Fin_10 a0 = ()
data T_Fin_10 = C_zero_12 | C_suc_16 T_Fin_10
-- Data.Fin.Base.toℕ
d_toℕ_18 :: Integer -> T_Fin_10 -> Integer
d_toℕ_18 ~v0 v1 = du_toℕ_18 v1
du_toℕ_18 :: T_Fin_10 -> Integer
du_toℕ_18 v0
  = case coe v0 of
      C_zero_12 -> coe (0 :: Integer)
      C_suc_16 v2
        -> coe addInt (coe (1 :: Integer)) (coe du_toℕ_18 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.Fin′
d_Fin'8242'_22 :: Integer -> T_Fin_10 -> ()
d_Fin'8242'_22 = erased
-- Data.Fin.Base.cast
d_cast_26 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T_Fin_10 -> T_Fin_10
d_cast_26 v0 ~v1 ~v2 v3 = du_cast_26 v0 v3
du_cast_26 :: Integer -> T_Fin_10 -> T_Fin_10
du_cast_26 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> case coe v1 of
             C_zero_12 -> coe C_zero_12
             C_suc_16 v3
               -> coe
                    C_suc_16
                    (coe
                       du_cast_26
                       (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v0 (1 :: Integer))
                       (coe v3))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.fromℕ
d_fromℕ_48 :: Integer -> T_Fin_10
d_fromℕ_48 v0
  = case coe v0 of
      0 -> coe C_zero_12
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe C_suc_16 (d_fromℕ_48 (coe v1))
-- Data.Fin.Base.fromℕ<
d_fromℕ'60'_52 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> T_Fin_10
d_fromℕ'60'_52 v0 ~v1 v2 = du_fromℕ'60'_52 v0 v2
du_fromℕ'60'_52 ::
  Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> T_Fin_10
du_fromℕ'60'_52 v0 v1
  = case coe v0 of
      0 -> case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v4
               -> coe seq (coe v4) (coe C_zero_12)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v5
               -> coe C_suc_16 (coe du_fromℕ'60'_52 (coe v2) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.fromℕ<″
d_fromℕ'60''8243'_66 ::
  Integer ->
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 -> T_Fin_10
d_fromℕ'60''8243'_66 v0 ~v1 v2 = du_fromℕ'60''8243'_66 v0 v2
du_fromℕ'60''8243'_66 ::
  Integer ->
  MAlonzo.Code.Data.Nat.Base.T__'8804''8243'__314 -> T_Fin_10
du_fromℕ'60''8243'_66 v0 v1
  = case coe v0 of
      0 -> coe seq (coe v1) (coe C_zero_12)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328 v3
               -> coe
                    C_suc_16
                    (coe
                       du_fromℕ'60''8243'_66 (coe v2)
                       (coe
                          MAlonzo.Code.Data.Nat.Base.C_less'45'than'45'or'45'equal_328 v3))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base._↑ˡ_
d__'8593''737'__74 :: Integer -> T_Fin_10 -> Integer -> T_Fin_10
d__'8593''737'__74 ~v0 v1 ~v2 = du__'8593''737'__74 v1
du__'8593''737'__74 :: T_Fin_10 -> T_Fin_10
du__'8593''737'__74 v0 = coe v0
-- Data.Fin.Base._↑ʳ_
d__'8593''691'__86 :: Integer -> Integer -> T_Fin_10 -> T_Fin_10
d__'8593''691'__86 ~v0 v1 v2 = du__'8593''691'__86 v1 v2
du__'8593''691'__86 :: Integer -> T_Fin_10 -> T_Fin_10
du__'8593''691'__86 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe C_suc_16 (coe du__'8593''691'__86 (coe v2) (coe v1))
-- Data.Fin.Base.reduce≥
d_reduce'8805'_98 ::
  Integer ->
  Integer ->
  T_Fin_10 -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> T_Fin_10
d_reduce'8805'_98 v0 ~v1 v2 v3 = du_reduce'8805'_98 v0 v2 v3
du_reduce'8805'_98 ::
  Integer ->
  T_Fin_10 -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> T_Fin_10
du_reduce'8805'_98 v0 v1 v2
  = case coe v0 of
      0 -> coe v1
      _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             C_suc_16 v5
               -> case coe v2 of
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v8
                      -> coe du_reduce'8805'_98 (coe v3) (coe v5) (coe v8)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.inject
d_inject_112 :: Integer -> T_Fin_10 -> T_Fin_10 -> T_Fin_10
d_inject_112 ~v0 v1 v2 = du_inject_112 v1 v2
du_inject_112 :: T_Fin_10 -> T_Fin_10 -> T_Fin_10
du_inject_112 v0 v1
  = case coe v0 of
      C_suc_16 v3
        -> case coe v1 of
             C_zero_12 -> coe C_zero_12
             C_suc_16 v5 -> coe C_suc_16 (coe du_inject_112 (coe v3) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.inject!
d_inject'33'_122 :: Integer -> T_Fin_10 -> T_Fin_10 -> T_Fin_10
d_inject'33'_122 ~v0 v1 v2 = du_inject'33'_122 v1 v2
du_inject'33'_122 :: T_Fin_10 -> T_Fin_10 -> T_Fin_10
du_inject'33'_122 v0 v1
  = case coe v0 of
      C_suc_16 v3
        -> case coe v1 of
             C_zero_12 -> coe C_zero_12
             C_suc_16 v5
               -> coe C_suc_16 (coe du_inject'33'_122 (coe v3) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.inject₁
d_inject'8321'_126 :: Integer -> T_Fin_10 -> T_Fin_10
d_inject'8321'_126 ~v0 v1 = du_inject'8321'_126 v1
du_inject'8321'_126 :: T_Fin_10 -> T_Fin_10
du_inject'8321'_126 v0 = coe v0
-- Data.Fin.Base.inject≤
d_inject'8804'_130 ::
  Integer ->
  Integer ->
  T_Fin_10 -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> T_Fin_10
d_inject'8804'_130 ~v0 ~v1 v2 v3 = du_inject'8804'_130 v2 v3
du_inject'8804'_130 ::
  T_Fin_10 -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> T_Fin_10
du_inject'8804'_130 v0 v1
  = case coe v0 of
      C_zero_12 -> coe C_zero_12
      C_suc_16 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30 v6
               -> coe C_suc_16 (coe du_inject'8804'_130 (coe v3) (coe v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.lower₁
d_lower'8321'_142 ::
  Integer ->
  T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  T_Fin_10
d_lower'8321'_142 v0 v1 ~v2 = du_lower'8321'_142 v0 v1
du_lower'8321'_142 :: Integer -> T_Fin_10 -> T_Fin_10
du_lower'8321'_142 v0 v1
  = case coe v0 of
      0 -> coe
             seq (coe v1) (coe MAlonzo.Code.Data.Empty.du_'8869''45'elim_14)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             C_zero_12 -> coe C_zero_12
             C_suc_16 v4
               -> coe C_suc_16 (coe du_lower'8321'_142 (coe v2) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.strengthen
d_strengthen_156 :: Integer -> T_Fin_10 -> T_Fin_10
d_strengthen_156 ~v0 v1 = du_strengthen_156 v1
du_strengthen_156 :: T_Fin_10 -> T_Fin_10
du_strengthen_156 v0 = coe v0
-- Data.Fin.Base.splitAt
d_splitAt_164 ::
  Integer ->
  Integer -> T_Fin_10 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_splitAt_164 v0 ~v1 v2 = du_splitAt_164 v0 v2
du_splitAt_164 ::
  Integer -> T_Fin_10 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_splitAt_164 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 (coe v1)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             C_zero_12
               -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe C_zero_12)
             C_suc_16 v4
               -> coe
                    MAlonzo.Code.Data.Sum.Base.du_map_84 (coe C_suc_16) (\ v5 -> v5)
                    (coe du_splitAt_164 (coe v2) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.join
d_join_178 ::
  Integer ->
  Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> T_Fin_10
d_join_178 v0 ~v1 = du_join_178 v0
du_join_178 ::
  Integer -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> T_Fin_10
du_join_178 v0
  = coe
      MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66 (\ v1 -> v1)
      (coe du__'8593''691'__86 (coe v0))
-- Data.Fin.Base.quotRem
d_quotRem_190 ::
  Integer ->
  Integer -> T_Fin_10 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_quotRem_190 ~v0 v1 v2 = du_quotRem_190 v1 v2
du_quotRem_190 ::
  Integer -> T_Fin_10 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_quotRem_190 v0 v1
  = let v2 = coe du_splitAt_164 (coe v0) (coe v1) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
             (coe C_zero_12)
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v3
        -> coe
             MAlonzo.Code.Data.Product.Base.du_map'8322'_126
             (\ v4 -> coe C_suc_16) (coe du_quotRem_190 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.remQuot
d_remQuot_220 ::
  Integer ->
  Integer -> T_Fin_10 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_remQuot_220 ~v0 v1 v2 = du_remQuot_220 v1 v2
du_remQuot_220 ::
  Integer -> T_Fin_10 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_remQuot_220 v0 v1
  = coe
      MAlonzo.Code.Data.Product.Base.du_swap_346
      (coe du_quotRem_190 (coe v0) (coe v1))
-- Data.Fin.Base.quotient
d_quotient_226 :: Integer -> Integer -> T_Fin_10 -> T_Fin_10
d_quotient_226 ~v0 v1 v2 = du_quotient_226 v1 v2
du_quotient_226 :: Integer -> T_Fin_10 -> T_Fin_10
du_quotient_226 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe du_remQuot_220 (coe v0) (coe v1))
-- Data.Fin.Base.remainder
d_remainder_232 :: Integer -> Integer -> T_Fin_10 -> T_Fin_10
d_remainder_232 ~v0 v1 v2 = du_remainder_232 v1 v2
du_remainder_232 :: Integer -> T_Fin_10 -> T_Fin_10
du_remainder_232 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe du_remQuot_220 (coe v0) (coe v1))
-- Data.Fin.Base.combine
d_combine_238 ::
  Integer -> Integer -> T_Fin_10 -> T_Fin_10 -> T_Fin_10
d_combine_238 ~v0 v1 v2 v3 = du_combine_238 v1 v2 v3
du_combine_238 :: Integer -> T_Fin_10 -> T_Fin_10 -> T_Fin_10
du_combine_238 v0 v1 v2
  = case coe v1 of
      C_zero_12 -> coe v2
      C_suc_16 v4
        -> coe
             du__'8593''691'__86 (coe v0)
             (coe du_combine_238 (coe v0) (coe v4) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.finToFun
d_finToFun_254 ::
  Integer -> Integer -> T_Fin_10 -> T_Fin_10 -> T_Fin_10
d_finToFun_254 v0 v1 v2 v3
  = let v4 = subInt (coe v1) (coe (1 :: Integer)) in
    case coe v3 of
      C_zero_12
        -> coe
             du_quotient_226
             (coe MAlonzo.Code.Data.Nat.Base.d__'94'__206 (coe v0) (coe v4))
             (coe v2)
      C_suc_16 v6
        -> coe
             d_finToFun_254 (coe v0) (coe v4)
             (coe
                du_remainder_232
                (coe MAlonzo.Code.Data.Nat.Base.d__'94'__206 (coe v0) (coe v4))
                (coe v2))
             (coe v6)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.funToFin
d_funToFin_270 ::
  Integer -> Integer -> (T_Fin_10 -> T_Fin_10) -> T_Fin_10
d_funToFin_270 v0 v1 v2
  = case coe v0 of
      0 -> coe C_zero_12
      _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             du_combine_238
             (coe MAlonzo.Code.Data.Nat.Base.d__'94'__206 (coe v1) (coe v3))
             (coe v2 (coe C_zero_12))
             (coe
                d_funToFin_270 (coe v3) (coe v1)
                (coe (\ v4 -> coe v2 (coe C_suc_16 v4))))
-- Data.Fin.Base.fold
d_fold_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (Integer -> ()) ->
  Integer ->
  (Integer -> AgdaAny -> AgdaAny) ->
  (Integer -> AgdaAny) -> T_Fin_10 -> AgdaAny
d_fold_288 ~v0 ~v1 v2 v3 v4 v5 = du_fold_288 v2 v3 v4 v5
du_fold_288 ::
  Integer ->
  (Integer -> AgdaAny -> AgdaAny) ->
  (Integer -> AgdaAny) -> T_Fin_10 -> AgdaAny
du_fold_288 v0 v1 v2 v3
  = case coe v3 of
      C_zero_12
        -> let v5 = subInt (coe v0) (coe (1 :: Integer)) in coe v2 v5
      C_suc_16 v5
        -> let v6 = subInt (coe v0) (coe (1 :: Integer)) in
           coe v1 v6 (coe du_fold_288 (coe v6) (coe v1) (coe v2) (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.fold′
d_fold'8242'_314 ::
  Integer ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_Fin_10 -> ()) ->
  (T_Fin_10 -> AgdaAny -> AgdaAny) -> AgdaAny -> T_Fin_10 -> AgdaAny
d_fold'8242'_314 ~v0 ~v1 ~v2 v3 v4 v5 = du_fold'8242'_314 v3 v4 v5
du_fold'8242'_314 ::
  (T_Fin_10 -> AgdaAny -> AgdaAny) -> AgdaAny -> T_Fin_10 -> AgdaAny
du_fold'8242'_314 v0 v1 v2
  = case coe v2 of
      C_zero_12 -> coe v1
      C_suc_16 v4
        -> coe
             v0 v4
             (coe du_fold'8242'_314 (coe (\ v5 -> coe v0 v5)) (coe v1) (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.lift
d_lift_334 ::
  Integer ->
  Integer ->
  Integer -> (T_Fin_10 -> T_Fin_10) -> T_Fin_10 -> T_Fin_10
d_lift_334 ~v0 ~v1 v2 v3 v4 = du_lift_334 v2 v3 v4
du_lift_334 ::
  Integer -> (T_Fin_10 -> T_Fin_10) -> T_Fin_10 -> T_Fin_10
du_lift_334 v0 v1 v2
  = case coe v0 of
      0 -> coe v1 v2
      _ -> let v3 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v2 of
             C_zero_12 -> coe C_zero_12
             C_suc_16 v5
               -> coe C_suc_16 (coe du_lift_334 (coe v3) (coe v1) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base._+_
d__'43'__354 ::
  Integer -> Integer -> T_Fin_10 -> T_Fin_10 -> T_Fin_10
d__'43'__354 ~v0 ~v1 v2 v3 = du__'43'__354 v2 v3
du__'43'__354 :: T_Fin_10 -> T_Fin_10 -> T_Fin_10
du__'43'__354 v0 v1
  = case coe v0 of
      C_zero_12 -> coe v1
      C_suc_16 v3 -> coe C_suc_16 (coe du__'43'__354 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base._-_
d__'45'__366 :: Integer -> T_Fin_10 -> T_Fin_10 -> T_Fin_10
d__'45'__366 ~v0 v1 v2 = du__'45'__366 v1 v2
du__'45'__366 :: T_Fin_10 -> T_Fin_10 -> T_Fin_10
du__'45'__366 v0 v1
  = case coe v1 of
      C_zero_12 -> coe v0
      C_suc_16 v3
        -> case coe v0 of
             C_suc_16 v5 -> coe du__'45'__366 (coe v5) (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base._ℕ-_
d__ℕ'45'__378 :: Integer -> T_Fin_10 -> T_Fin_10
d__ℕ'45'__378 v0 v1
  = case coe v1 of
      C_zero_12 -> coe d_fromℕ_48 (coe v0)
      C_suc_16 v3
        -> let v4 = subInt (coe v0) (coe (1 :: Integer)) in
           coe d__ℕ'45'__378 (coe v4) (coe v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base._ℕ-ℕ_
d__ℕ'45'ℕ__388 :: Integer -> T_Fin_10 -> Integer
d__ℕ'45'ℕ__388 v0 v1
  = case coe v1 of
      C_zero_12 -> coe v0
      C_suc_16 v3
        -> let v4 = subInt (coe v0) (coe (1 :: Integer)) in
           coe d__ℕ'45'ℕ__388 (coe v4) (coe v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.pred
d_pred_396 :: Integer -> T_Fin_10 -> T_Fin_10
d_pred_396 ~v0 v1 = du_pred_396 v1
du_pred_396 :: T_Fin_10 -> T_Fin_10
du_pred_396 v0
  = case coe v0 of
      C_zero_12 -> coe C_zero_12
      C_suc_16 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.opposite
d_opposite_400 :: Integer -> T_Fin_10 -> T_Fin_10
d_opposite_400 v0 v1
  = let v2 = subInt (coe v0) (coe (1 :: Integer)) in
    case coe v1 of
      C_zero_12 -> coe d_fromℕ_48 (coe v2)
      C_suc_16 v4 -> coe d_opposite_400 (coe v2) (coe v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.punchOut
d_punchOut_412 ::
  Integer ->
  T_Fin_10 ->
  T_Fin_10 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  T_Fin_10
d_punchOut_412 ~v0 v1 v2 ~v3 = du_punchOut_412 v1 v2
du_punchOut_412 :: T_Fin_10 -> T_Fin_10 -> T_Fin_10
du_punchOut_412 v0 v1
  = case coe v0 of
      C_zero_12
        -> case coe v1 of
             C_zero_12 -> coe MAlonzo.Code.Data.Empty.du_'8869''45'elim_14
             C_suc_16 v4 -> coe v4
             _ -> MAlonzo.RTE.mazUnreachableError
      C_suc_16 v3
        -> case coe v1 of
             C_zero_12 -> coe C_zero_12
             C_suc_16 v5 -> coe C_suc_16 (coe du_punchOut_412 (coe v3) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.punchIn
d_punchIn_426 :: Integer -> T_Fin_10 -> T_Fin_10 -> T_Fin_10
d_punchIn_426 ~v0 v1 v2 = du_punchIn_426 v1 v2
du_punchIn_426 :: T_Fin_10 -> T_Fin_10 -> T_Fin_10
du_punchIn_426 v0 v1
  = case coe v0 of
      C_zero_12 -> coe C_suc_16 v1
      C_suc_16 v3
        -> case coe v1 of
             C_zero_12 -> coe C_zero_12
             C_suc_16 v5 -> coe C_suc_16 (coe du_punchIn_426 (coe v3) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.pinch
d_pinch_436 :: Integer -> T_Fin_10 -> T_Fin_10 -> T_Fin_10
d_pinch_436 ~v0 v1 v2 = du_pinch_436 v1 v2
du_pinch_436 :: T_Fin_10 -> T_Fin_10 -> T_Fin_10
du_pinch_436 v0 v1
  = case coe v1 of
      C_zero_12 -> coe C_zero_12
      C_suc_16 v3
        -> case coe v0 of
             C_zero_12 -> coe v3
             C_suc_16 v5 -> coe C_suc_16 (coe du_pinch_436 (coe v5) (coe v3))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base._≤_
d__'8804'__450 :: Integer -> Integer -> T_Fin_10 -> T_Fin_10 -> ()
d__'8804'__450 = erased
-- Data.Fin.Base._≥_
d__'8805'__456 :: Integer -> Integer -> T_Fin_10 -> T_Fin_10 -> ()
d__'8805'__456 = erased
-- Data.Fin.Base._<_
d__'60'__462 :: Integer -> Integer -> T_Fin_10 -> T_Fin_10 -> ()
d__'60'__462 = erased
-- Data.Fin.Base._>_
d__'62'__468 :: Integer -> Integer -> T_Fin_10 -> T_Fin_10 -> ()
d__'62'__468 = erased
-- Data.Fin.Base.Ordering
d_Ordering_476 a0 a1 a2 = ()
data T_Ordering_476
  = C_less_484 T_Fin_10 | C_equal_488 | C_greater_494 T_Fin_10
-- Data.Fin.Base.compare
d_compare_500 :: Integer -> T_Fin_10 -> T_Fin_10 -> T_Ordering_476
d_compare_500 ~v0 v1 v2 = du_compare_500 v1 v2
du_compare_500 :: T_Fin_10 -> T_Fin_10 -> T_Ordering_476
du_compare_500 v0 v1
  = case coe v0 of
      C_zero_12
        -> case coe v1 of
             C_zero_12 -> coe C_equal_488
             C_suc_16 v4 -> coe C_less_484 (coe C_zero_12)
             _ -> MAlonzo.RTE.mazUnreachableError
      C_suc_16 v3
        -> case coe v1 of
             C_zero_12 -> coe C_greater_494 (coe C_zero_12)
             C_suc_16 v5
               -> let v6 = coe du_compare_500 (coe v3) (coe v5) in
                  case coe v6 of
                    C_less_484 v8 -> coe C_less_484 (coe C_suc_16 v8)
                    C_equal_488 -> coe C_equal_488
                    C_greater_494 v8 -> coe C_greater_494 (coe C_suc_16 v8)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Fin.Base.raise
d_raise_536 :: Integer -> Integer -> T_Fin_10 -> T_Fin_10
d_raise_536 v0 v1 v2 = coe du__'8593''691'__86 v1 v2
-- Data.Fin.Base.inject+
d_inject'43'_542 :: Integer -> Integer -> T_Fin_10 -> T_Fin_10
d_inject'43'_542 ~v0 ~v1 v2 = du_inject'43'_542 v2
du_inject'43'_542 :: T_Fin_10 -> T_Fin_10
du_inject'43'_542 v0 = coe v0
-- Data.Fin.Base._≺_
d__'8826'__548 a0 a1 = ()
newtype T__'8826'__548 = C__'8827'toℕ__554 T_Fin_10
