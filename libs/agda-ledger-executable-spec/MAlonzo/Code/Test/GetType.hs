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

module MAlonzo.Code.Test.GetType where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Primitive

-- Test.GetType.byTC
d_byTC_8 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_byTC_8 v0 ~v1 v2 v3 = du_byTC_8 v0 v2 v3
du_byTC_8 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_byTC_8 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 v0 () erased
         erased v1
         (coe MAlonzo.Code.Agda.Builtin.Reflection.d_quoteTC_354 v0 erased))
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328 v2)
-- Test.GetType.Temp.id
d_id_18 :: () -> AgdaAny -> AgdaAny
d_id_18 ~v0 v1 = du_id_18 v1
du_id_18 :: AgdaAny -> AgdaAny
du_id_18 v0 = coe v0
-- Test.GetType.Temp.id-type₁
d_id'45'type'8321'_22 ::
  () -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_id'45'type'8321'_22 ~v0 = du_id'45'type'8321'_22
du_id'45'type'8321'_22 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_id'45'type'8321'_22
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
               (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
               (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe (0 :: Integer))
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
         (coe ("_" :: Data.Text.Text))
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe (1 :: Integer))
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
-- Test.GetType.wrap
d_wrap_24 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_wrap_24 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
               (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
               (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216
               (coe (0 :: Integer)))))
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
         (coe ("A" :: Data.Text.Text)) (coe v0))
-- Test.GetType.id-type₂
d_id'45'type'8322'_28 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_id'45'type'8322'_28
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
               (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
               (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216
               (coe (0 :: Integer)))))
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
         (coe ("A" :: Data.Text.Text))
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe (0 :: Integer))
                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
               (coe ("_" :: Data.Text.Text))
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe (1 :: Integer))
                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
-- Test.GetType.pf
d_pf_32 :: () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pf_32 = erased
