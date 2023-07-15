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

module MAlonzo.Code.Tactic.Cong where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Reflection.AST.AlphaEquality
import qualified MAlonzo.Code.Reflection.AST.Argument
import qualified MAlonzo.Code.Reflection.AST.Literal

-- Tactic.Cong.varDescend
d_varDescend_4 :: Integer -> Integer -> Integer
d_varDescend_4 v0 v1
  = coe
      MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
      (coe
         MAlonzo.Code.Data.Nat.Base.d__'8804''7495'__10 (coe v0) (coe v1))
      (coe addInt (coe (1 :: Integer)) (coe v1)) (coe v1)
-- Tactic.Cong.patternDescend
d_patternDescend_10 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_patternDescend_10 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v2 v3
        -> coe
             MAlonzo.Code.Data.Product.Base.du_map'8321'_114
             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 (coe v2))
             (d_patternsDescend_12 (coe v0) (coe v3))
      MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v0)
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                MAlonzo.Code.Agda.Builtin.Reflection.C_var_244
                (coe d_varDescend_4 (coe v0) (coe v2)))
             (coe addInt (coe (1 :: Integer)) (coe v0))
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v0)
      MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v0)
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256
                (coe d_varDescend_4 (coe v0) (coe v2)))
             (coe addInt (coe (1 :: Integer)) (coe v0))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Cong.patternsDescend
d_patternsDescend_12 ::
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_patternsDescend_12 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v0)
      (:) v2 v3
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v4 v5
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                          (coe
                             d_patternsDescend_12
                             (coe
                                MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                (coe d_patternDescend_10 (coe v0) (coe v5)))
                             (coe v3))))
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                       (coe
                          d_patternsDescend_12
                          (coe
                             MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                             (coe d_patternDescend_10 (coe v0) (coe v5)))
                          (coe v3)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Cong.notEqualityError
d_notEqualityError_62 ::
  () -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_notEqualityError_62 ~v0 v1 = du_notEqualityError_62 v1
du_notEqualityError_62 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_notEqualityError_62 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334 () erased
      (coe
         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
            (coe
               ("Cannot rewrite a goal that is not equality: "
                ::
                Data.Text.Text)))
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_termErr_302 (coe v0))
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
-- Tactic.Cong.EqualityGoal
d_EqualityGoal_66 = ()
data T_EqualityGoal_66
  = C_equals_84 MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
                MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
                MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
                MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
-- Tactic.Cong.EqualityGoal.level
d_level_76 ::
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_level_76 v0
  = case coe v0 of
      C_equals_84 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Cong.EqualityGoal.type
d_type_78 ::
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_type_78 v0
  = case coe v0 of
      C_equals_84 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Cong.EqualityGoal.lhs
d_lhs_80 ::
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_lhs_80 v0
  = case coe v0 of
      C_equals_84 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Cong.EqualityGoal.rhs
d_rhs_82 ::
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_rhs_82 v0
  = case coe v0 of
      C_equals_84 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Cong.destructEqualityGoal
d_destructEqualityGoal_86 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_destructEqualityGoal_86 v0
  = let v1 = coe du_notEqualityError_62 (coe v0) in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v2 v3
        -> case coe v2 of
             MAlonzo.RTE.QName 12 1335258922519917603 _ _
               -> case coe v3 of
                    (:) v4 v5
                      -> case coe v5 of
                           (:) v6 v7
                             -> case coe v7 of
                                  (:) v8 v9
                                    -> case coe v9 of
                                         (:) v10 v11
                                           -> case coe v11 of
                                                []
                                                  -> coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316
                                                       () erased
                                                       (coe
                                                          C_equals_84
                                                          (coe
                                                             MAlonzo.Code.Reflection.AST.Argument.du_unArg_74
                                                             (coe v4))
                                                          (coe
                                                             MAlonzo.Code.Reflection.AST.Argument.du_unArg_74
                                                             (coe v6))
                                                          (coe
                                                             MAlonzo.Code.Reflection.AST.Argument.du_unArg_74
                                                             (coe v8))
                                                          (coe
                                                             MAlonzo.Code.Reflection.AST.Argument.du_unArg_74
                                                             (coe v10)))
                                                _ -> coe v1
                                         _ -> coe v1
                                  _ -> coe v1
                           _ -> coe v1
                    _ -> coe v1
             _ -> coe v1
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_blockOnMeta_402 () erased v2
      _ -> coe v1
-- Tactic.Cong.`cong
d_'96'cong_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_'96'cong_112 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_quoteTC_354 v0 erased v6)
      (\ v7 ->
         coe
           MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
           erased
           (coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_quoteTC_354 () erased v0)
           (\ v8 ->
              coe
                MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                erased
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.d_quoteTC_354 () erased v1)
                (\ v9 ->
                   coe
                     MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                     erased
                     (coe
                        MAlonzo.Code.Agda.Builtin.Reflection.d_quoteTC_354 v0 erased v2)
                     (\ v10 ->
                        coe
                          MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                          erased
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.d_quoteTC_354 v0 erased v3)
                          (\ v11 ->
                             coe
                               MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                               (coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                  (coe
                                     (MAlonzo.RTE.QName
                                        (38 :: Integer) (6189151057044369179 :: Integer)
                                        "Relation.Binary.PropositionalEquality.Core.cong"
                                        (MAlonzo.RTE.Fixity
                                           MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                           (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                        (coe v8))
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                           (coe v9))
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                              (coe d_level_76 (coe v4)))
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                                 (coe d_type_78 (coe v4)))
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
                                                          (coe ("\981" :: Data.Text.Text))
                                                          (coe v5))))
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                                       (coe v10))
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                                                (coe
                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                                                (coe
                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                                          (coe v11))
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                                                (coe
                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                                                (coe
                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                                                   (coe
                                                                      MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                                                   (coe
                                                                      MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                                             (coe v7))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))))))))))))
-- Tactic.Cong._.level
d_level_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_level_130 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 = du_level_130 v4
du_level_130 ::
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_level_130 v0 = coe d_level_76 (coe v0)
-- Tactic.Cong._.lhs
d_lhs_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_lhs_132 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 = du_lhs_132 v4
du_lhs_132 ::
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_lhs_132 v0 = coe d_lhs_80 (coe v0)
-- Tactic.Cong._.rhs
d_rhs_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_rhs_134 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 = du_rhs_134 v4
du_rhs_134 ::
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_rhs_134 v0 = coe d_rhs_82 (coe v0)
-- Tactic.Cong._.type
d_type_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_type_136 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 = du_type_136 v4
du_type_136 ::
  T_EqualityGoal_66 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_type_136 v0 = coe d_type_78 (coe v0)
-- Tactic.Cong.antiUnify
d_antiUnify_148 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_antiUnify_148 v0 v1 v2
  = let v3
          = coe
              MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
              (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v4 v5
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v6 v7
               -> let v8 = eqInt (coe v4) (coe v6) in
                  let v9 = d_antiUnifyArgs_150 (coe v0) (coe v5) (coe v7) in
                  case coe v9 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v10
                      -> if coe v8
                           then coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_var_164
                                  (coe d_varDescend_4 (coe v0) (coe v4)) (coe v10)
                           else coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0) (coe v10)
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v4 v5
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v6 v7
               -> let v8
                        = coe
                            MAlonzo.Code.Agda.Builtin.Reflection.d_primQNameEquality_8 v4 v6 in
                  let v9 = d_antiUnifyArgs_150 (coe v0) (coe v5) (coe v7) in
                  case coe v9 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v10
                      -> if coe v8
                           then coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 (coe v4) (coe v10)
                           else coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v4 v5
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v6 v7
               -> let v8
                        = coe
                            MAlonzo.Code.Agda.Builtin.Reflection.d_primQNameEquality_8 v4 v6 in
                  let v9 = d_antiUnifyArgs_150 (coe v0) (coe v5) (coe v7) in
                  case coe v9 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v10
                      -> if coe v8
                           then coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v4) (coe v10)
                           else coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v4 v5
        -> case coe v5 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v6 v7
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v8 v9
                      -> case coe v9 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v10 v11
                             -> coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 (coe v4)
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 (coe v6)
                                     (coe
                                        d_antiUnify_148 (coe addInt (coe (1 :: Integer)) (coe v0))
                                        (coe v7) (coe v11)))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> coe v3
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v4 v5
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v6 v7
               -> let v8 = d_antiUnifyClauses_152 (coe v0) (coe v4) (coe v6) in
                  let v9 = d_antiUnifyArgs_150 (coe v0) (coe v5) (coe v7) in
                  case coe v8 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v10
                      -> case coe v9 of
                           MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v11
                             -> coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 (coe v10)
                                  (coe v11)
                           MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                             -> coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                           _ -> MAlonzo.RTE.mazUnreachableError
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v4 v5
        -> case coe v4 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v6 v7
               -> case coe v5 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v8 v9
                      -> case coe v2 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v10 v11
                             -> case coe v10 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v12 v13
                                    -> case coe v11 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v14 v15
                                           -> coe
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                   (coe v6)
                                                   (coe
                                                      d_antiUnify_148 (coe v0) (coe v7) (coe v13)))
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
                                                   (coe v8)
                                                   (coe
                                                      d_antiUnify_148
                                                      (coe addInt (coe (1 :: Integer)) (coe v0))
                                                      (coe v9) (coe v15)))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> coe v3
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v4
        -> case coe v4 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v5
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v6
                      -> case coe v6 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v7
                             -> coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Reflection.C_set_212
                                     (coe d_antiUnify_148 (coe v0) (coe v5) (coe v7)))
                           _ -> coe v3
                    _ -> coe v3
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216 v5
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v6
                      -> case coe v6 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216 v7
                             -> let v8 = eqInt (coe v5) (coe v7) in
                                if coe v8
                                  then coe v1
                                  else coe
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                                         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                           _ -> coe v3
                    _ -> coe v3
             MAlonzo.Code.Agda.Builtin.Reflection.C_propLit_224 v5
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v6
                      -> case coe v6 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_propLit_224 v7
                             -> let v8 = eqInt (coe v5) (coe v7) in
                                if coe v8
                                  then coe v1
                                  else coe
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                                         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                           _ -> coe v3
                    _ -> coe v3
             MAlonzo.Code.Agda.Builtin.Reflection.C_inf_228 v5
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v6
                      -> case coe v6 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_inf_228 v7
                             -> let v8 = eqInt (coe v5) (coe v7) in
                                if coe v8
                                  then coe v1
                                  else coe
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                                         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                           _ -> coe v3
                    _ -> coe v3
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_230
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v5
                      -> case coe v5 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_230 -> coe v2
                           _ -> coe v3
                    _ -> coe v3
             _ -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v4
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v5
               -> let v6
                        = MAlonzo.Code.Reflection.AST.Literal.d__'8801''7495'__246
                            (coe v4) (coe v5) in
                  if coe v6
                    then coe v1
                    else coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
             _ -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v4 v5
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v6 v7
               -> let v8
                        = coe
                            MAlonzo.Code.Agda.Builtin.Reflection.d_primMetaEquality_40 v4 v6 in
                  let v9 = d_antiUnifyArgs_150 (coe v0) (coe v5) (coe v7) in
                  case coe v9 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v10
                      -> if coe v8
                           then coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 (coe v4) (coe v10)
                           else coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208 -> coe v2
             _ -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Cong.antiUnifyArgs
d_antiUnifyArgs_150 ::
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  Maybe [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_antiUnifyArgs_150 v0 v1 v2
  = let v3 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    case coe v1 of
      []
        -> case coe v2 of
             [] -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v2)
             _ -> coe v3
      (:) v4 v5
        -> case coe v4 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v6 v7
               -> case coe v2 of
                    (:) v8 v9
                      -> case coe v8 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v10 v11
                             -> coe
                                  MAlonzo.Code.Data.Maybe.Base.du_map_68
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v6)
                                        (coe d_antiUnify_148 (coe v0) (coe v7) (coe v11))))
                                  (d_antiUnifyArgs_150 (coe v0) (coe v5) (coe v9))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> coe v3
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Cong.antiUnifyClauses
d_antiUnifyClauses_152 ::
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] ->
  Maybe [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152]
d_antiUnifyClauses_152 v0 v1 v2
  = let v3
          = coe
              MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
              (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) in
    case coe v1 of
      (:) v4 v5
        -> case coe v2 of
             (:) v6 v7
               -> coe
                    MAlonzo.Code.Data.Maybe.Base.du_ap_72
                    (coe
                       MAlonzo.Code.Data.Maybe.Base.du_map_68
                       (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22)
                       (d_antiUnifyClause_154 (coe v0) (coe v4) (coe v6)))
                    (d_antiUnifyClauses_152 (coe v0) (coe v5) (coe v7))
             _ -> coe v3
      _ -> coe v3
-- Tactic.Cong.antiUnifyClause
d_antiUnifyClause_154 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152 ->
  Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152
d_antiUnifyClause_154 v0 v1 v2
  = let v3 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v4 v5 v6
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v7 v8 v9
               -> coe
                    MAlonzo.Code.Data.Maybe.Base.du_when_92
                    (coe
                       MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                       (coe
                          MAlonzo.Code.Reflection.AST.AlphaEquality.d__'61'α'61''45'Telescope__48
                          (coe v4) (coe v7))
                       (coe
                          MAlonzo.Code.Reflection.AST.AlphaEquality.d__'61'α'61''45'ArgsPattern__66
                          (coe v5) (coe v8)))
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 (coe v4)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                          (coe d_patternsDescend_12 (coe v0) (coe v5)))
                       (coe
                          d_antiUnify_148
                          (coe
                             MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                             (coe d_patternsDescend_12 (coe v0) (coe v5)))
                          (coe v6) (coe v9)))
             _ -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270 v4 v5
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270 v6 v7
               -> coe
                    MAlonzo.Code.Data.Maybe.Base.du_when_92
                    (coe
                       MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                       (coe
                          MAlonzo.Code.Reflection.AST.AlphaEquality.d__'61'α'61''45'Telescope__48
                          (coe v4) (coe v6))
                       (coe
                          MAlonzo.Code.Reflection.AST.AlphaEquality.d__'61'α'61''45'ArgsPattern__66
                          (coe v5) (coe v7)))
                    (coe v1)
             _ -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Cong.cong!
d_cong'33'_586 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_cong'33'_586 v0 ~v1 v2 v3 ~v4 v5 = du_cong'33'_586 v0 v2 v3 v5
du_cong'33'_586 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_cong'33'_586 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_withNormalisation_412 ()
      erased (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
         erased
         (coe MAlonzo.Code.Agda.Builtin.Reflection.d_inferType_336 v3)
         (\ v4 ->
            coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
              erased (d_destructEqualityGoal_86 (coe v4))
              (\ v5 ->
                 coe
                   MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                   erased
                   (d_'96'cong_112
                      (coe v0) erased (coe v1) (coe v2) (coe v5)
                      (coe
                         d_antiUnify_148 (coe (0 :: Integer)) (coe d_lhs_80 (coe v5))
                         (coe d_rhs_82 (coe v5)))
                      erased)
                   (\ v6 ->
                      coe MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328 v6 v3))))
