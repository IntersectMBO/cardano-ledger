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

module MAlonzo.Code.Reflection.AST.Show where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Float
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Nat.Show
import qualified MAlonzo.Code.Data.String
import qualified MAlonzo.Code.Data.String.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Reflection.AST.Argument
import qualified MAlonzo.Code.Reflection.AST.Argument.Information

-- Reflection.AST.Show.showRelevance
d_showRelevance_6 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Relevance_56 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showRelevance_6 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
        -> coe ("relevant" :: Data.Text.Text)
      MAlonzo.Code.Agda.Builtin.Reflection.C_irrelevant_60
        -> coe ("irrelevant" :: Data.Text.Text)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showRel
d_showRel_8 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Relevance_56 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showRel_8 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
        -> coe ("" :: Data.Text.Text)
      MAlonzo.Code.Agda.Builtin.Reflection.C_irrelevant_60
        -> coe ("." :: Data.Text.Text)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showVisibility
d_showVisibility_10 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Visibility_48 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showVisibility_10 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
        -> coe ("visible" :: Data.Text.Text)
      MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52
        -> coe ("hidden" :: Data.Text.Text)
      MAlonzo.Code.Agda.Builtin.Reflection.C_instance'8242'_54
        -> coe ("instance" :: Data.Text.Text)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showLiteral
d_showLiteral_12 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Literal_116 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showLiteral_12 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_nat_120 v1
        -> coe MAlonzo.Code.Data.Nat.Show.d_show_56 (coe v1)
      MAlonzo.Code.Agda.Builtin.Reflection.C_word64_124 v1
        -> coe
             MAlonzo.Code.Data.Nat.Show.d_show_56 (coe word64ToNat (coe v1))
      MAlonzo.Code.Agda.Builtin.Reflection.C_float_128 v1
        -> coe MAlonzo.Code.Agda.Builtin.Float.d_primShowFloat_46 v1
      MAlonzo.Code.Agda.Builtin.Reflection.C_char_132 v1
        -> coe MAlonzo.Code.Agda.Builtin.String.d_primShowChar_20 v1
      MAlonzo.Code.Agda.Builtin.Reflection.C_string_136 v1
        -> coe MAlonzo.Code.Agda.Builtin.String.d_primShowString_22 v1
      MAlonzo.Code.Agda.Builtin.Reflection.C_name_140 v1
        -> coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v1
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_144 v1
        -> coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowMeta_44 v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.visibilityParen
d_visibilityParen_28 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Visibility_48 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_visibilityParen_28 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
        -> coe MAlonzo.Code.Data.String.d_parensIfSpace_130 (coe v1)
      MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52
        -> coe MAlonzo.Code.Data.String.Base.d_braces_42 (coe v1)
      MAlonzo.Code.Agda.Builtin.Reflection.C_instance'8242'_54
        -> coe
             MAlonzo.Code.Data.String.Base.d_braces_42
             (coe MAlonzo.Code.Data.String.Base.d_braces_42 (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showTerms
d_showTerms_36 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showTerms_36 v0
  = case coe v0 of
      [] -> coe ("" :: Data.Text.Text)
      (:) v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v3 v4
               -> coe
                    MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                    (coe
                       d_visibilityParen_28
                       (coe
                          MAlonzo.Code.Reflection.AST.Argument.Information.d_visibility_16
                          (coe v3))
                       (coe d_showTerm_38 (coe v4)))
                    (coe d_showTerms_36 (coe v2))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showTerm
d_showTerm_38 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showTerm_38 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v1 v2
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("var" :: Data.Text.Text))
             (coe
                MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                (coe MAlonzo.Code.Data.Nat.Show.d_show_56 (coe v1))
                (coe d_showTerms_36 (coe v2)))
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v1 v2
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v1)
             (coe d_showTerms_36 (coe v2))
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v1 v2
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v1)
             (coe d_showTerms_36 (coe v2))
      MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v1 v2
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v3 v4
               -> coe
                    MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                    (coe ("\955" :: Data.Text.Text))
                    (coe
                       MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                       (coe d_visibilityParen_28 (coe v1) (coe v3))
                       (coe
                          MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                          (coe ("\8594" :: Data.Text.Text)) (coe d_showTerm_38 (coe v4))))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v1 v2
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("\955 {" :: Data.Text.Text))
             (coe
                MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                (coe d_showClauses_48 (coe v1))
                (coe
                   MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                   (coe ("}" :: Data.Text.Text)) (coe d_showTerms_36 (coe v2))))
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v3 v4
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v5 v6
                      -> coe
                           MAlonzo.Code.Data.String.Base.d__'43''43'__20
                           ("\928 (" :: Data.Text.Text)
                           (MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                              (coe
                                 d_visibilityParen_28
                                 (coe
                                    MAlonzo.Code.Reflection.AST.Argument.Information.d_visibility_16
                                    (coe v3))
                                 (coe v5))
                              (coe
                                 MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                                 (coe (":" :: Data.Text.Text))
                                 (coe
                                    MAlonzo.Code.Data.String.Base.d__'43''43'__20
                                    (MAlonzo.Code.Data.String.d_parensIfSpace_130
                                       (coe d_showTerm_38 (coe v4)))
                                    (MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                                       (coe (")" :: Data.Text.Text))
                                       (coe
                                          MAlonzo.Code.Data.String.d_parensIfSpace_130
                                          (coe d_showTerm_38 (coe v6)))))))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v1
        -> coe d_showSort_40 (coe v1)
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v1
        -> coe d_showLiteral_12 (coe v1)
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v1 v2
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowMeta_44 v1)
             (coe d_showTerms_36 (coe v2))
      MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
        -> coe ("unknown" :: Data.Text.Text)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showSort
d_showSort_40 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Sort_148 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showSort_40 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v1
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("Set" :: Data.Text.Text))
             (coe
                MAlonzo.Code.Data.String.d_parensIfSpace_130
                (coe d_showTerm_38 (coe v1)))
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216 v1
        -> coe
             MAlonzo.Code.Data.String.Base.d__'43''43'__20
             ("Set" :: Data.Text.Text)
             (MAlonzo.Code.Data.Nat.Show.d_show_56 (coe v1))
      MAlonzo.Code.Agda.Builtin.Reflection.C_prop_220 v1
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("Prop" :: Data.Text.Text))
             (coe
                MAlonzo.Code.Data.String.d_parensIfSpace_130
                (coe d_showTerm_38 (coe v1)))
      MAlonzo.Code.Agda.Builtin.Reflection.C_propLit_224 v1
        -> coe
             MAlonzo.Code.Data.String.Base.d__'43''43'__20
             ("Prop" :: Data.Text.Text)
             (MAlonzo.Code.Data.Nat.Show.d_show_56 (coe v1))
      MAlonzo.Code.Agda.Builtin.Reflection.C_inf_228 v1
        -> coe
             MAlonzo.Code.Data.String.Base.d__'43''43'__20
             ("Set\969" :: Data.Text.Text)
             (MAlonzo.Code.Data.Nat.Show.d_show_56 (coe v1))
      MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_230
        -> coe ("unknown" :: Data.Text.Text)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showPatterns
d_showPatterns_42 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showPatterns_42 v0
  = case coe v0 of
      [] -> coe ("" :: Data.Text.Text)
      (:) v1 v2
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe du_showArg_114 (coe v1)) (coe d_showPatterns_42 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showPattern
d_showPattern_44 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showPattern_44 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v1 v2
        -> let v3
                 = MAlonzo.Code.Data.String.Base.d_parens_38
                     (coe
                        MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                        (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v1)
                        (coe d_showPatterns_42 (coe v2))) in
           case coe v2 of
             []
               -> coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v1
             _ -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v1
        -> coe
             MAlonzo.Code.Data.String.Base.d__'43''43'__20
             ("." :: Data.Text.Text)
             (MAlonzo.Code.Data.String.Base.d_parens_38
                (coe d_showTerm_38 (coe v1)))
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v1
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("pat-var" :: Data.Text.Text))
             (coe MAlonzo.Code.Data.Nat.Show.d_show_56 (coe v1))
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v1
        -> coe d_showLiteral_12 (coe v1)
      MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v1
        -> coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v1
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v1
        -> coe ("()" :: Data.Text.Text)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showClause
d_showClause_46 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showClause_46 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v1 v2 v3
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("[" :: Data.Text.Text))
             (coe
                MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                (coe d_showTel_50 (coe v1))
                (coe
                   MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                   (coe ("]" :: Data.Text.Text))
                   (coe
                      MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                      (coe d_showPatterns_42 (coe v2))
                      (coe
                         MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                         (coe ("\8594" :: Data.Text.Text)) (coe d_showTerm_38 (coe v3))))))
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270 v1 v2
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("[" :: Data.Text.Text))
             (coe
                MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                (coe d_showTel_50 (coe v1))
                (coe
                   MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                   (coe ("]" :: Data.Text.Text)) (coe d_showPatterns_42 (coe v2))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showClauses
d_showClauses_48 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showClauses_48 v0
  = case coe v0 of
      [] -> coe ("" :: Data.Text.Text)
      (:) v1 v2
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe d_showClause_46 (coe v1))
             (coe
                MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                (coe (";" :: Data.Text.Text)) (coe d_showClauses_48 (coe v2)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showTel
d_showTel_50 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showTel_50 v0
  = case coe v0 of
      [] -> coe ("" :: Data.Text.Text)
      (:) v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
               -> case coe v4 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
                      -> coe
                           MAlonzo.Code.Data.String.Base.d__'43''43'__20
                           (d_visibilityParen_28
                              (coe
                                 MAlonzo.Code.Reflection.AST.Argument.Information.d_visibility_16
                                 (coe v5))
                              (coe
                                 MAlonzo.Code.Data.String.Base.d__'60''43''62'__46 (coe v3)
                                 (coe
                                    MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                                    (coe (":" :: Data.Text.Text)) (coe d_showTerm_38 (coe v6)))))
                           (d_showTel_50 (coe v2))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show._.showArg
d_showArg_114 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showArg_114 ~v0 ~v1 v2 = du_showArg_114 v2
du_showArg_114 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
du_showArg_114 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v3 v4
               -> case coe v4 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v5 v6
                      -> coe
                           du_braces'63'_126 v3
                           (coe
                              MAlonzo.Code.Data.String.Base.d__'43''43'__20
                              (d_showRel_8 (coe v5)) (d_showPattern_44 (coe v2)))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show._._.braces?
d_braces'63'_126 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Visibility_48 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Relevance_56 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Quantity_62 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_braces'63'_126 ~v0 ~v1 v2 ~v3 ~v4 ~v5 = du_braces'63'_126 v2
du_braces'63'_126 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Visibility_48 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
du_braces'63'_126 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
        -> coe (\ v1 -> v1)
      MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52
        -> coe MAlonzo.Code.Data.String.Base.d_braces_42
      MAlonzo.Code.Agda.Builtin.Reflection.C_instance'8242'_54
        -> coe
             MAlonzo.Code.Function.Base.du__'8728''8242'__216
             (coe MAlonzo.Code.Data.String.Base.d_braces_42)
             (coe MAlonzo.Code.Data.String.Base.d_braces_42)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.Show.showDefinition
d_showDefinition_166 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Definition_272 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_showDefinition_166 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_function_276 v1
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("function" :: Data.Text.Text))
             (coe
                MAlonzo.Code.Data.String.Base.d_braces_42
                (coe d_showClauses_48 (coe v1)))
      MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v1 v2
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("datatype" :: Data.Text.Text))
             (coe
                MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                (coe MAlonzo.Code.Data.Nat.Show.d_show_56 (coe v1))
                (coe
                   MAlonzo.Code.Data.String.Base.d_braces_42
                   (coe
                      MAlonzo.Code.Data.String.Base.d_intersperse_30
                      (", " :: Data.Text.Text)
                      (coe
                         MAlonzo.Code.Data.List.Base.du_map_22
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12)
                         (coe v2)))))
      MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v1 v2
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("record" :: Data.Text.Text))
             (coe
                MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v1)
                (coe
                   MAlonzo.Code.Data.String.Base.d_braces_42
                   (coe
                      MAlonzo.Code.Data.String.Base.d_intersperse_30
                      (", " :: Data.Text.Text)
                      (coe
                         MAlonzo.Code.Data.List.Base.du_map_22
                         (coe
                            MAlonzo.Code.Function.Base.du__'8728''8242'__216
                            (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12)
                            (coe MAlonzo.Code.Reflection.AST.Argument.du_unArg_74))
                         (coe v2)))))
      MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'cons_292 v1
        -> coe
             MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
             (coe ("constructor" :: Data.Text.Text))
             (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v1)
      MAlonzo.Code.Agda.Builtin.Reflection.C_axiom_294
        -> coe ("axiom" :: Data.Text.Text)
      MAlonzo.Code.Agda.Builtin.Reflection.C_prim'45'fun_296
        -> coe ("primitive" :: Data.Text.Text)
      _ -> MAlonzo.RTE.mazUnreachableError
