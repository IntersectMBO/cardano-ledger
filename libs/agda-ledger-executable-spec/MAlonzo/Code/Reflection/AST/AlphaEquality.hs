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

module MAlonzo.Code.Reflection.AST.AlphaEquality where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Reflection.AST.Argument.Information
import qualified MAlonzo.Code.Reflection.AST.Argument.Modality
import qualified MAlonzo.Code.Reflection.AST.Argument.Visibility
import qualified MAlonzo.Code.Reflection.AST.Literal
import qualified MAlonzo.Code.Reflection.AST.Meta
import qualified MAlonzo.Code.Reflection.AST.Name
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Reflection.AST.AlphaEquality.AlphaEquality
d_AlphaEquality_12 a0 = ()
newtype T_AlphaEquality_12
  = C_mkAlphaEquality_20 (AgdaAny -> AgdaAny -> Bool)
-- Reflection.AST.AlphaEquality.AlphaEquality._=α=_
d__'61'α'61'__18 ::
  T_AlphaEquality_12 -> AgdaAny -> AgdaAny -> Bool
d__'61'α'61'__18 v0
  = case coe v0 of
      C_mkAlphaEquality_20 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._._=α=_
d__'61'α'61'__24 ::
  T_AlphaEquality_12 -> AgdaAny -> AgdaAny -> Bool
d__'61'α'61'__24 v0 = coe d__'61'α'61'__18 (coe v0)
-- Reflection.AST.AlphaEquality.≟⇒α
d_'8799''8658'α_26 ::
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  T_AlphaEquality_12
d_'8799''8658'α_26 ~v0 v1 = du_'8799''8658'α_26 v1
du_'8799''8658'α_26 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  T_AlphaEquality_12
du_'8799''8658'α_26 v0
  = coe
      C_mkAlphaEquality_20
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
              erased (coe v0 v1 v2)))
-- Reflection.AST.AlphaEquality.α-Visibility
d_α'45'Visibility_34 :: T_AlphaEquality_12
d_α'45'Visibility_34
  = coe
      du_'8799''8658'α_26
      (coe MAlonzo.Code.Reflection.AST.Argument.Visibility.d__'8799'__8)
-- Reflection.AST.AlphaEquality.α-Modality
d_α'45'Modality_36 :: T_AlphaEquality_12
d_α'45'Modality_36
  = coe
      du_'8799''8658'α_26
      (coe MAlonzo.Code.Reflection.AST.Argument.Modality.d__'8799'__30)
-- Reflection.AST.AlphaEquality.α-ArgInfo
d_α'45'ArgInfo_38 :: T_AlphaEquality_12
d_α'45'ArgInfo_38
  = coe
      du_'8799''8658'α_26
      (coe
         MAlonzo.Code.Reflection.AST.Argument.Information.d__'8799'__30)
-- Reflection.AST.AlphaEquality.α-Literal
d_α'45'Literal_40 :: T_AlphaEquality_12
d_α'45'Literal_40
  = coe
      du_'8799''8658'α_26
      (coe MAlonzo.Code.Reflection.AST.Literal.d__'8799'__48)
-- Reflection.AST.AlphaEquality.α-Meta
d_α'45'Meta_42 :: T_AlphaEquality_12
d_α'45'Meta_42
  = coe
      du_'8799''8658'α_26
      (coe MAlonzo.Code.Reflection.AST.Meta.d__'8799'__10)
-- Reflection.AST.AlphaEquality.α-Name
d_α'45'Name_44 :: T_AlphaEquality_12
d_α'45'Name_44
  = coe
      du_'8799''8658'α_26
      (coe MAlonzo.Code.Reflection.AST.Name.d__'8799'__12)
-- Reflection.AST.AlphaEquality._=α=-AbsTerm_
d__'61'α'61''45'AbsTerm__46 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104 -> Bool
d__'61'α'61''45'AbsTerm__46 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v4 v5
               -> coe d__'61'α'61''45'Term__54 (coe v3) (coe v5)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._=α=-Telescope_
d__'61'α'61''45'Telescope__48 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> Bool
d__'61'α'61''45'Telescope__48 v0 v1
  = case coe v0 of
      []
        -> case coe v1 of
             [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
             (:) v2 v3 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v2 v3
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
               -> case coe v1 of
                    [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
                    (:) v6 v7
                      -> case coe v6 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
                             -> coe
                                  MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                                  (coe d__'61'α'61''45'ArgTerm__50 (coe v5) (coe v9))
                                  (coe d__'61'α'61''45'Telescope__48 (coe v3) (coe v7))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._=α=-ArgTerm_
d__'61'α'61''45'ArgTerm__50 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> Bool
d__'61'α'61''45'ArgTerm__50 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v4 v5
               -> coe d__'61'α'61''45'Term__54 (coe v3) (coe v5)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._=α=-ArgPattern_
d__'61'α'61''45'ArgPattern__52 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> Bool
d__'61'α'61''45'ArgPattern__52 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v4 v5
               -> coe d__'61'α'61''45'Pattern__64 (coe v3) (coe v5)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._=α=-Term_
d__'61'α'61''45'Term__54 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool
d__'61'α'61''45'Term__54 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe eqInt (coe v2) (coe v4))
                    (coe d__'61'α'61''45'ArgsTerm__62 (coe v3) (coe v5))
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                       erased
                       (MAlonzo.Code.Reflection.AST.Name.d__'8799'__12 (coe v2) (coe v4)))
                    (coe d__'61'α'61''45'ArgsTerm__62 (coe v3) (coe v5))
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                       erased
                       (MAlonzo.Code.Reflection.AST.Name.d__'8799'__12 (coe v2) (coe v4)))
                    (coe d__'61'α'61''45'ArgsTerm__62 (coe v3) (coe v5))
             MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                       erased
                       (MAlonzo.Code.Reflection.AST.Argument.Visibility.d__'8799'__8
                          (coe v2) (coe v4)))
                    (coe d__'61'α'61''45'AbsTerm__46 (coe v3) (coe v5))
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe d__'61'α'61''45'Clauses__60 (coe v2) (coe v4))
                    (coe d__'61'α'61''45'ArgsTerm__62 (coe v3) (coe v5))
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe d__'61'α'61''45'ArgTerm__50 (coe v2) (coe v4))
                    (coe d__'61'α'61''45'AbsTerm__46 (coe v3) (coe v5))
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v3
               -> coe d__'61'α'61''45'Sort__56 (coe v2) (coe v3)
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                    erased
                    (MAlonzo.Code.Reflection.AST.Literal.d__'8799'__48
                       (coe v2) (coe v3))
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v4 v5
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                       erased
                       (MAlonzo.Code.Reflection.AST.Meta.d__'8799'__10 (coe v2) (coe v4)))
                    (coe d__'61'α'61''45'ArgsTerm__62 (coe v3) (coe v5))
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v2 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v2 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v2 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v2 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v2 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v2 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v2
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v2
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v2 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._=α=-Sort_
d__'61'α'61''45'Sort__56 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Sort_148 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Sort_148 -> Bool
d__'61'α'61''45'Sort__56 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v3
               -> coe d__'61'α'61''45'Term__54 (coe v2) (coe v3)
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_prop_220 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_propLit_224 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_inf_228 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_230
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216 v3
               -> coe eqInt (coe v2) (coe v3)
             MAlonzo.Code.Agda.Builtin.Reflection.C_prop_220 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_propLit_224 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_inf_228 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_230
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_prop_220 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_prop_220 v3
               -> coe d__'61'α'61''45'Term__54 (coe v2) (coe v3)
             MAlonzo.Code.Agda.Builtin.Reflection.C_propLit_224 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_inf_228 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_230
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_propLit_224 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_prop_220 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_propLit_224 v3
               -> coe eqInt (coe v2) (coe v3)
             MAlonzo.Code.Agda.Builtin.Reflection.C_inf_228 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_230
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_inf_228 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_prop_220 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_propLit_224 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_inf_228 v3
               -> coe eqInt (coe v2) (coe v3)
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_230
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_230
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v2
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_216 v2
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_prop_220 v2
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_propLit_224 v2
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_inf_228 v2
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_230
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._=α=-Clause_
d__'61'α'61''45'Clause__58 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152 -> Bool
d__'61'α'61''45'Clause__58 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v2 v3 v4
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v5 v6 v7
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe d__'61'α'61''45'Telescope__48 (coe v2) (coe v5))
                    (coe
                       MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                       (coe d__'61'α'61''45'ArgsPattern__66 (coe v3) (coe v6))
                       (coe d__'61'α'61''45'Term__54 (coe v4) (coe v7)))
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270 v5 v6
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v4 v5 v6
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270 v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe d__'61'α'61''45'Telescope__48 (coe v2) (coe v4))
                    (coe d__'61'α'61''45'ArgsPattern__66 (coe v3) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._=α=-Clauses_
d__'61'α'61''45'Clauses__60 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> Bool
d__'61'α'61''45'Clauses__60 v0 v1
  = case coe v0 of
      []
        -> case coe v1 of
             [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
             (:) v2 v3 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v2 v3
        -> case coe v1 of
             [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             (:) v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe d__'61'α'61''45'Clause__58 (coe v2) (coe v4))
                    (coe d__'61'α'61''45'Clauses__60 (coe v3) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._=α=-ArgsTerm_
d__'61'α'61''45'ArgsTerm__62 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> Bool
d__'61'α'61''45'ArgsTerm__62 v0 v1
  = case coe v0 of
      []
        -> case coe v1 of
             [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
             (:) v2 v3 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v2 v3
        -> case coe v1 of
             [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             (:) v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe d__'61'α'61''45'ArgTerm__50 (coe v2) (coe v4))
                    (coe d__'61'α'61''45'ArgsTerm__62 (coe v3) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._=α=-Pattern_
d__'61'α'61''45'Pattern__64 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 -> Bool
d__'61'α'61''45'Pattern__64 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v2 v3
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                       erased
                       (MAlonzo.Code.Reflection.AST.Name.d__'8799'__12 (coe v2) (coe v4)))
                    (coe d__'61'α'61''45'ArgsPattern__66 (coe v3) (coe v5))
             MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v3
               -> coe d__'61'α'61''45'Term__54 (coe v2) (coe v3)
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v3
               -> coe eqInt (coe v2) (coe v3)
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                    erased
                    (MAlonzo.Code.Reflection.AST.Literal.d__'8799'__48
                       (coe v2) (coe v3))
             MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v3
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                    erased
                    (MAlonzo.Code.Reflection.AST.Name.d__'8799'__12 (coe v2) (coe v3))
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v3 v4
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v3
               -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v3
               -> coe eqInt (coe v2) (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality._=α=-ArgsPattern_
d__'61'α'61''45'ArgsPattern__66 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> Bool
d__'61'α'61''45'ArgsPattern__66 v0 v1
  = case coe v0 of
      []
        -> case coe v1 of
             [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
             (:) v2 v3 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v2 v3
        -> case coe v1 of
             [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
             (:) v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                    (coe d__'61'α'61''45'ArgPattern__52 (coe v2) (coe v4))
                    (coe d__'61'α'61''45'ArgsPattern__66 (coe v3) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.AST.AlphaEquality.α-AbsTerm
d_α'45'AbsTerm_754 :: T_AlphaEquality_12
d_α'45'AbsTerm_754
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'AbsTerm__46)
-- Reflection.AST.AlphaEquality.α-ArgTerm
d_α'45'ArgTerm_756 :: T_AlphaEquality_12
d_α'45'ArgTerm_756
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'ArgTerm__50)
-- Reflection.AST.AlphaEquality.α-ArgPattern
d_α'45'ArgPattern_758 :: T_AlphaEquality_12
d_α'45'ArgPattern_758
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'ArgPattern__52)
-- Reflection.AST.AlphaEquality.α-Telescope
d_α'45'Telescope_760 :: T_AlphaEquality_12
d_α'45'Telescope_760
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'Telescope__48)
-- Reflection.AST.AlphaEquality.α-Term
d_α'45'Term_762 :: T_AlphaEquality_12
d_α'45'Term_762
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'Term__54)
-- Reflection.AST.AlphaEquality.α-Sort
d_α'45'Sort_764 :: T_AlphaEquality_12
d_α'45'Sort_764
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'Sort__56)
-- Reflection.AST.AlphaEquality.α-Clause
d_α'45'Clause_766 :: T_AlphaEquality_12
d_α'45'Clause_766
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'Clause__58)
-- Reflection.AST.AlphaEquality.α-Clauses
d_α'45'Clauses_768 :: T_AlphaEquality_12
d_α'45'Clauses_768
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'Clauses__60)
-- Reflection.AST.AlphaEquality.α-ArgsTerm
d_α'45'ArgsTerm_770 :: T_AlphaEquality_12
d_α'45'ArgsTerm_770
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'ArgsTerm__62)
-- Reflection.AST.AlphaEquality.α-Pattern
d_α'45'Pattern_772 :: T_AlphaEquality_12
d_α'45'Pattern_772
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'Pattern__64)
-- Reflection.AST.AlphaEquality.α-ArgsPattern
d_α'45'ArgsPattern_774 :: T_AlphaEquality_12
d_α'45'ArgsPattern_774
  = coe C_mkAlphaEquality_20 (coe d__'61'α'61''45'ArgsPattern__66)
