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

module MAlonzo.Code.Generics.Utils where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Nat
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.NonEmpty.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.String.Base
import qualified MAlonzo.Code.Generics.Debug
import qualified MAlonzo.Code.Reflection.AST.Abstraction
import qualified MAlonzo.Code.Reflection.AST.Argument
import qualified MAlonzo.Code.Reflection.AST.Argument.Information
import qualified MAlonzo.Code.Reflection.AST.Argument.Visibility
import qualified MAlonzo.Code.Reflection.AST.Name
import qualified MAlonzo.Code.Reflection.AST.Show
import qualified MAlonzo.Code.Reflection.TCM
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Generics.Utils.absName
d_absName_10 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_absName_10 ~v0 ~v1 v2 = du_absName_10 v2
du_absName_10 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
du_absName_10 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.getVisibility
d_getVisibility_20 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Visibility_48
d_getVisibility_20 ~v0 ~v1 v2 = du_getVisibility_20 v2
du_getVisibility_20 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Visibility_48
du_getVisibility_20 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v3 v4
               -> coe v3
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.findMetas
d_findMetas_24 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146]
d_findMetas_24 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v1 v2
        -> coe d_findMetas''_26 (coe v2)
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v1 v2
        -> coe d_findMetas''_26 (coe v2)
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v1 v2
        -> coe d_findMetas''_26 (coe v2)
      MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v1 v2
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v3 v4
               -> coe d_findMetas_24 (coe v4)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v1 v2
        -> coe
             MAlonzo.Code.Data.List.Base.du__'43''43'__62
             (coe d_findMetasCl_28 (coe v1)) (coe d_findMetas''_26 (coe v2))
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v3 v4
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v5 v6
                      -> coe
                           MAlonzo.Code.Data.List.Base.du__'43''43'__62
                           (coe d_findMetas_24 (coe v4)) (coe d_findMetas_24 (coe v6))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v1
        -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v1
        -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0)
             (coe d_findMetas''_26 (coe v2))
      MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
        -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.findMetas'
d_findMetas''_26 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146]
d_findMetas''_26 v0
  = case coe v0 of
      [] -> coe v0
      (:) v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v3 v4
               -> coe
                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                    (coe d_findMetas_24 (coe v4)) (coe d_findMetas''_26 (coe v2))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.findMetasCl
d_findMetasCl_28 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146]
d_findMetasCl_28 v0
  = case coe v0 of
      [] -> coe v0
      (:) v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v3 v4 v5
               -> coe
                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                    (coe d_findMetas_24 (coe v5)) (coe d_findMetasCl_28 (coe v2))
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270 v3 v4
               -> coe d_findMetasCl_28 (coe v2)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.isMeta
d_isMeta_82 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool
d_isMeta_82 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v2 v3
        -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      _ -> coe v1
-- Generics.Utils.unArgs
d_unArgs_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> [AgdaAny]
d_unArgs_84 ~v0 ~v1 = du_unArgs_84
du_unArgs_84 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> [AgdaAny]
du_unArgs_84
  = coe
      MAlonzo.Code.Data.List.Base.du_map_22
      (coe MAlonzo.Code.Reflection.AST.Argument.du_unArg_74)
-- Generics.Utils.mapVariables
d_mapVariables_86 ::
  (Integer -> Integer) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_mapVariables_86 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 (coe v2)
             (coe du_go_102 (coe v0) (coe v3))
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v2
        -> coe MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 (coe v0 v2)
      _ -> coe v1
-- Generics.Utils._.go
d_go_102 ::
  (Integer -> Integer) ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_go_102 v0 ~v1 ~v2 v3 = du_go_102 v0 v3
du_go_102 ::
  (Integer -> Integer) ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
du_go_102 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v4 v5
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v4)
                       (coe d_mapVariables_86 (coe v0) (coe v5)))
                    (coe du_go_102 (coe v0) (coe v3))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.TypeView
d_TypeView_112 :: ()
d_TypeView_112 = erased
-- Generics.Utils.viewTy
d_viewTy_114 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_viewTy_114 v0
  = let v1
          = coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
              (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v0) in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v2 v3
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v4 v5
               -> coe
                    MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 (coe v4) (coe v2)))
                    (d_viewTy_114 (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> coe v1
-- Generics.Utils.tyView
d_tyView_126 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_tyView_126 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> case coe v1 of
             [] -> coe v2
             (:) v3 v4
               -> case coe v3 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v5 v6
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 (coe v6)
                           (coe
                              MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 (coe v5)
                              (coe
                                 d_tyView_126
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4) (coe v2))))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.argumentWise
d_argumentWise_138 ::
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
   MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_argumentWise_138 v0 v1
  = coe
      d_tyView_126
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            MAlonzo.Code.Data.List.Base.du_map_22
            (coe
               MAlonzo.Code.Reflection.AST.Abstraction.du_map_22
               (coe MAlonzo.Code.Reflection.AST.Argument.du_map_54 (coe v0)))
            (coe
               MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
               (coe d_viewTy_114 (coe v1))))
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
            (coe d_viewTy_114 (coe v1))))
-- Generics.Utils.viewTy′
d_viewTy'8242'_150 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_viewTy'8242'_150 v0
  = let v1
          = coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
              (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v0) in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v2 v3
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v4 v5
               -> coe
                    MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                    (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2))
                    (d_viewTy'8242'_150 (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> coe v1
-- Generics.Utils.argTys
d_argTys_160 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_argTys_160 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_viewTy'8242'_150 (coe v0))
-- Generics.Utils.resultTy
d_resultTy_162 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_resultTy_162 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_viewTy'8242'_150 (coe v0))
-- Generics.Utils.tyName
d_tyName_164 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Maybe AgdaAny
d_tyName_164 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v2 v3
        -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v2)
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v2 v3
        -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v2)
      _ -> coe v1
-- Generics.Utils.args
d_args_170 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_args_170 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16 in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v2 v3 -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v2 v3 -> coe v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v2 v3 -> coe v3
      _ -> coe v1
-- Generics.Utils.args′
d_args'8242'_178 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146]
d_args'8242'_178 v0 = coe du_unArgs_84 (d_args_170 (coe v0))
-- Generics.Utils._._⁇_
d__'8263'__186 ::
  (Integer -> Integer) -> Integer -> Integer -> Integer
d__'8263'__186 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
      (coe
         MAlonzo.Code.Data.Nat.Base.d__'8804''7495'__10 (coe v1) (coe v2))
      (coe v0 v2) (coe v2)
-- Generics.Utils._.mapFreeVars
d_mapFreeVars_192 ::
  (Integer -> Integer) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_mapFreeVars_192 v0 v1
  = coe d_'46'extendedlambda0_210 (coe v0) (coe v1)
-- Generics.Utils._.mapFreeVars∗
d_mapFreeVars'8727'_194 ::
  (Integer -> Integer) ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_mapFreeVars'8727'_194 v0 v1
  = coe d_'46'extendedlambda1_252 (coe v0) (coe v1)
-- Generics.Utils._.mapFreeVarsᵖ
d_mapFreeVars'7510'_196 ::
  (Integer -> Integer) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_mapFreeVars'7510'_196 v0 v1
  = coe d_'46'extendedlambda2_262 (coe v0) (coe v1)
-- Generics.Utils._.mapFreeVarsᵖ∗
d_mapFreeVars'7510''8727'_198 ::
  (Integer -> Integer) ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_mapFreeVars'7510''8727'_198 v0 v1
  = coe d_'46'extendedlambda3_276 (coe v0) (coe v1)
-- Generics.Utils._.mapFreeVarsᵗ
d_mapFreeVars'7511'_200 ::
  (Integer -> Integer) ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_mapFreeVars'7511'_200 v0 v1
  = coe d_'46'extendedlambda4_286 (coe v0) (coe v1)
-- Generics.Utils._.mapFreeVarsᶜ
d_mapFreeVars'7580'_202 ::
  (Integer -> Integer) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152
d_mapFreeVars'7580'_202 v0 v1
  = coe d_'46'extendedlambda5_298 (coe v0) (coe v1)
-- Generics.Utils._.mapFreeVarsᶜ∗
d_mapFreeVars'7580''8727'_204 ::
  (Integer -> Integer) ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152]
d_mapFreeVars'7580''8727'_204 v0 v1
  = coe d_'46'extendedlambda6_312 (coe v0) (coe v1)
-- Generics.Utils._.mapFreeVarsˢ
d_mapFreeVars'738'_206 ::
  (Integer -> Integer) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Sort_148 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Sort_148
d_mapFreeVars'738'_206 v0 v1
  = coe d_'46'extendedlambda7_320 (coe v0) (coe v1)
-- Generics.Utils._..extendedlambda0
d_'46'extendedlambda0_210 ::
  (Integer -> Integer) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'46'extendedlambda0_210 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164
             (coe d__'8263'__186 (coe v0) (coe v1) (coe v3))
             (coe d_mapFreeVars'8727'_194 v0 v1 v4)
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 (coe v3)
             (coe d_mapFreeVars'8727'_194 v0 v1 v4)
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v3)
             (coe d_mapFreeVars'8727'_194 v0 v1 v4)
      MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v3 v4
        -> case coe v4 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v5 v6
               -> coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 (coe v3)
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 (coe v5)
                       (coe
                          d_mapFreeVars_192 v0 (addInt (coe (1 :: Integer)) (coe v1)) v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188
             (coe d_mapFreeVars'7580''8727'_204 v0 v1 v3)
             (coe d_mapFreeVars'8727'_194 v0 v1 v4)
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
               -> case coe v4 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v7 v8
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194
                           (coe
                              MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v5)
                              (coe d_mapFreeVars_192 v0 v1 v6))
                           (coe
                              MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 (coe v7)
                              (coe
                                 d_mapFreeVars_192 v0 (addInt (coe (1 :: Integer)) (coe v1)) v8))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198
             (coe d_mapFreeVars'738'_206 v0 v1 v3)
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 (coe v3)
             (coe d_mapFreeVars'8727'_194 v0 v1 v4)
      _ -> coe v2
-- Generics.Utils._..extendedlambda1
d_'46'extendedlambda1_252 ::
  (Integer -> Integer) ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_'46'extendedlambda1_252 v0 v1 v2
  = case coe v2 of
      [] -> coe v2
      (:) v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v5)
                       (coe d_mapFreeVars_192 v0 v1 v6))
                    (coe d_mapFreeVars'8727'_194 v0 v1 v4)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._..extendedlambda2
d_'46'extendedlambda2_262 ::
  (Integer -> Integer) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_'46'extendedlambda2_262 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 (coe v3)
             (coe d_mapFreeVars'7510''8727'_198 v0 v1 v4)
      MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240
             (coe d_mapFreeVars_192 v0 v1 v3)
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256
             (coe d__'8263'__186 (coe v0) (coe v1) (coe v3))
      _ -> coe v2
-- Generics.Utils._..extendedlambda3
d_'46'extendedlambda3_276 ::
  (Integer -> Integer) ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_'46'extendedlambda3_276 v0 v1 v2
  = case coe v2 of
      [] -> coe v2
      (:) v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v5)
                       (coe d_mapFreeVars'7510'_196 v0 v1 v6))
                    (coe
                       d_mapFreeVars'7510''8727'_198 v0
                       (addInt (coe (1 :: Integer)) (coe v1)) v4)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._..extendedlambda4
d_'46'extendedlambda4_286 ::
  (Integer -> Integer) ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_'46'extendedlambda4_286 v0 v1 v2
  = case coe v2 of
      [] -> coe v2
      (:) v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
               -> case coe v6 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v7 v8
                      -> coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v5)
                              (coe
                                 MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v7)
                                 (coe d_mapFreeVars_192 v0 v1 v8)))
                           (coe
                              d_mapFreeVars'7511'_200 v0 (addInt (coe (1 :: Integer)) (coe v1))
                              v4)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._..extendedlambda5
d_'46'extendedlambda5_298 ::
  (Integer -> Integer) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152
d_'46'extendedlambda5_298 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v3 v4 v5
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264
             (coe d_mapFreeVars'7511'_200 v0 v1 v3)
             (coe d_mapFreeVars'7510''8727'_198 v0 v1 v4)
             (coe
                d_mapFreeVars_192 v0
                (addInt
                   (coe MAlonzo.Code.Data.List.Base.du_length_304 v3) (coe v1))
                v5)
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270
             (coe d_mapFreeVars'7511'_200 v0 v1 v3)
             (coe d_mapFreeVars'7510''8727'_198 v0 v1 v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._..extendedlambda6
d_'46'extendedlambda6_312 ::
  (Integer -> Integer) ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152]
d_'46'extendedlambda6_312 v0 v1 v2
  = case coe v2 of
      [] -> coe v2
      (:) v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
             (coe d_mapFreeVars'7580'_202 v0 v1 v3)
             (coe d_mapFreeVars'7580''8727'_204 v0 v1 v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._..extendedlambda7
d_'46'extendedlambda7_320 ::
  (Integer -> Integer) ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Sort_148 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Sort_148
d_'46'extendedlambda7_320 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_set_212
             (coe d_mapFreeVars_192 v0 v1 v3)
      MAlonzo.Code.Agda.Builtin.Reflection.C_prop_220 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_prop_220
             (coe d_mapFreeVars_192 v0 v1 v3)
      _ -> coe v2
-- Generics.Utils._.mapVars
d_mapVars_328 ::
  (Integer -> Integer) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_mapVars_328 v0
  = coe d_mapFreeVars_192 (coe v0) (coe (0 :: Integer))
-- Generics.Utils.varsToUnknown
d_varsToUnknown_330 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_varsToUnknown_330 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v1 v2
        -> coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 (coe v1)
             (coe d_varsToUnknown'8242'_332 (coe v2))
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v1)
             (coe d_varsToUnknown'8242'_332 (coe v2))
      _ -> coe v0
-- Generics.Utils.varsToUnknown′
d_varsToUnknown'8242'_332 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_varsToUnknown'8242'_332 v0
  = case coe v0 of
      [] -> coe v0
      (:) v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v3 v4
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v3)
                       (coe d_varsToUnknown_330 (coe v4)))
                    (coe d_varsToUnknown'8242'_332 (coe v2))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.parameters
d_parameters_350 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Definition_272 -> Integer
d_parameters_350 v0
  = let v1 = 0 :: Integer in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v2 v3
        -> coe v2
      _ -> coe v1
-- Generics.Utils.vArgs
d_vArgs_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> [AgdaAny]
d_vArgs_354 ~v0 ~v1 v2 = du_vArgs_354 v2
du_vArgs_354 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> [AgdaAny]
du_vArgs_354 v0
  = case coe v0 of
      [] -> coe v0
      (:) v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v3 v4
               -> case coe v3 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v5 v6
                      -> let v7 = coe du_vArgs_354 (coe v2) in
                         case coe v5 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                             -> case coe v6 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v8 v9
                                    -> case coe v8 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                           -> case coe v9 of
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                  -> coe
                                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                       (coe v4) (coe du_vArgs_354 (coe v2))
                                                _ -> coe v7
                                         _ -> coe v7
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> coe v7
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.argInfo
d_argInfo_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_ArgInfo_76
d_argInfo_362 ~v0 ~v1 v2 = du_argInfo_362 v2
du_argInfo_362 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_ArgInfo_76
du_argInfo_362 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.isVisible?
d_isVisible'63'_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isVisible'63'_368 ~v0 ~v1 v2 = du_isVisible'63'_368 v2
du_isVisible'63'_368 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_isVisible'63'_368 v0
  = coe
      MAlonzo.Code.Reflection.AST.Argument.Visibility.d__'8799'__8
      (coe
         MAlonzo.Code.Reflection.AST.Argument.Information.d_visibility_16
         (coe du_argInfo_362 (coe v0)))
      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
-- Generics.Utils.isInstance?
d_isInstance'63'_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isInstance'63'_374 ~v0 ~v1 v2 = du_isInstance'63'_374 v2
du_isInstance'63'_374 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_isInstance'63'_374 v0
  = coe
      MAlonzo.Code.Reflection.AST.Argument.Visibility.d__'8799'__8
      (coe
         MAlonzo.Code.Reflection.AST.Argument.Information.d_visibility_16
         (coe du_argInfo_362 (coe v0)))
      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_instance'8242'_54)
-- Generics.Utils.isHidden?
d_isHidden'63'_380 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isHidden'63'_380 ~v0 ~v1 v2 = du_isHidden'63'_380 v2
du_isHidden'63'_380 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_isHidden'63'_380 v0
  = coe
      MAlonzo.Code.Reflection.AST.Argument.Visibility.d__'8799'__8
      (coe
         MAlonzo.Code.Reflection.AST.Argument.Information.d_visibility_16
         (coe du_argInfo_362 (coe v0)))
      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
-- Generics.Utils.remove-iArgs
d_remove'45'iArgs_384 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_remove'45'iArgs_384 ~v0 ~v1 v2 = du_remove'45'iArgs_384 v2
du_remove'45'iArgs_384 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
du_remove'45'iArgs_384 v0
  = case coe v0 of
      [] -> coe v0
      (:) v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v3 v4
               -> case coe v3 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v5 v6
                      -> let v7
                               = coe
                                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
                                   (coe du_remove'45'iArgs_384 (coe v2)) in
                         case coe v5 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_instance'8242'_54
                             -> case coe v6 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v8 v9
                                    -> case coe v8 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                           -> case coe v9 of
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                  -> coe du_remove'45'iArgs_384 (coe v2)
                                                _ -> coe v7
                                         _ -> coe v7
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> coe v7
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.hide
d_hide_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88
d_hide_394 ~v0 ~v1 v2 = du_hide_394 v2
du_hide_394 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88
du_hide_394 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v3 v4
               -> case coe v3 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                      -> case coe v4 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v5 v6
                             -> case coe v5 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                    -> case coe v6 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                           -> coe
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                                   (coe
                                                      MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                                   (coe v4))
                                                (coe v2)
                                         _ -> coe v0
                                  _ -> coe v0
                           _ -> MAlonzo.RTE.mazUnreachableError
                    MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52
                      -> case coe v4 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v5 v6
                             -> case coe v5 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                    -> coe seq (coe v6) (coe v0)
                                  _ -> coe v0
                           _ -> MAlonzo.RTE.mazUnreachableError
                    MAlonzo.Code.Agda.Builtin.Reflection.C_instance'8242'_54
                      -> case coe v4 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v5 v6
                             -> case coe v5 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                    -> coe seq (coe v6) (coe v0)
                                  _ -> coe v0
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.∀indices⋯
d_'8704'indices'8943'_404 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'8704'indices'8943'_404 v0 v1
  = case coe v0 of
      [] -> coe v1
      (:) v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194
             (coe du_hide_394 (coe v2))
             (coe
                MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
                (coe ("_" :: Data.Text.Text))
                (coe d_'8704'indices'8943'_404 (coe v3) (coe v1)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.apply⋯
d_apply'8943'_414 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_apply'8943'_414 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v1)
      (coe
         du_remove'45'iArgs_384
         (coe
            MAlonzo.Code.Data.List.Base.du_map_22
            (coe
               (\ v2 ->
                  case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
                      -> case coe v4 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
                             -> coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v5)
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Reflection.C_var_164
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                                        (coe MAlonzo.Code.Data.List.Base.du_length_304 v0)
                                        (addInt
                                           (coe (1 :: Integer))
                                           (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v3))))
                                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError))
            (coe
               MAlonzo.Code.Data.List.Base.du_zip_212
               (MAlonzo.Code.Data.List.Base.d_allFin_436
                  (coe MAlonzo.Code.Data.List.Base.du_length_304 v0))
               v0)))
-- Generics.Utils.fresh-level
d_fresh'45'level_426 :: AgdaAny
d_fresh'45'level_426
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (coe
         MAlonzo.Code.Reflection.TCM.d_newMeta_4
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
            (coe
               (MAlonzo.RTE.QName
                  (14 :: Integer) (10880583612240331187 :: Integer)
                  "Agda.Primitive.Level"
                  (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_unquoteTC_360 () erased)
-- Generics.Utils.withHole
d_withHole_428 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny) ->
  AgdaAny
d_withHole_428 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased (coe MAlonzo.Code.Reflection.TCM.d_newMeta_4 v0)
      (\ v2 ->
         coe
           MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
           erased (coe v1 v2)
           (\ v3 ->
              coe
                MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased v2))
-- Generics.Utils.mkRecord
d_mkRecord_436 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_mkRecord_436 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe
            (\ v1 ->
               case coe v1 of
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
                   -> coe
                        MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264
                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                        (coe
                           MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                           (coe
                              MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                              (coe
                                 MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                 (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                    (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                    (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 (coe v2))))
                        (coe v3)
                 _ -> MAlonzo.RTE.mazUnreachableError))
         (coe v0))
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Generics.Utils.updateField
d_updateField_446 ::
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_updateField_446 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe
            (\ v4 ->
               coe
                 MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                 (coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                    erased
                    (MAlonzo.Code.Reflection.AST.Name.d__'8799'__12 (coe v4) (coe v2)))
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                    (coe
                       MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                             (coe
                                MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 (coe v2))))
                    (coe v3))
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                    (coe
                       MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                             (coe
                                MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 (coe v4))))
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v4)
                       (coe
                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                             (coe
                                MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                             (coe v1))
                          (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))
         (coe v0))
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Generics.Utils.apply
d_apply_458 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_apply_458 v0 v1 v2
  = case coe v2 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
             (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0) (coe v1))
      (:) v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
             erased (coe MAlonzo.Code.Agda.Builtin.Reflection.d_reduce_342 v0)
             (\ v5 ->
                coe
                  MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                  erased (coe du_apply'8321'_476 (coe v5) (coe v1) (coe v3))
                  (coe du_'46'extendedlambda2_530 (coe v4)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._.apply₁
d_apply'8321'_476 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny
d_apply'8321'_476 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_apply'8321'_476 v4 v5 v6
du_apply'8321'_476 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny
du_apply'8321'_476 v0 v1 v2
  = let v3
          = coe
              MAlonzo.Code.Generics.Debug.du_error_8 (coe ())
              (coe ("apply: not a \928-type" :: Data.Text.Text)) in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v4 v5
        -> case coe v4 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v6 v7
               -> case coe v6 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v8 v9
                      -> case coe v2 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v10 v11
                             -> coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                                  erased d_fresh'45'level_426
                                  (\ v12 ->
                                     coe
                                       MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () ()
                                       erased erased d_fresh'45'level_426
                                       (\ v13 ->
                                          coe
                                            MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () ()
                                            erased erased
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Reflection.d_unquoteTC_360
                                               () erased v7)
                                            (\ v14 ->
                                               coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                 () () erased erased
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.d_unquoteTC_360
                                                    () erased
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                                       (coe v5)))
                                                 (\ v15 ->
                                                    coe
                                                      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                      v12 () erased erased
                                                      (coe
                                                         MAlonzo.Code.Agda.Builtin.Reflection.d_unquoteTC_360
                                                         v12 erased v11)
                                                      (\ v16 ->
                                                         coe
                                                           MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                           () () erased erased
                                                           (coe
                                                              MAlonzo.Code.Agda.Builtin.Reflection.d_quoteTC_354
                                                              () erased (coe v15 v16))
                                                           (\ v17 ->
                                                              seq
                                                                (coe v8)
                                                                (coe
                                                                   MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                                   () () erased erased
                                                                   (coe
                                                                      MAlonzo.Code.Agda.Builtin.Reflection.d_unquoteTC_360
                                                                      () erased v1)
                                                                   (\ v18 ->
                                                                      coe
                                                                        MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                                        () () erased erased
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.Reflection.d_quoteTC_354
                                                                           v13 erased (coe v18 v16))
                                                                        (\ v19 ->
                                                                           coe
                                                                             MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316
                                                                             () erased
                                                                             (coe
                                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                (coe v17)
                                                                                (coe v19)))))))))))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v4 v5
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_blockOnMeta_402 () erased v4
      _ -> coe v3
-- Generics.Utils._..extendedlambda2
d_'46'extendedlambda2_530 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_'46'extendedlambda2_530 ~v0 ~v1 ~v2 v3 ~v4 v5
  = du_'46'extendedlambda2_530 v3 v5
du_'46'extendedlambda2_530 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_'46'extendedlambda2_530 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe d_apply_458 (coe v2) (coe v3) (coe v0)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._-∙-_
d__'45''8729''45'__536 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d__'45''8729''45'__536 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_inferType_336 v0)
      (\ v2 ->
         coe
           MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
           erased
           (d_apply_458
              (coe v2) (coe v0)
              (coe
                 MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                       (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                    (coe v1))))
           (\ v3 ->
              coe
                MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v3))))
-- Generics.Utils._-∗-_
d__'45''8727''45'__544 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] -> AgdaAny
d__'45''8727''45'__544 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased v0
      (:) v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
             erased (d__'45''8729''45'__536 (coe v0) (coe v2))
             (\ v4 -> d__'45''8727''45'__544 (coe v4) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils.instantiate
d_instantiate_556 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_instantiate_556 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased (coe MAlonzo.Code.Agda.Builtin.Reflection.d_reduce_342 v0)
      MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340
-- Generics.Utils._._.print
d_print_570 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_print_570
  = coe
      MAlonzo.Code.Generics.Debug.d_print_24
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("Generics.unifyStrict" :: Data.Text.Text))
         (coe (100 :: Integer)))
-- Generics.Utils._._.printLn
d_printLn_576 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_printLn_576
  = coe
      MAlonzo.Code.Generics.Debug.d_printLn_26
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("Generics.unifyStrict" :: Data.Text.Text))
         (coe (100 :: Integer)))
-- Generics.Utils._._.printTerm
d_printTerm_582 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_printTerm_582
  = coe
      MAlonzo.Code.Generics.Debug.d_printTerm_42
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("Generics.unifyStrict" :: Data.Text.Text))
         (coe (100 :: Integer)))
-- Generics.Utils._.ensureNoMetas
d_ensureNoMetas_584 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_ensureNoMetas_584 = coe d_'46'extendedlambda2_622
-- Generics.Utils._._.noMetaArg
d_noMetaArg_590 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny
d_noMetaArg_590 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v1 v2
        -> coe d_ensureNoMetas_584 v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._._.noMetaArgs
d_noMetaArgs_594 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_noMetaArgs_594 v0
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
             (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      (:) v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
             erased (d_noMetaArg_590 (coe v1))
             (\ v3 -> d_noMetaArgs_594 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._._.noMetaClause
d_noMetaClause_600 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152 -> AgdaAny
d_noMetaClause_600 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v1 v2 v3
        -> coe d_ensureNoMetas_584 v3
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
             (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._._.noMetaClauses
d_noMetaClauses_608 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
d_noMetaClauses_608 v0
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
             (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      (:) v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
             erased (d_noMetaClause_600 (coe v1))
             (\ v3 -> d_noMetaClauses_608 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._._.noMetaAbs
d_noMetaAbs_614 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104 -> AgdaAny
d_noMetaAbs_614 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v1 v2
        -> coe d_ensureNoMetas_584 v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._._.noMetaSort
d_noMetaSort_618 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Sort_148 -> AgdaAny
d_noMetaSort_618 v0
  = let v1
          = coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
              (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8) in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_set_212 v2
        -> coe d_ensureNoMetas_584 v2
      _ -> coe v1
-- Generics.Utils._._..extendedlambda2
d_'46'extendedlambda2_622 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_'46'extendedlambda2_622 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v1 v2
        -> coe d_noMetaArgs_594 (coe v2)
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v1 v2
        -> coe d_noMetaArgs_594 (coe v2)
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v1 v2
        -> coe d_noMetaArgs_594 (coe v2)
      MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v1 v2
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v3 v4
               -> coe d_ensureNoMetas_584 v4
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
             erased (d_noMetaClauses_608 (coe v1))
             (\ v3 -> d_noMetaArgs_594 (coe v2))
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
             erased (d_noMetaArg_590 (coe v1))
             (\ v3 -> d_noMetaAbs_614 (coe v2))
      MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v1
        -> coe d_noMetaSort_618 (coe v1)
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_202 v1
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
             (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_blockOnMeta_402 () erased v1
      MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
             (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._.NewMeta.unifyStrict
d_unifyStrict_656 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_unifyStrict_656 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
             erased
             (MAlonzo.Code.Generics.Debug.d_printLn_26
                (coe
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                   (coe ("Generics.unifyStrict" :: Data.Text.Text))
                   (coe (100 :: Integer)))
                (coe
                   MAlonzo.Code.Data.String.Base.d__'43''43'__20
                   (MAlonzo.Code.Reflection.AST.Show.d_showTerm_38 (coe v2))
                   (coe
                      MAlonzo.Code.Data.String.Base.d__'43''43'__20
                      (" :=? " :: Data.Text.Text)
                      (MAlonzo.Code.Reflection.AST.Show.d_showTerm_38 (coe v1)))))
             (\ v4 ->
                coe
                  MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                  erased (coe MAlonzo.Code.Reflection.TCM.d_newMeta_4 v3)
                  (\ v5 ->
                     coe
                       MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                       erased
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.d_noConstraints_440 () erased
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                             erased (coe MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328 v5 v1)
                             (\ v6 ->
                                coe MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328 v2 v5)))
                       (\ v6 ->
                          MAlonzo.Code.Generics.Debug.d_printLn_26
                            (coe
                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                               (coe ("Generics.unifyStrict" :: Data.Text.Text))
                               (coe (100 :: Integer)))
                            (coe
                               MAlonzo.Code.Data.String.Base.d__'43''43'__20
                               (MAlonzo.Code.Reflection.AST.Show.d_showTerm_38 (coe v2))
                               (coe
                                  MAlonzo.Code.Data.String.Base.d__'43''43'__20
                                  (" := " :: Data.Text.Text)
                                  (MAlonzo.Code.Reflection.AST.Show.d_showTerm_38 (coe v1)))))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._.NoMeta.unifyStrict
d_unifyStrict_668 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_unifyStrict_668 v0 v1
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
             erased
             (MAlonzo.Code.Generics.Debug.d_print_24
                (coe
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                   (coe ("Generics.unifyStrict" :: Data.Text.Text))
                   (coe (100 :: Integer)))
                (coe
                   ("\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212\8212"
                    ::
                    Data.Text.Text)))
             (\ v4 ->
                coe
                  MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                  erased
                  (MAlonzo.Code.Generics.Debug.d_printTerm_42
                     (coe
                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                        (coe ("Generics.unifyStrict" :: Data.Text.Text))
                        (coe (100 :: Integer)))
                     (coe ("x" :: Data.Text.Text)) (coe v1))
                  (\ v5 ->
                     coe
                       MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                       erased (coe MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328 v2 v1)
                       (\ v6 ->
                          coe
                            MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                            erased
                            (coe MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340 v2)
                            (\ v7 ->
                               coe
                                 MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                                 erased
                                 (MAlonzo.Code.Generics.Debug.d_printTerm_42
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                       (coe ("Generics.unifyStrict" :: Data.Text.Text))
                                       (coe (100 :: Integer)))
                                    (coe ("hole\8242" :: Data.Text.Text)) (coe v7))
                                 (\ v8 ->
                                    coe
                                      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                                      erased (coe d_ensureNoMetas_584 v7)
                                      (\ v9 ->
                                         MAlonzo.Code.Generics.Debug.d_printLn_26
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                              (coe ("Generics.unifyStrict" :: Data.Text.Text))
                                              (coe (100 :: Integer)))
                                           (coe ("No metas found :)" :: Data.Text.Text))))))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Utils._.unifyStricts
d_unifyStricts_678 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] -> AgdaAny
d_unifyStricts_678 v0 v1
  = coe
      MAlonzo.Code.Data.List.NonEmpty.Base.du_foldl'8321'_174
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_catchTC_348 () erased)
      (coe
         MAlonzo.Code.Data.List.NonEmpty.Base.du__'8759''691'__276
         (coe
            MAlonzo.Code.Data.List.Base.du_map_22
            (coe d_unifyStrict_656 (coe v0)) (coe v1))
         (coe
            MAlonzo.Code.Generics.Debug.du_error_8 (coe ())
            (coe ("\8709" :: Data.Text.Text))))
-- Generics.Utils.compatible?
d_compatible'63'_684 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_compatible'63'_684 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_runSpeculative_448 () erased
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
            erased
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.d_catchTC_348 () erased
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                  erased
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328
                     (d_varsToUnknown_330 (coe v0)) (d_varsToUnknown_330 (coe v1)))
                  (\ v2 ->
                     coe
                       MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                       (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)))
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                  (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)))
            (\ v2 ->
               coe
                 MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                 (coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)))))
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased)
