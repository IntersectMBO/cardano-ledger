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

module MAlonzo.Code.Tactic.ReduceDec where

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
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Generics.Utils
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Interface.MonadTC
import qualified MAlonzo.Code.Reflection.Debug
import qualified MAlonzo.Code.Reflection.TCI
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Tactic.Assumption
import qualified MAlonzo.Code.Tactic.Constrs
import qualified MAlonzo.Code.Tactic.Helpers

-- Tactic.ReduceDec.selectSubterms
d_selectSubterms_8 ::
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146]
d_selectSubterms_8 v0 v1
  = coe
      MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v1)
      (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
      (coe d_helper_18 (coe v0) (coe v1) (coe v1))
-- Tactic.ReduceDec._.helper
d_helper_18 ::
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146]
d_helper_18 v0 v1 v2
  = let v3 = coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16 in
    case coe v2 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v4 v5
        -> coe d_argHelper_20 (coe v0) (coe v1) (coe v5)
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v4 v5
        -> coe d_argHelper_20 (coe v0) (coe v1) (coe v5)
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v4 v5
        -> coe d_argHelper_20 (coe v0) (coe v1) (coe v5)
      MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v4 v5
        -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v4 v5
        -> coe d_argHelper_20 (coe v0) (coe v1) (coe v5)
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v4 v5
        -> case coe v4 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v6 v7
               -> coe
                    MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v7)
                    (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v7))
                    (coe d_helper_18 (coe v0) (coe v1) (coe v7))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v4 v5
        -> coe d_argHelper_20 (coe v0) (coe v1) (coe v5)
      _ -> coe v3
-- Tactic.ReduceDec._.argHelper
d_argHelper_20 ::
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146]
d_argHelper_20 v0 v1 v2
  = case coe v2 of
      [] -> coe v2
      (:) v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
               -> coe
                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                    (coe
                       MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v6)
                       (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v6))
                       (coe d_helper_18 (coe v0) (coe v1) (coe v6)))
                    (coe d_argHelper_20 (coe v0) (coe v1) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ReduceDec.generalizeSubterms
d_generalizeSubterms_52 ::
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_generalizeSubterms_52 v0 v1
  = coe
      d_lambdas_104 (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v1)
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe (0 :: Integer))
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
         (coe
            d_helper_62 (coe v0) (coe v1)
            (coe
               MAlonzo.Code.Generics.Utils.d_mapVars_328
               (\ v2 -> addInt (coe (1 :: Integer)) (coe v2)) v1)))
-- Tactic.ReduceDec._.helper
d_helper_62 ::
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_helper_62 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v3)
             (coe d_argHelper_64 (coe v0) (coe v1) (coe v4))
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 (coe v3)
             (coe d_argHelper_64 (coe v0) (coe v1) (coe v4))
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v3)
             (coe d_argHelper_64 (coe v0) (coe v1) (coe v4))
      MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v3 v4 -> coe v2
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 (coe v3)
             (coe d_argHelper_64 (coe v0) (coe v1) (coe v4))
      MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194 v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
               -> coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_pi_194
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v5)
                       (coe
                          MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v6)
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe (0 :: Integer))
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                          (coe d_helper_62 (coe v0) (coe v1) (coe v6))))
                    (coe v4)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 (coe v3)
             (coe d_argHelper_64 (coe v0) (coe v1) (coe v4))
      _ -> coe v2
-- Tactic.ReduceDec._.argHelper
d_argHelper_64 ::
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_argHelper_64 v0 v1 v2
  = case coe v2 of
      [] -> coe v2
      (:) v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v5)
                       (coe
                          MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v6)
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe (0 :: Integer))
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                          (coe d_helper_62 (coe v0) (coe v1) (coe v6))))
                    (coe d_argHelper_64 (coe v0) (coe v1) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ReduceDec._.lambdas
d_lambdas_104 ::
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_lambdas_104 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Base.du_foldl_256
      (coe
         (\ v3 v4 ->
            coe
              MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182
              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
              (coe
                 MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
                 (coe ("g" :: Data.Text.Text)) (coe v3))))
      (coe v2) (coe d_selectSubterms_8 (coe v0) (coe v1))
-- Tactic.ReduceDec.extractDec
d_extractDec_112 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_extractDec_112 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v2 v3
        -> case coe v2 of
             MAlonzo.RTE.QName 92 16368259409245829246 _ _
               -> case coe v3 of
                    (:) v4 v5
                      -> case coe v5 of
                           (:) v6 v7
                             -> case coe v7 of
                                  (:) v8 v9
                                    -> case coe v8 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v10 v11
                                           -> case coe v10 of
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v12 v13
                                                  -> case coe v12 of
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                                         -> case coe v13 of
                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v14 v15
                                                                -> case coe v14 of
                                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                                       -> case coe v15 of
                                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                              -> case coe v9 of
                                                                                   []
                                                                                     -> coe
                                                                                          MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                                                                                          (coe v11)
                                                                                   _ -> coe v1
                                                                            _ -> coe v1
                                                                     _ -> coe v1
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> coe v1
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> coe v1
                           _ -> coe v1
                    _ -> coe v1
             _ -> coe v1
      _ -> coe v1
-- Tactic.ReduceDec.isIsYes
d_isIsYes_118 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool
d_isIsYes_118 v0
  = coe
      MAlonzo.Code.Data.Maybe.Base.du_is'45'just_20
      (coe d_extractDec_112 (coe v0))
-- Tactic.ReduceDec.fromWitness'
d_fromWitness''_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fromWitness''_126 = erased
-- Tactic.ReduceDec.fromWitnessFalse'
d_fromWitnessFalse''_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fromWitnessFalse''_142 = erased
-- Tactic.ReduceDec.fromWitness'Type
d_fromWitness''Type_152 ::
  Bool ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_fromWitness''Type_152 v0 v1
  = if coe v0
      then coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
             (coe
                (MAlonzo.RTE.QName
                   (12 :: Integer) (1335258922519917603 :: Integer)
                   "Agda.Builtin.Equality._\8801_"
                   (MAlonzo.RTE.Fixity
                      MAlonzo.RTE.NonAssoc (MAlonzo.RTE.Related (4.0 :: Double)))))
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                         (coe
                            MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
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
                         (coe
                            MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                            (coe
                               (MAlonzo.RTE.QName
                                  (92 :: Integer) (16368259409245829246 :: Integer)
                                  "Relation.Nullary.Decidable.Core.isYes"
                                  (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
                            (coe
                               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                               (coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                        (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                               (coe
                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                        (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                           (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                           (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                        (coe v1))
                                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))
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
                            (coe
                               MAlonzo.Code.Agda.Builtin.Reflection.C_con_170
                               (coe
                                  (MAlonzo.RTE.QName
                                     (10 :: Integer) (4305008439024043551 :: Integer)
                                     "Agda.Builtin.Bool.Bool.true"
                                     (MAlonzo.RTE.Fixity
                                        MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
                               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
      else coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
             (coe
                (MAlonzo.RTE.QName
                   (12 :: Integer) (1335258922519917603 :: Integer)
                   "Agda.Builtin.Equality._\8801_"
                   (MAlonzo.RTE.Fixity
                      MAlonzo.RTE.NonAssoc (MAlonzo.RTE.Related (4.0 :: Double)))))
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                         (coe
                            MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
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
                         (coe
                            MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                            (coe
                               (MAlonzo.RTE.QName
                                  (92 :: Integer) (16368259409245829246 :: Integer)
                                  "Relation.Nullary.Decidable.Core.isYes"
                                  (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
                            (coe
                               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                               (coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                        (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                               (coe
                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                        (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                           (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                           (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                        (coe v1))
                                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))
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
                            (coe
                               MAlonzo.Code.Agda.Builtin.Reflection.C_con_170
                               (coe
                                  (MAlonzo.RTE.QName
                                     (8 :: Integer) (4305008439024043551 :: Integer)
                                     "Agda.Builtin.Bool.Bool.false"
                                     (MAlonzo.RTE.Fixity
                                        MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
                               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
-- Tactic.ReduceDec.findDecRWHypWith
d_findDecRWHypWith_158 ::
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_findDecRWHypWith_158 v0 v1
  = coe
      d_helper_168 (coe v0) (coe v1)
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
      (coe
         d_helper_168 (coe v0) (coe v1)
         (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
         (coe
            MAlonzo.Code.Interface.MonadTC.du_error1_664
            (coe
               MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
               (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
            (coe ())
            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
            (coe
               ("reduceDec: Could not find an equation to rewrite with!"
                ::
                Data.Text.Text))))
-- Tactic.ReduceDec._.helper
d_helper_168 ::
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Bool ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_helper_168 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.MonadTC.du_runSpeculativeMaybe_692
      (coe
         MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
      (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154) (coe ())
      (coe
         (\ v4 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
              (coe
                 MAlonzo.Code.Interface.MonadTC.du_newMeta_308
                 MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                 (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208) v4)
              (\ v5 ->
                 coe
                   MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                   MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                   (if coe v2
                      then coe
                             MAlonzo.Code.Interface.MonadTC.d_checkType_164
                             MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                             (coe
                                MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                (coe
                                   (MAlonzo.RTE.QName
                                      (126 :: Integer) (1919047259428906691 :: Integer)
                                      "Tactic.ReduceDec.fromWitness'"
                                      (MAlonzo.RTE.Fixity
                                         MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
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
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                      (coe v1))
                                   (coe
                                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                      (coe
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                         (coe
                                            MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                         (coe v5))
                                      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                             (d_fromWitness''Type_152 (coe v2) (coe v1)) v4
                      else coe
                             MAlonzo.Code.Interface.MonadTC.d_checkType_164
                             MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                             (coe
                                MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                (coe
                                   (MAlonzo.RTE.QName
                                      (142 :: Integer) (1919047259428906691 :: Integer)
                                      "Tactic.ReduceDec.fromWitnessFalse'"
                                      (MAlonzo.RTE.Fixity
                                         MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
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
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                      (coe v1))
                                   (coe
                                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                      (coe
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                         (coe
                                            MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                         (coe v5))
                                      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                             (d_fromWitness''Type_152 (coe v2) (coe v1)) v4)
                   (\ v6 ->
                      coe
                        MAlonzo.Code.Agda.Builtin.Reflection.d_catchTC_348 () erased
                        (coe
                           MAlonzo.Code.Interface.Monad.du__'62''62'__32
                           (coe
                              MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                              (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                           () ()
                           (coe
                              MAlonzo.Code.Interface.MonadTC.du_runWithHole_764
                              (coe
                                 MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                 (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                              () v5 v0)
                           (coe
                              MAlonzo.Code.Interface.Monad.du__'62''62'__32
                              (coe
                                 MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                 (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                              (coe ()) (coe ())
                              (coe
                                 MAlonzo.Code.Interface.MonadTC.du_debugLog'7504'_508
                                 (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                 (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                 (coe
                                    MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                    (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                 (coe
                                    MAlonzo.Code.Interface.MonadTC.du__'8759''7496''7504'__470
                                    (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                    (coe ("Hypothesis is " :: Data.Text.Text))
                                    (coe
                                       MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'IsErrorPart_452
                                       (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                       (coe
                                          MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22))
                                    (coe
                                       MAlonzo.Code.Interface.MonadTC.du__'8759''7496''7504'__470
                                       (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                       (coe
                                          MAlonzo.Code.Interface.MonadTC.du__'7515''8319'_486
                                          (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                          (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                          (coe
                                             MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                             (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                          (coe ()) (coe v2))
                                       (coe
                                          MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'MErrorPartWrap_462)
                                       (coe
                                          MAlonzo.Code.Interface.Monad.d_return_28
                                          MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18 () erased
                                          (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
                              (coe
                                 (\ v7 ->
                                    coe
                                      MAlonzo.Code.Interface.Monad.d_return_28
                                      MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased
                                      (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v6)))))
                           v4)
                        (coe
                           MAlonzo.Code.Interface.Monad.d_return_28
                           MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased
                           (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18))))))
      (coe v3)
-- Tactic.ReduceDec.reduceDecTermWith
d_reduceDecTermWith_182 ::
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_ReductionOptions_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_reduceDecTermWith_182 v0 v1 v2
  = coe
      MAlonzo.Code.Tactic.Helpers.du_inDebugPath_132
      (coe
         MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
      (coe
         MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
         (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
      (coe
         MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
      (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154) (coe ())
      (coe ("reduceDec" :: Data.Text.Text))
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
              (coe
                 MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                 MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                 (coe
                    MAlonzo.Code.Interface.MonadTC.d_inferType_162
                    MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v2
                    (coe
                       MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949
                       (coe MAlonzo.Code.Interface.MonadTC.d_normalisation_40 (coe v3))
                       (coe MAlonzo.Code.Interface.MonadTC.d_reconstruction_42 (coe v3))
                       (coe MAlonzo.Code.Interface.MonadTC.d_noConstraints_44 (coe v3))
                       (coe v1)
                       (coe MAlonzo.Code.Interface.MonadTC.d_globalContext_48 (coe v3))
                       (coe MAlonzo.Code.Interface.MonadTC.d_localContext_50 (coe v3))
                       (coe MAlonzo.Code.Interface.MonadTC.d_goal_52 (coe v3))
                       (coe MAlonzo.Code.Interface.MonadTC.d_debug_54 (coe v3))))
                 (\ v4 ->
                    coe
                      MAlonzo.Code.Interface.MonadTC.d_normalise_166
                      MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v4
                      (coe
                         MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949
                         (coe MAlonzo.Code.Interface.MonadTC.d_normalisation_40 (coe v3))
                         (coe MAlonzo.Code.Interface.MonadTC.d_reconstruction_42 (coe v3))
                         (coe MAlonzo.Code.Interface.MonadTC.d_noConstraints_44 (coe v3))
                         (coe v1)
                         (coe MAlonzo.Code.Interface.MonadTC.d_globalContext_48 (coe v3))
                         (coe MAlonzo.Code.Interface.MonadTC.d_localContext_50 (coe v3))
                         (coe MAlonzo.Code.Interface.MonadTC.d_goal_52 (coe v3))
                         (coe MAlonzo.Code.Interface.MonadTC.d_debug_54 (coe v3)))))
              (\ v4 ->
                 coe
                   MAlonzo.Code.Interface.Monad.du__'62''62'__32
                   (coe
                      MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                      (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                   () ()
                   (coe
                      MAlonzo.Code.Interface.MonadTC.du_debugLog_500
                      (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                      (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                      (coe
                         MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                      (coe
                         MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                         (coe ("Reduce dec in " :: Data.Text.Text))
                         (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                         (coe
                            MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v2)
                            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                            (coe
                               MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                               (coe (" : " :: Data.Text.Text))
                               (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                               (coe
                                  MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v4)
                                  (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))
                   (\ v5 ->
                      coe
                        MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                        MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                        (coe
                           MAlonzo.Code.Interface.Monad.d_return_28
                           MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased
                           (coe
                              MAlonzo.Code.Data.List.Base.du_mapMaybe_32 (coe d_extractDec_112)
                              (coe d_selectSubterms_8 (coe d_isIsYes_118) (coe v4))))
                        (\ v6 ->
                           let v7
                                 = coe
                                     MAlonzo.Code.Interface.MonadTC.du_error1_664
                                     (coe
                                        MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                        (coe
                                           MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                     (coe ())
                                     (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                     (coe
                                        ("No subterms of the form 'isYes t' found!"
                                         ::
                                         Data.Text.Text)) in
                           case coe v6 of
                             (:) v8 v9
                               -> coe
                                    MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                    (coe
                                       MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                       (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                    () ()
                                    (coe
                                       MAlonzo.Code.Interface.MonadTC.du_debugLog_500
                                       (coe
                                          MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                          (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                       (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                       (coe
                                          MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                          (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                       (coe
                                          MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                          (coe ("Rewrite scheme: " :: Data.Text.Text))
                                          (coe
                                             MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                          (coe
                                             MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                             (coe
                                                d_generalizeSubterms_52 (coe d_isIsYes_118)
                                                (coe v4))
                                             (coe
                                                MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                                    (coe
                                       MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                       (coe
                                          MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                          (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                       (coe ()) (coe ())
                                       (coe
                                          MAlonzo.Code.Interface.MonadTC.du_debugLog_500
                                          (coe
                                             MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                             (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                          (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                          (coe
                                             MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                             (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                          (coe
                                             MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                             (coe ("`isYes` to reduce: " :: Data.Text.Text))
                                             (coe
                                                MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                             (coe
                                                MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                (coe v8)
                                                (coe
                                                   MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                                       (coe
                                          (\ v10 ->
                                             coe
                                               MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                               MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 ()
                                               erased () erased
                                               (coe d_findDecRWHypWith_158 v0 v8 v10)
                                               (\ v11 ->
                                                  coe
                                                    MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                                    MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12
                                                    () erased () erased
                                                    (coe
                                                       MAlonzo.Code.Interface.MonadTC.d_inferType_162
                                                       MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                                       v11 v10)
                                                    (\ v12 ->
                                                       coe
                                                         MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                         (coe
                                                            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                            (coe
                                                               MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                         () ()
                                                         (coe
                                                            MAlonzo.Code.Interface.MonadTC.du_debugLog_500
                                                            (coe
                                                               MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                                            (coe
                                                               MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                                            (coe
                                                               MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                                               (coe
                                                                  MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                            (coe
                                                               MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                               (coe ("Eq: " :: Data.Text.Text))
                                                               (coe
                                                                  MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                               (coe
                                                                  MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                  (coe v11)
                                                                  (coe
                                                                     MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                  (coe
                                                                     MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                     (coe (" : " :: Data.Text.Text))
                                                                     (coe
                                                                        MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                     (coe
                                                                        MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                        (coe v12)
                                                                        (coe
                                                                           MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                        (coe
                                                                           MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))
                                                         (\ v13 ->
                                                            coe
                                                              MAlonzo.Code.Interface.Monad.d_return_28
                                                              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12
                                                              () erased
                                                              (coe
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                 (coe
                                                                    d_generalizeSubterms_52
                                                                    (coe d_isIsYes_118) (coe v4))
                                                                 (coe v11)))
                                                         v10)))))
                                    v5
                             _ -> coe v7 v5))
                   v3)))
-- Tactic.ReduceDec.reduceDecTerm
d_reduceDecTerm_206 ::
  MAlonzo.Code.Interface.MonadTC.T_ReductionOptions_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_reduceDecTerm_206
  = coe
      d_reduceDecTermWith_182
      (coe
         MAlonzo.Code.Tactic.Constrs.d_tryConstrsWith''_18
         (coe (10 :: Integer))
         (coe MAlonzo.Code.Tactic.Assumption.d_assumption''_20))
-- Tactic.ReduceDec.reduceDec'
d_reduceDec''_208 ::
  MAlonzo.Code.Interface.MonadTC.T_ReductionOptions_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_reduceDec''_208 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
      MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
      (coe d_reduceDecTerm_206 v0 v1 v2)
      (\ v3 ->
         case coe v3 of
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
             -> coe
                  MAlonzo.Code.Interface.Monad.d_return_28
                  MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                     (coe
                        (MAlonzo.RTE.QName
                           (98 :: Integer) (6189151057044369179 :: Integer)
                           "Relation.Binary.PropositionalEquality.Core.subst"
                           (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
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
                           (coe v4))
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
                              (coe v5))
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
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                 (coe v1))
                              (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Tactic.ReduceDec.reduceDec
d_reduceDec_220 ::
  MAlonzo.Code.Interface.MonadTC.T_ReductionOptions_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_reduceDec_220 v0 v1
  = coe
      MAlonzo.Code.Interface.Monad.du__'61''60''60'__40
      (coe
         MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
      (coe ()) (coe ())
      (coe
         MAlonzo.Code.Interface.MonadTC.du_unifyWithGoal_756
         (coe
            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
         (coe
            MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
            (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
         (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
         (coe
            MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12)))
      (coe d_reduceDec''_208 (coe v0) (coe v1))
-- Tactic.ReduceDec.reduceDecInGoal
d_reduceDecInGoal_226 ::
  MAlonzo.Code.Interface.MonadTC.T_ReductionOptions_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_reduceDecInGoal_226 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
      MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
      (coe
         MAlonzo.Code.Interface.MonadReader.du_reader_78 ()
         (coe
            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
         (coe
            MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
         () (\ v3 -> MAlonzo.Code.Interface.MonadTC.d_goal_52 (coe v3)) v2)
      (\ v3 ->
         let v4
               = coe
                   MAlonzo.Code.Interface.MonadTC.du_error1_664
                   (coe
                      MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                      (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                   (coe ())
                   (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                   (coe ("Goal is not a hole!" :: Data.Text.Text)) in
         case coe v3 of
           MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5
             -> coe
                  MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                  MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                  (coe d_reduceDecTerm_206 v0 v5 v2)
                  (\ v6 ->
                     case coe v6 of
                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
                         -> coe
                              MAlonzo.Code.Interface.MonadTC.du_unifyWithGoal_756
                              (coe
                                 MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                 (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                              (coe
                                 MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                 (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                              MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                              (coe
                                 MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                 (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                              (coe
                                 MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                 (coe
                                    (MAlonzo.RTE.QName
                                       (98 :: Integer) (6189151057044369179 :: Integer)
                                       "Relation.Binary.PropositionalEquality.Core.subst"
                                       (MAlonzo.RTE.Fixity
                                          MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                             (coe
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                             (coe
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                       (coe v7))
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                             (coe
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                             (coe
                                                (MAlonzo.RTE.QName
                                                   (92 :: Integer) (6189151057044369179 :: Integer)
                                                   "Relation.Binary.PropositionalEquality.Core.sym"
                                                   (MAlonzo.RTE.Fixity
                                                      MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
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
                                                   (coe v8))
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
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
                                             (coe v1))
                                          (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
                              v2
                       _ -> MAlonzo.RTE.mazUnreachableError)
           _ -> coe v4 v2)
-- Tactic.ReduceDec._.by-reduceDec
d_by'45'reduceDec_248 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_by'45'reduceDec_248 v0 v1
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe
         d_reduceDec_220 (coe MAlonzo.Code.Interface.MonadTC.d_reduceAll_20)
         (coe v1))
-- Tactic.ReduceDec._.by-reduceDecInGoal
d_by'45'reduceDecInGoal_252 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_by'45'reduceDecInGoal_252 v0 v1
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe
         d_reduceDecInGoal_226
         (coe MAlonzo.Code.Interface.MonadTC.d_reduceAll_20) (coe v1))
-- Tactic.ReduceDec._.by-reduceDec'
d_by'45'reduceDec''_256 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Interface.MonadTC.T_ReductionOptions_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_by'45'reduceDec''_256 v0 v1 v2
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe d_reduceDec_220 (coe v1) (coe v2))
-- Tactic.ReduceDec._.by-reduceDecInGoal'
d_by'45'reduceDecInGoal''_262 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Interface.MonadTC.T_ReductionOptions_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_by'45'reduceDecInGoal''_262 v0 v1 v2
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe d_reduceDecInGoal_226 (coe v1) (coe v2))
