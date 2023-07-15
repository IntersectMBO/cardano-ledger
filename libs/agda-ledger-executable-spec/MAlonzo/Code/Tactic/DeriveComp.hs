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

module MAlonzo.Code.Tactic.DeriveComp where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Nat
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.NonEmpty.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Nat.Show
import qualified MAlonzo.Code.Data.String.Base
import qualified MAlonzo.Code.Generics.Utils
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.Monad.Instance
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Interface.MonadTC
import qualified MAlonzo.Code.Reflection.AST.Abstraction
import qualified MAlonzo.Code.Reflection.AST.Argument
import qualified MAlonzo.Code.Reflection.AST.Argument.Visibility
import qualified MAlonzo.Code.Reflection.Debug
import qualified MAlonzo.Code.Reflection.TCI
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Tactic.ClauseBuilder
import qualified MAlonzo.Code.Tactic.Helpers
import qualified MAlonzo.Code.Tactic.ReduceDec

-- Tactic.DeriveComp.STSConstr
d_STSConstr_6 = ()
data T_STSConstr_6
  = C_STSConstr'46'constructor_239 AgdaAny
                                   [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
                                   [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146]
                                   MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
                                   MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
                                   MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
                                   MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
-- Tactic.DeriveComp.STSConstr.name
d_name_22 :: T_STSConstr_6 -> AgdaAny
d_name_22 v0
  = case coe v0 of
      C_STSConstr'46'constructor_239 v1 v2 v3 v4 v5 v6 v7 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.STSConstr.params
d_params_24 ::
  T_STSConstr_6 -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
d_params_24 v0
  = case coe v0 of
      C_STSConstr'46'constructor_239 v1 v2 v3 v4 v5 v6 v7 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.STSConstr.clauses
d_clauses_26 ::
  T_STSConstr_6 -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146]
d_clauses_26 v0
  = case coe v0 of
      C_STSConstr'46'constructor_239 v1 v2 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.STSConstr.context
d_context_28 ::
  T_STSConstr_6 -> MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_context_28 v0
  = case coe v0 of
      C_STSConstr'46'constructor_239 v1 v2 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.STSConstr.state
d_state_30 ::
  T_STSConstr_6 -> MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_state_30 v0
  = case coe v0 of
      C_STSConstr'46'constructor_239 v1 v2 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.STSConstr.signal
d_signal_32 ::
  T_STSConstr_6 -> MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_signal_32 v0
  = case coe v0 of
      C_STSConstr'46'constructor_239 v1 v2 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.STSConstr.result
d_result_34 ::
  T_STSConstr_6 -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_result_34 v0
  = case coe v0 of
      C_STSConstr'46'constructor_239 v1 v2 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.conOrVarToPattern
d_conOrVarToPattern_36 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_conOrVarToPattern_36 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v3 v4
        -> case coe v4 of
             []
               -> coe
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_var_244
                       (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v3 v0))
             _ -> coe v2
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v3 v4
        -> coe
             MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
             (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'Maybe_38)
             (coe ()) (coe ())
             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 (coe v3))
             (coe
                MAlonzo.Code.Interface.Monad.du_sequenceList_84
                MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'Maybe_38 ()
                (coe du_conOrVarToPattern'8242'_52 (coe v0) (coe v4)))
      _ -> coe v2
-- Tactic.DeriveComp._.conOrVarToPattern′
d_conOrVarToPattern'8242'_52 ::
  Integer ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_conOrVarToPattern'8242'_52 ~v0 ~v1 ~v2 v3 v4
  = du_conOrVarToPattern'8242'_52 v3 v4
du_conOrVarToPattern'8242'_52 ::
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
du_conOrVarToPattern'8242'_52 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v4 v5
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                       (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'Maybe_38)
                       (coe ()) (coe ())
                       (coe MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v4))
                       (coe d_conOrVarToPattern_36 (coe v0) (coe v5)))
                    (coe du_conOrVarToPattern'8242'_52 (coe v0) (coe v3))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.isArg
d_isArg_66 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isArg_66 v0
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_'172''63'_56
      (coe
         MAlonzo.Code.Reflection.AST.Argument.Visibility.d__'8799'__8
         (coe
            MAlonzo.Code.Generics.Utils.du_getVisibility_20
            (coe MAlonzo.Code.Reflection.AST.Abstraction.du_unAbs_36 (coe v0)))
         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50))
-- Tactic.DeriveComp.toSTSConstr
d_toSTSConstr_70 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_toSTSConstr_70 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
               -> let v5
                        = coe
                            MAlonzo.Code.Interface.MonadTC.du_error1_664
                            (coe
                               MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                               (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                            (coe ())
                            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                            (coe ("toSTSConstr: wrong constructor" :: Data.Text.Text)) in
                  case coe v4 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v6 v7
                      -> let v8
                               = coe
                                   MAlonzo.Code.Data.List.Base.du_mapMaybe_32
                                   (coe
                                      (\ v8 ->
                                         d_conOrVarToPattern_36
                                           (coe
                                              MAlonzo.Code.Data.List.Base.du_length_304
                                              (coe
                                                 MAlonzo.Code.Data.List.Base.du_dropWhile_786
                                                 d_isArg_66 v3))
                                           (coe
                                              MAlonzo.Code.Reflection.AST.Argument.du_unArg_74
                                              (coe v8))))
                                   (coe
                                      MAlonzo.Code.Data.List.Base.du_take_576 (coe (3 :: Integer))
                                      (coe v7)) in
                         let v9
                               = coe
                                   MAlonzo.Code.Interface.MonadTC.du_error1_664
                                   (coe
                                      MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                      (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                   (coe ())
                                   (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                   (coe
                                      MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                                      (coe
                                         ("toSTSConstr: wrong number of arguments:"
                                          ::
                                          Data.Text.Text))
                                      (coe
                                         MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                                         (coe
                                            MAlonzo.Code.Data.Nat.Show.d_show_56
                                            (coe MAlonzo.Code.Data.List.Base.du_length_304 v7))
                                         (coe
                                            MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                                            (coe ("," :: Data.Text.Text))
                                            (coe
                                               MAlonzo.Code.Data.Nat.Show.d_show_56
                                               (coe
                                                  MAlonzo.Code.Data.List.Base.du_length_304
                                                  v8))))) in
                         case coe v7 of
                           (:) v10 v11
                             -> case coe v11 of
                                  (:) v12 v13
                                    -> case coe v13 of
                                         (:) v14 v15
                                           -> case coe v15 of
                                                (:) v16 v17
                                                  -> case coe v17 of
                                                       []
                                                         -> case coe v8 of
                                                              (:) v18 v19
                                                                -> case coe v19 of
                                                                     (:) v20 v21
                                                                       -> case coe v21 of
                                                                            (:) v22 v23
                                                                              -> case coe v23 of
                                                                                   []
                                                                                     -> coe
                                                                                          (\ v24 ->
                                                                                             coe
                                                                                               MAlonzo.Code.Interface.Monad.d_return_28
                                                                                               MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12
                                                                                               ()
                                                                                               erased
                                                                                               (coe
                                                                                                  C_STSConstr'46'constructor_239
                                                                                                  (coe
                                                                                                     v1)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Data.List.Base.du_takeWhile_780
                                                                                                     d_isArg_66
                                                                                                     v3)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Tactic.Helpers.du_zipWithIndex_14
                                                                                                     (coe
                                                                                                        (\ v25 ->
                                                                                                           MAlonzo.Code.Generics.Utils.d_mapVars_328
                                                                                                             (coe
                                                                                                                (\ v26 ->
                                                                                                                   coe
                                                                                                                     MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                                                                                                                     v26
                                                                                                                     v25))))
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
                                                                                                        (coe
                                                                                                           ())
                                                                                                        (coe
                                                                                                           ())
                                                                                                        (coe
                                                                                                           (\ v25 ->
                                                                                                              coe
                                                                                                                MAlonzo.Code.Reflection.AST.Argument.du_unArg_74
                                                                                                                (coe
                                                                                                                   MAlonzo.Code.Reflection.AST.Abstraction.du_unAbs_36
                                                                                                                   (coe
                                                                                                                      v25))))
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Data.List.Base.du_dropWhile_786
                                                                                                           d_isArg_66
                                                                                                           v3)))
                                                                                                  (coe
                                                                                                     v18)
                                                                                                  (coe
                                                                                                     v20)
                                                                                                  (coe
                                                                                                     v22)
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Generics.Utils.d_mapVars_328
                                                                                                     (\ v25 ->
                                                                                                        coe
                                                                                                          MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                                                                                                          v25
                                                                                                          (coe
                                                                                                             MAlonzo.Code.Data.List.Base.du_length_304
                                                                                                             (coe
                                                                                                                MAlonzo.Code.Data.List.Base.du_dropWhile_786
                                                                                                                d_isArg_66
                                                                                                                v3)))
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Reflection.AST.Argument.du_unArg_74
                                                                                                        (coe
                                                                                                           v16)))))
                                                                                   _ -> coe v9
                                                                            _ -> coe v9
                                                                     _ -> coe v9
                                                              _ -> coe v9
                                                       _ -> coe v9
                                                _ -> coe v9
                                         _ -> coe v9
                                  _ -> coe v9
                           _ -> coe v9
                    _ -> coe v5
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.errorIfNothing
d_errorIfNothing_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Maybe AgdaAny ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_errorIfNothing_116 v0 ~v1 v2 v3 = du_errorIfNothing_116 v0 v2 v3
du_errorIfNothing_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  Maybe AgdaAny ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_errorIfNothing_116 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v3
        -> coe
             (\ v4 ->
                coe
                  MAlonzo.Code.Interface.Monad.d_return_28
                  MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 v0 erased v3)
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
        -> coe
             MAlonzo.Code.Interface.MonadTC.du_error1_664
             (coe
                MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
             (coe v0)
             (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
             (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.getSTSConstrs
d_getSTSConstrs_124 ::
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_getSTSConstrs_124 v0
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
      (coe ("getSTSConstrs" :: Data.Text.Text))
      (coe
         (\ v1 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
              (coe
                 MAlonzo.Code.Tactic.Helpers.du_getDataDef_178
                 MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                 (coe
                    MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                    (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                 (coe
                    MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                    (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                 MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v0 v1)
              (\ v2 ->
                 coe
                   MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                   MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                   (coe
                      MAlonzo.Code.Interface.Monad.du_traverseList_70
                      (coe
                         MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                      () d_toSTSConstr_70
                      (MAlonzo.Code.Tactic.Helpers.d_constructors_32 (coe v2)) v1)
                   (\ v3 ->
                      coe
                        MAlonzo.Code.Interface.Monad.du__'62''62'__32
                        (coe
                           MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                           (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                        () ()
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
                              (coe
                                 MAlonzo.Code.Interface.MonadTC.du__'7515''8319'_486
                                 (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                 (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                 (coe
                                    MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                    (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                 (coe ()) (coe v3))
                              (coe
                                 MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'MErrorPartWrap_462)
                              (coe
                                 MAlonzo.Code.Interface.Monad.d_return_28
                                 MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18 () erased
                                 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                        (\ v4 ->
                           coe
                             MAlonzo.Code.Interface.Monad.d_return_28
                             MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased v3)
                        v1))))
-- Tactic.DeriveComp.generatePred
d_generatePred_132 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_generatePred_132 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
      (coe
         (MAlonzo.RTE.QName
            (38 :: Integer) (17154534930547628895 :: Integer)
            "Interface.Decidable.Instance.\191_\191"
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
            (coe du_helper_140 (coe v0)))
         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
-- Tactic.DeriveComp._.helper
d_helper_140 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_helper_140 ~v0 v1 = du_helper_140 v1
du_helper_140 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_helper_140 v0
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
             (coe
                (MAlonzo.RTE.QName
                   (6 :: Integer) (13559399870857524843 :: Integer)
                   "Agda.Builtin.Unit.\8868"
                   (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
             (coe v0)
      (:) v1 v2
        -> case coe v2 of
             [] -> coe v1
             (:) v3 v4
               -> coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                    (coe
                       (MAlonzo.RTE.QName
                          (52 :: Integer) (14176793942586333973 :: Integer)
                          "Data.Product.Base._\215_"
                          (MAlonzo.RTE.Fixity
                             MAlonzo.RTE.RightAssoc (MAlonzo.RTE.Related (2.0 :: Double)))))
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
                             (coe du_helper_140 (coe v2)))
                          (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.DeriveComp.curryPredProof
d_curryPredProof_150 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_curryPredProof_150 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      1 -> coe
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
                (coe v1))
      2 -> coe
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
                         (28 :: Integer) (15581396396021577314 :: Integer)
                         "Agda.Builtin.Sigma.\931.fst"
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
                         (coe v1))
                      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
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
                            (30 :: Integer) (15581396396021577314 :: Integer)
                            "Agda.Builtin.Sigma.\931.snd"
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
                            (coe v1))
                         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
      _ -> coe
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
                         (28 :: Integer) (15581396396021577314 :: Integer)
                         "Agda.Builtin.Sigma.\931.fst"
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
                         (coe v1))
                      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
             (coe
                d_curryPredProof_150 (coe subInt (coe v0) (coe (1 :: Integer)))
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                   (coe
                      (MAlonzo.RTE.QName
                         (30 :: Integer) (15581396396021577314 :: Integer)
                         "Agda.Builtin.Sigma.\931.snd"
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
                         (coe v1))
                      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
-- Tactic.DeriveComp.generateFunctionClause
d_generateFunctionClause_162 ::
  ([MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] ->
   MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  T_STSConstr_6 -> MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152
d_generateFunctionClause_162 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264
      (coe
         MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
         (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
         (coe ()) (coe ())
         (coe
            (\ v2 ->
               case coe v2 of
                 MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v3 v4
                   -> coe
                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v4)
                 _ -> MAlonzo.RTE.mazUnreachableError))
         (coe d_params_24 (coe v1)))
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
            (coe d_context_28 (coe v1)))
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
               (coe d_state_30 (coe v1)))
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
                  (coe d_signal_32 (coe v1)))
               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
         (coe
            (MAlonzo.RTE.QName
               (42 :: Integer) (11214500610362984592 :: Integer)
               "Data.Bool.Base.if_then_else_"
               (MAlonzo.RTE.Fixity
                  MAlonzo.RTE.NonAssoc (MAlonzo.RTE.Related (0.0 :: Double)))))
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
                        (104 :: Integer) (16368259409245829246 :: Integer)
                        "Relation.Nullary.Decidable.Core.\8970_\8971"
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
                        (coe v0 (d_clauses_26 (coe v1))))
                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
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
                           (16 :: Integer) (15412666033012224255 :: Integer)
                           "Agda.Builtin.Maybe.Maybe.just"
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
                           (coe d_result_34 (coe v1)))
                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
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
                              (18 :: Integer) (15412666033012224255 :: Integer)
                              "Agda.Builtin.Maybe.Maybe.nothing"
                              (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
-- Tactic.DeriveComp._.clauses
d_clauses_170 ::
  T_STSConstr_6 -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146]
d_clauses_170 v0 = coe d_clauses_26 (coe v0)
-- Tactic.DeriveComp._.context
d_context_172 ::
  T_STSConstr_6 -> MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_context_172 v0 = coe d_context_28 (coe v0)
-- Tactic.DeriveComp._.name
d_name_174 :: T_STSConstr_6 -> AgdaAny
d_name_174 v0 = coe d_name_22 (coe v0)
-- Tactic.DeriveComp._.params
d_params_176 ::
  T_STSConstr_6 -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
d_params_176 v0 = coe d_params_24 (coe v0)
-- Tactic.DeriveComp._.result
d_result_178 ::
  T_STSConstr_6 -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_result_178 v0 = coe d_result_34 (coe v0)
-- Tactic.DeriveComp._.signal
d_signal_180 ::
  T_STSConstr_6 -> MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_signal_180 v0 = coe d_signal_32 (coe v0)
-- Tactic.DeriveComp._.state
d_state_182 ::
  T_STSConstr_6 -> MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_state_182 v0 = coe d_state_30 (coe v0)
-- Tactic.DeriveComp.generateFunction
d_generateFunction_190 ::
  [T_STSConstr_6] -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_generateFunction_190 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188
      (coe
         MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
         (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
         (coe ()) (coe ())
         (coe d_generateFunctionClause_162 (coe d_generatePred_132))
         (coe v0))
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Tactic.DeriveComp.rdOpts
d_rdOpts_194 ::
  MAlonzo.Code.Interface.MonadTC.T_ReductionOptions_14
d_rdOpts_194
  = coe
      MAlonzo.Code.Interface.MonadTC.C_onlyReduce_16
      (coe
         MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
         (coe
            (MAlonzo.RTE.QName
               (104 :: Integer) (16368259409245829246 :: Integer)
               "Relation.Nullary.Decidable.Core.\8970_\8971"
               (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated))))
-- Tactic.DeriveComp.derive⇐
d_derive'8656'_196 ::
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_derive'8656'_196
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
      (coe ("derive\8656" :: Data.Text.Text))
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
              (coe
                 MAlonzo.Code.Tactic.ClauseBuilder.du_currentTyConstrPatterns_512
                 MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                 (coe
                    MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                    (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                 (coe
                    MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                    (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                 MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v0)
              (\ v1 ->
                 let v2
                       = coe
                           MAlonzo.Code.Interface.MonadTC.du_error1_664
                           (coe
                              MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                              (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                           (coe ())
                           (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                           (coe
                              ("TODO: Support more than one constructor!" :: Data.Text.Text)) in
                 case coe v1 of
                   (:) v3 v4
                     -> case coe v4 of
                          []
                            -> coe
                                 MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                 MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                                 (coe
                                    MAlonzo.Code.Tactic.ClauseBuilder.du_singleMatchExpr_656
                                    (coe
                                       MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                       (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                    (coe
                                       MAlonzo.Code.Tactic.ClauseBuilder.du_ContextMonad'45'MonadTC_598
                                       (coe
                                          MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                          (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                       (coe
                                          MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                          (coe
                                             MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                       (coe
                                          MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                          (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                       (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154))
                                    v3
                                    (coe
                                       MAlonzo.Code.Tactic.ClauseBuilder.du_finishMatch_724
                                       (coe
                                          MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                          (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                       (coe
                                          MAlonzo.Code.Interface.MonadTC.du_withGoalHole_784
                                          (coe
                                             MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                             (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                          (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                          (coe
                                             MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                             (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                          (coe
                                             MAlonzo.Code.Tactic.ReduceDec.d_reduceDecInGoal_226
                                             (coe d_rdOpts_194)
                                             (coe
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_con_170
                                                (coe
                                                   (MAlonzo.RTE.QName
                                                      (20 :: Integer)
                                                      (1335258922519917603 :: Integer)
                                                      "Agda.Builtin.Equality._\8801_.refl"
                                                      (MAlonzo.RTE.Fixity
                                                         MAlonzo.RTE.NonAssoc
                                                         MAlonzo.RTE.Unrelated)))
                                                (coe v4)))))
                                    v0)
                                 (\ v5 ->
                                    coe
                                      MAlonzo.Code.Interface.MonadTC.du_unifyWithGoal_756
                                      MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                                      (coe
                                         MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                         (coe
                                            MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                      MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                      (coe
                                         MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                      (MAlonzo.Code.Tactic.ClauseBuilder.d_clauseExprToPatLam_434
                                         (coe v5))
                                      v0)
                          _ -> coe v2 v0
                   _ -> coe v2 v0)))
-- Tactic.DeriveComp.derive⇒
d_derive'8658'_204 ::
  AgdaAny ->
  [T_STSConstr_6] ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_derive'8658'_204 ~v0 v1 = du_derive'8658'_204 v1
du_derive'8658'_204 ::
  [T_STSConstr_6] ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_derive'8658'_204 v0
  = let v1
          = coe
              MAlonzo.Code.Interface.MonadTC.du_error1_664
              (coe
                 MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                 (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
              (coe ())
              (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
              (coe ("TODO: support multiple constructors" :: Data.Text.Text)) in
    case coe v0 of
      (:) v2 v3
        -> case coe v2 of
             C_STSConstr'46'constructor_239 v4 v5 v6 v7 v8 v9 v10
               -> case coe v3 of
                    []
                      -> coe
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
                           (coe ("derive\8658" :: Data.Text.Text))
                           (coe
                              (\ v11 ->
                                 coe
                                   MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                   MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased ()
                                   erased
                                   (coe
                                      MAlonzo.Code.Tactic.ClauseBuilder.du_introsExpr_694
                                      (coe
                                         MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                      (coe
                                         MAlonzo.Code.Tactic.ClauseBuilder.du_ContextMonad'45'MonadTC_598
                                         (coe
                                            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                         (coe
                                            MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                            (coe
                                               MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                         (coe
                                            MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                         (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154))
                                      (MAlonzo.Code.Data.Maybe.Base.d_from'45'just_64
                                         (coe
                                            MAlonzo.Code.Data.List.NonEmpty.Base.du_fromList_66
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
                                                  (coe ("h" :: Data.Text.Text)))
                                               (coe v3))))
                                      (coe
                                         MAlonzo.Code.Tactic.ClauseBuilder.du_finishMatch_724
                                         (coe
                                            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                         (coe
                                            MAlonzo.Code.Tactic.ClauseBuilder.du_caseMatch_502
                                            (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                            (coe
                                               MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                               (coe
                                                  MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                            (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                            (coe
                                               MAlonzo.Code.Generics.Utils.d_mapVars_328
                                               (\ v12 -> addInt (coe (2 :: Integer)) (coe v12))
                                               (d_generatePred_132 (coe v6)))
                                            (coe
                                               MAlonzo.Code.Tactic.ClauseBuilder.du_matchExprM_622
                                               (coe
                                                  MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                  (coe
                                                     MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                               (coe
                                                  MAlonzo.Code.Tactic.ClauseBuilder.du_ContextMonad'45'MonadTC_598
                                                  (coe
                                                     MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                     (coe
                                                        MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                  (coe
                                                     MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                                     (coe
                                                        MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                                  (coe
                                                     MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                                     (coe
                                                        MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                  (coe
                                                     MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154))
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                     (coe
                                                        MAlonzo.Code.Tactic.ClauseBuilder.d_multiSinglePattern_66
                                                        (coe
                                                           MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                           (coe ("" :: Data.Text.Text)))
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
                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_con_236
                                                              (coe
                                                                 (MAlonzo.RTE.QName
                                                                    (34 :: Integer)
                                                                    (16368259409245829246 ::
                                                                       Integer)
                                                                    "Relation.Nullary.Decidable.Core._because_"
                                                                    (MAlonzo.RTE.Fixity
                                                                       MAlonzo.RTE.NonAssoc
                                                                       (MAlonzo.RTE.Related
                                                                          (2.0 :: Double)))))
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
                                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_con_236
                                                                       (coe
                                                                          (MAlonzo.RTE.QName
                                                                             (8 :: Integer)
                                                                             (4305008439024043551 ::
                                                                                Integer)
                                                                             "Agda.Builtin.Bool.Bool.false"
                                                                             (MAlonzo.RTE.Fixity
                                                                                MAlonzo.RTE.NonAssoc
                                                                                MAlonzo.RTE.Unrelated)))
                                                                       (coe v3)))
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
                                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_con_236
                                                                          (coe
                                                                             (MAlonzo.RTE.QName
                                                                                (30 :: Integer)
                                                                                (5284306542668000596 ::
                                                                                   Integer)
                                                                                "Relation.Nullary.Reflects.Reflects.of\8319"
                                                                                (MAlonzo.RTE.Fixity
                                                                                   MAlonzo.RTE.NonAssoc
                                                                                   MAlonzo.RTE.Unrelated)))
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
                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_var_244
                                                                                   (coe
                                                                                      (0 ::
                                                                                         Integer))))
                                                                             (coe v3))))
                                                                    (coe v3))))))
                                                     (coe
                                                        MAlonzo.Code.Tactic.ClauseBuilder.du_finishMatch_724
                                                        (coe
                                                           MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                           (coe
                                                              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                        (coe
                                                           (\ v12 ->
                                                              coe
                                                                MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                                                MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12
                                                                () erased () erased
                                                                (MAlonzo.Code.Tactic.ReduceDec.d_reduceDec''_208
                                                                   (coe d_rdOpts_194)
                                                                   (coe
                                                                      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164
                                                                      (coe (1 :: Integer)) (coe v3))
                                                                   (coe v12))
                                                                (\ v13 ->
                                                                   coe
                                                                     MAlonzo.Code.Interface.Monad.d_return_28
                                                                     MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12
                                                                     () erased
                                                                     (coe
                                                                        MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                                                        (coe
                                                                           (MAlonzo.RTE.QName
                                                                              (234 :: Integer)
                                                                              (10779521135412943468 ::
                                                                                 Integer)
                                                                              "Function.Base.case_of_"
                                                                              (MAlonzo.RTE.Fixity
                                                                                 MAlonzo.RTE.NonAssoc
                                                                                 (MAlonzo.RTE.Related
                                                                                    (0.0 ::
                                                                                       Double)))))
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
                                                                              (coe v13))
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
                                                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188
                                                                                    (coe
                                                                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                       (coe
                                                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270
                                                                                          (coe v3)
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
                                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256
                                                                                                   (coe
                                                                                                      (0 ::
                                                                                                         Integer))))
                                                                                             (coe
                                                                                                v3)))
                                                                                       (coe v3))
                                                                                    (coe v3)))
                                                                              (coe v3)))))))))
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                        (coe
                                                           MAlonzo.Code.Tactic.ClauseBuilder.d_multiSinglePattern_66
                                                           (coe
                                                              MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                              (coe ("" :: Data.Text.Text)))
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
                                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_con_236
                                                                 (coe
                                                                    (MAlonzo.RTE.QName
                                                                       (34 :: Integer)
                                                                       (16368259409245829246 ::
                                                                          Integer)
                                                                       "Relation.Nullary.Decidable.Core._because_"
                                                                       (MAlonzo.RTE.Fixity
                                                                          MAlonzo.RTE.NonAssoc
                                                                          (MAlonzo.RTE.Related
                                                                             (2.0 :: Double)))))
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
                                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_con_236
                                                                          (coe
                                                                             (MAlonzo.RTE.QName
                                                                                (10 :: Integer)
                                                                                (4305008439024043551 ::
                                                                                   Integer)
                                                                                "Agda.Builtin.Bool.Bool.true"
                                                                                (MAlonzo.RTE.Fixity
                                                                                   MAlonzo.RTE.NonAssoc
                                                                                   MAlonzo.RTE.Unrelated)))
                                                                          (coe v3)))
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
                                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_con_236
                                                                             (coe
                                                                                (MAlonzo.RTE.QName
                                                                                   (26 :: Integer)
                                                                                   (5284306542668000596 ::
                                                                                      Integer)
                                                                                   "Relation.Nullary.Reflects.Reflects.of\696"
                                                                                   (MAlonzo.RTE.Fixity
                                                                                      MAlonzo.RTE.NonAssoc
                                                                                      MAlonzo.RTE.Unrelated)))
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
                                                                                      MAlonzo.Code.Agda.Builtin.Reflection.C_var_244
                                                                                      (coe
                                                                                         (0 ::
                                                                                            Integer))))
                                                                                (coe v3))))
                                                                       (coe v3))))))
                                                        (coe
                                                           MAlonzo.Code.Tactic.ClauseBuilder.du_finishMatch_724
                                                           (coe
                                                              MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                              (coe
                                                                 MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                           (coe
                                                              (\ v12 ->
                                                                 coe
                                                                   MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                                                   MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12
                                                                   () erased () erased
                                                                   (MAlonzo.Code.Tactic.ReduceDec.d_reduceDec''_208
                                                                      (coe d_rdOpts_194)
                                                                      (coe
                                                                         MAlonzo.Code.Agda.Builtin.Reflection.C_var_164
                                                                         (coe (1 :: Integer))
                                                                         (coe v3))
                                                                      (coe v12))
                                                                   (\ v13 ->
                                                                      coe
                                                                        MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                                                        MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12
                                                                        () erased () erased
                                                                        (coe
                                                                           MAlonzo.Code.Interface.MonadTC.du_goalTy_684
                                                                           MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                                                                           MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                                                           (coe
                                                                              MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                                                              (coe
                                                                                 MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                                           v12)
                                                                        (\ v14 ->
                                                                           case coe v14 of
                                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v15 v16
                                                                               -> case coe v16 of
                                                                                    (:) v17 v18
                                                                                      -> case coe
                                                                                                v17 of
                                                                                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v19 v20
                                                                                             -> case coe
                                                                                                       v19 of
                                                                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v21 v22
                                                                                                    -> case coe
                                                                                                              v21 of
                                                                                                         MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                                                                                           -> case coe
                                                                                                                     v22 of
                                                                                                                MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v23 v24
                                                                                                                  -> case coe
                                                                                                                            v23 of
                                                                                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                                                                                         -> case coe
                                                                                                                                   v24 of
                                                                                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                                                                                -> case coe
                                                                                                                                          v18 of
                                                                                                                                     (:) v25 v26
                                                                                                                                       -> case coe
                                                                                                                                                 v25 of
                                                                                                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v27 v28
                                                                                                                                              -> case coe
                                                                                                                                                        v27 of
                                                                                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v29 v30
                                                                                                                                                     -> case coe
                                                                                                                                                               v29 of
                                                                                                                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                                                                                                                                            -> case coe
                                                                                                                                                                      v30 of
                                                                                                                                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v31 v32
                                                                                                                                                                   -> case coe
                                                                                                                                                                             v31 of
                                                                                                                                                                        MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                                                                                                                                          -> case coe
                                                                                                                                                                                    v32 of
                                                                                                                                                                               MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                                                                                                                                 -> case coe
                                                                                                                                                                                           v26 of
                                                                                                                                                                                      (:) v33 v34
                                                                                                                                                                                        -> case coe
                                                                                                                                                                                                  v33 of
                                                                                                                                                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v35 v36
                                                                                                                                                                                               -> case coe
                                                                                                                                                                                                         v35 of
                                                                                                                                                                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v37 v38
                                                                                                                                                                                                      -> case coe
                                                                                                                                                                                                                v37 of
                                                                                                                                                                                                           MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                                                                                                                                                                                             -> case coe
                                                                                                                                                                                                                       v38 of
                                                                                                                                                                                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v39 v40
                                                                                                                                                                                                                    -> case coe
                                                                                                                                                                                                                              v39 of
                                                                                                                                                                                                                         MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                                                                                                                                                                                           -> case coe
                                                                                                                                                                                                                                     v40 of
                                                                                                                                                                                                                                MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                                                                                                                                                                                  -> case coe
                                                                                                                                                                                                                                            v34 of
                                                                                                                                                                                                                                       (:) v41 v42
                                                                                                                                                                                                                                         -> case coe
                                                                                                                                                                                                                                                   v41 of
                                                                                                                                                                                                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v43 v44
                                                                                                                                                                                                                                                -> case coe
                                                                                                                                                                                                                                                          v43 of
                                                                                                                                                                                                                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v45 v46
                                                                                                                                                                                                                                                       -> case coe
                                                                                                                                                                                                                                                                 v45 of
                                                                                                                                                                                                                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                                                                                                                                                                                                                                              -> case coe
                                                                                                                                                                                                                                                                        v46 of
                                                                                                                                                                                                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v47 v48
                                                                                                                                                                                                                                                                     -> case coe
                                                                                                                                                                                                                                                                               v47 of
                                                                                                                                                                                                                                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                                                                                                                                                                                                                                            -> case coe
                                                                                                                                                                                                                                                                                      v48 of
                                                                                                                                                                                                                                                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                                                                                                                                                                                                                                   -> case coe
                                                                                                                                                                                                                                                                                             v42 of
                                                                                                                                                                                                                                                                                        []
                                                                                                                                                                                                                                                                                          -> coe
                                                                                                                                                                                                                                                                                               MAlonzo.Code.Interface.Monad.d_return_28
                                                                                                                                                                                                                                                                                               MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12
                                                                                                                                                                                                                                                                                               ()
                                                                                                                                                                                                                                                                                               erased
                                                                                                                                                                                                                                                                                               (coe
                                                                                                                                                                                                                                                                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                                                                                                                                                                                                                                                                                  (coe
                                                                                                                                                                                                                                                                                                     (MAlonzo.RTE.QName
                                                                                                                                                                                                                                                                                                        (98 ::
                                                                                                                                                                                                                                                                                                           Integer)
                                                                                                                                                                                                                                                                                                        (6189151057044369179 ::
                                                                                                                                                                                                                                                                                                           Integer)
                                                                                                                                                                                                                                                                                                        "Relation.Binary.PropositionalEquality.Core.subst"
                                                                                                                                                                                                                                                                                                        (MAlonzo.RTE.Fixity
                                                                                                                                                                                                                                                                                                           MAlonzo.RTE.NonAssoc
                                                                                                                                                                                                                                                                                                           MAlonzo.RTE.Unrelated)))
                                                                                                                                                                                                                                                                                                  (coe
                                                                                                                                                                                                                                                                                                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                                                                                                                                                                                                     (coe
                                                                                                                                                                                                                                                                                                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                                                                                                                                                                                        (coe
                                                                                                                                                                                                                                                                                                           v43)
                                                                                                                                                                                                                                                                                                        (coe
                                                                                                                                                                                                                                                                                                           MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                                                                                                                                                                                                                                                                                           (coe
                                                                                                                                                                                                                                                                                                              v15)
                                                                                                                                                                                                                                                                                                           (coe
                                                                                                                                                                                                                                                                                                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                                                                                                                                                                                                              (coe
                                                                                                                                                                                                                                                                                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                                                                                    v43)
                                                                                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                                                                                    v20))
                                                                                                                                                                                                                                                                                                              (coe
                                                                                                                                                                                                                                                                                                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                                                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                                                       v43)
                                                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                                                       v28))
                                                                                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                                                                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                                                                                                                                                                                                       (coe
                                                                                                                                                                                                                                                                                                                          v43)
                                                                                                                                                                                                                                                                                                                       (coe
                                                                                                                                                                                                                                                                                                                          v36))
                                                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                                                       v42))))))
                                                                                                                                                                                                                                                                                                     (coe
                                                                                                                                                                                                                                                                                                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                                                                                                                                                                                                        (coe
                                                                                                                                                                                                                                                                                                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                                                                                                                                                                                           (coe
                                                                                                                                                                                                                                                                                                              v43)
                                                                                                                                                                                                                                                                                                           (coe
                                                                                                                                                                                                                                                                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                                                                                                                                                                                                                                                                                              (coe
                                                                                                                                                                                                                                                                                                                 (MAlonzo.RTE.QName
                                                                                                                                                                                                                                                                                                                    (22 ::
                                                                                                                                                                                                                                                                                                                       Integer)
                                                                                                                                                                                                                                                                                                                    (9726600929795345893 ::
                                                                                                                                                                                                                                                                                                                       Integer)
                                                                                                                                                                                                                                                                                                                    "Data.Maybe.Properties.just-injective"
                                                                                                                                                                                                                                                                                                                    (MAlonzo.RTE.Fixity
                                                                                                                                                                                                                                                                                                                       MAlonzo.RTE.NonAssoc
                                                                                                                                                                                                                                                                                                                       MAlonzo.RTE.Unrelated)))
                                                                                                                                                                                                                                                                                                              (coe
                                                                                                                                                                                                                                                                                                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                                                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                                                       v43)
                                                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                                                       v13))
                                                                                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                                                                                    v42))))
                                                                                                                                                                                                                                                                                                        (coe
                                                                                                                                                                                                                                                                                                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                                                                                                                                                                                                           (coe
                                                                                                                                                                                                                                                                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                                                                                                                                                                                              (coe
                                                                                                                                                                                                                                                                                                                 v43)
                                                                                                                                                                                                                                                                                                              (coe
                                                                                                                                                                                                                                                                                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_con_170
                                                                                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                                                                                    v4)
                                                                                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                                                                                    d_curryPredProof_150
                                                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                                                       MAlonzo.Code.Data.List.Base.du_length_304
                                                                                                                                                                                                                                                                                                                       v6)
                                                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_var_164
                                                                                                                                                                                                                                                                                                                       (coe
                                                                                                                                                                                                                                                                                                                          (0 ::
                                                                                                                                                                                                                                                                                                                             Integer))
                                                                                                                                                                                                                                                                                                                       (coe
                                                                                                                                                                                                                                                                                                                          v42)))))
                                                                                                                                                                                                                                                                                                           (coe
                                                                                                                                                                                                                                                                                                              v42)))))
                                                                                                                                                                                                                                                                                        _ -> coe
                                                                                                                                                                                                                                                                                               MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                                                                                                                                               ()
                                                                                                                                                                                                                                                                                               erased
                                                                                                                                                                                                                                                                                               (coe
                                                                                                                                                                                                                                                                                                  MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                                                                                  (coe
                                                                                                                                                                                                                                                                                                     ("BUG: Unexpected type"
                                                                                                                                                                                                                                                                                                      ::
                                                                                                                                                                                                                                                                                                      Data.Text.Text))
                                                                                                                                                                                                                                                                                                  (coe
                                                                                                                                                                                                                                                                                                     MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                                                                                                                                  (coe
                                                                                                                                                                                                                                                                                                     MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                                                                                     (coe
                                                                                                                                                                                                                                                                                                        v14)
                                                                                                                                                                                                                                                                                                     (coe
                                                                                                                                                                                                                                                                                                        MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                                                                                                                                     (coe
                                                                                                                                                                                                                                                                                                        v3)))
                                                                                                                                                                                                                                                                                 _ -> coe
                                                                                                                                                                                                                                                                                        MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                                                                                        erased
                                                                                                                                                                                                                                                                                        (coe
                                                                                                                                                                                                                                                                                           MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                                                                           (coe
                                                                                                                                                                                                                                                                                              ("BUG: Unexpected type"
                                                                                                                                                                                                                                                                                               ::
                                                                                                                                                                                                                                                                                               Data.Text.Text))
                                                                                                                                                                                                                                                                                           (coe
                                                                                                                                                                                                                                                                                              MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                                                                                                                           (coe
                                                                                                                                                                                                                                                                                              MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                                                                              (coe
                                                                                                                                                                                                                                                                                                 v14)
                                                                                                                                                                                                                                                                                              (coe
                                                                                                                                                                                                                                                                                                 MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                                                                                                                              (coe
                                                                                                                                                                                                                                                                                                 v3)))
                                                                                                                                                                                                                                                                          _ -> coe
                                                                                                                                                                                                                                                                                 MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                                                                                                                                 ()
                                                                                                                                                                                                                                                                                 erased
                                                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                                                    MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                       ("BUG: Unexpected type"
                                                                                                                                                                                                                                                                                        ::
                                                                                                                                                                                                                                                                                        Data.Text.Text))
                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                       MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                                                       MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                                                                       (coe
                                                                                                                                                                                                                                                                                          v14)
                                                                                                                                                                                                                                                                                       (coe
                                                                                                                                                                                                                                                                                          MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                                                                                                                       (coe
                                                                                                                                                                                                                                                                                          v3)))
                                                                                                                                                                                                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                                                                                                                                            _ -> coe
                                                                                                                                                                                                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                                                                                                                   ()
                                                                                                                                                                                                                                                                   erased
                                                                                                                                                                                                                                                                   (coe
                                                                                                                                                                                                                                                                      MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                                                      (coe
                                                                                                                                                                                                                                                                         ("BUG: Unexpected type"
                                                                                                                                                                                                                                                                          ::
                                                                                                                                                                                                                                                                          Data.Text.Text))
                                                                                                                                                                                                                                                                      (coe
                                                                                                                                                                                                                                                                         MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                                                                                                      (coe
                                                                                                                                                                                                                                                                         MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                                                         (coe
                                                                                                                                                                                                                                                                            v14)
                                                                                                                                                                                                                                                                         (coe
                                                                                                                                                                                                                                                                            MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                                                                                                         (coe
                                                                                                                                                                                                                                                                            v3)))
                                                                                                                                                                                                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                                                                                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                                                                                                                       _ -> coe
                                                                                                                                                                                                                                              MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                                              erased
                                                                                                                                                                                                                                              (coe
                                                                                                                                                                                                                                                 MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                    ("BUG: Unexpected type"
                                                                                                                                                                                                                                                     ::
                                                                                                                                                                                                                                                     Data.Text.Text))
                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                    MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                                                                                 (coe
                                                                                                                                                                                                                                                    MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                       v14)
                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                       MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                                                                                    (coe
                                                                                                                                                                                                                                                       v3)))
                                                                                                                                                                                                                                _ -> coe
                                                                                                                                                                                                                                       MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                                                                                       ()
                                                                                                                                                                                                                                       erased
                                                                                                                                                                                                                                       (coe
                                                                                                                                                                                                                                          MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                          (coe
                                                                                                                                                                                                                                             ("BUG: Unexpected type"
                                                                                                                                                                                                                                              ::
                                                                                                                                                                                                                                              Data.Text.Text))
                                                                                                                                                                                                                                          (coe
                                                                                                                                                                                                                                             MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                                                                          (coe
                                                                                                                                                                                                                                             MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                             (coe
                                                                                                                                                                                                                                                v14)
                                                                                                                                                                                                                                             (coe
                                                                                                                                                                                                                                                MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                                                                             (coe
                                                                                                                                                                                                                                                v3)))
                                                                                                                                                                                                                         _ -> coe
                                                                                                                                                                                                                                MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                                                                                ()
                                                                                                                                                                                                                                erased
                                                                                                                                                                                                                                (coe
                                                                                                                                                                                                                                   MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                   (coe
                                                                                                                                                                                                                                      ("BUG: Unexpected type"
                                                                                                                                                                                                                                       ::
                                                                                                                                                                                                                                       Data.Text.Text))
                                                                                                                                                                                                                                   (coe
                                                                                                                                                                                                                                      MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                                                                   (coe
                                                                                                                                                                                                                                      MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                                      (coe
                                                                                                                                                                                                                                         v14)
                                                                                                                                                                                                                                      (coe
                                                                                                                                                                                                                                         MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                                                                      (coe
                                                                                                                                                                                                                                         v3)))
                                                                                                                                                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                                                                                           _ -> coe
                                                                                                                                                                                                                  MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                                                                  ()
                                                                                                                                                                                                                  erased
                                                                                                                                                                                                                  (coe
                                                                                                                                                                                                                     MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                     (coe
                                                                                                                                                                                                                        ("BUG: Unexpected type"
                                                                                                                                                                                                                         ::
                                                                                                                                                                                                                         Data.Text.Text))
                                                                                                                                                                                                                     (coe
                                                                                                                                                                                                                        MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                                                     (coe
                                                                                                                                                                                                                        MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                                        (coe
                                                                                                                                                                                                                           v14)
                                                                                                                                                                                                                        (coe
                                                                                                                                                                                                                           MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                                                        (coe
                                                                                                                                                                                                                           v3)))
                                                                                                                                                                                                    _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                                                                      _ -> coe
                                                                                                                                                                                             MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                                             ()
                                                                                                                                                                                             erased
                                                                                                                                                                                             (coe
                                                                                                                                                                                                MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                (coe
                                                                                                                                                                                                   ("BUG: Unexpected type"
                                                                                                                                                                                                    ::
                                                                                                                                                                                                    Data.Text.Text))
                                                                                                                                                                                                (coe
                                                                                                                                                                                                   MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                                (coe
                                                                                                                                                                                                   MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                                   (coe
                                                                                                                                                                                                      v14)
                                                                                                                                                                                                   (coe
                                                                                                                                                                                                      MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                                   (coe
                                                                                                                                                                                                      v3)))
                                                                                                                                                                               _ -> coe
                                                                                                                                                                                      MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                                      ()
                                                                                                                                                                                      erased
                                                                                                                                                                                      (coe
                                                                                                                                                                                         MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                         (coe
                                                                                                                                                                                            ("BUG: Unexpected type"
                                                                                                                                                                                             ::
                                                                                                                                                                                             Data.Text.Text))
                                                                                                                                                                                         (coe
                                                                                                                                                                                            MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                         (coe
                                                                                                                                                                                            MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                            (coe
                                                                                                                                                                                               v14)
                                                                                                                                                                                            (coe
                                                                                                                                                                                               MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                            (coe
                                                                                                                                                                                               v3)))
                                                                                                                                                                        _ -> coe
                                                                                                                                                                               MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                               ()
                                                                                                                                                                               erased
                                                                                                                                                                               (coe
                                                                                                                                                                                  MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                  (coe
                                                                                                                                                                                     ("BUG: Unexpected type"
                                                                                                                                                                                      ::
                                                                                                                                                                                      Data.Text.Text))
                                                                                                                                                                                  (coe
                                                                                                                                                                                     MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                                  (coe
                                                                                                                                                                                     MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                                     (coe
                                                                                                                                                                                        v14)
                                                                                                                                                                                     (coe
                                                                                                                                                                                        MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                                     (coe
                                                                                                                                                                                        v3)))
                                                                                                                                                                 _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                                          _ -> coe
                                                                                                                                                                 MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                                                 ()
                                                                                                                                                                 erased
                                                                                                                                                                 (coe
                                                                                                                                                                    MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                    (coe
                                                                                                                                                                       ("BUG: Unexpected type"
                                                                                                                                                                        ::
                                                                                                                                                                        Data.Text.Text))
                                                                                                                                                                    (coe
                                                                                                                                                                       MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                                                    (coe
                                                                                                                                                                       MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                                       (coe
                                                                                                                                                                          v14)
                                                                                                                                                                       (coe
                                                                                                                                                                          MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                                       (coe
                                                                                                                                                                          v3)))
                                                                                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                                     _ -> coe
                                                                                                                                            MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                            ()
                                                                                                                                            erased
                                                                                                                                            (coe
                                                                                                                                               MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                               (coe
                                                                                                                                                  ("BUG: Unexpected type"
                                                                                                                                                   ::
                                                                                                                                                   Data.Text.Text))
                                                                                                                                               (coe
                                                                                                                                                  MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                               (coe
                                                                                                                                                  MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                                  (coe
                                                                                                                                                     v14)
                                                                                                                                                  (coe
                                                                                                                                                     MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                                  (coe
                                                                                                                                                     v3)))
                                                                                                                              _ -> coe
                                                                                                                                     MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                                     ()
                                                                                                                                     erased
                                                                                                                                     (coe
                                                                                                                                        MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                        (coe
                                                                                                                                           ("BUG: Unexpected type"
                                                                                                                                            ::
                                                                                                                                            Data.Text.Text))
                                                                                                                                        (coe
                                                                                                                                           MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                        (coe
                                                                                                                                           MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                           (coe
                                                                                                                                              v14)
                                                                                                                                           (coe
                                                                                                                                              MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                           (coe
                                                                                                                                              v3)))
                                                                                                                       _ -> coe
                                                                                                                              MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                              ()
                                                                                                                              erased
                                                                                                                              (coe
                                                                                                                                 MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                 (coe
                                                                                                                                    ("BUG: Unexpected type"
                                                                                                                                     ::
                                                                                                                                     Data.Text.Text))
                                                                                                                                 (coe
                                                                                                                                    MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                                 (coe
                                                                                                                                    MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                                    (coe
                                                                                                                                       v14)
                                                                                                                                    (coe
                                                                                                                                       MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                                    (coe
                                                                                                                                       v3)))
                                                                                                                _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                         _ -> coe
                                                                                                                MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                                                ()
                                                                                                                erased
                                                                                                                (coe
                                                                                                                   MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                   (coe
                                                                                                                      ("BUG: Unexpected type"
                                                                                                                       ::
                                                                                                                       Data.Text.Text))
                                                                                                                   (coe
                                                                                                                      MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                                   (coe
                                                                                                                      MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                                      (coe
                                                                                                                         v14)
                                                                                                                      (coe
                                                                                                                         MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                                      (coe
                                                                                                                         v3)))
                                                                                                  _ -> MAlonzo.RTE.mazUnreachableError
                                                                                           _ -> MAlonzo.RTE.mazUnreachableError
                                                                                    _ -> coe
                                                                                           MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                           () erased
                                                                                           (coe
                                                                                              MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                              (coe
                                                                                                 ("BUG: Unexpected type"
                                                                                                  ::
                                                                                                  Data.Text.Text))
                                                                                              (coe
                                                                                                 MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                              (coe
                                                                                                 MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                                 (coe
                                                                                                    v14)
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                                 (coe
                                                                                                    v3)))
                                                                             _ -> coe
                                                                                    MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334
                                                                                    () erased
                                                                                    (coe
                                                                                       MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                       (coe
                                                                                          ("BUG: Unexpected type"
                                                                                           ::
                                                                                           Data.Text.Text))
                                                                                       (coe
                                                                                          MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                       (coe
                                                                                          MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                          (coe v14)
                                                                                          (coe
                                                                                             MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                          (coe
                                                                                             v3)))))))))
                                                     (coe v3))))))
                                      v11)
                                   (\ v12 ->
                                      coe
                                        MAlonzo.Code.Interface.MonadTC.du_unifyWithGoal_756
                                        MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                                        (coe
                                           MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                           (coe
                                              MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                        MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                        (coe
                                           MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                           (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                        (MAlonzo.Code.Tactic.ClauseBuilder.d_clauseExprToPatLam_434
                                           (coe v12))
                                        v11)))
                    _ -> coe v1
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> coe v1
-- Tactic.DeriveComp._.derive⇔
d_derive'8660'_240 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  AgdaAny ->
  [T_STSConstr_6] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_derive'8660'_240 v0 ~v1 v2 = du_derive'8660'_240 v0 v2
du_derive'8660'_240 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  [T_STSConstr_6] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_derive'8660'_240 v0 v1
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe
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
         (coe ("derive\8660" :: Data.Text.Text))
         (coe
            (\ v2 ->
               coe
                 MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                 MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                 (coe
                    MAlonzo.Code.Interface.MonadTC.du_newMeta_308
                    MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                    (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208) v2)
                 (\ v3 ->
                    coe
                      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                      MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                      (coe
                         MAlonzo.Code.Interface.MonadTC.du_newMeta_308
                         MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208) v2)
                      (\ v4 ->
                         coe
                           MAlonzo.Code.Interface.Monad.du__'62''62'__32
                           (coe
                              MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                              (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                           () ()
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
                                 (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                              (coe
                                 MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                 (coe
                                    (MAlonzo.RTE.QName
                                       (1322 :: Integer) (16285757545730121603 :: Integer)
                                       "Function.Bundles._.mk\8660"
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
                                       (coe v3))
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
                                          (coe v4))
                                       (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
                           (coe
                              MAlonzo.Code.Interface.Monad.du__'62''62'__32
                              (coe
                                 MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                 (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                              (coe ()) (coe ())
                              (coe
                                 MAlonzo.Code.Interface.MonadTC.du_runWithHole_764
                                 (coe
                                    MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                    (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                 () v4 d_derive'8656'_196)
                              (coe
                                 MAlonzo.Code.Interface.MonadTC.du_runWithHole_764
                                 (coe
                                    MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                    (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                 () v3 (coe du_derive'8658'_204 (coe v1))))
                           v2)))))
-- Tactic.DeriveComp._.deriveComp
d_deriveComp_250 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_deriveComp_250 ~v0 v1 = du_deriveComp_250 v1
du_deriveComp_250 ::
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_deriveComp_250 v0
  = coe
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
            (coe ("\nDerive computation function for: " :: Data.Text.Text))
            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
            (coe
               MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v0)
               (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
      (coe
         (\ v1 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
              (coe d_getSTSConstrs_124 v0 v1)
              (\ v2 ->
                 coe
                   MAlonzo.Code.Interface.Monad.du__'62''62'__32
                   (coe
                      MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                      (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                   () ()
                   (coe
                      MAlonzo.Code.Interface.MonadTC.du_debugLog1_668
                      (coe
                         MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                      (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                      (coe
                         MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                      (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                      (coe d_generateFunction_190 (coe v2)))
                   (coe
                      MAlonzo.Code.Interface.MonadTC.du_unifyWithGoal_756
                      (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                      (coe
                         MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                         (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                      (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                      (coe
                         MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                      (coe d_generateFunction_190 (coe v2)))
                   v1)))
-- Tactic.DeriveComp._.by-derive⇔
d_by'45'derive'8660'_256 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  AgdaAny ->
  [T_STSConstr_6] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_by'45'derive'8660'_256 v0 v1 v2
  = coe du_derive'8660'_240 (coe v0) v2
-- Tactic.DeriveComp._.deriveComputational
d_deriveComputational_258 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_deriveComputational_258 v0 v1 v2
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initUnquoteWithGoal_302 (coe v0)
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
         (coe
            (MAlonzo.RTE.QName
               (6 :: Integer) (2181690833759761001 :: Integer)
               "Agda.Builtin.Reflection.Name"
               (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
      (coe
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
         (coe ("deriveComputational" :: Data.Text.Text))
         (coe
            MAlonzo.Code.Interface.Monad.du__'62''62'__32
            (coe
               MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
               (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
            (coe ()) (coe ())
            (coe
               MAlonzo.Code.Interface.MonadTC.du_debugLog1_668
               (coe
                  MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                  (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
               (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
               (coe
                  MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                  (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
               (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                  (coe
                     (MAlonzo.RTE.QName
                        (32 :: Integer) (6501990554091597195 :: Integer)
                        "Interface.ComputationalRelation._.Computational"
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
                        (coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v1)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
            (coe
               MAlonzo.Code.Interface.Monad.du__'62''62'__32
               (coe
                  MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                  (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
               (coe ()) (coe ())
               (coe
                  MAlonzo.Code.Interface.MonadTC.d_declareDef_180
                  MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                     (coe
                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                        (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                        (coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                           (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                           (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                     (coe v2))
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                     (coe
                        (MAlonzo.RTE.QName
                           (32 :: Integer) (6501990554091597195 :: Integer)
                           "Interface.ComputationalRelation._.Computational"
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
                           (coe
                              MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v1)
                              (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
               (coe
                  (\ v3 ->
                     coe
                       MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                       MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                       (coe
                          MAlonzo.Code.Tactic.Helpers.du_withSafeReset_256
                          (coe
                             MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                             (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                          (coe
                             MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                             (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                          (coe
                             MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                             (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                          MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
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
                                    MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased ()
                                    erased
                                    (coe
                                       MAlonzo.Code.Interface.MonadTC.du_newMeta_308
                                       MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                       (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208) v4)
                                    (\ v6 ->
                                       coe
                                         MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                         MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased
                                         () erased
                                         (coe
                                            MAlonzo.Code.Interface.MonadTC.du_mkRecord_274
                                            MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                                            (coe
                                               MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                               (coe
                                                  MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                            MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                            (MAlonzo.RTE.QName
                                               (32 :: Integer) (6501990554091597195 :: Integer)
                                               "Interface.ComputationalRelation._.Computational"
                                               (MAlonzo.RTE.Fixity
                                                  MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated))
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
                                                  (coe v5))
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
                                                     (coe v6))
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                                            v4)
                                         (\ v7 ->
                                            coe
                                              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 ()
                                              erased () erased
                                              (coe
                                                 MAlonzo.Code.Interface.MonadTC.d_checkType_164
                                                 MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v7
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                                    (coe
                                                       (MAlonzo.RTE.QName
                                                          (32 :: Integer)
                                                          (6501990554091597195 :: Integer)
                                                          "Interface.ComputationalRelation._.Computational"
                                                          (MAlonzo.RTE.Fixity
                                                             MAlonzo.RTE.NonAssoc
                                                             MAlonzo.RTE.Unrelated)))
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
                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                                             (coe v1)
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                                                 v4)
                                              (\ v8 ->
                                                 coe
                                                   MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                   (coe
                                                      MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                      (coe
                                                         MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                   () ()
                                                   (coe
                                                      MAlonzo.Code.Interface.MonadTC.du_debugLog1'7504'_516
                                                      (coe
                                                         MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                                      (coe
                                                         MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                                      (coe
                                                         MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                                         (coe
                                                            MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                      (coe
                                                         MAlonzo.Code.Interface.MonadTC.C_wrap_448
                                                         (coe
                                                            MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                                            MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                                                            () erased () erased
                                                            (coe
                                                               MAlonzo.Code.Interface.MonadTC.d_inferType_162
                                                               MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                                               v5)
                                                            (\ v9 ->
                                                               coe
                                                                 MAlonzo.Code.Interface.Monad.d_return_28
                                                                 MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                                                                 () erased
                                                                 (coe
                                                                    MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                    (coe v5)
                                                                    (coe
                                                                       MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                    (coe
                                                                       MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                       (coe
                                                                          (" : " :: Data.Text.Text))
                                                                       (coe
                                                                          MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                       (coe
                                                                          MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                          (coe v9)
                                                                          (coe
                                                                             MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                          (coe
                                                                             MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))))
                                                      (coe
                                                         MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'MErrorPartWrap_462))
                                                   (coe
                                                      MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                      (coe
                                                         MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                         (coe
                                                            MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                      (coe ()) (coe ())
                                                      (coe
                                                         MAlonzo.Code.Interface.MonadTC.du_runWithHole_764
                                                         (coe
                                                            MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                                            (coe
                                                               MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                         () v5 (coe du_deriveComp_250 (coe v1)))
                                                      (coe
                                                         MAlonzo.Code.Interface.MonadTC.d_reduce_168
                                                         MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                                         v5))
                                                   v4)))))
                          v3)
                       (\ v4 ->
                          coe
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
                                  (coe ("compRes: " :: Data.Text.Text))
                                  (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                  (coe
                                     MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v4)
                                     (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                            (\ v5 ->
                               coe
                                 MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                 MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                                 (coe
                                    MAlonzo.Code.Interface.Monad.du__'61''60''60'__40
                                    (coe
                                       MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                       (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                    () ()
                                    (coe
                                       MAlonzo.Code.Interface.MonadTC.d_quoteTC_170
                                       MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 () erased)
                                    (d_getSTSConstrs_124 (coe v1)) v5)
                                 (\ v6 ->
                                    coe
                                      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                      MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased ()
                                      erased
                                      (coe
                                         MAlonzo.Code.Interface.MonadTC.du_mkRecord_274
                                         (coe
                                            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                         (coe
                                            MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                            (coe
                                               MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                         MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                         (MAlonzo.RTE.QName
                                            (32 :: Integer) (6501990554091597195 :: Integer)
                                            "Interface.ComputationalRelation._.Computational"
                                            (MAlonzo.RTE.Fixity
                                               MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated))
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
                                               (coe v4))
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
                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                                     (coe
                                                        (MAlonzo.RTE.QName
                                                           (256 :: Integer)
                                                           (7375614310310668934 :: Integer)
                                                           "Tactic.DeriveComp._.by-derive\8660"
                                                           (MAlonzo.RTE.Fixity
                                                              MAlonzo.RTE.NonAssoc
                                                              MAlonzo.RTE.Unrelated)))
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
                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                                              (coe v1)
                                                              (coe
                                                                 MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
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
                                                              (coe v6))
                                                           (coe
                                                              MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
                                               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                                         v5)
                                      (\ v7 ->
                                         coe
                                           MAlonzo.Code.Interface.MonadTC.d_defineFun_184
                                           MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v2
                                           (coe
                                              MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                              (coe
                                                 MAlonzo.Code.Tactic.ClauseBuilder.d_nonBindingClause_432
                                                 v7))
                                           v5)))
                            v3))))))
