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

module MAlonzo.Code.Tactic.Helpers where

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
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.String.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Generics.Utils
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Interface.MonadTC
import qualified MAlonzo.Code.Reflection.AST.Abstraction
import qualified MAlonzo.Code.Reflection.AST.Name
import qualified MAlonzo.Code.Reflection.Debug
import qualified MAlonzo.Code.Reflection.TCI
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Tactic.Helpers.zipWithIndex
d_zipWithIndex_14 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (Integer -> AgdaAny -> AgdaAny) -> [AgdaAny] -> [AgdaAny]
d_zipWithIndex_14 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_zipWithIndex_14 v4 v5
du_zipWithIndex_14 ::
  (Integer -> AgdaAny -> AgdaAny) -> [AgdaAny] -> [AgdaAny]
du_zipWithIndex_14 v0 v1
  = coe
      MAlonzo.Code.Data.List.Base.du_zipWith_134 (coe v0)
      (coe
         MAlonzo.Code.Data.List.Base.d_upTo_430
         (coe MAlonzo.Code.Data.List.Base.du_length_304 v1))
      (coe v1)
-- Tactic.Helpers.DataDef
d_DataDef_20 = ()
data T_DataDef_20
  = C_DataDef'46'constructor_849 AgdaAny
                                 [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
                                 [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
                                 [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
-- Tactic.Helpers.DataDef.name
d_name_30 :: T_DataDef_20 -> AgdaAny
d_name_30 v0
  = case coe v0 of
      C_DataDef'46'constructor_849 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Helpers.DataDef.constructors
d_constructors_32 ::
  T_DataDef_20 -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_constructors_32 v0
  = case coe v0 of
      C_DataDef'46'constructor_849 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Helpers.DataDef.params
d_params_34 ::
  T_DataDef_20 -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
d_params_34 v0
  = case coe v0 of
      C_DataDef'46'constructor_849 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Helpers.DataDef.indices
d_indices_36 ::
  T_DataDef_20 -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
d_indices_36 v0
  = case coe v0 of
      C_DataDef'46'constructor_849 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Helpers.RecordDef
d_RecordDef_38 = ()
data T_RecordDef_38
  = C_RecordDef'46'constructor_989 AgdaAny
                                   [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
                                   [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
-- Tactic.Helpers.RecordDef.name
d_name_46 :: T_RecordDef_38 -> AgdaAny
d_name_46 v0
  = case coe v0 of
      C_RecordDef'46'constructor_989 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Helpers.RecordDef.fields
d_fields_48 ::
  T_RecordDef_38 -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_fields_48 v0
  = case coe v0 of
      C_RecordDef'46'constructor_989 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Helpers.RecordDef.params
d_params_50 ::
  T_RecordDef_38 -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
d_params_50 v0
  = case coe v0 of
      C_RecordDef'46'constructor_989 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Helpers._.logTelescope
d_logTelescope_68 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_logTelescope_68 ~v0 v1 v2 v3 v4 v5
  = du_logTelescope_68 v1 v2 v3 v4 v5
du_logTelescope_68 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
du_logTelescope_68 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.MonadTC.du_withAppendDebugPath_532 (coe v2)
      (coe ()) (coe ("context" :: Data.Text.Text))
      (coe
         MAlonzo.Code.Interface.MonadError.d_catch_58 v1 () erased
         (coe
            du_helper_76 (coe v0) (coe v2) (coe v3)
            (coe MAlonzo.Code.Data.List.Base.du_length_304 v4) (coe v4))
         (\ v5 ->
            coe
              MAlonzo.Code.Interface.MonadTC.du_debugLog1_668 (coe v0) (coe v3)
              (coe v2)
              (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
              (coe ("Error while printing the context!" :: Data.Text.Text))))
-- Tactic.Helpers._._.helper
d_helper_76 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  Integer -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_helper_76 ~v0 v1 ~v2 v3 v4 ~v5 v6 v7
  = du_helper_76 v1 v3 v4 v6 v7
du_helper_76 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  Integer -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
du_helper_76 v0 v1 v2 v3 v4
  = case coe v4 of
      []
        -> coe
             MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
             (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      (:) v5 v6
        -> case coe v5 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
               -> case coe v8 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v9 v10
                      -> coe
                           MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
                           (coe ())
                           (coe
                              MAlonzo.Code.Interface.MonadTC.du_debugLog_500 (coe v0) (coe v2)
                              (coe v1)
                              (coe
                                 MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                 (coe ("  " :: Data.Text.Text))
                                 (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                 (coe
                                    MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                    (case coe v7 of
                                       MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v11
                                         -> case coe v11 of
                                              l | (==) l ("" :: Data.Text.Text) ->
                                                  coe ("_" :: Data.Text.Text)
                                              _ -> coe v11
                                       MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                                         -> coe ("?" :: Data.Text.Text)
                                       _ -> MAlonzo.RTE.mazUnreachableError)
                                    (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                    (coe
                                       MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                       (coe (" : " :: Data.Text.Text))
                                       (coe
                                          MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                       (coe
                                          MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                          (coe
                                             MAlonzo.Code.Generics.Utils.d_mapVars_328
                                             (\ v11 ->
                                                coe
                                                  MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v11
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v3
                                                     (addInt
                                                        (coe (1 :: Integer))
                                                        (coe
                                                           MAlonzo.Code.Data.List.Base.du_length_304
                                                           v6))))
                                             v10)
                                          (coe
                                             MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                          (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))
                           (coe
                              MAlonzo.Code.Interface.MonadTC.du_extendContext_608 v1 () v8
                              (coe du_helper_76 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6)))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Helpers._.logContext
d_logContext_100 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_logContext_100 ~v0 v1 v2 v3 v4 v5
  = du_logContext_100 v1 v2 v3 v4 v5
du_logContext_100 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
du_logContext_100 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.MonadTC.du_withAppendDebugPath_532 (coe v2)
      (coe ()) (coe ("context" :: Data.Text.Text))
      (coe
         MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
         (coe ())
         (coe
            MAlonzo.Code.Interface.MonadTC.du_debugLog1_668 (coe v0) (coe v3)
            (coe v2)
            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
            (coe ("Context:" :: Data.Text.Text)))
         (coe
            MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
            (coe ())
            (coe
               MAlonzo.Code.Interface.MonadError.d_catch_58 v1 () erased
               (coe
                  du_helper_108 (coe v0) (coe v2) (coe v3)
                  (coe MAlonzo.Code.Data.List.Base.du_length_304 v4) (coe v4))
               (\ v5 ->
                  coe
                    MAlonzo.Code.Interface.MonadTC.du_debugLog1_668 (coe v0) (coe v3)
                    (coe v2)
                    (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                    (coe ("Error while printing the context!" :: Data.Text.Text))))
            (coe
               MAlonzo.Code.Interface.MonadError.d_catch_58 v1 () erased
               (coe
                  MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                  erased
                  (coe
                     MAlonzo.Code.Interface.MonadTC.du_goalTy_684 (coe v0) (coe v3)
                     (coe v2))
                  (\ v5 ->
                     coe
                       MAlonzo.Code.Interface.MonadTC.du_debugLog_500 (coe v0) (coe v3)
                       (coe v2)
                       (coe
                          MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                          (coe ("  \8866 " :: Data.Text.Text))
                          (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                          (coe
                             MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v5)
                             (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
               (\ v5 ->
                  coe
                    MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                    erased
                    (coe
                       MAlonzo.Code.Interface.MonadReader.du_reader_78 (coe ()) (coe v0)
                       (coe v2) (coe ())
                       (coe (\ v6 -> MAlonzo.Code.Interface.MonadTC.d_goal_52 (coe v6))))
                    (\ v6 ->
                       let v7
                             = coe
                                 MAlonzo.Code.Interface.MonadTC.du_error1_664 (coe v1) (coe ())
                                 (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                 (coe ("Bug in logContext!" :: Data.Text.Text)) in
                       case coe v6 of
                         MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v8
                           -> coe
                                MAlonzo.Code.Interface.MonadTC.du_debugLog_500 (coe v0) (coe v3)
                                (coe v2)
                                (coe
                                   MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                   (coe
                                      ("Error while infering the goal type! Goal: "
                                       ::
                                       Data.Text.Text))
                                   (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                   (coe
                                      MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v8)
                                      (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                         _ -> coe v7)))))
-- Tactic.Helpers._._.helper
d_helper_108 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_helper_108 ~v0 v1 ~v2 v3 v4 ~v5 v6 v7
  = du_helper_108 v1 v3 v4 v6 v7
du_helper_108 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
du_helper_108 v0 v1 v2 v3 v4
  = case coe v4 of
      []
        -> coe
             MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
             (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      (:) v5 v6
        -> case coe v5 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v7 v8
               -> coe
                    MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
                    (coe ())
                    (coe du_helper_108 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6))
                    (coe
                       MAlonzo.Code.Interface.MonadTC.du_debugLog_500 (coe v0) (coe v2)
                       (coe v1)
                       (coe
                          MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                          (coe ("  " :: Data.Text.Text))
                          (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                          (coe
                             MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                             (coe
                                MAlonzo.Code.Agda.Builtin.Reflection.C_var_164
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v3
                                   (addInt
                                      (coe (1 :: Integer))
                                      (coe MAlonzo.Code.Data.List.Base.du_length_304 v6)))
                                (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                             (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                             (coe
                                MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                (coe (" : " :: Data.Text.Text))
                                (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                (coe
                                   MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                   (coe
                                      MAlonzo.Code.Generics.Utils.d_mapVars_328
                                      (\ v9 ->
                                         addInt
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v3
                                              (coe MAlonzo.Code.Data.List.Base.du_length_304 v6))
                                           (coe v9))
                                      v8)
                                   (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                   (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Helpers._.logCurrentContext
d_logCurrentContext_130 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny
d_logCurrentContext_130 ~v0 v1 v2 v3 v4
  = du_logCurrentContext_130 v1 v2 v3 v4
du_logCurrentContext_130 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny
du_logCurrentContext_130 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.MonadTC.du_markDontFail_676 (coe v0)
      (coe v1) (coe v3) (coe v2) (coe ())
      (coe ("logCurrentContext" :: Data.Text.Text))
      (coe
         MAlonzo.Code.Interface.Monad.du__'61''60''60'__40 (coe v0) (coe ())
         (coe ())
         (coe du_logContext_100 (coe v0) (coe v1) (coe v2) (coe v3))
         (coe
            MAlonzo.Code.Interface.MonadTC.du_getContext_618 (coe v0)
            (coe v2)))
-- Tactic.Helpers._.inDebugPath
d_inDebugPath_132 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny -> AgdaAny
d_inDebugPath_132 ~v0 v1 v2 v3 v4 v5 ~v6 v7 v8
  = du_inDebugPath_132 v1 v2 v3 v4 v5 v7 v8
du_inDebugPath_132 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny -> AgdaAny
du_inDebugPath_132 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Interface.MonadTC.du_withAppendDebugPath_532 (coe v2)
      (coe v4) (coe v5)
      (coe
         MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
         (coe v4)
         (coe
            MAlonzo.Code.Interface.MonadTC.du_debugLog_500 (coe v0) (coe v3)
            (coe v2)
            (coe
               MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
               (coe
                  MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                  (coe ("*****" :: Data.Text.Text))
                  (coe
                     MAlonzo.Code.Data.String.Base.d__'60''43''62'__46 (coe v5)
                     (coe ("*****" :: Data.Text.Text))))
               (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
         (coe
            MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
            (coe v4)
            (coe du_logCurrentContext_130 (coe v0) (coe v1) (coe v2) (coe v3))
            (coe v6)))
-- Tactic.Helpers._.viewAndReduceTy
d_viewAndReduceTy_138 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_viewAndReduceTy_138 ~v0 v1 ~v2 v3 v4 v5
  = du_viewAndReduceTy_138 v1 v3 v4 v5
du_viewAndReduceTy_138 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_viewAndReduceTy_138 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.du__'61''60''60'__40 (coe v0) (coe ())
      (coe ()) (coe du_helper_146 (coe v0) (coe v1) (coe v2) (coe v3))
      (coe
         MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
         (coe ())
         (coe
            (\ v4 ->
               coe
                 MAlonzo.Code.Data.List.Base.du_length_304
                 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                    (coe MAlonzo.Code.Generics.Utils.d_viewTy_114 (coe v4)))))
         (coe MAlonzo.Code.Interface.MonadTC.d_normalise_166 v2 v3))
-- Tactic.Helpers._._.helper
d_helper_146 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer -> AgdaAny
d_helper_146 ~v0 v1 ~v2 v3 v4 ~v5 v6 v7
  = du_helper_146 v1 v3 v4 v6 v7
du_helper_146 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer -> AgdaAny
du_helper_146 v0 v1 v2 v3 v4
  = case coe v4 of
      0 -> coe
             MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
             (MAlonzo.Code.Generics.Utils.d_viewTy_114 (coe v3))
      _ -> let v5 = subInt (coe v4) (coe (1 :: Integer)) in
           let v6 = MAlonzo.Code.Generics.Utils.d_viewTy_114 (coe v3) in
           case coe v6 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
               -> coe
                    MAlonzo.Code.Interface.MonadTC.du_extendContext''_642 (coe v1)
                    (coe ())
                    (coe
                       MAlonzo.Code.Data.List.Base.du_map_22
                       (coe MAlonzo.Code.Reflection.AST.Abstraction.du_unAbs_36) (coe v7))
                    (coe
                       MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                       erased (coe MAlonzo.Code.Interface.MonadTC.d_reduce_168 v2 v8)
                       (\ v9 ->
                          coe
                            MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                            erased
                            (coe du_helper_146 (coe v0) (coe v1) (coe v2) (coe v9) (coe v5))
                            (\ v10 ->
                               case coe v10 of
                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
                                   -> coe
                                        MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                                        (coe
                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                           (coe
                                              MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v7)
                                              (coe v11))
                                           (coe v12))
                                 _ -> MAlonzo.RTE.mazUnreachableError)))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Helpers._.getType'
d_getType''_174 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_getType''_174 ~v0 v1 ~v2 v3 v4 v5 = du_getType''_174 v1 v3 v4 v5
du_getType''_174 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_getType''_174 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.du__'61''60''60'__40 (coe v0) (coe ())
      (coe ()) (coe du_viewAndReduceTy_138 (coe v0) (coe v1) (coe v2))
      (coe MAlonzo.Code.Interface.MonadTC.d_getType_186 v2 v3)
-- Tactic.Helpers._.getDataDef
d_getDataDef_178 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_getDataDef_178 ~v0 v1 v2 v3 v4 v5
  = du_getDataDef_178 v1 v2 v3 v4 v5
du_getDataDef_178 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_getDataDef_178 v0 v1 v2 v3 v4
  = coe
      du_inDebugPath_132 (coe v0) (coe v1) (coe v2) (coe v3) (coe ())
      (coe ("getDataDef" :: Data.Text.Text))
      (coe
         MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
         (coe ())
         (coe
            MAlonzo.Code.Interface.MonadTC.du_debugLog_500 (coe v0) (coe v3)
            (coe v2)
            (coe
               MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
               (coe ("Find details for datatype: " :: Data.Text.Text))
               (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
               (coe
                  MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v4)
                  (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
         (coe
            MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
            erased
            (coe MAlonzo.Code.Interface.MonadTC.d_getDefinition_188 v3 v4)
            (\ v5 ->
               let v6
                     = coe
                         MAlonzo.Code.Interface.MonadError.d_error_56 v1 () erased
                         (coe
                            MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v4)
                            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
                            (coe
                               MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                               (coe (" is not a 'data' type!" :: Data.Text.Text))
                               (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))) in
               case coe v5 of
                 MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v7 v8
                   -> coe
                        MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
                        (coe ())
                        (coe
                           MAlonzo.Code.Interface.MonadTC.du_debugLog'7504'_508 (coe v0)
                           (coe v3) (coe v2)
                           (coe
                              MAlonzo.Code.Interface.MonadTC.du__'8759''7496''7504'__470 (coe v0)
                              (coe ("Constructor names: " :: Data.Text.Text))
                              (coe
                                 MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'IsErrorPart_452
                                 (coe v0)
                                 (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22))
                              (coe
                                 MAlonzo.Code.Interface.MonadTC.du__'8759''7496''7504'__470 (coe v0)
                                 (coe
                                    MAlonzo.Code.Interface.MonadTC.du__'7515'_480 (coe v0) (coe v3)
                                    (coe ()) (coe v8))
                                 (coe
                                    MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'MErrorPartWrap_462)
                                 (coe
                                    MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
                        (coe
                           MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                           erased
                           (coe
                              MAlonzo.Code.Interface.Monad.du_traverseList_70 (coe v0) (coe ())
                              (coe
                                 (\ v9 ->
                                    coe
                                      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0)
                                      (coe ()) (coe ())
                                      (coe
                                         (\ v10 ->
                                            coe
                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v9)
                                              (coe v10)))
                                      (coe du_getType''_174 (coe v0) (coe v2) (coe v3) (coe v9))))
                              (coe v8))
                           (\ v9 ->
                              coe
                                MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
                                (coe ())
                                (coe
                                   MAlonzo.Code.Interface.MonadTC.du_debugLog'7504'_508 (coe v0)
                                   (coe v3) (coe v2)
                                   (coe
                                      MAlonzo.Code.Interface.MonadTC.du__'8759''7496''7504'__470
                                      (coe v0) (coe ("Result: " :: Data.Text.Text))
                                      (coe
                                         MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'IsErrorPart_452
                                         (coe v0)
                                         (coe
                                            MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22))
                                      (coe
                                         MAlonzo.Code.Interface.MonadTC.du__'8759''7496''7504'__470
                                         (coe v0)
                                         (coe
                                            MAlonzo.Code.Interface.MonadTC.du__'7515''8319'_486
                                            (coe v0) (coe v3) (coe v2) (coe ()) (coe v9))
                                         (coe
                                            MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'MErrorPartWrap_462)
                                         (coe
                                            MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                                            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
                                (coe
                                   MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                                   erased
                                   (coe
                                      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0)
                                      (coe ()) (coe ())
                                      (coe
                                         (\ v10 ->
                                            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                                      (coe du_getType''_174 (coe v0) (coe v2) (coe v3) (coe v4)))
                                   (\ v10 ->
                                      coe
                                        MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                                        (coe
                                           C_DataDef'46'constructor_849 (coe v4) (coe v9)
                                           (coe
                                              MAlonzo.Code.Data.List.Base.du_take_576 (coe v7)
                                              (coe v10))
                                           (coe
                                              MAlonzo.Code.Data.List.Base.du_drop_588 (coe v7)
                                              (coe v10)))))))
                 _ -> coe v6)))
-- Tactic.Helpers._.getRecordDef
d_getRecordDef_196 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_getRecordDef_196 ~v0 v1 v2 v3 v4 v5
  = du_getRecordDef_196 v1 v2 v3 v4 v5
du_getRecordDef_196 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_getRecordDef_196 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe MAlonzo.Code.Interface.MonadTC.d_getDefinition_188 v3 v4)
      (\ v5 ->
         let v6
               = coe
                   MAlonzo.Code.Interface.MonadError.d_error_56 v1 () erased
                   (coe
                      MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v4)
                      (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
                      (coe
                         MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                         (coe (" is not a 'record' type!" :: Data.Text.Text))
                         (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))) in
         case coe v5 of
           MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v7 v8
             -> coe
                  MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                  erased
                  (coe
                     MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
                     (coe ())
                     (coe (\ v9 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v9)))
                     (coe du_getType''_174 (coe v0) (coe v2) (coe v3) (coe v4)))
                  (\ v9 ->
                     coe
                       MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                       (coe C_RecordDef'46'constructor_989 (coe v7) (coe v8) (coe v9)))
           _ -> coe v6)
-- Tactic.Helpers._.getDataOrRecordDef
d_getDataOrRecordDef_208 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_getDataOrRecordDef_208 ~v0 v1 v2 v3 v4 v5
  = du_getDataOrRecordDef_208 v1 v2 v3 v4 v5
du_getDataOrRecordDef_208 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_getDataOrRecordDef_208 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.MonadError.d_catch_58 v1 () erased
      (coe
         MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
         (coe ()) (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
         (coe
            du_getDataDef_178 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)))
      (\ v5 ->
         coe
           MAlonzo.Code.Interface.MonadError.d_catch_58 v1 () erased
           (coe
              MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
              (coe ()) (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42)
              (coe
                 du_getRecordDef_196 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)))
           (\ v6 ->
              coe
                MAlonzo.Code.Interface.MonadError.d_error_56 v1 () erased
                (coe
                   MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v4)
                   (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
                   (coe
                      MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                      (coe
                         (" is neither a 'data' not a 'record' type!" :: Data.Text.Text))
                      (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
-- Tactic.Helpers._.getParams
d_getParams_216 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_getParams_216 ~v0 v1 v2 v3 v4 v5
  = du_getParams_216 v1 v2 v3 v4 v5
du_getParams_216 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_getParams_216 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
      (coe ())
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52
         (coe (\ v5 -> d_params_34 (coe v5)))
         (coe (\ v5 -> d_params_50 (coe v5))))
      (coe
         du_getDataOrRecordDef_208 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe v4))
-- Tactic.Helpers._.getParamsAndIndices
d_getParamsAndIndices_220 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_getParamsAndIndices_220 ~v0 v1 v2 v3 v4 v5
  = du_getParamsAndIndices_220 v1 v2 v3 v4 v5
du_getParamsAndIndices_220 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_getParamsAndIndices_220 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
      (coe ())
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52
         (coe
            (\ v5 ->
               coe
                 MAlonzo.Code.Data.List.Base.du__'43''43'__62
                 (coe d_params_34 (coe v5)) (coe d_indices_36 (coe v5))))
         (coe (\ v5 -> d_params_50 (coe v5))))
      (coe
         du_getDataOrRecordDef_208 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe v4))
-- Tactic.Helpers._.isSort
d_isSort_226 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_isSort_226 ~v0 v1 ~v2 ~v3 v4 v5 = du_isSort_226 v1 v4 v5
du_isSort_226 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_isSort_226 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe MAlonzo.Code.Interface.MonadTC.d_normalise_166 v1 v2)
      (\ v3 ->
         let v4
               = coe
                   MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8) in
         case coe v3 of
           MAlonzo.Code.Agda.Builtin.Reflection.C_agda'45'sort_198 v5
             -> coe
                  MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                  (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
           _ -> coe v4)
-- Tactic.Helpers._.isNArySort
d_isNArySort_232 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_isNArySort_232 ~v0 v1 ~v2 v3 v4 v5 v6
  = du_isNArySort_232 v1 v3 v4 v5 v6
du_isNArySort_232 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_isNArySort_232 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe du_viewAndReduceTy_138 (coe v0) (coe v1) (coe v2) (coe v4))
      (\ v5 ->
         case coe v5 of
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
             -> coe
                  MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                  erased (coe du_isSort_226 (coe v0) (coe v2) (coe v7))
                  (\ v8 ->
                     coe
                       MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                       (MAlonzo.Code.Data.Bool.Base.d__'8743'__24
                          (coe
                             MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                             erased
                             (MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                (coe MAlonzo.Code.Data.List.Base.du_length_304 v6) (coe v3)))
                          (coe v8)))
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Tactic.Helpers._.isDefT
d_isDefT_246 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_isDefT_246 ~v0 v1 ~v2 ~v3 v4 v5 v6 = du_isDefT_246 v1 v4 v5 v6
du_isDefT_246 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_isDefT_246 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe MAlonzo.Code.Interface.MonadTC.d_normalise_166 v1 v3)
      (\ v4 ->
         let v5
               = coe
                   MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8) in
         case coe v4 of
           MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v6 v7
             -> coe
                  MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                  (coe
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                     erased
                     (MAlonzo.Code.Reflection.AST.Name.d__'8799'__12 (coe v2) (coe v6)))
           _ -> coe v5)
-- Tactic.Helpers._.withSafeReset
d_withSafeReset_256 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_withSafeReset_256 ~v0 v1 v2 v3 v4 v5
  = du_withSafeReset_256 v1 v2 v3 v4 v5
du_withSafeReset_256 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_withSafeReset_256 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.MonadTC.du_runAndReset_234 (coe v0) (coe v3)
      (coe ())
      (coe
         MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
         erased v4
         (\ v5 ->
            coe
              MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
              (coe
                 MAlonzo.Code.Data.List.Base.du_null_282
                 (coe MAlonzo.Code.Generics.Utils.d_findMetas_24 (coe v5)))
              (coe MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased v5)
              (coe
                 MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
                 (coe ())
                 (coe
                    MAlonzo.Code.Interface.MonadTC.du_debugLog'7504'_508 (coe v0)
                    (coe v3) (coe v2)
                    (coe
                       MAlonzo.Code.Interface.MonadTC.du__'8759''7496''7504'__470 (coe v0)
                       (coe ("Remaining metavariables:" :: Data.Text.Text))
                       (coe
                          MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'IsErrorPart_452
                          (coe v0)
                          (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22))
                       (coe
                          MAlonzo.Code.Interface.MonadTC.du__'8759''7496''7504'__470 (coe v0)
                          (coe
                             MAlonzo.Code.Interface.MonadTC.du__'7515''8319'_486 (coe v0)
                             (coe v3) (coe v2) (coe ())
                             (coe MAlonzo.Code.Generics.Utils.d_findMetas_24 (coe v5)))
                          (coe
                             MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'MErrorPartWrap_462)
                          (coe
                             MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
                 (coe
                    MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
                    (coe ())
                    (coe
                       MAlonzo.Code.Interface.MonadTC.du_debugLog_500 (coe v0) (coe v3)
                       (coe v2)
                       (coe
                          MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                          (coe ("In term: " :: Data.Text.Text))
                          (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                          (coe
                             MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v5)
                             (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                    (coe
                       MAlonzo.Code.Interface.MonadTC.du_error1_664 (coe v1) (coe ())
                       (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                       (coe
                          ("Unsolved metavariables remaining in withSafeReset!"
                           ::
                           Data.Text.Text)))))))
-- Tactic.Helpers._.applyWithVisibility
d_applyWithVisibility_264 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] -> AgdaAny
d_applyWithVisibility_264 ~v0 v1 v2 v3 v4 v5 v6
  = du_applyWithVisibility_264 v1 v2 v3 v4 v5 v6
du_applyWithVisibility_264 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] -> AgdaAny
du_applyWithVisibility_264 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe du_getType''_174 (coe v0) (coe v2) (coe v3) (coe v4))
      (\ v6 ->
         case coe v6 of
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
             -> coe
                  MAlonzo.Code.Interface.MonadTC.du_nameConstr_258 (coe v0) (coe v1)
                  (coe v3) (coe v4)
                  (coe
                     MAlonzo.Code.Data.List.Base.du_zipWith_134
                     (coe
                        (\ v9 ->
                           case coe v9 of
                             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v10 v11
                               -> case coe v11 of
                                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v12 v13
                                      -> coe MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v12)
                                    _ -> MAlonzo.RTE.mazUnreachableError
                             _ -> MAlonzo.RTE.mazUnreachableError))
                     (coe v7) (coe v5))
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Tactic.Helpers.ITactic
d_ITactic_278 :: ()
d_ITactic_278 = erased
-- Tactic.Helpers.initTacEnv
d_initTacEnv_280 ::
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 ->
   MAlonzo.Code.Interface.MonadTC.T_TCEnv_22) ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_initTacEnv_280 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (MAlonzo.Code.Interface.MonadTC.d_initTCEnvWithGoal_56 (coe v2))
      (\ v3 -> coe v1 (coe v0 v3))
-- Tactic.Helpers.initTacOpts
d_initTacOpts_288 ::
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_initTacOpts_288 v0 v1
  = coe
      d_initTacEnv_280
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949
              (coe MAlonzo.Code.Interface.MonadTC.d_normalisation_40 (coe v2))
              (coe MAlonzo.Code.Interface.MonadTC.d_reconstruction_42 (coe v2))
              (coe MAlonzo.Code.Interface.MonadTC.d_noConstraints_44 (coe v2))
              (coe MAlonzo.Code.Interface.MonadTC.d_reduction_46 (coe v2))
              (coe MAlonzo.Code.Interface.MonadTC.d_globalContext_48 (coe v2))
              (coe MAlonzo.Code.Interface.MonadTC.d_localContext_50 (coe v2))
              (coe MAlonzo.Code.Interface.MonadTC.d_goal_52 (coe v2)) (coe v1)))
      (coe v0)
-- Tactic.Helpers.initTac
d_initTac_296 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_initTac_296 v0 v1 = coe d_initTacOpts_288 (coe v1) (coe v0)
-- Tactic.Helpers.initUnquoteWithGoal
d_initUnquoteWithGoal_302 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) -> AgdaAny
d_initUnquoteWithGoal_302 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (MAlonzo.Code.Interface.MonadTC.d_initTCEnvWithGoal_56 (coe v1))
      (\ v3 ->
         coe
           v2
           (coe
              MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949
              (coe MAlonzo.Code.Interface.MonadTC.d_normalisation_40 (coe v3))
              (coe MAlonzo.Code.Interface.MonadTC.d_reconstruction_42 (coe v3))
              (coe MAlonzo.Code.Interface.MonadTC.d_noConstraints_44 (coe v3))
              (coe MAlonzo.Code.Interface.MonadTC.d_reduction_46 (coe v3))
              (coe MAlonzo.Code.Interface.MonadTC.d_globalContext_48 (coe v3))
              (coe MAlonzo.Code.Interface.MonadTC.d_localContext_50 (coe v3))
              (coe MAlonzo.Code.Interface.MonadTC.d_goal_52 (coe v3)) (coe v0)))
-- Tactic.Helpers.initUnquote
d_initUnquote_312 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) -> AgdaAny
d_initUnquote_312 v0 v1
  = coe
      d_initUnquoteWithGoal_302 (coe v0)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208) (coe v1)
-- Tactic.Helpers._.byTC
d_byTC_324 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_byTC_324 v0 v1 ~v2 v3 = du_byTC_324 v0 v1 v3
du_byTC_324 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_byTC_324 v0 v1 v2
  = coe
      d_initTac_296 (coe v0)
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
              (coe
                 MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                 MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 v1 erased () erased
                 (coe v2 v3)
                 (\ v4 ->
                    coe
                      MAlonzo.Code.Interface.MonadTC.d_quoteTC_170
                      MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v1 erased v4 v3))
              (\ v4 ->
                 coe
                   MAlonzo.Code.Interface.MonadTC.du_unifyWithGoal_756
                   MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                   (coe
                      MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                      (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                   MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                   (coe
                      MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                      (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                   v4 v3)))
-- Tactic.Helpers._.by
d_by_328 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_by_328 v0 = coe d_initTac_296 (coe v0)
