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

module MAlonzo.Code.Tactic.Derive where

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
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.String.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Generics.Utils
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Interface.MonadTC
import qualified MAlonzo.Code.Reflection.AST.Abstraction
import qualified MAlonzo.Code.Reflection.AST.Argument
import qualified MAlonzo.Code.Reflection.AST.Name
import qualified MAlonzo.Code.Reflection.Debug
import qualified MAlonzo.Code.Reflection.TCI
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Tactic.ClauseBuilder
import qualified MAlonzo.Code.Tactic.Helpers

-- Tactic.Derive.genClassType
d_genClassType_10 ::
  AgdaAny ->
  AgdaAny ->
  Integer ->
  AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_genClassType_10 v0 ~v1 v2 v3 v4 v5
  = du_genClassType_10 v0 v2 v3 v4 v5
du_genClassType_10 ::
  AgdaAny ->
  Integer ->
  AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_genClassType_10 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
      MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
      (coe
         MAlonzo.Code.Tactic.Helpers.du_getParamsAndIndices_220
         (coe
            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
         (coe
            MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
            (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
         (coe
            MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
         MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v2 v4)
      (\ v5 ->
         coe
           MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
           MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
           (coe
              du_adjustParams_22 v0 v1
              (coe
                 MAlonzo.Code.Data.List.Base.du_take_576
                 (coe
                    MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                    (coe MAlonzo.Code.Data.List.Base.du_length_304 v5) v1)
                 (coe v5))
              v4)
           (\ v6 ->
              coe
                MAlonzo.Code.Interface.Monad.du__'62''62'__32
                (coe
                   MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                   (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                () ()
                (coe
                   MAlonzo.Code.Interface.MonadTC.du_debugLog1_668
                   (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                   (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                   (coe
                      MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                      (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                   (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                   (coe ("AdjustedParams: " :: Data.Text.Text)))
                (coe
                   MAlonzo.Code.Interface.Monad.du__'62''62'__32
                   (coe
                      MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                      (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                   (coe ()) (coe ())
                   (coe
                      MAlonzo.Code.Tactic.Helpers.du_logTelescope_68
                      (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                      (coe
                         MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                         (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                      (coe
                         MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                      (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                      (coe
                         MAlonzo.Code.Data.List.Base.du_map_22
                         (coe
                            (\ v7 ->
                               let v8 = MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7) in
                               case coe v8 of
                                 MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v9 v10
                                   -> coe
                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                        (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v9))
                                        (coe v10)
                                 _ -> MAlonzo.RTE.mazUnreachableError))
                         (coe v6)))
                   (coe
                      (\ v7 ->
                         coe
                           MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                           MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                           (coe
                              MAlonzo.Code.Tactic.Helpers.du_applyWithVisibility_264
                              MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                              (coe
                                 MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                 (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                              (coe
                                 MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                 (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                              MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v2
                              (coe
                                 MAlonzo.Code.Data.List.Base.du_map_22
                                 (coe
                                    (\ v8 ->
                                       coe
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v8)
                                         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                                 (coe du_trueIndices_52 (coe v6)))
                              v7)
                           (\ v8 ->
                              coe
                                MAlonzo.Code.Interface.Monad.d_return_28
                                MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased
                                (coe
                                   du_modifyClassType_58 (coe v0) (coe v3)
                                   (coe
                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                      (coe
                                         MAlonzo.Code.Data.List.Base.du_map_22
                                         (coe
                                            (\ v9 ->
                                               MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v9)))
                                         (coe v6))
                                      (coe v8)))))))
                v4))
-- Tactic.Derive._.adjustParams
d_adjustParams_22 ::
  AgdaAny ->
  AgdaAny ->
  Integer ->
  AgdaAny ->
  Maybe AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104] ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_adjustParams_22 v0 ~v1 v2 ~v3 ~v4 v5
  = du_adjustParams_22 v0 v2 v5
du_adjustParams_22 ::
  AgdaAny ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104] ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_adjustParams_22 v0 v1 v2
  = case coe v2 of
      []
        -> coe
             (\ v3 ->
                coe
                  MAlonzo.Code.Interface.Monad.d_return_28
                  MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased v2)
      (:) v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v5 v6
               -> case coe v6 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v7 v8
                      -> coe
                           (\ v9 ->
                              coe
                                MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                                (coe
                                   MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                                   (coe
                                      MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                      (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                   () ()
                                   (\ v10 ->
                                      coe
                                        MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v10)
                                        (coe
                                           MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
                                                 (coe ("_" :: Data.Text.Text))
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_instance'8242'_54)
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                                                       (coe v0)
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
                                                                MAlonzo.Code.Agda.Builtin.Reflection.C_var_164
                                                                (coe (0 :: Integer))
                                                                (coe
                                                                   MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
                                              (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)))
                                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                                   (coe
                                      MAlonzo.Code.Tactic.Helpers.du_isNArySort_232
                                      (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                      (coe
                                         MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                         (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                      (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                      (coe v1) (coe v8))
                                   v9)
                                (\ v10 ->
                                   coe
                                     MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                     MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased ()
                                     erased
                                     (coe
                                        MAlonzo.Code.Interface.MonadTC.du_extendContext_608
                                        (coe
                                           MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                           (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                        ()
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
                                        (coe du_adjustParams_22 (coe v0) (coe v1) (coe v4)) v9)
                                     (\ v11 ->
                                        coe
                                          MAlonzo.Code.Interface.Monad.d_return_28
                                          MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased
                                          (coe
                                             MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                             (coe
                                                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                   (coe
                                                      MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
                                                      (coe v5)
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
                                                         (coe v8)))
                                                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10))
                                                (coe v10))
                                             (coe
                                                MAlonzo.Code.Data.List.Base.du_map_22
                                                (coe
                                                   (\ v12 ->
                                                      case coe v12 of
                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v13 v14
                                                          -> case coe v13 of
                                                               MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v15 v16
                                                                 -> case coe v16 of
                                                                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v17 v18
                                                                        -> coe
                                                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                             (coe
                                                                                MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
                                                                                (coe v15)
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                   (coe v17)
                                                                                   (coe
                                                                                      MAlonzo.Code.Generics.Utils.d_mapVars_328
                                                                                      (\ v19 ->
                                                                                         addInt
                                                                                           (coe
                                                                                              MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                                                                                              (coe
                                                                                                 v14)
                                                                                              (coe
                                                                                                 MAlonzo.Code.Data.List.Base.du_length_304
                                                                                                 v10)
                                                                                              (coe
                                                                                                 (0 ::
                                                                                                    Integer)))
                                                                                           (coe
                                                                                              v19))
                                                                                      v18)))
                                                                             (coe v14)
                                                                      _ -> MAlonzo.RTE.mazUnreachableError
                                                               _ -> MAlonzo.RTE.mazUnreachableError
                                                        _ -> MAlonzo.RTE.mazUnreachableError))
                                                (coe v11))))))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Derive._.trueIndices
d_trueIndices_52 ::
  AgdaAny ->
  AgdaAny ->
  Integer ->
  AgdaAny ->
  Maybe AgdaAny ->
  () -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> [Integer]
d_trueIndices_52 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_trueIndices_52 v6
du_trueIndices_52 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> [Integer]
du_trueIndices_52 v0
  = case coe v0 of
      [] -> coe v0
      (:) v1 v2
        -> coe
             MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
             (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v1))
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                (coe MAlonzo.Code.Data.List.Base.du_length_304 v2)
                (coe du_trueIndices_52 (coe v2)))
             (coe du_trueIndices_52 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Derive._.modifyClassType
d_modifyClassType_58 ::
  AgdaAny ->
  AgdaAny ->
  Integer ->
  AgdaAny ->
  Maybe AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_modifyClassType_58 v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_modifyClassType_58 v0 v5 v6
du_modifyClassType_58 ::
  AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_modifyClassType_58 v0 v1 v2
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v3
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
               -> coe
                    MAlonzo.Code.Generics.Utils.d_tyView_126
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v0)
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
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v3)
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
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
               -> coe
                    MAlonzo.Code.Generics.Utils.d_tyView_126
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v0)
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
                                (coe v4))
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Derive.lookupName
d_lookupName_82 ::
  AgdaAny ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  AgdaAny -> Maybe AgdaAny
d_lookupName_82 ~v0 ~v1 v2 v3 = du_lookupName_82 v2 v3
du_lookupName_82 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  AgdaAny -> Maybe AgdaAny
du_lookupName_82 v0 v1
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      (:) v2 v3
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
                       erased
                       (MAlonzo.Code.Reflection.AST.Name.d__'8799'__12 (coe v4) (coe v1)))
                    (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v5))
                    (coe du_lookupName_82 (coe v3) (coe v1))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Derive.genMutualHelpers
d_genMutualHelpers_94 ::
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_genMutualHelpers_94 ~v0 ~v1 v2 v3 = du_genMutualHelpers_94 v2 v3
du_genMutualHelpers_94 ::
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_genMutualHelpers_94 v0 v1
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
      MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
      (coe
         MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
         (coe
            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
         () ()
         (coe
            MAlonzo.Code.Data.List.Base.du_map_22
            (coe
               (\ v2 ->
                  coe
                    MAlonzo.Code.Reflection.AST.Argument.du_unArg_74
                    (coe
                       MAlonzo.Code.Reflection.AST.Abstraction.du_unAbs_36 (coe v2)))))
         (coe
            MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
            (coe
               MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
               (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
            (coe ()) (coe ())
            (coe
               MAlonzo.Code.Data.List.Base.du_concatMap_272
               (coe
                  (\ v2 ->
                     MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                       (coe
                          MAlonzo.Code.Generics.Utils.d_viewTy_114
                          (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v2))))))
            (coe
               MAlonzo.Code.Interface.MonadTC.du_getConstrs_714
               (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
               (coe
                  MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                  (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
               (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154) (coe v0)))
         v1)
      (\ v2 ->
         coe
           MAlonzo.Code.Interface.Monad.d_return_28
           MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased
           (coe
              MAlonzo.Code.Data.List.Base.du_deduplicate_834
              MAlonzo.Code.Reflection.AST.Name.d__'8799'__12
              (coe
                 MAlonzo.Code.Data.List.Base.du_mapMaybe_32
                 (coe du_helper_102 (coe v0)) (coe v2))))
-- Tactic.Derive._.helper
d_helper_102 ::
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Maybe AgdaAny
d_helper_102 ~v0 ~v1 v2 v3 = du_helper_102 v2 v3
du_helper_102 ::
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Maybe AgdaAny
du_helper_102 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v3 v4
        -> coe
             MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
             (coe
                MAlonzo.Code.Data.List.Base.du_any_292
                (coe
                   (\ v5 ->
                      case coe v5 of
                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v6 v7
                          -> let v8 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
                             case coe v7 of
                               MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v9 v10
                                 -> coe
                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104
                                      () erased
                                      (MAlonzo.Code.Reflection.AST.Name.d__'8799'__12
                                         (coe v0) (coe v9))
                               _ -> coe v8
                        _ -> MAlonzo.RTE.mazUnreachableError))
                (coe v4))
             (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v3))
             (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
      _ -> coe v2
-- Tactic.Derive._.deriveSingle
d_deriveSingle_122 ::
  AgdaAny ->
  AgdaAny ->
  Integer ->
  ((AgdaAny -> Maybe AgdaAny) ->
   [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
   [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]) ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  AgdaAny ->
  AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_deriveSingle_122 v0 v1 v2 v3 v4 v5 v6 v7
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
      (coe ("DeriveSingle" :: Data.Text.Text))
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
               (coe ("For: " :: Data.Text.Text))
               (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
               (coe
                  MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v5)
                  (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
         (coe
            (\ v8 ->
               coe
                 MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                 MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                 (coe
                    du_genClassType_10 (coe v0) (coe v2) (coe v5) (coe v7) (coe v8))
                 (\ v9 ->
                    coe
                      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                      MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                      (coe
                         MAlonzo.Code.Tactic.ClauseBuilder.du_constructorPatterns''_276
                         MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                         MAlonzo.Code.Reflection.TCI.d_MonadError'45'TC_22
                         (coe
                            MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                         MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                         (coe
                            MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                            (coe MAlonzo.Code.Data.Maybe.Base.du_fromMaybe_50 v5 v7)
                            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                         v8)
                      (\ v10 ->
                         coe
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
                                    (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                 (coe
                                    MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                    (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                 (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154))
                              (coe
                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Reflection.C_instance'8242'_54)
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                    (coe MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 (coe v1))))
                              (coe
                                 MAlonzo.Code.Tactic.ClauseBuilder.du_contMatch_720
                                 (coe
                                    MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                    (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                 (coe
                                    MAlonzo.Code.Tactic.ClauseBuilder.du_multiMatchExprM_632
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
                                    (coe v3 (coe du_lookupName_82 (coe v4)) v10)))
                              (coe
                                 MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949
                                 (coe MAlonzo.Code.Interface.MonadTC.d_normalisation_40 (coe v8))
                                 (coe MAlonzo.Code.Interface.MonadTC.d_reconstruction_42 (coe v8))
                                 (coe MAlonzo.Code.Interface.MonadTC.d_noConstraints_44 (coe v8))
                                 (coe MAlonzo.Code.Interface.MonadTC.d_reduction_46 (coe v8))
                                 (coe MAlonzo.Code.Interface.MonadTC.d_globalContext_48 (coe v8))
                                 (coe MAlonzo.Code.Interface.MonadTC.d_localContext_50 (coe v8))
                                 (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 (coe v9))
                                 (coe MAlonzo.Code.Interface.MonadTC.d_debug_54 (coe v8))))
                           (\ v11 ->
                              coe
                                MAlonzo.Code.Interface.Monad.d_return_28
                                MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                   (coe
                                      MAlonzo.Code.Data.Maybe.Base.du_maybe_36
                                      (coe
                                         (\ v12 ->
                                            coe
                                              MAlonzo.Code.Data.Maybe.Base.du_maybe_36
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
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66))))
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_instance'8242'_54)
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                                 (coe v6))
                                              (coe du_lookupName_82 (coe v4) (coe v12))))
                                      (coe
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                         (coe
                                            MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Reflection.C_instance'8242'_54)
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                         (coe v6))
                                      (coe v7))
                                   (coe
                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v9)
                                      (coe
                                         MAlonzo.Code.Tactic.ClauseBuilder.d_clauseExprToClauses_422
                                         (coe v11))))))))))
-- Tactic.Derive._.deriveMulti
d_deriveMulti_142 ::
  AgdaAny ->
  AgdaAny ->
  Integer ->
  ((AgdaAny -> Maybe AgdaAny) ->
   [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
   [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_deriveMulti_142 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
        -> case coe v6 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
               -> coe
                    (\ v9 ->
                       coe
                         MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                         MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                         (coe
                            MAlonzo.Code.Interface.Monad.du_traverseList_70
                            (coe
                               MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                               (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                            ()
                            (\ v10 ->
                               coe
                                 MAlonzo.Code.Interface.MonadTC.d_freshName_178
                                 MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                 (coe
                                    MAlonzo.Code.Data.String.Base.d__'43''43'__20
                                    (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v0)
                                    (coe
                                       MAlonzo.Code.Data.String.Base.d__'43''43'__20
                                       ("-" :: Data.Text.Text)
                                       (coe
                                          MAlonzo.Code.Data.String.Base.d__'43''43'__20
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12
                                             v10)
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12
                                             v5)))))
                            v8 v9)
                         (\ v10 ->
                            coe
                              MAlonzo.Code.Interface.Monad.du_traverseList_70
                              (coe
                                 MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                 (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                              ()
                              (d_deriveSingle_122
                                 (coe v0) (coe v1) (coe v2) (coe v3)
                                 (coe MAlonzo.Code.Data.List.Base.du_zip_212 v8 v10) (coe v5)
                                 (coe v7))
                              (coe
                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                 (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du_map_22
                                    (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16) (coe v8)))
                              v9))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Derive._.derive-Class
d_derive'45'Class_156 ::
  AgdaAny ->
  AgdaAny ->
  Integer ->
  ((AgdaAny -> Maybe AgdaAny) ->
   [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
   [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]) ->
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_derive'45'Class_156 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initUnquoteWithGoal_302 (coe v4)
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v0)
         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
      (coe
         MAlonzo.Code.Interface.Monad.du__'61''60''60'__40
         (coe
            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
         (coe ()) (coe ())
         (coe
            MAlonzo.Code.Interface.MonadTC.du_declareAndDefineFuns_284
            (coe
               MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
               (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
            (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154))
         (coe
            MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
            (coe
               MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
               (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
            (coe ()) (coe ()) (coe MAlonzo.Code.Data.List.Base.du_concat_270)
            (coe
               MAlonzo.Code.Interface.Monad.du_traverseList_70
               (coe
                  MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                  (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
               (coe ()) (coe du_helper_164 (coe v0) (coe v1) (coe v2) (coe v3))
               (coe v5))))
-- Tactic.Derive._._.helper
d_helper_164 ::
  AgdaAny ->
  AgdaAny ->
  Integer ->
  ((AgdaAny -> Maybe AgdaAny) ->
   [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
   [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]) ->
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_helper_164 v0 v1 v2 v3 ~v4 ~v5 v6 = du_helper_164 v0 v1 v2 v3 v6
du_helper_164 ::
  AgdaAny ->
  AgdaAny ->
  Integer ->
  ((AgdaAny -> Maybe AgdaAny) ->
   [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
   [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_helper_164 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
        -> coe
             (\ v7 ->
                coe
                  MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                  MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                  (coe du_genMutualHelpers_94 (coe v5) (coe v7))
                  (\ v8 ->
                     coe
                       d_deriveMulti_142 v0 v1 v2 v3
                       (coe
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v5)
                          (coe
                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v6) (coe v8)))
                       v7))
      _ -> MAlonzo.RTE.mazUnreachableError
