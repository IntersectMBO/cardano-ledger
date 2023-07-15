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

module MAlonzo.Code.Tactic.AnyOf where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Interface.MonadTC
import qualified MAlonzo.Code.Reflection.Debug
import qualified MAlonzo.Code.Reflection.TCI
import qualified MAlonzo.Code.Tactic.Helpers

-- Tactic.AnyOf.anyOf'
d_anyOf''_4 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_anyOf''_4 v0
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
      (coe ("anyOf" :: Data.Text.Text))
      (coe
         MAlonzo.Code.Data.List.Base.du_foldl_256
         (coe
            (\ v1 v2 v3 ->
               coe
                 MAlonzo.Code.Agda.Builtin.Reflection.d_catchTC_348 () erased
                 (coe
                    MAlonzo.Code.Interface.Monad.du__'62''62'__32
                    (coe
                       MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                       (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                    () ()
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
                             (coe ("Attempting: " :: Data.Text.Text))
                             (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                             (coe
                                MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v2)
                                (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
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
                          (coe v2)))
                    (coe
                       MAlonzo.Code.Interface.MonadTC.du_debugLog_500
                       (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                       (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                       (coe
                          MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                          (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                       (coe
                          MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                          (coe ("Success with: " :: Data.Text.Text))
                          (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                          (coe
                             MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v2)
                             (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                    v3)
                 (coe v1 v3)))
         (coe
            MAlonzo.Code.Interface.MonadTC.du_logAndError1_672
            (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
            (coe
               MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
               (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
            (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
            (coe
               MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
               (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
            (coe ())
            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
            (coe
               ("None of the provded terms solve the goal!" :: Data.Text.Text)))
         (coe v0))
-- Tactic.AnyOf.anyOfⁿ
d_anyOf'8319'_12 ::
  [AgdaAny] -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_anyOf'8319'_12 v0
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
      (coe ("anyOf" :: Data.Text.Text))
      (coe
         MAlonzo.Code.Data.List.Base.du_foldl_256
         (coe
            (\ v1 v2 v3 ->
               coe
                 MAlonzo.Code.Agda.Builtin.Reflection.d_catchTC_348 () erased
                 (coe
                    MAlonzo.Code.Interface.Monad.du__'62''62'__32
                    (coe
                       MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                       (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                    () ()
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
                             (coe ("Attempting: " :: Data.Text.Text))
                             (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                             (coe
                                MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v2)
                                (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
                                (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
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
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v2)
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                    (coe
                       MAlonzo.Code.Interface.MonadTC.du_debugLog_500
                       (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                       (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                       (coe
                          MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                          (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                       (coe
                          MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                          (coe ("Success with: " :: Data.Text.Text))
                          (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                          (coe
                             MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v2)
                             (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                    v3)
                 (coe v1 v3)))
         (coe
            MAlonzo.Code.Interface.MonadTC.du_logAndError1_672
            (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
            (coe
               MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
               (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
            (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
            (coe
               MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
               (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
            (coe ())
            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
            (coe
               ("None of the provded terms solve the goal!" :: Data.Text.Text)))
         (coe v0))
-- Tactic.AnyOf._.anyOfᵗ
d_anyOf'7511'_26 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_anyOf'7511'_26 v0 v1
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe d_anyOf''_4 (coe v1))
-- Tactic.AnyOf._.anyOfⁿᵗ
d_anyOf'8319''7511'_30 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_anyOf'8319''7511'_30 v0 v1
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe d_anyOf'8319'_12 (coe v1))
-- Tactic.AnyOf._.anyOf
d_anyOf_34 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_anyOf_34 v0 = coe d_anyOf'7511'_26 (coe v0)
