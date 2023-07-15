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

module MAlonzo.Code.Tactic.Assumption where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Generics.Utils
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Interface.MonadTC
import qualified MAlonzo.Code.Reflection.Debug
import qualified MAlonzo.Code.Reflection.TCI
import qualified MAlonzo.Code.Tactic.Helpers

-- Tactic.Assumption.solve
d_solve_6 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_solve_6 v0 v1
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe
         MAlonzo.Code.Interface.MonadTC.d_runSpeculative_198
         MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 () erased
         (\ v2 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
              (coe
                 MAlonzo.Code.Interface.MonadReader.du_reader_78 ()
                 MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
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
                           (coe ("solve: Goal is not a term!" :: Data.Text.Text)) in
                 case coe v3 of
                   MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5
                     -> coe
                          MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                          MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                          (coe
                             MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                             (coe
                                MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                             () () MAlonzo.Code.Generics.Utils.d_findMetas_24
                             (coe
                                MAlonzo.Code.Interface.MonadTC.d_checkType_164
                                MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v1 v5)
                             v2)
                          (\ v6 ->
                             coe
                               MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                               (coe MAlonzo.Code.Data.List.Base.du_null_282 (coe v6))
                               (coe
                                  MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                                  (coe
                                     MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                     (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                  (coe ()) (coe ())
                                  (coe
                                     (\ v7 ->
                                        coe
                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v7)
                                          (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)))
                                  (coe
                                     MAlonzo.Code.Interface.MonadTC.d_unify_156
                                     MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v5 v1))
                               (coe
                                  MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                                  (coe
                                     MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                     (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                  (coe ()) (coe ())
                                  (coe
                                     (\ v7 ->
                                        coe
                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v7)
                                          (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)))
                                  (coe
                                     MAlonzo.Code.Interface.MonadTC.du_error1_664
                                     (coe
                                        MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                        (coe
                                           MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                     (coe ())
                                     (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                     (coe ("Unsolved metavariables remaining!" :: Data.Text.Text))))
                               v2)
                   _ -> coe v4 v2)))
-- Tactic.Assumption.assumption'
d_assumption''_20 ::
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_assumption''_20
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
      (coe ("assumption" :: Data.Text.Text))
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
              (coe
                 MAlonzo.Code.Interface.MonadTC.du_getContext_618
                 (coe
                    MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                    (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                 (coe
                    MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                    (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                 v0)
              (\ v1 ->
                 coe
                   MAlonzo.Code.Data.List.Base.du_foldl_256
                   (\ v2 v3 v4 ->
                      coe
                        MAlonzo.Code.Agda.Builtin.Reflection.d_catchTC_348 () erased
                        (coe
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
                                 MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v3)
                                 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
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
                                    MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v3)
                                       (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                                    (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                           v4)
                        (coe v2 v4))
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
                      (coe ("No valid assumption!" :: Data.Text.Text)))
                   (coe
                      MAlonzo.Code.Data.List.Base.d_downFrom_432
                      (coe MAlonzo.Code.Data.List.Base.du_length_304 v1))
                   v0)))
-- Tactic.Assumption._.assumption
d_assumption_36 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_assumption_36 v0
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe d_assumption''_20)
-- Tactic.Assumption._.assumptionOpts
d_assumptionOpts_38 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_assumptionOpts_38 ~v0 = du_assumptionOpts_38
du_assumptionOpts_38 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_assumptionOpts_38
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTacOpts_288
      (coe d_assumption''_20)
