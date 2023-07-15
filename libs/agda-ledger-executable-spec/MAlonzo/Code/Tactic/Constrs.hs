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

module MAlonzo.Code.Tactic.Constrs where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Generics.Utils
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Interface.MonadTC
import qualified MAlonzo.Code.Reflection.Debug
import qualified MAlonzo.Code.Reflection.TCI
import qualified MAlonzo.Code.Tactic.Helpers

-- Tactic.Constrs.applyConstrToUnknowns
d_applyConstrToUnknowns_4 ::
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_applyConstrToUnknowns_4 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 (coe v0)
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22 (coe du_toUnknown_14)
         (coe MAlonzo.Code.Generics.Utils.d_argTys_160 (coe v1)))
-- Tactic.Constrs._.toUnknown
d_toUnknown_14 ::
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88
d_toUnknown_14 ~v0 ~v1 v2 = du_toUnknown_14 v2
du_toUnknown_14 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88
du_toUnknown_14 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v1)
             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.Constrs.tryConstrsWith'
d_tryConstrsWith''_18 ::
  Integer ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_tryConstrsWith''_18 v0 v1
  = case coe v0 of
      0 -> coe
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
             (coe ("tryConstrs" :: Data.Text.Text))
             (coe
                (\ v2 ->
                   coe
                     MAlonzo.Code.Agda.Builtin.Reflection.d_catchTC_348 () erased
                     (coe v1 v2)
                     (coe
                        MAlonzo.Code.Interface.MonadTC.du_error1_664
                        MAlonzo.Code.Reflection.TCI.d_MonadError'45'TC_22 ()
                        MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22
                        ("Maximum depth reached!" :: Data.Text.Text) v2)))
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
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
             (coe ("tryConstrs" :: Data.Text.Text))
             (coe
                (\ v3 ->
                   coe
                     MAlonzo.Code.Agda.Builtin.Reflection.d_catchTC_348 () erased
                     (coe v1 v3)
                     (coe
                        MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                        MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                        (coe
                           MAlonzo.Code.Interface.MonadReader.du_reader_78 ()
                           MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18
                           (coe
                              MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                              (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                           () (\ v4 -> MAlonzo.Code.Interface.MonadTC.d_goal_52 (coe v4)) v3)
                        (\ v4 -> coe du_'46'extendedlambda0_30 v2 v1 v4 v3))))
-- Tactic.Constrs..extendedlambda0
d_'46'extendedlambda0_30 ::
  Integer ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_'46'extendedlambda0_30 v0 v1 ~v2 v3
  = du_'46'extendedlambda0_30 v0 v1 v3
du_'46'extendedlambda0_30 ::
  Integer ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_'46'extendedlambda0_30 v0 v1 v2
  = let v3
          = coe
              MAlonzo.Code.Interface.MonadTC.du_error1_664
              (coe
                 MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                 (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
              (coe ())
              (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
              (coe ("Goal is not a hole!" :: Data.Text.Text)) in
    case coe v2 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v4
        -> coe
             (\ v5 ->
                coe
                  MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                  MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                  (coe
                     MAlonzo.Code.Interface.MonadTC.d_inferType_162
                     MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v4 v5)
                  (\ v6 ->
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
                             (coe ("Find constructor for type " :: Data.Text.Text))
                             (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                             (coe
                                MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v6)
                                (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                       (\ v7 ->
                          coe
                            MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                            MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
                            (coe
                               MAlonzo.Code.Interface.MonadTC.du_getConstrsForTerm_744
                               (coe
                                  MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                  (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                               (coe
                                  MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                  (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                               MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154 v4 v7)
                            (\ v8 ->
                               coe
                                 MAlonzo.Code.Interface.MonadTC.du_try_702
                                 (coe
                                    MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                    (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du_map_22
                                    (coe
                                       (\ v9 ->
                                          coe
                                            MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                            (coe
                                               MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                               (coe
                                                  MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                            (coe ()) (coe ())
                                            (coe
                                               MAlonzo.Code.Interface.MonadTC.du_debugLog_500
                                               (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                               (coe
                                                  MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                               (coe
                                                  MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                                  (coe
                                                     MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                               (coe
                                                  MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                  (coe ("Try constructor " :: Data.Text.Text))
                                                  (coe
                                                     MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                  (coe
                                                     MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                        (coe v9))
                                                     (coe
                                                        MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
                                                     (coe
                                                        MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                        (coe (" of type: " :: Data.Text.Text))
                                                        (coe
                                                           MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                        (coe
                                                           MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                           (coe
                                                              MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                              (coe v9))
                                                           (coe
                                                              MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                           (coe
                                                              MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))
                                            (coe
                                               (\ v10 ->
                                                  coe
                                                    MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                                    MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12
                                                    () erased () erased
                                                    (coe
                                                       MAlonzo.Code.Interface.MonadTC.d_checkType_164
                                                       MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                                       (coe
                                                          MAlonzo.Code.Data.Product.Base.du_uncurry_220
                                                          (coe d_applyConstrToUnknowns_4) (coe v9))
                                                       v6
                                                       (coe
                                                          MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949
                                                          (coe
                                                             MAlonzo.Code.Interface.MonadTC.d_normalisation_40
                                                             (coe v10))
                                                          (coe
                                                             MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                                          (coe
                                                             MAlonzo.Code.Interface.MonadTC.d_noConstraints_44
                                                             (coe v10))
                                                          (coe
                                                             MAlonzo.Code.Interface.MonadTC.d_reduction_46
                                                             (coe v10))
                                                          (coe
                                                             MAlonzo.Code.Interface.MonadTC.d_globalContext_48
                                                             (coe v10))
                                                          (coe
                                                             MAlonzo.Code.Interface.MonadTC.d_localContext_50
                                                             (coe v10))
                                                          (coe
                                                             MAlonzo.Code.Interface.MonadTC.d_goal_52
                                                             (coe v10))
                                                          (coe
                                                             MAlonzo.Code.Interface.MonadTC.d_debug_54
                                                             (coe v10))))
                                                    (\ v11 ->
                                                       coe
                                                         MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                         (coe
                                                            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                            (coe
                                                               MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                         () ()
                                                         (coe
                                                            MAlonzo.Code.Interface.MonadTC.d_unify_156
                                                            MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154
                                                            v4 v11)
                                                         (coe
                                                            MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                            (coe
                                                               MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                               (coe
                                                                  MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                            (coe ()) (coe ())
                                                            (coe
                                                               MAlonzo.Code.Interface.MonadTC.du_debugLog1_668
                                                               (coe
                                                                  MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                                               (coe
                                                                  MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                                               (coe
                                                                  MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                                                  (coe
                                                                     MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                               (coe
                                                                  MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                               (coe ("Success!" :: Data.Text.Text)))
                                                            (coe
                                                               MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                               (coe
                                                                  MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                                  (coe
                                                                     MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                               (coe ()) (coe ())
                                                               (coe
                                                                  MAlonzo.Code.Interface.Monad.du_traverseList_70
                                                                  (coe
                                                                     MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
                                                                     (coe
                                                                        MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                                  (coe ())
                                                                  (coe
                                                                     (\ v12 ->
                                                                        coe
                                                                          MAlonzo.Code.Interface.MonadTC.du_runWithHole_764
                                                                          (coe
                                                                             MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                                                             (coe
                                                                                MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                                                          () v12
                                                                          (d_tryConstrsWith''_18
                                                                             (coe v0) (coe v1))))
                                                                  (coe
                                                                     MAlonzo.Code.Generics.Utils.d_findMetas_24
                                                                     (coe v11)))
                                                               (coe
                                                                  (\ v12 ->
                                                                     coe
                                                                       MAlonzo.Code.Interface.Monad.d_return_28
                                                                       MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12
                                                                       () erased
                                                                       (coe
                                                                          MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))))
                                                         v10)))))
                                    (coe v8))
                                 (coe
                                    MAlonzo.Code.Interface.MonadTC.du_logAndError1_672
                                    (coe MAlonzo.Code.Reflection.TCI.d_Monad'45'TC_18)
                                    (coe
                                       MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                                       (coe
                                          MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                                    (coe MAlonzo.Code.Reflection.TCI.d_MonadTC'45'TCI_154)
                                    (coe
                                       MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
                                       (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
                                    (coe ())
                                    (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                    (coe
                                       ("No constructors were able to solve the goal!"
                                        ::
                                        Data.Text.Text)))
                                 v7))
                       v5))
      _ -> coe v3
-- Tactic.Constrs._.tryConstrsᵗ
d_tryConstrs'7511'_54 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_tryConstrs'7511'_54 v0 v1
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe
         d_tryConstrsWith''_18 (coe v1)
         (coe
            MAlonzo.Code.Interface.MonadTC.du_error1_664
            (coe
               MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
               (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
            (coe ())
            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
            (coe ("Leaf reached!" :: Data.Text.Text))))
-- Tactic.Constrs._.tryConstrsWith
d_tryConstrsWith_58 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  Integer ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_tryConstrsWith_58 v0 v1 v2
  = coe
      MAlonzo.Code.Tactic.Helpers.d_initTac_296 (coe v0)
      (coe d_tryConstrsWith''_18 (coe v1) (coe v2))
-- Tactic.Constrs._.tryConstrs
d_tryConstrs_64 ::
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_tryConstrs_64 v0 = coe d_tryConstrs'7511'_54 (coe v0)
