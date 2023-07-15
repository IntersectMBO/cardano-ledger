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

module MAlonzo.Code.Interface.MonadTC where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.String.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Reflection.AST.Abstraction
import qualified MAlonzo.Code.Reflection.Debug

-- Interface.MonadTC.Monad-TC
d_Monad'45'TC_12 :: MAlonzo.Code.Interface.Monad.T_Monad_20
d_Monad'45'TC_12
  = coe
      MAlonzo.Code.Interface.Monad.C_Monad'46'constructor_273
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316)
      (coe
         (\ v0 v1 v2 ->
            coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 v0 v2 erased))
-- Interface.MonadTC.ReductionOptions
d_ReductionOptions_14 = ()
data T_ReductionOptions_14
  = C_onlyReduce_16 [AgdaAny] | C_dontReduce_18 [AgdaAny]
-- Interface.MonadTC.reduceAll
d_reduceAll_20 :: T_ReductionOptions_14
d_reduceAll_20
  = coe
      C_dontReduce_18 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Interface.MonadTC.TCEnv
d_TCEnv_22 = ()
data T_TCEnv_22
  = C_TCEnv'46'constructor_949 Bool Bool Bool T_ReductionOptions_14
                               [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
                               [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
                               MAlonzo.Code.Data.Sum.Base.T__'8846'__30
                               MAlonzo.Code.Reflection.Debug.T_DebugOptions_234
-- Interface.MonadTC.TCEnv.normalisation
d_normalisation_40 :: T_TCEnv_22 -> Bool
d_normalisation_40 v0
  = case coe v0 of
      C_TCEnv'46'constructor_949 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.TCEnv.reconstruction
d_reconstruction_42 :: T_TCEnv_22 -> Bool
d_reconstruction_42 v0
  = case coe v0 of
      C_TCEnv'46'constructor_949 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.TCEnv.noConstraints
d_noConstraints_44 :: T_TCEnv_22 -> Bool
d_noConstraints_44 v0
  = case coe v0 of
      C_TCEnv'46'constructor_949 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.TCEnv.reduction
d_reduction_46 :: T_TCEnv_22 -> T_ReductionOptions_14
d_reduction_46 v0
  = case coe v0 of
      C_TCEnv'46'constructor_949 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.TCEnv.globalContext
d_globalContext_48 ::
  T_TCEnv_22 -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_globalContext_48 v0
  = case coe v0 of
      C_TCEnv'46'constructor_949 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.TCEnv.localContext
d_localContext_50 ::
  T_TCEnv_22 -> [MAlonzo.Code.Agda.Builtin.Reflection.T_Abs_104]
d_localContext_50 v0
  = case coe v0 of
      C_TCEnv'46'constructor_949 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.TCEnv.goal
d_goal_52 :: T_TCEnv_22 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_goal_52 v0
  = case coe v0 of
      C_TCEnv'46'constructor_949 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.TCEnv.debug
d_debug_54 ::
  T_TCEnv_22 -> MAlonzo.Code.Reflection.Debug.T_DebugOptions_234
d_debug_54 v0
  = case coe v0 of
      C_TCEnv'46'constructor_949 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.initTCEnvWithGoal
d_initTCEnvWithGoal_56 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_initTCEnvWithGoal_56 v0
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''38''62'__68
      (coe d_Monad'45'TC_12) (coe ()) (coe ())
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_getContext_366)
      (coe
         (\ v1 ->
            coe
              C_TCEnv'46'constructor_949
              (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
              (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
              (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8) (coe d_reduceAll_20)
              (coe
                 MAlonzo.Code.Data.List.Base.du_map_22
                 (coe (\ v2 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v2)))
                 (coe v1))
              (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
              (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v0))
              (coe MAlonzo.Code.Reflection.Debug.d_defaultDebugOptions_252)))
-- Interface.MonadTC.initTCEnv
d_initTCEnv_94 :: AgdaAny
d_initTCEnv_94
  = coe
      d_initTCEnvWithGoal_56
      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208)
-- Interface.MonadTC.MonadTC
d_MonadTC_104 a0 a1 a2 = ()
data T_MonadTC_104
  = C_MonadTC'46'constructor_6643 (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
                                   MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny)
                                  (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                   () ->
                                   [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] ->
                                   AgdaAny)
                                  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny)
                                  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
                                   MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny)
                                  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny)
                                  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny)
                                  (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                   () -> AgdaAny -> AgdaAny)
                                  (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                   () -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny)
                                  (() -> AgdaAny -> AgdaAny)
                                  (MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny)
                                  (MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
                                   MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny)
                                  (MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
                                   MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny)
                                  (AgdaAny ->
                                   [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny)
                                  (AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
                                  (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                   () -> AgdaAny -> AgdaAny)
                                  AgdaAny (AgdaAny -> AgdaAny)
                                  (MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
                                   Integer ->
                                   [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] ->
                                   AgdaAny)
                                  (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                   () -> AgdaAny -> AgdaAny)
-- Interface.MonadTC.MonadTC.unify
d_unify_156 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_unify_156 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.typeError
d_typeError_160 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
d_typeError_160 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.inferType
d_inferType_162 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_inferType_162 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.checkType
d_checkType_164 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_checkType_164 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.normalise
d_normalise_166 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_normalise_166 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.reduce
d_reduce_168 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_reduce_168 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.quoteTC
d_quoteTC_170 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_quoteTC_170 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.unquoteTC
d_unquoteTC_172 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_unquoteTC_172 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.quoteωTC
d_quoteωTC_176 :: T_MonadTC_104 -> () -> AgdaAny -> AgdaAny
d_quoteωTC_176 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.freshName
d_freshName_178 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_freshName_178 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.declareDef
d_declareDef_180 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_declareDef_180 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v11
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.declarePostulate
d_declarePostulate_182 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_declarePostulate_182 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v12
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.defineFun
d_defineFun_184 ::
  T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
d_defineFun_184 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v13
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.getType
d_getType_186 :: T_MonadTC_104 -> AgdaAny -> AgdaAny
d_getType_186 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v14
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.getDefinition
d_getDefinition_188 :: T_MonadTC_104 -> AgdaAny -> AgdaAny
d_getDefinition_188 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v15
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.blockOnMeta
d_blockOnMeta_190 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_blockOnMeta_190 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v16
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.commitTC
d_commitTC_192 :: T_MonadTC_104 -> AgdaAny
d_commitTC_192 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v17
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.isMacro
d_isMacro_194 :: T_MonadTC_104 -> AgdaAny -> AgdaAny
d_isMacro_194 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v18
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.debugPrint
d_debugPrint_196 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
d_debugPrint_196 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v19
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.runSpeculative
d_runSpeculative_198 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_runSpeculative_198 v0
  = case coe v0 of
      C_MonadTC'46'constructor_6643 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
        -> coe v20
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC.MonadTC.runAndReset
d_runAndReset_234 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_runAndReset_234 ~v0 v1 ~v2 v3 v4 ~v5 v6
  = du_runAndReset_234 v1 v3 v4 v6
du_runAndReset_234 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> AgdaAny -> AgdaAny
du_runAndReset_234 v0 v1 v2 v3
  = coe
      d_runSpeculative_198 v1 v2 erased
      (coe
         MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe v2)
         (coe v2)
         (coe
            (\ v4 ->
               coe
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4)
                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)))
         (coe v3))
-- Interface.MonadTC.MonadTC.isSuccessful
d_isSuccessful_240 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_isSuccessful_240 ~v0 v1 v2 v3 v4 ~v5 v6
  = du_isSuccessful_240 v1 v2 v3 v4 v6
du_isSuccessful_240 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> AgdaAny -> AgdaAny
du_isSuccessful_240 v0 v1 v2 v3 v4
  = coe
      du_runAndReset_234 (coe v0) (coe v2) (coe ())
      (coe
         MAlonzo.Code.Interface.MonadError.d_catch_58 v1 () erased
         (coe
            MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe v3)
            (coe ()) (coe v4)
            (coe
               MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
               (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)))
         (\ v5 ->
            coe
              MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
              (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)))
-- Interface.MonadTC.MonadTC.isDef
d_isDef_246 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 -> AgdaAny -> AgdaAny
d_isDef_246 ~v0 v1 ~v2 v3 v4 = du_isDef_246 v1 v3 v4
du_isDef_246 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 -> AgdaAny -> AgdaAny
du_isDef_246 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe d_getDefinition_188 v1 v2)
      (\ v3 ->
         let v4
               = coe
                   MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10) in
         case coe v3 of
           MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'cons_292 v5
             -> coe
                  MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                  (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
           _ -> coe v4)
-- Interface.MonadTC.MonadTC.isCon
d_isCon_252 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 -> AgdaAny -> AgdaAny
d_isCon_252 ~v0 v1 ~v2 v3 v4 = du_isCon_252 v1 v3 v4
du_isCon_252 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 -> AgdaAny -> AgdaAny
du_isCon_252 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe d_getDefinition_188 v1 v2)
      (\ v3 ->
         let v4
               = coe
                   MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                   (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8) in
         case coe v3 of
           MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'cons_292 v5
             -> coe
                  MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                  (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
           _ -> coe v4)
-- Interface.MonadTC.MonadTC.nameConstr
d_nameConstr_258 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_nameConstr_258 ~v0 v1 v2 v3 v4 v5
  = du_nameConstr_258 v1 v2 v3 v4 v5
du_nameConstr_258 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
du_nameConstr_258 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe du_isDef_246 (coe v0) (coe v2) (coe v3))
      (\ v5 ->
         coe
           MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
           erased (coe du_isCon_252 (coe v0) (coe v2) (coe v3))
           (\ v6 ->
              if coe v5
                then coe
                       MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v3) (coe v4))
                else (if coe v6
                        then coe
                               MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                               (coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 (coe v3) (coe v4))
                        else coe
                               MAlonzo.Code.Interface.MonadError.d_error_56 v1 () erased
                               (coe
                                  MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                  (coe
                                     MAlonzo.Code.Data.String.Base.d__'60''43''62'__46
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v3)
                                     (coe
                                        ("is neither a definition nor a constructor!"
                                         ::
                                         Data.Text.Text)))
                                  (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
-- Interface.MonadTC.MonadTC.termFromName
d_termFromName_270 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 -> AgdaAny -> AgdaAny
d_termFromName_270 ~v0 v1 v2 v3 v4
  = du_termFromName_270 v1 v2 v3 v4
du_termFromName_270 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 -> AgdaAny -> AgdaAny
du_termFromName_270 v0 v1 v2 v3
  = coe
      du_nameConstr_258 (coe v0) (coe v1) (coe v2) (coe v3)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Interface.MonadTC.MonadTC.mkRecord
d_mkRecord_274 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_mkRecord_274 ~v0 v1 v2 v3 v4 v5 = du_mkRecord_274 v1 v2 v3 v4 v5
du_mkRecord_274 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
du_mkRecord_274 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe d_getDefinition_188 v2 v3)
      (\ v5 ->
         let v6
               = coe
                   MAlonzo.Code.Interface.MonadError.d_error_56 v1 () erased
                   (coe
                      MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                      (coe ("Not a record!" :: Data.Text.Text))
                      (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)) in
         case coe v5 of
           MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v7 v8
             -> coe
                  MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 (coe v7) (coe v4))
           _ -> coe v6)
-- Interface.MonadTC.MonadTC.declareAndDefineFuns
d_declareAndDefineFuns_284 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_declareAndDefineFuns_284 ~v0 v1 ~v2 v3 v4
  = du_declareAndDefineFuns_284 v1 v3 v4
du_declareAndDefineFuns_284 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
du_declareAndDefineFuns_284 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
      (coe ())
      (coe
         MAlonzo.Code.Interface.Monad.du_traverseList_70 (coe v0) (coe ())
         (coe
            (\ v3 ->
               case coe v3 of
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                   -> case coe v5 of
                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                          -> coe d_declareDef_180 v1 v4 v6
                        _ -> MAlonzo.RTE.mazUnreachableError
                 _ -> MAlonzo.RTE.mazUnreachableError))
         (coe v2))
      (coe
         MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
         (coe ())
         (coe
            MAlonzo.Code.Interface.Monad.du_traverseList_70 (coe v0) (coe ())
            (coe
               (\ v3 ->
                  case coe v3 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                      -> case coe v4 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v6 v7
                             -> case coe v5 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
                                    -> coe d_defineFun_184 v1 v7 v9
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError))
            (coe v2))
         (coe
            MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
            (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
-- Interface.MonadTC.MonadTC.declareAndDefineFun
d_declareAndDefineFun_300 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
d_declareAndDefineFun_300 ~v0 v1 ~v2 v3 v4 v5 v6
  = du_declareAndDefineFun_300 v1 v3 v4 v5 v6
du_declareAndDefineFun_300 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
du_declareAndDefineFun_300 v0 v1 v2 v3 v4
  = coe
      du_declareAndDefineFuns_284 (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
            (coe
               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v4))))
-- Interface.MonadTC.MonadTC.newMeta
d_newMeta_308 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_newMeta_308 ~v0 ~v1 ~v2 v3 = du_newMeta_308 v3
du_newMeta_308 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_newMeta_308 v0
  = coe
      d_checkType_164 v0
      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208)
-- Interface.MonadTC._._.blockOnMeta
d_blockOnMeta_362 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_blockOnMeta_362 ~v0 ~v1 ~v2 v3 ~v4 = du_blockOnMeta_362 v3
du_blockOnMeta_362 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
du_blockOnMeta_362 v0 = coe d_blockOnMeta_190 (coe v0)
-- Interface.MonadTC._._.checkType
d_checkType_364 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_checkType_364 ~v0 ~v1 ~v2 v3 ~v4 = du_checkType_364 v3
du_checkType_364 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_checkType_364 v0 = coe d_checkType_164 (coe v0)
-- Interface.MonadTC._._.commitTC
d_commitTC_366 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
d_commitTC_366 ~v0 ~v1 ~v2 v3 ~v4 = du_commitTC_366 v3
du_commitTC_366 :: T_MonadTC_104 -> AgdaAny
du_commitTC_366 v0 = coe d_commitTC_192 (coe v0)
-- Interface.MonadTC._._.debugPrint
d_debugPrint_368 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
d_debugPrint_368 ~v0 ~v1 ~v2 v3 ~v4 = du_debugPrint_368 v3
du_debugPrint_368 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
du_debugPrint_368 v0 = coe d_debugPrint_196 (coe v0)
-- Interface.MonadTC._._.declareAndDefineFun
d_declareAndDefineFun_370 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
d_declareAndDefineFun_370 ~v0 v1 ~v2 v3 ~v4
  = du_declareAndDefineFun_370 v1 v3
du_declareAndDefineFun_370 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
du_declareAndDefineFun_370 v0 v1
  = coe du_declareAndDefineFun_300 (coe v0) (coe v1)
-- Interface.MonadTC._._.declareAndDefineFuns
d_declareAndDefineFuns_372 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_declareAndDefineFuns_372 ~v0 v1 ~v2 v3 ~v4
  = du_declareAndDefineFuns_372 v1 v3
du_declareAndDefineFuns_372 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
du_declareAndDefineFuns_372 v0 v1
  = coe du_declareAndDefineFuns_284 (coe v0) (coe v1)
-- Interface.MonadTC._._.declareDef
d_declareDef_374 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_declareDef_374 ~v0 ~v1 ~v2 v3 ~v4 = du_declareDef_374 v3
du_declareDef_374 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_declareDef_374 v0 = coe d_declareDef_180 (coe v0)
-- Interface.MonadTC._._.declarePostulate
d_declarePostulate_376 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_declarePostulate_376 ~v0 ~v1 ~v2 v3 ~v4
  = du_declarePostulate_376 v3
du_declarePostulate_376 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_declarePostulate_376 v0 = coe d_declarePostulate_182 (coe v0)
-- Interface.MonadTC._._.defineFun
d_defineFun_378 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
d_defineFun_378 ~v0 ~v1 ~v2 v3 ~v4 = du_defineFun_378 v3
du_defineFun_378 ::
  T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
du_defineFun_378 v0 = coe d_defineFun_184 (coe v0)
-- Interface.MonadTC._._.freshName
d_freshName_380 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_freshName_380 ~v0 ~v1 ~v2 v3 ~v4 = du_freshName_380 v3
du_freshName_380 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
du_freshName_380 v0 = coe d_freshName_178 (coe v0)
-- Interface.MonadTC._._.getDefinition
d_getDefinition_382 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
d_getDefinition_382 ~v0 ~v1 ~v2 v3 ~v4 = du_getDefinition_382 v3
du_getDefinition_382 :: T_MonadTC_104 -> AgdaAny -> AgdaAny
du_getDefinition_382 v0 = coe d_getDefinition_188 (coe v0)
-- Interface.MonadTC._._.getType
d_getType_384 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
d_getType_384 ~v0 ~v1 ~v2 v3 ~v4 = du_getType_384 v3
du_getType_384 :: T_MonadTC_104 -> AgdaAny -> AgdaAny
du_getType_384 v0 = coe d_getType_186 (coe v0)
-- Interface.MonadTC._._.inferType
d_inferType_386 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_inferType_386 ~v0 ~v1 ~v2 v3 ~v4 = du_inferType_386 v3
du_inferType_386 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_inferType_386 v0 = coe d_inferType_162 (coe v0)
-- Interface.MonadTC._._.isCon
d_isCon_388 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
d_isCon_388 ~v0 v1 ~v2 v3 ~v4 = du_isCon_388 v1 v3
du_isCon_388 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 -> AgdaAny -> AgdaAny
du_isCon_388 v0 v1 = coe du_isCon_252 (coe v0) (coe v1)
-- Interface.MonadTC._._.isDef
d_isDef_390 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
d_isDef_390 ~v0 v1 ~v2 v3 ~v4 = du_isDef_390 v1 v3
du_isDef_390 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 -> AgdaAny -> AgdaAny
du_isDef_390 v0 v1 = coe du_isDef_246 (coe v0) (coe v1)
-- Interface.MonadTC._._.isMacro
d_isMacro_392 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
d_isMacro_392 ~v0 ~v1 ~v2 v3 ~v4 = du_isMacro_392 v3
du_isMacro_392 :: T_MonadTC_104 -> AgdaAny -> AgdaAny
du_isMacro_392 v0 = coe d_isMacro_194 (coe v0)
-- Interface.MonadTC._._.isSuccessful
d_isSuccessful_394 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_isSuccessful_394 ~v0 v1 v2 v3 ~v4 = du_isSuccessful_394 v1 v2 v3
du_isSuccessful_394 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
du_isSuccessful_394 v0 v1 v2 v3 v4 v5
  = coe du_isSuccessful_240 (coe v0) (coe v1) (coe v2) v3 v5
-- Interface.MonadTC._._.mkRecord
d_mkRecord_396 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_mkRecord_396 ~v0 v1 v2 v3 ~v4 = du_mkRecord_396 v1 v2 v3
du_mkRecord_396 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
du_mkRecord_396 v0 v1 v2
  = coe du_mkRecord_274 (coe v0) (coe v1) (coe v2)
-- Interface.MonadTC._._.nameConstr
d_nameConstr_398 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_nameConstr_398 ~v0 v1 v2 v3 ~v4 = du_nameConstr_398 v1 v2 v3
du_nameConstr_398 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
du_nameConstr_398 v0 v1 v2
  = coe du_nameConstr_258 (coe v0) (coe v1) (coe v2)
-- Interface.MonadTC._._.newMeta
d_newMeta_400 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_newMeta_400 ~v0 ~v1 ~v2 v3 ~v4 = du_newMeta_400 v3
du_newMeta_400 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_newMeta_400 v0 = coe du_newMeta_308 (coe v0)
-- Interface.MonadTC._._.normalise
d_normalise_402 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_normalise_402 ~v0 ~v1 ~v2 v3 ~v4 = du_normalise_402 v3
du_normalise_402 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_normalise_402 v0 = coe d_normalise_166 (coe v0)
-- Interface.MonadTC._._.quoteTC
d_quoteTC_404 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_quoteTC_404 ~v0 ~v1 ~v2 v3 ~v4 = du_quoteTC_404 v3
du_quoteTC_404 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
du_quoteTC_404 v0 = coe d_quoteTC_170 (coe v0)
-- Interface.MonadTC._._.quoteωTC
d_quoteωTC_406 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  () -> AgdaAny -> AgdaAny
d_quoteωTC_406 ~v0 ~v1 ~v2 v3 ~v4 = du_quoteωTC_406 v3
du_quoteωTC_406 :: T_MonadTC_104 -> () -> AgdaAny -> AgdaAny
du_quoteωTC_406 v0 = coe d_quoteωTC_176 (coe v0)
-- Interface.MonadTC._._.reduce
d_reduce_408 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_reduce_408 ~v0 ~v1 ~v2 v3 ~v4 = du_reduce_408 v3
du_reduce_408 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_reduce_408 v0 = coe d_reduce_168 (coe v0)
-- Interface.MonadTC._._.runAndReset
d_runAndReset_410 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_runAndReset_410 ~v0 v1 ~v2 v3 ~v4 = du_runAndReset_410 v1 v3
du_runAndReset_410 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
du_runAndReset_410 v0 v1 v2 v3 v4
  = coe du_runAndReset_234 (coe v0) (coe v1) v2 v4
-- Interface.MonadTC._._.runSpeculative
d_runSpeculative_412 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_runSpeculative_412 ~v0 ~v1 ~v2 v3 ~v4 = du_runSpeculative_412 v3
du_runSpeculative_412 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
du_runSpeculative_412 v0 = coe d_runSpeculative_198 (coe v0)
-- Interface.MonadTC._._.termFromName
d_termFromName_414 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
d_termFromName_414 ~v0 v1 v2 v3 ~v4 = du_termFromName_414 v1 v2 v3
du_termFromName_414 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 -> AgdaAny -> AgdaAny
du_termFromName_414 v0 v1 v2
  = coe du_termFromName_270 (coe v0) (coe v1) (coe v2)
-- Interface.MonadTC._._.typeError
d_typeError_416 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
d_typeError_416 ~v0 ~v1 ~v2 v3 ~v4 = du_typeError_416 v3
du_typeError_416 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
du_typeError_416 v0 = coe d_typeError_160 (coe v0)
-- Interface.MonadTC._._.unify
d_unify_418 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_unify_418 ~v0 ~v1 ~v2 v3 ~v4 = du_unify_418 v3
du_unify_418 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_unify_418 v0 = coe d_unify_156 (coe v0)
-- Interface.MonadTC._._.unquoteTC
d_unquoteTC_420 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_unquoteTC_420 ~v0 ~v1 ~v2 v3 ~v4 = du_unquoteTC_420 v3
du_unquoteTC_420 ::
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_unquoteTC_420 v0 = coe d_unquoteTC_172 (coe v0)
-- Interface.MonadTC._.IsMErrorPart
d_IsMErrorPart_434 a0 a1 a2 a3 a4 a5 a6 = ()
newtype T_IsMErrorPart_434
  = C_IsMErrorPart'46'constructor_24391 (AgdaAny -> AgdaAny)
-- Interface.MonadTC._.IsMErrorPart.toMErrorPart
d_toMErrorPart_440 :: T_IsMErrorPart_434 -> AgdaAny -> AgdaAny
d_toMErrorPart_440 v0
  = case coe v0 of
      C_IsMErrorPart'46'constructor_24391 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC._._.toMErrorPart
d_toMErrorPart_444 :: T_IsMErrorPart_434 -> AgdaAny -> AgdaAny
d_toMErrorPart_444 v0 = coe d_toMErrorPart_440 (coe v0)
-- Interface.MonadTC._.MErrorPartWrap
d_MErrorPartWrap_446 a0 a1 a2 a3 a4 = ()
newtype T_MErrorPartWrap_446 = C_wrap_448 AgdaAny
-- Interface.MonadTC._.IsMErrorPart-IsErrorPart
d_IsMErrorPart'45'IsErrorPart_452 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  () ->
  MAlonzo.Code.Reflection.Debug.T_IsErrorPart_10 ->
  T_IsMErrorPart_434
d_IsMErrorPart'45'IsErrorPart_452 ~v0 v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_IsMErrorPart'45'IsErrorPart_452 v1 v6
du_IsMErrorPart'45'IsErrorPart_452 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Reflection.Debug.T_IsErrorPart_10 ->
  T_IsMErrorPart_434
du_IsMErrorPart'45'IsErrorPart_452 v0 v1
  = coe
      C_IsMErrorPart'46'constructor_24391
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
              (coe
                 MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                 (coe MAlonzo.Code.Reflection.Debug.d_toErrorPart_16 v1 v2))))
-- Interface.MonadTC._.IsMErrorPart-String
d_IsMErrorPart'45'String_456 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  T_IsMErrorPart_434
d_IsMErrorPart'45'String_456 ~v0 v1 ~v2 ~v3 ~v4
  = du_IsMErrorPart'45'String_456 v1
du_IsMErrorPart'45'String_456 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 -> T_IsMErrorPart_434
du_IsMErrorPart'45'String_456 v0
  = coe
      du_IsMErrorPart'45'IsErrorPart_452 (coe v0)
      (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
-- Interface.MonadTC._.IsMErrorPart-Term
d_IsMErrorPart'45'Term_458 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  T_IsMErrorPart_434
d_IsMErrorPart'45'Term_458 ~v0 v1 ~v2 ~v3 ~v4
  = du_IsMErrorPart'45'Term_458 v1
du_IsMErrorPart'45'Term_458 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 -> T_IsMErrorPart_434
du_IsMErrorPart'45'Term_458 v0
  = coe
      du_IsMErrorPart'45'IsErrorPart_452 (coe v0)
      (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
-- Interface.MonadTC._.IsMErrorPart-Name
d_IsMErrorPart'45'Name_460 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  T_IsMErrorPart_434
d_IsMErrorPart'45'Name_460 ~v0 v1 ~v2 ~v3 ~v4
  = du_IsMErrorPart'45'Name_460 v1
du_IsMErrorPart'45'Name_460 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 -> T_IsMErrorPart_434
du_IsMErrorPart'45'Name_460 v0
  = coe
      du_IsMErrorPart'45'IsErrorPart_452 (coe v0)
      (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
-- Interface.MonadTC._.IsMErrorPart-MErrorPartWrap
d_IsMErrorPart'45'MErrorPartWrap_462 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  T_IsMErrorPart_434
d_IsMErrorPart'45'MErrorPartWrap_462 ~v0 ~v1 ~v2 ~v3 ~v4
  = du_IsMErrorPart'45'MErrorPartWrap_462
du_IsMErrorPart'45'MErrorPartWrap_462 :: T_IsMErrorPart_434
du_IsMErrorPart'45'MErrorPartWrap_462
  = coe
      C_IsMErrorPart'46'constructor_24391
      (coe
         (\ v0 ->
            case coe v0 of
              C_wrap_448 v1 -> coe v1
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Interface.MonadTC._.[]ᵐ
d_'91''93''7504'_466 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
d_'91''93''7504'_466 v0 ~v1 ~v2 ~v3 = du_'91''93''7504'_466 v0
du_'91''93''7504'_466 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 -> AgdaAny
du_'91''93''7504'_466 v0
  = coe
      MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Interface.MonadTC._._∷ᵈᵐ_
d__'8759''7496''7504'__470 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> T_IsMErrorPart_434 -> AgdaAny -> AgdaAny
d__'8759''7496''7504'__470 ~v0 v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9
  = du__'8759''7496''7504'__470 v1 v7 v8 v9
du__'8759''7496''7504'__470 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  AgdaAny -> T_IsMErrorPart_434 -> AgdaAny -> AgdaAny
du__'8759''7496''7504'__470 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe d_toMErrorPart_440 v2 v1)
      (\ v4 ->
         coe
           MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
           erased v3
           (\ v5 ->
              coe
                MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                (coe
                   MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4) (coe v5))))
-- Interface.MonadTC._._ᵛ
d__'7515'_480 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> T_MErrorPartWrap_446
d__'7515'_480 ~v0 v1 ~v2 v3 ~v4 v5 ~v6 v7
  = du__'7515'_480 v1 v3 v5 v7
du__'7515'_480 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> T_MErrorPartWrap_446
du__'7515'_480 v0 v1 v2 v3
  = coe
      C_wrap_448
      (coe
         MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
         erased (coe d_quoteTC_170 v1 v2 erased v3)
         (\ v4 ->
            coe
              MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
              (coe
                 MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v4)
                 (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
-- Interface.MonadTC._._ᵛⁿ
d__'7515''8319'_486 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> T_MErrorPartWrap_446
d__'7515''8319'_486 ~v0 v1 ~v2 v3 v4 v5 ~v6 v7
  = du__'7515''8319'_486 v1 v3 v4 v5 v7
du__'7515''8319'_486 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> T_MErrorPartWrap_446
du__'7515''8319'_486 v0 v1 v2 v3 v4
  = coe
      C_wrap_448
      (coe
         MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
         erased
         (coe
            MAlonzo.Code.Interface.MonadReader.d_local_72 v2 () erased
            (\ v5 ->
               coe
                 C_TCEnv'46'constructor_949
                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                 (coe d_reconstruction_42 (coe v5))
                 (coe d_noConstraints_44 (coe v5)) (coe d_reduction_46 (coe v5))
                 (coe d_globalContext_48 (coe v5)) (coe d_localContext_50 (coe v5))
                 (coe d_goal_52 (coe v5)) (coe d_debug_54 (coe v5)))
            (coe d_quoteTC_170 v1 v3 erased v4))
         (\ v5 ->
            coe
              MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
              (coe
                 MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v5)
                 (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
-- Interface.MonadTC._._ᵗ
d__'7511'_494 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_MErrorPartWrap_446
d__'7511'_494 v0 ~v1 v2 ~v3 v4 = du__'7511'_494 v0 v2 v4
du__'7511'_494 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_MErrorPartWrap_446
du__'7511'_494 v0 v1 v2
  = coe
      C_wrap_448
      (coe
         MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
         erased (coe d_inferType_162 v1 v2)
         (\ v3 ->
            coe
              MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
              (coe
                 MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v2)
                 (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                 (coe
                    MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                    (coe (" : " :: Data.Text.Text))
                    (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                    (coe
                       MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v3)
                       (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                       (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))
-- Interface.MonadTC._.debugLog
d_debugLog_500 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
d_debugLog_500 ~v0 v1 ~v2 v3 v4 v5 = du_debugLog_500 v1 v3 v4 v5
du_debugLog_500 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
du_debugLog_500 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (MAlonzo.Code.Interface.MonadReader.d_ask_66 (coe v2))
      (\ v4 ->
         case coe v4 of
           C_TCEnv'46'constructor_949 v5 v6 v7 v8 v9 v10 v11 v12
             -> coe
                  MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                  (coe
                     MAlonzo.Code.Reflection.Debug.d_filter_248 v12
                     (MAlonzo.Code.Reflection.Debug.d_path_244 (coe v12)))
                  (coe
                     d_debugPrint_196 v1
                     (MAlonzo.Code.Reflection.Debug.d_debugOptionsPath_262 (coe v12))
                     (MAlonzo.Code.Reflection.Debug.d_level_250 (coe v12))
                     (coe
                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                        (coe
                           MAlonzo.Code.Reflection.Debug.d_debugPrintPrefix_284 (coe v12))
                        (coe v3)))
                  (coe
                     MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                     (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Interface.MonadTC._.debugLogᵐ
d_debugLog'7504'_508 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
d_debugLog'7504'_508 ~v0 v1 ~v2 v3 v4 v5
  = du_debugLog'7504'_508 v1 v3 v4 v5
du_debugLog'7504'_508 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
du_debugLog'7504'_508 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased v3 (coe du_debugLog_500 (coe v0) (coe v1) (coe v2))
-- Interface.MonadTC._.debugLog1ᵐ
d_debugLog1'7504'_516 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> T_IsMErrorPart_434 -> AgdaAny
d_debugLog1'7504'_516 ~v0 v1 ~v2 v3 v4 ~v5 ~v6 v7 v8
  = du_debugLog1'7504'_516 v1 v3 v4 v7 v8
du_debugLog1'7504'_516 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> T_IsMErrorPart_434 -> AgdaAny
du_debugLog1'7504'_516 v0 v1 v2 v3 v4
  = coe
      du_debugLog'7504'_508 (coe v0) (coe v1) (coe v2)
      (coe
         du__'8759''7496''7504'__470 (coe v0) (coe v3) (coe v4)
         (coe
            MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
-- Interface.MonadTC._.withDebugOptions
d_withDebugOptions_520 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  AgdaAny -> AgdaAny
d_withDebugOptions_520 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 v7 v8
  = du_withDebugOptions_520 v4 v5 v7 v8
du_withDebugOptions_520 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Reflection.Debug.T_DebugOptions_234 ->
  AgdaAny -> AgdaAny
du_withDebugOptions_520 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.MonadReader.d_local_72 v0 v1 erased
      (\ v4 ->
         case coe v4 of
           C_TCEnv'46'constructor_949 v5 v6 v7 v8 v9 v10 v11 v12
             -> coe
                  C_TCEnv'46'constructor_949 (coe v5) (coe v6) (coe v7) (coe v8)
                  (coe v9) (coe v10) (coe v11)
                  (coe
                     MAlonzo.Code.Reflection.Debug.d_specializeDebugOptions_254
                     (coe v12) (coe v2))
           _ -> MAlonzo.RTE.mazUnreachableError)
      v3
-- Interface.MonadTC._.withAppendDebugPath
d_withAppendDebugPath_532 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny -> AgdaAny
d_withAppendDebugPath_532 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 v7 v8
  = du_withAppendDebugPath_532 v4 v5 v7 v8
du_withAppendDebugPath_532 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny -> AgdaAny
du_withAppendDebugPath_532 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.MonadReader.d_local_72 v0 v1 erased
      (\ v4 ->
         case coe v4 of
           C_TCEnv'46'constructor_949 v5 v6 v7 v8 v9 v10 v11 v12
             -> coe
                  C_TCEnv'46'constructor_949 (coe v5) (coe v6) (coe v7) (coe v8)
                  (coe v9) (coe v10) (coe v11)
                  (coe
                     MAlonzo.Code.Reflection.Debug.C_DebugOptions'46'constructor_1975
                     (coe
                        MAlonzo.Code.Data.List.Base.du__'8759''691'__494
                        (coe MAlonzo.Code.Reflection.Debug.d_path_244 (coe v12)) (coe v2))
                     (coe MAlonzo.Code.Reflection.Debug.d_selection_246 (coe v12))
                     (coe MAlonzo.Code.Reflection.Debug.d_filter_248 (coe v12))
                     (coe MAlonzo.Code.Reflection.Debug.d_level_250 (coe v12)))
           _ -> MAlonzo.RTE.mazUnreachableError)
      v3
-- Interface.MonadTC._.noConstraints
d_noConstraints_544 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_noConstraints_544 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6
  = du_noConstraints_544 v4 v5
du_noConstraints_544 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> AgdaAny -> AgdaAny
du_noConstraints_544 v0 v1
  = coe
      MAlonzo.Code.Interface.MonadReader.d_local_72 v0 v1 erased
      (\ v2 ->
         coe
           C_TCEnv'46'constructor_949 (coe d_normalisation_40 (coe v2))
           (coe d_reconstruction_42 (coe v2))
           (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
           (coe d_reduction_46 (coe v2)) (coe d_globalContext_48 (coe v2))
           (coe d_localContext_50 (coe v2)) (coe d_goal_52 (coe v2))
           (coe d_debug_54 (coe v2)))
-- Interface.MonadTC._.hasType
d_hasType_548 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_hasType_548 ~v0 v1 v2 v3 v4 v5 v6
  = du_hasType_548 v1 v2 v3 v4 v5 v6
du_hasType_548 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_hasType_548 v0 v1 v2 v3 v4 v5
  = coe
      du_isSuccessful_240 (coe v0) (coe v1) (coe v2) (coe ())
      (coe du_noConstraints_544 v3 () (coe d_checkType_164 v2 v4 v5))
-- Interface.MonadTC._.hasType'
d_hasType''_554 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_hasType''_554 ~v0 v1 v2 v3 v4 v5 v6
  = du_hasType''_554 v1 v2 v3 v4 v5 v6
du_hasType''_554 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_hasType''_554 v0 v1 v2 v3 v4 v5
  = coe
      du_isSuccessful_240 (coe v0) (coe v1) (coe v2) (coe ())
      (coe
         du_noConstraints_544 v3 ()
         (coe
            MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
            erased
            (coe du_termFromName_270 (coe v0) (coe v1) (coe v2) (coe v4))
            (\ v6 ->
               coe
                 MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                 erased (coe d_checkType_164 v2 v6 v5)
                 (\ v7 ->
                    coe
                      MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                      (coe du_isAppliedToUnknownsAndMetas_582 (coe v7))
                      (coe
                         MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
                      (coe
                         MAlonzo.Code.Interface.MonadError.d_error_56 v1 () erased
                         (coe
                            MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                            (coe ("This makes the function return false" :: Data.Text.Text))
                            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))))
-- Interface.MonadTC._._.isUnknownsAndMetas
d_isUnknownsAndMetas_564 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> Bool
d_isUnknownsAndMetas_564 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isUnknownsAndMetas_564 v7
du_isUnknownsAndMetas_564 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> Bool
du_isUnknownsAndMetas_564 v0
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      (:) v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v3 v4
               -> let v5 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
                  case coe v4 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v6 v7
                      -> coe du_isUnknownsAndMetas_564 (coe v2)
                    MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208
                      -> coe du_isUnknownsAndMetas_564 (coe v2)
                    _ -> coe v5
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC._._.isAppliedToUnknownsAndMetas
d_isAppliedToUnknownsAndMetas_582 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool
d_isAppliedToUnknownsAndMetas_582 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du_isAppliedToUnknownsAndMetas_582 v7
du_isAppliedToUnknownsAndMetas_582 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Bool
du_isAppliedToUnknownsAndMetas_582 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10 in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v2 v3
        -> coe du_isUnknownsAndMetas_564 (coe v3)
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v2 v3
        -> coe du_isUnknownsAndMetas_564 (coe v3)
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v2 v3
        -> coe du_isUnknownsAndMetas_564 (coe v3)
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v2 v3
        -> coe du_isUnknownsAndMetas_564 (coe v3)
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v2 v3
        -> coe du_isUnknownsAndMetas_564 (coe v3)
      _ -> coe v1
-- Interface.MonadTC._.extendContext
d_extendContext_608 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny -> AgdaAny
d_extendContext_608 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 v7
  = du_extendContext_608 v4 v5 v7
du_extendContext_608 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny -> AgdaAny
du_extendContext_608 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.MonadReader.d_local_72 v0 v1 erased
      (\ v3 ->
         case coe v3 of
           C_TCEnv'46'constructor_949 v4 v5 v6 v7 v8 v9 v10 v11
             -> coe
                  C_TCEnv'46'constructor_949 (coe v4) (coe v5) (coe v6) (coe v7)
                  (coe v8)
                  (coe
                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                     (coe
                        MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
                        (coe ("" :: Data.Text.Text)) (coe v2))
                     (coe v9))
                  (coe v10) (coe v11)
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Interface.MonadTC._.getContext
d_getContext_618 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
d_getContext_618 ~v0 v1 ~v2 ~v3 v4 = du_getContext_618 v1 v4
du_getContext_618 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
du_getContext_618 v0 v1
  = coe
      MAlonzo.Code.Interface.MonadReader.du_reader_78 (coe ()) (coe v0)
      (coe v1) (coe ())
      (coe
         (\ v2 ->
            case coe v2 of
              C_TCEnv'46'constructor_949 v3 v4 v5 v6 v7 v8 v9 v10
                -> coe
                     MAlonzo.Code.Data.List.Base.du__'43''43'__62
                     (coe
                        MAlonzo.Code.Data.List.Base.du_map_22
                        (coe MAlonzo.Code.Reflection.AST.Abstraction.du_unAbs_36) (coe v8))
                     (coe v7)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Interface.MonadTC._.getLocalContext
d_getLocalContext_628 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
d_getLocalContext_628 ~v0 v1 ~v2 ~v3 v4
  = du_getLocalContext_628 v1 v4
du_getLocalContext_628 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
du_getLocalContext_628 v0 v1
  = coe
      MAlonzo.Code.Interface.MonadReader.du_reader_78 (coe ()) (coe v0)
      (coe v1) (coe ())
      (coe
         (\ v2 ->
            case coe v2 of
              C_TCEnv'46'constructor_949 v3 v4 v5 v6 v7 v8 v9 v10
                -> coe
                     MAlonzo.Code.Data.List.Base.du_map_22
                     (coe MAlonzo.Code.Reflection.AST.Abstraction.du_unAbs_36) (coe v8)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Interface.MonadTC._.inContext
d_inContext_636 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> AgdaAny
d_inContext_636 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 v7
  = du_inContext_636 v4 v5 v7
du_inContext_636 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> AgdaAny
du_inContext_636 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.MonadReader.d_local_72 v0 v1 erased
      (\ v3 ->
         coe
           C_TCEnv'46'constructor_949 (coe d_normalisation_40 (coe v3))
           (coe d_reconstruction_42 (coe v3))
           (coe d_noConstraints_44 (coe v3)) (coe d_reduction_46 (coe v3))
           (coe d_globalContext_48 (coe v3))
           (coe
              MAlonzo.Code.Data.List.Base.du_map_22
              (coe
                 MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114
                 (coe ("" :: Data.Text.Text)))
              (coe v2))
           (coe d_goal_52 (coe v3)) (coe d_debug_54 (coe v3)))
-- Interface.MonadTC._.extendContext'
d_extendContext''_642 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> AgdaAny
d_extendContext''_642 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 v7 v8
  = du_extendContext''_642 v4 v5 v7 v8
du_extendContext''_642 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> AgdaAny
du_extendContext''_642 v0 v1 v2 v3
  = case coe v2 of
      [] -> coe v3
      (:) v4 v5
        -> coe
             du_extendContext_608 v0 v1 v4
             (coe du_extendContext''_642 (coe v0) (coe v1) (coe v5) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC._.dropContext
d_dropContext_652 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> AgdaAny -> AgdaAny
d_dropContext_652 ~v0 v1 ~v2 ~v3 v4 v5 ~v6 v7 v8
  = du_dropContext_652 v1 v4 v5 v7 v8
du_dropContext_652 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  Integer -> AgdaAny -> AgdaAny
du_dropContext_652 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased v2
      erased (coe du_getContext_618 (coe v0) (coe v1))
      (\ v5 ->
         coe
           du_inContext_636 v1 v2
           (coe MAlonzo.Code.Data.List.Base.du_drop_588 (coe v3) (coe v5)) v4)
-- Interface.MonadTC._.logAndError
d_logAndError_660 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
d_logAndError_660 ~v0 v1 v2 v3 v4 v5 ~v6 v7
  = du_logAndError_660 v1 v2 v3 v4 v5 v7
du_logAndError_660 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
du_logAndError_660 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
      (coe v4) (coe du_debugLog_500 (coe v0) (coe v2) (coe v3) (coe v5))
      (coe MAlonzo.Code.Interface.MonadError.d_error_56 v1 v4 erased v5)
-- Interface.MonadTC._.error1
d_error1_664 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Reflection.Debug.T_IsErrorPart_10 ->
  AgdaAny -> AgdaAny
d_error1_664 ~v0 ~v1 v2 ~v3 ~v4 ~v5 v6 ~v7 v8 v9
  = du_error1_664 v2 v6 v8 v9
du_error1_664 ::
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Reflection.Debug.T_IsErrorPart_10 ->
  AgdaAny -> AgdaAny
du_error1_664 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.MonadError.d_error_56 v0 v1 erased
      (coe
         MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v3)
         (coe v2) (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
-- Interface.MonadTC._.debugLog1
d_debugLog1_668 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  () ->
  MAlonzo.Code.Reflection.Debug.T_IsErrorPart_10 ->
  AgdaAny -> AgdaAny
d_debugLog1_668 ~v0 v1 ~v2 v3 v4 ~v5 v6 v7
  = du_debugLog1_668 v1 v3 v4 v6 v7
du_debugLog1_668 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Reflection.Debug.T_IsErrorPart_10 ->
  AgdaAny -> AgdaAny
du_debugLog1_668 v0 v1 v2 v3 v4
  = coe
      du_debugLog_500 (coe v0) (coe v1) (coe v2)
      (coe
         MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v4)
         (coe v3) (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
-- Interface.MonadTC._.logAndError1
d_logAndError1_672 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Reflection.Debug.T_IsErrorPart_10 ->
  AgdaAny -> AgdaAny
d_logAndError1_672 ~v0 v1 v2 v3 v4 ~v5 v6 ~v7 v8 v9
  = du_logAndError1_672 v1 v2 v3 v4 v6 v8 v9
du_logAndError1_672 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Reflection.Debug.T_IsErrorPart_10 ->
  AgdaAny -> AgdaAny
du_logAndError1_672 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
      (coe v4)
      (coe du_debugLog1_668 (coe v0) (coe v2) (coe v3) (coe v5) (coe v6))
      (coe du_error1_664 (coe v1) (coe v4) (coe v5) (coe v6))
-- Interface.MonadTC._.markDontFail
d_markDontFail_676 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny -> AgdaAny
d_markDontFail_676 ~v0 v1 v2 v3 v4 v5 ~v6 v7 v8
  = du_markDontFail_676 v1 v2 v3 v4 v5 v7 v8
du_markDontFail_676 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny -> AgdaAny
du_markDontFail_676 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Interface.MonadError.d_catch_58 v1 v4 erased v6
      (\ v7 ->
         coe
           du_logAndError_660 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
           (coe
              MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v5)
              (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
              (coe
                 MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                 (coe
                    (" should never fail! This is a bug!\nError:\n" :: Data.Text.Text))
                 (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                 (coe v7))))
-- Interface.MonadTC._.goalTy
d_goalTy_684 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
d_goalTy_684 ~v0 v1 ~v2 v3 v4 = du_goalTy_684 v1 v3 v4
du_goalTy_684 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
du_goalTy_684 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe
         MAlonzo.Code.Interface.MonadReader.du_reader_78 (coe ()) (coe v0)
         (coe v2) (coe ()) (coe (\ v3 -> d_goal_52 (coe v3))))
      (\ v3 ->
         case coe v3 of
           MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v4
             -> coe d_inferType_162 v1 v4
           MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v4
             -> coe MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased v4
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Interface.MonadTC._.runSpeculativeMaybe
d_runSpeculativeMaybe_692 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d_runSpeculativeMaybe_692 ~v0 v1 ~v2 v3 ~v4 v5 ~v6 v7 v8
  = du_runSpeculativeMaybe_692 v1 v3 v5 v7 v8
du_runSpeculativeMaybe_692 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_runSpeculativeMaybe_692 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 v2 erased v2
      erased
      (coe
         d_runSpeculative_198 v1 v2 erased
         (coe
            MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe v2)
            (coe v2)
            (coe
               MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88
               (coe (\ v5 -> v5))
               (coe MAlonzo.Code.Data.Maybe.Base.du_is'45'just_20))
            (coe v3)))
      (\ v5 ->
         case coe v5 of
           MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v6
             -> coe MAlonzo.Code.Interface.Monad.d_return_28 v0 v2 erased v6
           MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v4
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Interface.MonadTC._.try
d_try_702 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  [AgdaAny] -> AgdaAny -> AgdaAny
d_try_702 ~v0 ~v1 v2 ~v3 ~v4 v5 v6 = du_try_702 v2 v5 v6
du_try_702 ::
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  [AgdaAny] -> AgdaAny -> AgdaAny
du_try_702 v0 v1 v2
  = case coe v1 of
      [] -> coe v2
      (:) v3 v4
        -> coe
             MAlonzo.Code.Interface.MonadError.d_catch_58 v0 () erased v3
             (\ v5 -> coe du_try_702 (coe v0) (coe v4) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.MonadTC._.getConstrs
d_getConstrs_714 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
d_getConstrs_714 ~v0 v1 v2 v3 ~v4 v5
  = du_getConstrs_714 v1 v2 v3 v5
du_getConstrs_714 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 -> AgdaAny -> AgdaAny
du_getConstrs_714 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe d_getDefinition_188 v2 v3)
      (\ v4 ->
         coe
           MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
           erased
           (let v5
                  = coe
                      MAlonzo.Code.Interface.MonadError.d_error_56 v1 () erased
                      (coe
                         MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v3)
                         (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Name_26)
                         (coe
                            MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                            (coe ("is not a data or record definition!" :: Data.Text.Text))
                            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))) in
            case coe v4 of
              MAlonzo.Code.Agda.Builtin.Reflection.C_data'45'type_282 v6 v7
                -> coe MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased v7
              MAlonzo.Code.Agda.Builtin.Reflection.C_record'45'type_288 v6 v7
                -> coe
                     MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                     (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v6))
              _ -> coe v5)
           (\ v5 ->
              coe
                MAlonzo.Code.Interface.Monad.du_traverseList_70 (coe v0) (coe ())
                (coe
                   (\ v6 ->
                      coe
                        MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
                        (coe ())
                        (coe
                           (\ v7 ->
                              coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v6) (coe v7)))
                        (coe
                           MAlonzo.Code.Interface.Monad.du__'61''60''60'__40 (coe v0) (coe ())
                           (coe ()) (coe d_normalise_166 (coe v2))
                           (coe d_getType_186 v2 v6))))
                (coe v5)))
-- Interface.MonadTC._.getConstrsForType
d_getConstrsForType_736 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_getConstrsForType_736 ~v0 v1 v2 v3 ~v4 v5
  = du_getConstrsForType_736 v1 v2 v3 v5
du_getConstrsForType_736 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_getConstrsForType_736 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe d_normalise_166 v2 v3)
      (\ v4 ->
         let v5
               = coe
                   MAlonzo.Code.Interface.MonadError.d_error_56 v1 () erased
                   (coe
                      MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v3)
                      (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                      (coe
                         MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                         (coe ("does not reduce to a definition!" :: Data.Text.Text))
                         (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))) in
         case coe v4 of
           MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v6 v7
             -> coe du_getConstrs_714 (coe v0) (coe v1) (coe v2) (coe v6)
           _ -> coe v5)
-- Interface.MonadTC._.getConstrsForTerm
d_getConstrsForTerm_744 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_getConstrsForTerm_744 ~v0 v1 v2 v3 ~v4 v5
  = du_getConstrsForTerm_744 v1 v2 v3 v5
du_getConstrsForTerm_744 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_getConstrsForTerm_744 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.du__'61''60''60'__40 (coe v0) (coe ())
      (coe ()) (coe du_getConstrsForType_736 (coe v0) (coe v1) (coe v2))
      (coe d_inferType_162 v2 v3)
-- Interface.MonadTC._.withPattern
d_withPattern_748 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> AgdaAny
d_withPattern_748 ~v0 v1 ~v2 ~v3 v4 v5 v6 v7
  = du_withPattern_748 v1 v4 v5 v6 v7
du_withPattern_748 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> AgdaAny
du_withPattern_748 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
      (coe ())
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 (coe v2)
         (coe v3))
      (coe
         du_extendContext''_642 (coe v1) (coe ())
         (coe
            MAlonzo.Code.Data.List.Base.du_map_22
            (coe (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))
            (coe v2))
         (coe v4))
-- Interface.MonadTC._.unifyWithGoal
d_unifyWithGoal_756 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_unifyWithGoal_756 ~v0 v1 v2 v3 v4 v5
  = du_unifyWithGoal_756 v1 v2 v3 v4 v5
du_unifyWithGoal_756 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_unifyWithGoal_756 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe
         MAlonzo.Code.Interface.MonadReader.du_reader_78 (coe ()) (coe v0)
         (coe v3) (coe ()) (coe (\ v5 -> d_goal_52 (coe v5))))
      (\ v5 ->
         let v6
               = coe
                   du_error1_664 (coe v1) (coe ())
                   (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                   (coe ("unifyWithGoal: Goal is not a term!" :: Data.Text.Text)) in
         case coe v5 of
           MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7
             -> coe d_unify_156 v2 v7 v4
           _ -> coe v6)
-- Interface.MonadTC._.runWithHole
d_runWithHole_764 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny -> AgdaAny
d_runWithHole_764 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 v7
  = du_runWithHole_764 v4 v5 v7
du_runWithHole_764 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny -> AgdaAny
du_runWithHole_764 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.MonadReader.d_local_72 v0 v1 erased
      (\ v3 ->
         coe
           C_TCEnv'46'constructor_949 (coe d_normalisation_40 (coe v3))
           (coe d_reconstruction_42 (coe v3))
           (coe d_noConstraints_44 (coe v3)) (coe d_reduction_46 (coe v3))
           (coe d_globalContext_48 (coe v3)) (coe d_localContext_50 (coe v3))
           (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v2))
           (coe d_debug_54 (coe v3)))
-- Interface.MonadTC._.runWithGoalTy
d_runWithGoalTy_770 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny -> AgdaAny
d_runWithGoalTy_770 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 v7
  = du_runWithGoalTy_770 v4 v5 v7
du_runWithGoalTy_770 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny -> AgdaAny
du_runWithGoalTy_770 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.MonadReader.d_local_72 v0 v1 erased
      (\ v3 ->
         coe
           C_TCEnv'46'constructor_949 (coe d_normalisation_40 (coe v3))
           (coe d_reconstruction_42 (coe v3))
           (coe d_noConstraints_44 (coe v3)) (coe d_reduction_46 (coe v3))
           (coe d_globalContext_48 (coe v3)) (coe d_localContext_50 (coe v3))
           (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 (coe v2))
           (coe d_debug_54 (coe v3)))
-- Interface.MonadTC._.goalHole
d_goalHole_776 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
d_goalHole_776 ~v0 v1 ~v2 v3 v4 = du_goalHole_776 v1 v3 v4
du_goalHole_776 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
du_goalHole_776 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe
         MAlonzo.Code.Interface.MonadReader.du_reader_78 (coe ()) (coe v0)
         (coe v2) (coe ()) (coe (\ v3 -> d_goal_52 (coe v3))))
      (\ v3 ->
         case coe v3 of
           MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v4
             -> coe MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased v4
           MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v4
             -> coe du_newMeta_308 v1 v4
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Interface.MonadTC._.withGoalHole
d_withGoalHole_784 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
d_withGoalHole_784 ~v0 v1 ~v2 v3 v4 v5
  = du_withGoalHole_784 v1 v3 v4 v5
du_withGoalHole_784 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_MonadTC_104 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
du_withGoalHole_784 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe du_goalHole_776 (coe v0) (coe v1) (coe v2))
      (\ v4 ->
         coe
           MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
           (coe ()) (coe du_runWithHole_764 v2 () v4 v3)
           (coe MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased v4))
-- Interface.MonadTC.MonadTC-TC
d_MonadTC'45'TC_790 :: T_MonadTC_104
d_MonadTC'45'TC_790
  = coe
      C_MonadTC'46'constructor_6643
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_inferType_336)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_checkType_338)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_reduce_342)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_quoteTC_354)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_unquoteTC_360)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_quoteωTC_364)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_freshName_380)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_declareDef_382)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_declarePostulate_384)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_defineFun_392)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_getType_394)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_getDefinition_396)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_blockOnMeta_402)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_commitTC_404)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_isMacro_406)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_debugPrint_422)
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_runSpeculative_448)
