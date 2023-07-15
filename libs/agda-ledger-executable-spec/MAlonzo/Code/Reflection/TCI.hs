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

module MAlonzo.Code.Reflection.TCI where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Interface.MonadTC
import qualified MAlonzo.Code.Reflection.AST.Abstraction

-- Reflection.TCI.TC
d_TC_16 :: MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()
d_TC_16 = erased
-- Reflection.TCI.Monad-TC
d_Monad'45'TC_18 :: MAlonzo.Code.Interface.Monad.T_Monad_20
d_Monad'45'TC_18
  = coe
      MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
      (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12)
-- Reflection.TCI.MonadReader-TC
d_MonadReader'45'TC_20 ::
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50
d_MonadReader'45'TC_20
  = coe
      MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
      (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12)
-- Reflection.TCI.MonadError-TC
d_MonadError'45'TC_22 ::
  MAlonzo.Code.Interface.MonadError.T_MonadError_46
d_MonadError'45'TC_22
  = coe
      MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
      (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60)
-- Reflection.TCI.applyReductionOptions
d_applyReductionOptions_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_applyReductionOptions_24 v0 ~v1 v2 v3
  = du_applyReductionOptions_24 v0 v2 v3
du_applyReductionOptions_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_applyReductionOptions_24 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949 v3 v4 v5 v6 v7 v8 v9 v10
        -> case coe v6 of
             MAlonzo.Code.Interface.MonadTC.C_onlyReduce_16 v11
               -> coe
                    MAlonzo.Code.Agda.Builtin.Reflection.d_onlyReduceDefs_428 v0 erased
                    v11 (coe v1 v2)
             MAlonzo.Code.Interface.MonadTC.C_dontReduce_18 v11
               -> coe
                    MAlonzo.Code.Agda.Builtin.Reflection.d_dontReduceDefs_434 v0 erased
                    v11 (coe v1 v2)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.TCI.applyNormalisation
d_applyNormalisation_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_applyNormalisation_38 v0 ~v1 v2 v3
  = du_applyNormalisation_38 v0 v2 v3
du_applyNormalisation_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_applyNormalisation_38 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_withNormalisation_412 v0
             erased v3
             (coe du_applyReductionOptions_24 (coe v0) (coe v1) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.TCI.applyReconstruction
d_applyReconstruction_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_applyReconstruction_46 v0 ~v1 v2 v3
  = du_applyReconstruction_46 v0 v2 v3
du_applyReconstruction_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_applyReconstruction_46 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949 v3 v4 v5 v6 v7 v8 v9 v10
        -> if coe v4
             then coe
                    MAlonzo.Code.Agda.Builtin.Reflection.d_withReconstructed_418 v0
                    erased (coe v1 v2)
             else coe v1 v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.TCI.applyNoConstraints
d_applyNoConstraints_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_applyNoConstraints_56 v0 ~v1 v2 v3
  = du_applyNoConstraints_56 v0 v2 v3
du_applyNoConstraints_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_applyNoConstraints_56 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949 v3 v4 v5 v6 v7 v8 v9 v10
        -> if coe v5
             then coe
                    MAlonzo.Code.Agda.Builtin.Reflection.d_noConstraints_440 v0 erased
                    (coe v1 v2)
             else coe v1 v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.TCI.applyExtContext
d_applyExtContext_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> AgdaAny
d_applyExtContext_66 v0 ~v1 v2 v3 = du_applyExtContext_66 v0 v2 v3
du_applyExtContext_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> AgdaAny
du_applyExtContext_66 v0 v1 v2
  = case coe v1 of
      [] -> coe v2
      (:) v3 v4
        -> coe
             du_applyExtContext_66 (coe v0) (coe v4)
             (coe
                MAlonzo.Code.Agda.Builtin.Reflection.d_extendContext_372 v0 erased
                ("_" :: Data.Text.Text) v3 v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Reflection.TCI.liftTC
d_liftTC_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_liftTC_76 v0 ~v1 v2 v3 = du_liftTC_76 v0 v2 v3
du_liftTC_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_liftTC_76 v0 v1 v2
  = coe
      du_applyExtContext_66 (coe v0)
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe MAlonzo.Code.Reflection.AST.Abstraction.du_unAbs_36)
         (coe MAlonzo.Code.Interface.MonadTC.d_localContext_50 (coe v2)))
      (coe v1)
-- Reflection.TCI.liftTC1
d_liftTC1_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_liftTC1_82 ~v0 ~v1 v2 ~v3 v4 v5 = du_liftTC1_82 v2 v4 v5
du_liftTC1_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_liftTC1_82 v0 v1 v2 = coe du_liftTC_76 (coe v0) (coe v1 v2)
-- Reflection.TCI.liftTC2
d_liftTC2_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_liftTC2_88 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 v8
  = du_liftTC2_88 v4 v6 v7 v8
du_liftTC2_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_liftTC2_88 v0 v1 v2 v3
  = coe du_liftTC_76 (coe v0) (coe v1 v2 v3)
-- Reflection.TCI.liftTC3
d_liftTC3_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_liftTC3_96 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 v8 v9 v10 v11
  = du_liftTC3_96 v6 v8 v9 v10 v11
du_liftTC3_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_liftTC3_96 v0 v1 v2 v3 v4
  = coe du_liftTC_76 (coe v0) (coe v1 v2 v3 v4)
-- Reflection.TCI.MonadTCI.unify
d_unify_108 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_unify_108
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8322'__92
      (coe (\ v0 v1 -> coe du_applyNoConstraints_56 (coe ())))
      (coe
         du_liftTC2_88 (coe ())
         (coe MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328))
-- Reflection.TCI.MonadTCI.typeError
d_typeError_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_typeError_110 v0 ~v1 = du_typeError_110 v0
du_typeError_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_typeError_110 v0
  = coe
      du_liftTC1_82 (coe v0)
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334 v0 erased)
-- Reflection.TCI.MonadTCI.inferType
d_inferType_112 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_inferType_112 v0
  = coe
      du_applyReconstruction_46 (coe ())
      (coe
         du_applyNormalisation_38 (coe ())
         (coe
            du_liftTC1_82 (coe ())
            (coe MAlonzo.Code.Agda.Builtin.Reflection.d_inferType_336)
            (coe v0)))
-- Reflection.TCI.MonadTCI.checkType
d_checkType_114 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_checkType_114
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8322'__92
      (coe
         (\ v0 v1 v2 ->
            coe
              du_applyReconstruction_46 (coe ())
              (coe du_applyNormalisation_38 (coe ()) (coe v2))))
      (coe
         du_liftTC2_88 (coe ())
         (coe MAlonzo.Code.Agda.Builtin.Reflection.d_checkType_338))
-- Reflection.TCI.MonadTCI.normalise
d_normalise_116 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_normalise_116 v0
  = coe
      du_applyReductionOptions_24 (coe ())
      (coe
         du_applyReconstruction_46 (coe ())
         (coe
            du_liftTC1_82 (coe ())
            (coe MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340)
            (coe v0)))
-- Reflection.TCI.MonadTCI.reduce
d_reduce_118 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_reduce_118 v0
  = coe
      du_applyReductionOptions_24 (coe ())
      (coe
         du_applyReconstruction_46 (coe ())
         (coe
            du_liftTC1_82 (coe ())
            (coe MAlonzo.Code.Agda.Builtin.Reflection.d_reduce_342) (coe v0)))
-- Reflection.TCI.MonadTCI.quoteTC
d_quoteTC_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_quoteTC_120 v0 ~v1 v2 = du_quoteTC_120 v0 v2
du_quoteTC_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_quoteTC_120 v0 v1
  = coe
      du_applyNormalisation_38 (coe ())
      (coe
         du_liftTC1_82 (coe ())
         (coe MAlonzo.Code.Agda.Builtin.Reflection.d_quoteTC_354 v0 erased)
         (coe v1))
-- Reflection.TCI.MonadTCI.unquoteTC
d_unquoteTC_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_unquoteTC_122 v0 ~v1 = du_unquoteTC_122 v0
du_unquoteTC_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_unquoteTC_122 v0
  = coe
      du_liftTC1_82 (coe v0)
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_unquoteTC_360 v0 erased)
-- Reflection.TCI.MonadTCI.quoteωTC
d_quoteωTC_126 ::
  () ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_quoteωTC_126 ~v0 v1 = du_quoteωTC_126 v1
du_quoteωTC_126 ::
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_quoteωTC_126 v0
  = coe
      du_liftTC_76 (coe ())
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_quoteωTC_364 erased v0)
-- Reflection.TCI.MonadTCI.freshName
d_freshName_130 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_freshName_130
  = coe
      du_liftTC1_82 (coe ())
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_freshName_380)
-- Reflection.TCI.MonadTCI.declareDef
d_declareDef_132 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_declareDef_132
  = coe
      du_liftTC2_88 (coe ())
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_declareDef_382)
-- Reflection.TCI.MonadTCI.declarePostulate
d_declarePostulate_134 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_declarePostulate_134
  = coe
      du_liftTC2_88 (coe ())
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_declarePostulate_384)
-- Reflection.TCI.MonadTCI.defineFun
d_defineFun_136 ::
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_defineFun_136
  = coe
      du_liftTC2_88 (coe ())
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_defineFun_392)
-- Reflection.TCI.MonadTCI.getType
d_getType_138 ::
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_getType_138 v0
  = coe
      du_applyReconstruction_46 (coe ())
      (coe
         du_liftTC1_82 (coe ())
         (coe MAlonzo.Code.Agda.Builtin.Reflection.d_getType_394) (coe v0))
-- Reflection.TCI.MonadTCI.getDefinition
d_getDefinition_140 ::
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_getDefinition_140 v0
  = coe
      du_applyReconstruction_46 (coe ())
      (coe
         du_liftTC1_82 (coe ())
         (coe MAlonzo.Code.Agda.Builtin.Reflection.d_getDefinition_396)
         (coe v0))
-- Reflection.TCI.MonadTCI.blockOnMeta
d_blockOnMeta_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_blockOnMeta_142 v0 ~v1 = du_blockOnMeta_142 v0
du_blockOnMeta_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_blockOnMeta_142 v0
  = coe
      du_liftTC1_82 (coe v0)
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_blockOnMeta_402 v0 erased)
-- Reflection.TCI.MonadTCI.commitTC
d_commitTC_144 ::
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_commitTC_144
  = coe
      du_liftTC_76 (coe ())
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_commitTC_404)
-- Reflection.TCI.MonadTCI.isMacro
d_isMacro_146 ::
  AgdaAny -> MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_isMacro_146
  = coe
      du_liftTC1_82 (coe ())
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_isMacro_406)
-- Reflection.TCI.MonadTCI.debugPrint
d_debugPrint_148 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_debugPrint_148
  = coe
      du_liftTC3_96 (coe ())
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_debugPrint_422)
-- Reflection.TCI.MonadTCI.runSpeculative
d_runSpeculative_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_runSpeculative_150 v0 ~v1 v2 v3 = du_runSpeculative_150 v0 v2 v3
du_runSpeculative_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny) ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
du_runSpeculative_150 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_runSpeculative_448 v0 erased
      (coe v1 v2)
-- Reflection.TCI.MonadTC-TCI
d_MonadTC'45'TCI_154 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104
d_MonadTC'45'TCI_154
  = coe
      MAlonzo.Code.Interface.MonadTC.C_MonadTC'46'constructor_6643
      (coe d_unify_108) (\ v0 v1 -> coe du_typeError_110 v0)
      (coe d_inferType_112) (coe d_checkType_114) (coe d_normalise_116)
      (coe d_reduce_118) (\ v0 v1 v2 -> coe du_quoteTC_120 v0 v2)
      (\ v0 v1 -> coe du_unquoteTC_122 v0)
      (\ v0 v1 -> coe du_quoteωTC_126 v1) (coe d_freshName_130)
      (coe d_declareDef_132) (coe d_declarePostulate_134)
      (coe d_defineFun_136) (coe d_getType_138) (coe d_getDefinition_140)
      (\ v0 v1 -> coe du_blockOnMeta_142 v0) (coe d_commitTC_144)
      (coe d_isMacro_146) (coe d_debugPrint_148)
      (\ v0 v1 v2 v3 -> coe du_runSpeculative_150 v0 v2 v3)
