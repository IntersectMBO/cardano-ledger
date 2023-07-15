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

module MAlonzo.Code.Interface.MonadTC.Instance where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadTC

-- Interface.MonadTC.Instance._.blockOnMeta
d_blockOnMeta_8 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_blockOnMeta_8 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_blockOnMeta_190 (coe v0)
-- Interface.MonadTC.Instance._.checkType
d_checkType_10 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_checkType_10 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_checkType_164 (coe v0)
-- Interface.MonadTC.Instance._.commitTC
d_commitTC_12 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny
d_commitTC_12 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_commitTC_192 (coe v0)
-- Interface.MonadTC.Instance._.debugPrint
d_debugPrint_14 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
d_debugPrint_14 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_debugPrint_196 (coe v0)
-- Interface.MonadTC.Instance._.declareAndDefineFun
d_declareAndDefineFun_16 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
d_declareAndDefineFun_16 ~v0 v1 ~v2 v3
  = du_declareAndDefineFun_16 v1 v3
du_declareAndDefineFun_16 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
du_declareAndDefineFun_16 v0 v1
  = coe
      MAlonzo.Code.Interface.MonadTC.du_declareAndDefineFun_300 (coe v0)
      (coe v1)
-- Interface.MonadTC.Instance._.declareAndDefineFuns
d_declareAndDefineFuns_18 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_declareAndDefineFuns_18 ~v0 v1 ~v2 v3
  = du_declareAndDefineFuns_18 v1 v3
du_declareAndDefineFuns_18 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
du_declareAndDefineFuns_18 v0 v1
  = coe
      MAlonzo.Code.Interface.MonadTC.du_declareAndDefineFuns_284 (coe v0)
      (coe v1)
-- Interface.MonadTC.Instance._.declareDef
d_declareDef_20 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_declareDef_20 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_declareDef_180 (coe v0)
-- Interface.MonadTC.Instance._.declarePostulate
d_declarePostulate_22 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_declarePostulate_22 v0
  = coe
      MAlonzo.Code.Interface.MonadTC.d_declarePostulate_182 (coe v0)
-- Interface.MonadTC.Instance._.defineFun
d_defineFun_24 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152] -> AgdaAny
d_defineFun_24 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_defineFun_184 (coe v0)
-- Interface.MonadTC.Instance._.freshName
d_freshName_26 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_freshName_26 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_freshName_178 (coe v0)
-- Interface.MonadTC.Instance._.getDefinition
d_getDefinition_28 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_getDefinition_28 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_getDefinition_188 (coe v0)
-- Interface.MonadTC.Instance._.getType
d_getType_30 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_getType_30 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_getType_186 (coe v0)
-- Interface.MonadTC.Instance._.inferType
d_inferType_32 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_inferType_32 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_inferType_162 (coe v0)
-- Interface.MonadTC.Instance._.isCon
d_isCon_34 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_isCon_34 ~v0 v1 ~v2 v3 = du_isCon_34 v1 v3
du_isCon_34 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_isCon_34 v0 v1
  = coe MAlonzo.Code.Interface.MonadTC.du_isCon_252 (coe v0) (coe v1)
-- Interface.MonadTC.Instance._.isDef
d_isDef_36 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_isDef_36 ~v0 v1 ~v2 v3 = du_isDef_36 v1 v3
du_isDef_36 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_isDef_36 v0 v1
  = coe MAlonzo.Code.Interface.MonadTC.du_isDef_246 (coe v0) (coe v1)
-- Interface.MonadTC.Instance._.isMacro
d_isMacro_38 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_isMacro_38 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_isMacro_194 (coe v0)
-- Interface.MonadTC.Instance._.isSuccessful
d_isSuccessful_40 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_isSuccessful_40 ~v0 v1 v2 v3 = du_isSuccessful_40 v1 v2 v3
du_isSuccessful_40 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
du_isSuccessful_40 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Interface.MonadTC.du_isSuccessful_240 (coe v0)
      (coe v1) (coe v2) v3 v5
-- Interface.MonadTC.Instance._.mkRecord
d_mkRecord_42 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_mkRecord_42 ~v0 v1 v2 v3 = du_mkRecord_42 v1 v2 v3
du_mkRecord_42 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
du_mkRecord_42 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.MonadTC.du_mkRecord_274 (coe v0) (coe v1)
      (coe v2)
-- Interface.MonadTC.Instance._.nameConstr
d_nameConstr_44 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_nameConstr_44 ~v0 v1 v2 v3 = du_nameConstr_44 v1 v2 v3
du_nameConstr_44 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
du_nameConstr_44 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.MonadTC.du_nameConstr_258 (coe v0) (coe v1)
      (coe v2)
-- Interface.MonadTC.Instance._.newMeta
d_newMeta_46 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_newMeta_46 ~v0 ~v1 ~v2 v3 = du_newMeta_46 v3
du_newMeta_46 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_newMeta_46 v0
  = coe MAlonzo.Code.Interface.MonadTC.du_newMeta_308 (coe v0)
-- Interface.MonadTC.Instance._.normalise
d_normalise_48 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_normalise_48 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_normalise_166 (coe v0)
-- Interface.MonadTC.Instance._.quoteTC
d_quoteTC_50 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_quoteTC_50 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_quoteTC_170 (coe v0)
-- Interface.MonadTC.Instance._.quoteωTC
d_quoteωTC_52 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  () -> AgdaAny -> AgdaAny
d_quoteωTC_52 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_quoteωTC_176 (coe v0)
-- Interface.MonadTC.Instance._.reduce
d_reduce_54 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_reduce_54 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_reduce_168 (coe v0)
-- Interface.MonadTC.Instance._.runAndReset
d_runAndReset_56 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_runAndReset_56 ~v0 v1 ~v2 v3 = du_runAndReset_56 v1 v3
du_runAndReset_56 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
du_runAndReset_56 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.MonadTC.du_runAndReset_234 (coe v0) (coe v1)
      v2 v4
-- Interface.MonadTC.Instance._.runSpeculative
d_runSpeculative_58 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_runSpeculative_58 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_runSpeculative_198 (coe v0)
-- Interface.MonadTC.Instance._.termFromName
d_termFromName_60 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_termFromName_60 ~v0 v1 v2 v3 = du_termFromName_60 v1 v2 v3
du_termFromName_60 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_termFromName_60 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.MonadTC.du_termFromName_270 (coe v0)
      (coe v1) (coe v2)
-- Interface.MonadTC.Instance._.typeError
d_typeError_62 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_ErrorPart_298] -> AgdaAny
d_typeError_62 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_typeError_160 (coe v0)
-- Interface.MonadTC.Instance._.unify
d_unify_64 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_unify_64 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_unify_156 (coe v0)
-- Interface.MonadTC.Instance._.unquoteTC
d_unquoteTC_66 ::
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_unquoteTC_66 v0
  = coe MAlonzo.Code.Interface.MonadTC.d_unquoteTC_172 (coe v0)
