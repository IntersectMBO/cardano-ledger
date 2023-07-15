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

module MAlonzo.Code.Agda.Builtin.Reflection where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Char
import qualified MAlonzo.Code.Agda.Builtin.Float
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive

-- Agda.Builtin.Reflection.Name
d_Name_6
  = error
      "MAlonzo Runtime Error: postulate evaluated: Agda.Builtin.Reflection.Name"
-- Agda.Builtin.Reflection.primQNameEquality
d_primQNameEquality_8
  = (\ x y -> ((==) :: MAlonzo.RTE.QName -> MAlonzo.RTE.QName -> Bool) ( x) ( y))
-- Agda.Builtin.Reflection.primQNameLess
d_primQNameLess_10
  = (\ x y -> ((<) :: MAlonzo.RTE.QName -> MAlonzo.RTE.QName -> Bool) ( x) ( y))
-- Agda.Builtin.Reflection.primShowQName
d_primShowQName_12 = Data.Text.pack . MAlonzo.RTE.qnameString
-- Agda.Builtin.Reflection.Associativity
d_Associativity_14 = ()
type T_Associativity_14 = MAlonzo.RTE.Assoc
pattern C_left'45'assoc_16 = MAlonzo.RTE.LeftAssoc
pattern C_right'45'assoc_18 = MAlonzo.RTE.RightAssoc
pattern C_non'45'assoc_20 = MAlonzo.RTE.NonAssoc
check_left'45'assoc_16 :: T_Associativity_14
check_left'45'assoc_16 = MAlonzo.RTE.LeftAssoc
check_right'45'assoc_18 :: T_Associativity_14
check_right'45'assoc_18 = MAlonzo.RTE.RightAssoc
check_non'45'assoc_20 :: T_Associativity_14
check_non'45'assoc_20 = MAlonzo.RTE.NonAssoc
cover_Associativity_14 :: MAlonzo.RTE.Assoc -> ()
cover_Associativity_14 x
  = case x of
      MAlonzo.RTE.LeftAssoc -> ()
      MAlonzo.RTE.RightAssoc -> ()
      MAlonzo.RTE.NonAssoc -> ()
-- Agda.Builtin.Reflection.Precedence
d_Precedence_22 = ()
type T_Precedence_22 = MAlonzo.RTE.Precedence
pattern C_related_24 a0 = MAlonzo.RTE.Related a0
pattern C_unrelated_26 = MAlonzo.RTE.Unrelated
check_related_24 ::
  MAlonzo.Code.Agda.Builtin.Float.T_Float_6 -> T_Precedence_22
check_related_24 = MAlonzo.RTE.Related
check_unrelated_26 :: T_Precedence_22
check_unrelated_26 = MAlonzo.RTE.Unrelated
cover_Precedence_22 :: MAlonzo.RTE.Precedence -> ()
cover_Precedence_22 x
  = case x of
      MAlonzo.RTE.Related _ -> ()
      MAlonzo.RTE.Unrelated -> ()
-- Agda.Builtin.Reflection.Fixity
d_Fixity_28 = ()
type T_Fixity_28 = MAlonzo.RTE.Fixity
pattern C_fixity_30 a0 a1 = MAlonzo.RTE.Fixity a0 a1
check_fixity_30 ::
  T_Associativity_14 -> T_Precedence_22 -> T_Fixity_28
check_fixity_30 = MAlonzo.RTE.Fixity
cover_Fixity_28 :: MAlonzo.RTE.Fixity -> ()
cover_Fixity_28 x
  = case x of
      MAlonzo.RTE.Fixity _ _ -> ()
-- Agda.Builtin.Reflection.primQNameFixity
d_primQNameFixity_32 = MAlonzo.RTE.qnameFixity
-- Agda.Builtin.Reflection.primQNameToWord64s
d_primQNameToWord64s_36
  = \ qn -> (MAlonzo.RTE.nameId qn, MAlonzo.RTE.moduleId qn)
-- Agda.Builtin.Reflection.Meta
d_Meta_38
  = error
      "MAlonzo Runtime Error: postulate evaluated: Agda.Builtin.Reflection.Meta"
-- Agda.Builtin.Reflection.primMetaEquality
d_primMetaEquality_40
  = (\ x y -> ((==) :: (Integer, Integer) -> (Integer, Integer) -> Bool) ( x) ( y))
-- Agda.Builtin.Reflection.primMetaLess
d_primMetaLess_42
  = (\ x y -> ((<) :: (Integer, Integer) -> (Integer, Integer) -> Bool) ( x) ( y))
-- Agda.Builtin.Reflection.primShowMeta
d_primShowMeta_44
  = \ (m, h) -> Data.Text.pack ("_" ++ show (m :: Integer) ++ "@" ++ show (h :: Integer))
-- Agda.Builtin.Reflection.primMetaToNat
d_primMetaToNat_46
  = \ (m, h) -> (h :: Integer) * 2^64 + (m :: Integer)
-- Agda.Builtin.Reflection.Visibility
d_Visibility_48 = ()
data T_Visibility_48
  = C_visible_50 | C_hidden_52 | C_instance'8242'_54
-- Agda.Builtin.Reflection.Relevance
d_Relevance_56 = ()
data T_Relevance_56 = C_relevant_58 | C_irrelevant_60
-- Agda.Builtin.Reflection.Quantity
d_Quantity_62 = ()
data T_Quantity_62 = C_quantity'45'0_64 | C_quantity'45'ω_66
-- Agda.Builtin.Reflection.Modality
d_Modality_68 = ()
data T_Modality_68 = C_modality_74 T_Relevance_56 T_Quantity_62
-- Agda.Builtin.Reflection.ArgInfo
d_ArgInfo_76 = ()
data T_ArgInfo_76 = C_arg'45'info_82 T_Visibility_48 T_Modality_68
-- Agda.Builtin.Reflection.Arg
d_Arg_88 a0 a1 = ()
data T_Arg_88 = C_arg_98 T_ArgInfo_76 AgdaAny
-- Agda.Builtin.Reflection.Abs
d_Abs_104 a0 a1 = ()
data T_Abs_104
  = C_abs_114 MAlonzo.Code.Agda.Builtin.String.T_String_6 AgdaAny
-- Agda.Builtin.Reflection.Literal
d_Literal_116 = ()
data T_Literal_116
  = C_nat_120 Integer | C_word64_124 MAlonzo.RTE.Word64 |
    C_float_128 MAlonzo.Code.Agda.Builtin.Float.T_Float_6 |
    C_char_132 MAlonzo.Code.Agda.Builtin.Char.T_Char_6 |
    C_string_136 MAlonzo.Code.Agda.Builtin.String.T_String_6 |
    C_name_140 AgdaAny | C_meta_144 AgdaAny
-- Agda.Builtin.Reflection.Term
d_Term_146 = ()
data T_Term_146
  = C_var_164 Integer [T_Arg_88] | C_con_170 AgdaAny [T_Arg_88] |
    C_def_176 AgdaAny [T_Arg_88] |
    C_lam_182 T_Visibility_48 T_Abs_104 |
    C_pat'45'lam_188 [T_Clause_152] [T_Arg_88] |
    C_pi_194 T_Arg_88 T_Abs_104 | C_agda'45'sort_198 T_Sort_148 |
    C_lit_202 T_Literal_116 | C_meta_206 AgdaAny [T_Arg_88] |
    C_unknown_208
-- Agda.Builtin.Reflection.Sort
d_Sort_148 = ()
data T_Sort_148
  = C_set_212 T_Term_146 | C_lit_216 Integer |
    C_prop_220 T_Term_146 | C_propLit_224 Integer | C_inf_228 Integer |
    C_unknown_230
-- Agda.Builtin.Reflection.Pattern
d_Pattern_150 = ()
data T_Pattern_150
  = C_con_236 AgdaAny [T_Arg_88] | C_dot_240 T_Term_146 |
    C_var_244 Integer | C_lit_248 T_Literal_116 | C_proj_252 AgdaAny |
    C_absurd_256 Integer
-- Agda.Builtin.Reflection.Clause
d_Clause_152 = ()
data T_Clause_152
  = C_clause_264 [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] [T_Arg_88]
                 T_Term_146 |
    C_absurd'45'clause_270 [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
                           [T_Arg_88]
-- Agda.Builtin.Reflection.Type
d_Type_154 :: ()
d_Type_154 = erased
-- Agda.Builtin.Reflection.Telescope
d_Telescope_156 :: ()
d_Telescope_156 = erased
-- Agda.Builtin.Reflection.Definition
d_Definition_272 = ()
data T_Definition_272
  = C_function_276 [T_Clause_152] |
    C_data'45'type_282 Integer [AgdaAny] |
    C_record'45'type_288 AgdaAny [T_Arg_88] |
    C_data'45'cons_292 AgdaAny | C_axiom_294 | C_prim'45'fun_296
-- Agda.Builtin.Reflection.ErrorPart
d_ErrorPart_298 = ()
data T_ErrorPart_298
  = C_strErr_300 MAlonzo.Code.Agda.Builtin.String.T_String_6 |
    C_termErr_302 T_Term_146 | C_pattErr_304 T_Pattern_150 |
    C_nameErr_306 AgdaAny
-- Agda.Builtin.Reflection.TC
d_TC_310
  = error
      "MAlonzo Runtime Error: postulate evaluated: Agda.Builtin.Reflection.TC"
-- Agda.Builtin.Reflection.returnTC
d_returnTC_316 = erased
-- Agda.Builtin.Reflection.bindTC
d_bindTC_326 = erased
-- Agda.Builtin.Reflection.unify
d_unify_328 = erased
-- Agda.Builtin.Reflection.typeError
d_typeError_334 = erased
-- Agda.Builtin.Reflection.inferType
d_inferType_336 = erased
-- Agda.Builtin.Reflection.checkType
d_checkType_338 = erased
-- Agda.Builtin.Reflection.normalise
d_normalise_340 = erased
-- Agda.Builtin.Reflection.reduce
d_reduce_342 = erased
-- Agda.Builtin.Reflection.catchTC
d_catchTC_348 = erased
-- Agda.Builtin.Reflection.quoteTC
d_quoteTC_354 = erased
-- Agda.Builtin.Reflection.unquoteTC
d_unquoteTC_360 = erased
-- Agda.Builtin.Reflection.quoteωTC
d_quoteωTC_364 = erased
-- Agda.Builtin.Reflection.getContext
d_getContext_366 = erased
-- Agda.Builtin.Reflection.extendContext
d_extendContext_372 = erased
-- Agda.Builtin.Reflection.inContext
d_inContext_378 = erased
-- Agda.Builtin.Reflection.freshName
d_freshName_380 = erased
-- Agda.Builtin.Reflection.declareDef
d_declareDef_382 = erased
-- Agda.Builtin.Reflection.declarePostulate
d_declarePostulate_384 = erased
-- Agda.Builtin.Reflection.declareData
d_declareData_386 = erased
-- Agda.Builtin.Reflection.defineData
d_defineData_390 = erased
-- Agda.Builtin.Reflection.defineFun
d_defineFun_392 = erased
-- Agda.Builtin.Reflection.getType
d_getType_394 = erased
-- Agda.Builtin.Reflection.getDefinition
d_getDefinition_396 = erased
-- Agda.Builtin.Reflection.blockOnMeta
d_blockOnMeta_402 = erased
-- Agda.Builtin.Reflection.commitTC
d_commitTC_404 = erased
-- Agda.Builtin.Reflection.isMacro
d_isMacro_406 = erased
-- Agda.Builtin.Reflection.withNormalisation
d_withNormalisation_412 = erased
-- Agda.Builtin.Reflection.withReconstructed
d_withReconstructed_418 = erased
-- Agda.Builtin.Reflection.formatErrorParts
d_formatErrorParts_420 = erased
-- Agda.Builtin.Reflection.debugPrint
d_debugPrint_422 = erased
-- Agda.Builtin.Reflection.onlyReduceDefs
d_onlyReduceDefs_428 = erased
-- Agda.Builtin.Reflection.dontReduceDefs
d_dontReduceDefs_434 = erased
-- Agda.Builtin.Reflection.noConstraints
d_noConstraints_440 = erased
-- Agda.Builtin.Reflection.runSpeculative
d_runSpeculative_448 = erased
-- Agda.Builtin.Reflection.getInstances
d_getInstances_450 = erased
