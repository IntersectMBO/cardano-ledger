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

module MAlonzo.Code.Reflection.TCM.Format where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Char
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Text.Format
import qualified MAlonzo.Code.Text.Format.Generic
import qualified MAlonzo.Code.Text.Printf
import qualified MAlonzo.Code.Text.Printf.Generic

-- Reflection.TCM.Format.Specification.ErrorChunk
d_ErrorChunk_12 = ()
data T_ErrorChunk_12
  = C_'96'Term_14 | C_'96'Name_16 | C_'96'Parts_18
-- Reflection.TCM.Format.Specification.errorSpec
d_errorSpec_20 :: MAlonzo.Code.Text.Format.Generic.T_FormatSpec_6
d_errorSpec_20
  = coe
      MAlonzo.Code.Text.Format.Generic.C_FormatSpec'46'constructor_25
      (\ v0 ->
         let v1 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
         case coe v0 of
           'e'
             -> coe
                  MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe C_'96'Parts_18)
           'q'
             -> coe
                  MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe C_'96'Name_16)
           't'
             -> coe
                  MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe C_'96'Term_14)
           _ -> coe v1)
-- Reflection.TCM.Format.Specification.formatSpec
d_formatSpec_22 :: MAlonzo.Code.Text.Format.Generic.T_FormatSpec_6
d_formatSpec_22
  = coe
      MAlonzo.Code.Text.Format.Generic.d_unionSpec_24
      (coe d_errorSpec_20) (coe MAlonzo.Code.Text.Format.d_formatSpec_22)
-- Reflection.TCM.Format.Specification.printfSpec
d_printfSpec_24 :: MAlonzo.Code.Text.Printf.Generic.T_PrintfSpec_18
d_printfSpec_24
  = coe
      MAlonzo.Code.Text.Printf.Generic.C_PrintfSpec'46'constructor_111
      (coe
         (\ v0 ->
            case coe v0 of
              MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v1
                -> case coe v1 of
                     C_'96'Term_14
                       -> coe
                            (\ v2 ->
                               coe
                                 MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                 (coe MAlonzo.Code.Agda.Builtin.Reflection.C_termErr_302 (coe v2)))
                     C_'96'Name_16
                       -> coe
                            (\ v2 ->
                               coe
                                 MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                 (coe MAlonzo.Code.Agda.Builtin.Reflection.C_nameErr_306 (coe v2)))
                     C_'96'Parts_18 -> coe (\ v2 -> v2)
                     _ -> MAlonzo.RTE.mazUnreachableError
              MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v1
                -> coe
                     (\ v2 ->
                        coe
                          MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
                             (coe
                                MAlonzo.Code.Text.Printf.Generic.d_renderArg_50
                                MAlonzo.Code.Text.Printf.d_printfSpec_4 v1 v2)))
              _ -> MAlonzo.RTE.mazUnreachableError))
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300 (coe v0))))
-- Reflection.TCM.Format._.Chunk
d_Chunk_42 = ()
-- Reflection.TCM.Format._.Error
d_Error_44 = ()
-- Reflection.TCM.Format._.lexer
d_lexer_54 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_lexer_54
  = coe
      MAlonzo.Code.Text.Format.Generic.d_lexer_88 (coe d_formatSpec_22)
-- Reflection.TCM.Format._.Printf
d_Printf_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> () -> ()
d_Printf_76 = erased
-- Reflection.TCM.Format._.map
d_map_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map_78 v0 v1 v2 v3 v4 v5 v6
  = coe MAlonzo.Code.Text.Printf.Generic.du_map_118 v4 v5 v6
-- Reflection.TCM.Format._.printf
d_printf_86 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_printf_86
  = coe
      MAlonzo.Code.Text.Printf.Generic.du_printf_226
      (coe d_formatSpec_22) (coe d_printfSpec_24)
-- Reflection.TCM.Format.typeErrorFmt
d_typeErrorFmt_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_typeErrorFmt_90 v0 ~v1 v2 = du_typeErrorFmt_90 v0 v2
du_typeErrorFmt_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
du_typeErrorFmt_90 v0 v1
  = coe
      MAlonzo.Code.Text.Printf.Generic.du_map_118
      (coe
         MAlonzo.Code.Text.Format.Generic.d_lexer_88 (coe d_formatSpec_22)
         (coe v1))
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334 v0 erased
              (coe MAlonzo.Code.Data.List.Base.du_concat_270 v2)))
      (coe
         MAlonzo.Code.Text.Printf.Generic.du_printf_226
         (coe d_formatSpec_22) (coe d_printfSpec_24) (coe v1))
-- Reflection.TCM.Format.debugPrintFmt
d_debugPrintFmt_96 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  Integer -> MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_debugPrintFmt_96 v0 v1 v2
  = coe
      MAlonzo.Code.Text.Printf.Generic.du_map_118
      (coe
         MAlonzo.Code.Text.Format.Generic.d_lexer_88 (coe d_formatSpec_22)
         (coe v2))
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_debugPrint_422 v0 v1
              (coe MAlonzo.Code.Data.List.Base.du_concat_270 v3)))
      (coe
         MAlonzo.Code.Text.Printf.Generic.du_printf_226
         (coe d_formatSpec_22) (coe d_printfSpec_24) (coe v2))
-- Reflection.TCM.Format.errorPartFmt
d_errorPartFmt_106 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_errorPartFmt_106 v0
  = coe
      MAlonzo.Code.Text.Printf.Generic.du_map_118
      (coe
         MAlonzo.Code.Text.Format.Generic.d_lexer_88 (coe d_formatSpec_22)
         (coe v0))
      (coe MAlonzo.Code.Data.List.Base.du_concat_270)
      (coe
         MAlonzo.Code.Text.Printf.Generic.du_printf_226
         (coe d_formatSpec_22) (coe d_printfSpec_24) (coe v0))
