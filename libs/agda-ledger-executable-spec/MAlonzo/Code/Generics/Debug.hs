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

module MAlonzo.Code.Generics.Debug where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Functor.Instances
import qualified MAlonzo.Code.Class.Monad.Core
import qualified MAlonzo.Code.Class.Monad.Instances
import qualified MAlonzo.Code.Class.Monad.Utils
import qualified MAlonzo.Code.Class.Show.Core
import qualified MAlonzo.Code.Class.Show.Instances
import qualified MAlonzo.Code.Class.Traversable.Core
import qualified MAlonzo.Code.Class.Traversable.Instances
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Nat.Show
import qualified MAlonzo.Code.Data.String.Base
import qualified MAlonzo.Code.Reflection.AST.Show

-- Generics.Debug.error
d_error_8 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_error_8 v0 ~v1 v2 = du_error_8 v0 v2
du_error_8 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
du_error_8 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334 v0 erased
      (coe
         MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300 (coe v1)))
-- Generics.Debug._IMPOSSIBLE_
d__IMPOSSIBLE__12 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny
d__IMPOSSIBLE__12 v0 ~v1 = du__IMPOSSIBLE__12 v0
du__IMPOSSIBLE__12 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> AgdaAny
du__IMPOSSIBLE__12 v0
  = coe du_error_8 (coe v0) (coe ("IMPOSSIBLE" :: Data.Text.Text))
-- Generics.Debug.enumerate
d_enumerate_16 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_enumerate_16 ~v0 ~v1 v2 = du_enumerate_16 v2
du_enumerate_16 ::
  [AgdaAny] -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
du_enumerate_16 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_zip_212
      (MAlonzo.Code.Data.List.Base.d_allFin_436
         (coe MAlonzo.Code.Data.List.Base.du_length_304 v0))
      v0
-- Generics.Debug.Debug.print
d_print_24 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_print_24 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_debugPrint_422
      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0))
      (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v0))
      (coe
         MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300 (coe v1)))
-- Generics.Debug.Debug.printLn
d_printLn_26 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_printLn_26 v0 v1
  = coe
      d_print_24 (coe v0)
      (coe
         MAlonzo.Code.Data.String.Base.d__'43''43'__20 v1
         ("\n" :: Data.Text.Text))
-- Generics.Debug.Debug.printLns
d_printLns_32 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] -> AgdaAny
d_printLns_32 v0 v1
  = coe
      d_print_24 (coe v0)
      (coe MAlonzo.Code.Data.String.Base.d_unlines_36 v1)
-- Generics.Debug.Debug.printS
d_printS_36 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Class.Show.Core.T_Show_10 -> AgdaAny -> AgdaAny
d_printS_36 v0 ~v1 ~v2 v3 v4 = du_printS_36 v0 v3 v4
du_printS_36 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 -> AgdaAny -> AgdaAny
du_printS_36 v0 v1 v2
  = coe
      d_print_24 (coe v0)
      (coe MAlonzo.Code.Class.Show.Core.d_show_18 v1 v2)
-- Generics.Debug.Debug.errorP
d_errorP_38 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_errorP_38 v0 v1 ~v2 v3 = du_errorP_38 v0 v1 v3
du_errorP_38 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
du_errorP_38 v0 v1 v2
  = coe
      MAlonzo.Code.Class.Monad.Core.du__'62''62'__38
      (coe MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34) (coe ())
      (coe v1) (coe d_printLn_26 (coe v0) (coe v2))
      (coe du_error_8 (coe v1) (coe v2))
-- Generics.Debug.Debug.printTerm
d_printTerm_42 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_printTerm_42 v0 v1 v2
  = coe
      MAlonzo.Code.Class.Monad.Core.d__'62''62''61'__36
      MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34 () erased ()
      erased
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_inferType_336 v2)
      (\ v3 ->
         d_printLns_32
           (coe v0)
           (coe
              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
              (coe
                 MAlonzo.Code.Data.String.Base.d__'43''43'__20 v1
                 (": {" :: Data.Text.Text))
              (coe
                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                 (coe MAlonzo.Code.Reflection.AST.Show.d_showTerm_38 (coe v3))
                 (coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe (" \8715 " :: Data.Text.Text))
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                       (coe MAlonzo.Code.Reflection.AST.Show.d_showTerm_38 (coe v2))
                       (coe
                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                          (coe ("}\n" :: Data.Text.Text))
                          (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))))
-- Generics.Debug.Debug.printContext
d_printContext_50 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_printContext_50 v0 v1
  = coe
      MAlonzo.Code.Class.Monad.Core.du__'62''62'__38
      (coe MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34) (coe ())
      (coe ())
      (coe d_print_24 (coe v0) (coe ("\t----CTX----" :: Data.Text.Text)))
      (coe
         MAlonzo.Code.Class.Monad.Utils.du_void_54
         MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34 ()
         (coe
            MAlonzo.Code.Class.Traversable.Core.du_traverseM_62
            (coe MAlonzo.Code.Class.Functor.Instances.d_Functor'45'List_22)
            (coe
               MAlonzo.Code.Class.Traversable.Instances.d_TraversableM'45'List_32)
            (coe ()) (coe ())
            (coe MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34)
            (coe du_go_58 (coe v0)) (coe du_enumerate_16 (coe v1))))
-- Generics.Debug.Debug._.go
d_go_58 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_go_58 v0 ~v1 v2 = du_go_58 v0 v2
du_go_58 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_go_58 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe
             d_print_24 (coe v0)
             (coe
                MAlonzo.Code.Data.String.Base.d__'43''43'__20
                ("\t" :: Data.Text.Text)
                (coe
                   MAlonzo.Code.Data.String.Base.d__'43''43'__20
                   (coe
                      MAlonzo.Code.Data.String.Base.d__'43''43'__20
                      ("# " :: Data.Text.Text)
                      (MAlonzo.Code.Data.Nat.Show.d_show_56
                         (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v2))))
                   (coe
                      MAlonzo.Code.Data.String.Base.d__'43''43'__20
                      (" : " :: Data.Text.Text)
                      (coe
                         MAlonzo.Code.Class.Show.Core.d_show_18
                         (coe
                            MAlonzo.Code.Class.Show.Instances.du_Show'45'Arg_66
                            (coe
                               MAlonzo.Code.Class.Show.Core.C_mkShow_20
                               (coe MAlonzo.Code.Reflection.AST.Show.d_showTerm_38)))
                         v3))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Generics.Debug.Debug.printCurrentContext
d_printCurrentContext_64 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_printCurrentContext_64 v0
  = coe
      MAlonzo.Code.Class.Monad.Core.du__'61''60''60'__46
      (coe MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34) (coe ())
      (coe ())
      (coe
         (\ v1 ->
            d_printContext_50
              (coe v0)
              (coe
                 MAlonzo.Code.Data.List.Base.du_map_22
                 (coe (\ v2 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v2)))
                 (coe v1))))
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_getContext_366)
-- Generics.Debug.Debug.genSimpleDef
d_genSimpleDef_68 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_genSimpleDef_68 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Class.Monad.Core.du__'62''62'__38
      (coe MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34) (coe ())
      (coe ())
      (coe d_print_24 (coe v0) (coe ("Generaring..." :: Data.Text.Text)))
      (coe
         MAlonzo.Code.Class.Monad.Core.du__'62''62'__38
         (coe MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34) (coe ())
         (coe ())
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.d_declareDef_382
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
               (coe v1))
            v2)
         (coe
            MAlonzo.Code.Class.Monad.Core.du__'62''62'__38
            (coe MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34) (coe ())
            (coe ())
            (coe
               d_print_24 (coe v0)
               (coe
                  MAlonzo.Code.Data.String.Base.d__'43''43'__20
                  ("```\n" :: Data.Text.Text)
                  (coe
                     MAlonzo.Code.Data.String.Base.d__'43''43'__20
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v1)
                     (coe
                        MAlonzo.Code.Data.String.Base.d__'43''43'__20
                        (" : " :: Data.Text.Text)
                        (coe
                           MAlonzo.Code.Data.String.Base.d__'43''43'__20
                           (" " :: Data.Text.Text)
                           (MAlonzo.Code.Reflection.AST.Show.d_showTerm_38 (coe v2)))))))
            (coe
               MAlonzo.Code.Class.Monad.Core.du__'62''62'__38
               (coe MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34) (coe ())
               (coe ())
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.d_defineFun_392 v1
                  (coe
                     MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                     (coe
                        MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264
                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v3))))
               (coe
                  d_print_24 (coe v0)
                  (coe
                     MAlonzo.Code.Data.String.Base.d__'43''43'__20
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12 v1)
                     (coe
                        MAlonzo.Code.Data.String.Base.d__'43''43'__20
                        (" = " :: Data.Text.Text)
                        (coe
                           MAlonzo.Code.Data.String.Base.d__'43''43'__20
                           (" " :: Data.Text.Text)
                           (coe
                              MAlonzo.Code.Data.String.Base.d__'43''43'__20
                              (MAlonzo.Code.Reflection.AST.Show.d_showTerm_38 (coe v3))
                              ("\n```" :: Data.Text.Text)))))))))
-- Generics.Debug.DebugI._.errorP
d_errorP_82 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_errorP_82 v0 v1 v2 v3
  = coe
      du_errorP_38
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
         (coe (0 :: Integer)))
      v1 v3
-- Generics.Debug.DebugI._.genSimpleDef
d_genSimpleDef_84 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_genSimpleDef_84 v0
  = coe
      d_genSimpleDef_68
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
         (coe (0 :: Integer)))
-- Generics.Debug.DebugI._.print
d_print_86 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_print_86 v0
  = coe
      d_print_24
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
         (coe (0 :: Integer)))
-- Generics.Debug.DebugI._.printContext
d_printContext_88 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_printContext_88 v0
  = coe
      d_printContext_50
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
         (coe (0 :: Integer)))
-- Generics.Debug.DebugI._.printCurrentContext
d_printCurrentContext_90 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_printCurrentContext_90 v0
  = coe
      d_printCurrentContext_64
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
         (coe (0 :: Integer)))
-- Generics.Debug.DebugI._.printLn
d_printLn_92 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_printLn_92 v0
  = coe
      d_printLn_26
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
         (coe (0 :: Integer)))
-- Generics.Debug.DebugI._.printLns
d_printLns_94 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] -> AgdaAny
d_printLns_94 v0
  = coe
      d_printLns_32
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
         (coe (0 :: Integer)))
-- Generics.Debug.DebugI._.printS
d_printS_96 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Class.Show.Core.T_Show_10 -> AgdaAny -> AgdaAny
d_printS_96 v0 v1 v2 v3 v4
  = coe
      du_printS_36
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
         (coe (0 :: Integer)))
      v3 v4
-- Generics.Debug.DebugI._.printTerm
d_printTerm_98 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_printTerm_98 v0
  = coe
      d_printTerm_42
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
         (coe (0 :: Integer)))
-- Generics.Debug.trace
d_trace_104 ::
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_trace_104 ~v0 v1 v2 v3 v4 = du_trace_104 v1 v2 v3 v4
du_trace_104 ::
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_trace_104 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Class.Monad.Core.du__'62''62'__38
      (coe MAlonzo.Code.Class.Monad.Instances.d_Monad'45'TC_34) (coe ())
      (coe ())
      (coe
         d_print_24
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
            (coe ("trace" :: Data.Text.Text)) (coe (100 :: Integer)))
         (coe
            MAlonzo.Code.Data.String.Base.d__'43''43'__20
            ("trace: " :: Data.Text.Text)
            (coe MAlonzo.Code.Class.Show.Core.d_show_18 v0 v1)))
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328 v3 v2)
-- Generics.Debug._._.errorP
d_errorP_118 ::
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_errorP_118 ~v0 ~v1 ~v2 ~v3 ~v4 = du_errorP_118
du_errorP_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
du_errorP_118 v0 v1 v2
  = coe
      du_errorP_38
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("trace" :: Data.Text.Text)) (coe (100 :: Integer)))
      v0 v2
-- Generics.Debug._._.genSimpleDef
d_genSimpleDef_120 ::
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_genSimpleDef_120 ~v0 ~v1 ~v2 ~v3 ~v4 = du_genSimpleDef_120
du_genSimpleDef_120 ::
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_genSimpleDef_120
  = coe
      d_genSimpleDef_68
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("trace" :: Data.Text.Text)) (coe (100 :: Integer)))
-- Generics.Debug._._.print
d_print_122 ::
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_print_122 ~v0 ~v1 ~v2 ~v3 ~v4 = du_print_122
du_print_122 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
du_print_122
  = coe
      d_print_24
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("trace" :: Data.Text.Text)) (coe (100 :: Integer)))
-- Generics.Debug._._.printContext
d_printContext_124 ::
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_printContext_124 ~v0 ~v1 ~v2 ~v3 ~v4 = du_printContext_124
du_printContext_124 ::
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
du_printContext_124
  = coe
      d_printContext_50
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("trace" :: Data.Text.Text)) (coe (100 :: Integer)))
-- Generics.Debug._._.printCurrentContext
d_printCurrentContext_126 ::
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_printCurrentContext_126 ~v0 ~v1 ~v2 ~v3 ~v4
  = du_printCurrentContext_126
du_printCurrentContext_126 :: AgdaAny
du_printCurrentContext_126
  = coe
      d_printCurrentContext_64
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("trace" :: Data.Text.Text)) (coe (100 :: Integer)))
-- Generics.Debug._._.printLn
d_printLn_128 ::
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
d_printLn_128 ~v0 ~v1 ~v2 ~v3 ~v4 = du_printLn_128
du_printLn_128 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 -> AgdaAny
du_printLn_128
  = coe
      d_printLn_26
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("trace" :: Data.Text.Text)) (coe (100 :: Integer)))
-- Generics.Debug._._.printLns
d_printLns_130 ::
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] -> AgdaAny
d_printLns_130 ~v0 ~v1 ~v2 ~v3 ~v4 = du_printLns_130
du_printLns_130 ::
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] -> AgdaAny
du_printLns_130
  = coe
      d_printLns_32
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("trace" :: Data.Text.Text)) (coe (100 :: Integer)))
-- Generics.Debug._._.printS
d_printS_132 ::
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Class.Show.Core.T_Show_10 -> AgdaAny -> AgdaAny
d_printS_132 ~v0 ~v1 ~v2 ~v3 ~v4 = du_printS_132
du_printS_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Class.Show.Core.T_Show_10 -> AgdaAny -> AgdaAny
du_printS_132 v0 v1 v2 v3
  = coe
      du_printS_36
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("trace" :: Data.Text.Text)) (coe (100 :: Integer)))
      v2 v3
-- Generics.Debug._._.printTerm
d_printTerm_134 ::
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_printTerm_134 ~v0 ~v1 ~v2 ~v3 ~v4 = du_printTerm_134
du_printTerm_134 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_printTerm_134
  = coe
      d_printTerm_42
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe ("trace" :: Data.Text.Text)) (coe (100 :: Integer)))
