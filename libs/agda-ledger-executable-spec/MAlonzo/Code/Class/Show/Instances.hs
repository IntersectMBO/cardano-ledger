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

module MAlonzo.Code.Class.Show.Instances where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Float
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Show.Core
import qualified MAlonzo.Code.Data.Bool.Show
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Nat.Show
import qualified MAlonzo.Code.Data.String.Base
import qualified MAlonzo.Code.Reflection.AST.Show

-- Class.Show.Instances.mkShow-×
d_mkShow'45''215'_14 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10
d_mkShow'45''215'_14 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_mkShow'45''215'_14 v4 v5
du_mkShow'45''215'_14 ::
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10
du_mkShow'45''215'_14 v0 v1
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe
         (\ v2 ->
            case coe v2 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
                -> coe
                     MAlonzo.Code.Data.String.Base.d_parens_38
                     (coe
                        MAlonzo.Code.Data.String.Base.d__'43''43'__20
                        (coe MAlonzo.Code.Class.Show.Core.d_show_18 v0 v3)
                        (coe
                           MAlonzo.Code.Data.String.Base.d__'43''43'__20
                           (" , " :: Data.Text.Text)
                           (coe MAlonzo.Code.Class.Show.Core.d_show_18 v1 v4)))
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Class.Show.Instances.Show-List
d_Show'45'List_20 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'List_20 ~v0 ~v1 v2 = du_Show'45'List_20 v2
du_Show'45'List_20 ::
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10
du_Show'45'List_20 v0
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe
         (\ v1 ->
            MAlonzo.Code.Data.String.Base.d_braces_42
              (coe
                 MAlonzo.Code.Data.String.Base.d_intersperse_30
                 (", " :: Data.Text.Text)
                 (coe
                    MAlonzo.Code.Data.List.Base.du_map_22
                    (coe MAlonzo.Code.Class.Show.Core.d_show_18 (coe v0)) (coe v1)))))
-- Class.Show.Instances.Show-String
d_Show'45'String_22 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'String_22
  = coe MAlonzo.Code.Class.Show.Core.C_mkShow_20 (coe (\ v0 -> v0))
-- Class.Show.Instances.Show-⊤
d_Show'45''8868'_24 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45''8868'_24
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe (\ v0 -> seq (coe v0) (coe ("tt" :: Data.Text.Text))))
-- Class.Show.Instances.Show-Char
d_Show'45'Char_26 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Char_26
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Agda.Builtin.String.d_primShowChar_20)
-- Class.Show.Instances.Show-Bool
d_Show'45'Bool_32 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Bool_32
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Data.Bool.Show.d_show_6)
-- Class.Show.Instances.Show-ℕ
d_Show'45'ℕ_38 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'ℕ_38
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Data.Nat.Show.d_show_56)
-- Class.Show.Instances.Show-Fin
d_Show'45'Fin_46 ::
  Integer -> MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Fin_46 ~v0 = du_Show'45'Fin_46
du_Show'45'Fin_46 :: MAlonzo.Code.Class.Show.Core.T_Show_10
du_Show'45'Fin_46
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Data.String.Base.d__'43''43'__20
              ("# " :: Data.Text.Text)
              (MAlonzo.Code.Data.Nat.Show.d_show_56
                 (coe MAlonzo.Code.Data.Fin.Base.du_toℕ_18 (coe v0)))))
-- Class.Show.Instances.Show-Float
d_Show'45'Float_50 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Float_50
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Agda.Builtin.Float.d_primShowFloat_46)
-- Class.Show.Instances.Show-Name
d_Show'45'Name_56 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Name_56
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12)
-- Class.Show.Instances.Show-Meta
d_Show'45'Meta_58 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Meta_58
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowMeta_44)
-- Class.Show.Instances.Show-Relevance
d_Show'45'Relevance_60 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Relevance_60
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showRel_8)
-- Class.Show.Instances.Show-Vis
d_Show'45'Vis_62 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Vis_62
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showVisibility_10)
-- Class.Show.Instances.Show-Literal
d_Show'45'Literal_64 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Literal_64
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showLiteral_12)
-- Class.Show.Instances.Show-Arg
d_Show'45'Arg_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Arg_66 ~v0 ~v1 v2 = du_Show'45'Arg_66 v2
du_Show'45'Arg_66 ::
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10
du_Show'45'Arg_66 v0
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe
         (\ v1 ->
            case coe v1 of
              MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v2 v3
                -> case coe v2 of
                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v4 v5
                       -> coe
                            MAlonzo.Code.Data.String.Base.d__'43''43'__20
                            (MAlonzo.Code.Reflection.AST.Show.d_showVisibility_10 (coe v4))
                            (coe MAlonzo.Code.Class.Show.Core.d_show_18 v0 v3)
                     _ -> MAlonzo.RTE.mazUnreachableError
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Class.Show.Instances.Show-Abs
d_Show'45'Abs_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Abs_72 ~v0 ~v1 v2 = du_Show'45'Abs_72 v2
du_Show'45'Abs_72 ::
  MAlonzo.Code.Class.Show.Core.T_Show_10 ->
  MAlonzo.Code.Class.Show.Core.T_Show_10
du_Show'45'Abs_72 v0
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe
         (\ v1 ->
            case coe v1 of
              MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v2 v3
                -> coe
                     MAlonzo.Code.Data.String.Base.d__'43''43'__20
                     ("abs " :: Data.Text.Text)
                     (coe
                        MAlonzo.Code.Data.String.Base.d__'43''43'__20 v2
                        (coe
                           MAlonzo.Code.Data.String.Base.d__'43''43'__20
                           (" " :: Data.Text.Text)
                           (coe MAlonzo.Code.Class.Show.Core.d_show_18 v0 v3)))
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Class.Show.Instances.Show-Names
d_Show'45'Names_78 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Names_78
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showTerms_36)
-- Class.Show.Instances.Show-Term
d_Show'45'Term_80 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Term_80
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showTerm_38)
-- Class.Show.Instances.Show-Sort
d_Show'45'Sort_82 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Sort_82
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showSort_40)
-- Class.Show.Instances.Show-Patterns
d_Show'45'Patterns_84 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Patterns_84
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showPatterns_42)
-- Class.Show.Instances.Show-Pattern
d_Show'45'Pattern_86 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Pattern_86
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showPattern_44)
-- Class.Show.Instances.Show-Clause
d_Show'45'Clause_88 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Clause_88
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showClause_46)
-- Class.Show.Instances.Show-Clauses
d_Show'45'Clauses_90 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Clauses_90
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showClauses_48)
-- Class.Show.Instances.Show-Tel
d_Show'45'Tel_92 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Tel_92
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showTel_50)
-- Class.Show.Instances.Show-Definition
d_Show'45'Definition_94 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'Definition_94
  = coe
      MAlonzo.Code.Class.Show.Core.C_mkShow_20
      (coe MAlonzo.Code.Reflection.AST.Show.d_showDefinition_166)
-- Class.Show.Instances.Show-AName
d_Show'45'AName_96 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'AName_96
  = coe
      du_Show'45'Arg_66
      (coe
         MAlonzo.Code.Class.Show.Core.C_mkShow_20
         (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12))
-- Class.Show.Instances.Show-AType
d_Show'45'AType_98 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'AType_98
  = coe
      du_Show'45'Arg_66
      (coe
         MAlonzo.Code.Class.Show.Core.C_mkShow_20
         (coe MAlonzo.Code.Reflection.AST.Show.d_showTerm_38))
-- Class.Show.Instances.Show-ATerms
d_Show'45'ATerms_100 :: MAlonzo.Code.Class.Show.Core.T_Show_10
d_Show'45'ATerms_100
  = coe
      du_Show'45'List_20
      (coe
         du_Show'45'Arg_66
         (coe
            MAlonzo.Code.Class.Show.Core.C_mkShow_20
            (coe MAlonzo.Code.Agda.Builtin.Reflection.d_primShowQName_12)))
