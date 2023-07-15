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

module MAlonzo.Code.Tactic.ClauseBuilder where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Nat
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.NonEmpty.Base
import qualified MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional
import qualified MAlonzo.Code.Data.List.Relation.Unary.Linked
import qualified MAlonzo.Code.Data.List.Sort.MergeSort
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Generics.Utils
import qualified MAlonzo.Code.Interface.Monad
import qualified MAlonzo.Code.Interface.Monad.Instance
import qualified MAlonzo.Code.Interface.MonadError
import qualified MAlonzo.Code.Interface.MonadReader
import qualified MAlonzo.Code.Interface.MonadTC
import qualified MAlonzo.Code.Reflection.AST.Argument
import qualified MAlonzo.Code.Reflection.Debug
import qualified MAlonzo.Code.Tactic.Helpers

-- Tactic.ClauseBuilder._.sort
d_sort_6 :: [Integer] -> [Integer]
d_sort_6
  = coe
      MAlonzo.Code.Data.List.Sort.MergeSort.du_sort_168
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'8804''45'decTotalOrder_2638)
-- Tactic.ClauseBuilder._.sort-↗
d_sort'45''8599'_8 ::
  [Integer] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_sort'45''8599'_8
  = coe
      MAlonzo.Code.Data.List.Sort.MergeSort.du_sort'45''8599'_240
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'8804''45'decTotalOrder_2638)
-- Tactic.ClauseBuilder._.sort-↭
d_sort'45''8621'_10 ::
  [Integer] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_sort'45''8621'_10
  = coe
      MAlonzo.Code.Data.List.Sort.MergeSort.du_sort'45''8621'_202
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'8804''45'decTotalOrder_2638)
-- Tactic.ClauseBuilder.ClauseBuilder
d_ClauseBuilder_20 a0 = ()
data T_ClauseBuilder_20
  = C_ClauseBuilder'46'constructor_375 (() -> AgdaAny -> AgdaAny)
                                       (() ->
                                        [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
                                        MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
                                        AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny)
-- Tactic.ClauseBuilder.ClauseBuilder.Base
d_Base_32 :: T_ClauseBuilder_20 -> () -> ()
d_Base_32 = erased
-- Tactic.ClauseBuilder.ClauseBuilder.liftBase
d_liftBase_34 :: T_ClauseBuilder_20 -> () -> AgdaAny -> AgdaAny
d_liftBase_34 v0
  = case coe v0 of
      C_ClauseBuilder'46'constructor_375 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.ClauseBuilder.addPattern
d_addPattern_36 ::
  T_ClauseBuilder_20 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny -> AgdaAny
d_addPattern_36 v0
  = case coe v0 of
      C_ClauseBuilder'46'constructor_375 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.ClauseBuilder.toClause
d_toClause_38 :: T_ClauseBuilder_20 -> AgdaAny -> AgdaAny
d_toClause_38 v0
  = case coe v0 of
      C_ClauseBuilder'46'constructor_375 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._.Base
d_Base_42 :: T_ClauseBuilder_20 -> () -> ()
d_Base_42 = erased
-- Tactic.ClauseBuilder._.addPattern
d_addPattern_44 ::
  T_ClauseBuilder_20 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny -> AgdaAny
d_addPattern_44 v0 = coe d_addPattern_36 (coe v0)
-- Tactic.ClauseBuilder._.liftBase
d_liftBase_46 :: T_ClauseBuilder_20 -> () -> AgdaAny -> AgdaAny
d_liftBase_46 v0 = coe d_liftBase_34 (coe v0)
-- Tactic.ClauseBuilder._.toClause
d_toClause_48 :: T_ClauseBuilder_20 -> AgdaAny -> AgdaAny
d_toClause_48 v0 = coe d_toClause_38 (coe v0)
-- Tactic.ClauseBuilder.SinglePattern
d_SinglePattern_50 :: ()
d_SinglePattern_50 = erased
-- Tactic.ClauseBuilder.typedVarSinglePattern
d_typedVarSinglePattern_52 ::
  MAlonzo.Code.Agda.Builtin.String.T_String_6 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_typedVarSinglePattern_52 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                (coe
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0) (coe v1)))
             (coe
                MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v2)
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_var_244
                   (coe (0 :: Integer))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.varSinglePattern
d_varSinglePattern_60 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_varSinglePattern_60 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                (coe
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v1)
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))))
             (coe
                MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v1)
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_var_244
                   (coe (0 :: Integer))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.multiSinglePattern
d_multiSinglePattern_66 ::
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_multiSinglePattern_66 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
         (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
         (coe ()) (coe ())
         (coe
            (\ v2 ->
               coe
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                       (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                    (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))))
         (coe v0))
      (coe v1)
-- Tactic.ClauseBuilder.findIndexDefault
d_findIndexDefault_74 :: [Integer] -> Integer -> Integer -> Integer
d_findIndexDefault_74 v0 v1 v2
  = let v3
          = coe
              MAlonzo.Code.Data.List.Base.du_filter_792
              (\ v3 ->
                 case coe v3 of
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                     -> coe
                          MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464 (coe v5) (coe v2)
                   _ -> MAlonzo.RTE.mazUnreachableError)
              (coe
                 MAlonzo.Code.Tactic.Helpers.du_zipWithIndex_14
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32) (coe v0)) in
    case coe v3 of
      [] -> coe v1
      (:) v4 v5
        -> case coe v4 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7 -> coe v6
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.singlePatternFromPattern
d_singlePatternFromPattern_106 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_singlePatternFromPattern_106 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                MAlonzo.Code.Data.List.Base.du_replicate_314
                (coe
                   MAlonzo.Code.Data.List.Base.du_length_304
                   (d_appearingIndices_116 (coe v1) (coe v2) (coe v2)))
                (coe
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                   (coe ("" :: Data.Text.Text))
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                         (coe
                            MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                            (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))))
             (coe
                MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v1)
                (coe d_replacePatternIndex_140 (coe v1) (coe v2) (coe v2)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._.appearingIndices
d_appearingIndices_116 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_ArgInfo_76 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 -> [Integer]
d_appearingIndices_116 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v3 v4
        -> coe d_appearingIndicesHelper_118 (coe v0) (coe v1) (coe v4)
      MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v3
        -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v3
        -> coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v3)
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v3
        -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v3
        -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v3
        -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._.appearingIndicesHelper
d_appearingIndicesHelper_118 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_ArgInfo_76 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> [Integer]
d_appearingIndicesHelper_118 v0 v1 v2
  = case coe v2 of
      [] -> coe v2
      (:) v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
               -> coe
                    MAlonzo.Code.Data.List.Base.du__'43''43'__62
                    (coe d_appearingIndices_116 (coe v0) (coe v1) (coe v6))
                    (coe d_appearingIndicesHelper_118 (coe v0) (coe v1) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._.normalisedIndexList
d_normalisedIndexList_138 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_ArgInfo_76 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 -> [Integer]
d_normalisedIndexList_138 v0 v1
  = coe
      MAlonzo.Code.Data.List.Sort.MergeSort.du_sort_168
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'8804''45'decTotalOrder_2638)
      (coe
         MAlonzo.Code.Data.List.Base.du_deduplicate_834
         MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
         (d_appearingIndices_116 (coe v0) (coe v1) (coe v1)))
-- Tactic.ClauseBuilder._.replacePatternIndex
d_replacePatternIndex_140 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_ArgInfo_76 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150
d_replacePatternIndex_140 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 (coe v3)
             (coe d_replacePatternIndexHelper_142 (coe v0) (coe v1) (coe v4))
      MAlonzo.Code.Agda.Builtin.Reflection.C_dot_240 v3 -> coe v2
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_var_244
             (coe
                d_findIndexDefault_74
                (coe d_normalisedIndexList_138 (coe v0) (coe v1))
                (coe (0 :: Integer)) (coe v3))
      MAlonzo.Code.Agda.Builtin.Reflection.C_lit_248 v3 -> coe v2
      MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v3 -> coe v2
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd_256 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._.replacePatternIndexHelper
d_replacePatternIndexHelper_142 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_ArgInfo_76 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_replacePatternIndexHelper_142 v0 v1 v2
  = case coe v2 of
      [] -> coe v2
      (:) v3 v4
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v5)
                       (coe d_replacePatternIndex_140 (coe v0) (coe v1) (coe v6)))
                    (coe d_replacePatternIndexHelper_142 (coe v0) (coe v1) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._.ctxSinglePatterns
d_ctxSinglePatterns_180 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny
d_ctxSinglePatterns_180 ~v0 v1 ~v2 v3 ~v4
  = du_ctxSinglePatterns_180 v1 v3
du_ctxSinglePatterns_180 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 -> AgdaAny
du_ctxSinglePatterns_180 v0 v1
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe
         MAlonzo.Code.Interface.MonadTC.du_getContext_618 (coe v0) (coe v1))
      (\ v2 ->
         coe
           MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
           (coe
              MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
              (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
              (coe ()) (coe ()) (coe d_singlePatternFromPattern_106)
              (coe
                 MAlonzo.Code.Tactic.Helpers.du_zipWithIndex_14
                 (coe
                    (\ v3 v4 ->
                       case coe v4 of
                         MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v5 v6
                           -> coe
                                MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v5)
                                (coe MAlonzo.Code.Agda.Builtin.Reflection.C_var_244 (coe v3))
                         _ -> MAlonzo.RTE.mazUnreachableError))
                 (coe v2))))
-- Tactic.ClauseBuilder._.constrToPattern
d_constrToPattern_190 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_constrToPattern_190 ~v0 v1 ~v2 v3 v4 v5 ~v6
  = du_constrToPattern_190 v1 v3 v4 v5
du_constrToPattern_190 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
du_constrToPattern_190 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe
         MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
         (coe ()) (coe MAlonzo.Code.Generics.Utils.d_viewTy_114)
         (coe
            MAlonzo.Code.Interface.MonadTC.du_runAndReset_234 (coe v0) (coe v2)
            (coe ())
            (coe
               MAlonzo.Code.Interface.MonadReader.d_local_72 v1 () erased
               (\ v4 ->
                  coe
                    MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                    (coe MAlonzo.Code.Interface.MonadTC.d_reconstruction_42 (coe v4))
                    (coe MAlonzo.Code.Interface.MonadTC.d_noConstraints_44 (coe v4))
                    (coe MAlonzo.Code.Interface.MonadTC.d_reduction_46 (coe v4))
                    (coe MAlonzo.Code.Interface.MonadTC.d_globalContext_48 (coe v4))
                    (coe MAlonzo.Code.Interface.MonadTC.d_localContext_50 (coe v4))
                    (coe MAlonzo.Code.Interface.MonadTC.d_goal_52 (coe v4))
                    (coe MAlonzo.Code.Interface.MonadTC.d_debug_54 (coe v4)))
               (coe
                  MAlonzo.Code.Interface.MonadTC.d_inferType_162 v2
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 (coe v3)
                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))
      (\ v4 ->
         case coe v4 of
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
             -> coe
                  MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                  (coe
                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                     (coe
                        MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                        (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
                        (coe ()) (coe ())
                        (coe
                           (\ v7 ->
                              case coe v7 of
                                MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v8 v9
                                  -> case coe v9 of
                                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v10 v11
                                         -> coe
                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v8)
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                 (coe v10)
                                                 (coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                                       _ -> MAlonzo.RTE.mazUnreachableError
                                _ -> MAlonzo.RTE.mazUnreachableError))
                        (coe v5))
                     (coe
                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                        (coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                           (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                           (coe
                              MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                        (coe
                           MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 (coe v3)
                           (coe
                              MAlonzo.Code.Data.List.Base.du_zipWith_134
                              (coe
                                 (\ v7 v8 ->
                                    case coe v7 of
                                      MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v9 v10
                                        -> case coe v10 of
                                             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v11 v12
                                               -> coe
                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                    (coe v11)
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_var_244
                                                       (coe v8))
                                             _ -> MAlonzo.RTE.mazUnreachableError
                                      _ -> MAlonzo.RTE.mazUnreachableError))
                              (coe v5)
                              (coe
                                 MAlonzo.Code.Data.List.Base.d_downFrom_432
                                 (coe MAlonzo.Code.Data.List.Base.du_length_304 v5))))))
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Tactic.ClauseBuilder._.constrToPatternTyped
d_constrToPatternTyped_218 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] -> AgdaAny
d_constrToPatternTyped_218 ~v0 v1 v2 v3 v4 v5 v6
  = du_constrToPatternTyped_218 v1 v2 v3 v4 v5 v6
du_constrToPatternTyped_218 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] -> AgdaAny
du_constrToPatternTyped_218 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe
         MAlonzo.Code.Tactic.Helpers.du_applyWithVisibility_264 (coe v0)
         (coe v1) (coe v2) (coe v3) (coe v4) (coe v5))
      (\ v6 ->
         coe
           MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
           (coe ())
           (coe
              MAlonzo.Code.Interface.MonadTC.du_debugLog'7504'_508 (coe v0)
              (coe v3) (coe v2)
              (coe
                 MAlonzo.Code.Interface.MonadTC.du__'8759''7496''7504'__470 (coe v0)
                 (coe
                    MAlonzo.Code.Interface.MonadTC.du__'7515''8319'_486 (coe v0)
                    (coe v3) (coe v2) (coe ()) (coe v6))
                 (coe
                    MAlonzo.Code.Interface.MonadTC.du_IsMErrorPart'45'MErrorPartWrap_462)
                 (coe
                    MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
           (coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
              erased
              (coe
                 MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
                 (coe ()) (coe MAlonzo.Code.Generics.Utils.d_viewTy_114)
                 (coe
                    MAlonzo.Code.Interface.MonadReader.d_local_72 v2 () erased
                    (\ v7 ->
                       coe
                         MAlonzo.Code.Interface.MonadTC.C_TCEnv'46'constructor_949
                         (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                         (coe MAlonzo.Code.Interface.MonadTC.d_reconstruction_42 (coe v7))
                         (coe MAlonzo.Code.Interface.MonadTC.d_noConstraints_44 (coe v7))
                         (coe MAlonzo.Code.Interface.MonadTC.d_reduction_46 (coe v7))
                         (coe MAlonzo.Code.Interface.MonadTC.d_globalContext_48 (coe v7))
                         (coe MAlonzo.Code.Interface.MonadTC.d_localContext_50 (coe v7))
                         (coe MAlonzo.Code.Interface.MonadTC.d_goal_52 (coe v7))
                         (coe MAlonzo.Code.Interface.MonadTC.d_debug_54 (coe v7)))
                    (coe MAlonzo.Code.Interface.MonadTC.d_inferType_162 v3 v6)))
              (\ v7 ->
                 case coe v7 of
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
                     -> coe
                          MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                          (coe
                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                             (coe
                                MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                                (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
                                (coe ()) (coe ())
                                (coe
                                   (\ v10 ->
                                      case coe v10 of
                                        MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v11 v12
                                          -> coe
                                               seq (coe v12)
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                  (coe v11) (coe v12))
                                        _ -> MAlonzo.RTE.mazUnreachableError))
                                (coe v8))
                             (coe
                                MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                                   (coe
                                      MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                      (coe
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 (coe v4)
                                   (coe
                                      MAlonzo.Code.Data.List.Base.du_zipWith_134
                                      (coe
                                         (\ v10 v11 ->
                                            case coe v10 of
                                              MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v12 v13
                                                -> case coe v13 of
                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v14 v15
                                                       -> coe
                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                            (coe v14)
                                                            (coe
                                                               MAlonzo.Code.Agda.Builtin.Reflection.C_var_244
                                                               (coe v11))
                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                              _ -> MAlonzo.RTE.mazUnreachableError))
                                      (coe v8)
                                      (coe
                                         MAlonzo.Code.Data.List.Base.d_downFrom_432
                                         (coe MAlonzo.Code.Data.List.Base.du_length_304 v8))))))
                   _ -> MAlonzo.RTE.mazUnreachableError)))
-- Tactic.ClauseBuilder._.constructorPatterns
d_constructorPatterns_248 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny
d_constructorPatterns_248 ~v0 v1 v2 ~v3 v4 v5
  = du_constructorPatterns_248 v1 v2 v4 v5
du_constructorPatterns_248 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny
du_constructorPatterns_248 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v4 v5
        -> coe
             MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
             erased
             (coe
                MAlonzo.Code.Interface.MonadTC.du_getConstrsForType_736 (coe v0)
                (coe v1) (coe v2) (coe v5))
             (\ v6 ->
                coe
                  MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                  (coe
                     MAlonzo.Code.Interface.Monad.du__'60''38''62'__68
                     (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
                     (coe ()) (coe ()) (coe v6)
                     (coe
                        (\ v7 ->
                           case coe v7 of
                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
                               -> coe
                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                    (coe
                                       MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                                       (coe
                                          MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
                                       (coe ()) (coe ())
                                       (coe
                                          (\ v10 ->
                                             case coe v10 of
                                               MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v11 v12
                                                 -> coe
                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                      (coe v11) (coe v12)
                                               _ -> MAlonzo.RTE.mazUnreachableError))
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                          (coe MAlonzo.Code.Generics.Utils.d_viewTy_114 (coe v9))))
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 (coe v4)
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Reflection.C_con_236 (coe v8)
                                          (coe
                                             MAlonzo.Code.Tactic.Helpers.du_zipWithIndex_14
                                             (coe
                                                (\ v10 v11 ->
                                                   case coe v11 of
                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v12 v13
                                                       -> case coe v13 of
                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v14 v15
                                                              -> coe
                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                   (coe v14)
                                                                   (coe
                                                                      MAlonzo.Code.Agda.Builtin.Reflection.C_var_244
                                                                      (coe
                                                                         MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                                                                         (coe
                                                                            MAlonzo.Code.Data.List.Base.du_length_304
                                                                            (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                               (coe
                                                                                  MAlonzo.Code.Generics.Utils.d_viewTy_114
                                                                                  (coe v9))))
                                                                         (addInt
                                                                            (coe (1 :: Integer))
                                                                            (coe v10))))
                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                     _ -> MAlonzo.RTE.mazUnreachableError))
                                             (coe
                                                MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                (coe
                                                   MAlonzo.Code.Generics.Utils.d_viewTy_114
                                                   (coe v9))))))
                             _ -> MAlonzo.RTE.mazUnreachableError))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._.constructorPatterns'
d_constructorPatterns''_276 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_constructorPatterns''_276 ~v0 v1 v2 v3 v4 v5
  = du_constructorPatterns''_276 v1 v2 v3 v4 v5
du_constructorPatterns''_276 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_constructorPatterns''_276 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe
         MAlonzo.Code.Interface.MonadTC.du_getConstrsForType_736 (coe v0)
         (coe v1) (coe v3) (coe v4))
      (\ v5 ->
         coe
           MAlonzo.Code.Interface.Monad.du_traverseList_70 (coe v0) (coe ())
           (coe
              (\ v6 ->
                 case coe v6 of
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
                     -> coe du_constrToPattern_190 (coe v0) (coe v2) (coe v3) (coe v7)
                   _ -> MAlonzo.RTE.mazUnreachableError))
           (coe v5))
-- Tactic.ClauseBuilder._.constructorPatternsTyped
d_constructorPatternsTyped_286 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] -> AgdaAny
d_constructorPatternsTyped_286 ~v0 v1 v2 v3 v4 v5 v6
  = du_constructorPatternsTyped_286 v1 v2 v3 v4 v5 v6
du_constructorPatternsTyped_286 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146] -> AgdaAny
du_constructorPatternsTyped_286 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe
         MAlonzo.Code.Interface.MonadTC.du_getConstrsForType_736 (coe v0)
         (coe v1) (coe v3) (coe v4))
      (\ v6 ->
         coe
           MAlonzo.Code.Interface.Monad.du_traverseList_70 (coe v0) (coe ())
           (coe
              (\ v7 ->
                 case coe v7 of
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
                     -> coe
                          du_constrToPatternTyped_218 (coe v0) (coe v1) (coe v2) (coe v3)
                          (coe v8) (coe v5)
                   _ -> MAlonzo.RTE.mazUnreachableError))
           (coe v6))
-- Tactic.ClauseBuilder.ClauseInfo
d_ClauseInfo_298 :: ()
d_ClauseInfo_298 = erased
-- Tactic.ClauseBuilder.ClauseExpr
d_ClauseExpr_300 = ()
newtype T_ClauseExpr_300
  = C_MatchExpr_302 [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
-- Tactic.ClauseBuilder.multiClauseExpr
d_multiClauseExpr_304 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> T_ClauseExpr_300
d_multiClauseExpr_304 v0
  = coe
      C_MatchExpr_302
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22 (coe d_helper_320) (coe v0))
-- Tactic.ClauseBuilder._.helper'
d_helper''_310 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_helper''_310 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> case coe v1 of
             [] -> coe v2
             (:) v3 v4
               -> coe
                    MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
                    (coe
                       C_MatchExpr_302
                       (coe
                          MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                          (coe
                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                             (coe
                                d_helper''_310
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4)
                                   (coe v2))))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._.helper
d_helper_320 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_helper_320 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> case coe v1 of
             MAlonzo.Code.Data.List.NonEmpty.Base.C__'8759'__34 v3 v4
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                    (coe
                       d_helper''_310
                       (coe
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4) (coe v2)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.clauseExprToClauseInfo
d_clauseExprToClauseInfo_328 ::
  T_ClauseExpr_300 -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_clauseExprToClauseInfo_328 v0
  = case coe v0 of
      C_MatchExpr_302 v1
        -> case coe v1 of
             [] -> coe v1
             (:) v2 v3
               -> case coe v2 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                      -> case coe v5 of
                           MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v6
                             -> coe
                                  MAlonzo.Code.Data.List.Base.du__'43''43'__62
                                  (coe
                                     MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                                     (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
                                     (coe ()) (coe ())
                                     (coe
                                        MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                                        (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v4)))
                                     (coe d_clauseExprToClauseInfo_328 (coe v6)))
                                  (coe d_clauseExprToClauseInfo_328 (coe C_MatchExpr_302 (coe v3)))
                           MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v6
                             -> coe
                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                     (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v4))
                                     (coe v6))
                                  (coe d_clauseExprToClauseInfo_328 (coe C_MatchExpr_302 (coe v3)))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.clauseInfoToClauseArgs
d_clauseInfoToClauseArgs_344 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_clauseInfoToClauseArgs_344 v0
  = let v1 = coe du_helper_352 (coe (0 :: Integer)) (coe v0) in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> case coe v3 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v4)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._.helper
d_helper_352 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_helper_352 ~v0 v1 v2 = du_helper_352 v1 v2
du_helper_352 ::
  Integer ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_helper_352 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1)
             (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v0))
      (:) v2 v3
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
               -> case coe v5 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v6 v7
                      -> let v8 = coe du_helper_352 (coe v0) (coe v3) in
                         case coe v8 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v9 v10
                             -> case coe v10 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
                                    -> coe
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                         (coe
                                            MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4)
                                            (coe v9))
                                         (coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                  (coe v6)
                                                  (coe
                                                     MAlonzo.Code.Generics.Utils.d_mapVariables_86
                                                     (coe (\ v13 -> addInt (coe v12) (coe v13)))
                                                     (coe v7)))
                                               (coe v11))
                                            (coe
                                               addInt
                                               (coe MAlonzo.Code.Data.List.Base.du_length_304 v4)
                                               (coe v12)))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.clauseInfoToClause
d_clauseInfoToClause_394 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152
d_clauseInfoToClause_394 v0 v1
  = let v2 = d_clauseInfoToClauseArgs_344 (coe v0) in
    case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v5
               -> coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 (coe v3) (coe v4)
                    (coe v5)
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270
                    (coe v3) (coe v4)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.clauseExprToClauses
d_clauseExprToClauses_422 ::
  T_ClauseExpr_300 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152]
d_clauseExprToClauses_422 v0
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
      (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
      (coe ()) (coe ())
      (coe
         (\ v1 ->
            case coe v1 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
                -> coe d_clauseInfoToClause_394 (coe v2) (coe v3)
              _ -> MAlonzo.RTE.mazUnreachableError))
      (coe d_clauseExprToClauseInfo_328 (coe v0))
-- Tactic.ClauseBuilder.nonBindingClause
d_nonBindingClause_432 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152
d_nonBindingClause_432
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Tactic.ClauseBuilder.clauseExprToPatLam
d_clauseExprToPatLam_434 ::
  T_ClauseExpr_300 -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_clauseExprToPatLam_434 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188
      (coe d_clauseExprToClauses_422 (coe v0))
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Tactic.ClauseBuilder.ContextMonad
d_ContextMonad_444 a0 a1 = ()
newtype T_ContextMonad_444
  = C_ContextMonad'46'constructor_50015 (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                         () ->
                                         MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
                                         AgdaAny -> AgdaAny)
-- Tactic.ClauseBuilder.ContextMonad.introPatternM
d_introPatternM_452 ::
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_introPatternM_452 v0
  = case coe v0 of
      C_ContextMonad'46'constructor_50015 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.ContextMonad.extendContextM
d_extendContextM_454 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny -> AgdaAny
d_extendContextM_454 ~v0 ~v1 v2 v3 ~v4 v5 v6
  = du_extendContextM_454 v2 v3 v5 v6
du_extendContextM_454 ::
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny -> AgdaAny
du_extendContextM_454 v0 v1 v2 v3
  = coe
      d_introPatternM_452 v0 v1 erased
      (d_typedVarSinglePattern_52 (coe ("" :: Data.Text.Text)) (coe v2))
      v3
-- Tactic.ClauseBuilder._.introPatternM
d_introPatternM_464 ::
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_introPatternM_464 v0 = coe d_introPatternM_452 (coe v0)
-- Tactic.ClauseBuilder.Monad-Id
d_Monad'45'Id_466 :: MAlonzo.Code.Interface.Monad.T_Monad_20
d_Monad'45'Id_466
  = coe
      MAlonzo.Code.Interface.Monad.C_Monad'46'constructor_273
      (coe (\ v0 v1 v2 -> v2)) (coe (\ v0 v1 v2 v3 v4 v5 -> coe v5 v4))
-- Tactic.ClauseBuilder.ContextMonad-Id
d_ContextMonad'45'Id_468 :: T_ContextMonad_444
d_ContextMonad'45'Id_468
  = coe
      C_ContextMonad'46'constructor_50015 (coe (\ v0 v1 v2 v3 -> v3))
-- Tactic.ClauseBuilder._.refineWithSingle
d_refineWithSingle_488 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
   MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  AgdaAny -> AgdaAny
d_refineWithSingle_488 ~v0 v1 ~v2 v3 v4 v5 v6
  = du_refineWithSingle_488 v1 v3 v4 v5 v6
du_refineWithSingle_488 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  (MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
   MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  AgdaAny -> AgdaAny
du_refineWithSingle_488 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe
         MAlonzo.Code.Interface.MonadTC.du_goalTy_684 (coe v0) (coe v2)
         (coe v1))
      (\ v5 ->
         coe
           MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
           erased
           (coe
              MAlonzo.Code.Interface.MonadTC.du_newMeta_308 v2
              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
           (\ v6 ->
              coe
                MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                erased
                (coe
                   MAlonzo.Code.Interface.MonadTC.d_checkType_164 v2 (coe v3 v6) v5)
                (\ v7 ->
                   coe
                     MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                     erased
                     (coe MAlonzo.Code.Interface.MonadTC.du_runWithHole_764 v1 () v6 v4)
                     (\ v8 ->
                        coe
                          MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
                          (coe ()) (coe MAlonzo.Code.Interface.MonadTC.d_unify_156 v2 v6 v8)
                          (coe
                             MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                             (coe v3 v8))))))
-- Tactic.ClauseBuilder._.caseMatch
d_caseMatch_502 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny -> AgdaAny
d_caseMatch_502 ~v0 v1 ~v2 v3 v4 v5 v6
  = du_caseMatch_502 v1 v3 v4 v5 v6
du_caseMatch_502 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny -> AgdaAny
du_caseMatch_502 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
      (coe ())
      (coe
         MAlonzo.Code.Interface.MonadTC.du_debugLog_500 (coe v0) (coe v2)
         (coe v1)
         (coe
            MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
            (coe ("Match" :: Data.Text.Text))
            (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
            (coe
               MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v3)
               (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
      (coe
         du_refineWithSingle_488 (coe v0) (coe v1) (coe v2)
         (coe
            (\ v5 ->
               coe
                 MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
                 (coe
                    (MAlonzo.RTE.QName
                       (234 :: Integer) (10779521135412943468 :: Integer)
                       "Function.Base.case_of_"
                       (MAlonzo.RTE.Fixity
                          MAlonzo.RTE.NonAssoc (MAlonzo.RTE.Related (0.0 :: Double)))))
                 (coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                       (coe v3))
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                             (coe
                                MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                                (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                                (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                          (coe v5))
                       (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
         (coe
            MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
            (coe ())
            (coe
               (\ v5 ->
                  coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188
                    (coe d_clauseExprToClauses_422 (coe v5))
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
            (coe v4)))
-- Tactic.ClauseBuilder._.currentTyConstrPatterns
d_currentTyConstrPatterns_512 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny
d_currentTyConstrPatterns_512 ~v0 v1 v2 v3 v4
  = du_currentTyConstrPatterns_512 v1 v2 v3 v4
du_currentTyConstrPatterns_512 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny
du_currentTyConstrPatterns_512 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased
      (coe
         MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
         (coe ()) (coe MAlonzo.Code.Generics.Utils.d_viewTy'8242'_150)
         (coe
            MAlonzo.Code.Interface.MonadTC.du_goalTy_684 (coe v0) (coe v3)
            (coe v2)))
      (\ v4 ->
         case coe v4 of
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
             -> let v7
                      = coe
                          MAlonzo.Code.Interface.MonadTC.du_error1_664 (coe v1) (coe ())
                          (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                          (coe
                             ("currentTyConstrPatterns: Goal type is not a forall!"
                              ::
                              Data.Text.Text)) in
                case coe v5 of
                  (:) v8 v9
                    -> coe
                         du_constructorPatterns''_276 (coe v0) (coe v1) (coe v2) (coe v3)
                         (coe MAlonzo.Code.Reflection.AST.Argument.du_unArg_74 (coe v8))
                  _ -> coe v7
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Tactic.ClauseBuilder.stripMetaLambdas
d_stripMetaLambdas_518 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_stripMetaLambdas_518 = coe d_helper_524 (coe (0 :: Integer))
-- Tactic.ClauseBuilder._.helper
d_helper_524 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_helper_524 v0 v1
  = let v2
          = coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208 in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_lam_182 v3 v4
        -> case coe v4 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_abs_114 v5 v6
               -> coe
                    d_helper_524 (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v6)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 (coe v3)
             (coe
                MAlonzo.Code.Reflection.AST.Argument.du_map'45'Args_62
                (coe
                   MAlonzo.Code.Generics.Utils.d_mapVars_328
                   (coe
                      (\ v5 -> coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v5 v0)))
                (coe
                   MAlonzo.Code.Data.List.Base.du_take_576
                   (coe
                      MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                      (coe MAlonzo.Code.Data.List.Base.du_length_304 v4) v0)
                   (coe v4)))
      _ -> coe v2
-- Tactic.ClauseBuilder._.isProj
d_isProj_554 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 -> Bool
d_isProj_554 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_isProj_554 v5
du_isProj_554 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Pattern_150 -> Bool
du_isProj_554 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
    case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_proj_252 v2
        -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      _ -> coe v1
-- Tactic.ClauseBuilder._.specializeType
d_specializeType_556 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_specializeType_556 ~v0 v1 v2 v3 v4 v5 v6
  = du_specializeType_556 v1 v2 v3 v4 v5 v6
du_specializeType_556 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_specializeType_556 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
        -> case coe v7 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v8 v9
               -> coe
                    MAlonzo.Code.Interface.MonadTC.du_markDontFail_676 (coe v0)
                    (coe v1) (coe v3) (coe v2) (coe ())
                    (coe ("specializeType" :: Data.Text.Text))
                    (coe
                       MAlonzo.Code.Tactic.Helpers.du_inDebugPath_132 (coe v0) (coe v1)
                       (coe v2) (coe v3) (coe ())
                       (coe ("specializeType" :: Data.Text.Text))
                       (coe
                          MAlonzo.Code.Interface.MonadTC.du_runAndReset_234 (coe v0) (coe v3)
                          (coe ())
                          (coe
                             MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) (coe ())
                             (coe ())
                             (coe
                                MAlonzo.Code.Interface.MonadTC.du_debugLog_500 (coe v0) (coe v3)
                                (coe v2)
                                (coe
                                   MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                   (coe ("Goal type to specialize: " :: Data.Text.Text))
                                   (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                   (coe
                                      MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38 (coe v5)
                                      (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                             (coe
                                MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
                                erased
                                (coe
                                   MAlonzo.Code.Interface.Monad.d_return_28 v0 () erased
                                   (d_clauseExprToClauses_422
                                      (coe
                                         C_MatchExpr_302
                                         (coe
                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4)
                                               (coe
                                                  MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))))
                                            (coe
                                               MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                                               (coe du_isProj_554 (coe v9))
                                               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
                                               (coe
                                                  MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                     (coe
                                                        d_varSinglePattern_60
                                                        (coe
                                                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                           (coe v8) (coe ("_" :: Data.Text.Text))))
                                                     (coe
                                                        MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                                                        (coe
                                                           MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                                                           (coe
                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))))))))))
                                (\ v10 ->
                                   let v11
                                         = coe
                                             MAlonzo.Code.Interface.MonadTC.du_error1_664 (coe v1)
                                             (coe ())
                                             (coe
                                                MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                             (coe ("BUG" :: Data.Text.Text)) in
                                   case coe v10 of
                                     (:) v12 v13
                                       -> case coe v12 of
                                            MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v14 v15 v16
                                              -> coe
                                                   MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                   (coe v0) (coe ()) (coe ())
                                                   (coe
                                                      MAlonzo.Code.Interface.MonadTC.du_debugLog_500
                                                      (coe v0) (coe v3) (coe v2)
                                                      (coe
                                                         MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                         (coe ("With pattern: " :: Data.Text.Text))
                                                         (coe
                                                            MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                         (coe
                                                            MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                            (coe v10)
                                                            (coe
                                                               MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'ListClause_32)
                                                            (coe
                                                               MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                                                   (coe
                                                      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                                      v0 () erased () erased
                                                      (coe
                                                         MAlonzo.Code.Interface.MonadTC.d_checkType_164
                                                         v3
                                                         (coe
                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188
                                                            (coe v10)
                                                            (coe
                                                               MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                                                         v5)
                                                      (\ v17 ->
                                                         let v18
                                                               = coe
                                                                   MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                                   (coe v0) (coe ()) (coe ())
                                                                   (coe
                                                                      MAlonzo.Code.Interface.MonadTC.du_debugLog_500
                                                                      (coe v0) (coe v3) (coe v2)
                                                                      (coe
                                                                         MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                         (coe
                                                                            ("BUG in specializeType:"
                                                                             ::
                                                                             Data.Text.Text))
                                                                         (coe
                                                                            MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                         (coe
                                                                            MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                            (coe v17)
                                                                            (coe
                                                                               MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                            (coe
                                                                               MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                               (coe
                                                                                  ("\nWith pattern:"
                                                                                   ::
                                                                                   Data.Text.Text))
                                                                               (coe
                                                                                  MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                               (coe
                                                                                  MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                  (coe v10)
                                                                                  (coe
                                                                                     MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'ListClause_32)
                                                                                  (coe
                                                                                     MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                     (coe
                                                                                        ("\nWith type:"
                                                                                         ::
                                                                                         Data.Text.Text))
                                                                                     (coe
                                                                                        MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                     (coe
                                                                                        MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                        (coe v5)
                                                                                        (coe
                                                                                           MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'Term_24)
                                                                                        (coe
                                                                                           MAlonzo.Code.Reflection.Debug.du__'8759''7496'__38
                                                                                           (coe
                                                                                              ("\nSinglePattern:"
                                                                                               ::
                                                                                               Data.Text.Text))
                                                                                           (coe
                                                                                              MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                           (coe
                                                                                              MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))))))
                                                                   (coe
                                                                      MAlonzo.Code.Interface.MonadTC.du_error1_664
                                                                      (coe v1) (coe ())
                                                                      (coe
                                                                         MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                      (coe
                                                                         ("BUG"
                                                                          ::
                                                                          Data.Text.Text))) in
                                                         case coe v17 of
                                                           MAlonzo.Code.Agda.Builtin.Reflection.C_pat'45'lam_188 v19 v20
                                                             -> case coe v19 of
                                                                  (:) v21 v22
                                                                    -> case coe v21 of
                                                                         MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v23 v24 v25
                                                                           -> case coe v25 of
                                                                                MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206 v26 v27
                                                                                  -> case coe v20 of
                                                                                       []
                                                                                         -> coe
                                                                                              MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                                                              (coe
                                                                                                 v0)
                                                                                              (coe
                                                                                                 ())
                                                                                              (coe
                                                                                                 ())
                                                                                              (coe
                                                                                                 MAlonzo.Code.Tactic.Helpers.du_logCurrentContext_130
                                                                                                 (coe
                                                                                                    v0)
                                                                                                 (coe
                                                                                                    v1)
                                                                                                 (coe
                                                                                                    v2)
                                                                                                 (coe
                                                                                                    v3))
                                                                                              (coe
                                                                                                 MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                                                                 (coe
                                                                                                    v0)
                                                                                                 (coe
                                                                                                    ())
                                                                                                 (coe
                                                                                                    ())
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Interface.MonadTC.du_debugLog1_668
                                                                                                    (coe
                                                                                                       v0)
                                                                                                    (coe
                                                                                                       v3)
                                                                                                    (coe
                                                                                                       v2)
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                    (coe
                                                                                                       ("New context:"
                                                                                                        ::
                                                                                                        Data.Text.Text)))
                                                                                                 (coe
                                                                                                    MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                                                                    (coe
                                                                                                       v0)
                                                                                                    (coe
                                                                                                       ())
                                                                                                    (coe
                                                                                                       ())
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Tactic.Helpers.du_logTelescope_68
                                                                                                       (coe
                                                                                                          v0)
                                                                                                       (coe
                                                                                                          v1)
                                                                                                       (coe
                                                                                                          v2)
                                                                                                       (coe
                                                                                                          v3)
                                                                                                       (coe
                                                                                                          MAlonzo.Code.Data.List.Base.du_map_22
                                                                                                          (coe
                                                                                                             (\ v28 ->
                                                                                                                coe
                                                                                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                  (coe
                                                                                                                     MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
                                                                                                                  (coe
                                                                                                                     v28)))
                                                                                                          (coe
                                                                                                             MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                                                                                                             (coe
                                                                                                                MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
                                                                                                             (coe
                                                                                                                ())
                                                                                                             (coe
                                                                                                                ())
                                                                                                             (coe
                                                                                                                (\ v28 ->
                                                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                                                                     (coe
                                                                                                                        v28)))
                                                                                                             (coe
                                                                                                                v23))))
                                                                                                    (coe
                                                                                                       MAlonzo.Code.Interface.Monad.du__'62''62'__32
                                                                                                       (coe
                                                                                                          v0)
                                                                                                       (coe
                                                                                                          ())
                                                                                                       (coe
                                                                                                          ())
                                                                                                       (coe
                                                                                                          MAlonzo.Code.Interface.MonadTC.du_debugLog1_668
                                                                                                          (coe
                                                                                                             v0)
                                                                                                          (coe
                                                                                                             v3)
                                                                                                          (coe
                                                                                                             v2)
                                                                                                          (coe
                                                                                                             MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                                                                                                          (coe
                                                                                                             ("TEST"
                                                                                                              ::
                                                                                                              Data.Text.Text)))
                                                                                                       (coe
                                                                                                          MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
                                                                                                          v0
                                                                                                          ()
                                                                                                          erased
                                                                                                          ()
                                                                                                          erased
                                                                                                          (coe
                                                                                                             MAlonzo.Code.Interface.MonadTC.du_extendContext''_642
                                                                                                             (coe
                                                                                                                v2)
                                                                                                             (coe
                                                                                                                ())
                                                                                                             (coe
                                                                                                                MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                                                                                                                (coe
                                                                                                                   MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
                                                                                                                (coe
                                                                                                                   ())
                                                                                                                (coe
                                                                                                                   ())
                                                                                                                (coe
                                                                                                                   (\ v28 ->
                                                                                                                      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                                                                        (coe
                                                                                                                           v28)))
                                                                                                                (coe
                                                                                                                   v23))
                                                                                                             (coe
                                                                                                                MAlonzo.Code.Interface.MonadTC.d_inferType_162
                                                                                                                v3
                                                                                                                (coe
                                                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_meta_206
                                                                                                                   (coe
                                                                                                                      v26)
                                                                                                                   (coe
                                                                                                                      MAlonzo.Code.Reflection.AST.Argument.du_map'45'Args_62
                                                                                                                      (coe
                                                                                                                         MAlonzo.Code.Generics.Utils.d_mapVars_328
                                                                                                                         (coe
                                                                                                                            (\ v28 ->
                                                                                                                               coe
                                                                                                                                 MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                                                                                                                                 v28
                                                                                                                                 (0 ::
                                                                                                                                    Integer))))
                                                                                                                      (coe
                                                                                                                         MAlonzo.Code.Data.List.Base.du_take_576
                                                                                                                         (coe
                                                                                                                            MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22
                                                                                                                            (coe
                                                                                                                               MAlonzo.Code.Data.List.Base.du_length_304
                                                                                                                               v27)
                                                                                                                            (0 ::
                                                                                                                               Integer))
                                                                                                                         (coe
                                                                                                                            v27))))))
                                                                                                          (\ v28 ->
                                                                                                             coe
                                                                                                               MAlonzo.Code.Interface.Monad.d_return_28
                                                                                                               v0
                                                                                                               ()
                                                                                                               erased
                                                                                                               (coe
                                                                                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                                  (coe
                                                                                                                     v28)
                                                                                                                  (coe
                                                                                                                     MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
                                                                                                                     (coe
                                                                                                                        MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
                                                                                                                     (coe
                                                                                                                        ())
                                                                                                                     (coe
                                                                                                                        ())
                                                                                                                     (coe
                                                                                                                        (\ v29 ->
                                                                                                                           MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                                                                                                             (coe
                                                                                                                                v29)))
                                                                                                                     (coe
                                                                                                                        v23))))))))
                                                                                       _ -> coe v18
                                                                                _ -> coe v18
                                                                         _ -> coe v18
                                                                  _ -> coe v18
                                                           _ -> coe v18))
                                            _ -> coe v11
                                     _ -> coe v11)))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._.ContextMonad-MonadTC
d_ContextMonad'45'MonadTC_598 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> T_ContextMonad_444
d_ContextMonad'45'MonadTC_598 ~v0 v1 v2 v3 v4
  = du_ContextMonad'45'MonadTC_598 v1 v2 v3 v4
du_ContextMonad'45'MonadTC_598 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> T_ContextMonad_444
du_ContextMonad'45'MonadTC_598 v0 v1 v2 v3
  = coe
      C_ContextMonad'46'constructor_50015
      (coe
         (\ v4 v5 v6 v7 ->
            coe
              MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased v4
              erased
              (coe
                 MAlonzo.Code.Interface.MonadTC.du_goalTy_684 (coe v0) (coe v3)
                 (coe v2))
              (\ v8 ->
                 coe
                   MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased v4
                   erased
                   (coe
                      du_specializeType_556 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6)
                      (coe v8))
                   (\ v9 ->
                      case coe v9 of
                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v10 v11
                          -> coe
                               MAlonzo.Code.Interface.MonadTC.du_extendContext''_642 (coe v2)
                               (coe v4) (coe v11)
                               (coe
                                  MAlonzo.Code.Interface.MonadTC.du_runWithGoalTy_770 v2 v4 v10 v7)
                        _ -> MAlonzo.RTE.mazUnreachableError))))
-- Tactic.ClauseBuilder.ClauseExprM.matchExprM
d_matchExprM_622 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_matchExprM_622 ~v0 v1 v2 v3 = du_matchExprM_622 v1 v2 v3
du_matchExprM_622 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
du_matchExprM_622 v0 v1 v2
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
      (coe ()) (coe C_MatchExpr_302)
      (coe
         MAlonzo.Code.Interface.Monad.du_traverseList_70 (coe v0) (coe ())
         (coe
            (\ v3 ->
               case coe v3 of
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                   -> coe
                        MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
                        (coe ())
                        (coe
                           (\ v6 ->
                              coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4) (coe v6)))
                        (coe d_introPatternM_452 v1 () erased v4 v5)
                 _ -> MAlonzo.RTE.mazUnreachableError))
         (coe v2))
-- Tactic.ClauseBuilder.ClauseExprM.multiMatchExprM
d_multiMatchExprM_632 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
d_multiMatchExprM_632 ~v0 v1 v2 v3
  = du_multiMatchExprM_632 v1 v2 v3
du_multiMatchExprM_632 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny
du_multiMatchExprM_632 v0 v1 v2
  = coe
      du_matchExprM_622 (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe du_helper_648 (coe v0) (coe v1)) (coe v2))
-- Tactic.ClauseBuilder.ClauseExprM._.helper'
d_helper''_638 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_helper''_638 ~v0 v1 v2 v3 = du_helper''_638 v1 v2 v3
du_helper''_638 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_helper''_638 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> case coe v3 of
             [] -> coe v4
             (:) v5 v6
               -> coe
                    MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
                    (coe ()) (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
                    (coe
                       du_matchExprM_622 (coe v0) (coe v1)
                       (coe
                          MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
                          (coe
                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v5)
                             (coe
                                du_helper''_638 (coe v0) (coe v1)
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v6)
                                   (coe v4))))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.ClauseExprM._.helper
d_helper_648 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_helper_648 ~v0 v1 v2 v3 = du_helper_648 v1 v2 v3
du_helper_648 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_helper_648 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> case coe v3 of
             MAlonzo.Code.Data.List.NonEmpty.Base.C__'8759'__34 v5 v6
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v5)
                    (coe
                       du_helper''_638 (coe v0) (coe v1)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v6) (coe v4)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.ClauseExprM.singleMatchExpr
d_singleMatchExpr_656 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_singleMatchExpr_656 ~v0 v1 v2 v3 v4
  = du_singleMatchExpr_656 v1 v2 v3 v4
du_singleMatchExpr_656 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
du_singleMatchExpr_656 v0 v1 v2 v3
  = coe
      du_matchExprM_622 (coe v0) (coe v1)
      (coe
         MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v3)))
-- Tactic.ClauseBuilder.ClauseExprM.singleTelescopeMatchExpr
d_singleTelescopeMatchExpr_662 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22 ->
  AgdaAny -> AgdaAny
d_singleTelescopeMatchExpr_662 ~v0 v1 v2 v3 v4
  = du_singleTelescopeMatchExpr_662 v1 v2 v3 v4
du_singleTelescopeMatchExpr_662 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22 ->
  AgdaAny -> AgdaAny
du_singleTelescopeMatchExpr_662 v0 v1 v2 v3
  = case coe v2 of
      MAlonzo.Code.Data.List.NonEmpty.Base.C__'8759'__34 v4 v5
        -> coe du_helper_674 (coe v0) (coe v1) (coe v4) (coe v5) (coe v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.ClauseExprM._.helper
d_helper_674 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny -> AgdaAny
d_helper_674 ~v0 v1 v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_helper_674 v1 v2 v6 v7 v8
du_helper_674 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> AgdaAny -> AgdaAny
du_helper_674 v0 v1 v2 v3 v4
  = case coe v3 of
      []
        -> coe du_singleMatchExpr_656 (coe v0) (coe v1) (coe v2) (coe v4)
      (:) v5 v6
        -> coe
             du_singleMatchExpr_656 (coe v0) (coe v1) (coe v2)
             (coe
                MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
                (coe ()) (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
                (coe du_helper_674 (coe v0) (coe v1) (coe v5) (coe v6) (coe v4)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.ClauseExprM.introExpr
d_introExpr_688 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny -> AgdaAny
d_introExpr_688 ~v0 v1 v2 v3 v4 = du_introExpr_688 v1 v2 v3 v4
du_introExpr_688 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 -> AgdaAny -> AgdaAny
du_introExpr_688 v0 v1 v2 v3
  = coe
      du_singleMatchExpr_656 (coe v0) (coe v1)
      (coe d_varSinglePattern_60 (coe v2)) (coe v3)
-- Tactic.ClauseBuilder.ClauseExprM.introsExpr
d_introsExpr_694 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22 ->
  AgdaAny -> AgdaAny
d_introsExpr_694 ~v0 v1 v2 v3 v4 = du_introsExpr_694 v1 v2 v3 v4
du_introsExpr_694 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22 ->
  AgdaAny -> AgdaAny
du_introsExpr_694 v0 v1 v2 v3
  = case coe v2 of
      MAlonzo.Code.Data.List.NonEmpty.Base.C__'8759'__34 v4 v5
        -> coe du_helper_706 (coe v0) (coe v1) (coe v4) (coe v5) (coe v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.ClauseExprM._.helper
d_helper_706 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> AgdaAny
d_helper_706 ~v0 v1 v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_helper_706 v1 v2 v6 v7 v8
du_helper_706 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  AgdaAny -> AgdaAny
du_helper_706 v0 v1 v2 v3 v4
  = case coe v3 of
      [] -> coe du_introExpr_688 (coe v0) (coe v1) (coe v2) (coe v4)
      (:) v5 v6
        -> coe
             du_introExpr_688 (coe v0) (coe v1) (coe v2)
             (coe
                MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
                (coe ()) (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
                (coe du_helper_706 (coe v0) (coe v1) (coe v2) (coe v6) (coe v4)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.ClauseExprM.contMatch
d_contMatch_720 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 -> AgdaAny -> AgdaAny
d_contMatch_720 ~v0 v1 ~v2 v3 = du_contMatch_720 v1 v3
du_contMatch_720 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 -> AgdaAny -> AgdaAny
du_contMatch_720 v0 v1
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
      (coe ()) (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38) (coe v1)
-- Tactic.ClauseBuilder.ClauseExprM.finishMatch
d_finishMatch_724 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 -> AgdaAny -> AgdaAny
d_finishMatch_724 ~v0 v1 ~v2 v3 = du_finishMatch_724 v1 v3
du_finishMatch_724 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 -> AgdaAny -> AgdaAny
du_finishMatch_724 v0 v1
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) (coe ())
      (coe ())
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
              (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v2))))
      (coe v1)
-- Tactic.ClauseBuilder.ClauseExprM.bindCtxMatchExpr
d_bindCtxMatchExpr_734 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 -> AgdaAny -> AgdaAny
d_bindCtxMatchExpr_734 ~v0 v1 v2 ~v3 v4 ~v5 v6
  = du_bindCtxMatchExpr_734 v1 v2 v4 v6
du_bindCtxMatchExpr_734 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  T_ContextMonad_444 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  AgdaAny -> AgdaAny
du_bindCtxMatchExpr_734 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 v0 () erased ()
      erased (coe du_ctxSinglePatterns_180 (coe v0) (coe v2))
      (\ v4 ->
         let v5
               = coe
                   MAlonzo.Code.Data.List.NonEmpty.Base.du_fromList_66 (coe v4) in
         case coe v5 of
           MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v6
             -> coe
                  du_singleTelescopeMatchExpr_662 (coe v0) (coe v1) (coe v6)
                  (coe du_contMatch_720 (coe v0) (coe v3))
           MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v3
           _ -> MAlonzo.RTE.mazUnreachableError)
-- Tactic.ClauseBuilder.clauseTelescope
d_clauseTelescope_744 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_clauseTelescope_744 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 v1 v2 v3
        -> coe v1
      MAlonzo.Code.Agda.Builtin.Reflection.C_absurd'45'clause_270 v1 v2
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder._._.bindCtxMatchExpr
d_bindCtxMatchExpr_756 ::
  MAlonzo.Code.Interface.MonadError.T_MonadError_46 ->
  MAlonzo.Code.Interface.MonadReader.T_MonadReader_50 ->
  MAlonzo.Code.Interface.MonadTC.T_MonadTC_104 ->
  T_ClauseExpr_300 -> T_ClauseExpr_300
d_bindCtxMatchExpr_756 v0 v1 v2 v3
  = coe
      du_bindCtxMatchExpr_734 (coe d_Monad'45'Id_466)
      (coe d_ContextMonad'45'Id_468) v1 v3
-- Tactic.ClauseBuilder._._.contMatch
d_contMatch_758 ::
  T_ClauseExpr_300 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_contMatch_758 = coe du_contMatch_720 (coe d_Monad'45'Id_466)
-- Tactic.ClauseBuilder._._.finishMatch
d_finishMatch_760 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_finishMatch_760 = coe du_finishMatch_724 (coe d_Monad'45'Id_466)
-- Tactic.ClauseBuilder._._.introExpr
d_introExpr_762 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> T_ClauseExpr_300
d_introExpr_762
  = coe
      du_introExpr_688 (coe d_Monad'45'Id_466)
      (coe d_ContextMonad'45'Id_468)
-- Tactic.ClauseBuilder._._.introsExpr
d_introsExpr_764 ::
  MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> T_ClauseExpr_300
d_introsExpr_764
  = coe
      du_introsExpr_694 (coe d_Monad'45'Id_466)
      (coe d_ContextMonad'45'Id_468)
-- Tactic.ClauseBuilder._._.matchExprM
d_matchExprM_766 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> T_ClauseExpr_300
d_matchExprM_766
  = coe
      du_matchExprM_622 (coe d_Monad'45'Id_466)
      (coe d_ContextMonad'45'Id_468)
-- Tactic.ClauseBuilder._._.multiMatchExprM
d_multiMatchExprM_768 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] -> T_ClauseExpr_300
d_multiMatchExprM_768
  = coe
      du_multiMatchExprM_632 (coe d_Monad'45'Id_466)
      (coe d_ContextMonad'45'Id_468)
-- Tactic.ClauseBuilder._._.singleMatchExpr
d_singleMatchExpr_770 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> T_ClauseExpr_300
d_singleMatchExpr_770
  = coe
      du_singleMatchExpr_656 (coe d_Monad'45'Id_466)
      (coe d_ContextMonad'45'Id_468)
-- Tactic.ClauseBuilder._._.singleTelescopeMatchExpr
d_singleTelescopeMatchExpr_772 ::
  MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> T_ClauseExpr_300
d_singleTelescopeMatchExpr_772
  = coe
      du_singleTelescopeMatchExpr_662 (coe d_Monad'45'Id_466)
      (coe d_ContextMonad'45'Id_468)
-- Tactic.ClauseBuilder._.instanciatePattern
d_instanciatePattern_774 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88]
d_instanciatePattern_774 v0
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62
      (coe MAlonzo.Code.Interface.Monad.Instance.d_Monad'45'List_34)
      (coe ()) (coe ())
      (coe (\ v1 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v1)))
      (coe
         d_clauseTelescope_744
         (coe
            MAlonzo.Code.Data.Maybe.Base.d_from'45'just_64
            (coe
               MAlonzo.Code.Data.List.Base.du_head_562
               (coe
                  d_clauseExprToClauses_422
                  (coe
                     du_singleMatchExpr_656 (coe d_Monad'45'Id_466)
                     (coe d_ContextMonad'45'Id_468) (coe v0)
                     (coe
                        du_finishMatch_724 (coe d_Monad'45'Id_466)
                        (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208)))))))
-- Tactic.ClauseBuilder._.instanciatePatterns
d_instanciatePatterns_778 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Clause_152]
d_instanciatePatterns_778 v0 v1
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Data.List.Base.du_'91'_'93'_306
             (coe
                MAlonzo.Code.Agda.Builtin.Reflection.C_clause_264 (coe v0) (coe v0)
                (coe v1))
      (:) v2 v3
        -> coe
             d_clauseExprToClauses_422
             (coe
                du_singleTelescopeMatchExpr_662 (coe d_Monad'45'Id_466)
                (coe d_ContextMonad'45'Id_468)
                (coe
                   MAlonzo.Code.Data.List.NonEmpty.Base.C__'8759'__34 (coe v2)
                   (coe v3))
                (coe du_finishMatch_724 (coe d_Monad'45'Id_466) (coe v1)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.ClauseBuilder.ctxBindingClause
d_ctxBindingClause_788 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Interface.MonadTC.T_TCEnv_22 -> AgdaAny
d_ctxBindingClause_788 v0 v1
  = coe
      MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
      MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
      (coe
         du_ctxSinglePatterns_180
         (coe
            MAlonzo.Code.Interface.MonadReader.du_Monad'45'ReaderT_118
            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
         (coe
            MAlonzo.Code.Interface.MonadReader.du_MonadReader'45'ReaderT_132
            (coe MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12))
         v1)
      (\ v2 ->
         coe
           MAlonzo.Code.Interface.Monad.d__'62''62''61'__30
           MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased () erased
           (coe
              MAlonzo.Code.Interface.Monad.d_return_28
              MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased
              (d_instanciatePatterns_778
                 (coe MAlonzo.Code.Data.List.Base.du_reverse_490 v2) (coe v0)))
           (\ v3 ->
              let v4
                    = coe
                        MAlonzo.Code.Interface.MonadTC.du_error1_664
                        (coe
                           MAlonzo.Code.Interface.MonadReader.du_MonadError'45'ReaderT_144
                           (coe MAlonzo.Code.Interface.MonadError.d_MonadError'45'TC_60))
                        (coe ())
                        (coe MAlonzo.Code.Reflection.Debug.d_IsErrorPart'45'String_22)
                        (coe ("Bug in ctxBindingClause" :: Data.Text.Text)) in
              case coe v3 of
                (:) v5 v6
                  -> coe
                       MAlonzo.Code.Interface.Monad.d_return_28
                       MAlonzo.Code.Interface.MonadTC.d_Monad'45'TC_12 () erased v5
                _ -> coe v4 v1))
