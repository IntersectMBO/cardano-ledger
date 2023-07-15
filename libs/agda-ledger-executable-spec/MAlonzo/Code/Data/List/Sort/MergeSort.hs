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

module MAlonzo.Code.Data.List.Sort.MergeSort where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional
import qualified MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.Properties
import qualified MAlonzo.Code.Data.List.Relation.Unary.All
import qualified MAlonzo.Code.Data.List.Relation.Unary.All.Properties
import qualified MAlonzo.Code.Data.List.Relation.Unary.Linked
import qualified MAlonzo.Code.Data.List.Relation.Unary.Sorted.TotalOrder.Properties
import qualified MAlonzo.Code.Data.List.Sort.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Induction.WellFounded
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Data.List.Sort.MergeSort._.SortingAlgorithm
d_SortingAlgorithm_98 a0 a1 a2 a3 = ()
-- Data.List.Sort.MergeSort._.Sorted
d_Sorted_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] -> ()
d_Sorted_110 = erased
-- Data.List.Sort.MergeSort.mergePairs
d_mergePairs_128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] -> [[AgdaAny]]
d_mergePairs_128 ~v0 ~v1 ~v2 v3 v4 = du_mergePairs_128 v3 v4
du_mergePairs_128 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] -> [[AgdaAny]]
du_mergePairs_128 v0 v1
  = case coe v1 of
      (:) v2 v3
        -> case coe v3 of
             (:) v4 v5
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Data.List.Base.du_merge_222
                       (coe
                          MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
                          (coe
                             MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
                             (coe v0)))
                       (coe v2) (coe v4))
                    (coe du_mergePairs_128 (coe v0) (coe v5))
             _ -> coe v1
      _ -> coe v1
-- Data.List.Sort.MergeSort.length-mergePairs
d_length'45'mergePairs_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  [AgdaAny] ->
  [[AgdaAny]] -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_length'45'mergePairs_144 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_length'45'mergePairs_144 v6
du_length'45'mergePairs_144 ::
  [[AgdaAny]] -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18
du_length'45'mergePairs_144 v0
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
             (coe
                MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22))
      (:) v1 v2
        -> case coe v2 of
             []
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe
                       MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                       (coe
                          MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                          (coe MAlonzo.Code.Data.Nat.Base.C_z'8804'n_22)))
             (:) v3 v4
               -> coe
                    MAlonzo.Code.Data.Nat.Base.C_s'8804's_30
                    (coe
                       MAlonzo.Code.Data.Nat.Properties.du_m'60'n'8658'm'60'1'43'n_2906
                       (coe du_length'45'mergePairs_144 (coe v4)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Sort.MergeSort.mergeAll
d_mergeAll_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 -> [AgdaAny]
d_mergeAll_156 ~v0 ~v1 ~v2 v3 v4 ~v5 = du_mergeAll_156 v3 v4
du_mergeAll_156 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] -> [AgdaAny]
du_mergeAll_156 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> case coe v3 of
             [] -> coe v2
             (:) v4 v5
               -> coe
                    du_mergeAll_156 (coe v0) (coe du_mergePairs_128 (coe v0) (coe v1))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Sort.MergeSort.sort
d_sort_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] -> [AgdaAny]
d_sort_168 ~v0 ~v1 ~v2 v3 v4 = du_sort_168 v3 v4
du_sort_168 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] -> [AgdaAny]
du_sort_168 v0 v1
  = coe
      du_mergeAll_156 (coe v0)
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306) (coe v1))
-- Data.List.Sort.MergeSort.mergePairs-↭
d_mergePairs'45''8621'_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_mergePairs'45''8621'_174 ~v0 ~v1 ~v2 v3 v4
  = du_mergePairs'45''8621'_174 v3 v4
du_mergePairs'45''8621'_174 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_mergePairs'45''8621'_174 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'refl_50
      (:) v2 v3
        -> case coe v3 of
             []
               -> coe
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'refl_50
             (:) v4 v5
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45''8621'_132
                       (coe
                          MAlonzo.Code.Data.List.Base.du__'43''43'__62
                          (coe
                             MAlonzo.Code.Data.List.Base.du_merge_222
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
                                   (coe v0)))
                             (coe v2) (coe v4))
                          (coe
                             MAlonzo.Code.Data.List.Base.du_concat_270
                             (coe du_mergePairs_128 (coe v0) (coe v5))))
                       (coe
                          MAlonzo.Code.Data.List.Base.du__'43''43'__62
                          (coe
                             MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v4))
                          (coe MAlonzo.Code.Data.List.Base.du_concat_270 v5))
                       (coe
                          MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
                          (coe
                             MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4)
                             (coe MAlonzo.Code.Data.List.Base.du_concat_270 v5)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                          (coe
                             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                             (coe
                                MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82))
                          (coe
                             MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2)
                             (coe
                                MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v4)
                                (coe MAlonzo.Code.Data.List.Base.du_concat_270 v5))))
                       (coe
                          MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.Properties.du_'43''43''8314'_380
                          (coe
                             MAlonzo.Code.Data.List.Base.du_merge_222
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
                                   (coe v0)))
                             (coe v2) (coe v4))
                          (coe
                             MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v2) (coe v4))
                          (coe
                             MAlonzo.Code.Data.List.Base.du_concat_270
                             (coe du_mergePairs_128 (coe v0) (coe v5)))
                          (coe
                             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.Properties.du_merge'45''8621'_828
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
                                   (coe v0)))
                             (coe v2) (coe v4))
                          (coe du_mergePairs'45''8621'_174 (coe v0) (coe v5))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Sort.MergeSort.mergeAll-↭
d_mergeAll'45''8621'_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_mergeAll'45''8621'_188 ~v0 ~v1 ~v2 v3 v4 ~v5
  = du_mergeAll'45''8621'_188 v3 v4
du_mergeAll'45''8621'_188 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_mergeAll'45''8621'_188 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'refl_50
      (:) v2 v3
        -> case coe v3 of
             []
               -> coe
                    MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'sym_56
                    (coe MAlonzo.Code.Data.List.Base.du_concat_270 v1)
                    (coe du_mergeAll_156 (coe v0) (coe v1))
                    (coe
                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.Properties.du_'43''43''45'identity'691'_718)
             (:) v4 v5
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45''8621'_132
                       (coe
                          du_mergeAll_156 (coe v0) (coe du_mergePairs_128 (coe v0) (coe v1)))
                       (coe
                          MAlonzo.Code.Data.List.Base.du_concat_270
                          (coe du_mergePairs_128 (coe v0) (coe v1)))
                       (coe MAlonzo.Code.Data.List.Base.du_concat_270 v1)
                       (coe
                          MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45''8621'_132
                          (coe
                             MAlonzo.Code.Data.List.Base.du_concat_270
                             (coe du_mergePairs_128 (coe v0) (coe v1)))
                          (coe MAlonzo.Code.Data.List.Base.du_concat_270 v1)
                          (coe MAlonzo.Code.Data.List.Base.du_concat_270 v1)
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82))
                             (coe MAlonzo.Code.Data.List.Base.du_concat_270 v1))
                          (coe du_mergePairs'45''8621'_174 (coe v0) (coe v1)))
                       (coe
                          du_mergeAll'45''8621'_188 (coe v0)
                          (coe du_mergePairs_128 (coe v0) (coe v1))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Sort.MergeSort.sort-↭
d_sort'45''8621'_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_sort'45''8621'_202 ~v0 ~v1 ~v2 v3 v4
  = du_sort'45''8621'_202 v3 v4
du_sort'45''8621'_202 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_sort'45''8621'_202 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_step'45''8621'_132
         (coe
            du_mergeAll_156 (coe v0)
            (coe
               MAlonzo.Code.Data.List.Base.du_map_22
               (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306) (coe v1)))
         (coe
            MAlonzo.Code.Data.List.Base.du_concat_270
            (coe
               MAlonzo.Code.Data.List.Base.du_map_22
               (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306) (coe v1)))
         v1
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_refl_34
               (coe
                  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.du_'8621''45'isEquivalence_82))
            (coe v1))
         (coe
            du_mergeAll'45''8621'_188 (coe v0)
            (coe
               MAlonzo.Code.Data.List.Base.du_map_22
               (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306) (coe v1))))
-- Data.List.Sort.MergeSort.mergePairs-↗
d_mergePairs'45''8599'_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
d_mergePairs'45''8599'_208 ~v0 ~v1 ~v2 v3 v4 v5
  = du_mergePairs'45''8599'_208 v3 v4 v5
du_mergePairs'45''8599'_208 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44
du_mergePairs'45''8599'_208 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.List.Relation.Unary.All.C_'91''93'_50 -> coe v2
      MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v5 v6
        -> case coe v1 of
             (:) v7 v8
               -> case coe v6 of
                    MAlonzo.Code.Data.List.Relation.Unary.All.C_'91''93'_50
                      -> coe
                           MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v5 v6
                    MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v11 v12
                      -> case coe v8 of
                           (:) v13 v14
                             -> coe
                                  MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60
                                  (coe
                                     MAlonzo.Code.Data.List.Relation.Unary.Sorted.TotalOrder.Properties.du_merge'8314'_716
                                     (coe v0) (coe v7) (coe v13) (coe v5) (coe v11))
                                  (coe du_mergePairs'45''8599'_208 (coe v0) (coe v14) (coe v12))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Sort.MergeSort.mergeAll-↗
d_mergeAll'45''8599'_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] ->
  MAlonzo.Code.Induction.WellFounded.T_Acc_42 ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_mergeAll'45''8599'_222 ~v0 ~v1 ~v2 v3 v4 ~v5 v6
  = du_mergeAll'45''8599'_222 v3 v4 v6
du_mergeAll'45''8599'_222 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [[AgdaAny]] ->
  MAlonzo.Code.Data.List.Relation.Unary.All.T_All_44 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_mergeAll'45''8599'_222 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.List.Relation.Unary.All.C_'91''93'_50
        -> coe MAlonzo.Code.Data.List.Relation.Unary.Linked.C_'91''93'_30
      MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v5 v6
        -> case coe v6 of
             MAlonzo.Code.Data.List.Relation.Unary.All.C_'91''93'_50 -> coe v5
             MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v9 v10
               -> coe
                    du_mergeAll'45''8599'_222 (coe v0)
                    (coe du_mergePairs_128 (coe v0) (coe v1))
                    (coe
                       du_mergePairs'45''8599'_208 (coe v0) (coe v1)
                       (coe
                          MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v5
                          (coe
                             MAlonzo.Code.Data.List.Relation.Unary.All.C__'8759'__60 v9 v10)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Sort.MergeSort.sort-↗
d_sort'45''8599'_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_sort'45''8599'_240 ~v0 ~v1 ~v2 v3 v4
  = du_sort'45''8599'_240 v3 v4
du_sort'45''8599'_240 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_sort'45''8599'_240 v0 v1
  = coe
      du_mergeAll'45''8599'_222 (coe v0)
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe
            (\ v2 ->
               coe
                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
                 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
         (coe v1))
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.All.Properties.du_map'8314'_672
         (coe v1)
         (coe
            MAlonzo.Code.Data.List.Relation.Unary.All.du_universal_524
            (coe
               (\ v2 ->
                  coe
                    MAlonzo.Code.Data.List.Relation.Unary.Linked.C_'91''45''93'_34))
            (coe v1)))
-- Data.List.Sort.MergeSort.mergeSort
d_mergeSort_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Data.List.Sort.Base.T_SortingAlgorithm_94
d_mergeSort_246 ~v0 ~v1 ~v2 v3 = du_mergeSort_246 v3
du_mergeSort_246 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Data.List.Sort.Base.T_SortingAlgorithm_94
du_mergeSort_246 v0
  = coe
      MAlonzo.Code.Data.List.Sort.Base.C_SortingAlgorithm'46'constructor_839
      (coe du_sort_168 (coe v0)) (coe du_sort'45''8621'_202 (coe v0))
      (coe du_sort'45''8599'_240 (coe v0))
