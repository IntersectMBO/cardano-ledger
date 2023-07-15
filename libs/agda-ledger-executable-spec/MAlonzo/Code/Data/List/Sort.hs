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

module MAlonzo.Code.Data.List.Sort where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional
import qualified MAlonzo.Code.Data.List.Relation.Unary.Linked
import qualified MAlonzo.Code.Data.List.Sort.Base
import qualified MAlonzo.Code.Data.List.Sort.MergeSort
import qualified MAlonzo.Code.Relation.Binary.Bundles

-- Data.List.Sort._.SortingAlgorithm
d_SortingAlgorithm_96 a0 a1 a2 a3 = ()
-- Data.List.Sort._.SortingAlgorithm.sort
d_sort_100 ::
  MAlonzo.Code.Data.List.Sort.Base.T_SortingAlgorithm_94 ->
  [AgdaAny] -> [AgdaAny]
d_sort_100 v0
  = coe MAlonzo.Code.Data.List.Sort.Base.d_sort_106 (coe v0)
-- Data.List.Sort._.SortingAlgorithm.sort-↗
d_sort'45''8599'_102 ::
  MAlonzo.Code.Data.List.Sort.Base.T_SortingAlgorithm_94 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_sort'45''8599'_102 v0
  = coe
      MAlonzo.Code.Data.List.Sort.Base.d_sort'45''8599'_114 (coe v0)
-- Data.List.Sort._.SortingAlgorithm.sort-↭
d_sort'45''8621'_104 ::
  MAlonzo.Code.Data.List.Sort.Base.T_SortingAlgorithm_94 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_sort'45''8621'_104 v0
  = coe
      MAlonzo.Code.Data.List.Sort.Base.d_sort'45''8621'_110 (coe v0)
-- Data.List.Sort.sortingAlgorithm
d_sortingAlgorithm_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Data.List.Sort.Base.T_SortingAlgorithm_94
d_sortingAlgorithm_110 ~v0 ~v1 ~v2 v3 = du_sortingAlgorithm_110 v3
du_sortingAlgorithm_110 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Data.List.Sort.Base.T_SortingAlgorithm_94
du_sortingAlgorithm_110 v0
  = coe
      MAlonzo.Code.Data.List.Sort.MergeSort.du_mergeSort_246 (coe v0)
-- Data.List.Sort._.sort
d_sort_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] -> [AgdaAny]
d_sort_114 ~v0 ~v1 ~v2 v3 = du_sort_114 v3
du_sort_114 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] -> [AgdaAny]
du_sort_114 v0
  = coe
      MAlonzo.Code.Data.List.Sort.Base.d_sort_106
      (coe du_sortingAlgorithm_110 (coe v0))
-- Data.List.Sort._.sort-↗
d_sort'45''8599'_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_sort'45''8599'_116 ~v0 ~v1 ~v2 v3 = du_sort'45''8599'_116 v3
du_sort'45''8599'_116 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_sort'45''8599'_116 v0
  = coe
      MAlonzo.Code.Data.List.Sort.Base.d_sort'45''8599'_114
      (coe du_sortingAlgorithm_110 (coe v0))
-- Data.List.Sort._.sort-↭
d_sort'45''8621'_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_sort'45''8621'_118 ~v0 ~v1 ~v2 v3 = du_sort'45''8621'_118 v3
du_sort'45''8621'_118 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_sort'45''8621'_118 v0
  = coe
      MAlonzo.Code.Data.List.Sort.Base.d_sort'45''8621'_110
      (coe du_sortingAlgorithm_110 (coe v0))
