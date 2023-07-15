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

module MAlonzo.Code.Data.List.Relation.Unary.Sorted.TotalOrder.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core
import qualified MAlonzo.Code.Data.List.Relation.Unary.Linked
import qualified MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties
import qualified MAlonzo.Code.Data.Maybe.Relation.Binary.Connected
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Properties.TotalOrder
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.AllPairs⇒Sorted
d_AllPairs'8658'Sorted_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_AllPairs'8658'Sorted_106 ~v0 ~v1 ~v2 ~v3 v4
  = du_AllPairs'8658'Sorted_106 v4
du_AllPairs'8658'Sorted_106 ::
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_AllPairs'8658'Sorted_106 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties.du_AllPairs'8658'Linked_36
      (coe v0)
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.Sorted⇒AllPairs
d_Sorted'8658'AllPairs_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
d_Sorted'8658'AllPairs_110 ~v0 ~v1 ~v2 v3 v4
  = du_Sorted'8658'AllPairs_110 v3 v4
du_Sorted'8658'AllPairs_110 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20
du_Sorted'8658'AllPairs_110 v0 v1
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties.du_Linked'8658'AllPairs_76
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_84
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                  (coe v0)))))
      (coe v1)
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.O₁._≤_
d__'8804'__130 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  AgdaAny -> AgdaAny -> ()
d__'8804'__130 = erased
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.map⁺
d_map'8314'_258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_map'8314'_258 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_map'8314'_258 v6 v7 v8
du_map'8314'_258 ::
  [AgdaAny] ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_map'8314'_258 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties.du_map'8314'_98
      (coe v0)
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.Linked.du_map_106 (coe v1)
         (coe v0) (coe v2))
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.map⁻
d_map'8315'_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_map'8315'_272 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_map'8315'_272 v6 v7 v8
du_map'8315'_272 ::
  [AgdaAny] ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_map'8315'_272 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Linked.du_map_106
      (coe (\ v3 v4 -> coe v1 v3 v4)) (coe v0)
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties.du_map'8315'_106
         (coe v0) (coe v2))
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.++⁺
d_'43''43''8314'_358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_'43''43''8314'_358 ~v0 ~v1 ~v2 ~v3 v4 ~v5
  = du_'43''43''8314'_358 v4
du_'43''43''8314'_358 ::
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_'43''43''8314'_358 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties.du_'43''43''8314'_134
      (coe v0)
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.applyUpTo⁺₁
d_applyUpTo'8314''8321'_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (Integer -> AgdaAny) ->
  Integer ->
  (Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_applyUpTo'8314''8321'_442 ~v0 ~v1 ~v2 ~v3
  = du_applyUpTo'8314''8321'_442
du_applyUpTo'8314''8321'_442 ::
  (Integer -> AgdaAny) ->
  Integer ->
  (Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_applyUpTo'8314''8321'_442 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties.du_applyUpTo'8314''8321'_170
      v1 v2
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.applyUpTo⁺₂
d_applyUpTo'8314''8322'_450 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (Integer -> AgdaAny) ->
  Integer ->
  (Integer -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_applyUpTo'8314''8322'_450 ~v0 ~v1 ~v2 ~v3
  = du_applyUpTo'8314''8322'_450
du_applyUpTo'8314''8322'_450 ::
  (Integer -> AgdaAny) ->
  Integer ->
  (Integer -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_applyUpTo'8314''8322'_450 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties.du_applyUpTo'8314''8322'_192
      v1 v2
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.applyDownFrom⁺₁
d_applyDownFrom'8314''8321'_534 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (Integer -> AgdaAny) ->
  Integer ->
  (Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_applyDownFrom'8314''8321'_534 ~v0 ~v1 ~v2 ~v3
  = du_applyDownFrom'8314''8321'_534
du_applyDownFrom'8314''8321'_534 ::
  (Integer -> AgdaAny) ->
  Integer ->
  (Integer -> MAlonzo.Code.Data.Nat.Base.T__'8804'__18 -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_applyDownFrom'8314''8321'_534 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties.du_applyDownFrom'8314''8321'_218
      v1 v2
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.applyDownFrom⁺₂
d_applyDownFrom'8314''8322'_542 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (Integer -> AgdaAny) ->
  Integer ->
  (Integer -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_applyDownFrom'8314''8322'_542 ~v0 ~v1 ~v2 ~v3
  = du_applyDownFrom'8314''8322'_542
du_applyDownFrom'8314''8322'_542 ::
  (Integer -> AgdaAny) ->
  Integer ->
  (Integer -> AgdaAny) ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_applyDownFrom'8314''8322'_542 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties.du_applyDownFrom'8314''8322'_240
      v1 v2
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._._.totalOrder
d_totalOrder_568 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
d_totalOrder_568 ~v0 ~v1 ~v2 v3 = du_totalOrder_568 v3
du_totalOrder_568 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648
du_totalOrder_568 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812 (coe v0)
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.merge-con
d_merge'45'con_646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42 ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42 ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42
d_merge'45'con_646 ~v0 ~v1 ~v2 v3 ~v4 v5 v6 v7 v8
  = du_merge'45'con_646 v3 v5 v6 v7 v8
du_merge'45'con_646 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42 ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42 ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42
du_merge'45'con_646 v0 v1 v2 v3 v4
  = case coe v1 of
      [] -> coe seq (coe v2) (coe v4)
      (:) v5 v6
        -> case coe v2 of
             [] -> coe v3
             (:) v7 v8
               -> let v9
                        = coe
                            MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
                            (MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
                               (coe v0))
                            v5 v7 in
                  case coe v9 of
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v10 v11
                      -> if coe v10
                           then coe seq (coe v11) (coe v3)
                           else coe seq (coe v11) (coe v4)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.merge⁺
d_merge'8314'_716 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_merge'8314'_716 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7
  = du_merge'8314'_716 v3 v4 v5 v6 v7
du_merge'8314'_716 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecTotalOrder_736 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_merge'8314'_716 v0 v1 v2 v3 v4
  = case coe v1 of
      []
        -> case coe v2 of
             []
               -> coe MAlonzo.Code.Data.List.Relation.Unary.Linked.C_'91''93'_30
             (:) v5 v6 -> coe v4
             _ -> MAlonzo.RTE.mazUnreachableError
      (:) v5 v6
        -> case coe v2 of
             [] -> coe v3
             (:) v7 v8
               -> let v9
                        = coe
                            MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
                            (MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
                               (coe v0))
                            v5 v7 in
                  let v10
                        = coe
                            du_merge'8314'_716 (coe v0) (coe v6) (coe v2)
                            (coe
                               MAlonzo.Code.Data.List.Relation.Unary.Linked.du_tail_68 (coe v3))
                            (coe v4) in
                  let v11
                        = coe
                            du_merge'8314'_716 (coe v0) (coe v1) (coe v8) (coe v3)
                            (coe
                               MAlonzo.Code.Data.List.Relation.Unary.Linked.du_tail_68
                               (coe v4)) in
                  case coe v9 of
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v12 v13
                      -> if coe v12
                           then case coe v13 of
                                  MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v14
                                    -> coe
                                         MAlonzo.Code.Data.List.Relation.Unary.Linked.du__'8759''8242'__84
                                         (coe
                                            MAlonzo.Code.Data.List.Base.du_merge_222
                                            (coe
                                               MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
                                               (coe
                                                  MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
                                                  (coe v0)))
                                            (coe v6) (coe v2))
                                         (coe
                                            du_merge'45'con_646 (coe v0) (coe v6) (coe v2)
                                            (coe
                                               MAlonzo.Code.Data.List.Relation.Unary.Linked.du_head'8242'_76
                                               (coe v3))
                                            (coe
                                               MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.C_just_50
                                               v14))
                                         (coe v10)
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           else coe
                                  seq (coe v13)
                                  (coe
                                     MAlonzo.Code.Data.List.Relation.Unary.Linked.du__'8759''8242'__84
                                     (coe
                                        MAlonzo.Code.Data.List.Base.du_merge_222
                                        (coe
                                           MAlonzo.Code.Relation.Binary.Structures.d__'8804''63'__444
                                           (coe
                                              MAlonzo.Code.Relation.Binary.Bundles.d_isDecTotalOrder_758
                                              (coe v0)))
                                        (coe v1) (coe v8))
                                     (coe
                                        du_merge'45'con_646 (coe v0) (coe v1) (coe v8)
                                        (coe
                                           MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.C_just_50
                                           (coe
                                              MAlonzo.Code.Relation.Binary.Properties.TotalOrder.du_'8816''8658''8805'_208
                                              (coe
                                                 MAlonzo.Code.Relation.Binary.Bundles.du_totalOrder_812
                                                 (coe v0))
                                              (coe v5) (coe v7)))
                                        (coe
                                           MAlonzo.Code.Data.List.Relation.Unary.Linked.du_head'8242'_76
                                           (coe v4)))
                                     (coe v11))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Unary.Sorted.TotalOrder.Properties._.filter⁺
d_filter'8314'_870 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
d_filter'8314'_870 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7
  = du_filter'8314'_870 v4 v6 v7
du_filter'8314'_870 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_TotalOrder_648 ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Linked.T_Linked_26
du_filter'8314'_870 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Linked.Properties.du_filter'8314'_336
      (coe v1)
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_84
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_isPreorder_170
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_isPartialOrder_388
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.d_isTotalOrder_670
                  (coe v0)))))
      (coe v2)
