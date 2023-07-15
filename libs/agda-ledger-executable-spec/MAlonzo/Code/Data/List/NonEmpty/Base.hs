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

module MAlonzo.Code.Data.List.NonEmpty.Base where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.These.Base
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.List.NonEmpty.Base.List⁺
d_List'8314'_22 a0 a1 = ()
data T_List'8314'_22 = C__'8759'__34 AgdaAny [AgdaAny]
-- Data.List.NonEmpty.Base.List⁺.head
d_head_30 :: T_List'8314'_22 -> AgdaAny
d_head_30 v0
  = case coe v0 of
      C__'8759'__34 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.List⁺.tail
d_tail_32 :: T_List'8314'_22 -> [AgdaAny]
d_tail_32 v0
  = case coe v0 of
      C__'8759'__34 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.uncons
d_uncons_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_uncons_36 ~v0 ~v1 v2 = du_uncons_36 v2
du_uncons_36 ::
  T_List'8314'_22 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_uncons_36 v0
  = case coe v0 of
      C__'8759'__34 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.[_]
d_'91'_'93'_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> T_List'8314'_22
d_'91'_'93'_42 ~v0 ~v1 v2 = du_'91'_'93'_42 v2
du_'91'_'93'_42 :: AgdaAny -> T_List'8314'_22
du_'91'_'93'_42 v0
  = coe
      C__'8759'__34 (coe v0)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Data.List.NonEmpty.Base._∷⁺_
d__'8759''8314'__46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> T_List'8314'_22 -> T_List'8314'_22
d__'8759''8314'__46 ~v0 ~v1 v2 v3 = du__'8759''8314'__46 v2 v3
du__'8759''8314'__46 ::
  AgdaAny -> T_List'8314'_22 -> T_List'8314'_22
du__'8759''8314'__46 v0 v1
  = case coe v1 of
      C__'8759'__34 v2 v3
        -> coe
             C__'8759'__34 (coe v0)
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.length
d_length_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> Integer
d_length_54 ~v0 ~v1 v2 = du_length_54 v2
du_length_54 :: T_List'8314'_22 -> Integer
du_length_54 v0
  = case coe v0 of
      C__'8759'__34 v1 v2
        -> coe
             addInt (coe (1 :: Integer))
             (coe MAlonzo.Code.Data.List.Base.du_length_304 v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.toList
d_toList_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> [AgdaAny]
d_toList_60 ~v0 ~v1 v2 = du_toList_60 v2
du_toList_60 :: T_List'8314'_22 -> [AgdaAny]
du_toList_60 v0
  = case coe v0 of
      C__'8759'__34 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.fromList
d_fromList_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> Maybe T_List'8314'_22
d_fromList_66 ~v0 ~v1 v2 = du_fromList_66 v2
du_fromList_66 :: [AgdaAny] -> Maybe T_List'8314'_22
du_fromList_66 v0
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      (:) v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
             (coe C__'8759'__34 (coe v1) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.fromVec
d_fromVec_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> T_List'8314'_22
d_fromVec_74 ~v0 ~v1 ~v2 v3 = du_fromVec_74 v3
du_fromVec_74 ::
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> T_List'8314'_22
du_fromVec_74 v0
  = case coe v0 of
      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v2 v3
        -> coe
             C__'8759'__34 (coe v2)
             (coe MAlonzo.Code.Data.Vec.Base.du_toList_630 (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.toVec
d_toVec_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_toVec_82 ~v0 ~v1 v2 = du_toVec_82 v2
du_toVec_82 ::
  T_List'8314'_22 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28
du_toVec_82 v0
  = case coe v0 of
      C__'8759'__34 v1 v2
        -> coe
             MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v1
             (coe MAlonzo.Code.Data.Vec.Base.du_fromList_638 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.lift
d_lift_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (Integer ->
   MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  T_List'8314'_22 -> T_List'8314'_22
d_lift_92 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_lift_92 v4 v5
du_lift_92 ::
  (Integer ->
   MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  T_List'8314'_22 -> T_List'8314'_22
du_lift_92 v0 v1
  = coe
      du_fromVec_74
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
         (coe
            v0
            (coe
               MAlonzo.Code.Data.List.Base.du_foldr_242
               (let v2 = \ v2 -> addInt (coe (1 :: Integer)) (coe v2) in
                coe (\ v3 -> v2))
               (coe (0 :: Integer)) (coe d_tail_32 (coe v1)))
            (coe du_toVec_82 (coe v1))))
-- Data.List.NonEmpty.Base.map
d_map_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> T_List'8314'_22 -> T_List'8314'_22
d_map_98 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_map_98 v4 v5
du_map_98 ::
  (AgdaAny -> AgdaAny) -> T_List'8314'_22 -> T_List'8314'_22
du_map_98 v0 v1
  = case coe v1 of
      C__'8759'__34 v2 v3
        -> coe
             C__'8759'__34 (coe v0 v2)
             (coe MAlonzo.Code.Data.List.Base.du_map_22 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.replicate
d_replicate_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny -> T_List'8314'_22
d_replicate_108 ~v0 ~v1 v2 ~v3 v4 = du_replicate_108 v2 v4
du_replicate_108 :: Integer -> AgdaAny -> T_List'8314'_22
du_replicate_108 v0 v1
  = coe
      C__'8759'__34 (coe v1)
      (coe
         MAlonzo.Code.Data.List.Base.du_replicate_314
         (coe MAlonzo.Code.Data.Nat.Base.d_pred_126 (coe v0)) (coe v1))
-- Data.List.NonEmpty.Base.drop+
d_drop'43'_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> T_List'8314'_22 -> T_List'8314'_22
d_drop'43'_116 ~v0 ~v1 v2 v3 = du_drop'43'_116 v2 v3
du_drop'43'_116 :: Integer -> T_List'8314'_22 -> T_List'8314'_22
du_drop'43'_116 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             C__'8759'__34 v3 v4
               -> case coe v4 of
                    [] -> coe v1
                    (:) v5 v6
                      -> coe
                           du_drop'43'_116 (coe v2) (coe C__'8759'__34 (coe v5) (coe v6))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.foldr
d_foldr_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_List'8314'_22 -> AgdaAny
d_foldr_132 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_foldr_132 v4 v5 v6
du_foldr_132 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_List'8314'_22 -> AgdaAny
du_foldr_132 v0 v1 v2
  = case coe v2 of
      C__'8759'__34 v3 v4
        -> coe du_foldr'8242'_150 (coe v0) (coe v1) (coe v3) (coe v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base._.foldr′
d_foldr'8242'_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> AgdaAny -> [AgdaAny] -> AgdaAny
d_foldr'8242'_150 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 ~v7 v8 v9
  = du_foldr'8242'_150 v4 v5 v8 v9
du_foldr'8242'_150 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
du_foldr'8242'_150 v0 v1 v2 v3
  = case coe v3 of
      [] -> coe v1 v2
      (:) v4 v5
        -> coe
             v0 v2 (coe du_foldr'8242'_150 (coe v0) (coe v1) (coe v4) (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.foldr₁
d_foldr'8321'_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> AgdaAny) -> T_List'8314'_22 -> AgdaAny
d_foldr'8321'_160 ~v0 ~v1 v2 = du_foldr'8321'_160 v2
du_foldr'8321'_160 ::
  (AgdaAny -> AgdaAny -> AgdaAny) -> T_List'8314'_22 -> AgdaAny
du_foldr'8321'_160 v0
  = coe du_foldr_132 (coe v0) (coe (\ v1 -> v1))
-- Data.List.NonEmpty.Base.foldl
d_foldl_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_List'8314'_22 -> AgdaAny
d_foldl_164 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_foldl_164 v4 v5 v6
du_foldl_164 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_List'8314'_22 -> AgdaAny
du_foldl_164 v0 v1 v2
  = case coe v2 of
      C__'8759'__34 v3 v4
        -> coe
             MAlonzo.Code.Data.List.Base.du_foldl_256 (coe v0) (coe v1 v3)
             (coe v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.foldl₁
d_foldl'8321'_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> AgdaAny) -> T_List'8314'_22 -> AgdaAny
d_foldl'8321'_174 ~v0 ~v1 v2 = du_foldl'8321'_174 v2
du_foldl'8321'_174 ::
  (AgdaAny -> AgdaAny -> AgdaAny) -> T_List'8314'_22 -> AgdaAny
du_foldl'8321'_174 v0
  = coe du_foldl_164 (coe v0) (coe (\ v1 -> v1))
-- Data.List.NonEmpty.Base._⁺++⁺_
d__'8314''43''43''8314'__178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
d__'8314''43''43''8314'__178 ~v0 ~v1 v2 v3
  = du__'8314''43''43''8314'__178 v2 v3
du__'8314''43''43''8314'__178 ::
  T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
du__'8314''43''43''8314'__178 v0 v1
  = case coe v0 of
      C__'8759'__34 v2 v3
        -> case coe v1 of
             C__'8759'__34 v4 v5
               -> coe
                    C__'8759'__34 (coe v2)
                    (coe
                       MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v3)
                       (coe
                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v4) (coe v5)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base._⁺++_
d__'8314''43''43'__188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> [AgdaAny] -> T_List'8314'_22
d__'8314''43''43'__188 ~v0 ~v1 v2 v3
  = du__'8314''43''43'__188 v2 v3
du__'8314''43''43'__188 ::
  T_List'8314'_22 -> [AgdaAny] -> T_List'8314'_22
du__'8314''43''43'__188 v0 v1
  = case coe v0 of
      C__'8759'__34 v2 v3
        -> coe
             C__'8759'__34 (coe v2)
             (coe
                MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base._++⁺_
d__'43''43''8314'__196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> T_List'8314'_22 -> T_List'8314'_22
d__'43''43''8314'__196 ~v0 ~v1 v2 v3
  = du__'43''43''8314'__196 v2 v3
du__'43''43''8314'__196 ::
  [AgdaAny] -> T_List'8314'_22 -> T_List'8314'_22
du__'43''43''8314'__196 v0 v1
  = coe
      MAlonzo.Code.Data.List.Base.du_foldr_242 (coe du__'8759''8314'__46)
      (coe v1) (coe v0)
-- Data.List.NonEmpty.Base.concat
d_concat_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> T_List'8314'_22
d_concat_202 ~v0 ~v1 v2 = du_concat_202 v2
du_concat_202 :: T_List'8314'_22 -> T_List'8314'_22
du_concat_202 v0
  = case coe v0 of
      C__'8759'__34 v1 v2
        -> coe
             du__'8314''43''43'__188 (coe v1)
             (coe
                MAlonzo.Code.Data.List.Base.du_concat_270
                (coe
                   MAlonzo.Code.Data.List.Base.du_map_22 (coe du_toList_60) (coe v2)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.concatMap
d_concatMap_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> T_List'8314'_22) -> T_List'8314'_22 -> T_List'8314'_22
d_concatMap_208 ~v0 ~v1 ~v2 ~v3 v4 = du_concatMap_208 v4
du_concatMap_208 ::
  (AgdaAny -> T_List'8314'_22) -> T_List'8314'_22 -> T_List'8314'_22
du_concatMap_208 v0
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe du_concat_202) (coe du_map_98 (coe v0))
-- Data.List.NonEmpty.Base.ap
d_ap_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
d_ap_212 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_ap_212 v4 v5
du_ap_212 :: T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
du_ap_212 v0 v1
  = coe du_concatMap_208 (\ v2 -> coe du_map_98 (coe v2) (coe v1)) v0
-- Data.List.NonEmpty.Base.reverse
d_reverse_220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> T_List'8314'_22
d_reverse_220 ~v0 ~v1 = du_reverse_220
du_reverse_220 :: T_List'8314'_22 -> T_List'8314'_22
du_reverse_220
  = coe
      du_lift_92
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Function.Base.du__'8728''8242'__216
              (coe MAlonzo.Code.Data.Product.Base.du_'45''44'__68 (coe v0))
              (coe MAlonzo.Code.Data.Vec.Base.du_reverse_654)))
-- Data.List.NonEmpty.Base.alignWith
d_alignWith_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
d_alignWith_222 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_alignWith_222 v6 v7 v8
du_alignWith_222 ::
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
du_alignWith_222 v0 v1 v2
  = case coe v1 of
      C__'8759'__34 v3 v4
        -> case coe v2 of
             C__'8759'__34 v5 v6
               -> coe
                    C__'8759'__34
                    (coe
                       v0 (coe MAlonzo.Code.Data.These.Base.C_these_52 (coe v3) (coe v5)))
                    (coe
                       MAlonzo.Code.Data.List.Base.du_alignWith_114 (coe v0) (coe v4)
                       (coe v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.zipWith
d_zipWith_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
d_zipWith_234 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_zipWith_234 v6 v7 v8
du_zipWith_234 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
du_zipWith_234 v0 v1 v2
  = case coe v1 of
      C__'8759'__34 v3 v4
        -> case coe v2 of
             C__'8759'__34 v5 v6
               -> coe
                    C__'8759'__34 (coe v0 v3 v5)
                    (coe
                       MAlonzo.Code.Data.List.Base.du_zipWith_134 (coe v0) (coe v4)
                       (coe v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.unalignWith
d_unalignWith_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38) ->
  T_List'8314'_22 -> MAlonzo.Code.Data.These.Base.T_These_38
d_unalignWith_246 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_unalignWith_246 v6
du_unalignWith_246 ::
  (AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38) ->
  T_List'8314'_22 -> MAlonzo.Code.Data.These.Base.T_These_38
du_unalignWith_246 v0
  = coe
      du_foldr_132
      (coe
         MAlonzo.Code.Function.Base.du__'8728''8242'__216
         (coe
            MAlonzo.Code.Data.These.Base.du_alignWith_130 (coe du_mcons_258)
            (coe du_mcons_258))
         (coe v0))
      (coe
         MAlonzo.Code.Function.Base.du__'8728''8242'__216
         (coe
            MAlonzo.Code.Data.These.Base.du_map_60 (coe du_'91'_'93'_42)
            (coe du_'91'_'93'_42))
         (coe v0))
-- Data.List.NonEmpty.Base._.mcons
d_mcons_258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Data.These.Base.T_These_38 -> T_List'8314'_22
d_mcons_258 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 = du_mcons_258
du_mcons_258 ::
  MAlonzo.Code.Data.These.Base.T_These_38 -> T_List'8314'_22
du_mcons_258
  = coe
      MAlonzo.Code.Data.These.Base.du_fold_92 (coe du_'91'_'93'_42)
      (coe (\ v0 -> v0)) (coe du__'8759''8314'__46)
-- Data.List.NonEmpty.Base.unzipWith
d_unzipWith_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  T_List'8314'_22 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unzipWith_260 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_unzipWith_260 v6 v7
du_unzipWith_260 ::
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  T_List'8314'_22 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unzipWith_260 v0 v1
  = case coe v1 of
      C__'8759'__34 v2 v3
        -> coe
             MAlonzo.Code.Data.Product.Base.du_zip_174 (coe C__'8759'__34)
             (coe (\ v4 v5 -> coe C__'8759'__34)) (coe v0 v2)
             (coe
                MAlonzo.Code.Data.List.Base.du_unzipWith_196 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.align
d_align_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
d_align_268 ~v0 ~v1 ~v2 ~v3 = du_align_268
du_align_268 ::
  T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
du_align_268 = coe du_alignWith_222 (coe (\ v0 -> v0))
-- Data.List.NonEmpty.Base.zip
d_zip_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
d_zip_270 ~v0 ~v1 ~v2 ~v3 = du_zip_270
du_zip_270 :: T_List'8314'_22 -> T_List'8314'_22 -> T_List'8314'_22
du_zip_270
  = coe
      du_zipWith_234 (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32)
-- Data.List.NonEmpty.Base.unalign
d_unalign_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> MAlonzo.Code.Data.These.Base.T_These_38
d_unalign_272 ~v0 ~v1 ~v2 ~v3 = du_unalign_272
du_unalign_272 ::
  T_List'8314'_22 -> MAlonzo.Code.Data.These.Base.T_These_38
du_unalign_272 = coe du_unalignWith_246 (coe (\ v0 -> v0))
-- Data.List.NonEmpty.Base.unzip
d_unzip_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unzip_274 ~v0 ~v1 ~v2 ~v3 = du_unzip_274
du_unzip_274 ::
  T_List'8314'_22 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unzip_274 = coe du_unzipWith_260 (coe (\ v0 -> v0))
-- Data.List.NonEmpty.Base._∷ʳ_
d__'8759''691'__276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> AgdaAny -> T_List'8314'_22
d__'8759''691'__276 ~v0 ~v1 v2 v3 = du__'8759''691'__276 v2 v3
du__'8759''691'__276 :: [AgdaAny] -> AgdaAny -> T_List'8314'_22
du__'8759''691'__276 v0 v1
  = case coe v0 of
      [] -> coe du_'91'_'93'_42 (coe v1)
      (:) v2 v3
        -> coe
             C__'8759'__34 (coe v2)
             (coe
                MAlonzo.Code.Data.List.Base.du__'8759''691'__494 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base._⁺∷ʳ_
d__'8314''8759''691'__286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> AgdaAny -> T_List'8314'_22
d__'8314''8759''691'__286 ~v0 ~v1 v2 v3
  = du__'8314''8759''691'__286 v2 v3
du__'8314''8759''691'__286 ::
  T_List'8314'_22 -> AgdaAny -> T_List'8314'_22
du__'8314''8759''691'__286 v0 v1
  = coe du__'8759''691'__276 (coe du_toList_60 (coe v0)) (coe v1)
-- Data.List.NonEmpty.Base.SnocView
d_SnocView_296 a0 a1 a2 = ()
data T_SnocView_296 = C__'8759''691''8242'__304 [AgdaAny] AgdaAny
-- Data.List.NonEmpty.Base.snocView
d_snocView_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> T_SnocView_296
d_snocView_308 ~v0 ~v1 v2 = du_snocView_308 v2
du_snocView_308 :: T_List'8314'_22 -> T_SnocView_296
du_snocView_308 v0
  = case coe v0 of
      C__'8759'__34 v1 v2
        -> let v3
                 = coe MAlonzo.Code.Data.List.Base.du_initLast_518 (coe v2) in
           case coe v3 of
             MAlonzo.Code.Data.List.Base.C_'91''93'_508
               -> coe
                    C__'8759''691''8242'__304
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v1)
             MAlonzo.Code.Data.List.Base.C__'8759''691''8242'__514 v4 v5
               -> coe
                    C__'8759''691''8242'__304
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v4))
                    (coe v5)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.last
d_last_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_List'8314'_22 -> AgdaAny
d_last_326 ~v0 ~v1 v2 = du_last_326 v2
du_last_326 :: T_List'8314'_22 -> AgdaAny
du_last_326 v0
  = let v1 = coe du_snocView_308 (coe v0) in
    case coe v1 of
      C__'8759''691''8242'__304 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.groupSeqsᵇ
d_groupSeqs'7495'_338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> Bool) ->
  [AgdaAny] -> [MAlonzo.Code.Data.Sum.Base.T__'8846'__30]
d_groupSeqs'7495'_338 ~v0 ~v1 v2 v3 = du_groupSeqs'7495'_338 v2 v3
du_groupSeqs'7495'_338 ::
  (AgdaAny -> Bool) ->
  [AgdaAny] -> [MAlonzo.Code.Data.Sum.Base.T__'8846'__30]
du_groupSeqs'7495'_338 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> let v4 = coe v0 v2 in
           let v5 = coe du_groupSeqs'7495'_338 (coe v0) (coe v3) in
           if coe v4
             then let v6
                        = coe
                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                            (coe
                               MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
                               (coe du_'91'_'93'_42 (coe v2)))
                            (coe v5) in
                  case coe v5 of
                    (:) v7 v8
                      -> case coe v7 of
                           MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v9
                             -> coe
                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                  (coe
                                     MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
                                     (coe du__'8759''8314'__46 (coe v2) (coe v9)))
                                  (coe v8)
                           _ -> coe v6
                    _ -> coe v6
             else (let v6
                         = coe
                             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                             (coe
                                MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                                (coe du_'91'_'93'_42 (coe v2)))
                             (coe v5) in
                   case coe v5 of
                     (:) v7 v8
                       -> case coe v7 of
                            MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v9
                              -> coe
                                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                   (coe
                                      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                                      (coe du__'8759''8314'__46 (coe v2) (coe v9)))
                                   (coe v8)
                            _ -> coe v6
                     _ -> coe v6)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.NonEmpty.Base.wordsByᵇ
d_wordsBy'7495'_388 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Bool) -> [AgdaAny] -> [T_List'8314'_22]
d_wordsBy'7495'_388 ~v0 ~v1 v2 v3 = du_wordsBy'7495'_388 v2 v3
du_wordsBy'7495'_388 ::
  (AgdaAny -> Bool) -> [AgdaAny] -> [T_List'8314'_22]
du_wordsBy'7495'_388 v0 v1
  = coe
      MAlonzo.Code.Data.List.Base.du_mapMaybe_32
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52
         (let v2 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
          coe (\ v3 -> v2))
         (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16))
      (coe du_groupSeqs'7495'_338 (coe v0) (coe v1))
-- Data.List.NonEmpty.Base.groupSeqs
d_groupSeqs_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [MAlonzo.Code.Data.Sum.Base.T__'8846'__30]
d_groupSeqs_394 ~v0 ~v1 ~v2 ~v3 v4 = du_groupSeqs_394 v4
du_groupSeqs_394 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [MAlonzo.Code.Data.Sum.Base.T__'8846'__30]
du_groupSeqs_394 v0
  = coe
      du_groupSeqs'7495'_338
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.List.NonEmpty.Base.wordsBy
d_wordsBy_400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [T_List'8314'_22]
d_wordsBy_400 ~v0 ~v1 ~v2 ~v3 v4 = du_wordsBy_400 v4
du_wordsBy_400 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [T_List'8314'_22]
du_wordsBy_400 v0
  = coe
      du_wordsBy'7495'_388
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.List.NonEmpty.Base.ungroupSeqs
d_ungroupSeqs_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [MAlonzo.Code.Data.Sum.Base.T__'8846'__30] -> [AgdaAny]
d_ungroupSeqs_404 ~v0 ~v1 v2 = du_ungroupSeqs_404 v2
du_ungroupSeqs_404 ::
  [MAlonzo.Code.Data.Sum.Base.T__'8846'__30] -> [AgdaAny]
du_ungroupSeqs_404 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_concat_270
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe
            MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52 (coe du_toList_60)
            (coe du_toList_60))
         (coe v0))
