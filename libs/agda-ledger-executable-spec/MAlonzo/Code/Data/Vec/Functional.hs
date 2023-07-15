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

module MAlonzo.Code.Data.Vec.Functional where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.Vec.Base

-- Data.Vec.Functional.Vector
d_Vector_18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> Integer -> ()
d_Vector_18 = erased
-- Data.Vec.Functional.toVec
d_toVec_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_toVec_26 ~v0 ~v1 v2 = du_toVec_26 v2
du_toVec_26 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28
du_toVec_26 v0
  = coe MAlonzo.Code.Data.Vec.Base.du_tabulate_454 (coe v0)
-- Data.Vec.Functional.fromVec
d_fromVec_30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_fromVec_30 ~v0 ~v1 ~v2 = du_fromVec_30
du_fromVec_30 ::
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_fromVec_30 = coe MAlonzo.Code.Data.Vec.Base.du_lookup_82
-- Data.Vec.Functional.toList
d_toList_34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> [AgdaAny]
d_toList_34 ~v0 ~v1 v2 = du_toList_34 v2
du_toList_34 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> [AgdaAny]
du_toList_34 v0
  = coe MAlonzo.Code.Data.List.Base.du_tabulate_408 (coe v0)
-- Data.Vec.Functional.fromList
d_fromList_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_fromList_38 ~v0 ~v1 = du_fromList_38
du_fromList_38 ::
  [AgdaAny] -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_fromList_38 = coe MAlonzo.Code.Data.List.Base.du_lookup_418
-- Data.Vec.Functional.[]
d_'91''93'_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_'91''93'_40 ~v0 ~v1 ~v2 = du_'91''93'_40
du_'91''93'_40 :: AgdaAny
du_'91''93'_40 = MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Functional._∷_
d__'8759'__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  AgdaAny ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d__'8759'__44 ~v0 ~v1 ~v2 v3 v4 v5 = du__'8759'__44 v3 v4 v5
du__'8759'__44 ::
  AgdaAny ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du__'8759'__44 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12 -> coe v0
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v4 -> coe v1 v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Functional.length
d_length_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> Integer
d_length_58 ~v0 ~v1 v2 ~v3 = du_length_58 v2
du_length_58 :: Integer -> Integer
du_length_58 v0 = coe v0
-- Data.Vec.Functional.head
d_head_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
d_head_64 ~v0 ~v1 ~v2 v3 = du_head_64 v3
du_head_64 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
du_head_64 v0 = coe v0 (coe MAlonzo.Code.Data.Fin.Base.C_zero_12)
-- Data.Vec.Functional.tail
d_tail_70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_tail_70 ~v0 ~v1 ~v2 v3 v4 = du_tail_70 v3 v4
du_tail_70 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_tail_70 v0 v1
  = coe v0 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v1)
-- Data.Vec.Functional.uncons
d_uncons_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_uncons_76 ~v0 ~v1 ~v2 v3 = du_uncons_76 v3
du_uncons_76 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_uncons_76 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_head_64 (coe v0)) (coe du_tail_70 (coe v0))
-- Data.Vec.Functional.replicate
d_replicate_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_replicate_82 ~v0 ~v1 ~v2 v3 ~v4 = du_replicate_82 v3
du_replicate_82 :: AgdaAny -> AgdaAny
du_replicate_82 v0 = coe v0
-- Data.Vec.Functional.insert
d_insert_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  AgdaAny -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_insert_86 ~v0 ~v1 ~v2 v3 v4 v5 v6 = du_insert_86 v3 v4 v5 v6
du_insert_86 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  AgdaAny -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_insert_86 v0 v1 v2 v3
  = case coe v1 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> case coe v3 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12 -> coe v2
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v6 -> coe v0 v6
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v5
        -> case coe v3 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12 -> coe du_head_64 (coe v0)
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v7
               -> coe
                    du_insert_86 (coe du_tail_70 (coe v0)) (coe v5) (coe v2) (coe v7)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Functional.remove
d_remove_122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_remove_122 ~v0 ~v1 ~v2 v3 v4 v5 = du_remove_122 v3 v4 v5
du_remove_122 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_remove_122 v0 v1 v2
  = coe
      v1
      (coe MAlonzo.Code.Data.Fin.Base.du_punchIn_426 (coe v0) (coe v2))
-- Data.Vec.Functional.updateAt
d_updateAt_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (AgdaAny -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_updateAt_130 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_updateAt_130 v3 v4 v5 v6
du_updateAt_130 ::
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (AgdaAny -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_updateAt_130 v0 v1 v2 v3
  = case coe v0 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> case coe v3 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe v1 (coe du_head_64 (coe v2))
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v6
               -> coe v2 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v6)
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v5
        -> case coe v3 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12 -> coe du_head_64 (coe v2)
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v7
               -> coe
                    du_updateAt_130 (coe v5) (coe v1) (coe du_tail_70 (coe v2))
                    (coe v7)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Vec.Functional.map
d_map_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_map_166 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 = du_map_166 v4 v6 v7
du_map_166 ::
  (AgdaAny -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_map_166 v0 v1 v2 = coe v0 (coe v1 v2)
-- Data.Vec.Functional._++_
d__'43''43'__176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d__'43''43'__176 ~v0 ~v1 v2 ~v3 v4 v5 v6
  = du__'43''43'__176 v2 v4 v5 v6
du__'43''43'__176 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du__'43''43'__176 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52 (coe v1) (coe v2)
      (coe MAlonzo.Code.Data.Fin.Base.du_splitAt_164 (coe v0) (coe v3))
-- Data.Vec.Functional.concat
d_concat_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_concat_190 ~v0 ~v1 v2 ~v3 v4 v5 = du_concat_190 v2 v4 v5
du_concat_190 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_concat_190 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Product.Base.du_uncurry_220
      (coe (\ v3 v4 -> coe v1 v4 v3))
      (coe MAlonzo.Code.Data.Fin.Base.du_quotRem_190 (coe v0) (coe v2))
-- Data.Vec.Functional.foldr
d_foldr_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
d_foldr_200 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_foldr_200 v4 v5 v6 v7
du_foldr_200 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
du_foldr_200 v0 v1 v2 v3
  = case coe v2 of
      0 -> coe v1
      _ -> let v4 = subInt (coe v2) (coe (1 :: Integer)) in
           coe
             v0 (coe du_head_64 (coe v3))
             (coe
                du_foldr_200 (coe v0) (coe v1) (coe v4) (coe du_tail_70 (coe v3)))
-- Data.Vec.Functional.foldl
d_foldl_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
d_foldl_218 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_foldl_218 v4 v5 v6 v7
du_foldl_218 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
du_foldl_218 v0 v1 v2 v3
  = case coe v2 of
      0 -> coe v1
      _ -> let v4 = subInt (coe v2) (coe (1 :: Integer)) in
           coe
             du_foldl_218 (coe v0) (coe v0 v1 (coe du_head_64 (coe v3)))
             (coe v4) (coe du_tail_70 (coe v3))
-- Data.Vec.Functional.rearrange
d_rearrange_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_rearrange_238 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_rearrange_238 v4 v5 v6
du_rearrange_238 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_rearrange_238 v0 v1 v2 = coe v1 (coe v0 v2)
-- Data.Vec.Functional._⊛_
d__'8859'__246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d__'8859'__246 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7
  = du__'8859'__246 v5 v6 v7
du__'8859'__246 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du__'8859'__246 v0 v1 v2 = coe v0 v2 (coe v1 v2)
-- Data.Vec.Functional._>>=_
d__'62''62''61'__252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d__'62''62''61'__252 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7
  = du__'62''62''61'__252 v5 v6 v7
du__'62''62''61'__252 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du__'62''62''61'__252 v0 v1 v2
  = coe du_concat_190 (coe v0) (coe du_map_166 (coe v2) (coe v1))
-- Data.Vec.Functional.zipWith
d_zipWith_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_zipWith_260 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 v8 v9 v10
  = du_zipWith_260 v6 v8 v9 v10
du_zipWith_260 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_zipWith_260 v0 v1 v2 v3 = coe v0 (coe v1 v3) (coe v2 v3)
-- Data.Vec.Functional.unzipWith
d_unzipWith_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unzipWith_272 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8
  = du_unzipWith_272 v7 v8
du_unzipWith_272 ::
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unzipWith_272 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         (\ v2 ->
            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0 (coe v1 v2))))
      (coe
         (\ v2 ->
            MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v0 (coe v1 v2))))
-- Data.Vec.Functional.zip
d_zip_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_zip_280 ~v0 ~v1 ~v2 ~v3 ~v4 = du_zip_280
du_zip_280 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_zip_280
  = coe
      du_zipWith_260 (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32)
-- Data.Vec.Functional.unzip
d_unzip_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unzip_284 ~v0 ~v1 ~v2 ~v3 ~v4 = du_unzip_284
du_unzip_284 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unzip_284 = coe du_unzipWith_272 (coe (\ v0 -> v0))
-- Data.Vec.Functional.take
d_take_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_take_290 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_take_290 v4 v5
du_take_290 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_take_290 v0 v1 = coe v0 v1
-- Data.Vec.Functional.drop
d_drop_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_drop_302 ~v0 ~v1 v2 ~v3 v4 v5 = du_drop_302 v2 v4 v5
du_drop_302 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_drop_302 v0 v1 v2
  = coe
      v1
      (coe
         MAlonzo.Code.Data.Fin.Base.du__'8593''691'__86 (coe v0) (coe v2))
-- Data.Vec.Functional.reverse
d_reverse_312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_reverse_312 ~v0 ~v1 v2 v3 v4 = du_reverse_312 v2 v3 v4
du_reverse_312 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_reverse_312 v0 v1 v2
  = coe
      v1 (MAlonzo.Code.Data.Fin.Base.d_opposite_400 (coe v0) (coe v2))
-- Data.Vec.Functional.init
d_init_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_init_318 ~v0 ~v1 ~v2 v3 v4 = du_init_318 v3 v4
du_init_318 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_init_318 v0 v1 = coe v0 v1
-- Data.Vec.Functional.last
d_last_324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
d_last_324 ~v0 ~v1 v2 v3 = du_last_324 v2 v3
du_last_324 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> AgdaAny
du_last_324 v0 v1
  = coe v1 (MAlonzo.Code.Data.Fin.Base.d_fromℕ_48 (coe v0))
-- Data.Vec.Functional.transpose
d_transpose_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_transpose_334 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_transpose_334 v4 v5 v6
du_transpose_334 ::
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
   MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_transpose_334 v0 v1 v2 = coe v0 v2 v1
