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

module MAlonzo.Code.Data.List.Base where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.These.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.List.Base.map
d_map_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> [AgdaAny]
d_map_22 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_map_22 v4 v5
du_map_22 :: (AgdaAny -> AgdaAny) -> [AgdaAny] -> [AgdaAny]
du_map_22 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0 v2)
             (coe du_map_22 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.mapMaybe
d_mapMaybe_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Maybe AgdaAny) -> [AgdaAny] -> [AgdaAny]
d_mapMaybe_32 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_mapMaybe_32 v4 v5
du_mapMaybe_32 ::
  (AgdaAny -> Maybe AgdaAny) -> [AgdaAny] -> [AgdaAny]
du_mapMaybe_32 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> let v4 = coe v0 v2 in
           case coe v4 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v5
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v5)
                    (coe du_mapMaybe_32 (coe v0) (coe v3))
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe du_mapMaybe_32 (coe v0) (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.catMaybes
d_catMaybes_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [Maybe AgdaAny] -> [AgdaAny]
d_catMaybes_60 ~v0 ~v1 = du_catMaybes_60
du_catMaybes_60 :: [Maybe AgdaAny] -> [AgdaAny]
du_catMaybes_60 = coe du_mapMaybe_32 (coe (\ v0 -> v0))
-- Data.List.Base._++_
d__'43''43'__62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> [AgdaAny]
d__'43''43'__62 ~v0 ~v1 v2 v3 = du__'43''43'__62 v2 v3
du__'43''43'__62 :: [AgdaAny] -> [AgdaAny] -> [AgdaAny]
du__'43''43'__62 v0 v1
  = case coe v0 of
      [] -> coe v1
      (:) v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
             (coe du__'43''43'__62 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.intersperse
d_intersperse_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> [AgdaAny] -> [AgdaAny]
d_intersperse_72 ~v0 ~v1 v2 v3 = du_intersperse_72 v2 v3
du_intersperse_72 :: AgdaAny -> [AgdaAny] -> [AgdaAny]
du_intersperse_72 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> let v4
                 = coe
                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
                     (coe
                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0)
                        (coe du_intersperse_72 (coe v0) (coe v3))) in
           case coe v3 of
             [] -> coe v1
             _ -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.intercalate
d_intercalate_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [[AgdaAny]] -> [AgdaAny]
d_intercalate_86 ~v0 ~v1 v2 v3 = du_intercalate_86 v2 v3
du_intercalate_86 :: [AgdaAny] -> [[AgdaAny]] -> [AgdaAny]
du_intercalate_86 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> let v4
                 = coe
                     du__'43''43'__62 (coe v2)
                     (coe
                        du__'43''43'__62 (coe v0)
                        (coe du_intercalate_86 (coe v0) (coe v3))) in
           case coe v3 of
             [] -> coe v2
             _ -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.cartesianProductWith
d_cartesianProductWith_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> [AgdaAny]
d_cartesianProductWith_100 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_cartesianProductWith_100 v6 v7 v8
du_cartesianProductWith_100 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> [AgdaAny]
du_cartesianProductWith_100 v0 v1 v2
  = case coe v1 of
      [] -> coe v1
      (:) v3 v4
        -> coe
             du__'43''43'__62 (coe du_map_22 (coe v0 v3) (coe v2))
             (coe du_cartesianProductWith_100 (coe v0) (coe v4) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.cartesianProduct
d_cartesianProduct_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] -> [AgdaAny] -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_cartesianProduct_112 ~v0 ~v1 ~v2 ~v3 = du_cartesianProduct_112
du_cartesianProduct_112 ::
  [AgdaAny] -> [AgdaAny] -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
du_cartesianProduct_112
  = coe
      du_cartesianProductWith_100
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32)
-- Data.List.Base.alignWith
d_alignWith_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> [AgdaAny]
d_alignWith_114 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_alignWith_114 v6 v7 v8
du_alignWith_114 ::
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> [AgdaAny]
du_alignWith_114 v0 v1 v2
  = case coe v1 of
      []
        -> coe
             du_map_22
             (coe
                MAlonzo.Code.Function.Base.du__'8728''8242'__216 (coe v0)
                (coe MAlonzo.Code.Data.These.Base.C_that_50))
             (coe v2)
      (:) v3 v4
        -> case coe v2 of
             []
               -> coe
                    du_map_22
                    (coe
                       MAlonzo.Code.Function.Base.du__'8728''8242'__216 (coe v0)
                       (coe MAlonzo.Code.Data.These.Base.C_this_48))
                    (coe v1)
             (:) v5 v6
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       v0 (coe MAlonzo.Code.Data.These.Base.C_these_52 (coe v3) (coe v5)))
                    (coe du_alignWith_114 (coe v0) (coe v4) (coe v6))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.zipWith
d_zipWith_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> [AgdaAny]
d_zipWith_134 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_zipWith_134 v6 v7 v8
du_zipWith_134 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> [AgdaAny]
du_zipWith_134 v0 v1 v2
  = let v3 = coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16 in
    case coe v1 of
      (:) v4 v5
        -> case coe v2 of
             (:) v6 v7
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0 v4 v6)
                    (coe du_zipWith_134 (coe v0) (coe v5) (coe v7))
             _ -> coe v3
      _ -> coe v3
-- Data.List.Base.unalignWith
d_unalignWith_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unalignWith_148 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_unalignWith_148 v6 v7
du_unalignWith_148 ::
  (AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unalignWith_148 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v1)
      (:) v2 v3
        -> let v4 = coe v0 v2 in
           case coe v4 of
             MAlonzo.Code.Data.These.Base.C_this_48 v5
               -> coe
                    MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                    (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v5))
                    (coe du_unalignWith_148 (coe v0) (coe v3))
             MAlonzo.Code.Data.These.Base.C_that_50 v5
               -> coe
                    MAlonzo.Code.Data.Product.Base.du_map'8322'_126
                    (\ v6 -> coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v5))
                    (coe du_unalignWith_148 (coe v0) (coe v3))
             MAlonzo.Code.Data.These.Base.C_these_52 v5 v6
               -> coe
                    MAlonzo.Code.Data.Product.Base.du_map_104
                    (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v5))
                    (coe
                       (\ v7 ->
                          coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v6)))
                    (coe du_unalignWith_148 (coe v0) (coe v3))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.unzipWith
d_unzipWith_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unzipWith_196 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_unzipWith_196 v6 v7
du_unzipWith_196 ::
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unzipWith_196 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v1)
      (:) v2 v3
        -> coe
             MAlonzo.Code.Data.Product.Base.du_zip_174
             (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22)
             (coe (\ v4 v5 -> coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22))
             (coe v0 v2) (coe du_unzipWith_196 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.partitionSumsWith
d_partitionSumsWith_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_partitionSumsWith_206 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_partitionSumsWith_206 v6
du_partitionSumsWith_206 ::
  (AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_partitionSumsWith_206 v0
  = coe
      du_unalignWith_148
      (coe
         MAlonzo.Code.Function.Base.du__'8728''8242'__216
         (coe MAlonzo.Code.Data.These.Base.du_fromSum_54) (coe v0))
-- Data.List.Base.align
d_align_210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] -> [AgdaAny] -> [MAlonzo.Code.Data.These.Base.T_These_38]
d_align_210 ~v0 ~v1 ~v2 ~v3 = du_align_210
du_align_210 ::
  [AgdaAny] -> [AgdaAny] -> [MAlonzo.Code.Data.These.Base.T_These_38]
du_align_210 = coe du_alignWith_114 (coe (\ v0 -> v0))
-- Data.List.Base.zip
d_zip_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] -> [AgdaAny] -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_zip_212 ~v0 ~v1 ~v2 ~v3 = du_zip_212
du_zip_212 ::
  [AgdaAny] -> [AgdaAny] -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
du_zip_212
  = coe
      du_zipWith_134 (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32)
-- Data.List.Base.unalign
d_unalign_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Data.These.Base.T_These_38] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unalign_214 ~v0 ~v1 ~v2 ~v3 = du_unalign_214
du_unalign_214 ::
  [MAlonzo.Code.Data.These.Base.T_These_38] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unalign_214 = coe du_unalignWith_148 (coe (\ v0 -> v0))
-- Data.List.Base.unzip
d_unzip_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unzip_216 ~v0 ~v1 ~v2 ~v3 = du_unzip_216
du_unzip_216 ::
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unzip_216 = coe du_unzipWith_196 (coe (\ v0 -> v0))
-- Data.List.Base.partitionSums
d_partitionSums_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [MAlonzo.Code.Data.Sum.Base.T__'8846'__30] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_partitionSums_218 ~v0 ~v1 ~v2 ~v3 = du_partitionSums_218
du_partitionSums_218 ::
  [MAlonzo.Code.Data.Sum.Base.T__'8846'__30] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_partitionSums_218
  = coe du_partitionSumsWith_206 (coe (\ v0 -> v0))
-- Data.List.Base.merge
d_merge_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny] -> [AgdaAny]
d_merge_222 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_merge_222 v4 v5 v6
du_merge_222 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny] -> [AgdaAny]
du_merge_222 v0 v1 v2
  = case coe v1 of
      [] -> coe v2
      (:) v3 v4
        -> case coe v2 of
             [] -> coe v1
             (:) v5 v6
               -> coe
                    MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                       (coe v0 v3 v5))
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                       (coe du_merge_222 (coe v0) (coe v4) (coe v2)))
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v5)
                       (coe du_merge_222 (coe v0) (coe v1) (coe v6)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.foldr
d_foldr_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
d_foldr_242 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_foldr_242 v4 v5 v6
du_foldr_242 ::
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
du_foldr_242 v0 v1 v2
  = case coe v2 of
      [] -> coe v1
      (:) v3 v4
        -> coe v0 v3 (coe du_foldr_242 (coe v0) (coe v1) (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.foldl
d_foldl_256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
d_foldl_256 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_foldl_256 v4 v5 v6
du_foldl_256 ::
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> [AgdaAny] -> AgdaAny
du_foldl_256 v0 v1 v2
  = case coe v2 of
      [] -> coe v1
      (:) v3 v4 -> coe du_foldl_256 (coe v0) (coe v0 v1 v3) (coe v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.concat
d_concat_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [[AgdaAny]] -> [AgdaAny]
d_concat_270 ~v0 ~v1 = du_concat_270
du_concat_270 :: [[AgdaAny]] -> [AgdaAny]
du_concat_270
  = coe
      du_foldr_242 (coe du__'43''43'__62)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Data.List.Base.concatMap
d_concatMap_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> [AgdaAny]) -> [AgdaAny] -> [AgdaAny]
d_concatMap_272 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_concatMap_272 v4 v5
du_concatMap_272 ::
  (AgdaAny -> [AgdaAny]) -> [AgdaAny] -> [AgdaAny]
du_concatMap_272 v0 v1
  = coe du_concat_270 (coe du_map_22 (coe v0) (coe v1))
-- Data.List.Base.ap
d_ap_276 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny -> AgdaAny] -> [AgdaAny] -> [AgdaAny]
d_ap_276 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_ap_276 v4 v5
du_ap_276 :: [AgdaAny -> AgdaAny] -> [AgdaAny] -> [AgdaAny]
du_ap_276 v0 v1
  = coe
      du_concatMap_272 (coe (\ v2 -> coe du_map_22 (coe v2) (coe v1)))
      (coe v0)
-- Data.List.Base.null
d_null_282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> [AgdaAny] -> Bool
d_null_282 ~v0 ~v1 v2 = du_null_282 v2
du_null_282 :: [AgdaAny] -> Bool
du_null_282 v0
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      (:) v1 v2 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.and
d_and_288 :: [Bool] -> Bool
d_and_288
  = coe
      du_foldr_242 (coe MAlonzo.Code.Data.Bool.Base.d__'8743'__24)
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
-- Data.List.Base.or
d_or_290 :: [Bool] -> Bool
d_or_290
  = coe
      du_foldr_242 (coe MAlonzo.Code.Data.Bool.Base.d__'8744'__30)
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
-- Data.List.Base.any
d_any_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Bool) -> [AgdaAny] -> Bool
d_any_292 ~v0 ~v1 v2 v3 = du_any_292 v2 v3
du_any_292 :: (AgdaAny -> Bool) -> [AgdaAny] -> Bool
du_any_292 v0 v1 = coe d_or_290 (coe du_map_22 (coe v0) (coe v1))
-- Data.List.Base.all
d_all_296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Bool) -> [AgdaAny] -> Bool
d_all_296 ~v0 ~v1 v2 v3 = du_all_296 v2 v3
du_all_296 :: (AgdaAny -> Bool) -> [AgdaAny] -> Bool
du_all_296 v0 v1 = coe d_and_288 (coe du_map_22 (coe v0) (coe v1))
-- Data.List.Base.sum
d_sum_300 :: [Integer] -> Integer
d_sum_300 = coe du_foldr_242 (coe addInt) (coe (0 :: Integer))
-- Data.List.Base.product
d_product_302 :: [Integer] -> Integer
d_product_302 = coe du_foldr_242 (coe mulInt) (coe (1 :: Integer))
-- Data.List.Base.length
d_length_304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> Integer
d_length_304 ~v0 ~v1 = du_length_304
du_length_304 :: [AgdaAny] -> Integer
du_length_304
  = coe
      du_foldr_242
      (let v0 = \ v0 -> addInt (coe (1 :: Integer)) (coe v0) in
       coe (\ v1 -> v0))
      (coe (0 :: Integer))
-- Data.List.Base.[_]
d_'91'_'93'_306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> [AgdaAny]
d_'91'_'93'_306 ~v0 ~v1 v2 = du_'91'_'93'_306 v2
du_'91'_'93'_306 :: AgdaAny -> [AgdaAny]
du_'91'_'93'_306 v0
  = coe
      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Data.List.Base.fromMaybe
d_fromMaybe_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Maybe AgdaAny -> [AgdaAny]
d_fromMaybe_310 ~v0 ~v1 v2 = du_fromMaybe_310 v2
du_fromMaybe_310 :: Maybe AgdaAny -> [AgdaAny]
du_fromMaybe_310 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v1
        -> coe du_'91'_'93'_306 (coe v1)
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
        -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.replicate
d_replicate_314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> AgdaAny -> [AgdaAny]
d_replicate_314 ~v0 ~v1 v2 v3 = du_replicate_314 v2 v3
du_replicate_314 :: Integer -> AgdaAny -> [AgdaAny]
du_replicate_314 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
             (coe du_replicate_314 (coe v2) (coe v1))
-- Data.List.Base.inits
d_inits_322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [[AgdaAny]]
d_inits_322 ~v0 ~v1 v2 = du_inits_322 v2
du_inits_322 :: [AgdaAny] -> [[AgdaAny]]
du_inits_322 v0
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v0)
      (:) v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
             (coe
                du_map_22
                (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1))
                (coe du_inits_322 (coe v2)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.tails
d_tails_330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [[AgdaAny]]
d_tails_330 ~v0 ~v1 v2 = du_tails_330 v2
du_tails_330 :: [AgdaAny] -> [[AgdaAny]]
du_tails_330 v0
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v0)
      (:) v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0)
             (coe du_tails_330 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.scanr
d_scanr_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> [AgdaAny]
d_scanr_336 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_scanr_336 v4 v5 v6
du_scanr_336 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> [AgdaAny]
du_scanr_336 v0 v1 v2
  = case coe v2 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v2)
      (:) v3 v4
        -> let v5 = coe du_scanr_336 (coe v0) (coe v1) (coe v4) in
           case coe v5 of
             [] -> coe v5
             (:) v6 v7
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0 v3 v6)
                    (coe v5)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.scanl
d_scanl_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> [AgdaAny]
d_scanl_374 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_scanl_374 v4 v5 v6
du_scanl_374 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> [AgdaAny] -> [AgdaAny]
du_scanl_374 v0 v1 v2
  = case coe v2 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v2)
      (:) v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
             (coe du_scanl_374 (coe v0) (coe v0 v1 v3) (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.applyUpTo
d_applyUpTo_388 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (Integer -> AgdaAny) -> Integer -> [AgdaAny]
d_applyUpTo_388 ~v0 ~v1 v2 v3 = du_applyUpTo_388 v2 v3
du_applyUpTo_388 :: (Integer -> AgdaAny) -> Integer -> [AgdaAny]
du_applyUpTo_388 v0 v1
  = case coe v1 of
      0 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
             (coe v0 (0 :: Integer))
             (coe
                du_applyUpTo_388
                (coe (\ v3 -> coe v0 (addInt (coe (1 :: Integer)) (coe v3))))
                (coe v2))
-- Data.List.Base.applyDownFrom
d_applyDownFrom_396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (Integer -> AgdaAny) -> Integer -> [AgdaAny]
d_applyDownFrom_396 ~v0 ~v1 v2 v3 = du_applyDownFrom_396 v2 v3
du_applyDownFrom_396 ::
  (Integer -> AgdaAny) -> Integer -> [AgdaAny]
du_applyDownFrom_396 v0 v1
  = case coe v1 of
      0 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      _ -> let v2 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0 v2)
             (coe du_applyDownFrom_396 (coe v0) (coe v2))
-- Data.List.Base.tabulate
d_tabulate_408 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> [AgdaAny]
d_tabulate_408 ~v0 ~v1 v2 v3 = du_tabulate_408 v2 v3
du_tabulate_408 ::
  Integer ->
  (MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny) -> [AgdaAny]
du_tabulate_408 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
             (coe v1 (coe MAlonzo.Code.Data.Fin.Base.C_zero_12))
             (coe
                du_tabulate_408 (coe v2)
                (coe
                   (\ v3 -> coe v1 (coe MAlonzo.Code.Data.Fin.Base.C_suc_16 v3))))
-- Data.List.Base.lookup
d_lookup_418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
d_lookup_418 ~v0 ~v1 v2 v3 = du_lookup_418 v2 v3
du_lookup_418 ::
  [AgdaAny] -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny
du_lookup_418 v0 v1
  = case coe v0 of
      (:) v2 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12 -> coe v2
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v5
               -> coe du_lookup_418 (coe v3) (coe v5)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.upTo
d_upTo_430 :: Integer -> [Integer]
d_upTo_430 = coe du_applyUpTo_388 (coe (\ v0 -> v0))
-- Data.List.Base.downFrom
d_downFrom_432 :: Integer -> [Integer]
d_downFrom_432 = coe du_applyDownFrom_396 (coe (\ v0 -> v0))
-- Data.List.Base.allFin
d_allFin_436 :: Integer -> [MAlonzo.Code.Data.Fin.Base.T_Fin_10]
d_allFin_436 v0 = coe du_tabulate_408 (coe v0) (coe (\ v1 -> v1))
-- Data.List.Base.unfold
d_unfold_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (Integer -> ()) ->
  (Integer ->
   AgdaAny -> Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  Integer -> AgdaAny -> [AgdaAny]
d_unfold_448 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 = du_unfold_448 v4 v5 v6
du_unfold_448 ::
  (Integer ->
   AgdaAny -> Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  Integer -> AgdaAny -> [AgdaAny]
du_unfold_448 v0 v1 v2
  = case coe v1 of
      0 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
           let v4 = coe v0 v3 v2 in
           case coe v4 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v5
               -> case coe v5 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                      -> coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v6)
                           (coe du_unfold_448 (coe v0) (coe v3) (coe v7))
                    _ -> MAlonzo.RTE.mazUnreachableError
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.reverseAcc
d_reverseAcc_488 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> [AgdaAny]
d_reverseAcc_488 ~v0 ~v1 = du_reverseAcc_488
du_reverseAcc_488 :: [AgdaAny] -> [AgdaAny] -> [AgdaAny]
du_reverseAcc_488
  = coe
      du_foldl_256
      (coe
         (\ v0 v1 ->
            coe
              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v0)))
-- Data.List.Base.reverse
d_reverse_490 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny]
d_reverse_490 ~v0 ~v1 = du_reverse_490
du_reverse_490 :: [AgdaAny] -> [AgdaAny]
du_reverse_490
  = coe
      du_reverseAcc_488
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Data.List.Base._ʳ++_
d__'691''43''43'__492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> [AgdaAny]
d__'691''43''43'__492 ~v0 ~v1 v2 v3 = du__'691''43''43'__492 v2 v3
du__'691''43''43'__492 :: [AgdaAny] -> [AgdaAny] -> [AgdaAny]
du__'691''43''43'__492 v0 v1 = coe du_reverseAcc_488 v1 v0
-- Data.List.Base._∷ʳ_
d__'8759''691'__494 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> AgdaAny -> [AgdaAny]
d__'8759''691'__494 ~v0 ~v1 v2 v3 = du__'8759''691'__494 v2 v3
du__'8759''691'__494 :: [AgdaAny] -> AgdaAny -> [AgdaAny]
du__'8759''691'__494 v0 v1
  = coe du__'43''43'__62 (coe v0) (coe du_'91'_'93'_306 (coe v1))
-- Data.List.Base.InitLast
d_InitLast_504 a0 a1 a2 = ()
data T_InitLast_504
  = C_'91''93'_508 | C__'8759''691''8242'__514 [AgdaAny] AgdaAny
-- Data.List.Base.initLast
d_initLast_518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> T_InitLast_504
d_initLast_518 ~v0 ~v1 v2 = du_initLast_518 v2
du_initLast_518 :: [AgdaAny] -> T_InitLast_504
du_initLast_518 v0
  = case coe v0 of
      [] -> coe C_'91''93'_508
      (:) v1 v2
        -> let v3 = coe du_initLast_518 (coe v2) in
           case coe v3 of
             C_'91''93'_508
               -> coe
                    C__'8759''691''8242'__514
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v1)
             C__'8759''691''8242'__514 v4 v5
               -> coe
                    C__'8759''691''8242'__514
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1) (coe v4))
                    (coe v5)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.unsnoc
d_unsnoc_540 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unsnoc_540 ~v0 ~v1 v2 = du_unsnoc_540 v2
du_unsnoc_540 ::
  [AgdaAny] -> Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unsnoc_540 v0
  = let v1 = coe du_initLast_518 (coe v0) in
    case coe v1 of
      C_'91''93'_508 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      C__'8759''691''8242'__514 v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
             (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.uncons
d_uncons_556 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_uncons_556 ~v0 ~v1 v2 = du_uncons_556 v2
du_uncons_556 ::
  [AgdaAny] -> Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_uncons_556 v0
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      (:) v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
             (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.head
d_head_562 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> Maybe AgdaAny
d_head_562 ~v0 ~v1 v2 = du_head_562 v2
du_head_562 :: [AgdaAny] -> Maybe AgdaAny
du_head_562 v0
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      (:) v1 v2 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.tail
d_tail_566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> Maybe [AgdaAny]
d_tail_566 ~v0 ~v1 v2 = du_tail_566 v2
du_tail_566 :: [AgdaAny] -> Maybe [AgdaAny]
du_tail_566 v0
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      (:) v1 v2 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.last
d_last_570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> Maybe AgdaAny
d_last_570 ~v0 ~v1 v2 = du_last_570 v2
du_last_570 :: [AgdaAny] -> Maybe AgdaAny
du_last_570 v0
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      (:) v1 v2
        -> let v3 = coe du_last_570 (coe v2) in
           case coe v2 of
             [] -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v1)
             _ -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.take
d_take_576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> [AgdaAny] -> [AgdaAny]
d_take_576 ~v0 ~v1 v2 v3 = du_take_576 v2 v3
du_take_576 :: Integer -> [AgdaAny] -> [AgdaAny]
du_take_576 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             [] -> coe v1
             (:) v3 v4
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                    (coe du_take_576 (coe v2) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.drop
d_drop_588 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Integer -> [AgdaAny] -> [AgdaAny]
d_drop_588 ~v0 ~v1 v2 v3 = du_drop_588 v2 v3
du_drop_588 :: Integer -> [AgdaAny] -> [AgdaAny]
du_drop_588 v0 v1
  = case coe v0 of
      0 -> coe v1
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             [] -> coe v1
             (:) v3 v4 -> coe du_drop_588 (coe v2) (coe v4)
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.splitAt
d_splitAt_600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  Integer -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_splitAt_600 ~v0 ~v1 v2 v3 = du_splitAt_600 v2 v3
du_splitAt_600 ::
  Integer -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_splitAt_600 v0 v1
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v1)
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           case coe v1 of
             []
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v1)
             (:) v3 v4
               -> coe
                    MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                    (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3))
                    (coe du_splitAt_600 (coe v2) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.takeWhileᵇ
d_takeWhile'7495'_614 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny]
d_takeWhile'7495'_614 ~v0 ~v1 v2 v3 = du_takeWhile'7495'_614 v2 v3
du_takeWhile'7495'_614 ::
  (AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny]
du_takeWhile'7495'_614 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> coe
             MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v2)
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
                (coe du_takeWhile'7495'_614 (coe v0) (coe v3)))
             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.dropWhileᵇ
d_dropWhile'7495'_624 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny]
d_dropWhile'7495'_624 ~v0 ~v1 v2 v3 = du_dropWhile'7495'_624 v2 v3
du_dropWhile'7495'_624 ::
  (AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny]
du_dropWhile'7495'_624 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> coe
             MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v2)
             (coe du_dropWhile'7495'_624 (coe v0) (coe v3)) (coe v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.filterᵇ
d_filter'7495'_634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny]
d_filter'7495'_634 ~v0 ~v1 v2 v3 = du_filter'7495'_634 v2 v3
du_filter'7495'_634 :: (AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny]
du_filter'7495'_634 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> coe
             MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v2)
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
                (coe du_filter'7495'_634 (coe v0) (coe v3)))
             (coe du_filter'7495'_634 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.partitionᵇ
d_partition'7495'_644 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> Bool) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_partition'7495'_644 ~v0 ~v1 v2 v3 = du_partition'7495'_644 v2 v3
du_partition'7495'_644 ::
  (AgdaAny -> Bool) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_partition'7495'_644 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v1)
      (:) v2 v3
        -> coe
             MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v2)
             (coe MAlonzo.Code.Data.Product.Base.du_map'8321'_114)
             (coe MAlonzo.Code.Data.Product.Base.du_map'8322''8242'_298)
             (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2))
             (coe du_partition'7495'_644 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.spanᵇ
d_span'7495'_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> Bool) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_span'7495'_656 ~v0 ~v1 v2 v3 = du_span'7495'_656 v2 v3
du_span'7495'_656 ::
  (AgdaAny -> Bool) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_span'7495'_656 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v1)
      (:) v2 v3
        -> coe
             MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v2)
             (coe
                MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2))
                (coe du_span'7495'_656 (coe v0) (coe v3)))
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.breakᵇ
d_break'7495'_668 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> Bool) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_break'7495'_668 ~v0 ~v1 v2 = du_break'7495'_668 v2
du_break'7495'_668 ::
  (AgdaAny -> Bool) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_break'7495'_668 v0
  = coe
      du_span'7495'_656
      (coe (\ v1 -> MAlonzo.Code.Data.Bool.Base.d_not_22 (coe v0 v1)))
-- Data.List.Base.linesByᵇ
d_linesBy'7495'_672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Bool) -> [AgdaAny] -> [[AgdaAny]]
d_linesBy'7495'_672 ~v0 ~v1 v2 = du_linesBy'7495'_672 v2
du_linesBy'7495'_672 ::
  (AgdaAny -> Bool) -> [AgdaAny] -> [[AgdaAny]]
du_linesBy'7495'_672 v0
  = coe
      du_go_682 (coe v0)
      (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
-- Data.List.Base._.go
d_go_682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> Bool) -> Maybe [AgdaAny] -> [AgdaAny] -> [[AgdaAny]]
d_go_682 ~v0 ~v1 v2 v3 v4 = du_go_682 v2 v3 v4
du_go_682 ::
  (AgdaAny -> Bool) -> Maybe [AgdaAny] -> [AgdaAny] -> [[AgdaAny]]
du_go_682 v0 v1 v2
  = case coe v2 of
      []
        -> coe
             MAlonzo.Code.Data.Maybe.Base.du_maybe'8242'_48
             (coe
                MAlonzo.Code.Function.Base.du__'8728''8242'__216
                (coe du_'91'_'93'_306) (coe du_reverse_490))
             v2 v1
      (:) v3 v4
        -> let v5 = coe v0 v3 in
           if coe v5
             then coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       du_reverse_490
                       (coe
                          MAlonzo.Code.Data.Maybe.Base.du_fromMaybe_50
                          (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) v1))
                    (coe
                       du_go_682 (coe v0)
                       (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18) (coe v4))
             else coe
                    du_go_682 (coe v0)
                    (coe
                       MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                       (coe
                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                          (coe
                             MAlonzo.Code.Data.Maybe.Base.du_fromMaybe_50
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) v1)))
                    (coe v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.wordsByᵇ
d_wordsBy'7495'_708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Bool) -> [AgdaAny] -> [[AgdaAny]]
d_wordsBy'7495'_708 ~v0 ~v1 v2 = du_wordsBy'7495'_708 v2
du_wordsBy'7495'_708 ::
  (AgdaAny -> Bool) -> [AgdaAny] -> [[AgdaAny]]
du_wordsBy'7495'_708 v0
  = coe
      du_go_726 (coe v0)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Data.List.Base._.cons
d_cons_718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Bool) -> [AgdaAny] -> [[AgdaAny]] -> [[AgdaAny]]
d_cons_718 ~v0 ~v1 ~v2 v3 v4 = du_cons_718 v3 v4
du_cons_718 :: [AgdaAny] -> [[AgdaAny]] -> [[AgdaAny]]
du_cons_718 v0 v1
  = let v2
          = coe
              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
              (coe du_reverse_490 v0) (coe v1) in
    case coe v0 of
      [] -> coe v1
      _ -> coe v2
-- Data.List.Base._.go
d_go_726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny] -> [[AgdaAny]]
d_go_726 ~v0 ~v1 v2 v3 v4 = du_go_726 v2 v3 v4
du_go_726 ::
  (AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny] -> [[AgdaAny]]
du_go_726 v0 v1 v2
  = case coe v2 of
      [] -> coe du_cons_718 (coe v1) (coe v2)
      (:) v3 v4
        -> let v5 = coe v0 v3 in
           if coe v5
             then coe
                    du_cons_718 (coe v1)
                    (coe
                       du_go_726 (coe v0)
                       (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v4))
             else coe
                    du_go_726 (coe v0)
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3) (coe v1))
                    (coe v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.derunᵇ
d_derun'7495'_752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny]
d_derun'7495'_752 ~v0 ~v1 v2 v3 = du_derun'7495'_752 v2 v3
du_derun'7495'_752 ::
  (AgdaAny -> AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny]
du_derun'7495'_752 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> case coe v3 of
             [] -> coe v1
             (:) v4 v5
               -> coe
                    MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v0 v2 v4)
                    (coe du_derun'7495'_752 (coe v0) (coe v3))
                    (coe
                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
                       (coe du_derun'7495'_752 (coe v0) (coe v3)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.deduplicateᵇ
d_deduplicate'7495'_768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny]
d_deduplicate'7495'_768 ~v0 ~v1 v2 v3
  = du_deduplicate'7495'_768 v2 v3
du_deduplicate'7495'_768 ::
  (AgdaAny -> AgdaAny -> Bool) -> [AgdaAny] -> [AgdaAny]
du_deduplicate'7495'_768 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
             (coe
                du_filter'7495'_634
                (coe (\ v4 -> MAlonzo.Code.Data.Bool.Base.d_not_22 (coe v0 v2 v4)))
                (coe du_deduplicate'7495'_768 (coe v0) (coe v3)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base.takeWhile
d_takeWhile_780 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
d_takeWhile_780 ~v0 ~v1 ~v2 ~v3 v4 = du_takeWhile_780 v4
du_takeWhile_780 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
du_takeWhile_780 v0
  = coe
      du_takeWhile'7495'_614
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.List.Base.dropWhile
d_dropWhile_786 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
d_dropWhile_786 ~v0 ~v1 ~v2 ~v3 v4 = du_dropWhile_786 v4
du_dropWhile_786 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
du_dropWhile_786 v0
  = coe
      du_dropWhile'7495'_624
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.List.Base.filter
d_filter_792 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
d_filter_792 ~v0 ~v1 ~v2 ~v3 v4 = du_filter_792 v4
du_filter_792 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
du_filter_792 v0
  = coe
      du_filter'7495'_634
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.List.Base.partition
d_partition_798 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_partition_798 ~v0 ~v1 ~v2 ~v3 v4 = du_partition_798 v4
du_partition_798 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_partition_798 v0
  = coe
      du_partition'7495'_644
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.List.Base.span
d_span_804 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_span_804 ~v0 ~v1 ~v2 ~v3 v4 = du_span_804 v4
du_span_804 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_span_804 v0
  = coe
      du_span'7495'_656
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.List.Base.break
d_break_810 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_break_810 ~v0 ~v1 ~v2 ~v3 v4 = du_break_810 v4
du_break_810 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_break_810 v0
  = coe
      du_break'7495'_668
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.List.Base.linesBy
d_linesBy_816 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [[AgdaAny]]
d_linesBy_816 ~v0 ~v1 ~v2 ~v3 v4 = du_linesBy_816 v4
du_linesBy_816 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [[AgdaAny]]
du_linesBy_816 v0
  = coe
      du_linesBy'7495'_672
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.List.Base.wordsBy
d_wordsBy_822 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [[AgdaAny]]
d_wordsBy_822 ~v0 ~v1 ~v2 ~v3 v4 = du_wordsBy_822 v4
du_wordsBy_822 ::
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [[AgdaAny]]
du_wordsBy_822 v0
  = coe
      du_wordsBy'7495'_708
      (coe
         (\ v1 ->
            MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
              (coe v0 v1)))
-- Data.List.Base.derun
d_derun_828 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
d_derun_828 ~v0 ~v1 ~v2 ~v3 v4 = du_derun_828 v4
du_derun_828 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
du_derun_828 v0
  = coe
      du_derun'7495'_752
      (coe
         MAlonzo.Code.Function.Base.du__'8728''8322'__92
         (coe
            (\ v1 v2 v3 ->
               MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30 (coe v3)))
         (coe v0))
-- Data.List.Base.deduplicate
d_deduplicate_834 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
d_deduplicate_834 ~v0 ~v1 ~v2 ~v3 v4 = du_deduplicate_834 v4
du_deduplicate_834 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> [AgdaAny]
du_deduplicate_834 v0
  = coe
      du_deduplicate'7495'_768
      (coe
         MAlonzo.Code.Function.Base.du__'8728''8322'__92
         (coe
            (\ v1 v2 v3 ->
               MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30 (coe v3)))
         (coe v0))
-- Data.List.Base._[_]%=_
d__'91'_'93''37''61'__840 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny]
d__'91'_'93''37''61'__840 ~v0 ~v1 v2 v3 v4
  = du__'91'_'93''37''61'__840 v2 v3 v4
du__'91'_'93''37''61'__840 ::
  [AgdaAny] ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny]
du__'91'_'93''37''61'__840 v0 v1 v2
  = case coe v0 of
      (:) v3 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2 v3) (coe v4)
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v6
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                    (coe du__'91'_'93''37''61'__840 (coe v4) (coe v6) (coe v2))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base._[_]∷=_
d__'91'_'93''8759''61'__858 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny -> [AgdaAny]
d__'91'_'93''8759''61'__858 ~v0 ~v1 v2 v3 v4
  = du__'91'_'93''8759''61'__858 v2 v3 v4
du__'91'_'93''8759''61'__858 ::
  [AgdaAny] ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> AgdaAny -> [AgdaAny]
du__'91'_'93''8759''61'__858 v0 v1 v2
  = coe
      du__'91'_'93''37''61'__840 (coe v0) (coe v1) (coe (\ v3 -> v2))
-- Data.List.Base._─_
d__'9472'__868 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> [AgdaAny]
d__'9472'__868 ~v0 ~v1 v2 v3 = du__'9472'__868 v2 v3
du__'9472'__868 ::
  [AgdaAny] -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> [AgdaAny]
du__'9472'__868 v0 v1
  = case coe v0 of
      (:) v2 v3
        -> case coe v1 of
             MAlonzo.Code.Data.Fin.Base.C_zero_12 -> coe v3
             MAlonzo.Code.Data.Fin.Base.C_suc_16 v5
               -> coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
                    (coe du__'9472'__868 (coe v3) (coe v5))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Base._?∷_
d__'63''8759'__880 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Maybe AgdaAny -> [AgdaAny] -> [AgdaAny]
d__'63''8759'__880 ~v0 ~v1 = du__'63''8759'__880
du__'63''8759'__880 :: Maybe AgdaAny -> [AgdaAny] -> [AgdaAny]
du__'63''8759'__880
  = coe
      MAlonzo.Code.Data.Maybe.Base.du_maybe'8242'_48
      (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22) (\ v0 -> v0)
-- Data.List.Base._∷ʳ?_
d__'8759''691''63'__882 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> Maybe AgdaAny -> [AgdaAny]
d__'8759''691''63'__882 ~v0 ~v1 v2 v3
  = du__'8759''691''63'__882 v2 v3
du__'8759''691''63'__882 :: [AgdaAny] -> Maybe AgdaAny -> [AgdaAny]
du__'8759''691''63'__882 v0 v1
  = coe
      MAlonzo.Code.Data.Maybe.Base.du_maybe'8242'_48
      (coe du__'8759''691'__494 (coe v0)) v0 v1
-- Data.List.Base._.++-rawMagma
d_'43''43''45'rawMagma_898 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
d_'43''43''45'rawMagma_898 ~v0 ~v1 = du_'43''43''45'rawMagma_898
du_'43''43''45'rawMagma_898 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMagma_10
du_'43''43''45'rawMagma_898
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMagma'46'constructor_77
      (coe du__'43''43'__62)
-- Data.List.Base._.++-[]-rawMonoid
d_'43''43''45''91''93''45'rawMonoid_900 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
d_'43''43''45''91''93''45'rawMonoid_900 ~v0 ~v1
  = du_'43''43''45''91''93''45'rawMonoid_900
du_'43''43''45''91''93''45'rawMonoid_900 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawMonoid_38
du_'43''43''45''91''93''45'rawMonoid_900
  = coe
      MAlonzo.Code.Algebra.Bundles.Raw.C_RawMonoid'46'constructor_473
      (coe du__'43''43'__62)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Data.List.Base._∷ʳ'_
d__'8759''691'''__906 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> AgdaAny -> T_InitLast_504
d__'8759''691'''__906 ~v0 ~v1 = du__'8759''691'''__906
du__'8759''691'''__906 :: [AgdaAny] -> AgdaAny -> T_InitLast_504
du__'8759''691'''__906 = coe C__'8759''691''8242'__514
