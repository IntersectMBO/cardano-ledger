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

module MAlonzo.Code.Data.List.Relation.Unary.Linked where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Maybe.Relation.Binary.Connected
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Data.List.Relation.Unary.Linked.Linked
d_Linked_26 a0 a1 a2 a3 a4 = ()
data T_Linked_26
  = C_'91''93'_30 | C_'91''45''93'_34 |
    C__'8759'__42 AgdaAny T_Linked_26
-- Data.List.Relation.Unary.Linked._.head
d_head_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> [AgdaAny] -> T_Linked_26 -> AgdaAny
d_head_58 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 = du_head_58 v7
du_head_58 :: T_Linked_26 -> AgdaAny
du_head_58 v0
  = case coe v0 of
      C__'8759'__42 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Unary.Linked._.tail
d_tail_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> [AgdaAny] -> T_Linked_26 -> T_Linked_26
d_tail_68 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_tail_68 v6
du_tail_68 :: T_Linked_26 -> T_Linked_26
du_tail_68 v0
  = case coe v0 of
      C_'91''45''93'_34 -> coe C_'91''93'_30
      C__'8759'__42 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Unary.Linked._.head′
d_head'8242'_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  [AgdaAny] ->
  T_Linked_26 ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42
d_head'8242'_76 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_head'8242'_76 v6
du_head'8242'_76 ::
  T_Linked_26 ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42
du_head'8242'_76 v0
  = case coe v0 of
      C_'91''45''93'_34
        -> coe
             MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.C_just'45'nothing_52
      C__'8759'__42 v4 v5
        -> coe
             MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.C_just_50 v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Unary.Linked._._∷′_
d__'8759''8242'__84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42 ->
  T_Linked_26 -> T_Linked_26
d__'8759''8242'__84 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7
  = du__'8759''8242'__84 v5 v6 v7
du__'8759''8242'__84 ::
  [AgdaAny] ->
  MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.T_Connected_42 ->
  T_Linked_26 -> T_Linked_26
du__'8759''8242'__84 v0 v1 v2
  = case coe v0 of
      [] -> coe C_'91''45''93'_34
      (:) v3 v4
        -> case coe v1 of
             MAlonzo.Code.Data.Maybe.Relation.Binary.Connected.C_just_50 v7
               -> coe C__'8759'__42 v7 v2
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Unary.Linked._.map
d_map_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> T_Linked_26 -> T_Linked_26
d_map_106 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 = du_map_106 v6 v7 v8
du_map_106 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> T_Linked_26 -> T_Linked_26
du_map_106 v0 v1 v2
  = case coe v2 of
      C_'91''93'_30 -> coe v2
      C_'91''45''93'_34 -> coe C_'91''45''93'_34
      C__'8759'__42 v6 v7
        -> case coe v1 of
             (:) v8 v9
               -> case coe v9 of
                    (:) v10 v11
                      -> coe
                           C__'8759'__42 (coe v0 v8 v10 v6)
                           (coe du_map_106 (coe v0) (coe v9) (coe v7))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Unary.Linked._.zipWith
d_zipWith_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Linked_26
d_zipWith_134 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du_zipWith_134 v8 v9 v10
du_zipWith_134 ::
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Linked_26
du_zipWith_134 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> case coe v3 of
             C_'91''93'_30 -> coe seq (coe v4) (coe v3)
             C_'91''45''93'_34 -> coe seq (coe v4) (coe C_'91''45''93'_34)
             C__'8759'__42 v8 v9
               -> case coe v1 of
                    (:) v10 v11
                      -> case coe v11 of
                           (:) v12 v13
                             -> case coe v4 of
                                  C__'8759'__42 v17 v18
                                    -> coe
                                         C__'8759'__42
                                         (coe
                                            v0 v10 v12
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v8)
                                               (coe v17)))
                                         (coe
                                            du_zipWith_134 (coe v0) (coe v11)
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v9)
                                               (coe v18)))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Unary.Linked._.unzipWith
d_unzipWith_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  [AgdaAny] -> T_Linked_26 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unzipWith_150 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du_unzipWith_150 v8 v9 v10
du_unzipWith_150 ::
  (AgdaAny ->
   AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) ->
  [AgdaAny] -> T_Linked_26 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unzipWith_150 v0 v1 v2
  = case coe v2 of
      C_'91''93'_30
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v2)
      C_'91''45''93'_34
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe C_'91''45''93'_34)
             (coe C_'91''45''93'_34)
      C__'8759'__42 v6 v7
        -> case coe v1 of
             (:) v8 v9
               -> case coe v9 of
                    (:) v10 v11
                      -> coe
                           MAlonzo.Code.Data.Product.Base.du_zip_174 (coe C__'8759'__42)
                           (coe (\ v12 v13 -> coe C__'8759'__42)) (coe v0 v8 v10 v6)
                           (coe du_unzipWith_150 (coe v0) (coe v9) (coe v7))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Unary.Linked._.zip
d_zip_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Linked_26
d_zip_174 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_zip_174 v6
du_zip_174 ::
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> T_Linked_26
du_zip_174 v0
  = coe du_zipWith_134 (coe (\ v1 v2 v3 -> v3)) (coe v0)
-- Data.List.Relation.Unary.Linked._.unzip
d_unzip_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  [AgdaAny] -> T_Linked_26 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unzip_176 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_unzip_176 v6
du_unzip_176 ::
  [AgdaAny] -> T_Linked_26 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unzip_176 v0
  = coe du_unzipWith_150 (coe (\ v1 v2 v3 -> v3)) (coe v0)
-- Data.List.Relation.Unary.Linked._.linked?
d_linked'63'_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_linked'63'_186 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_linked'63'_186 v4 v5
du_linked'63'_186 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  [AgdaAny] -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_linked'63'_186 v0 v1
  = case coe v1 of
      []
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe
                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                (coe C_'91''93'_30))
      (:) v2 v3
        -> case coe v3 of
             []
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                    (coe
                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                       (coe C_'91''45''93'_34))
             (:) v4 v5
               -> coe
                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
                    (coe
                       MAlonzo.Code.Data.Product.Base.du_uncurry_220 (coe C__'8759'__42))
                    (coe
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                       (coe v0 v2 v4) (coe du_linked'63'_186 (coe v0) (coe v3)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Unary.Linked._.irrelevant
d_irrelevant_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  [AgdaAny] ->
  T_Linked_26 ->
  T_Linked_26 -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_irrelevant_202 = erased
-- Data.List.Relation.Unary.Linked._.satisfiable
d_satisfiable_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_satisfiable_218 ~v0 ~v1 ~v2 ~v3 = du_satisfiable_218
du_satisfiable_218 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_satisfiable_218
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe C_'91''93'_30)
