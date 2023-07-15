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

module MAlonzo.Code.Data.List.Relation.Binary.Permutation.Homogeneous where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base
import qualified MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Properties
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Data.List.Relation.Binary.Permutation.Homogeneous.Permutation
d_Permutation_28 a0 a1 a2 a3 a4 a5 = ()
data T_Permutation_28
  = C_refl_38 MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.T_Pointwise_48 |
    C_prep_50 AgdaAny T_Permutation_28 |
    C_swap_68 AgdaAny AgdaAny T_Permutation_28 |
    C_trans_76 [AgdaAny] T_Permutation_28 T_Permutation_28
-- Data.List.Relation.Binary.Permutation.Homogeneous._.sym
d_sym_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> T_Permutation_28 -> T_Permutation_28
d_sym_88 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 = du_sym_88 v4 v5 v6 v7
du_sym_88 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> T_Permutation_28 -> T_Permutation_28
du_sym_88 v0 v1 v2 v3
  = case coe v3 of
      C_refl_38 v6
        -> coe
             C_refl_38
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Properties.du_symmetric_40
                (coe v0) (coe v1) (coe v2) (coe v6))
      C_prep_50 v8 v9
        -> case coe v1 of
             (:) v10 v11
               -> case coe v2 of
                    (:) v12 v13
                      -> coe
                           C_prep_50 (coe v0 v10 v12 v8)
                           (coe du_sym_88 (coe v0) (coe v11) (coe v13) (coe v9))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_swap_68 v10 v11 v12
        -> case coe v1 of
             (:) v13 v14
               -> case coe v14 of
                    (:) v15 v16
                      -> case coe v2 of
                           (:) v17 v18
                             -> case coe v18 of
                                  (:) v19 v20
                                    -> coe
                                         C_swap_68 (coe v0 v15 v17 v11) (coe v0 v13 v19 v10)
                                         (coe du_sym_88 (coe v0) (coe v16) (coe v20) (coe v12))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_trans_76 v5 v7 v8
        -> coe
             C_trans_76 v5 (coe du_sym_88 (coe v0) (coe v5) (coe v2) (coe v8))
             (coe du_sym_88 (coe v0) (coe v1) (coe v5) (coe v7))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Permutation.Homogeneous._.isEquivalence
d_isEquivalence_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_114 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_isEquivalence_114 v4 v5
du_isEquivalence_114 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_114 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (coe
         (\ v2 ->
            coe
              C_refl_38
              (coe
                 MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Properties.du_refl_30
                 (coe v0) (coe v2))))
      (coe du_sym_88 (coe v1))
      (\ v2 v3 v4 v5 v6 -> coe C_trans_76 v3 v5 v6)
-- Data.List.Relation.Binary.Permutation.Homogeneous._.setoid
d_setoid_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_120 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_setoid_120 v4 v5
du_setoid_120 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_120 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe du_isEquivalence_114 (coe v0) (coe v1))
-- Data.List.Relation.Binary.Permutation.Homogeneous.map
d_map_130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> T_Permutation_28 -> T_Permutation_28
d_map_130 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9
  = du_map_130 v6 v7 v8 v9
du_map_130 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  [AgdaAny] -> [AgdaAny] -> T_Permutation_28 -> T_Permutation_28
du_map_130 v0 v1 v2 v3
  = case coe v3 of
      C_refl_38 v6
        -> coe
             C_refl_38
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Pointwise.Base.du_map_120
                (coe v0) (coe v1) (coe v2) (coe v6))
      C_prep_50 v8 v9
        -> case coe v1 of
             (:) v10 v11
               -> case coe v2 of
                    (:) v12 v13
                      -> coe
                           C_prep_50 (coe v0 v10 v12 v8)
                           (coe du_map_130 (coe v0) (coe v11) (coe v13) (coe v9))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_swap_68 v10 v11 v12
        -> case coe v1 of
             (:) v13 v14
               -> case coe v14 of
                    (:) v15 v16
                      -> case coe v2 of
                           (:) v17 v18
                             -> case coe v18 of
                                  (:) v19 v20
                                    -> coe
                                         C_swap_68 (coe v0 v13 v19 v10) (coe v0 v15 v17 v11)
                                         (coe du_map_130 (coe v0) (coe v16) (coe v20) (coe v12))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_trans_76 v5 v7 v8
        -> coe
             C_trans_76 v5 (coe du_map_130 (coe v0) (coe v1) (coe v5) (coe v7))
             (coe du_map_130 (coe v0) (coe v5) (coe v2) (coe v8))
      _ -> MAlonzo.RTE.mazUnreachableError
