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

module MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Container.Core
import qualified MAlonzo.Code.Data.Container.Relation.Binary.Pointwise

-- Data.Container.Relation.Binary.Pointwise.Properties._.refl
d_refl_30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
d_refl_30 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 = du_refl_30 v7 v8
du_refl_30 ::
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
du_refl_30 v0 v1
  = coe
      MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.C__'44'__66
      (\ v2 ->
         coe v0 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v1 v2))
-- Data.Container.Relation.Binary.Pointwise.Properties._.sym
d_sym_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
d_sym_36 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9 v10
  = du_sym_36 v7 v8 v9 v10
du_sym_36 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
du_sym_36 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.C__'44'__66 v5
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
               -> coe
                    MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.C__'44'__66
                    (\ v8 ->
                       coe
                         v0 (coe v7 v8) (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v2 v8)
                         (coe v5 v8))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Container.Relation.Binary.Pointwise.Properties._.trans
d_trans_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
d_trans_44 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9 v10 v11 v12
  = du_trans_44 v7 v8 v9 v10 v11 v12
du_trans_44 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36 ->
  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.T_Pointwise_36
du_trans_44 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.C__'44'__66 v7
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
               -> case coe v5 of
                    MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.C__'44'__66 v11
                      -> case coe v2 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v12 v13
                             -> coe
                                  MAlonzo.Code.Data.Container.Relation.Binary.Pointwise.C__'44'__66
                                  (\ v14 ->
                                     coe
                                       v0 (coe v9 v14) (coe v13 v14)
                                       (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v3 v14)
                                       (coe v7 v14) (coe v11 v14))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
