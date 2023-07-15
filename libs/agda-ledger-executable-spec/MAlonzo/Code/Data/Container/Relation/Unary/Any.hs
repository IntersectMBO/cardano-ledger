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

module MAlonzo.Code.Data.Container.Relation.Unary.Any where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Container.Core
import qualified MAlonzo.Code.Data.Container.Morphism

-- Data.Container.Relation.Unary.Any.◇
d_'9671'_26 a0 a1 a2 a3 a4 a5 a6 a7 = ()
newtype T_'9671'_26
  = C_any_52 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Data.Container.Relation.Unary.Any.◇.proof
d_proof_50 :: T_'9671'_26 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_proof_50 v0
  = case coe v0 of
      C_any_52 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Container.Relation.Unary.Any._.map
d_map_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_'9671'_26 -> T_'9671'_26
d_map_84 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
         v14 v15
  = du_map_84 v12 v13 v14 v15
du_map_84 ::
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_'9671'_26 -> T_'9671'_26
du_map_84 v0 v1 v2 v3
  = coe
      C_any_52
      (case coe v3 of
         C_any_52 v4
           -> case coe v4 of
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
                  -> coe
                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                       (coe
                          MAlonzo.Code.Data.Container.Core.d_position_98 v0
                          (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v2)) v5)
                       (coe
                          v1
                          (coe
                             MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                             (coe MAlonzo.Code.Data.Container.Core.du_'10218'_'10219'_104 v0 v2)
                             v5)
                          v6)
                _ -> MAlonzo.RTE.mazUnreachableError
         _ -> MAlonzo.RTE.mazUnreachableError)
-- Data.Container.Relation.Unary.Any._.map₁
d_map'8321'_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_'9671'_26 -> T_'9671'_26
d_map'8321'_120 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10 v11
  = du_map'8321'_120 v10 v11
du_map'8321'_120 ::
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_'9671'_26 -> T_'9671'_26
du_map'8321'_120 v0 v1
  = coe du_map_84 (coe v0) (coe (\ v2 v3 -> v3)) (coe v1)
-- Data.Container.Relation.Unary.Any._.map₂
d_map'8322'_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_'9671'_26 -> T_'9671'_26
d_map'8322'_146 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
  = du_map'8322'_146
du_map'8322'_146 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_'9671'_26 -> T_'9671'_26
du_map'8322'_146
  = coe du_map_84 (coe MAlonzo.Code.Data.Container.Morphism.du_id_16)
