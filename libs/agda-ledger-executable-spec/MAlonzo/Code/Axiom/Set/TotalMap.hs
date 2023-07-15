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

module MAlonzo.Code.Axiom.Set.TotalMap where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Rel
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Axiom.Set.TotalMap._.Map
d_Map_10 :: MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> () -> ()
d_Map_10 = erased
-- Axiom.Set.TotalMap._.left-unique
d_left'45'unique_12 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> () -> AgdaAny -> ()
d_left'45'unique_12 = erased
-- Axiom.Set.TotalMap._.Rel
d_Rel_18 :: MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> () -> ()
d_Rel_18 = erased
-- Axiom.Set.TotalMap.total
d_total_40 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> () -> AgdaAny -> ()
d_total_40 = erased
-- Axiom.Set.TotalMap.TotalMap
d_TotalMap_50 a0 a1 a2 = ()
data T_TotalMap_50
  = C_TotalMap'46'constructor_563 AgdaAny (AgdaAny -> AgdaAny)
-- Axiom.Set.TotalMap.TotalMap.rel
d_rel_62 :: T_TotalMap_50 -> AgdaAny
d_rel_62 v0
  = case coe v0 of
      C_TotalMap'46'constructor_563 v1 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.TotalMap.TotalMap.left-unique-rel
d_left'45'unique'45'rel_64 ::
  T_TotalMap_50 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'unique'45'rel_64 = erased
-- Axiom.Set.TotalMap.TotalMap.total-rel
d_total'45'rel_66 :: T_TotalMap_50 -> AgdaAny -> AgdaAny
d_total'45'rel_66 v0
  = case coe v0 of
      C_TotalMap'46'constructor_563 v1 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.TotalMap.TotalMap.toMap
d_toMap_68 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> T_TotalMap_50 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_toMap_68 ~v0 ~v1 ~v2 v3 = du_toMap_68 v3
du_toMap_68 ::
  T_TotalMap_50 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_toMap_68 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe d_rel_62 (coe v0))
      erased
-- Axiom.Set.TotalMap.TotalMap.lookup
d_lookup_70 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> T_TotalMap_50 -> AgdaAny -> AgdaAny
d_lookup_70 v0 ~v1 ~v2 v3 v4 = du_lookup_70 v0 v3 v4
du_lookup_70 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  T_TotalMap_50 -> AgdaAny -> AgdaAny
du_lookup_70 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe
         MAlonzo.Code.Function.Bundles.d_to_938
         (coe
            MAlonzo.Code.Axiom.Set.Rel.du_dom'8712'_374 (coe v0)
            (coe d_rel_62 (coe v1)) (coe v2))
         (coe d_total'45'rel_66 v1 v2))
-- Axiom.Set.TotalMap.TotalMap.lookup∈rel
d_lookup'8712'rel_74 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> T_TotalMap_50 -> AgdaAny -> AgdaAny
d_lookup'8712'rel_74 v0 ~v1 ~v2 v3 v4
  = du_lookup'8712'rel_74 v0 v3 v4
du_lookup'8712'rel_74 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  T_TotalMap_50 -> AgdaAny -> AgdaAny
du_lookup'8712'rel_74 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe
         MAlonzo.Code.Function.Bundles.d_to_938
         (coe
            MAlonzo.Code.Axiom.Set.Rel.du_dom'8712'_374 (coe v0)
            (coe d_rel_62 (coe v1)) (coe v2))
         (coe d_total'45'rel_66 v1 v2))
-- Axiom.Set.TotalMap.TotalMap.rel⇒lookup
d_rel'8658'lookup_80 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  T_TotalMap_50 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rel'8658'lookup_80 = erased
-- Axiom.Set.TotalMap.Update.updateFn
d_updateFn_92 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_updateFn_92 ~v0 ~v1 ~v2 v3 v4 v5 v6 = du_updateFn_92 v3 v4 v5 v6
du_updateFn_92 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_updateFn_92 v0 v1 v2 v3
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
        -> let v6
                 = coe MAlonzo.Code.Interface.DecEq.d__'8799'__20 v0 v2 v4 in
           case coe v6 of
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v7 v8
               -> if coe v7
                    then coe seq (coe v8) (coe v5)
                    else coe seq (coe v8) (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.TotalMap.Update.mapWithKey
d_mapWithKey_124 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> T_TotalMap_50 -> T_TotalMap_50
d_mapWithKey_124 v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_mapWithKey_124 v0 v5 v6
du_mapWithKey_124 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> T_TotalMap_50 -> T_TotalMap_50
du_mapWithKey_124 v0 v1 v2
  = coe
      C_TotalMap'46'constructor_563
      (coe
         MAlonzo.Code.Axiom.Set.du_map_360 v0
         (\ v3 ->
            case coe v3 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                -> coe
                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4) (coe v1 v4 v5)
              _ -> MAlonzo.RTE.mazUnreachableError)
         (d_rel_62 (coe v2)))
      (\ v3 ->
         coe
           MAlonzo.Code.Axiom.Set.du_'8712''45'map'8242'_374 (coe v0)
           (coe
              MAlonzo.Code.Axiom.Set.du_map_360 v0
              (\ v4 ->
                 case coe v4 of
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
                     -> coe
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v5) (coe v1 v5 v6)
                   _ -> MAlonzo.RTE.mazUnreachableError)
              (d_rel_62 (coe v2)))
           (coe (\ v4 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v4)))
           (let v4
                  = MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                      (coe
                         MAlonzo.Code.Function.Bundles.d_to_938
                         (coe
                            MAlonzo.Code.Axiom.Set.Rel.du_dom'8712'_374 (coe v0)
                            (coe d_rel_62 (coe v2)) (coe v3))
                         (coe d_total'45'rel_66 v2 v3)) in
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
              (coe v1 v3 v4))
           (coe
              MAlonzo.Code.Axiom.Set.du_'8712''45'map'8242'_374 (coe v0)
              (coe d_rel_62 (coe v2))
              (coe
                 (\ v4 ->
                    case coe v4 of
                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
                        -> coe
                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v5) (coe v1 v5 v6)
                      _ -> MAlonzo.RTE.mazUnreachableError))
              (coe
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                 (coe
                    MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                    (coe
                       MAlonzo.Code.Function.Bundles.d_to_938
                       (coe
                          MAlonzo.Code.Axiom.Set.Rel.du_dom'8712'_374 (coe v0)
                          (coe d_rel_62 (coe v2)) (coe v3))
                       (coe d_total'45'rel_66 v2 v3))))
              (coe
                 MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                 (coe
                    MAlonzo.Code.Function.Bundles.d_to_938
                    (coe
                       MAlonzo.Code.Axiom.Set.Rel.du_dom'8712'_374 (coe v0)
                       (coe d_rel_62 (coe v2)) (coe v3))
                    (coe d_total'45'rel_66 v2 v3)))))
-- Axiom.Set.TotalMap.Update.update
d_update_140 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> T_TotalMap_50 -> T_TotalMap_50
d_update_140 v0 ~v1 ~v2 v3 v4 v5 = du_update_140 v0 v3 v4 v5
du_update_140 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> T_TotalMap_50 -> T_TotalMap_50
du_update_140 v0 v1 v2 v3
  = coe
      du_mapWithKey_124 (coe v0)
      (coe
         du_updateFn_92 (coe v1)
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v3)))
