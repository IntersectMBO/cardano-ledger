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

module MAlonzo.Code.Axiom.Set.TotalMapOn where

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

-- Axiom.Set.TotalMapOn._.Map
d_Map_10 :: MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> () -> ()
d_Map_10 = erased
-- Axiom.Set.TotalMapOn._.left-unique
d_left'45'unique_12 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> () -> AgdaAny -> ()
d_left'45'unique_12 = erased
-- Axiom.Set.TotalMapOn._.Rel
d_Rel_18 :: MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> () -> ()
d_Rel_18 = erased
-- Axiom.Set.TotalMapOn._TotalOn_
d__TotalOn__40 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> AgdaAny -> AgdaAny -> ()
d__TotalOn__40 = erased
-- Axiom.Set.TotalMapOn.TotalMapOn
d_TotalMapOn_52 a0 a1 a2 a3 = ()
data T_TotalMapOn_52
  = C_TotalMapOn'46'constructor_585 AgdaAny
                                    (AgdaAny -> AgdaAny -> AgdaAny)
-- Axiom.Set.TotalMapOn.TotalMapOn.rel
d_rel_66 :: T_TotalMapOn_52 -> AgdaAny
d_rel_66 v0
  = case coe v0 of
      C_TotalMapOn'46'constructor_585 v1 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.TotalMapOn.TotalMapOn.left-unique-rel
d_left'45'unique'45'rel_68 ::
  T_TotalMapOn_52 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'unique'45'rel_68 = erased
-- Axiom.Set.TotalMapOn.TotalMapOn.total-rel
d_total'45'rel_70 ::
  T_TotalMapOn_52 -> AgdaAny -> AgdaAny -> AgdaAny
d_total'45'rel_70 v0
  = case coe v0 of
      C_TotalMapOn'46'constructor_585 v1 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.TotalMapOn.TotalMapOn.toMap
d_toMap_72 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  () -> T_TotalMapOn_52 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_toMap_72 ~v0 ~v1 ~v2 ~v3 v4 = du_toMap_72 v4
du_toMap_72 ::
  T_TotalMapOn_52 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_toMap_72 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe d_rel_66 (coe v0))
      erased
-- Axiom.Set.TotalMapOn.TotalMapOn.lookup
d_lookup_76 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  () ->
  T_TotalMapOn_52 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_lookup_76 v0 ~v1 ~v2 ~v3 v4 v5 = du_lookup_76 v0 v4 v5
du_lookup_76 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  T_TotalMapOn_52 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_lookup_76 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
             (coe
                MAlonzo.Code.Function.Bundles.d_to_938
                (coe
                   MAlonzo.Code.Axiom.Set.Rel.du_dom'8712'_374 (coe v0)
                   (coe d_rel_66 (coe v1)) (coe v3))
                (coe d_total'45'rel_70 v1 v3 v4))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.TotalMapOn.TotalMapOn.lookup∈rel
d_lookup'8712'rel_84 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny -> () -> T_TotalMapOn_52 -> AgdaAny -> AgdaAny -> AgdaAny
d_lookup'8712'rel_84 v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_lookup'8712'rel_84 v0 v4 v5 v6
du_lookup'8712'rel_84 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  T_TotalMapOn_52 -> AgdaAny -> AgdaAny -> AgdaAny
du_lookup'8712'rel_84 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe
         MAlonzo.Code.Function.Bundles.d_to_938
         (coe
            MAlonzo.Code.Axiom.Set.Rel.du_dom'8712'_374 (coe v0)
            (coe d_rel_66 (coe v1)) (coe v2))
         (coe d_total'45'rel_70 v1 v2 v3))
-- Axiom.Set.TotalMapOn.TotalMapOn.rel⇒lookup
d_rel'8658'lookup_94 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  () ->
  T_TotalMapOn_52 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rel'8658'lookup_94 = erased
-- Axiom.Set.TotalMapOn.UpdateOn.updateFn
d_updateFn_110 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_updateFn_110 ~v0 ~v1 ~v2 v3 v4 v5 v6
  = du_updateFn_110 v3 v4 v5 v6
du_updateFn_110 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_updateFn_110 v0 v1 v2 v3
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
-- Axiom.Set.TotalMapOn.UpdateOn.mapWithKeyOn
d_mapWithKeyOn_144 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_TotalMapOn_52 -> T_TotalMapOn_52
d_mapWithKeyOn_144 v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
  = du_mapWithKeyOn_144 v0 v6 v7
du_mapWithKeyOn_144 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  T_TotalMapOn_52 -> T_TotalMapOn_52
du_mapWithKeyOn_144 v0 v1 v2
  = coe
      C_TotalMapOn'46'constructor_585
      (coe
         MAlonzo.Code.Axiom.Set.du_map_360 v0
         (\ v3 ->
            case coe v3 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                -> coe
                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4) (coe v1 v4 v5)
              _ -> MAlonzo.RTE.mazUnreachableError)
         (d_rel_66 (coe v2)))
      (\ v3 v4 ->
         coe
           MAlonzo.Code.Axiom.Set.du_'8712''45'map'8242'_374 (coe v0)
           (coe
              MAlonzo.Code.Axiom.Set.du_map_360 v0
              (\ v5 ->
                 case coe v5 of
                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                     -> coe
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v6) (coe v1 v6 v7)
                   _ -> MAlonzo.RTE.mazUnreachableError)
              (d_rel_66 (coe v2)))
           (coe (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)))
           (let v5
                  = MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                      (coe
                         MAlonzo.Code.Function.Bundles.d_to_938
                         (coe
                            MAlonzo.Code.Axiom.Set.Rel.du_dom'8712'_374 (coe v0)
                            (coe d_rel_66 (coe v2)) (coe v3))
                         (coe d_total'45'rel_70 v2 v3 v4)) in
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
              (coe v1 v3 v5))
           (coe
              MAlonzo.Code.Axiom.Set.du_'8712''45'map'8242'_374 (coe v0)
              (coe d_rel_66 (coe v2))
              (coe
                 (\ v5 ->
                    case coe v5 of
                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                        -> coe
                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v6) (coe v1 v6 v7)
                      _ -> MAlonzo.RTE.mazUnreachableError))
              (coe
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                 (coe
                    MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                    (coe
                       MAlonzo.Code.Function.Bundles.d_to_938
                       (coe
                          MAlonzo.Code.Axiom.Set.Rel.du_dom'8712'_374 (coe v0)
                          (coe d_rel_66 (coe v2)) (coe v3))
                       (coe d_total'45'rel_70 v2 v3 v4))))
              (coe
                 MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                 (coe
                    MAlonzo.Code.Function.Bundles.d_to_938
                    (coe
                       MAlonzo.Code.Axiom.Set.Rel.du_dom'8712'_374 (coe v0)
                       (coe d_rel_66 (coe v2)) (coe v3))
                    (coe d_total'45'rel_70 v2 v3 v4)))))
-- Axiom.Set.TotalMapOn.UpdateOn.update
d_update_164 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> T_TotalMapOn_52 -> T_TotalMapOn_52
d_update_164 v0 ~v1 ~v2 v3 ~v4 v5 v6 = du_update_164 v0 v3 v5 v6
du_update_164 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> T_TotalMapOn_52 -> T_TotalMapOn_52
du_update_164 v0 v1 v2 v3
  = coe
      du_mapWithKeyOn_144 (coe v0)
      (coe
         du_updateFn_110 (coe v1)
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v3)))
