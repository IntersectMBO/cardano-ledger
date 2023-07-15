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

module MAlonzo.Code.Axiom.Set.Rel where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Properties
import qualified MAlonzo.Code.Data.Empty
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Axiom.Set.Rel._._∉_
d__'8713'__16 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'8713'__16 = erased
-- Axiom.Set.Rel._._∪_
d__'8746'__18 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__18 v0 v1 v2 v3
  = coe MAlonzo.Code.Axiom.Set.du__'8746'__642 (coe v0) v2 v3
-- Axiom.Set.Rel._._≡ᵉ_
d__'8801''7497'__20 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'__20 = erased
-- Axiom.Set.Rel._._⊆_
d__'8838'__24 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'8838'__24 = erased
-- Axiom.Set.Rel._.disjoint
d_disjoint_44 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d_disjoint_44 = erased
-- Axiom.Set.Rel._.map
d_map_56 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map_56 v0 v1 v2 = coe MAlonzo.Code.Axiom.Set.du_map_360 (coe v0)
-- Axiom.Set.Rel._.spec-∈
d_spec'45''8712'_76 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> ()
d_spec'45''8712'_76 = erased
-- Axiom.Set.Rel._.∅
d_'8709'_90 :: MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> AgdaAny
d_'8709'_90 v0 v1
  = coe MAlonzo.Code.Axiom.Set.du_'8709'_404 (coe v0)
-- Axiom.Set.Rel._.Intersection._∩_
d__'8745'__122 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__122 v0 v1 v2 v3 v4
  = coe MAlonzo.Code.Axiom.Set.du__'8745'__666 (coe v0) v2 v3 v4
-- Axiom.Set.Rel._._≡_⨿_
d__'8801'_'10815'__130 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> ()
d__'8801'_'10815'__130 = erased
-- Axiom.Set.Rel.Rel
d_Rel_264 :: MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> () -> ()
d_Rel_264 = erased
-- Axiom.Set.Rel.relatedˡ
d_related'737'_286 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> AgdaAny -> AgdaAny
d_related'737'_286 v0 ~v1 ~v2 = du_related'737'_286 v0
du_related'737'_286 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> AgdaAny -> AgdaAny
du_related'737'_286 v0
  = coe
      MAlonzo.Code.Axiom.Set.du_map_360 v0
      (\ v1 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v1))
-- Axiom.Set.Rel.∅ʳ
d_'8709''691'_288 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> () -> AgdaAny
d_'8709''691'_288 v0 ~v1 ~v2 = du_'8709''691'_288 v0
du_'8709''691'_288 :: MAlonzo.Code.Axiom.Set.T_Theory_82 -> AgdaAny
du_'8709''691'_288 v0
  = coe MAlonzo.Code.Axiom.Set.du_'8709'_404 (coe v0)
-- Axiom.Set.Rel.dom
d_dom_290 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> AgdaAny -> AgdaAny
d_dom_290 v0 ~v1 ~v2 = du_dom_290 v0
du_dom_290 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> AgdaAny -> AgdaAny
du_dom_290 v0
  = coe
      MAlonzo.Code.Axiom.Set.du_map_360 v0
      (\ v1 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v1))
-- Axiom.Set.Rel.range
d_range_292 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> AgdaAny -> AgdaAny
d_range_292 v0 ~v1 ~v2 = du_range_292 v0
du_range_292 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> AgdaAny -> AgdaAny
du_range_292 v0
  = coe
      MAlonzo.Code.Axiom.Set.du_map_360 v0
      (\ v1 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v1))
-- Axiom.Set.Rel.disjoint-dom⇒disjoint
d_disjoint'45'dom'8658'disjoint_294 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_disjoint'45'dom'8658'disjoint_294 = erased
-- Axiom.Set.Rel._∣'_
d__'8739'''__300 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8739'''__300 v0 ~v1 ~v2 ~v3 v4 v5 = du__'8739'''__300 v0 v4 v5
du__'8739'''__300 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> AgdaAny -> AgdaAny -> AgdaAny
du__'8739'''__300 v0 v1 v2
  = coe
      MAlonzo.Code.Axiom.Set.du_filter_382 v0
      (coe
         MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
         (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased v2
         (\ v3 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3)))
      v1
-- Axiom.Set.Rel._↾'_
d__'8638'''__308 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8638'''__308 v0 ~v1 ~v2 ~v3 v4 v5 = du__'8638'''__308 v0 v4 v5
du__'8638'''__308 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> AgdaAny -> AgdaAny -> AgdaAny
du__'8638'''__308 v0 v1 v2
  = coe
      MAlonzo.Code.Axiom.Set.du_filter_382 v0
      (coe
         MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
         (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased v2
         (\ v3 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v3)))
      v1
-- Axiom.Set.Rel.impl⇒res⊆
d_impl'8658'res'8838'_326 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_impl'8658'res'8838'_326 v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7 v8 v9 v10
  = du_impl'8658'res'8838'_326 v0 v3 v6 v7 v8 v9 v10
du_impl'8658'res'8838'_326 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
du_impl'8658'res'8838'_326 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8314'''_200
      v0 v1
      (coe
         MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
         (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased v3
         (\ v7 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7)))
      v5
      (coe
         MAlonzo.Code.Data.Product.Base.du_map'8321'_114
         (coe v4 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)))
         (coe
            MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
            v0 v1
            (coe
               MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
               (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased v2
               (\ v7 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7)))
            v5 v6))
-- Axiom.Set.Rel.impl⇒cores⊆
d_impl'8658'cores'8838'_348 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_impl'8658'cores'8838'_348 v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7 v8 v9 v10
  = du_impl'8658'cores'8838'_348 v0 v3 v6 v7 v8 v9 v10
du_impl'8658'cores'8838'_348 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
du_impl'8658'cores'8838'_348 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8314'''_200
      v0 v1
      (coe
         MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
         (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased v3
         (\ v7 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v7)))
      v5
      (coe
         MAlonzo.Code.Data.Product.Base.du_map'8321'_114
         (coe v4 (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))
         (coe
            MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
            v0 v1
            (coe
               MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
               (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased v2
               (\ v7 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v7)))
            v5 v6))
-- Axiom.Set.Rel.mapˡ
d_map'737'_358 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map'737'_358 v0 ~v1 ~v2 ~v3 v4 v5 = du_map'737'_358 v0 v4 v5
du_map'737'_358 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_map'737'_358 v0 v1 v2
  = coe
      MAlonzo.Code.Axiom.Set.du_map_360 v0
      (coe MAlonzo.Code.Data.Product.Base.du_map'8321'_114 (coe v1)) v2
-- Axiom.Set.Rel.mapʳ
d_map'691'_364 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map'691'_364 v0 ~v1 ~v2 ~v3 v4 v5 = du_map'691'_364 v0 v4 v5
du_map'691'_364 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_map'691'_364 v0 v1 v2
  = coe
      MAlonzo.Code.Axiom.Set.du_map_360 v0
      (coe
         MAlonzo.Code.Data.Product.Base.du_map'8322'_126 (coe (\ v3 -> v1)))
      v2
-- Axiom.Set.Rel.dom∈
d_dom'8712'_374 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_dom'8712'_374 v0 ~v1 ~v2 v3 v4 = du_dom'8712'_374 v0 v3 v4
du_dom'8712'_374 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_dom'8712'_374 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
      (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
      (coe
         MAlonzo.Code.Function.Related.Propositional.du_SK'45'sym_168
         (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_88)
         (coe
            MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v1)
            (coe (\ v3 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3)))
            (coe v2)))
      (coe
         MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
         (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
         (coe
            MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
            (coe
               (\ v3 ->
                  case coe v3 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                      -> case coe v4 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                             -> case coe v5 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
                                    -> coe
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v7)
                                         (coe v9)
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError))
            (coe
               (\ v3 ->
                  case coe v3 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                           (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v4))
                           (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased (coe v5))
                    _ -> MAlonzo.RTE.mazUnreachableError)))
         (coe
            MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
            (coe
               MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)))
-- Axiom.Set.Rel.dom-⊆mapʳ
d_dom'45''8838'map'691'_402 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_dom'45''8838'map'691'_402 v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_dom'45''8838'map'691'_402 v0 v4 v5 v6 v7
du_dom'45''8838'map'691'_402 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_dom'45''8838'map'691'_402 v0 v1 v2 v3 v4
  = let v5
          = coe
              MAlonzo.Code.Function.Bundles.d_to_938
              (coe du_dom'8712'_374 (coe v0) (coe v1) (coe v3)) v4 in
    case coe v5 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
        -> coe
             MAlonzo.Code.Function.Bundles.d_from_940
             (coe
                du_dom'8712'_374 (coe v0)
                (coe
                   MAlonzo.Code.Axiom.Set.du_map_360 v0
                   (coe
                      MAlonzo.Code.Data.Product.Base.du_map_104 (coe (\ v8 -> v8))
                      (coe (\ v8 -> v2)))
                   v1)
                (coe v3))
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2 v6)
                (coe
                   MAlonzo.Code.Function.Bundles.d_to_938
                   (coe
                      MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v1)
                      (coe
                         MAlonzo.Code.Data.Product.Base.du_map_104 (coe (\ v8 -> v8))
                         (coe (\ v8 -> v2)))
                      (coe
                         MAlonzo.Code.Data.Product.Base.du_map_104 (coe (\ v8 -> v8))
                         (coe (\ v8 -> v2))
                         (coe
                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v6))))
                   (coe
                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v6))
                      (coe
                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased (coe v7)))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.dom-mapʳ⊆
d_dom'45'map'691''8838'_426 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_dom'45'map'691''8838'_426 v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_dom'45'map'691''8838'_426 v0 v4 v5 v6 v7
du_dom'45'map'691''8838'_426 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_dom'45'map'691''8838'_426 v0 v1 v2 v3 v4
  = let v5
          = coe
              MAlonzo.Code.Function.Bundles.d_to_938
              (coe
                 du_dom'8712'_374 (coe v0)
                 (coe du_map'691'_364 (coe v0) (coe v2) (coe v1)) (coe v3))
              v4 in
    case coe v5 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
        -> let v8
                 = coe
                     MAlonzo.Code.Function.Bundles.d_from_940
                     (coe
                        MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v1)
                        (coe
                           MAlonzo.Code.Data.Product.Base.du_map_104 (coe (\ v8 -> v8))
                           (coe (\ v8 -> v2)))
                        (coe
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v6)))
                     v7 in
           case coe v8 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v9 v10
               -> case coe v9 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
                      -> case coe v10 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v13 v14
                             -> coe
                                  MAlonzo.Code.Function.Bundles.d_from_940
                                  (coe du_dom'8712'_374 (coe v0) (coe v1) (coe v11))
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v12)
                                     (coe v14))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.mapʳ-dom
d_map'691''45'dom_452 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_map'691''45'dom_452 v0 ~v1 ~v2 ~v3 v4 v5
  = du_map'691''45'dom_452 v0 v4 v5
du_map'691''45'dom_452 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_map'691''45'dom_452 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_dom'45''8838'map'691'_402 (coe v0) (coe v1) (coe v2))
      (coe du_dom'45'map'691''8838'_426 (coe v0) (coe v1) (coe v2))
-- Axiom.Set.Rel.dom-∅
d_dom'45''8709'_454 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_dom'45''8709'_454 ~v0 ~v1 ~v2 ~v3 ~v4 = du_dom'45''8709'_454
du_dom'45''8709'_454 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_dom'45''8709'_454
  = coe
      MAlonzo.Code.Axiom.Set.Properties.du_'8709''45'least_436
      (coe (\ v0 v1 -> coe MAlonzo.Code.Data.Empty.du_'8869''45'elim_14))
-- Axiom.Set.Rel.mapPartialLiftKey
d_mapPartialLiftKey_462 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_mapPartialLiftKey_462 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_mapPartialLiftKey_462 v4 v5
du_mapPartialLiftKey_462 ::
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_mapPartialLiftKey_462 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe
             MAlonzo.Code.Data.Maybe.Base.du_map_68
             (\ v4 ->
                coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v4))
             (coe v0 v2 v3)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.mapPartialLiftKey-map
d_mapPartialLiftKey'45'map_482 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_mapPartialLiftKey'45'map_482 v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_mapPartialLiftKey'45'map_482 v0 v4 v5 v6 v7 v8
du_mapPartialLiftKey'45'map_482 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_mapPartialLiftKey'45'map_482 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Function.Bundles.d_from_940
              (coe
                 MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v4)
                 (coe du_mapPartialLiftKey_462 (coe v3))
                 (coe
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v2))))
              v5 in
    case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
        -> case coe v7 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v9 v10
               -> case coe v8 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
                      -> let v13 = coe v3 v9 v10 in
                         coe
                           seq (coe v13)
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v10)
                              (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased (coe v12)))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.mapMaybeWithKey
d_mapMaybeWithKey_522 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () -> (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_mapMaybeWithKey_522 v0 ~v1 ~v2 ~v3 v4 v5
  = du_mapMaybeWithKey_522 v0 v4 v5
du_mapMaybeWithKey_522 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
du_mapMaybeWithKey_522 v0 v1 v2
  = coe
      MAlonzo.Code.Axiom.Set.du_mapPartial_538 v0
      (coe du_mapPartialLiftKey_462 (coe v1)) v2
-- Axiom.Set.Rel.∈-mapMaybeWithKey
d_'8712''45'mapMaybeWithKey_538 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8712''45'mapMaybeWithKey_538 v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8
  = du_'8712''45'mapMaybeWithKey_538 v0 v4 v5 v6 v7 v8
du_'8712''45'mapMaybeWithKey_538 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8712''45'mapMaybeWithKey_538 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Function.Bundles.d_to_938
              (coe
                 MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0)
                 (coe du_mapMaybeWithKey_522 (coe v0) (coe v3) (coe v4))
                 (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16)
                 (coe
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v2))))
              (coe
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v2))
                 (coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased (coe v5))) in
    coe
      du_mapPartialLiftKey'45'map_482 (coe v0) (coe v1) (coe v2) (coe v3)
      (coe v4)
      (coe
         MAlonzo.Code.Axiom.Set.du_'8838''45'mapPartial_566 (coe v0)
         (coe v4) (coe du_mapPartialLiftKey_462 (coe v3))
         (coe
            MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
            (coe
               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v2)))
         (coe v6))
-- Axiom.Set.Rel.Restriction._∣_
d__'8739'__568 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8739'__568 v0 ~v1 v2 ~v3 v4 v5 = du__'8739'__568 v0 v2 v4 v5
du__'8739'__568 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'8739'__568 v0 v1 v2 v3
  = coe du__'8739'''__300 (coe v0) (coe v2) (coe v1 v3)
-- Axiom.Set.Rel.Restriction._∣_ᶜ
d__'8739'_'7580'_574 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8739'_'7580'_574 v0 ~v1 v2 ~v3 v4 v5
  = du__'8739'_'7580'_574 v0 v2 v4 v5
du__'8739'_'7580'_574 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'8739'_'7580'_574 v0 v1 v2 v3
  = coe
      du__'8739'''__300 (coe v0) (coe v2)
      (coe
         MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
         (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
         (coe v1 v3))
-- Axiom.Set.Rel.Restriction._⟪$⟫_
d__'10218''36''10219'__580 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'10218''36''10219'__580 v0 ~v1 v2 ~v3 v4 v5
  = du__'10218''36''10219'__580 v0 v2 v4 v5
du__'10218''36''10219'__580 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'10218''36''10219'__580 v0 v1 v2 v3
  = coe
      du_range_292 v0
      (coe du__'8739'__568 (coe v0) (coe v1) (coe v2) (coe v3))
-- Axiom.Set.Rel.Restriction.res-cong
d_res'45'cong_588 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_res'45'cong_588 v0 ~v1 v2 ~v3 v4 v5 v6 v7
  = du_res'45'cong_588 v0 v2 v4 v5 v6 v7
du_res'45'cong_588 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_res'45'cong_588 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                (\ v8 v9 ->
                   coe
                     MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8314'''_200
                     v0 v2
                     (coe
                        MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                        (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                        (coe v1 v4)
                        (\ v10 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                     v8
                     (coe
                        MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                        (coe v6 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v8)))
                        (coe
                           MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
                           v0 v2
                           (coe
                              MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                              (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                              (coe v1 v3)
                              (\ v10 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                           v8 v9))))
             (coe
                (\ v8 v9 ->
                   coe
                     MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8314'''_200
                     v0 v2
                     (coe
                        MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                        (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                        (coe v1 v3)
                        (\ v10 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                     v8
                     (coe
                        MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                        (coe v7 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v8)))
                        (coe
                           MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
                           v0 v2
                           (coe
                              MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                              (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                              (coe v1 v4)
                              (\ v10 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                           v8 v9))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.Restriction.res-dom
d_res'45'dom_598 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_res'45'dom_598 v0 ~v1 v2 ~v3 v4 v5 v6 v7
  = du_res'45'dom_598 v0 v2 v4 v5 v6 v7
du_res'45'dom_598 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_res'45'dom_598 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'map'8315'''_184 v0
              (\ v6 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v6))
              (coe du__'8739'__568 (coe v0) (coe v1) (coe v2) (coe v3)) v4 v5 in
    case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
        -> case coe v8 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v9 v10
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                    (coe
                       MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
                       v0 v2
                       (coe
                          MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                          (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                          (coe v1 v3)
                          (\ v11 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v11)))
                       v7 v10)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.Restriction.res-domᵐ
d_res'45'dom'7504'_610 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_res'45'dom'7504'_610 v0 ~v1 v2 ~v3 v4 v5 v6 v7
  = du_res'45'dom'7504'_610 v0 v2 v4 v5 v6 v7
du_res'45'dom'7504'_610 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_res'45'dom'7504'_610 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'map'8315'''_184 v0
              (\ v6 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v6))
              (coe du__'8739'__568 (coe v0) (coe v1) (coe v2) (coe v3)) v4 v5 in
    case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
        -> case coe v8 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v9 v10
               -> coe
                    MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'map'8314'''''_154
                    (coe v0)
                    (coe (\ v11 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v11)))
                    (coe v2) (coe v7)
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                       (coe
                          MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
                          v0 v2
                          (coe
                             MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                             (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                             (coe v1 v3)
                             (\ v11 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v11)))
                          v7 v10))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.Restriction.res-comp-cong
d_res'45'comp'45'cong_624 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_res'45'comp'45'cong_624 v0 ~v1 v2 ~v3 v4 v5 v6 v7
  = du_res'45'comp'45'cong_624 v0 v2 v4 v5 v6 v7
du_res'45'comp'45'cong_624 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_res'45'comp'45'cong_624 v0 v1 v2 v3 v4 v5
  = case coe v5 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                (\ v8 v9 ->
                   coe
                     MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8314'''_200
                     v0 v2
                     (coe
                        MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                        (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                        (coe
                           MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
                           (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                           (coe v1 v4))
                        (\ v10 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                     v8
                     (coe
                        MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                        (\ v10 v11 ->
                           coe
                             v10
                             (coe v7 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v8)) v11))
                        (coe
                           MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
                           v0 v2
                           (coe
                              MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                              (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                              (coe
                                 MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
                                 (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                 (coe v1 v3))
                              (\ v10 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                           v8 v9))))
             (coe
                (\ v8 v9 ->
                   coe
                     MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8314'''_200
                     v0 v2
                     (coe
                        MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                        (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                        (coe
                           MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
                           (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                           (coe v1 v3))
                        (\ v10 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                     v8
                     (coe
                        MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                        (\ v10 v11 ->
                           coe
                             v10
                             (coe v6 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v8)) v11))
                        (coe
                           MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
                           v0 v2
                           (coe
                              MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                              (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                              (coe
                                 MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
                                 (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                 (coe v1 v4))
                              (\ v10 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                           v8 v9))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.Restriction.res-comp-dom
d_res'45'comp'45'dom_640 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_res'45'comp'45'dom_640 = erased
-- Axiom.Set.Rel.Restriction.res-comp-domᵐ
d_res'45'comp'45'dom'7504'_652 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_res'45'comp'45'dom'7504'_652 v0 ~v1 v2 ~v3 v4 v5 v6 v7
  = du_res'45'comp'45'dom'7504'_652 v0 v2 v4 v5 v6 v7
du_res'45'comp'45'dom'7504'_652 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_res'45'comp'45'dom'7504'_652 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'map'8315'''_184 v0
              (\ v6 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v6))
              (coe du__'8739'_'7580'_574 (coe v0) (coe v1) (coe v2) (coe v3)) v4
              v5 in
    case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
        -> case coe v8 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v9 v10
               -> coe
                    MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'map'8314'''''_154
                    (coe v0)
                    (coe (\ v11 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v11)))
                    (coe v2) (coe v7)
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                       (coe
                          MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
                          v0 v2
                          (coe
                             MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                             (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                             (coe
                                MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
                                (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                (coe v1 v3))
                             (\ v11 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v11)))
                          v7 v10))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.Restriction.res-⊆
d_res'45''8838'_664 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_res'45''8838'_664 v0 ~v1 v2 ~v3 v4 v5 v6
  = du_res'45''8838'_664 v0 v2 v4 v5 v6
du_res'45''8838'_664 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
du_res'45''8838'_664 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))
      (coe
         MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
         (coe v0) (coe v2)
         (coe
            MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
            (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
            (coe v1 v3)
            (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)))
         (coe v4))
-- Axiom.Set.Rel.Restriction.ex-⊆
d_ex'45''8838'_666 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_ex'45''8838'_666 v0 ~v1 v2 ~v3 v4 v5 v6
  = du_ex'45''8838'_666 v0 v2 v4 v5 v6
du_ex'45''8838'_666 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
du_ex'45''8838'_666 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))
      (coe
         MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
         (coe v0) (coe v2)
         (coe
            MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
            (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
            (coe
               MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
               (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
               (coe v1 v3))
            (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)))
         (coe v4))
-- Axiom.Set.Rel.Restriction.res-∅
d_res'45''8709'_668 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_res'45''8709'_668 ~v0 ~v1 ~v2 ~v3 ~v4 = du_res'45''8709'_668
du_res'45''8709'_668 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_res'45''8709'_668 = coe du_dom'45''8709'_454
-- Axiom.Set.Rel.Restriction.res-ex-∪
d_res'45'ex'45''8746'_672 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  () ->
  AgdaAny ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_res'45'ex'45''8746'_672 v0 ~v1 v2 v3 ~v4 v5 v6
  = du_res'45'ex'45''8746'_672 v0 v2 v3 v5 v6
du_res'45'ex'45''8746'_672 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_res'45'ex'45''8746'_672 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         MAlonzo.Code.Axiom.Set.Properties.du_'8746''45''8838'_558 (coe v0)
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
            (coe
               MAlonzo.Code.Axiom.Set.d_specification_174 v0 erased erased v3
               (coe
                  MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                  (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                  (coe v1 v2)
                  (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)))))
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
            (coe
               MAlonzo.Code.Axiom.Set.d_specification_174 v0 erased erased v3
               (coe
                  MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                  (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                  (coe
                     MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
                     (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                     (coe v1 v2))
                  (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)))))
         (coe du_res'45''8838'_664 (coe v0) (coe v1) (coe v3) (coe v2))
         (coe du_ex'45''8838'_666 (coe v0) (coe v1) (coe v3) (coe v2)))
      (coe
         (\ v5 v6 ->
            let v7
                  = coe v4 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)) in
            case coe v7 of
              MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v8 v9
                -> if coe v8
                     then case coe v9 of
                            MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v10
                              -> coe
                                   MAlonzo.Code.Axiom.Set.Properties.du_'8712''45''8746''8314'_208
                                   v0
                                   (coe
                                      MAlonzo.Code.Axiom.Set.du_filter_382 v0
                                      (coe
                                         MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                                         (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                         erased (coe v1 v2)
                                         (\ v11 ->
                                            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v11)))
                                      v3)
                                   (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                      (coe
                                         MAlonzo.Code.Axiom.Set.d_specification_174 v0 erased erased
                                         v3
                                         (coe
                                            MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                                            (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                            erased
                                            (coe
                                               MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
                                               (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased
                                               erased (coe v1 v2))
                                            (\ v11 ->
                                               MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                 (coe v11)))))
                                   v5
                                   (coe
                                      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
                                      (coe
                                         MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8314'''_200
                                         v0 v3
                                         (coe
                                            MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                                            (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                            erased (coe v1 v2)
                                            (\ v11 ->
                                               MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v11)))
                                         v5
                                         (coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v10)
                                            (coe v6))))
                            _ -> MAlonzo.RTE.mazUnreachableError
                     else coe
                            seq (coe v9)
                            (coe
                               MAlonzo.Code.Axiom.Set.Properties.du_'8712''45''8746''8314'_208 v0
                               (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                  (coe
                                     MAlonzo.Code.Axiom.Set.d_specification_174 v0 erased erased v3
                                     (coe
                                        MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                                        (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                        erased (coe v1 v2)
                                        (\ v10 ->
                                           MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))))
                               (coe
                                  MAlonzo.Code.Axiom.Set.du_filter_382 v0
                                  (coe
                                     MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                                     (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                                     (coe
                                        MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
                                        (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                        (coe v1 v2))
                                     (\ v10 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                                  v3)
                               v5
                               (coe
                                  MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                                  (coe
                                     MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8314'''_200
                                     v0 v3
                                     (coe
                                        MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                                        (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                        erased
                                        (coe
                                           MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
                                           (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                           (coe v1 v2))
                                        (\ v10 ->
                                           MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v10)))
                                     v5
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased
                                        (coe v6)))))
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Axiom.Set.Rel.Restriction.res-ex-disjoint
d_res'45'ex'45'disjoint_686 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_res'45'ex'45'disjoint_686 = erased
-- Axiom.Set.Rel.Restriction.res-ex-disj-∪
d_res'45'ex'45'disj'45''8746'_694 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  () ->
  AgdaAny ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_res'45'ex'45'disj'45''8746'_694 v0 ~v1 v2 v3 ~v4 v5 v6
  = du_res'45'ex'45'disj'45''8746'_694 v0 v2 v3 v5 v6
du_res'45'ex'45'disj'45''8746'_694 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_res'45'ex'45'disj'45''8746'_694 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (let v5
             = coe
                 du_res'45'ex'45''8746'_672 (coe v0) (coe v1) (coe v2) (coe v3)
                 (coe v4) in
       case coe v5 of
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
           -> coe
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v7) (coe v6)
         _ -> MAlonzo.RTE.mazUnreachableError)
      erased
-- Axiom.Set.Rel.Restriction.curryʳ
d_curry'691'_702 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d_curry'691'_702 v0 ~v1 v2 ~v3 ~v4 v5 v6
  = du_curry'691'_702 v0 v2 v5 v6
du_curry'691'_702 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_curry'691'_702 v0 v1 v2 v3
  = coe
      du_map'737'_358 (coe v0)
      (coe (\ v4 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v4)))
      (coe
         du__'8739'''__300 (coe v0) (coe v2)
         (coe
            MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
            (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
            (coe
               v1
               (coe MAlonzo.Code.Axiom.Set.du_'10100'_'10101'_414 v0 erased v3))
            (\ v4 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v4))))
-- Axiom.Set.Rel.Restriction.∈-curryʳ
d_'8712''45'curry'691'_714 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  () ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8712''45'curry'691'_714 v0 ~v1 v2 ~v3 ~v4 v5 v6 v7 v8 v9
  = du_'8712''45'curry'691'_714 v0 v2 v5 v6 v7 v8 v9
du_'8712''45'curry'691'_714 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8712''45'curry'691'_714 v0 v1 v2 v3 v4 v5 v6
  = let v7
          = coe
              MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'map'8315'''_184 v0
              (coe
                 MAlonzo.Code.Data.Product.Base.du_map'8321'_114
                 (coe (\ v7 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v7))))
              (coe
                 du__'8739'''__300 (coe v0) (coe v2)
                 (coe
                    MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                    (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                    (coe
                       v1
                       (coe MAlonzo.Code.Axiom.Set.du_'10100'_'10101'_414 v0 erased v3))
                    (\ v7 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7))))
              (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4) (coe v5))
              v6 in
    case coe v7 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
        -> case coe v8 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v10 v11
               -> coe
                    seq (coe v10)
                    (case coe v9 of
                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v12 v13
                         -> let v14
                                  = coe
                                      MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
                                      v0 v2
                                      (coe
                                         MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                                         (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                         erased
                                         (coe
                                            MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                                            (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
                                            erased
                                            (coe
                                               v1
                                               (coe
                                                  MAlonzo.Code.Axiom.Set.du_'10100'_'10101'_414 v0
                                                  erased v3))
                                            (\ v14 ->
                                               MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v14)))
                                         (\ v14 ->
                                            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v14)))
                                      v8 v13 in
                            case coe v14 of
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v15 v16 -> coe v16
                              _ -> MAlonzo.RTE.mazUnreachableError
                       _ -> MAlonzo.RTE.mazUnreachableError)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.Restriction._._∩_
d__'8745'__738 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__738 v0 ~v1 v2 = du__'8745'__738 v0 v2
du__'8745'__738 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'8745'__738 v0 v1
  = coe MAlonzo.Code.Axiom.Set.du__'8745'__666 (coe v0) (coe v1)
-- Axiom.Set.Rel.Restriction.res-dom-comm⊆∩
d_res'45'dom'45'comm'8838''8745'_778 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () -> () -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_res'45'dom'45'comm'8838''8745'_778 v0 ~v1 v2 ~v3 ~v4 v5 v6 v7 v8
  = du_res'45'dom'45'comm'8838''8745'_778 v0 v2 v5 v6 v7 v8
du_res'45'dom'45'comm'8838''8745'_778 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_res'45'dom'45'comm'8838''8745'_778 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Function.Bundles.d_to_938
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45''8745'_674 (coe v0) (coe v1)
         (coe du_dom_290 v0 v2) (coe du_dom_290 v0 v3) (coe v4))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            du_res'45'dom'7504'_610 (coe v0) (coe v1) (coe v2)
            (coe du_dom_290 v0 v3) (coe v4) (coe v5))
         (coe
            du_res'45'dom_598 (coe v0) (coe v1) (coe v2) (coe du_dom_290 v0 v3)
            (coe v4) (coe v5)))
-- Axiom.Set.Rel.Restriction.res-dom-comm∩⊆
d_res'45'dom'45'comm'8745''8838'_786 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () -> () -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_res'45'dom'45'comm'8745''8838'_786 v0 ~v1 v2 ~v3 ~v4 v5 v6 v7 v8
  = du_res'45'dom'45'comm'8745''8838'_786 v0 v2 v5 v6 v7 v8
du_res'45'dom'45'comm'8745''8838'_786 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_res'45'dom'45'comm'8745''8838'_786 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Function.Bundles.d_from_940
              (coe
                 MAlonzo.Code.Axiom.Set.du_'8712''45''8745'_674 (coe v0) (coe v1)
                 (coe du_dom_290 v0 v2) (coe du_dom_290 v0 v3) (coe v4))
              v5 in
    case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
        -> let v9
                 = coe
                     MAlonzo.Code.Function.Bundles.d_to_938
                     (coe du_dom'8712'_374 (coe v0) (coe v2) (coe v4)) v7 in
           let v10
                 = coe
                     MAlonzo.Code.Function.Bundles.d_to_938
                     (coe du_dom'8712'_374 (coe v0) (coe v3) (coe v4)) v8 in
           case coe v9 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
               -> coe
                    seq (coe v10)
                    (coe
                       MAlonzo.Code.Function.Bundles.d_from_940
                       (coe
                          du_dom'8712'_374 (coe v0)
                          (coe
                             MAlonzo.Code.Axiom.Set.du_filter_382 v0
                             (coe
                                MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                                (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                                (coe
                                   v1
                                   (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                      (coe
                                         MAlonzo.Code.Axiom.Set.d_replacement_196 v0 erased erased
                                         (\ v13 ->
                                            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v13))
                                         v3)))
                                (\ v13 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v13)))
                             v2)
                          (coe v4))
                       (coe
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v11)
                          (coe
                             MAlonzo.Code.Function.Bundles.d_to_938
                             (coe
                                MAlonzo.Code.Axiom.Set.du_'8712''45'filter_388 (coe v0) (coe v2)
                                (coe
                                   MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
                                   (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
                                   (coe
                                      v1
                                      (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                         (coe
                                            MAlonzo.Code.Axiom.Set.d_replacement_196 v0 erased
                                            erased
                                            (\ v13 ->
                                               MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v13))
                                            v3)))
                                   (\ v13 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v13)))
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4) (coe v11)))
                             (coe
                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v8) (coe v12)))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Rel.Restriction.res-dom-comm'
d_res'45'dom'45'comm''_834 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_res'45'dom'45'comm''_834 v0 ~v1 v2 ~v3 ~v4 v5 v6
  = du_res'45'dom'45'comm''_834 v0 v2 v5 v6
du_res'45'dom'45'comm''_834 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_res'45'dom'45'comm''_834 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_res'45'dom'45'comm'8838''8745'_778 (coe v0) (coe v1) (coe v2)
         (coe v3))
      (coe
         du_res'45'dom'45'comm'8745''8838'_786 (coe v0) (coe v1) (coe v2)
         (coe v3))
-- Axiom.Set.Rel.Restriction.res-dom-comm
d_res'45'dom'45'comm_840 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_res'45'dom'45'comm_840 v0 ~v1 v2 ~v3 ~v4 v5 v6
  = du_res'45'dom'45'comm_840 v0 v2 v5 v6
du_res'45'dom'45'comm_840 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_res'45'dom'45'comm_840 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         (coe
            MAlonzo.Code.Axiom.Set.Properties.du_'8801''7497''45'Setoid_292)
         (coe
            du_dom_290 v0
            (coe
               du__'8739'__568 (coe v0) (coe v1) (coe v2) (coe du_dom_290 v0 v3)))
         (coe
            MAlonzo.Code.Axiom.Set.du__'8745'__666 (coe v0) (coe v1)
            (coe du_dom_290 v0 v2) (coe du_dom_290 v0 v3))
         (coe
            du_dom_290 v0
            (coe
               du__'8739'__568 (coe v0) (coe v1) (coe v3) (coe du_dom_290 v0 v2)))
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (coe
               MAlonzo.Code.Axiom.Set.Properties.du_'8801''7497''45'Setoid_292)
            (coe
               MAlonzo.Code.Axiom.Set.du__'8745'__666 (coe v0) (coe v1)
               (coe du_dom_290 v0 v2) (coe du_dom_290 v0 v3))
            (coe
               MAlonzo.Code.Axiom.Set.du__'8745'__666 (coe v0) (coe v1)
               (coe du_dom_290 v0 v3) (coe du_dom_290 v0 v2))
            (coe
               du_dom_290 v0
               (coe
                  du__'8739'__568 (coe v0) (coe v1) (coe v3) (coe du_dom_290 v0 v2)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
               (coe
                  MAlonzo.Code.Axiom.Set.Properties.du_'8801''7497''45'Setoid_292)
               (coe
                  MAlonzo.Code.Axiom.Set.du__'8745'__666 (coe v0) (coe v1)
                  (coe du_dom_290 v0 v3) (coe du_dom_290 v0 v2))
               (coe
                  du_dom_290 v0
                  (coe
                     du__'8739'__568 (coe v0) (coe v1) (coe v3) (coe du_dom_290 v0 v2)))
               (coe
                  du_dom_290 v0
                  (coe
                     du__'8739'__568 (coe v0) (coe v1) (coe v3) (coe du_dom_290 v0 v2)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     (\ v4 ->
                        coe
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe (\ v5 v6 -> v6))
                          (coe (\ v5 v6 -> v6))))
                  (coe
                     du_dom_290 v0
                     (coe
                        du__'8739'__568 (coe v0) (coe v1) (coe v3)
                        (coe du_dom_290 v0 v2))))
               (coe
                  du_res'45'dom'45'comm''_834 (coe v0) (coe v1) (coe v3) (coe v2)))
            (coe
               MAlonzo.Code.Axiom.Set.Properties.du_'8745''45'sym_694 (coe v0)
               (coe v1) (coe du_dom_290 v0 v3) (coe du_dom_290 v0 v2)))
         (coe
            du_res'45'dom'45'comm''_834 (coe v0) (coe v1) (coe v2) (coe v3)))
-- Axiom.Set.Rel.Corestriction._↾_
d__'8638'__880 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8638'__880 v0 ~v1 v2 ~v3 v4 v5 = du__'8638'__880 v0 v2 v4 v5
du__'8638'__880 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'8638'__880 v0 v1 v2 v3
  = coe du__'8638'''__308 (coe v0) (coe v2) (coe v1 v3)
-- Axiom.Set.Rel.Corestriction._↾_ᶜ
d__'8638'_'7580'_886 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8638'_'7580'_886 v0 ~v1 v2 ~v3 v4 v5
  = du__'8638'_'7580'_886 v0 v2 v4 v5
du__'8638'_'7580'_886 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'8638'_'7580'_886 v0 v1 v2 v3
  = coe
      du__'8638'''__308 (coe v0) (coe v2)
      (coe
         MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
         (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
         (coe v1 v3))
-- Axiom.Set.Rel.Corestriction.cores-⊆
d_cores'45''8838'_892 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_cores'45''8838'_892 v0 ~v1 v2 ~v3 v4 v5 v6
  = du_cores'45''8838'_892 v0 v2 v4 v5 v6
du_cores'45''8838'_892 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
du_cores'45''8838'_892 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))
      (coe
         MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
         (coe v0) (coe v2)
         (coe
            MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
            (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
            (coe v1 v3)
            (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))
         (coe v4))
-- Axiom.Set.Rel.Corestriction.coex-⊆
d_coex'45''8838'_894 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
d_coex'45''8838'_894 v0 ~v1 v2 ~v3 v4 v5 v6
  = du_coex'45''8838'_894 v0 v2 v4 v5 v6
du_coex'45''8838'_894 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> AgdaAny
du_coex'45''8838'_894 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))
      (coe
         MAlonzo.Code.Axiom.Set.Properties.du_'8712''45'filter'8315'''_166
         (coe v0) (coe v2)
         (coe
            MAlonzo.Code.Axiom.Set.d_sp'45''8728'_68
            (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased erased
            (coe
               MAlonzo.Code.Axiom.Set.d_sp'45''172'_70
               (MAlonzo.Code.Axiom.Set.d_sp_150 (coe v0)) erased erased
               (coe v1 v3))
            (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))
         (coe v4))
