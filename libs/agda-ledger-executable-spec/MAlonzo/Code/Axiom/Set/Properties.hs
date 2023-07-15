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

module MAlonzo.Code.Axiom.Set.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Data.Empty
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Ext.Properties
import qualified MAlonzo.Code.Data.List.Membership.DecSetoid
import qualified MAlonzo.Code.Data.List.Membership.Propositional.Properties
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Lattice.Structures
import qualified MAlonzo.Code.Relation.Binary.Morphism.Structures
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Axiom.Set.Properties._._Preservesˢ₂_
d__Preserves'738''8322'__14 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738''8322'__14 = erased
-- Axiom.Set.Properties._._∉_
d__'8713'__18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'8713'__18 = erased
-- Axiom.Set.Properties._._∪_
d__'8746'__20 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__20 ~v0 v1 = du__'8746'__20 v1
du__'8746'__20 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8746'__20 v0 v1 v2 v3
  = coe MAlonzo.Code.Axiom.Set.du__'8746'__642 (coe v0) v2 v3
-- Axiom.Set.Properties._._≡ᵉ_
d__'8801''7497'__22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'__22 = erased
-- Axiom.Set.Properties._._≡ᵉ'_
d__'8801''7497'''__24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'''__24 = erased
-- Axiom.Set.Properties._._⊆_
d__'8838'__26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'8838'__26 = erased
-- Axiom.Set.Properties._.card
d_card_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_card_40 ~v0 ~v1 = du_card_40
du_card_40 ::
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
du_card_40 v0 v1 = coe MAlonzo.Code.Axiom.Set.du_card_298 v1
-- Axiom.Set.Properties._.disjoint
d_disjoint_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> ()
d_disjoint_46 = erased
-- Axiom.Set.Properties._.filter
d_filter_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d_filter_48 ~v0 v1 = du_filter_48 v1
du_filter_48 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
du_filter_48 v0 v1 v2
  = coe MAlonzo.Code.Axiom.Set.du_filter_382 (coe v0)
-- Axiom.Set.Properties._.finite
d_finite_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> AgdaAny -> ()
d_finite_50 = erased
-- Axiom.Set.Properties._.fromList
d_fromList_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> [AgdaAny] -> AgdaAny
d_fromList_52 ~v0 v1 = du_fromList_52 v1
du_fromList_52 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> [AgdaAny] -> AgdaAny
du_fromList_52 v0 v1 v2
  = coe MAlonzo.Code.Axiom.Set.du_fromList_390 (coe v0) v2
-- Axiom.Set.Properties._.map
d_map_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map_58 ~v0 v1 = du_map_58 v1
du_map_58 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_map_58 v0 v1 v2 = coe MAlonzo.Code.Axiom.Set.du_map_360 (coe v0)
-- Axiom.Set.Properties._.mapPartial
d_mapPartial_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_mapPartial_60 ~v0 v1 = du_mapPartial_60 v1
du_mapPartial_60 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
du_mapPartial_60 v0 v1 v2 v3
  = coe MAlonzo.Code.Axiom.Set.du_mapPartial_538 (coe v0) v3
-- Axiom.Set.Properties._.spec-∈
d_spec'45''8712'_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> ()
d_spec'45''8712'_78 = erased
-- Axiom.Set.Properties._.strongly-finite
d_strongly'45'finite_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> AgdaAny -> ()
d_strongly'45'finite_86 = erased
-- Axiom.Set.Properties._.weakly-finite
d_weakly'45'finite_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> AgdaAny -> ()
d_weakly'45'finite_90 = erased
-- Axiom.Set.Properties._.∅
d_'8709'_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> AgdaAny
d_'8709'_92 ~v0 v1 = du_'8709'_92 v1
du_'8709'_92 :: MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> AgdaAny
du_'8709'_92 v0 v1
  = coe MAlonzo.Code.Axiom.Set.du_'8709'_404 (coe v0)
-- Axiom.Set.Properties._.❴_❵
d_'10100'_'10101'_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> AgdaAny -> AgdaAny
d_'10100'_'10101'_120 ~v0 v1 = du_'10100'_'10101'_120 v1
du_'10100'_'10101'_120 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 -> () -> AgdaAny -> AgdaAny
du_'10100'_'10101'_120 v0
  = coe MAlonzo.Code.Axiom.Set.du_'10100'_'10101'_414 (coe v0)
-- Axiom.Set.Properties._.Intersection._∩_
d__'8745'__124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__124 ~v0 v1 = du__'8745'__124 v1
du__'8745'__124 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'8745'__124 v0 v1 v2 v3 v4
  = coe MAlonzo.Code.Axiom.Set.du__'8745'__666 (coe v0) v2 v3 v4
-- Axiom.Set.Properties._.Intersection.disjoint'
d_disjoint''_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_disjoint''_126 = erased
-- Axiom.Set.Properties.∈-map⁺''
d_'8712''45'map'8314'''''_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8712''45'map'8314'''''_154 ~v0 v1 ~v2 ~v3 v4 v5 v6 v7
  = du_'8712''45'map'8314'''''_154 v1 v4 v5 v6 v7
du_'8712''45'map'8314'''''_154 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8712''45'map'8314'''''_154 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Function.Bundles.d_to_938
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v2)
         (coe v1) (coe v1 v3))
      (coe
         MAlonzo.Code.Data.Product.Base.du_'45''44'__68 (coe v3)
         (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased (coe v4)))
-- Axiom.Set.Properties.∈-filter⁻'
d_'8712''45'filter'8315'''_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8712''45'filter'8315'''_166 ~v0 v1 ~v2 v3 ~v4 v5 v6
  = du_'8712''45'filter'8315'''_166 v1 v3 v5 v6
du_'8712''45'filter'8315'''_166 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8712''45'filter'8315'''_166 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Bundles.d_from_940
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45'filter_388 (coe v0) (coe v1)
         (coe v2) (coe v3))
-- Axiom.Set.Properties.∈-∪⁻
d_'8712''45''8746''8315'_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'8712''45''8746''8315'_174 ~v0 v1 ~v2 v3 v4 v5
  = du_'8712''45''8746''8315'_174 v1 v3 v4 v5
du_'8712''45''8746''8315'_174 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_'8712''45''8746''8315'_174 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Bundles.d_from_940
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45''8746'_650 (coe v0) (coe v1)
         (coe v2) (coe v3))
-- Axiom.Set.Properties.∈-map⁻'
d_'8712''45'map'8315'''_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8712''45'map'8315'''_184 ~v0 v1 ~v2 ~v3 v4 v5 v6
  = du_'8712''45'map'8315'''_184 v1 v4 v5 v6
du_'8712''45'map'8315'''_184 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8712''45'map'8315'''_184 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Bundles.d_from_940
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v2)
         (coe v1) (coe v3))
-- Axiom.Set.Properties.∈-fromList⁻
d_'8712''45'fromList'8315'_190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  [AgdaAny] ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
d_'8712''45'fromList'8315'_190 ~v0 v1 ~v2 v3 v4
  = du_'8712''45'fromList'8315'_190 v1 v3 v4
du_'8712''45'fromList'8315'_190 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  [AgdaAny] ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
du_'8712''45'fromList'8315'_190 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Bundles.d_from_940
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45'fromList_394 (coe v0) (coe v1)
         (coe v2))
-- Axiom.Set.Properties.∈-filter⁺'
d_'8712''45'filter'8314'''_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_'8712''45'filter'8314'''_200 ~v0 v1 ~v2 v3 ~v4 v5 v6
  = du_'8712''45'filter'8314'''_200 v1 v3 v5 v6
du_'8712''45'filter'8314'''_200 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_'8712''45'filter'8314'''_200 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Bundles.d_to_938
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45'filter_388 (coe v0) (coe v1)
         (coe v2) (coe v3))
-- Axiom.Set.Properties.∈-∪⁺
d_'8712''45''8746''8314'_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_'8712''45''8746''8314'_208 ~v0 v1 ~v2 v3 v4 v5
  = du_'8712''45''8746''8314'_208 v1 v3 v4 v5
du_'8712''45''8746''8314'_208 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_'8712''45''8746''8314'_208 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Bundles.d_to_938
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45''8746'_650 (coe v0) (coe v1)
         (coe v2) (coe v3))
-- Axiom.Set.Properties.∈-map⁺'
d_'8712''45'map'8314'''_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_'8712''45'map'8314'''_218 ~v0 v1 ~v2 ~v3 v4 v5 v6
  = du_'8712''45'map'8314'''_218 v1 v4 v5 v6
du_'8712''45'map'8314'''_218 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_'8712''45'map'8314'''_218 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Bundles.d_to_938
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v2)
         (coe v1) (coe v3))
-- Axiom.Set.Properties.∈-fromList⁺
d_'8712''45'fromList'8314'_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 -> AgdaAny
d_'8712''45'fromList'8314'_224 ~v0 v1 ~v2 v3 v4
  = du_'8712''45'fromList'8314'_224 v1 v3 v4
du_'8712''45'fromList'8314'_224 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  [AgdaAny] ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 -> AgdaAny
du_'8712''45'fromList'8314'_224 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Bundles.d_to_938
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45'fromList_394 (coe v0) (coe v1)
         (coe v2))
-- Axiom.Set.Properties._≡_⨿_
d__'8801'_'10815'__232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> ()
d__'8801'_'10815'__232 = erased
-- Axiom.Set.Properties.≡ᵉ⇔≡ᵉ'
d_'8801''7497''8660''8801''7497'''_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8801''7497''8660''8801''7497'''_240 ~v0 ~v1 ~v2 ~v3 ~v4
  = du_'8801''7497''8660''8801''7497'''_240
du_'8801''7497''8660''8801''7497'''_240 ::
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8801''7497''8660''8801''7497'''_240
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
      (coe
         (\ v0 v1 ->
            case coe v0 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
                -> coe
                     MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 (coe v2 v1)
                     (coe v3 v1)
              _ -> MAlonzo.RTE.mazUnreachableError))
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
              (coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_to_938 (coe v0 v1)))
              (coe
                 (\ v1 -> MAlonzo.Code.Function.Bundles.d_from_940 (coe v0 v1)))))
-- Axiom.Set.Properties.cong-⊆⇒cong
d_cong'45''8838''8658'cong_256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cong'45''8838''8658'cong_256 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8
  = du_cong'45''8838''8658'cong_256 v5 v6 v7 v8
du_cong'45''8838''8658'cong_256 ::
  (AgdaAny ->
   AgdaAny ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_cong'45''8838''8658'cong_256 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe v0 v1 v2 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3)))
      (coe v0 v2 v1 (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v3)))
-- Axiom.Set.Properties.cong-⊆⇒cong₂
d_cong'45''8838''8658'cong'8322'_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cong'45''8838''8658'cong'8322'_264 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
                                     v8 v9 v10 v11 v12
  = du_cong'45''8838''8658'cong'8322'_264 v6 v7 v8 v9 v10 v11 v12
du_cong'45''8838''8658'cong'8322'_264 ::
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny -> AgdaAny) ->
   AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_cong'45''8838''8658'cong'8322'_264 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         v0 v1 v2 v3 v4 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5))
         (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v6)))
      (coe
         v0 v2 v1 v4 v3 (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5))
         (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v6)))
-- Axiom.Set.Properties.⊆-Transitive
d_'8838''45'Transitive_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8838''45'Transitive_272 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9
  = du_'8838''45'Transitive_272 v6 v7 v8 v9
du_'8838''45'Transitive_272 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'8838''45'Transitive_272 v0 v1 v2 v3 = coe v1 v2 (coe v0 v2 v3)
-- Axiom.Set.Properties.≡ᵉ-isEquivalence
d_'8801''7497''45'isEquivalence_278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8801''7497''45'isEquivalence_278 ~v0 ~v1 ~v2
  = du_'8801''7497''45'isEquivalence_278
du_'8801''7497''45'isEquivalence_278 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_'8801''7497''45'isEquivalence_278
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe (\ v1 v2 -> v2))
              (coe (\ v1 v2 -> v2))))
      (coe
         (\ v0 v1 v2 ->
            case coe v2 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
                -> coe
                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4) (coe v3)
              _ -> MAlonzo.RTE.mazUnreachableError))
      (coe
         (\ v0 v1 v2 v3 v4 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
              (coe
                 du_'8838''45'Transitive_272
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3))
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v4)))
              (coe
                 du_'8838''45'Transitive_272
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v4))
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v3)))))
-- Axiom.Set.Properties.≡ᵉ-Setoid
d_'8801''7497''45'Setoid_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8801''7497''45'Setoid_292 ~v0 ~v1 ~v2
  = du_'8801''7497''45'Setoid_292
du_'8801''7497''45'Setoid_292 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_'8801''7497''45'Setoid_292
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe du_'8801''7497''45'isEquivalence_278)
-- Axiom.Set.Properties.⊆-isPreorder
d_'8838''45'isPreorder_296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8838''45'isPreorder_296 ~v0 ~v1 ~v2
  = du_'8838''45'isPreorder_296
du_'8838''45'isPreorder_296 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_'8838''45'isPreorder_296
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe du_'8801''7497''45'isEquivalence_278)
      (coe
         (\ v0 v1 v2 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v2)))
      (coe (\ v0 v1 v2 -> coe du_'8838''45'Transitive_272))
-- Axiom.Set.Properties.⊆-Preorder
d_'8838''45'Preorder_304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8838''45'Preorder_304 ~v0 ~v1 ~v2 ~v3
  = du_'8838''45'Preorder_304
du_'8838''45'Preorder_304 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_'8838''45'Preorder_304
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe du_'8838''45'isPreorder_296)
-- Axiom.Set.Properties.⊆-PartialOrder
d_'8838''45'PartialOrder_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8838''45'PartialOrder_308 ~v0 ~v1 ~v2
  = du_'8838''45'PartialOrder_308
du_'8838''45'PartialOrder_308 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_'8838''45'PartialOrder_308
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe du_'8838''45'isPreorder_296)
      (coe (\ v0 v1 -> coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32))
-- Axiom.Set.Properties.∈-×
d_'8712''45''215'_314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8712''45''215'_314 ~v0 v1 ~v2 ~v3 v4 v5 v6 v7
  = du_'8712''45''215'_314 v1 v4 v5 v6 v7
du_'8712''45''215'_314 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8712''45''215'_314 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         MAlonzo.Code.Function.Bundles.d_to_938
         (coe
            MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v1)
            (coe (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)))
            (coe v2))
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
            (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v3))
            (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased (coe v4))))
      (coe
         MAlonzo.Code.Function.Bundles.d_to_938
         (coe
            MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v1)
            (coe (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))
            (coe v3))
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
            (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2) (coe v3))
            (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased (coe v4))))
-- Axiom.Set.Properties.map-⊆∘
d_map'45''8838''8728'_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_map'45''8838''8728'_326 ~v0 v1 ~v2 ~v3 ~v4 v5 v6 v7 v8 v9
  = du_map'45''8838''8728'_326 v1 v5 v6 v7 v8 v9
du_map'45''8838''8728'_326 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_map'45''8838''8728'_326 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Function.Bundles.d_from_940
              (coe
                 MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0)
                 (coe MAlonzo.Code.Axiom.Set.du_map_360 v0 v2 v1) (coe v3) (coe v4))
              v5 in
    case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
        -> case coe v8 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v9 v10
               -> let v11
                        = coe
                            MAlonzo.Code.Function.Bundles.d_from_940
                            (coe
                               MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v1)
                               (coe v2) (coe v7))
                            v10 in
                  case coe v11 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v12 v13
                      -> case coe v13 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v14 v15
                             -> coe
                                  MAlonzo.Code.Function.Bundles.d_to_938
                                  (coe
                                     MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v1)
                                     (coe (\ v16 -> coe v3 (coe v2 v16))) (coe v4))
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v12)
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v9)
                                        (coe v15)))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Properties.map-∘⊆
d_map'45''8728''8838'_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_map'45''8728''8838'_362 ~v0 v1 ~v2 ~v3 ~v4 v5 v6 v7 v8 v9
  = du_map'45''8728''8838'_362 v1 v5 v6 v7 v8 v9
du_map'45''8728''8838'_362 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_map'45''8728''8838'_362 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Function.Bundles.d_from_940
              (coe
                 MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v1)
                 (coe (\ v6 -> coe v3 (coe v2 v6))) (coe v4))
              v5 in
    case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
        -> case coe v8 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v9 v10
               -> coe
                    MAlonzo.Code.Function.Bundles.d_to_938
                    (coe
                       MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0)
                       (coe MAlonzo.Code.Axiom.Set.du_map_360 v0 v2 v1) (coe v3) (coe v4))
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2 v7)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v9)
                          (coe
                             MAlonzo.Code.Function.Bundles.d_to_938
                             (coe
                                MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v1)
                                (coe v2) (coe v2 v7))
                             (coe
                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v7)
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased (coe v10))))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Properties.map-∘
d_map'45''8728'_386 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_map'45''8728'_386 ~v0 v1 ~v2 ~v3 ~v4 v5 v6 v7
  = du_map'45''8728'_386 v1 v5 v6 v7
du_map'45''8728'_386 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_map'45''8728'_386 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_map'45''8838''8728'_326 (coe v0) (coe v1) (coe v2) (coe v3))
      (coe
         du_map'45''8728''8838'_362 (coe v0) (coe v1) (coe v2) (coe v3))
-- Axiom.Set.Properties.map-⊆
d_map'45''8838'_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_map'45''8838'_394 ~v0 v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
  = du_map'45''8838'_394 v1 v4 v5 v6 v7 v8 v9
du_map'45''8838'_394 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_map'45''8838'_394 v0 v1 v2 v3 v4 v5 v6
  = let v7
          = coe
              MAlonzo.Code.Function.Bundles.d_from_940
              (coe
                 MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v1)
                 (coe v3) (coe v5))
              v6 in
    case coe v7 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
        -> case coe v9 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v10 v11
               -> coe
                    MAlonzo.Code.Function.Bundles.d_to_938
                    (coe
                       MAlonzo.Code.Axiom.Set.du_'8712''45'map_368 (coe v0) (coe v2)
                       (coe v3) (coe v5))
                    (coe
                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v8)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v10)
                          (coe v4 v8 v11)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Properties.map-≡ᵉ
d_map'45''8801''7497'_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_map'45''8801''7497'_420 ~v0 v1 ~v2 ~v3 v4 v5 v6 v7
  = du_map'45''8801''7497'_420 v1 v4 v5 v6 v7
du_map'45''8801''7497'_420 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_map'45''8801''7497'_420 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                du_map'45''8838'_394 (coe v0) (coe v1) (coe v2) (coe v3) (coe v5))
             (coe
                du_map'45''8838'_394 (coe v0) (coe v2) (coe v1) (coe v3) (coe v6))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Properties.∉-∅
d_'8713''45''8709'_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8713''45''8709'_428 = erased
-- Axiom.Set.Properties.∅-minimum
d_'8709''45'minimum_432 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8709''45'minimum_432 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'8709''45'minimum_432
du_'8709''45'minimum_432 :: AgdaAny
du_'8709''45'minimum_432
  = coe MAlonzo.Code.Data.Empty.du_'8869''45'elim_14
-- Axiom.Set.Properties.∅-least
d_'8709''45'least_436 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8709''45'least_436 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8709''45'least_436 v4
du_'8709''45'least_436 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8709''45'least_436 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
      (\ v1 v2 -> coe du_'8709''45'minimum_432)
-- Axiom.Set.Properties.∅-weakly-finite
d_'8709''45'weakly'45'finite_440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8709''45'weakly'45'finite_440 ~v0 ~v1 ~v2
  = du_'8709''45'weakly'45'finite_440
du_'8709''45'weakly'45'finite_440 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8709''45'weakly'45'finite_440
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe (\ v0 v1 -> coe MAlonzo.Code.Data.Empty.du_'8869''45'elim_14))
-- Axiom.Set.Properties.∅-finite
d_'8709''45'finite_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8709''45'finite_442 ~v0 ~v1 ~v2 = du_'8709''45'finite_442
du_'8709''45'finite_442 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8709''45'finite_442
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
              (coe (\ v1 -> coe MAlonzo.Code.Data.Empty.du_'8869''45'elim_14))
              erased))
-- Axiom.Set.Properties.map-∅
d_map'45''8709'_448 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_map'45''8709'_448 ~v0 v1 ~v2 ~v3 ~v4 v5
  = du_map'45''8709'_448 v1 v5
du_map'45''8709'_448 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_map'45''8709'_448 v0 v1
  = coe
      du_'8709''45'least_436
      (coe
         (\ v2 v3 ->
            let v4
                  = coe
                      du_'8712''45'map'8315'''_184 v0 v1
                      (coe MAlonzo.Code.Axiom.Set.du_'8709'_404 (coe v0)) v2 v3 in
            case coe v4 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
                -> coe
                     seq (coe v6) (coe MAlonzo.Code.Data.Empty.du_'8869''45'elim_14)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Axiom.Set.Properties.mapPartial-∅
d_mapPartial'45''8709'_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_mapPartial'45''8709'_458 ~v0 v1 ~v2 ~v3 v4
  = du_mapPartial'45''8709'_458 v1 v4
du_mapPartial'45''8709'_458 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_mapPartial'45''8709'_458 v0 v1
  = coe
      du_'8709''45'least_436
      (coe
         (\ v2 v3 ->
            let v4
                  = coe
                      MAlonzo.Code.Function.Bundles.d_from_940
                      (coe
                         MAlonzo.Code.Axiom.Set.du_'8712''45'mapPartial_548 (coe v0)
                         (coe MAlonzo.Code.Axiom.Set.du_'8709'_404 (coe v0)) (coe v2)
                         (coe v1))
                      v3 in
            case coe v4 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
                -> coe
                     seq (coe v6) (coe MAlonzo.Code.Data.Empty.du_'8869''45'elim_14)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Axiom.Set.Properties.card-≡ᵉ
d_card'45''8801''7497'_472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_card'45''8801''7497'_472 = erased
-- Axiom.Set.Properties.filter-⊆
d_filter'45''8838'_502 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_filter'45''8838'_502 ~v0 v1 ~v2 v3 ~v4 v5 v6
  = du_filter'45''8838'_502 v1 v3 v5 v6
du_filter'45''8838'_502 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_filter'45''8838'_502 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe (\ v4 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v4)))
      (coe
         du_'8712''45'filter'8315'''_166 (coe v0) (coe v1) (coe v2)
         (coe v3))
-- Axiom.Set.Properties.Dec-∈-fromList
d_Dec'45''8712''45'fromList_510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_Dec'45''8712''45'fromList_510 ~v0 v1 ~v2 ~v3 v4 v5 v6
  = du_Dec'45''8712''45'fromList_510 v1 v4 v5 v6
du_Dec'45''8712''45'fromList_510 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_Dec'45''8712''45'fromList_510 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.du_map_18
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45'fromList_394 (coe v0) (coe v2)
         (coe v3))
      (coe
         MAlonzo.Code.Data.List.Membership.DecSetoid.du__'8712''63'__60
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_decSetoid_406
            (coe MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v1)))
         (coe v3) (coe v2))
-- Axiom.Set.Properties.Dec-∈-singleton
d_Dec'45''8712''45'singleton_516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_Dec'45''8712''45'singleton_516 ~v0 v1 ~v2 v3 v4 v5
  = du_Dec'45''8712''45'singleton_516 v1 v3 v4 v5
du_Dec'45''8712''45'singleton_516 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_Dec'45''8712''45'singleton_516 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.du_map_18
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45'singleton_420 (coe v0) (coe v3)
         (coe v1))
      (coe MAlonzo.Code.Interface.DecEq.d__'8799'__20 v2 v3 v1)
-- Axiom.Set.Properties.singleton-finite
d_singleton'45'finite_520 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_singleton'45'finite_520 ~v0 v1 ~v2 v3
  = du_singleton'45'finite_520 v1 v3
du_singleton'45'finite_520 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_singleton'45'finite_520 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
              (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
              (coe
                 MAlonzo.Code.Function.Related.Propositional.du_SK'45'sym_168
                 (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_88)
                 (coe
                    MAlonzo.Code.Axiom.Set.du_'8712''45'fromList_394 (coe v0)
                    (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
                    (coe v2)))
              (coe
                 MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
                 (coe
                    MAlonzo.Code.Function.Related.Propositional.C_equivalence_12))))
-- Axiom.Set.Properties.filter-finite
d_filter'45'finite_534 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_filter'45'finite_534 ~v0 v1 ~v2 v3 ~v4 v5 v6 v7
  = du_filter'45'finite_534 v1 v3 v5 v6 v7
du_filter'45'finite_534 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_filter'45'finite_534 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe MAlonzo.Code.Data.List.Base.du_filter_792 v3 v5)
             (coe
                (\ v7 ->
                   coe
                     MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                     (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                     (coe
                        MAlonzo.Code.Function.Related.Propositional.du_SK'45'sym_168
                        (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_88)
                        (coe
                           MAlonzo.Code.Axiom.Set.du_'8712''45'filter_388 (coe v0) (coe v1)
                           (coe v2) (coe v7)))
                     (coe
                        MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                        (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                        (coe
                           MAlonzo.Code.Data.List.Ext.Properties.du__'215''45'cong__26
                           (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                           (coe
                              MAlonzo.Code.Function.Related.Propositional.du_K'45'refl_160
                              (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12))
                           (coe v6 v7))
                        (coe
                           MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                           (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                           (coe
                              MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                              (coe
                                 MAlonzo.Code.Data.Product.Base.du_uncurry_220
                                 (coe
                                    (\ v8 v9 ->
                                       coe
                                         MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45'filter'8314'_522
                                         v3 v5 v9 v8)))
                              (coe
                                 (\ v8 ->
                                    coe
                                      MAlonzo.Code.Data.Product.Base.du_swap_346
                                      (coe
                                         MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45'filter'8315'_528
                                         v3 v7 v5 v8))))
                           (coe
                              MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
                              (coe
                                 MAlonzo.Code.Function.Related.Propositional.C_equivalence_12))))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Properties.∪-⊆ˡ
d_'8746''45''8838''737'_554 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8746''45''8838''737'_554 ~v0 v1 ~v2 v3 v4 v5
  = du_'8746''45''8838''737'_554 v1 v3 v4 v5
du_'8746''45''8838''737'_554 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8746''45''8838''737'_554 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe
         du_'8712''45''8746''8314'_208 (coe v0) (coe v1) (coe v2) (coe v3))
      (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
-- Axiom.Set.Properties.∪-⊆ʳ
d_'8746''45''8838''691'_556 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8746''45''8838''691'_556 ~v0 v1 ~v2 v3 v4 v5
  = du_'8746''45''8838''691'_556 v1 v3 v4 v5
du_'8746''45''8838''691'_556 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8746''45''8838''691'_556 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe
         du_'8712''45''8746''8314'_208 (coe v0) (coe v2) (coe v1) (coe v3))
      (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42)
-- Axiom.Set.Properties.∪-⊆
d_'8746''45''8838'_558 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8746''45''8838'_558 ~v0 v1 ~v2 v3 ~v4 v5 v6 v7 v8 v9
  = du_'8746''45''8838'_558 v1 v3 v5 v6 v7 v8 v9
du_'8746''45''8838'_558 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'8746''45''8838'_558 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66 (coe v3 v5)
      (coe v4 v5) (coe du_'8712''45''8746''8315'_174 v0 v1 v2 v5 v6)
-- Axiom.Set.Properties.∪-Supremum
d_'8746''45'Supremum_566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8746''45'Supremum_566 ~v0 v1 ~v2 v3 v4
  = du_'8746''45'Supremum_566 v1 v3 v4
du_'8746''45'Supremum_566 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8746''45'Supremum_566 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_'8746''45''8838''737'_554 (coe v0) (coe v1) (coe v2))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe du_'8746''45''8838''691'_556 (coe v0) (coe v2) (coe v1))
         (coe
            (\ v3 -> coe du_'8746''45''8838'_558 (coe v0) (coe v1) (coe v2))))
-- Axiom.Set.Properties.∪-cong-⊆
d_'8746''45'cong'45''8838'_570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8746''45'cong'45''8838'_570 ~v0 v1 ~v2 v3 v4 v5 v6 v7 v8 v9
  = du_'8746''45'cong'45''8838'_570 v1 v3 v4 v5 v6 v7 v8 v9
du_'8746''45'cong'45''8838'_570 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'8746''45'cong'45''8838'_570 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe
         du_'8712''45''8746''8314'_208 (coe v0) (coe v2) (coe v4) (coe v7))
      (coe
         MAlonzo.Code.Function.Base.du__'8728''8242'__216
         (coe MAlonzo.Code.Data.Sum.Base.du_map_84 (coe v5 v7) (coe v6 v7))
         (coe
            du_'8712''45''8746''8315'_174 (coe v0) (coe v1) (coe v3) (coe v7)))
-- Axiom.Set.Properties.∪-cong
d_'8746''45'cong_576 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8746''45'cong_576 ~v0 v1 ~v2 v3 v4 v5 v6
  = du_'8746''45'cong_576 v1 v3 v4 v5 v6
du_'8746''45'cong_576 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8746''45'cong_576 v0 v1 v2 v3 v4
  = coe
      du_cong'45''8838''8658'cong'8322'_264
      (coe du_'8746''45'cong'45''8838'_570 (coe v0)) (coe v1) (coe v2)
      (coe v3) (coe v4)
-- Axiom.Set.Properties.∪-preserves-finite
d_'8746''45'preserves'45'finite_578 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8746''45'preserves'45'finite_578 ~v0 v1 ~v2 v3 v4 v5 v6
  = du_'8746''45'preserves'45'finite_578 v1 v3 v4 v5 v6
du_'8746''45'preserves'45'finite_578 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8746''45'preserves'45'finite_578 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
        -> case coe v4 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                    (coe
                       MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v5) (coe v7))
                    (coe
                       (\ v9 ->
                          coe
                            MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                            (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                            (coe
                               MAlonzo.Code.Function.Related.Propositional.du_SK'45'sym_168
                               (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_88)
                               (coe
                                  MAlonzo.Code.Axiom.Set.du_'8712''45''8746'_650 (coe v0) (coe v1)
                                  (coe v2) (coe v9)))
                            (coe
                               MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                               (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                               (coe
                                  MAlonzo.Code.Data.List.Ext.Properties.du__'8846''45'cong__54
                                  (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                                  (coe v6 v9) (coe v8 v9))
                               (coe
                                  MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                                  (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                                  (coe
                                     MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                                     (coe
                                        MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52
                                        (coe
                                           MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45''43''43''8314''737'_200
                                           (coe v5))
                                        (coe
                                           MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45''43''43''8314''691'_206
                                           v5 v7))
                                     (coe
                                        MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45''43''43''8315'_212
                                        v5 v7))
                                  (coe
                                     MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
                                     (coe
                                        MAlonzo.Code.Function.Related.Propositional.C_equivalence_12))))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Properties.∪-sym
d_'8746''45'sym_598 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8746''45'sym_598 ~v0 v1 ~v2 v3 v4
  = du_'8746''45'sym_598 v1 v3 v4
du_'8746''45'sym_598 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8746''45'sym_598 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_'8746''45''8838'_558 (coe v0) (coe v1) (coe v2)
         (coe du_'8746''45''8838''691'_556 (coe v0) (coe v1) (coe v2))
         (coe du_'8746''45''8838''737'_554 (coe v0) (coe v2) (coe v1)))
      (coe
         du_'8746''45''8838'_558 (coe v0) (coe v2) (coe v1)
         (coe du_'8746''45''8838''691'_556 (coe v0) (coe v2) (coe v1))
         (coe du_'8746''45''8838''737'_554 (coe v0) (coe v1) (coe v2)))
-- Axiom.Set.Properties.Set-JoinSemilattice
d_Set'45'JoinSemilattice_600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
d_Set'45'JoinSemilattice_600 ~v0 v1 ~v2
  = du_Set'45'JoinSemilattice_600 v1
du_Set'45'JoinSemilattice_600 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsJoinSemilattice_22
du_Set'45'JoinSemilattice_600 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.C_IsJoinSemilattice'46'constructor_527
      (coe du_'8838''45'PartialOrder_308)
      (coe du_'8746''45'Supremum_566 (coe v0))
-- Axiom.Set.Properties.Set-BoundedJoinSemilattice
d_Set'45'BoundedJoinSemilattice_602 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedJoinSemilattice_110
d_Set'45'BoundedJoinSemilattice_602 ~v0 v1 ~v2
  = du_Set'45'BoundedJoinSemilattice_602 v1
du_Set'45'BoundedJoinSemilattice_602 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsBoundedJoinSemilattice_110
du_Set'45'BoundedJoinSemilattice_602 v0
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.C_IsBoundedJoinSemilattice'46'constructor_5169
      (coe du_Set'45'JoinSemilattice_600 (coe v0))
      (\ v1 v2 v3 -> coe du_'8709''45'minimum_432)
-- Axiom.Set.Properties.disjoint-sym
d_disjoint'45'sym_604 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_disjoint'45'sym_604 = erased
-- Axiom.Set.Properties.Intersectionᵖ._._∩_
d__'8745'__616 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__616 ~v0 v1 ~v2 v3 = du__'8745'__616 v1 v3
du__'8745'__616 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'8745'__616 v0 v1
  = coe MAlonzo.Code.Axiom.Set.du__'8745'__666 (coe v0) (coe v1)
-- Axiom.Set.Properties.Intersectionᵖ._.disjoint'
d_disjoint''_618 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_disjoint''_618 = erased
-- Axiom.Set.Properties.Intersectionᵖ.disjoint⇒disjoint'
d_disjoint'8658'disjoint''_622 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_disjoint'8658'disjoint''_622 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
  = du_disjoint'8658'disjoint''_622
du_disjoint'8658'disjoint''_622 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_disjoint'8658'disjoint''_622
  = coe
      du_'8709''45'least_436
      (coe (\ v0 v1 -> coe MAlonzo.Code.Data.Empty.du_'8869''45'elim_14))
-- Axiom.Set.Properties.Intersectionᵖ.disjoint'⇒disjoint
d_disjoint'''8658'disjoint_626 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_disjoint'''8658'disjoint_626 = erased
-- Axiom.Set.Properties.Intersectionᵖ.∩-⊆ˡ
d_'8745''45''8838''737'_634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8745''45''8838''737'_634 ~v0 v1 ~v2 v3 v4 v5 v6 v7
  = du_'8745''45''8838''737'_634 v1 v3 v4 v5 v6 v7
du_'8745''45''8838''737'_634 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8745''45''8838''737'_634 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe
         MAlonzo.Code.Function.Bundles.d_from_940
         (coe
            MAlonzo.Code.Axiom.Set.du_'8712''45''8745'_674 (coe v0) (coe v1)
            (coe v2) (coe v3) (coe v4))
         v5)
-- Axiom.Set.Properties.Intersectionᵖ.∩-⊆ʳ
d_'8745''45''8838''691'_636 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8745''45''8838''691'_636 ~v0 v1 ~v2 v3 v4 v5 v6 v7
  = du_'8745''45''8838''691'_636 v1 v3 v4 v5 v6 v7
du_'8745''45''8838''691'_636 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8745''45''8838''691'_636 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe
         MAlonzo.Code.Function.Bundles.d_from_940
         (coe
            MAlonzo.Code.Axiom.Set.du_'8712''45''8745'_674 (coe v0) (coe v1)
            (coe v2) (coe v3) (coe v4))
         v5)
-- Axiom.Set.Properties.Intersectionᵖ.∩-⊆
d_'8745''45''8838'_638 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8745''45''8838'_638 ~v0 v1 ~v2 v3 ~v4 v5 v6 v7 v8 v9 v10
  = du_'8745''45''8838'_638 v1 v3 v5 v6 v7 v8 v9 v10
du_'8745''45''8838'_638 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'8745''45''8838'_638 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      MAlonzo.Code.Function.Bundles.d_to_938
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45''8745'_674 (coe v0) (coe v1)
         (coe v2) (coe v3) (coe v6))
      (coe
         MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88 (coe v4 v6)
         (coe v5 v6) (coe v7))
-- Axiom.Set.Properties.Intersectionᵖ.∩-Infimum
d_'8745''45'Infimum_646 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8745''45'Infimum_646 ~v0 v1 ~v2 v3 v4 v5
  = du_'8745''45'Infimum_646 v1 v3 v4 v5
du_'8745''45'Infimum_646 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8745''45'Infimum_646 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_'8745''45''8838''737'_634 (coe v0) (coe v1) (coe v2) (coe v3))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            du_'8745''45''8838''691'_636 (coe v0) (coe v1) (coe v2) (coe v3))
         (coe
            (\ v4 ->
               coe du_'8745''45''8838'_638 (coe v0) (coe v1) (coe v2) (coe v3))))
-- Axiom.Set.Properties.Intersectionᵖ.∩-preserves-finite
d_'8745''45'preserves'45'finite_654 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8745''45'preserves'45'finite_654 ~v0 v1 ~v2 v3 v4 v5 ~v6
  = du_'8745''45'preserves'45'finite_654 v1 v3 v4 v5
du_'8745''45'preserves'45'finite_654 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8745''45'preserves'45'finite_654 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Axiom.Set.du_'8838''45'weakly'45'finite_302
      (coe
         du_'8745''45''8838''691'_636 (coe v0) (coe v1) (coe v2) (coe v3))
-- Axiom.Set.Properties.Intersectionᵖ.∩-cong-⊆
d_'8745''45'cong'45''8838'_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8745''45'cong'45''8838'_656 ~v0 v1 ~v2 v3 v4 v5 v6 v7 v8 v9 v10
                               v11
  = du_'8745''45'cong'45''8838'_656 v1 v3 v4 v5 v6 v7 v8 v9 v10 v11
du_'8745''45'cong'45''8838'_656 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'8745''45'cong'45''8838'_656 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe
      MAlonzo.Code.Function.Bundles.d_to_938
      (coe
         MAlonzo.Code.Axiom.Set.du_'8712''45''8745'_674 (coe v0) (coe v1)
         (coe v3) (coe v5) (coe v8))
      (coe
         MAlonzo.Code.Data.Product.Base.du_map_104 (coe v6 v8)
         (coe (\ v10 -> coe v7 v8))
         (coe
            MAlonzo.Code.Function.Bundles.d_from_940
            (coe
               MAlonzo.Code.Axiom.Set.du_'8712''45''8745'_674 (coe v0) (coe v1)
               (coe v2) (coe v4) (coe v8))
            v9))
-- Axiom.Set.Properties.Intersectionᵖ.∩-cong
d_'8745''45'cong_664 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8745''45'cong_664 ~v0 v1 ~v2 v3 v4 v5 v6 v7
  = du_'8745''45'cong_664 v1 v3 v4 v5 v6 v7
du_'8745''45'cong_664 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8745''45'cong_664 v0 v1 v2 v3 v4 v5
  = coe
      du_cong'45''8838''8658'cong'8322'_264
      (coe du_'8745''45'cong'45''8838'_656 (coe v0) (coe v1)) (coe v2)
      (coe v3) (coe v4) (coe v5)
-- Axiom.Set.Properties.Intersectionᵖ.∩-OrderHomomorphismʳ
d_'8745''45'OrderHomomorphism'691'_670 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderHomomorphism_138
d_'8745''45'OrderHomomorphism'691'_670 ~v0 v1 ~v2 v3 v4
  = du_'8745''45'OrderHomomorphism'691'_670 v1 v3 v4
du_'8745''45'OrderHomomorphism'691'_670 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderHomomorphism_138
du_'8745''45'OrderHomomorphism'691'_670 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsOrderHomomorphism'46'constructor_5407
      (coe
         (\ v3 v4 ->
            coe
              du_'8745''45'cong_664 v0 v1 v2 v2 v3 v4
              (coe
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe (\ v5 v6 -> v6))
                 (coe (\ v5 v6 -> v6)))))
      (coe
         (\ v3 v4 ->
            coe
              du_'8745''45'cong'45''8838'_656 (coe v0) (coe v1) (coe v2) (coe v2)
              (coe v3) (coe v4) (coe (\ v5 v6 -> v6))))
-- Axiom.Set.Properties.Intersectionᵖ.∩-OrderHomomorphismˡ
d_'8745''45'OrderHomomorphism'737'_676 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderHomomorphism_138
d_'8745''45'OrderHomomorphism'737'_676 ~v0 v1 ~v2 v3 v4
  = du_'8745''45'OrderHomomorphism'737'_676 v1 v3 v4
du_'8745''45'OrderHomomorphism'737'_676 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Morphism.Structures.T_IsOrderHomomorphism_138
du_'8745''45'OrderHomomorphism'737'_676 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Morphism.Structures.C_IsOrderHomomorphism'46'constructor_5407
      (coe
         (\ v3 v4 v5 ->
            coe
              du_'8745''45'cong_664 v0 v1 v3 v4 v2 v2 v5
              (coe
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe (\ v6 v7 -> v7))
                 (coe (\ v6 v7 -> v7)))))
      (coe
         (\ v3 v4 v5 ->
            coe
              du_'8745''45'cong'45''8838'_656 (coe v0) (coe v1) (coe v3) (coe v4)
              (coe v2) (coe v2) (coe v5) (coe (\ v6 v7 -> v7))))
-- Axiom.Set.Properties.Intersectionᵖ.Set-Lattice
d_Set'45'Lattice_678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
d_Set'45'Lattice_678 ~v0 v1 ~v2 v3 = du_Set'45'Lattice_678 v1 v3
du_Set'45'Lattice_678 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Lattice.Structures.T_IsLattice_316
du_Set'45'Lattice_678 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Lattice.Structures.C_IsLattice'46'constructor_14441
      (coe du_'8838''45'PartialOrder_308)
      (coe du_'8746''45'Supremum_566 (coe v0))
      (coe du_'8745''45'Infimum_646 (coe v0) (coe v1))
-- Axiom.Set.Properties.Intersectionᵖ.∩-sym⊆
d_'8745''45'sym'8838'_680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8745''45'sym'8838'_680 ~v0 v1 ~v2 v3 v4 v5 v6 v7
  = du_'8745''45'sym'8838'_680 v1 v3 v4 v5 v6 v7
du_'8745''45'sym'8838'_680 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8745''45'sym'8838'_680 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Function.Bundles.d_from_940
              (coe
                 MAlonzo.Code.Axiom.Set.du_'8712''45''8745'_674 (coe v0) (coe v1)
                 (coe v2) (coe v3) (coe v4))
              v5 in
    case coe v6 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
        -> coe
             MAlonzo.Code.Function.Bundles.d_to_938
             (coe
                MAlonzo.Code.Axiom.Set.du_'8712''45''8745'_674 (coe v0) (coe v1)
                (coe v3) (coe v2) (coe v4))
             (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v8) (coe v7))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Properties.Intersectionᵖ.∩-sym
d_'8745''45'sym_694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8745''45'sym_694 ~v0 v1 ~v2 v3 v4 v5
  = du_'8745''45'sym_694 v1 v3 v4 v5
du_'8745''45'sym_694 ::
  MAlonzo.Code.Axiom.Set.T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8745''45'sym_694 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_'8745''45'sym'8838'_680 (coe v0) (coe v1) (coe v2) (coe v3))
      (coe
         du_'8745''45'sym'8838'_680 (coe v0) (coe v1) (coe v3) (coe v2))
