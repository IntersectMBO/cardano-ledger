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

module MAlonzo.Code.Axiom.Set where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Ext.Properties
import qualified MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.List.Relation.Unary.Unique.DecPropositional.Properties
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Product.Algebra
import qualified MAlonzo.Code.Data.Product.Properties
import qualified MAlonzo.Code.Data.Product.Properties.Ext
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Axiom.Set._Preserves₁_⟶_
d__Preserves'8321'_'10230'__18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__Preserves'8321'_'10230'__18 = erased
-- Axiom.Set._Preserves₁₂_⟶_⟶_
d__Preserves'8321''8322'_'10230'_'10230'__32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__Preserves'8321''8322'_'10230'_'10230'__32 = erased
-- Axiom.Set.SpecProperty
d_SpecProperty_48 a0 = ()
data T_SpecProperty_48
  = C_SpecProperty'46'constructor_1675 (() ->
                                        (AgdaAny -> ()) ->
                                        () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny)
                                       (() -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny)
-- Axiom.Set.SpecProperty.specProperty
d_specProperty_64 ::
  T_SpecProperty_48 -> () -> (AgdaAny -> ()) -> ()
d_specProperty_64 = erased
-- Axiom.Set.SpecProperty.sp-∘
d_sp'45''8728'_68 ::
  T_SpecProperty_48 ->
  () ->
  (AgdaAny -> ()) -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_sp'45''8728'_68 v0
  = case coe v0 of
      C_SpecProperty'46'constructor_1675 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.SpecProperty.sp-¬
d_sp'45''172'_70 ::
  T_SpecProperty_48 -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny
d_sp'45''172'_70 v0
  = case coe v0 of
      C_SpecProperty'46'constructor_1675 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Dec-SpecProperty
d_Dec'45'SpecProperty_72 :: T_SpecProperty_48
d_Dec'45'SpecProperty_72
  = coe
      C_SpecProperty'46'constructor_1675
      (\ v0 v1 v2 v3 v4 v5 -> coe v3 (coe v4 v5))
      (\ v0 v1 v2 v3 ->
         coe
           MAlonzo.Code.Relation.Nullary.Decidable.Core.du_'172''63'_56
           (coe v2 v3))
-- Axiom.Set.Theory
d_Theory_82 a0 = ()
data T_Theory_82
  = C_Theory'46'constructor_6769 T_SpecProperty_48
                                 (() ->
                                  (AgdaAny -> ()) ->
                                  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
                                 (() -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
                                 (() ->
                                  () ->
                                  (AgdaAny -> AgdaAny) ->
                                  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
                                 (() -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Axiom.Set._.sp-¬
d_sp'45''172'_94 ::
  T_SpecProperty_48 -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny
d_sp'45''172'_94 v0 = coe d_sp'45''172'_70 (coe v0)
-- Axiom.Set._.sp-∘
d_sp'45''8728'_96 ::
  T_SpecProperty_48 ->
  () ->
  (AgdaAny -> ()) -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_sp'45''8728'_96 v0 = coe d_sp'45''8728'_68 (coe v0)
-- Axiom.Set._.specProperty
d_specProperty_98 ::
  T_SpecProperty_48 -> () -> (AgdaAny -> ()) -> ()
d_specProperty_98 = erased
-- Axiom.Set.Theory.Set
d_Set_146 :: T_Theory_82 -> () -> ()
d_Set_146 = erased
-- Axiom.Set.Theory._∈_
d__'8712'__148 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8712'__148 = erased
-- Axiom.Set.Theory.sp
d_sp_150 :: T_Theory_82 -> T_SpecProperty_48
d_sp_150 v0
  = case coe v0 of
      C_Theory'46'constructor_6769 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory._.sp-¬
d_sp'45''172'_154 ::
  T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny
d_sp'45''172'_154 v0 = coe d_sp'45''172'_70 (coe d_sp_150 (coe v0))
-- Axiom.Set.Theory._.sp-∘
d_sp'45''8728'_156 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_sp'45''8728'_156 v0
  = coe d_sp'45''8728'_68 (coe d_sp_150 (coe v0))
-- Axiom.Set.Theory._.specProperty
d_specProperty_158 :: T_Theory_82 -> () -> (AgdaAny -> ()) -> ()
d_specProperty_158 = erased
-- Axiom.Set.Theory._⊆_
d__'8838'__160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8838'__160 = erased
-- Axiom.Set.Theory.specification
d_specification_174 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_specification_174 v0
  = case coe v0 of
      C_Theory'46'constructor_6769 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory.unions
d_unions_184 ::
  T_Theory_82 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unions_184 v0
  = case coe v0 of
      C_Theory'46'constructor_6769 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory.replacement
d_replacement_196 ::
  T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_replacement_196 v0
  = case coe v0 of
      C_Theory'46'constructor_6769 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory.listing
d_listing_204 ::
  T_Theory_82 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_listing_204 v0
  = case coe v0 of
      C_Theory'46'constructor_6769 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory._≡ᵉ_
d__'8801''7497'__212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'__212 = erased
-- Axiom.Set.Theory._≡ᵉ'_
d__'8801''7497'''__218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'''__218 = erased
-- Axiom.Set.Theory._∉_
d__'8713'__226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8713'__226 = erased
-- Axiom.Set.Theory._Preservesˢ_
d__Preserves'738'__230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738'__230 = erased
-- Axiom.Set.Theory._Preservesˢ₂_
d__Preserves'738''8322'__238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738''8322'__238 = erased
-- Axiom.Set.Theory.disjoint
d_disjoint_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d_disjoint_244 = erased
-- Axiom.Set.Theory.finite
d_finite_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> ()
d_finite_252 = erased
-- Axiom.Set.Theory.weakly-finite
d_weakly'45'finite_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> ()
d_weakly'45'finite_260 = erased
-- Axiom.Set.Theory.strongly-finite
d_strongly'45'finite_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> ()
d_strongly'45'finite_268 = erased
-- Axiom.Set.Theory.DecEq∧finite⇒strongly-finite
d_DecEq'8743'finite'8658'strongly'45'finite_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_DecEq'8743'finite'8658'strongly'45'finite_280 ~v0 ~v1 ~v2 v3 ~v4
                                                v5
  = du_DecEq'8743'finite'8658'strongly'45'finite_280 v3 v5
du_DecEq'8743'finite'8658'strongly'45'finite_280 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_DecEq'8743'finite'8658'strongly'45'finite_280 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                MAlonzo.Code.Data.List.Base.du_deduplicate_834
                (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) v2)
             (coe
                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                (coe
                   MAlonzo.Code.Data.List.Relation.Unary.Unique.DecPropositional.Properties.du_deduplicate'45''33'_42
                   (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) v2)
                (coe
                   (\ v4 ->
                      coe
                        MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                        (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                        (coe v3 v4)
                        (coe
                           MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                           (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                           (coe
                              MAlonzo.Code.Data.List.Ext.Properties.du_'8712''45'dedup_146
                              (coe v0) (coe v2) (coe v4))
                           (coe
                              MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
                              (coe
                                 MAlonzo.Code.Function.Related.Propositional.C_equivalence_12))))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory.card
d_card_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_card_298 ~v0 ~v1 ~v2 v3 = du_card_298 v3
du_card_298 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
du_card_298 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
               -> coe MAlonzo.Code.Data.List.Base.du_length_304 v3
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory.⊆-weakly-finite
d_'8838''45'weakly'45'finite_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8838''45'weakly'45'finite_302 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_'8838''45'weakly'45'finite_302 v5 v6
du_'8838''45'weakly'45'finite_302 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8838''45'weakly'45'finite_302 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
             (coe (\ v4 v5 -> coe v3 v4 (coe v0 v4 v5)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory.isMaximal
d_isMaximal_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> ()
d_isMaximal_310 = erased
-- Axiom.Set.Theory.maximal-⊆
d_maximal'45''8838'_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_maximal'45''8838'_318 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 ~v7
  = du_maximal'45''8838'_318 v5 v6
du_maximal'45''8838'_318 ::
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_maximal'45''8838'_318 v0 v1 = coe v0 v1
-- Axiom.Set.Theory.maximal-unique
d_maximal'45'unique_322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_maximal'45'unique_322 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6
  = du_maximal'45'unique_322 v5 v6
du_maximal'45'unique_322 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_maximal'45'unique_322 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v2 v3 -> coe du_maximal'45''8838'_318 (coe v1) v2)
      (\ v2 v3 -> coe du_maximal'45''8838'_318 (coe v0) v2)
-- Axiom.Set.Theory.FinSet
d_FinSet_328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Theory_82 -> () -> ()
d_FinSet_328 = erased
-- Axiom.Set.Theory.strictify
d_strictify_340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_strictify_340 ~v0 v1 ~v2 ~v3 v4 v5 = du_strictify_340 v1 v4 v5
du_strictify_340 ::
  T_Theory_82 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_strictify_340 v0 v1 v2
  = let v3
          = coe
              d_specification_174 v0 erased erased
              (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v2)) v1 in
    case coe v3 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4)
             (coe
                (\ v6 ->
                   coe
                     MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                     (coe
                        (\ v7 ->
                           coe
                             MAlonzo.Code.Function.Bundles.d_to_938 (coe v5 v6)
                             (coe
                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v7)
                                (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 v2 v6 v7))))
                     (coe
                        (\ v7 ->
                           MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                             (coe MAlonzo.Code.Function.Bundles.d_from_940 (coe v5 v6) v7)))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory.map
d_map_360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map_360 ~v0 v1 ~v2 ~v3 = du_map_360 v1
du_map_360 ::
  T_Theory_82 -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_map_360 v0
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8322'__92
      (coe
         (\ v1 v2 v3 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3)))
      (coe d_replacement_196 v0 erased erased)
-- Axiom.Set.Theory.∈-map
d_'8712''45'map_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'map_368 ~v0 v1 ~v2 ~v3 v4 v5 v6
  = du_'8712''45'map_368 v1 v4 v5 v6
du_'8712''45'map_368 ::
  T_Theory_82 ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45'map_368 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_replacement_196 v0 erased erased v2 v1) v3
-- Axiom.Set.Theory.∈-map′
d_'8712''45'map'8242'_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8712''45'map'8242'_374 ~v0 v1 ~v2 ~v3 v4 v5 v6 v7
  = du_'8712''45'map'8242'_374 v1 v4 v5 v6 v7
du_'8712''45'map'8242'_374 ::
  T_Theory_82 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_'8712''45'map'8242'_374 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Function.Bundles.d_to_938
      (coe du_'8712''45'map_368 (coe v0) (coe v1) (coe v2) (coe v2 v3))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
         (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased (coe v4)))
-- Axiom.Set.Theory.filter
d_filter_382 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d_filter_382 ~v0 v1 ~v2 ~v3 = du_filter_382 v1
du_filter_382 :: T_Theory_82 -> AgdaAny -> AgdaAny -> AgdaAny
du_filter_382 v0
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8322'__92
      (coe
         (\ v1 v2 v3 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3)))
      (coe (\ v1 v2 -> coe d_specification_174 v0 erased erased v2 v1))
-- Axiom.Set.Theory.∈-filter
d_'8712''45'filter_388 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'filter_388 ~v0 v1 ~v2 ~v3 v4 v5 v6
  = du_'8712''45'filter_388 v1 v4 v5 v6
du_'8712''45'filter_388 ::
  T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45'filter_388 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_specification_174 v0 erased erased v1 v2) v3
-- Axiom.Set.Theory.fromList
d_fromList_390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> [AgdaAny] -> AgdaAny
d_fromList_390 ~v0 v1 ~v2 v3 = du_fromList_390 v1 v3
du_fromList_390 :: T_Theory_82 -> [AgdaAny] -> AgdaAny
du_fromList_390 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_listing_204 v0 erased v1)
-- Axiom.Set.Theory.∈-fromList
d_'8712''45'fromList_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'fromList_394 ~v0 v1 ~v2 v3 v4
  = du_'8712''45'fromList_394 v1 v3 v4
du_'8712''45'fromList_394 ::
  T_Theory_82 ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45'fromList_394 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_listing_204 v0 erased v1) v2
-- Axiom.Set.Theory.∈-unions
d_'8712''45'unions_402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'unions_402 ~v0 v1 ~v2 v3 v4
  = du_'8712''45'unions_402 v1 v3 v4
du_'8712''45'unions_402 ::
  T_Theory_82 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45'unions_402 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe d_unions_184 v0 erased v2) v1
-- Axiom.Set.Theory.∅
d_'8709'_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny
d_'8709'_404 ~v0 v1 ~v2 = du_'8709'_404 v1
du_'8709'_404 :: T_Theory_82 -> AgdaAny
du_'8709'_404 v0
  = coe
      du_fromList_390 (coe v0)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Axiom.Set.Theory.∅-strongly-finite
d_'8709''45'strongly'45'finite_406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8709''45'strongly'45'finite_406 ~v0 v1 ~v2
  = du_'8709''45'strongly'45'finite_406 v1
du_'8709''45'strongly'45'finite_406 ::
  T_Theory_82 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8709''45'strongly'45'finite_406 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C_'91''93'_22)
         (coe
            (\ v1 ->
               coe
                 MAlonzo.Code.Function.Related.Propositional.du_SK'45'sym_168
                 (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_88)
                 (coe
                    du_'8712''45'fromList_394 (coe v0)
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) (coe v1)))))
-- Axiom.Set.Theory.card-∅
d_card'45''8709'_408 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_card'45''8709'_408 = erased
-- Axiom.Set.Theory.singleton
d_singleton_410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> AgdaAny
d_singleton_410 ~v0 v1 ~v2 v3 = du_singleton_410 v1 v3
du_singleton_410 :: T_Theory_82 -> AgdaAny -> AgdaAny
du_singleton_410 v0 v1
  = coe
      du_fromList_390 (coe v0)
      (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v1))
-- Axiom.Set.Theory.❴_❵
d_'10100'_'10101'_414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> AgdaAny
d_'10100'_'10101'_414 ~v0 v1 = du_'10100'_'10101'_414 v1
du_'10100'_'10101'_414 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny
du_'10100'_'10101'_414 v0 v1 v2 = coe du_singleton_410 (coe v0) v2
-- Axiom.Set.Theory.∈-singleton
d_'8712''45'singleton_420 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'singleton_420 ~v0 v1 ~v2 v3 v4
  = du_'8712''45'singleton_420 v1 v3 v4
du_'8712''45'singleton_420 ::
  T_Theory_82 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45'singleton_420 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
      (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
      (coe
         MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
         (coe
            (\ v3 ->
               coe MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 erased))
         erased)
      (coe
         MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
         (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
         (coe
            du_'8712''45'fromList_394 (coe v0)
            (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v2))
            (coe v1))
         (coe
            MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
            (coe
               MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)))
-- Axiom.Set.Theory.partialToSet
d_partialToSet_434 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_partialToSet_434 ~v0 v1 ~v2 ~v3 ~v4 v5 v6
  = du_partialToSet_434 v1 v5 v6
du_partialToSet_434 ::
  T_Theory_82 -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
du_partialToSet_434 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Maybe.Base.du_maybe_36
      (coe
         (\ v3 ->
            coe
              du_fromList_390 (coe v0)
              (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v3))))
      (coe du_'8709'_404 (coe v0)) (coe v1 v2)
-- Axiom.Set.Theory.∈-partialToSet
d_'8712''45'partialToSet_446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'partialToSet_446 ~v0 v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7
  = du_'8712''45'partialToSet_446 v1 v6
du_'8712''45'partialToSet_446 ::
  T_Theory_82 ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45'partialToSet_446 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
      (coe
         (\ v2 ->
            coe
              MAlonzo.Code.Function.Bundles.d_to_938
              (coe du_'8712''45'singleton_420 (coe v0) (coe v1) (coe v1))
              erased))
      erased
-- Axiom.Set.Theory.concatMapˢ
d_concatMap'738'_470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_concatMap'738'_470 ~v0 v1 ~v2 ~v3 v4 v5
  = du_concatMap'738'_470 v1 v4 v5
du_concatMap'738'_470 ::
  T_Theory_82 -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_concatMap'738'_470 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe d_unions_184 v0 erased (coe du_map_360 v0 v1 v2))
-- Axiom.Set.Theory.∈-concatMapˢ
d_'8712''45'concatMap'738'_482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'concatMap'738'_482 ~v0 v1 ~v2 ~v3 v4 v5 v6
  = du_'8712''45'concatMap'738'_482 v1 v4 v5 v6
du_'8712''45'concatMap'738'_482 ::
  T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45'concatMap'738'_482 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
      (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
      (coe
         MAlonzo.Code.Data.Product.Properties.Ext.du_'8707''45'cong'8242'_46
         (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
         (coe
            (\ v4 ->
               coe
                 MAlonzo.Code.Data.Product.Properties.Ext.du_'8707''45''8801'_58
                 (coe v3 v4))))
      (coe
         MAlonzo.Code.Function.Related.Propositional.du__'8596''10216'_'10217'__220
         (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
         (coe
            MAlonzo.Code.Data.Product.Properties.du_'8707''8707''8596''8707''8707'_250)
         (coe
            MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
            (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
            (coe
               MAlonzo.Code.Data.Product.Properties.Ext.du_'8707''45'cong'8242'_46
               (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
               (coe
                  (\ v4 ->
                     coe
                       MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                       (coe
                          (\ v5 ->
                             case coe v5 of
                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                                 -> case coe v7 of
                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
                                        -> case coe v9 of
                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v10 v11
                                               -> coe
                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                       (coe v6)
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                          (coe v8) (coe v10)))
                                                    (coe v11)
                                             _ -> MAlonzo.RTE.mazUnreachableError
                                      _ -> MAlonzo.RTE.mazUnreachableError
                               _ -> MAlonzo.RTE.mazUnreachableError))
                       (coe
                          (\ v5 ->
                             case coe v5 of
                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                                 -> case coe v6 of
                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
                                        -> case coe v9 of
                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v10 v11
                                               -> coe
                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                    (coe v8)
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                       (coe v10)
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                          (coe v11) (coe v7)))
                                             _ -> MAlonzo.RTE.mazUnreachableError
                                      _ -> MAlonzo.RTE.mazUnreachableError
                               _ -> MAlonzo.RTE.mazUnreachableError)))))
            (coe
               MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
               (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
               (coe
                  MAlonzo.Code.Data.Product.Properties.Ext.du_'8707''45'cong'8242'_46
                  (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                  (coe
                     (\ v4 ->
                        coe
                          MAlonzo.Code.Data.List.Ext.Properties.du__'215''45'cong__26
                          (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                          (coe du_'8712''45'map_368 (coe v0) (coe v1) (coe v3) (coe v4))
                          (coe
                             MAlonzo.Code.Function.Related.Propositional.du_K'45'refl_160
                             (coe
                                MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)))))
               (coe
                  MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                  (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                  (coe
                     du_'8712''45'unions_402 (coe v0) (coe v2)
                     (coe du_map_360 v0 v3 v1))
                  (coe
                     MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
                     (coe
                        MAlonzo.Code.Function.Related.Propositional.C_equivalence_12))))))
-- Axiom.Set.Theory.mapPartial
d_mapPartial_538 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_mapPartial_538 ~v0 v1 ~v2 ~v3 v4 = du_mapPartial_538 v1 v4
du_mapPartial_538 ::
  T_Theory_82 -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
du_mapPartial_538 v0 v1
  = coe
      du_concatMap'738'_470 (coe v0)
      (coe du_partialToSet_434 (coe v0) (coe v1))
-- Axiom.Set.Theory.∈-mapPartial
d_'8712''45'mapPartial_548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'mapPartial_548 ~v0 v1 ~v2 ~v3 v4 v5 v6
  = du_'8712''45'mapPartial_548 v1 v4 v5 v6
du_'8712''45'mapPartial_548 ::
  T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45'mapPartial_548 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
      (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
      (coe
         MAlonzo.Code.Data.Product.Properties.Ext.du_'8707''45'cong'8242'_46
         (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
         (coe
            (\ v4 ->
               coe
                 MAlonzo.Code.Data.List.Ext.Properties.du__'215''45'cong__26
                 (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                 (coe
                    MAlonzo.Code.Function.Related.Propositional.du_K'45'refl_160
                    (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12))
                 (coe du_'8712''45'partialToSet_446 (coe v0) (coe v2)))))
      (coe
         MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
         (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
         (coe
            du_'8712''45'concatMap'738'_482 (coe v0) (coe v1) (coe v2)
            (coe du_partialToSet_434 (coe v0) (coe v3)))
         (coe
            MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
            (coe
               MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)))
-- Axiom.Set.Theory.⊆-mapPartial
d_'8838''45'mapPartial_566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) -> Maybe AgdaAny -> AgdaAny -> AgdaAny
d_'8838''45'mapPartial_566 ~v0 v1 ~v2 ~v3 v4 v5 v6 v7
  = du_'8838''45'mapPartial_566 v1 v4 v5 v6 v7
du_'8838''45'mapPartial_566 ::
  T_Theory_82 ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) -> Maybe AgdaAny -> AgdaAny -> AgdaAny
du_'8838''45'mapPartial_566 v0 v1 v2 v3 v4
  = let v5
          = coe
              MAlonzo.Code.Function.Bundles.d_from_940
              (coe
                 du_'8712''45'map_368 (coe v0) (coe du_mapPartial_538 v0 v2 v1)
                 (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16) (coe v3))
              v4 in
    case coe v5 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
        -> case coe v7 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
               -> let v10
                        = coe
                            MAlonzo.Code.Function.Bundles.d_from_940
                            (coe
                               du_'8712''45'mapPartial_548 (coe v0) (coe v1) (coe v6) (coe v2))
                            v9 in
                  case coe v10 of
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
                      -> case coe v12 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v13 v14
                             -> coe
                                  MAlonzo.Code.Function.Bundles.d_to_938
                                  (coe
                                     du_'8712''45'map_368 (coe v0) (coe v1) (coe v2)
                                     (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v6)))
                                  (coe
                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v11)
                                     (coe
                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased
                                        (coe v13)))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory.binary-unions
d_binary'45'unions_606 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_binary'45'unions_606 ~v0 v1 ~v2 v3 v4
  = du_binary'45'unions_606 v1 v3 v4
du_binary'45'unions_606 ::
  T_Theory_82 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_binary'45'unions_606 v0 v1 v2
  = let v3
          = coe
              d_unions_184 v0 erased
              (coe
                 du_fromList_390 (coe v0)
                 (coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
                    (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v2)))) in
    case coe v3 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4)
             (coe
                (\ v6 ->
                   coe
                     MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                     (coe
                        (\ v7 ->
                           case coe v7 of
                             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v8
                               -> coe
                                    MAlonzo.Code.Function.Bundles.d_to_938 (coe v5 v6)
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1)
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                          (coe
                                             MAlonzo.Code.Function.Bundles.d_to_938
                                             (coe
                                                du_'8712''45'fromList_394 (coe v0)
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                   (coe v1)
                                                   (coe
                                                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                      (coe v2)
                                                      (coe
                                                         MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                                                (coe v1))
                                             (coe
                                                MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46
                                                erased))
                                          (coe v8)))
                             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v8
                               -> coe
                                    MAlonzo.Code.Function.Bundles.d_to_938 (coe v5 v6)
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                          (coe
                                             MAlonzo.Code.Function.Bundles.d_to_938
                                             (coe
                                                du_'8712''45'fromList_394 (coe v0)
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                   (coe v1)
                                                   (coe
                                                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                      (coe v2)
                                                      (coe
                                                         MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                                                (coe v2))
                                             (coe
                                                MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54
                                                (coe
                                                   MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46
                                                   erased)))
                                          (coe v8)))
                             _ -> MAlonzo.RTE.mazUnreachableError))
                     (coe
                        (\ v7 ->
                           let v8
                                 = coe MAlonzo.Code.Function.Bundles.d_from_940 (coe v5 v6) v7 in
                           case coe v8 of
                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v9 v10
                               -> case coe v10 of
                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v11 v12
                                      -> let v13
                                               = coe
                                                   MAlonzo.Code.Function.Bundles.d_from_940
                                                   (coe
                                                      du_'8712''45'fromList_394 (coe v0)
                                                      (coe
                                                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                         (coe v1)
                                                         (coe
                                                            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                            (coe v2)
                                                            (coe
                                                               MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                                                      (coe v9))
                                                   v11 in
                                         case coe v13 of
                                           MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v16
                                             -> coe
                                                  MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
                                                  (coe v12)
                                           MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v16
                                             -> coe
                                                  seq (coe v16)
                                                  (coe
                                                     MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                                                     (coe v12))
                                           _ -> MAlonzo.RTE.mazUnreachableError
                                    _ -> MAlonzo.RTE.mazUnreachableError
                             _ -> MAlonzo.RTE.mazUnreachableError))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theory._∪_
d__'8746'__642 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__642 ~v0 v1 ~v2 v3 v4 = du__'8746'__642 v1 v3 v4
du__'8746'__642 :: T_Theory_82 -> AgdaAny -> AgdaAny -> AgdaAny
du__'8746'__642 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (coe du_binary'45'unions_606 (coe v0) (coe v1) (coe v2))
-- Axiom.Set.Theory.∈-∪
d_'8712''45''8746'_650 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8746'_650 ~v0 v1 ~v2 v3 v4 v5
  = du_'8712''45''8746'_650 v1 v3 v4 v5
du_'8712''45''8746'_650 ::
  T_Theory_82 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45''8746'_650 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (coe du_binary'45'unions_606 (coe v0) (coe v1) (coe v2)) v3
-- Axiom.Set.Theory.spec-∈
d_spec'45''8712'_652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Theory_82 -> () -> ()
d_spec'45''8712'_652 = erased
-- Axiom.Set.Theory.Intersection._∩_
d__'8745'__666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__666 ~v0 v1 ~v2 v3 v4 v5 = du__'8745'__666 v1 v3 v4 v5
du__'8745'__666 ::
  T_Theory_82 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'8745'__666 v0 v1 v2 v3 = coe du_filter_382 v0 (coe v1 v3) v2
-- Axiom.Set.Theory.Intersection.∈-∩
d_'8712''45''8745'_674 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8745'_674 ~v0 v1 ~v2 v3 v4 v5 v6
  = du_'8712''45''8745'_674 v1 v3 v4 v5 v6
du_'8712''45''8745'_674 ::
  T_Theory_82 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45''8745'_674 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Function.Related.Propositional.du__'8596''10216'_'10217'__220
      (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
      (coe MAlonzo.Code.Data.Product.Algebra.du_'215''45'comm_244)
      (coe
         MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
         (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
         (coe
            du_'8712''45'filter_388 (coe v0) (coe v2) (coe v1 v3) (coe v4))
         (coe
            MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
            (coe
               MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)))
-- Axiom.Set.Theory.Intersection.disjoint'
d_disjoint''_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_disjoint''_686 = erased
-- Axiom.Set.Theory.All
d_All_692 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_All_692 = erased
-- Axiom.Set.Theory.Any
d_Any_700 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_Any_700 = erased
-- Axiom.Set.Theoryᶠ
d_Theory'7584'_708 = ()
data T_Theory'7584'_708
  = C_Theory'7584''46'constructor_98831 T_Theory_82
                                        (() -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14)
-- Axiom.Set._._Preservesˢ_
d__Preserves'738'__714 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738'__714 = erased
-- Axiom.Set._._Preservesˢ₂_
d__Preserves'738''8322'__716 ::
  T_Theory_82 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738''8322'__716 = erased
-- Axiom.Set._._∈_
d__'8712'__718 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8712'__718 = erased
-- Axiom.Set._._∉_
d__'8713'__720 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8713'__720 = erased
-- Axiom.Set._._∪_
d__'8746'__722 ::
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__722 v0 v1 v2 v3 = coe du__'8746'__642 (coe v0) v2 v3
-- Axiom.Set._._≡ᵉ_
d__'8801''7497'__724 ::
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'__724 = erased
-- Axiom.Set._._≡ᵉ'_
d__'8801''7497'''__726 ::
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'''__726 = erased
-- Axiom.Set._._⊆_
d__'8838'__728 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8838'__728 = erased
-- Axiom.Set._.All
d_All_730 :: T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_All_730 = erased
-- Axiom.Set._.Any
d_Any_732 :: T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_Any_732 = erased
-- Axiom.Set._.DecEq∧finite⇒strongly-finite
d_DecEq'8743'finite'8658'strongly'45'finite_734 ::
  T_Theory_82 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_DecEq'8743'finite'8658'strongly'45'finite_734 ~v0
  = du_DecEq'8743'finite'8658'strongly'45'finite_734
du_DecEq'8743'finite'8658'strongly'45'finite_734 ::
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_DecEq'8743'finite'8658'strongly'45'finite_734 v0 v1 v2 v3
  = coe du_DecEq'8743'finite'8658'strongly'45'finite_280 v1 v3
-- Axiom.Set._.FinSet
d_FinSet_736 :: T_Theory_82 -> () -> ()
d_FinSet_736 = erased
-- Axiom.Set._.Set
d_Set_738 :: T_Theory_82 -> () -> ()
d_Set_738 = erased
-- Axiom.Set._.binary-unions
d_binary'45'unions_740 ::
  T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_binary'45'unions_740 v0 v1 v2 v3
  = coe du_binary'45'unions_606 (coe v0) v2 v3
-- Axiom.Set._.card
d_card_742 ::
  T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_card_742 ~v0 = du_card_742
du_card_742 ::
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
du_card_742 v0 v1 = coe du_card_298 v1
-- Axiom.Set._.card-∅
d_card'45''8709'_744 ::
  T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_card'45''8709'_744 = erased
-- Axiom.Set._.concatMapˢ
d_concatMap'738'_746 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_concatMap'738'_746 v0 v1 v2 v3 v4
  = coe du_concatMap'738'_470 (coe v0) v3 v4
-- Axiom.Set._.disjoint
d_disjoint_748 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d_disjoint_748 = erased
-- Axiom.Set._.filter
d_filter_750 ::
  T_Theory_82 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d_filter_750 v0 v1 v2 = coe du_filter_382 (coe v0)
-- Axiom.Set._.finite
d_finite_752 :: T_Theory_82 -> () -> AgdaAny -> ()
d_finite_752 = erased
-- Axiom.Set._.fromList
d_fromList_754 :: T_Theory_82 -> () -> [AgdaAny] -> AgdaAny
d_fromList_754 v0 v1 v2 = coe du_fromList_390 (coe v0) v2
-- Axiom.Set._.isMaximal
d_isMaximal_756 :: T_Theory_82 -> () -> AgdaAny -> ()
d_isMaximal_756 = erased
-- Axiom.Set._.listing
d_listing_758 ::
  T_Theory_82 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_listing_758 v0 = coe d_listing_204 (coe v0)
-- Axiom.Set._.map
d_map_760 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map_760 v0 v1 v2 = coe du_map_360 (coe v0)
-- Axiom.Set._.mapPartial
d_mapPartial_762 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_mapPartial_762 v0 v1 v2 v3 = coe du_mapPartial_538 (coe v0) v3
-- Axiom.Set._.maximal-unique
d_maximal'45'unique_764 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_maximal'45'unique_764 ~v0 = du_maximal'45'unique_764
du_maximal'45'unique_764 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_maximal'45'unique_764 v0 v1 v2 v3 v4
  = coe du_maximal'45'unique_322 v3 v4
-- Axiom.Set._.maximal-⊆
d_maximal'45''8838'_766 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_maximal'45''8838'_766 ~v0 = du_maximal'45''8838'_766
du_maximal'45''8838'_766 ::
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_maximal'45''8838'_766 v0 v1 v2 v3 v4 v5
  = coe du_maximal'45''8838'_318 v3 v4
-- Axiom.Set._.partialToSet
d_partialToSet_768 ::
  T_Theory_82 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_partialToSet_768 v0 v1 v2 v3 v4 v5
  = coe du_partialToSet_434 (coe v0) v4 v5
-- Axiom.Set._.replacement
d_replacement_770 ::
  T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_replacement_770 v0 = coe d_replacement_196 (coe v0)
-- Axiom.Set._.singleton
d_singleton_772 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny
d_singleton_772 v0 v1 v2 = coe du_singleton_410 (coe v0) v2
-- Axiom.Set._.sp
d_sp_774 :: T_Theory_82 -> T_SpecProperty_48
d_sp_774 v0 = coe d_sp_150 (coe v0)
-- Axiom.Set._.sp-¬
d_sp'45''172'_776 ::
  T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny
d_sp'45''172'_776 v0 = coe d_sp'45''172'_70 (coe d_sp_150 (coe v0))
-- Axiom.Set._.sp-∘
d_sp'45''8728'_778 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_sp'45''8728'_778 v0
  = coe d_sp'45''8728'_68 (coe d_sp_150 (coe v0))
-- Axiom.Set._.spec-∈
d_spec'45''8712'_780 :: T_Theory_82 -> () -> ()
d_spec'45''8712'_780 = erased
-- Axiom.Set._.specProperty
d_specProperty_782 :: T_Theory_82 -> () -> (AgdaAny -> ()) -> ()
d_specProperty_782 = erased
-- Axiom.Set._.specification
d_specification_784 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_specification_784 v0 = coe d_specification_174 (coe v0)
-- Axiom.Set._.strictify
d_strictify_786 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_strictify_786 v0 v1 v2 v3 v4
  = coe du_strictify_340 (coe v0) v3 v4
-- Axiom.Set._.strongly-finite
d_strongly'45'finite_788 :: T_Theory_82 -> () -> AgdaAny -> ()
d_strongly'45'finite_788 = erased
-- Axiom.Set._.unions
d_unions_790 ::
  T_Theory_82 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unions_790 v0 = coe d_unions_184 (coe v0)
-- Axiom.Set._.weakly-finite
d_weakly'45'finite_792 :: T_Theory_82 -> () -> AgdaAny -> ()
d_weakly'45'finite_792 = erased
-- Axiom.Set._.∅
d_'8709'_794 :: T_Theory_82 -> () -> AgdaAny
d_'8709'_794 v0 v1 = coe du_'8709'_404 (coe v0)
-- Axiom.Set._.∅-strongly-finite
d_'8709''45'strongly'45'finite_796 ::
  T_Theory_82 -> () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8709''45'strongly'45'finite_796 v0 v1
  = coe du_'8709''45'strongly'45'finite_406 (coe v0)
-- Axiom.Set._.∈-concatMapˢ
d_'8712''45'concatMap'738'_798 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'concatMap'738'_798 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'concatMap'738'_482 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-filter
d_'8712''45'filter_800 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'filter_800 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'filter_388 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-fromList
d_'8712''45'fromList_802 ::
  T_Theory_82 ->
  () ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'fromList_802 v0 v1 v2 v3
  = coe du_'8712''45'fromList_394 (coe v0) v2 v3
-- Axiom.Set._.∈-map
d_'8712''45'map_804 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'map_804 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'map_368 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-mapPartial
d_'8712''45'mapPartial_806 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'mapPartial_806 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'mapPartial_548 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-map′
d_'8712''45'map'8242'_808 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8712''45'map'8242'_808 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8712''45'map'8242'_374 (coe v0) v3 v4 v5 v6
-- Axiom.Set._.∈-partialToSet
d_'8712''45'partialToSet_810 ::
  T_Theory_82 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'partialToSet_810 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8712''45'partialToSet_446 (coe v0) v5
-- Axiom.Set._.∈-singleton
d_'8712''45'singleton_812 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'singleton_812 v0 v1 v2 v3
  = coe du_'8712''45'singleton_420 (coe v0) v2 v3
-- Axiom.Set._.∈-unions
d_'8712''45'unions_814 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'unions_814 v0 v1 v2 v3
  = coe du_'8712''45'unions_402 (coe v0) v2 v3
-- Axiom.Set._.∈-∪
d_'8712''45''8746'_816 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8746'_816 v0 v1 v2 v3 v4
  = coe du_'8712''45''8746'_650 (coe v0) v2 v3 v4
-- Axiom.Set._.⊆-mapPartial
d_'8838''45'mapPartial_818 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) -> Maybe AgdaAny -> AgdaAny -> AgdaAny
d_'8838''45'mapPartial_818 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8838''45'mapPartial_566 (coe v0) v3 v4 v5 v6
-- Axiom.Set._.⊆-weakly-finite
d_'8838''45'weakly'45'finite_820 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8838''45'weakly'45'finite_820 ~v0
  = du_'8838''45'weakly'45'finite_820
du_'8838''45'weakly'45'finite_820 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8838''45'weakly'45'finite_820 v0 v1 v2 v3 v4
  = coe du_'8838''45'weakly'45'finite_302 v3 v4
-- Axiom.Set._.❴_❵
d_'10100'_'10101'_822 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny
d_'10100'_'10101'_822 v0 = coe du_'10100'_'10101'_414 (coe v0)
-- Axiom.Set._.Intersection._∩_
d__'8745'__826 ::
  T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__826 v0 v1 v2 v3 v4
  = coe du__'8745'__666 (coe v0) v2 v3 v4
-- Axiom.Set._.Intersection.disjoint'
d_disjoint''_828 ::
  T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_disjoint''_828 = erased
-- Axiom.Set._.Intersection.∈-∩
d_'8712''45''8745'_830 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8745'_830 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45''8745'_674 (coe v0) v2 v3 v4 v5
-- Axiom.Set.Theoryᶠ.theory
d_theory_836 :: T_Theory'7584'_708 -> T_Theory_82
d_theory_836 v0
  = case coe v0 of
      C_Theory'7584''46'constructor_98831 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᶠ._._Preservesˢ_
d__Preserves'738'__840 ::
  T_Theory'7584'_708 ->
  () -> () -> (AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738'__840 = erased
-- Axiom.Set.Theoryᶠ._._Preservesˢ₂_
d__Preserves'738''8322'__842 ::
  T_Theory'7584'_708 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738''8322'__842 = erased
-- Axiom.Set.Theoryᶠ._._∈_
d__'8712'__844 ::
  T_Theory'7584'_708 -> () -> AgdaAny -> AgdaAny -> ()
d__'8712'__844 = erased
-- Axiom.Set.Theoryᶠ._._∉_
d__'8713'__846 ::
  T_Theory'7584'_708 -> () -> AgdaAny -> AgdaAny -> ()
d__'8713'__846 = erased
-- Axiom.Set.Theoryᶠ._._∪_
d__'8746'__848 ::
  T_Theory'7584'_708 -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__848 v0 v1 v2 v3
  = coe du__'8746'__642 (coe d_theory_836 (coe v0)) v2 v3
-- Axiom.Set.Theoryᶠ._._≡ᵉ_
d__'8801''7497'__850 ::
  T_Theory'7584'_708 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'__850 = erased
-- Axiom.Set.Theoryᶠ._._≡ᵉ'_
d__'8801''7497'''__852 ::
  T_Theory'7584'_708 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'''__852 = erased
-- Axiom.Set.Theoryᶠ._._⊆_
d__'8838'__854 ::
  T_Theory'7584'_708 -> () -> AgdaAny -> AgdaAny -> ()
d__'8838'__854 = erased
-- Axiom.Set.Theoryᶠ._.All
d_All_856 ::
  T_Theory'7584'_708 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_All_856 = erased
-- Axiom.Set.Theoryᶠ._.Any
d_Any_858 ::
  T_Theory'7584'_708 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_Any_858 = erased
-- Axiom.Set.Theoryᶠ._.DecEq∧finite⇒strongly-finite
d_DecEq'8743'finite'8658'strongly'45'finite_860 ::
  T_Theory'7584'_708 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_DecEq'8743'finite'8658'strongly'45'finite_860 ~v0
  = du_DecEq'8743'finite'8658'strongly'45'finite_860
du_DecEq'8743'finite'8658'strongly'45'finite_860 ::
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_DecEq'8743'finite'8658'strongly'45'finite_860 v0 v1 v2 v3
  = coe du_DecEq'8743'finite'8658'strongly'45'finite_280 v1 v3
-- Axiom.Set.Theoryᶠ._.FinSet
d_FinSet_862 :: T_Theory'7584'_708 -> () -> ()
d_FinSet_862 = erased
-- Axiom.Set.Theoryᶠ._.Set
d_Set_864 :: T_Theory'7584'_708 -> () -> ()
d_Set_864 = erased
-- Axiom.Set.Theoryᶠ._.binary-unions
d_binary'45'unions_866 ::
  T_Theory'7584'_708 ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_binary'45'unions_866 v0 v1 v2 v3
  = coe du_binary'45'unions_606 (coe d_theory_836 (coe v0)) v2 v3
-- Axiom.Set.Theoryᶠ._.card
d_card_868 ::
  T_Theory'7584'_708 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_card_868 ~v0 = du_card_868
du_card_868 ::
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
du_card_868 v0 v1 = coe du_card_298 v1
-- Axiom.Set.Theoryᶠ._.card-∅
d_card'45''8709'_870 ::
  T_Theory'7584'_708 ->
  () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_card'45''8709'_870 = erased
-- Axiom.Set.Theoryᶠ._.concatMapˢ
d_concatMap'738'_872 ::
  T_Theory'7584'_708 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_concatMap'738'_872 v0 v1 v2 v3 v4
  = coe du_concatMap'738'_470 (coe d_theory_836 (coe v0)) v3 v4
-- Axiom.Set.Theoryᶠ._.disjoint
d_disjoint_874 ::
  T_Theory'7584'_708 -> () -> AgdaAny -> AgdaAny -> ()
d_disjoint_874 = erased
-- Axiom.Set.Theoryᶠ._.filter
d_filter_876 ::
  T_Theory'7584'_708 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d_filter_876 v0 v1 v2
  = coe du_filter_382 (coe d_theory_836 (coe v0))
-- Axiom.Set.Theoryᶠ._.finite
d_finite_878 :: T_Theory'7584'_708 -> () -> AgdaAny -> ()
d_finite_878 = erased
-- Axiom.Set.Theoryᶠ._.fromList
d_fromList_880 :: T_Theory'7584'_708 -> () -> [AgdaAny] -> AgdaAny
d_fromList_880 v0 v1 v2
  = coe du_fromList_390 (coe d_theory_836 (coe v0)) v2
-- Axiom.Set.Theoryᶠ._.isMaximal
d_isMaximal_882 :: T_Theory'7584'_708 -> () -> AgdaAny -> ()
d_isMaximal_882 = erased
-- Axiom.Set.Theoryᶠ._.listing
d_listing_884 ::
  T_Theory'7584'_708 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_listing_884 v0 = coe d_listing_204 (coe d_theory_836 (coe v0))
-- Axiom.Set.Theoryᶠ._.map
d_map_886 ::
  T_Theory'7584'_708 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map_886 v0 v1 v2 = coe du_map_360 (coe d_theory_836 (coe v0))
-- Axiom.Set.Theoryᶠ._.mapPartial
d_mapPartial_888 ::
  T_Theory'7584'_708 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_mapPartial_888 v0 v1 v2 v3
  = coe du_mapPartial_538 (coe d_theory_836 (coe v0)) v3
-- Axiom.Set.Theoryᶠ._.maximal-unique
d_maximal'45'unique_890 ::
  T_Theory'7584'_708 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_maximal'45'unique_890 ~v0 = du_maximal'45'unique_890
du_maximal'45'unique_890 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_maximal'45'unique_890 v0 v1 v2 v3 v4
  = coe du_maximal'45'unique_322 v3 v4
-- Axiom.Set.Theoryᶠ._.maximal-⊆
d_maximal'45''8838'_892 ::
  T_Theory'7584'_708 ->
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_maximal'45''8838'_892 ~v0 = du_maximal'45''8838'_892
du_maximal'45''8838'_892 ::
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_maximal'45''8838'_892 v0 v1 v2 v3 v4 v5
  = coe du_maximal'45''8838'_318 v3 v4
-- Axiom.Set.Theoryᶠ._.partialToSet
d_partialToSet_894 ::
  T_Theory'7584'_708 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_partialToSet_894 v0 v1 v2 v3 v4 v5
  = coe du_partialToSet_434 (coe d_theory_836 (coe v0)) v4 v5
-- Axiom.Set.Theoryᶠ._.replacement
d_replacement_896 ::
  T_Theory'7584'_708 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_replacement_896 v0
  = coe d_replacement_196 (coe d_theory_836 (coe v0))
-- Axiom.Set.Theoryᶠ._.singleton
d_singleton_898 :: T_Theory'7584'_708 -> () -> AgdaAny -> AgdaAny
d_singleton_898 v0 v1 v2
  = coe du_singleton_410 (coe d_theory_836 (coe v0)) v2
-- Axiom.Set.Theoryᶠ._.sp
d_sp_900 :: T_Theory'7584'_708 -> T_SpecProperty_48
d_sp_900 v0 = coe d_sp_150 (coe d_theory_836 (coe v0))
-- Axiom.Set.Theoryᶠ._.sp-¬
d_sp'45''172'_902 ::
  T_Theory'7584'_708 -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny
d_sp'45''172'_902 v0
  = coe d_sp'45''172'_70 (coe d_sp_150 (coe d_theory_836 (coe v0)))
-- Axiom.Set.Theoryᶠ._.sp-∘
d_sp'45''8728'_904 ::
  T_Theory'7584'_708 ->
  () ->
  (AgdaAny -> ()) -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_sp'45''8728'_904 v0
  = coe d_sp'45''8728'_68 (coe d_sp_150 (coe d_theory_836 (coe v0)))
-- Axiom.Set.Theoryᶠ._.spec-∈
d_spec'45''8712'_906 :: T_Theory'7584'_708 -> () -> ()
d_spec'45''8712'_906 = erased
-- Axiom.Set.Theoryᶠ._.specProperty
d_specProperty_908 ::
  T_Theory'7584'_708 -> () -> (AgdaAny -> ()) -> ()
d_specProperty_908 = erased
-- Axiom.Set.Theoryᶠ._.specification
d_specification_910 ::
  T_Theory'7584'_708 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_specification_910 v0
  = coe d_specification_174 (coe d_theory_836 (coe v0))
-- Axiom.Set.Theoryᶠ._.strictify
d_strictify_912 ::
  T_Theory'7584'_708 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_strictify_912 v0 v1 v2 v3 v4
  = coe du_strictify_340 (coe d_theory_836 (coe v0)) v3 v4
-- Axiom.Set.Theoryᶠ._.strongly-finite
d_strongly'45'finite_914 ::
  T_Theory'7584'_708 -> () -> AgdaAny -> ()
d_strongly'45'finite_914 = erased
-- Axiom.Set.Theoryᶠ._.unions
d_unions_916 ::
  T_Theory'7584'_708 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unions_916 v0 = coe d_unions_184 (coe d_theory_836 (coe v0))
-- Axiom.Set.Theoryᶠ._.weakly-finite
d_weakly'45'finite_918 :: T_Theory'7584'_708 -> () -> AgdaAny -> ()
d_weakly'45'finite_918 = erased
-- Axiom.Set.Theoryᶠ._.∅
d_'8709'_920 :: T_Theory'7584'_708 -> () -> AgdaAny
d_'8709'_920 v0 v1 = coe du_'8709'_404 (coe d_theory_836 (coe v0))
-- Axiom.Set.Theoryᶠ._.∅-strongly-finite
d_'8709''45'strongly'45'finite_922 ::
  T_Theory'7584'_708 -> () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8709''45'strongly'45'finite_922 v0 v1
  = coe
      du_'8709''45'strongly'45'finite_406 (coe d_theory_836 (coe v0))
-- Axiom.Set.Theoryᶠ._.∈-concatMapˢ
d_'8712''45'concatMap'738'_924 ::
  T_Theory'7584'_708 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'concatMap'738'_924 v0 v1 v2 v3 v4 v5
  = coe
      du_'8712''45'concatMap'738'_482 (coe d_theory_836 (coe v0)) v3 v4
      v5
-- Axiom.Set.Theoryᶠ._.∈-filter
d_'8712''45'filter_926 ::
  T_Theory'7584'_708 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'filter_926 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'filter_388 (coe d_theory_836 (coe v0)) v3 v4 v5
-- Axiom.Set.Theoryᶠ._.∈-fromList
d_'8712''45'fromList_928 ::
  T_Theory'7584'_708 ->
  () ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'fromList_928 v0 v1 v2 v3
  = coe du_'8712''45'fromList_394 (coe d_theory_836 (coe v0)) v2 v3
-- Axiom.Set.Theoryᶠ._.∈-map
d_'8712''45'map_930 ::
  T_Theory'7584'_708 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'map_930 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'map_368 (coe d_theory_836 (coe v0)) v3 v4 v5
-- Axiom.Set.Theoryᶠ._.∈-mapPartial
d_'8712''45'mapPartial_932 ::
  T_Theory'7584'_708 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'mapPartial_932 v0 v1 v2 v3 v4 v5
  = coe
      du_'8712''45'mapPartial_548 (coe d_theory_836 (coe v0)) v3 v4 v5
-- Axiom.Set.Theoryᶠ._.∈-map′
d_'8712''45'map'8242'_934 ::
  T_Theory'7584'_708 ->
  () ->
  () ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8712''45'map'8242'_934 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_'8712''45'map'8242'_374 (coe d_theory_836 (coe v0)) v3 v4 v5 v6
-- Axiom.Set.Theoryᶠ._.∈-partialToSet
d_'8712''45'partialToSet_936 ::
  T_Theory'7584'_708 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'partialToSet_936 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8712''45'partialToSet_446 (coe d_theory_836 (coe v0)) v5
-- Axiom.Set.Theoryᶠ._.∈-singleton
d_'8712''45'singleton_938 ::
  T_Theory'7584'_708 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'singleton_938 v0 v1 v2 v3
  = coe du_'8712''45'singleton_420 (coe d_theory_836 (coe v0)) v2 v3
-- Axiom.Set.Theoryᶠ._.∈-unions
d_'8712''45'unions_940 ::
  T_Theory'7584'_708 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'unions_940 v0 v1 v2 v3
  = coe du_'8712''45'unions_402 (coe d_theory_836 (coe v0)) v2 v3
-- Axiom.Set.Theoryᶠ._.∈-∪
d_'8712''45''8746'_942 ::
  T_Theory'7584'_708 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8746'_942 v0 v1 v2 v3 v4
  = coe du_'8712''45''8746'_650 (coe d_theory_836 (coe v0)) v2 v3 v4
-- Axiom.Set.Theoryᶠ._.⊆-mapPartial
d_'8838''45'mapPartial_944 ::
  T_Theory'7584'_708 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) -> Maybe AgdaAny -> AgdaAny -> AgdaAny
d_'8838''45'mapPartial_944 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_'8838''45'mapPartial_566 (coe d_theory_836 (coe v0)) v3 v4 v5 v6
-- Axiom.Set.Theoryᶠ._.⊆-weakly-finite
d_'8838''45'weakly'45'finite_946 ::
  T_Theory'7584'_708 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8838''45'weakly'45'finite_946 ~v0
  = du_'8838''45'weakly'45'finite_946
du_'8838''45'weakly'45'finite_946 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8838''45'weakly'45'finite_946 v0 v1 v2 v3 v4
  = coe du_'8838''45'weakly'45'finite_302 v3 v4
-- Axiom.Set.Theoryᶠ._.❴_❵
d_'10100'_'10101'_948 ::
  T_Theory'7584'_708 -> () -> AgdaAny -> AgdaAny
d_'10100'_'10101'_948 v0
  = coe du_'10100'_'10101'_414 (coe d_theory_836 (coe v0))
-- Axiom.Set.Theoryᶠ._.Intersection._∩_
d__'8745'__952 ::
  T_Theory'7584'_708 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__952 v0 v1 v2 v3 v4
  = coe du__'8745'__666 (coe d_theory_836 (coe v0)) v2 v3 v4
-- Axiom.Set.Theoryᶠ._.Intersection.disjoint'
d_disjoint''_954 ::
  T_Theory'7584'_708 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_disjoint''_954 = erased
-- Axiom.Set.Theoryᶠ._.Intersection.∈-∩
d_'8712''45''8745'_956 ::
  T_Theory'7584'_708 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8745'_956 v0 v1 v2 v3 v4 v5
  = coe
      du_'8712''45''8745'_674 (coe d_theory_836 (coe v0)) v2 v3 v4 v5
-- Axiom.Set.Theoryᶠ.finiteness
d_finiteness_960 ::
  T_Theory'7584'_708 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_finiteness_960 v0
  = case coe v0 of
      C_Theory'7584''46'constructor_98831 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᶠ.DecEq⇒strongly-finite
d_DecEq'8658'strongly'45'finite_964 ::
  T_Theory'7584'_708 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_DecEq'8658'strongly'45'finite_964 v0 ~v1 v2 v3
  = du_DecEq'8658'strongly'45'finite_964 v0 v2 v3
du_DecEq'8658'strongly'45'finite_964 ::
  T_Theory'7584'_708 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_DecEq'8658'strongly'45'finite_964 v0 v1 v2
  = coe
      du_DecEq'8743'finite'8658'strongly'45'finite_280 (coe v1)
      (coe d_finiteness_960 v0 erased v2)
-- Axiom.Set.Theoryᶠ.lengthˢ
d_length'738'_968 ::
  T_Theory'7584'_708 ->
  () -> MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> AgdaAny -> Integer
d_length'738'_968 v0 ~v1 v2 v3 = du_length'738'_968 v0 v2 v3
du_length'738'_968 ::
  T_Theory'7584'_708 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> AgdaAny -> Integer
du_length'738'_968 v0 v1 v2
  = coe
      du_card_298
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v2)
         (coe
            du_DecEq'8658'strongly'45'finite_964 (coe v0) (coe v1) (coe v2)))
-- Axiom.Set.Theoryⁱ
d_Theory'8305'_972 = ()
data T_Theory'8305'_972
  = C_Theory'8305''46'constructor_101947 T_Theory_82
                                         MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Axiom.Set._._Preservesˢ_
d__Preserves'738'__978 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738'__978 = erased
-- Axiom.Set._._Preservesˢ₂_
d__Preserves'738''8322'__980 ::
  T_Theory_82 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738''8322'__980 = erased
-- Axiom.Set._._∈_
d__'8712'__982 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8712'__982 = erased
-- Axiom.Set._._∉_
d__'8713'__984 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8713'__984 = erased
-- Axiom.Set._._∪_
d__'8746'__986 ::
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__986 v0 v1 v2 v3 = coe du__'8746'__642 (coe v0) v2 v3
-- Axiom.Set._._≡ᵉ_
d__'8801''7497'__988 ::
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'__988 = erased
-- Axiom.Set._._≡ᵉ'_
d__'8801''7497'''__990 ::
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'''__990 = erased
-- Axiom.Set._._⊆_
d__'8838'__992 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8838'__992 = erased
-- Axiom.Set._.All
d_All_994 :: T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_All_994 = erased
-- Axiom.Set._.Any
d_Any_996 :: T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_Any_996 = erased
-- Axiom.Set._.DecEq∧finite⇒strongly-finite
d_DecEq'8743'finite'8658'strongly'45'finite_998 ::
  T_Theory_82 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_DecEq'8743'finite'8658'strongly'45'finite_998 ~v0
  = du_DecEq'8743'finite'8658'strongly'45'finite_998
du_DecEq'8743'finite'8658'strongly'45'finite_998 ::
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_DecEq'8743'finite'8658'strongly'45'finite_998 v0 v1 v2 v3
  = coe du_DecEq'8743'finite'8658'strongly'45'finite_280 v1 v3
-- Axiom.Set._.FinSet
d_FinSet_1000 :: T_Theory_82 -> () -> ()
d_FinSet_1000 = erased
-- Axiom.Set._.Set
d_Set_1002 :: T_Theory_82 -> () -> ()
d_Set_1002 = erased
-- Axiom.Set._.binary-unions
d_binary'45'unions_1004 ::
  T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_binary'45'unions_1004 v0 v1 v2 v3
  = coe du_binary'45'unions_606 (coe v0) v2 v3
-- Axiom.Set._.card
d_card_1006 ::
  T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_card_1006 ~v0 = du_card_1006
du_card_1006 ::
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
du_card_1006 v0 v1 = coe du_card_298 v1
-- Axiom.Set._.card-∅
d_card'45''8709'_1008 ::
  T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_card'45''8709'_1008 = erased
-- Axiom.Set._.concatMapˢ
d_concatMap'738'_1010 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_concatMap'738'_1010 v0 v1 v2 v3 v4
  = coe du_concatMap'738'_470 (coe v0) v3 v4
-- Axiom.Set._.disjoint
d_disjoint_1012 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d_disjoint_1012 = erased
-- Axiom.Set._.filter
d_filter_1014 ::
  T_Theory_82 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d_filter_1014 v0 v1 v2 = coe du_filter_382 (coe v0)
-- Axiom.Set._.finite
d_finite_1016 :: T_Theory_82 -> () -> AgdaAny -> ()
d_finite_1016 = erased
-- Axiom.Set._.fromList
d_fromList_1018 :: T_Theory_82 -> () -> [AgdaAny] -> AgdaAny
d_fromList_1018 v0 v1 v2 = coe du_fromList_390 (coe v0) v2
-- Axiom.Set._.isMaximal
d_isMaximal_1020 :: T_Theory_82 -> () -> AgdaAny -> ()
d_isMaximal_1020 = erased
-- Axiom.Set._.listing
d_listing_1022 ::
  T_Theory_82 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_listing_1022 v0 = coe d_listing_204 (coe v0)
-- Axiom.Set._.map
d_map_1024 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map_1024 v0 v1 v2 = coe du_map_360 (coe v0)
-- Axiom.Set._.mapPartial
d_mapPartial_1026 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_mapPartial_1026 v0 v1 v2 v3 = coe du_mapPartial_538 (coe v0) v3
-- Axiom.Set._.maximal-unique
d_maximal'45'unique_1028 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_maximal'45'unique_1028 ~v0 = du_maximal'45'unique_1028
du_maximal'45'unique_1028 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_maximal'45'unique_1028 v0 v1 v2 v3 v4
  = coe du_maximal'45'unique_322 v3 v4
-- Axiom.Set._.maximal-⊆
d_maximal'45''8838'_1030 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_maximal'45''8838'_1030 ~v0 = du_maximal'45''8838'_1030
du_maximal'45''8838'_1030 ::
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_maximal'45''8838'_1030 v0 v1 v2 v3 v4 v5
  = coe du_maximal'45''8838'_318 v3 v4
-- Axiom.Set._.partialToSet
d_partialToSet_1032 ::
  T_Theory_82 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_partialToSet_1032 v0 v1 v2 v3 v4 v5
  = coe du_partialToSet_434 (coe v0) v4 v5
-- Axiom.Set._.replacement
d_replacement_1034 ::
  T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_replacement_1034 v0 = coe d_replacement_196 (coe v0)
-- Axiom.Set._.singleton
d_singleton_1036 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny
d_singleton_1036 v0 v1 v2 = coe du_singleton_410 (coe v0) v2
-- Axiom.Set._.sp
d_sp_1038 :: T_Theory_82 -> T_SpecProperty_48
d_sp_1038 v0 = coe d_sp_150 (coe v0)
-- Axiom.Set._.sp-¬
d_sp'45''172'_1040 ::
  T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny
d_sp'45''172'_1040 v0
  = coe d_sp'45''172'_70 (coe d_sp_150 (coe v0))
-- Axiom.Set._.sp-∘
d_sp'45''8728'_1042 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_sp'45''8728'_1042 v0
  = coe d_sp'45''8728'_68 (coe d_sp_150 (coe v0))
-- Axiom.Set._.spec-∈
d_spec'45''8712'_1044 :: T_Theory_82 -> () -> ()
d_spec'45''8712'_1044 = erased
-- Axiom.Set._.specProperty
d_specProperty_1046 :: T_Theory_82 -> () -> (AgdaAny -> ()) -> ()
d_specProperty_1046 = erased
-- Axiom.Set._.specification
d_specification_1048 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_specification_1048 v0 = coe d_specification_174 (coe v0)
-- Axiom.Set._.strictify
d_strictify_1050 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_strictify_1050 v0 v1 v2 v3 v4
  = coe du_strictify_340 (coe v0) v3 v4
-- Axiom.Set._.strongly-finite
d_strongly'45'finite_1052 :: T_Theory_82 -> () -> AgdaAny -> ()
d_strongly'45'finite_1052 = erased
-- Axiom.Set._.unions
d_unions_1054 ::
  T_Theory_82 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unions_1054 v0 = coe d_unions_184 (coe v0)
-- Axiom.Set._.weakly-finite
d_weakly'45'finite_1056 :: T_Theory_82 -> () -> AgdaAny -> ()
d_weakly'45'finite_1056 = erased
-- Axiom.Set._.∅
d_'8709'_1058 :: T_Theory_82 -> () -> AgdaAny
d_'8709'_1058 v0 v1 = coe du_'8709'_404 (coe v0)
-- Axiom.Set._.∅-strongly-finite
d_'8709''45'strongly'45'finite_1060 ::
  T_Theory_82 -> () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8709''45'strongly'45'finite_1060 v0 v1
  = coe du_'8709''45'strongly'45'finite_406 (coe v0)
-- Axiom.Set._.∈-concatMapˢ
d_'8712''45'concatMap'738'_1062 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'concatMap'738'_1062 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'concatMap'738'_482 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-filter
d_'8712''45'filter_1064 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'filter_1064 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'filter_388 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-fromList
d_'8712''45'fromList_1066 ::
  T_Theory_82 ->
  () ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'fromList_1066 v0 v1 v2 v3
  = coe du_'8712''45'fromList_394 (coe v0) v2 v3
-- Axiom.Set._.∈-map
d_'8712''45'map_1068 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'map_1068 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'map_368 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-mapPartial
d_'8712''45'mapPartial_1070 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'mapPartial_1070 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'mapPartial_548 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-map′
d_'8712''45'map'8242'_1072 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8712''45'map'8242'_1072 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8712''45'map'8242'_374 (coe v0) v3 v4 v5 v6
-- Axiom.Set._.∈-partialToSet
d_'8712''45'partialToSet_1074 ::
  T_Theory_82 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'partialToSet_1074 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8712''45'partialToSet_446 (coe v0) v5
-- Axiom.Set._.∈-singleton
d_'8712''45'singleton_1076 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'singleton_1076 v0 v1 v2 v3
  = coe du_'8712''45'singleton_420 (coe v0) v2 v3
-- Axiom.Set._.∈-unions
d_'8712''45'unions_1078 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'unions_1078 v0 v1 v2 v3
  = coe du_'8712''45'unions_402 (coe v0) v2 v3
-- Axiom.Set._.∈-∪
d_'8712''45''8746'_1080 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8746'_1080 v0 v1 v2 v3 v4
  = coe du_'8712''45''8746'_650 (coe v0) v2 v3 v4
-- Axiom.Set._.⊆-mapPartial
d_'8838''45'mapPartial_1082 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) -> Maybe AgdaAny -> AgdaAny -> AgdaAny
d_'8838''45'mapPartial_1082 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8838''45'mapPartial_566 (coe v0) v3 v4 v5 v6
-- Axiom.Set._.⊆-weakly-finite
d_'8838''45'weakly'45'finite_1084 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8838''45'weakly'45'finite_1084 ~v0
  = du_'8838''45'weakly'45'finite_1084
du_'8838''45'weakly'45'finite_1084 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8838''45'weakly'45'finite_1084 v0 v1 v2 v3 v4
  = coe du_'8838''45'weakly'45'finite_302 v3 v4
-- Axiom.Set._.❴_❵
d_'10100'_'10101'_1086 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny
d_'10100'_'10101'_1086 v0 = coe du_'10100'_'10101'_414 (coe v0)
-- Axiom.Set._.Intersection._∩_
d__'8745'__1090 ::
  T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__1090 v0 v1 v2 v3 v4
  = coe du__'8745'__666 (coe v0) v2 v3 v4
-- Axiom.Set._.Intersection.disjoint'
d_disjoint''_1092 ::
  T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_disjoint''_1092 = erased
-- Axiom.Set._.Intersection.∈-∩
d_'8712''45''8745'_1094 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8745'_1094 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45''8745'_674 (coe v0) v2 v3 v4 v5
-- Axiom.Set.Theoryⁱ.theory
d_theory_1102 :: T_Theory'8305'_972 -> T_Theory_82
d_theory_1102 v0
  = case coe v0 of
      C_Theory'8305''46'constructor_101947 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryⁱ._._Preservesˢ_
d__Preserves'738'__1106 ::
  T_Theory'8305'_972 ->
  () -> () -> (AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738'__1106 = erased
-- Axiom.Set.Theoryⁱ._._Preservesˢ₂_
d__Preserves'738''8322'__1108 ::
  T_Theory'8305'_972 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738''8322'__1108 = erased
-- Axiom.Set.Theoryⁱ._._∈_
d__'8712'__1110 ::
  T_Theory'8305'_972 -> () -> AgdaAny -> AgdaAny -> ()
d__'8712'__1110 = erased
-- Axiom.Set.Theoryⁱ._._∉_
d__'8713'__1112 ::
  T_Theory'8305'_972 -> () -> AgdaAny -> AgdaAny -> ()
d__'8713'__1112 = erased
-- Axiom.Set.Theoryⁱ._._∪_
d__'8746'__1114 ::
  T_Theory'8305'_972 -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__1114 v0 v1 v2 v3
  = coe du__'8746'__642 (coe d_theory_1102 (coe v0)) v2 v3
-- Axiom.Set.Theoryⁱ._._≡ᵉ_
d__'8801''7497'__1116 ::
  T_Theory'8305'_972 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'__1116 = erased
-- Axiom.Set.Theoryⁱ._._≡ᵉ'_
d__'8801''7497'''__1118 ::
  T_Theory'8305'_972 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'''__1118 = erased
-- Axiom.Set.Theoryⁱ._._⊆_
d__'8838'__1120 ::
  T_Theory'8305'_972 -> () -> AgdaAny -> AgdaAny -> ()
d__'8838'__1120 = erased
-- Axiom.Set.Theoryⁱ._.All
d_All_1122 ::
  T_Theory'8305'_972 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_All_1122 = erased
-- Axiom.Set.Theoryⁱ._.Any
d_Any_1124 ::
  T_Theory'8305'_972 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_Any_1124 = erased
-- Axiom.Set.Theoryⁱ._.DecEq∧finite⇒strongly-finite
d_DecEq'8743'finite'8658'strongly'45'finite_1126 ::
  T_Theory'8305'_972 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_DecEq'8743'finite'8658'strongly'45'finite_1126 ~v0
  = du_DecEq'8743'finite'8658'strongly'45'finite_1126
du_DecEq'8743'finite'8658'strongly'45'finite_1126 ::
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_DecEq'8743'finite'8658'strongly'45'finite_1126 v0 v1 v2 v3
  = coe du_DecEq'8743'finite'8658'strongly'45'finite_280 v1 v3
-- Axiom.Set.Theoryⁱ._.FinSet
d_FinSet_1128 :: T_Theory'8305'_972 -> () -> ()
d_FinSet_1128 = erased
-- Axiom.Set.Theoryⁱ._.Set
d_Set_1130 :: T_Theory'8305'_972 -> () -> ()
d_Set_1130 = erased
-- Axiom.Set.Theoryⁱ._.binary-unions
d_binary'45'unions_1132 ::
  T_Theory'8305'_972 ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_binary'45'unions_1132 v0 v1 v2 v3
  = coe du_binary'45'unions_606 (coe d_theory_1102 (coe v0)) v2 v3
-- Axiom.Set.Theoryⁱ._.card
d_card_1134 ::
  T_Theory'8305'_972 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_card_1134 ~v0 = du_card_1134
du_card_1134 ::
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
du_card_1134 v0 v1 = coe du_card_298 v1
-- Axiom.Set.Theoryⁱ._.card-∅
d_card'45''8709'_1136 ::
  T_Theory'8305'_972 ->
  () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_card'45''8709'_1136 = erased
-- Axiom.Set.Theoryⁱ._.concatMapˢ
d_concatMap'738'_1138 ::
  T_Theory'8305'_972 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_concatMap'738'_1138 v0 v1 v2 v3 v4
  = coe du_concatMap'738'_470 (coe d_theory_1102 (coe v0)) v3 v4
-- Axiom.Set.Theoryⁱ._.disjoint
d_disjoint_1140 ::
  T_Theory'8305'_972 -> () -> AgdaAny -> AgdaAny -> ()
d_disjoint_1140 = erased
-- Axiom.Set.Theoryⁱ._.filter
d_filter_1142 ::
  T_Theory'8305'_972 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d_filter_1142 v0 v1 v2
  = coe du_filter_382 (coe d_theory_1102 (coe v0))
-- Axiom.Set.Theoryⁱ._.finite
d_finite_1144 :: T_Theory'8305'_972 -> () -> AgdaAny -> ()
d_finite_1144 = erased
-- Axiom.Set.Theoryⁱ._.fromList
d_fromList_1146 :: T_Theory'8305'_972 -> () -> [AgdaAny] -> AgdaAny
d_fromList_1146 v0 v1 v2
  = coe du_fromList_390 (coe d_theory_1102 (coe v0)) v2
-- Axiom.Set.Theoryⁱ._.isMaximal
d_isMaximal_1148 :: T_Theory'8305'_972 -> () -> AgdaAny -> ()
d_isMaximal_1148 = erased
-- Axiom.Set.Theoryⁱ._.listing
d_listing_1150 ::
  T_Theory'8305'_972 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_listing_1150 v0 = coe d_listing_204 (coe d_theory_1102 (coe v0))
-- Axiom.Set.Theoryⁱ._.map
d_map_1152 ::
  T_Theory'8305'_972 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map_1152 v0 v1 v2 = coe du_map_360 (coe d_theory_1102 (coe v0))
-- Axiom.Set.Theoryⁱ._.mapPartial
d_mapPartial_1154 ::
  T_Theory'8305'_972 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_mapPartial_1154 v0 v1 v2 v3
  = coe du_mapPartial_538 (coe d_theory_1102 (coe v0)) v3
-- Axiom.Set.Theoryⁱ._.maximal-unique
d_maximal'45'unique_1156 ::
  T_Theory'8305'_972 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_maximal'45'unique_1156 ~v0 = du_maximal'45'unique_1156
du_maximal'45'unique_1156 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_maximal'45'unique_1156 v0 v1 v2 v3 v4
  = coe du_maximal'45'unique_322 v3 v4
-- Axiom.Set.Theoryⁱ._.maximal-⊆
d_maximal'45''8838'_1158 ::
  T_Theory'8305'_972 ->
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_maximal'45''8838'_1158 ~v0 = du_maximal'45''8838'_1158
du_maximal'45''8838'_1158 ::
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_maximal'45''8838'_1158 v0 v1 v2 v3 v4 v5
  = coe du_maximal'45''8838'_318 v3 v4
-- Axiom.Set.Theoryⁱ._.partialToSet
d_partialToSet_1160 ::
  T_Theory'8305'_972 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_partialToSet_1160 v0 v1 v2 v3 v4 v5
  = coe du_partialToSet_434 (coe d_theory_1102 (coe v0)) v4 v5
-- Axiom.Set.Theoryⁱ._.replacement
d_replacement_1162 ::
  T_Theory'8305'_972 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_replacement_1162 v0
  = coe d_replacement_196 (coe d_theory_1102 (coe v0))
-- Axiom.Set.Theoryⁱ._.singleton
d_singleton_1164 :: T_Theory'8305'_972 -> () -> AgdaAny -> AgdaAny
d_singleton_1164 v0 v1 v2
  = coe du_singleton_410 (coe d_theory_1102 (coe v0)) v2
-- Axiom.Set.Theoryⁱ._.sp
d_sp_1166 :: T_Theory'8305'_972 -> T_SpecProperty_48
d_sp_1166 v0 = coe d_sp_150 (coe d_theory_1102 (coe v0))
-- Axiom.Set.Theoryⁱ._.sp-¬
d_sp'45''172'_1168 ::
  T_Theory'8305'_972 -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny
d_sp'45''172'_1168 v0
  = coe d_sp'45''172'_70 (coe d_sp_150 (coe d_theory_1102 (coe v0)))
-- Axiom.Set.Theoryⁱ._.sp-∘
d_sp'45''8728'_1170 ::
  T_Theory'8305'_972 ->
  () ->
  (AgdaAny -> ()) -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_sp'45''8728'_1170 v0
  = coe d_sp'45''8728'_68 (coe d_sp_150 (coe d_theory_1102 (coe v0)))
-- Axiom.Set.Theoryⁱ._.spec-∈
d_spec'45''8712'_1172 :: T_Theory'8305'_972 -> () -> ()
d_spec'45''8712'_1172 = erased
-- Axiom.Set.Theoryⁱ._.specProperty
d_specProperty_1174 ::
  T_Theory'8305'_972 -> () -> (AgdaAny -> ()) -> ()
d_specProperty_1174 = erased
-- Axiom.Set.Theoryⁱ._.specification
d_specification_1176 ::
  T_Theory'8305'_972 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_specification_1176 v0
  = coe d_specification_174 (coe d_theory_1102 (coe v0))
-- Axiom.Set.Theoryⁱ._.strictify
d_strictify_1178 ::
  T_Theory'8305'_972 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_strictify_1178 v0 v1 v2 v3 v4
  = coe du_strictify_340 (coe d_theory_1102 (coe v0)) v3 v4
-- Axiom.Set.Theoryⁱ._.strongly-finite
d_strongly'45'finite_1180 ::
  T_Theory'8305'_972 -> () -> AgdaAny -> ()
d_strongly'45'finite_1180 = erased
-- Axiom.Set.Theoryⁱ._.unions
d_unions_1182 ::
  T_Theory'8305'_972 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unions_1182 v0 = coe d_unions_184 (coe d_theory_1102 (coe v0))
-- Axiom.Set.Theoryⁱ._.weakly-finite
d_weakly'45'finite_1184 ::
  T_Theory'8305'_972 -> () -> AgdaAny -> ()
d_weakly'45'finite_1184 = erased
-- Axiom.Set.Theoryⁱ._.∅
d_'8709'_1186 :: T_Theory'8305'_972 -> () -> AgdaAny
d_'8709'_1186 v0 v1
  = coe du_'8709'_404 (coe d_theory_1102 (coe v0))
-- Axiom.Set.Theoryⁱ._.∅-strongly-finite
d_'8709''45'strongly'45'finite_1188 ::
  T_Theory'8305'_972 -> () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8709''45'strongly'45'finite_1188 v0 v1
  = coe
      du_'8709''45'strongly'45'finite_406 (coe d_theory_1102 (coe v0))
-- Axiom.Set.Theoryⁱ._.∈-concatMapˢ
d_'8712''45'concatMap'738'_1190 ::
  T_Theory'8305'_972 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'concatMap'738'_1190 v0 v1 v2 v3 v4 v5
  = coe
      du_'8712''45'concatMap'738'_482 (coe d_theory_1102 (coe v0)) v3 v4
      v5
-- Axiom.Set.Theoryⁱ._.∈-filter
d_'8712''45'filter_1192 ::
  T_Theory'8305'_972 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'filter_1192 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'filter_388 (coe d_theory_1102 (coe v0)) v3 v4 v5
-- Axiom.Set.Theoryⁱ._.∈-fromList
d_'8712''45'fromList_1194 ::
  T_Theory'8305'_972 ->
  () ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'fromList_1194 v0 v1 v2 v3
  = coe du_'8712''45'fromList_394 (coe d_theory_1102 (coe v0)) v2 v3
-- Axiom.Set.Theoryⁱ._.∈-map
d_'8712''45'map_1196 ::
  T_Theory'8305'_972 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'map_1196 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'map_368 (coe d_theory_1102 (coe v0)) v3 v4 v5
-- Axiom.Set.Theoryⁱ._.∈-mapPartial
d_'8712''45'mapPartial_1198 ::
  T_Theory'8305'_972 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'mapPartial_1198 v0 v1 v2 v3 v4 v5
  = coe
      du_'8712''45'mapPartial_548 (coe d_theory_1102 (coe v0)) v3 v4 v5
-- Axiom.Set.Theoryⁱ._.∈-map′
d_'8712''45'map'8242'_1200 ::
  T_Theory'8305'_972 ->
  () ->
  () ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8712''45'map'8242'_1200 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_'8712''45'map'8242'_374 (coe d_theory_1102 (coe v0)) v3 v4 v5 v6
-- Axiom.Set.Theoryⁱ._.∈-partialToSet
d_'8712''45'partialToSet_1202 ::
  T_Theory'8305'_972 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'partialToSet_1202 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8712''45'partialToSet_446 (coe d_theory_1102 (coe v0)) v5
-- Axiom.Set.Theoryⁱ._.∈-singleton
d_'8712''45'singleton_1204 ::
  T_Theory'8305'_972 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'singleton_1204 v0 v1 v2 v3
  = coe du_'8712''45'singleton_420 (coe d_theory_1102 (coe v0)) v2 v3
-- Axiom.Set.Theoryⁱ._.∈-unions
d_'8712''45'unions_1206 ::
  T_Theory'8305'_972 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'unions_1206 v0 v1 v2 v3
  = coe du_'8712''45'unions_402 (coe d_theory_1102 (coe v0)) v2 v3
-- Axiom.Set.Theoryⁱ._.∈-∪
d_'8712''45''8746'_1208 ::
  T_Theory'8305'_972 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8746'_1208 v0 v1 v2 v3 v4
  = coe du_'8712''45''8746'_650 (coe d_theory_1102 (coe v0)) v2 v3 v4
-- Axiom.Set.Theoryⁱ._.⊆-mapPartial
d_'8838''45'mapPartial_1210 ::
  T_Theory'8305'_972 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) -> Maybe AgdaAny -> AgdaAny -> AgdaAny
d_'8838''45'mapPartial_1210 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_'8838''45'mapPartial_566 (coe d_theory_1102 (coe v0)) v3 v4 v5
      v6
-- Axiom.Set.Theoryⁱ._.⊆-weakly-finite
d_'8838''45'weakly'45'finite_1212 ::
  T_Theory'8305'_972 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8838''45'weakly'45'finite_1212 ~v0
  = du_'8838''45'weakly'45'finite_1212
du_'8838''45'weakly'45'finite_1212 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8838''45'weakly'45'finite_1212 v0 v1 v2 v3 v4
  = coe du_'8838''45'weakly'45'finite_302 v3 v4
-- Axiom.Set.Theoryⁱ._.❴_❵
d_'10100'_'10101'_1214 ::
  T_Theory'8305'_972 -> () -> AgdaAny -> AgdaAny
d_'10100'_'10101'_1214 v0
  = coe du_'10100'_'10101'_414 (coe d_theory_1102 (coe v0))
-- Axiom.Set.Theoryⁱ._.Intersection._∩_
d__'8745'__1218 ::
  T_Theory'8305'_972 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__1218 v0 v1 v2 v3 v4
  = coe du__'8745'__666 (coe d_theory_1102 (coe v0)) v2 v3 v4
-- Axiom.Set.Theoryⁱ._.Intersection.disjoint'
d_disjoint''_1220 ::
  T_Theory'8305'_972 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_disjoint''_1220 = erased
-- Axiom.Set.Theoryⁱ._.Intersection.∈-∩
d_'8712''45''8745'_1222 ::
  T_Theory'8305'_972 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8745'_1222 v0 v1 v2 v3 v4 v5
  = coe
      du_'8712''45''8745'_674 (coe d_theory_1102 (coe v0)) v2 v3 v4 v5
-- Axiom.Set.Theoryⁱ.infinity
d_infinity_1228 ::
  T_Theory'8305'_972 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_infinity_1228 v0
  = case coe v0 of
      C_Theory'8305''46'constructor_101947 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᵈ
d_Theory'7496'_1230 = ()
data T_Theory'7496'_1230
  = C_Theory'7496''46'constructor_105429 T_Theory_82
                                         (() ->
                                          MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
                                          AgdaAny -> AgdaAny)
                                         (() ->
                                          MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
                                          AgdaAny ->
                                          AgdaAny ->
                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
                                         (() ->
                                          MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
                                          (AgdaAny -> ()) ->
                                          (AgdaAny ->
                                           MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
                                          AgdaAny ->
                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
                                         (() ->
                                          MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
                                          (AgdaAny -> ()) ->
                                          (AgdaAny ->
                                           MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
                                          AgdaAny ->
                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
-- Axiom.Set._._Preservesˢ_
d__Preserves'738'__1236 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738'__1236 = erased
-- Axiom.Set._._Preservesˢ₂_
d__Preserves'738''8322'__1238 ::
  T_Theory_82 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738''8322'__1238 = erased
-- Axiom.Set._._∈_
d__'8712'__1240 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8712'__1240 = erased
-- Axiom.Set._._∉_
d__'8713'__1242 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8713'__1242 = erased
-- Axiom.Set._._∪_
d__'8746'__1244 ::
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__1244 v0 v1 v2 v3 = coe du__'8746'__642 (coe v0) v2 v3
-- Axiom.Set._._≡ᵉ_
d__'8801''7497'__1246 ::
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'__1246 = erased
-- Axiom.Set._._≡ᵉ'_
d__'8801''7497'''__1248 ::
  T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'''__1248 = erased
-- Axiom.Set._._⊆_
d__'8838'__1250 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d__'8838'__1250 = erased
-- Axiom.Set._.All
d_All_1252 :: T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_All_1252 = erased
-- Axiom.Set._.Any
d_Any_1254 :: T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_Any_1254 = erased
-- Axiom.Set._.DecEq∧finite⇒strongly-finite
d_DecEq'8743'finite'8658'strongly'45'finite_1256 ::
  T_Theory_82 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_DecEq'8743'finite'8658'strongly'45'finite_1256 ~v0
  = du_DecEq'8743'finite'8658'strongly'45'finite_1256
du_DecEq'8743'finite'8658'strongly'45'finite_1256 ::
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_DecEq'8743'finite'8658'strongly'45'finite_1256 v0 v1 v2 v3
  = coe du_DecEq'8743'finite'8658'strongly'45'finite_280 v1 v3
-- Axiom.Set._.FinSet
d_FinSet_1258 :: T_Theory_82 -> () -> ()
d_FinSet_1258 = erased
-- Axiom.Set._.Set
d_Set_1260 :: T_Theory_82 -> () -> ()
d_Set_1260 = erased
-- Axiom.Set._.binary-unions
d_binary'45'unions_1262 ::
  T_Theory_82 ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_binary'45'unions_1262 v0 v1 v2 v3
  = coe du_binary'45'unions_606 (coe v0) v2 v3
-- Axiom.Set._.card
d_card_1264 ::
  T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_card_1264 ~v0 = du_card_1264
du_card_1264 ::
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
du_card_1264 v0 v1 = coe du_card_298 v1
-- Axiom.Set._.card-∅
d_card'45''8709'_1266 ::
  T_Theory_82 ->
  () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_card'45''8709'_1266 = erased
-- Axiom.Set._.concatMapˢ
d_concatMap'738'_1268 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_concatMap'738'_1268 v0 v1 v2 v3 v4
  = coe du_concatMap'738'_470 (coe v0) v3 v4
-- Axiom.Set._.disjoint
d_disjoint_1270 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny -> ()
d_disjoint_1270 = erased
-- Axiom.Set._.filter
d_filter_1272 ::
  T_Theory_82 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d_filter_1272 v0 v1 v2 = coe du_filter_382 (coe v0)
-- Axiom.Set._.finite
d_finite_1274 :: T_Theory_82 -> () -> AgdaAny -> ()
d_finite_1274 = erased
-- Axiom.Set._.fromList
d_fromList_1276 :: T_Theory_82 -> () -> [AgdaAny] -> AgdaAny
d_fromList_1276 v0 v1 v2 = coe du_fromList_390 (coe v0) v2
-- Axiom.Set._.isMaximal
d_isMaximal_1278 :: T_Theory_82 -> () -> AgdaAny -> ()
d_isMaximal_1278 = erased
-- Axiom.Set._.listing
d_listing_1280 ::
  T_Theory_82 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_listing_1280 v0 = coe d_listing_204 (coe v0)
-- Axiom.Set._.map
d_map_1282 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map_1282 v0 v1 v2 = coe du_map_360 (coe v0)
-- Axiom.Set._.mapPartial
d_mapPartial_1284 ::
  T_Theory_82 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_mapPartial_1284 v0 v1 v2 v3 = coe du_mapPartial_538 (coe v0) v3
-- Axiom.Set._.maximal-unique
d_maximal'45'unique_1286 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_maximal'45'unique_1286 ~v0 = du_maximal'45'unique_1286
du_maximal'45'unique_1286 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_maximal'45'unique_1286 v0 v1 v2 v3 v4
  = coe du_maximal'45'unique_322 v3 v4
-- Axiom.Set._.maximal-⊆
d_maximal'45''8838'_1288 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_maximal'45''8838'_1288 ~v0 = du_maximal'45''8838'_1288
du_maximal'45''8838'_1288 ::
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_maximal'45''8838'_1288 v0 v1 v2 v3 v4 v5
  = coe du_maximal'45''8838'_318 v3 v4
-- Axiom.Set._.partialToSet
d_partialToSet_1290 ::
  T_Theory_82 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_partialToSet_1290 v0 v1 v2 v3 v4 v5
  = coe du_partialToSet_434 (coe v0) v4 v5
-- Axiom.Set._.replacement
d_replacement_1292 ::
  T_Theory_82 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_replacement_1292 v0 = coe d_replacement_196 (coe v0)
-- Axiom.Set._.singleton
d_singleton_1294 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny
d_singleton_1294 v0 v1 v2 = coe du_singleton_410 (coe v0) v2
-- Axiom.Set._.sp
d_sp_1296 :: T_Theory_82 -> T_SpecProperty_48
d_sp_1296 v0 = coe d_sp_150 (coe v0)
-- Axiom.Set._.sp-¬
d_sp'45''172'_1298 ::
  T_Theory_82 -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny
d_sp'45''172'_1298 v0
  = coe d_sp'45''172'_70 (coe d_sp_150 (coe v0))
-- Axiom.Set._.sp-∘
d_sp'45''8728'_1300 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_sp'45''8728'_1300 v0
  = coe d_sp'45''8728'_68 (coe d_sp_150 (coe v0))
-- Axiom.Set._.spec-∈
d_spec'45''8712'_1302 :: T_Theory_82 -> () -> ()
d_spec'45''8712'_1302 = erased
-- Axiom.Set._.specProperty
d_specProperty_1304 :: T_Theory_82 -> () -> (AgdaAny -> ()) -> ()
d_specProperty_1304 = erased
-- Axiom.Set._.specification
d_specification_1306 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_specification_1306 v0 = coe d_specification_174 (coe v0)
-- Axiom.Set._.strictify
d_strictify_1308 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_strictify_1308 v0 v1 v2 v3 v4
  = coe du_strictify_340 (coe v0) v3 v4
-- Axiom.Set._.strongly-finite
d_strongly'45'finite_1310 :: T_Theory_82 -> () -> AgdaAny -> ()
d_strongly'45'finite_1310 = erased
-- Axiom.Set._.unions
d_unions_1312 ::
  T_Theory_82 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unions_1312 v0 = coe d_unions_184 (coe v0)
-- Axiom.Set._.weakly-finite
d_weakly'45'finite_1314 :: T_Theory_82 -> () -> AgdaAny -> ()
d_weakly'45'finite_1314 = erased
-- Axiom.Set._.∅
d_'8709'_1316 :: T_Theory_82 -> () -> AgdaAny
d_'8709'_1316 v0 v1 = coe du_'8709'_404 (coe v0)
-- Axiom.Set._.∅-strongly-finite
d_'8709''45'strongly'45'finite_1318 ::
  T_Theory_82 -> () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8709''45'strongly'45'finite_1318 v0 v1
  = coe du_'8709''45'strongly'45'finite_406 (coe v0)
-- Axiom.Set._.∈-concatMapˢ
d_'8712''45'concatMap'738'_1320 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'concatMap'738'_1320 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'concatMap'738'_482 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-filter
d_'8712''45'filter_1322 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'filter_1322 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'filter_388 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-fromList
d_'8712''45'fromList_1324 ::
  T_Theory_82 ->
  () ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'fromList_1324 v0 v1 v2 v3
  = coe du_'8712''45'fromList_394 (coe v0) v2 v3
-- Axiom.Set._.∈-map
d_'8712''45'map_1326 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'map_1326 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'map_368 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-mapPartial
d_'8712''45'mapPartial_1328 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'mapPartial_1328 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'mapPartial_548 (coe v0) v3 v4 v5
-- Axiom.Set._.∈-map′
d_'8712''45'map'8242'_1330 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8712''45'map'8242'_1330 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8712''45'map'8242'_374 (coe v0) v3 v4 v5 v6
-- Axiom.Set._.∈-partialToSet
d_'8712''45'partialToSet_1332 ::
  T_Theory_82 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'partialToSet_1332 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8712''45'partialToSet_446 (coe v0) v5
-- Axiom.Set._.∈-singleton
d_'8712''45'singleton_1334 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'singleton_1334 v0 v1 v2 v3
  = coe du_'8712''45'singleton_420 (coe v0) v2 v3
-- Axiom.Set._.∈-unions
d_'8712''45'unions_1336 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'unions_1336 v0 v1 v2 v3
  = coe du_'8712''45'unions_402 (coe v0) v2 v3
-- Axiom.Set._.∈-∪
d_'8712''45''8746'_1338 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8746'_1338 v0 v1 v2 v3 v4
  = coe du_'8712''45''8746'_650 (coe v0) v2 v3 v4
-- Axiom.Set._.⊆-mapPartial
d_'8838''45'mapPartial_1340 ::
  T_Theory_82 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) -> Maybe AgdaAny -> AgdaAny -> AgdaAny
d_'8838''45'mapPartial_1340 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8838''45'mapPartial_566 (coe v0) v3 v4 v5 v6
-- Axiom.Set._.⊆-weakly-finite
d_'8838''45'weakly'45'finite_1342 ::
  T_Theory_82 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8838''45'weakly'45'finite_1342 ~v0
  = du_'8838''45'weakly'45'finite_1342
du_'8838''45'weakly'45'finite_1342 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8838''45'weakly'45'finite_1342 v0 v1 v2 v3 v4
  = coe du_'8838''45'weakly'45'finite_302 v3 v4
-- Axiom.Set._.❴_❵
d_'10100'_'10101'_1344 :: T_Theory_82 -> () -> AgdaAny -> AgdaAny
d_'10100'_'10101'_1344 v0 = coe du_'10100'_'10101'_414 (coe v0)
-- Axiom.Set._.Intersection._∩_
d__'8745'__1348 ::
  T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__1348 v0 v1 v2 v3 v4
  = coe du__'8745'__666 (coe v0) v2 v3 v4
-- Axiom.Set._.Intersection.disjoint'
d_disjoint''_1350 ::
  T_Theory_82 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_disjoint''_1350 = erased
-- Axiom.Set._.Intersection.∈-∩
d_'8712''45''8745'_1352 ::
  T_Theory_82 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8745'_1352 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45''8745'_674 (coe v0) v2 v3 v4 v5
-- Axiom.Set.Theoryᵈ.th
d_th_1374 :: T_Theory'7496'_1230 -> T_Theory_82
d_th_1374 v0
  = case coe v0 of
      C_Theory'7496''46'constructor_105429 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᵈ._._Preservesˢ_
d__Preserves'738'__1378 ::
  T_Theory'7496'_1230 ->
  () -> () -> (AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738'__1378 = erased
-- Axiom.Set.Theoryᵈ._._Preservesˢ₂_
d__Preserves'738''8322'__1380 ::
  T_Theory'7496'_1230 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> (() -> AgdaAny -> ()) -> ()
d__Preserves'738''8322'__1380 = erased
-- Axiom.Set.Theoryᵈ._._∈_
d__'8712'__1382 ::
  T_Theory'7496'_1230 -> () -> AgdaAny -> AgdaAny -> ()
d__'8712'__1382 = erased
-- Axiom.Set.Theoryᵈ._._∉_
d__'8713'__1384 ::
  T_Theory'7496'_1230 -> () -> AgdaAny -> AgdaAny -> ()
d__'8713'__1384 = erased
-- Axiom.Set.Theoryᵈ._._∪_
d__'8746'__1386 ::
  T_Theory'7496'_1230 -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__1386 v0 v1 v2 v3
  = coe du__'8746'__642 (coe d_th_1374 (coe v0)) v2 v3
-- Axiom.Set.Theoryᵈ._._≡ᵉ_
d__'8801''7497'__1388 ::
  T_Theory'7496'_1230 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'__1388 = erased
-- Axiom.Set.Theoryᵈ._._≡ᵉ'_
d__'8801''7497'''__1390 ::
  T_Theory'7496'_1230 -> () -> AgdaAny -> AgdaAny -> ()
d__'8801''7497'''__1390 = erased
-- Axiom.Set.Theoryᵈ._._⊆_
d__'8838'__1392 ::
  T_Theory'7496'_1230 -> () -> AgdaAny -> AgdaAny -> ()
d__'8838'__1392 = erased
-- Axiom.Set.Theoryᵈ._.All
d_All_1394 ::
  T_Theory'7496'_1230 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_All_1394 = erased
-- Axiom.Set.Theoryᵈ._.Any
d_Any_1396 ::
  T_Theory'7496'_1230 -> () -> (AgdaAny -> ()) -> AgdaAny -> ()
d_Any_1396 = erased
-- Axiom.Set.Theoryᵈ._.DecEq∧finite⇒strongly-finite
d_DecEq'8743'finite'8658'strongly'45'finite_1398 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_DecEq'8743'finite'8658'strongly'45'finite_1398 ~v0
  = du_DecEq'8743'finite'8658'strongly'45'finite_1398
du_DecEq'8743'finite'8658'strongly'45'finite_1398 ::
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_DecEq'8743'finite'8658'strongly'45'finite_1398 v0 v1 v2 v3
  = coe du_DecEq'8743'finite'8658'strongly'45'finite_280 v1 v3
-- Axiom.Set.Theoryᵈ._.FinSet
d_FinSet_1400 :: T_Theory'7496'_1230 -> () -> ()
d_FinSet_1400 = erased
-- Axiom.Set.Theoryᵈ._.Set
d_Set_1402 :: T_Theory'7496'_1230 -> () -> ()
d_Set_1402 = erased
-- Axiom.Set.Theoryᵈ._.binary-unions
d_binary'45'unions_1404 ::
  T_Theory'7496'_1230 ->
  () -> AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_binary'45'unions_1404 v0 v1 v2 v3
  = coe du_binary'45'unions_606 (coe d_th_1374 (coe v0)) v2 v3
-- Axiom.Set.Theoryᵈ._.card
d_card_1406 ::
  T_Theory'7496'_1230 ->
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
d_card_1406 ~v0 = du_card_1406
du_card_1406 ::
  () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> Integer
du_card_1406 v0 v1 = coe du_card_298 v1
-- Axiom.Set.Theoryᵈ._.card-∅
d_card'45''8709'_1408 ::
  T_Theory'7496'_1230 ->
  () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_card'45''8709'_1408 = erased
-- Axiom.Set.Theoryᵈ._.concatMapˢ
d_concatMap'738'_1410 ::
  T_Theory'7496'_1230 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_concatMap'738'_1410 v0 v1 v2 v3 v4
  = coe du_concatMap'738'_470 (coe d_th_1374 (coe v0)) v3 v4
-- Axiom.Set.Theoryᵈ._.disjoint
d_disjoint_1412 ::
  T_Theory'7496'_1230 -> () -> AgdaAny -> AgdaAny -> ()
d_disjoint_1412 = erased
-- Axiom.Set.Theoryᵈ._.filter
d_filter_1414 ::
  T_Theory'7496'_1230 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> AgdaAny
d_filter_1414 v0 v1 v2 = coe du_filter_382 (coe d_th_1374 (coe v0))
-- Axiom.Set.Theoryᵈ._.finite
d_finite_1416 :: T_Theory'7496'_1230 -> () -> AgdaAny -> ()
d_finite_1416 = erased
-- Axiom.Set.Theoryᵈ._.fromList
d_fromList_1418 ::
  T_Theory'7496'_1230 -> () -> [AgdaAny] -> AgdaAny
d_fromList_1418 v0 v1 v2
  = coe du_fromList_390 (coe d_th_1374 (coe v0)) v2
-- Axiom.Set.Theoryᵈ._.isMaximal
d_isMaximal_1420 :: T_Theory'7496'_1230 -> () -> AgdaAny -> ()
d_isMaximal_1420 = erased
-- Axiom.Set.Theoryᵈ._.listing
d_listing_1422 ::
  T_Theory'7496'_1230 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_listing_1422 v0 = coe d_listing_204 (coe d_th_1374 (coe v0))
-- Axiom.Set.Theoryᵈ._.map
d_map_1424 ::
  T_Theory'7496'_1230 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_map_1424 v0 v1 v2 = coe du_map_360 (coe d_th_1374 (coe v0))
-- Axiom.Set.Theoryᵈ._.mapPartial
d_mapPartial_1426 ::
  T_Theory'7496'_1230 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_mapPartial_1426 v0 v1 v2 v3
  = coe du_mapPartial_538 (coe d_th_1374 (coe v0)) v3
-- Axiom.Set.Theoryᵈ._.maximal-unique
d_maximal'45'unique_1428 ::
  T_Theory'7496'_1230 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_maximal'45'unique_1428 ~v0 = du_maximal'45'unique_1428
du_maximal'45'unique_1428 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_maximal'45'unique_1428 v0 v1 v2 v3 v4
  = coe du_maximal'45'unique_322 v3 v4
-- Axiom.Set.Theoryᵈ._.maximal-⊆
d_maximal'45''8838'_1430 ::
  T_Theory'7496'_1230 ->
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_maximal'45''8838'_1430 ~v0 = du_maximal'45''8838'_1430
du_maximal'45''8838'_1430 ::
  () ->
  AgdaAny ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_maximal'45''8838'_1430 v0 v1 v2 v3 v4 v5
  = coe du_maximal'45''8838'_318 v3 v4
-- Axiom.Set.Theoryᵈ._.partialToSet
d_partialToSet_1432 ::
  T_Theory'7496'_1230 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_partialToSet_1432 v0 v1 v2 v3 v4 v5
  = coe du_partialToSet_434 (coe d_th_1374 (coe v0)) v4 v5
-- Axiom.Set.Theoryᵈ._.replacement
d_replacement_1434 ::
  T_Theory'7496'_1230 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_replacement_1434 v0
  = coe d_replacement_196 (coe d_th_1374 (coe v0))
-- Axiom.Set.Theoryᵈ._.singleton
d_singleton_1436 :: T_Theory'7496'_1230 -> () -> AgdaAny -> AgdaAny
d_singleton_1436 v0 v1 v2
  = coe du_singleton_410 (coe d_th_1374 (coe v0)) v2
-- Axiom.Set.Theoryᵈ._.sp
d_sp_1438 :: T_Theory'7496'_1230 -> T_SpecProperty_48
d_sp_1438 v0 = coe d_sp_150 (coe d_th_1374 (coe v0))
-- Axiom.Set.Theoryᵈ._.sp-¬
d_sp'45''172'_1440 ::
  T_Theory'7496'_1230 -> () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny
d_sp'45''172'_1440 v0
  = coe d_sp'45''172'_70 (coe d_sp_150 (coe d_th_1374 (coe v0)))
-- Axiom.Set.Theoryᵈ._.sp-∘
d_sp'45''8728'_1442 ::
  T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> ()) -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_sp'45''8728'_1442 v0
  = coe d_sp'45''8728'_68 (coe d_sp_150 (coe d_th_1374 (coe v0)))
-- Axiom.Set.Theoryᵈ._.spec-∈
d_spec'45''8712'_1444 :: T_Theory'7496'_1230 -> () -> ()
d_spec'45''8712'_1444 = erased
-- Axiom.Set.Theoryᵈ._.specProperty
d_specProperty_1446 ::
  T_Theory'7496'_1230 -> () -> (AgdaAny -> ()) -> ()
d_specProperty_1446 = erased
-- Axiom.Set.Theoryᵈ._.specification
d_specification_1448 ::
  T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_specification_1448 v0
  = coe d_specification_174 (coe d_th_1374 (coe v0))
-- Axiom.Set.Theoryᵈ._.strictify
d_strictify_1450 ::
  T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_strictify_1450 v0 v1 v2 v3 v4
  = coe du_strictify_340 (coe d_th_1374 (coe v0)) v3 v4
-- Axiom.Set.Theoryᵈ._.strongly-finite
d_strongly'45'finite_1452 ::
  T_Theory'7496'_1230 -> () -> AgdaAny -> ()
d_strongly'45'finite_1452 = erased
-- Axiom.Set.Theoryᵈ._.unions
d_unions_1454 ::
  T_Theory'7496'_1230 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unions_1454 v0 = coe d_unions_184 (coe d_th_1374 (coe v0))
-- Axiom.Set.Theoryᵈ._.weakly-finite
d_weakly'45'finite_1456 ::
  T_Theory'7496'_1230 -> () -> AgdaAny -> ()
d_weakly'45'finite_1456 = erased
-- Axiom.Set.Theoryᵈ._.∅
d_'8709'_1458 :: T_Theory'7496'_1230 -> () -> AgdaAny
d_'8709'_1458 v0 v1 = coe du_'8709'_404 (coe d_th_1374 (coe v0))
-- Axiom.Set.Theoryᵈ._.∅-strongly-finite
d_'8709''45'strongly'45'finite_1460 ::
  T_Theory'7496'_1230 -> () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8709''45'strongly'45'finite_1460 v0 v1
  = coe du_'8709''45'strongly'45'finite_406 (coe d_th_1374 (coe v0))
-- Axiom.Set.Theoryᵈ._.∈-concatMapˢ
d_'8712''45'concatMap'738'_1462 ::
  T_Theory'7496'_1230 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'concatMap'738'_1462 v0 v1 v2 v3 v4 v5
  = coe
      du_'8712''45'concatMap'738'_482 (coe d_th_1374 (coe v0)) v3 v4 v5
-- Axiom.Set.Theoryᵈ._.∈-filter
d_'8712''45'filter_1464 ::
  T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'filter_1464 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'filter_388 (coe d_th_1374 (coe v0)) v3 v4 v5
-- Axiom.Set.Theoryᵈ._.∈-fromList
d_'8712''45'fromList_1466 ::
  T_Theory'7496'_1230 ->
  () ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'fromList_1466 v0 v1 v2 v3
  = coe du_'8712''45'fromList_394 (coe d_th_1374 (coe v0)) v2 v3
-- Axiom.Set.Theoryᵈ._.∈-map
d_'8712''45'map_1468 ::
  T_Theory'7496'_1230 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'map_1468 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'map_368 (coe d_th_1374 (coe v0)) v3 v4 v5
-- Axiom.Set.Theoryᵈ._.∈-mapPartial
d_'8712''45'mapPartial_1470 ::
  T_Theory'7496'_1230 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'mapPartial_1470 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45'mapPartial_548 (coe d_th_1374 (coe v0)) v3 v4 v5
-- Axiom.Set.Theoryᵈ._.∈-map′
d_'8712''45'map'8242'_1472 ::
  T_Theory'7496'_1230 ->
  () ->
  () ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_'8712''45'map'8242'_1472 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_'8712''45'map'8242'_374 (coe d_th_1374 (coe v0)) v3 v4 v5 v6
-- Axiom.Set.Theoryᵈ._.∈-partialToSet
d_'8712''45'partialToSet_1474 ::
  T_Theory'7496'_1230 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'partialToSet_1474 v0 v1 v2 v3 v4 v5 v6
  = coe du_'8712''45'partialToSet_446 (coe d_th_1374 (coe v0)) v5
-- Axiom.Set.Theoryᵈ._.∈-singleton
d_'8712''45'singleton_1476 ::
  T_Theory'7496'_1230 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'singleton_1476 v0 v1 v2 v3
  = coe du_'8712''45'singleton_420 (coe d_th_1374 (coe v0)) v2 v3
-- Axiom.Set.Theoryᵈ._.∈-unions
d_'8712''45'unions_1478 ::
  T_Theory'7496'_1230 ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'unions_1478 v0 v1 v2 v3
  = coe du_'8712''45'unions_402 (coe d_th_1374 (coe v0)) v2 v3
-- Axiom.Set.Theoryᵈ._.∈-∪
d_'8712''45''8746'_1480 ::
  T_Theory'7496'_1230 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8746'_1480 v0 v1 v2 v3 v4
  = coe du_'8712''45''8746'_650 (coe d_th_1374 (coe v0)) v2 v3 v4
-- Axiom.Set.Theoryᵈ._.⊆-mapPartial
d_'8838''45'mapPartial_1482 ::
  T_Theory'7496'_1230 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> Maybe AgdaAny) -> Maybe AgdaAny -> AgdaAny -> AgdaAny
d_'8838''45'mapPartial_1482 v0 v1 v2 v3 v4 v5 v6
  = coe
      du_'8838''45'mapPartial_566 (coe d_th_1374 (coe v0)) v3 v4 v5 v6
-- Axiom.Set.Theoryᵈ._.⊆-weakly-finite
d_'8838''45'weakly'45'finite_1484 ::
  T_Theory'7496'_1230 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8838''45'weakly'45'finite_1484 ~v0
  = du_'8838''45'weakly'45'finite_1484
du_'8838''45'weakly'45'finite_1484 ::
  () ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8838''45'weakly'45'finite_1484 v0 v1 v2 v3 v4
  = coe du_'8838''45'weakly'45'finite_302 v3 v4
-- Axiom.Set.Theoryᵈ._.❴_❵
d_'10100'_'10101'_1486 ::
  T_Theory'7496'_1230 -> () -> AgdaAny -> AgdaAny
d_'10100'_'10101'_1486 v0
  = coe du_'10100'_'10101'_414 (coe d_th_1374 (coe v0))
-- Axiom.Set.Theoryᵈ._.Intersection._∩_
d__'8745'__1490 ::
  T_Theory'7496'_1230 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'8745'__1490 v0 v1 v2 v3 v4
  = coe du__'8745'__666 (coe d_th_1374 (coe v0)) v2 v3 v4
-- Axiom.Set.Theoryᵈ._.Intersection.disjoint'
d_disjoint''_1492 ::
  T_Theory'7496'_1230 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> ()
d_disjoint''_1492 = erased
-- Axiom.Set.Theoryᵈ._.Intersection.∈-∩
d_'8712''45''8745'_1494 ::
  T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45''8745'_1494 v0 v1 v2 v3 v4 v5
  = coe du_'8712''45''8745'_674 (coe d_th_1374 (coe v0)) v2 v3 v4 v5
-- Axiom.Set.Theoryᵈ.∈-sp
d_'8712''45'sp_1496 ::
  T_Theory'7496'_1230 ->
  () -> MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> AgdaAny -> AgdaAny
d_'8712''45'sp_1496 v0
  = case coe v0 of
      C_Theory'7496''46'constructor_105429 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᵈ._∈?_
d__'8712''63'__1498 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8712''63'__1498 v0
  = case coe v0 of
      C_Theory'7496''46'constructor_105429 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᵈ.all?
d_all'63'_1506 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_all'63'_1506 v0
  = case coe v0 of
      C_Theory'7496''46'constructor_105429 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᵈ.any?
d_any'63'_1514 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_any'63'_1514 v0
  = case coe v0 of
      C_Theory'7496''46'constructor_105429 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᵈ._∈ᵇ_
d__'8712''7495'__1516 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> Bool
d__'8712''7495'__1516 v0 ~v1 v2 v3 v4
  = du__'8712''7495'__1516 v0 v2 v3 v4
du__'8712''7495'__1516 ::
  T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> Bool
du__'8712''7495'__1516 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
      erased (coe d__'8712''63'__1498 v0 erased v1 v2 v3)
-- Axiom.Set.Theoryᵈ.allᵇ
d_all'7495'_1528 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny -> Bool
d_all'7495'_1528 v0 ~v1 v2 ~v3 v4 v5
  = du_all'7495'_1528 v0 v2 v4 v5
du_all'7495'_1528 ::
  T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny -> Bool
du_all'7495'_1528 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
      erased (coe d_all'63'_1506 v0 erased v1 erased v2 v3)
-- Axiom.Set.Theoryᵈ.anyᵇ
d_any'7495'_1540 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny -> Bool
d_any'7495'_1540 v0 ~v1 v2 ~v3 v4 v5
  = du_any'7495'_1540 v0 v2 v4 v5
du_any'7495'_1540 ::
  T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny -> Bool
du_any'7495'_1540 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
      erased (coe d_any'63'_1514 v0 erased v1 erased v2 v3)
-- Axiom.Set.Theoryᵈ.incl-set'
d_incl'45'set''_1550 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_incl'45'set''_1550 v0 ~v1 v2 v3 v4
  = du_incl'45'set''_1550 v0 v2 v3 v4
du_incl'45'set''_1550 ::
  T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_incl'45'set''_1550 v0 v1 v2 v3
  = let v4 = coe d__'8712''63'__1498 v0 erased v1 v3 v2 in
    case coe v4 of
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v5 v6
        -> if coe v5
             then case coe v6 of
                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v7
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                           (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v7))
                    _ -> MAlonzo.RTE.mazUnreachableError
             else coe
                    seq (coe v6) (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᵈ.incl-set
d_incl'45'set_1576 ::
  T_Theory'7496'_1230 ->
  () -> MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> AgdaAny -> AgdaAny
d_incl'45'set_1576 v0 ~v1 v2 v3 = du_incl'45'set_1576 v0 v2 v3
du_incl'45'set_1576 ::
  T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> AgdaAny -> AgdaAny
du_incl'45'set_1576 v0 v1 v2
  = coe
      du_mapPartial_538 (d_th_1374 (coe v0))
      (coe du_incl'45'set''_1550 (coe v0) (coe v1) (coe v2)) v2
-- Axiom.Set.Theoryᵈ.incl-set-proj₁⊆
d_incl'45'set'45'proj'8321''8838'_1584 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_incl'45'set'45'proj'8321''8838'_1584 v0 ~v1 v2 v3 v4 v5
  = du_incl'45'set'45'proj'8321''8838'_1584 v0 v2 v3 v4 v5
du_incl'45'set'45'proj'8321''8838'_1584 ::
  T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_incl'45'set'45'proj'8321''8838'_1584 v0 v1 v2 v3 v4
  = let v5
          = coe
              MAlonzo.Code.Function.Bundles.d_from_940
              (coe
                 du_'8712''45'map_368 (coe d_th_1374 (coe v0))
                 (coe du_incl'45'set_1576 (coe v0) (coe v1) (coe v2))
                 (coe (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)))
                 (coe v3))
              v4 in
    case coe v5 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
        -> case coe v6 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v8 v9
               -> coe seq (coe v7) (coe v9)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᵈ.incl-set-proj₁⊇
d_incl'45'set'45'proj'8321''8839'_1600 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_incl'45'set'45'proj'8321''8839'_1600 v0 ~v1 v2 v3 v4 v5
  = du_incl'45'set'45'proj'8321''8839'_1600 v0 v2 v3 v4 v5
du_incl'45'set'45'proj'8321''8839'_1600 ::
  T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_incl'45'set'45'proj'8321''8839'_1600 v0 v1 v2 v3 v4
  = let v5 = coe d__'8712''63'__1498 v0 erased v1 v3 v2 in
    case coe v5 of
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v6 v7
        -> if coe v6
             then case coe v7 of
                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v8
                      -> coe
                           MAlonzo.Code.Function.Bundles.d_to_938
                           (coe
                              du_'8712''45'map_368 (coe d_th_1374 (coe v0))
                              (coe
                                 du_mapPartial_538 (d_th_1374 (coe v0))
                                 (coe du_incl'45'set''_1550 (coe v0) (coe v1) (coe v2)) v2)
                              (coe (\ v9 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v9)))
                              (coe v3))
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                              (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v8))
                              (coe
                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased
                                 (coe
                                    MAlonzo.Code.Function.Bundles.d_to_938
                                    (coe
                                       du_'8712''45'mapPartial_548 (coe d_th_1374 (coe v0)) (coe v2)
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                                          (coe v8))
                                       (coe du_incl'45'set''_1550 (coe v0) (coe v1) (coe v2)))
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v4)
                                          erased)))))
                    _ -> MAlonzo.RTE.mazUnreachableError
             else coe
                    seq (coe v7)
                    (coe
                       MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Theoryᵈ._.helper
d_helper_1636 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.PropositionalEquality.T_Reveal_'183'_is__86 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_helper_1636 = erased
-- Axiom.Set.Theoryᵈ.incl-set-proj₁
d_incl'45'set'45'proj'8321'_1650 ::
  T_Theory'7496'_1230 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_incl'45'set'45'proj'8321'_1650 v0 ~v1 v2 v3
  = du_incl'45'set'45'proj'8321'_1650 v0 v2 v3
du_incl'45'set'45'proj'8321'_1650 ::
  T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_incl'45'set'45'proj'8321'_1650 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe
         du_incl'45'set'45'proj'8321''8838'_1584 (coe v0) (coe v1) (coe v2))
      (coe
         du_incl'45'set'45'proj'8321''8839'_1600 (coe v0) (coe v1) (coe v2))
