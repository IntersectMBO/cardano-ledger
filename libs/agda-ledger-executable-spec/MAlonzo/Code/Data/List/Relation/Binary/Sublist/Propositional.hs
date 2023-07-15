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

module MAlonzo.Code.Data.List.Relation.Binary.Sublist.Propositional where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous
import qualified MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core
import qualified MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Properties
import qualified MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Data.List.Relation.Binary.Sublist.Propositional._._∷ʳ₁_
d__'8759''691''8321'__22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232
d__'8759''691''8321'__22 ~v0 ~v1 = du__'8759''691''8321'__22
du__'8759''691''8321'__22 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232
du__'8759''691''8321'__22 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.du__'8759''691''8321'__268
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      v5 v6
-- Data.List.Relation.Binary.Sublist.Propositional._._∷ʳ₂_
d__'8759''691''8322'__24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232
d__'8759''691''8322'__24 ~v0 ~v1 = du__'8759''691''8322'__24
du__'8759''691''8322'__24 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232
du__'8759''691''8322'__24 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.du__'8759''691''8322'__286
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      v5 v6
-- Data.List.Relation.Binary.Sublist.Propositional._._∷ᵣ-ub_
d__'8759''7523''45'ub__30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388
d__'8759''7523''45'ub__30 ~v0 ~v1 = du__'8759''7523''45'ub__30
du__'8759''7523''45'ub__30 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388
du__'8759''7523''45'ub__30 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.du__'8759''7523''45'ub__470
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      v6 v7 v8
-- Data.List.Relation.Binary.Sublist.Propositional._._∷ₗ-ub_
d__'8759''8343''45'ub__36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388
d__'8759''8343''45'ub__36 ~v0 ~v1 = du__'8759''8343''45'ub__36
du__'8759''8343''45'ub__36 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388
du__'8759''8343''45'ub__36 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.du__'8759''8343''45'ub__448
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      v6 v7 v8
-- Sublist
d_Sublist_39 a0 a1 a2 a3 a4 a5 a6 = ()
-- Data.List.Relation.Binary.Sublist.Propositional._._⊂_
d__'8834'__42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> ()
d__'8834'__42 = erased
-- Data.List.Relation.Binary.Sublist.Propositional._._⊃_
d__'8835'__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> ()
d__'8835'__44 = erased
-- Data.List.Relation.Binary.Sublist.Propositional._._⊄_
d__'8836'__46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> ()
d__'8836'__46 = erased
-- Data.List.Relation.Binary.Sublist.Propositional._._⊅_
d__'8837'__48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> ()
d__'8837'__48 = erased
-- Data.List.Relation.Binary.Sublist.Propositional._._⊆_
d__'8838'__50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> ()
d__'8838'__50 = erased
-- Data.List.Relation.Binary.Sublist.Propositional._._⊇_
d__'8839'__52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> ()
d__'8839'__52 = erased
-- Data.List.Relation.Binary.Sublist.Propositional._._⊈_
d__'8840'__54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> ()
d__'8840'__54 = erased
-- Data.List.Relation.Binary.Sublist.Propositional._._⊉_
d__'8841'__56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> ()
d__'8841'__56 = erased
-- Data.List.Relation.Binary.Sublist.Propositional._.Disjoint
d_Disjoint_58 a0 a1 a2 a3 a4 a5 a6 = ()
-- Data.List.Relation.Binary.Sublist.Propositional._.DisjointUnion
d_DisjointUnion_60 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
-- Data.List.Relation.Binary.Sublist.Propositional._.RawPushout
d_RawPushout_62 a0 a1 a2 a3 a4 a5 a6 = ()
-- Data.List.Relation.Binary.Sublist.Propositional._.UpperBound
d_UpperBound_64 a0 a1 a2 a3 a4 a5 a6 = ()
-- Data.List.Relation.Binary.Sublist.Propositional._.fromAny
d_fromAny_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_fromAny_72 ~v0 ~v1 = du_fromAny_72
du_fromAny_72 ::
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
du_fromAny_72 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.du_fromAny_74
      v1 v2
-- Data.List.Relation.Binary.Sublist.Propositional._.map
d_map_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_map_74 ~v0 ~v1 = du_map_74
du_map_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
du_map_74 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.du_map_32
      v2 v3 v4 v5
-- Data.List.Relation.Binary.Sublist.Propositional._.minimum
d_minimum_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_minimum_76 ~v0 ~v1 = du_minimum_76
du_minimum_76 ::
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
du_minimum_76
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.du_minimum_48
-- Data.List.Relation.Binary.Sublist.Propositional._.toAny
d_toAny_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
d_toAny_78 ~v0 ~v1 = du_toAny_78
du_toAny_78 ::
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
du_toAny_78 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.du_toAny_60
      v2 v3
-- Data.List.Relation.Binary.Sublist.Propositional._.∷-rpo
d_'8759''45'rpo_80 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232
d_'8759''45'rpo_80 ~v0 ~v1 = du_'8759''45'rpo_80
du_'8759''45'rpo_80 ::
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232
du_'8759''45'rpo_80 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.du_'8759''45'rpo_312
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      v0 v1 v2 v8 v9 v10
-- Data.List.Relation.Binary.Sublist.Propositional._.∷ₙ-ub
d_'8759''8345''45'ub_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388
d_'8759''8345''45'ub_82 ~v0 ~v1 = du_'8759''8345''45'ub_82
du_'8759''8345''45'ub_82 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388
du_'8759''8345''45'ub_82 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.du_'8759''8345''45'ub_428
      v6
-- Data.List.Relation.Binary.Sublist.Propositional._.⊆-disjoint-union
d_'8838''45'disjoint'45'union_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.T_Disjoint_130 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388
d_'8838''45'disjoint'45'union_84 ~v0 ~v1
  = du_'8838''45'disjoint'45'union_84
du_'8838''45'disjoint'45'union_84 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.T_Disjoint_130 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388
du_'8838''45'disjoint'45'union_84
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.du_'8838''45'disjoint'45'union_486
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Data.List.Relation.Binary.Sublist.Propositional._.⊆-joinˡ
d_'8838''45'join'737'_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8838''45'join'737'_86 ~v0 ~v1 = du_'8838''45'join'737'_86
du_'8838''45'join'737'_86 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8838''45'join'737'_86
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.du_'8838''45'join'737'_366
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Data.List.Relation.Binary.Sublist.Propositional._.⊆-pushoutˡ
d_'8838''45'pushout'737'_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232
d_'8838''45'pushout'737'_88 ~v0 ~v1 = du_'8838''45'pushout'737'_88
du_'8838''45'pushout'737'_88 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232
du_'8838''45'pushout'737'_88
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.du_'8838''45'pushout'737'_330
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Data.List.Relation.Binary.Sublist.Propositional._.refl
d_refl_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_refl_90 ~v0 ~v1 = du_refl_90
du_refl_90 ::
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
du_refl_90
  = let v0
          = coe
              MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402 in
    coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Properties.du_refl_1318
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
-- Data.List.Relation.Binary.Sublist.Propositional._.trans
d_trans_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_trans_92 ~v0 ~v1 = du_trans_92
du_trans_92 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
du_trans_92
  = let v0
          = coe
              MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402 in
    coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Properties.du_trans_1348
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_38
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
-- Data.List.Relation.Binary.Sublist.Propositional._.RawPushout.leg₁
d_leg'8321'_116 ::
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_leg'8321'_116 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.d_leg'8321'_252
      (coe v0)
-- Data.List.Relation.Binary.Sublist.Propositional._.RawPushout.leg₂
d_leg'8322'_118 ::
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_leg'8322'_118 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.d_leg'8322'_254
      (coe v0)
-- Data.List.Relation.Binary.Sublist.Propositional._.RawPushout.upperBound
d_upperBound_120 ::
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_RawPushout_232 ->
  [AgdaAny]
d_upperBound_120 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.d_upperBound_250
      (coe v0)
-- Data.List.Relation.Binary.Sublist.Propositional._.UpperBound.inj₁
d_inj'8321'_124 ::
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_inj'8321'_124 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.d_inj'8321'_412
      (coe v0)
-- Data.List.Relation.Binary.Sublist.Propositional._.UpperBound.inj₂
d_inj'8322'_126 ::
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_inj'8322'_126 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.d_inj'8322'_414
      (coe v0)
-- Data.List.Relation.Binary.Sublist.Propositional._.UpperBound.sub
d_sub_128 ::
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_sub_128 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.d_sub_410
      (coe v0)
-- Data.List.Relation.Binary.Sublist.Propositional._.UpperBound.theUpperBound
d_theUpperBound_130 ::
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.T_UpperBound_388 ->
  [AgdaAny]
d_theUpperBound_130 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Setoid.d_theUpperBound_408
      (coe v0)
-- Data.List.Relation.Binary.Sublist.Propositional._.lookup
d_lookup_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
d_lookup_144 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_lookup_144 v4 v5
du_lookup_144 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34
du_lookup_144 v0 v1
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.du_lookup_98
      (coe (\ v2 v3 v4 v5 -> v5)) (coe v0) (coe v1)
-- Data.List.Relation.Binary.Sublist.Propositional.⊆-reflexive
d_'8838''45'reflexive_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_'8838''45'reflexive_146 ~v0 ~v1 v2 ~v3 ~v4
  = du_'8838''45'reflexive_146 v2
du_'8838''45'reflexive_146 ::
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
du_'8838''45'reflexive_146 v0
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Properties.du_refl_1318
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396))
      (coe v0)
-- Data.List.Relation.Binary.Sublist.Propositional.⊆-antisym
d_'8838''45'antisym_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8838''45'antisym_148 = erased
-- Data.List.Relation.Binary.Sublist.Propositional.⊆-isPreorder
d_'8838''45'isPreorder_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8838''45'isPreorder_154 ~v0 ~v1 = du_'8838''45'isPreorder_154
du_'8838''45'isPreorder_154 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_'8838''45'isPreorder_154
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 v2 -> coe du_'8838''45'reflexive_146 v0)
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Properties.du_trans_1348
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_trans_38
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)))
-- Data.List.Relation.Binary.Sublist.Propositional.⊆-isPartialOrder
d_'8838''45'isPartialOrder_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8838''45'isPartialOrder_156 ~v0 ~v1
  = du_'8838''45'isPartialOrder_156
du_'8838''45'isPartialOrder_156 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
du_'8838''45'isPartialOrder_156
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe du_'8838''45'isPreorder_154) erased
-- Data.List.Relation.Binary.Sublist.Propositional.⊆-preorder
d_'8838''45'preorder_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8838''45'preorder_158 ~v0 ~v1 = du_'8838''45'preorder_158
du_'8838''45'preorder_158 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_'8838''45'preorder_158
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe du_'8838''45'isPreorder_154)
-- Data.List.Relation.Binary.Sublist.Propositional.⊆-poset
d_'8838''45'poset_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8838''45'poset_160 ~v0 ~v1 = du_'8838''45'poset_160
du_'8838''45'poset_160 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
du_'8838''45'poset_160
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      (coe du_'8838''45'isPartialOrder_156)
-- Data.List.Relation.Binary.Sublist.Propositional.Separation
d_Separation_172 a0 a1 a2 a3 a4 a5 a6 = ()
data T_Separation_172
  = C_Separation'46'constructor_4407 [AgdaAny]
                                     MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
                                     MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
                                     MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.T_Disjoint_130
-- Data.List.Relation.Binary.Sublist.Propositional.Separation.inflation
d_inflation_196 :: T_Separation_172 -> [AgdaAny]
d_inflation_196 v0
  = case coe v0 of
      C_Separation'46'constructor_4407 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Sublist.Propositional.Separation.separator₁
d_separator'8321'_198 ::
  T_Separation_172 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_separator'8321'_198 v0
  = case coe v0 of
      C_Separation'46'constructor_4407 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Sublist.Propositional.Separation.separator₂
d_separator'8322'_200 ::
  T_Separation_172 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_separator'8322'_200 v0
  = case coe v0 of
      C_Separation'46'constructor_4407 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Sublist.Propositional.Separation.separated₁
d_separated'8321'_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  T_Separation_172 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_separated'8321'_202 ~v0 ~v1 v2 ~v3 v4 v5 ~v6 v7
  = du_separated'8321'_202 v2 v4 v5 v7
du_separated'8321'_202 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  T_Separation_172 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
du_separated'8321'_202 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402 in
    coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Properties.du_trans_1348
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_38
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4)))
      (coe v0) (coe v1) (coe d_inflation_196 (coe v3)) (coe v2)
      (coe d_separator'8321'_198 (coe v3))
-- Data.List.Relation.Binary.Sublist.Propositional.Separation.separated₂
d_separated'8322'_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  T_Separation_172 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
d_separated'8322'_204 ~v0 ~v1 ~v2 v3 v4 ~v5 v6 v7
  = du_separated'8322'_204 v3 v4 v6 v7
du_separated'8322'_204 ::
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  T_Separation_172 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26
du_separated'8322'_204 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402 in
    coe
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Properties.du_trans_1348
      (coe
         MAlonzo.Code.Relation.Binary.Structures.d_trans_38
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v4)))
      (coe v0) (coe v1) (coe d_inflation_196 (coe v3)) (coe v2)
      (coe d_separator'8322'_200 (coe v3))
-- Data.List.Relation.Binary.Sublist.Propositional.Separation.disjoint
d_disjoint_206 ::
  T_Separation_172 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.T_Disjoint_130
d_disjoint_206 v0
  = case coe v0 of
      C_Separation'46'constructor_4407 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Sublist.Propositional.[]-Sep
d_'91''93''45'Sep_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> T_Separation_172
d_'91''93''45'Sep_208 ~v0 ~v1 = du_'91''93''45'Sep_208
du_'91''93''45'Sep_208 :: T_Separation_172
du_'91''93''45'Sep_208
  = coe
      C_Separation'46'constructor_4407
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C_'91''93'_28)
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C_'91''93'_28)
      (coe
         MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.C_'91''93'_132)
-- Data.List.Relation.Binary.Sublist.Propositional._∷ₙ-Sep_
d__'8759''8345''45'Sep__222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny -> T_Separation_172 -> T_Separation_172
d__'8759''8345''45'Sep__222 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8
  = du__'8759''8345''45'Sep__222 v7 v8
du__'8759''8345''45'Sep__222 ::
  AgdaAny -> T_Separation_172 -> T_Separation_172
du__'8759''8345''45'Sep__222 v0 v1
  = case coe v1 of
      C_Separation'46'constructor_4407 v2 v3 v4 v5
        -> coe
             C_Separation'46'constructor_4407
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v2))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46
                erased v3)
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46
                erased v4)
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.C__'8759''8345'__146
                v5)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Sublist.Propositional._∷ₗ-Sep_
d__'8759''8343''45'Sep__248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T_Separation_172 -> T_Separation_172
d__'8759''8343''45'Sep__248 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 ~v8 ~v9
                            v10
  = du__'8759''8343''45'Sep__248 v7 v10
du__'8759''8343''45'Sep__248 ::
  AgdaAny -> T_Separation_172 -> T_Separation_172
du__'8759''8343''45'Sep__248 v0 v1
  = case coe v1 of
      C_Separation'46'constructor_4407 v2 v3 v4 v5
        -> coe
             C_Separation'46'constructor_4407
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v2))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46
                erased v3)
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46
                erased v4)
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.C__'8759''8343'__164
                v5)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Sublist.Propositional._∷ᵣ-Sep_
d__'8759''7523''45'Sep__272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T_Separation_172 -> T_Separation_172
d__'8759''7523''45'Sep__272 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 ~v8 ~v9
                            v10
  = du__'8759''7523''45'Sep__272 v7 v10
du__'8759''7523''45'Sep__272 ::
  AgdaAny -> T_Separation_172 -> T_Separation_172
du__'8759''7523''45'Sep__272 v0 v1
  = case coe v1 of
      C_Separation'46'constructor_4407 v2 v3 v4 v5
        -> coe
             C_Separation'46'constructor_4407
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v2))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46
                erased v3)
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46
                erased v4)
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.C__'8759''7523'__182
                v5)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Sublist.Propositional.∷-Sepˡ
d_'8759''45'Sep'737'_300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T_Separation_172 -> T_Separation_172
d_'8759''45'Sep'737'_300 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 ~v8 ~v9
                         ~v10 ~v11 v12
  = du_'8759''45'Sep'737'_300 v7 v12
du_'8759''45'Sep'737'_300 ::
  AgdaAny -> T_Separation_172 -> T_Separation_172
du_'8759''45'Sep'737'_300 v0 v1
  = case coe v1 of
      C_Separation'46'constructor_4407 v2 v3 v4 v5
        -> coe
             C_Separation'46'constructor_4407
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0)
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v0) (coe v2)))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759''691'__36
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46
                   erased v3))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46
                erased
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759''691'__36
                   v4))
             (coe
                MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.C__'8759''7523'__182
                (coe
                   MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.C__'8759''8343'__164
                   v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Relation.Binary.Sublist.Propositional.separateˡ
d_separate'737'_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  T_Separation_172
d_separate'737'_318 ~v0 ~v1 v2 v3 v4 v5 v6
  = du_separate'737'_318 v2 v3 v4 v5 v6
du_separate'737'_318 ::
  [AgdaAny] ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.T_Sublist_26 ->
  T_Separation_172
du_separate'737'_318 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C_'91''93'_28
        -> coe seq (coe v4) (coe du_'91''93''45'Sep_208)
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759''691'__36 v8
        -> case coe v2 of
             (:) v9 v10
               -> case coe v4 of
                    MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759''691'__36 v14
                      -> coe
                           du__'8759''8345''45'Sep__222 (coe v9)
                           (coe
                              du_separate'737'_318 (coe v0) (coe v1) (coe v10) (coe v8)
                              (coe v14))
                    MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46 v15 v16
                      -> case coe v1 of
                           (:) v17 v18
                             -> coe
                                  du__'8759''7523''45'Sep__272 (coe v17)
                                  (coe
                                     du_separate'737'_318 (coe v0) (coe v18) (coe v10) (coe v8)
                                     (coe v16))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46 v9 v10
        -> case coe v0 of
             (:) v11 v12
               -> case coe v2 of
                    (:) v13 v14
                      -> case coe v4 of
                           MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759''691'__36 v18
                             -> coe
                                  du__'8759''8343''45'Sep__248 (coe v11)
                                  (coe
                                     du_separate'737'_318 (coe v12) (coe v1) (coe v14) (coe v10)
                                     (coe v18))
                           MAlonzo.Code.Data.List.Relation.Binary.Sublist.Heterogeneous.Core.C__'8759'__46 v19 v20
                             -> case coe v1 of
                                  (:) v21 v22
                                    -> coe
                                         du_'8759''45'Sep'737'_300 (coe v11)
                                         (coe
                                            du_separate'737'_318 (coe v12) (coe v22) (coe v14)
                                            (coe v10) (coe v20))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
