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

module MAlonzo.Code.Relation.Binary.Structures where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Consequences
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Relation.Binary.Structures.IsPartialEquivalence
d_IsPartialEquivalence_16 a0 a1 a2 a3 = ()
data T_IsPartialEquivalence_16
  = C_IsPartialEquivalence'46'constructor_273 (AgdaAny ->
                                               AgdaAny -> AgdaAny -> AgdaAny)
                                              (AgdaAny ->
                                               AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Relation.Binary.Structures.IsPartialEquivalence.sym
d_sym_22 ::
  T_IsPartialEquivalence_16 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_22 v0
  = case coe v0 of
      C_IsPartialEquivalence'46'constructor_273 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsPartialEquivalence.trans
d_trans_24 ::
  T_IsPartialEquivalence_16 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_24 v0
  = case coe v0 of
      C_IsPartialEquivalence'46'constructor_273 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsEquivalence
d_IsEquivalence_26 a0 a1 a2 a3 = ()
data T_IsEquivalence_26
  = C_IsEquivalence'46'constructor_743 (AgdaAny -> AgdaAny)
                                       (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                       (AgdaAny ->
                                        AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Relation.Binary.Structures.IsEquivalence.refl
d_refl_34 :: T_IsEquivalence_26 -> AgdaAny -> AgdaAny
d_refl_34 v0
  = case coe v0 of
      C_IsEquivalence'46'constructor_743 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsEquivalence.sym
d_sym_36 ::
  T_IsEquivalence_26 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_36 v0
  = case coe v0 of
      C_IsEquivalence'46'constructor_743 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsEquivalence.trans
d_trans_38 ::
  T_IsEquivalence_26 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_38 v0
  = case coe v0 of
      C_IsEquivalence'46'constructor_743 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsEquivalence.reflexive
d_reflexive_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsEquivalence_26 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_40 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 ~v7
  = du_reflexive_40 v4 v5
du_reflexive_40 :: T_IsEquivalence_26 -> AgdaAny -> AgdaAny
du_reflexive_40 v0 v1 = coe d_refl_34 v0 v1
-- Relation.Binary.Structures.IsEquivalence.isPartialEquivalence
d_isPartialEquivalence_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsEquivalence_26 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_42 ~v0 ~v1 ~v2 ~v3 v4
  = du_isPartialEquivalence_42 v4
du_isPartialEquivalence_42 ::
  T_IsEquivalence_26 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_42 v0
  = coe
      C_IsPartialEquivalence'46'constructor_273 (coe d_sym_36 (coe v0))
      (coe d_trans_38 (coe v0))
-- Relation.Binary.Structures.IsDecEquivalence
d_IsDecEquivalence_44 a0 a1 a2 a3 = ()
data T_IsDecEquivalence_44
  = C_IsDecEquivalence'46'constructor_2293 T_IsEquivalence_26
                                           (AgdaAny ->
                                            AgdaAny ->
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
-- Relation.Binary.Structures.IsDecEquivalence.isEquivalence
d_isEquivalence_50 :: T_IsDecEquivalence_44 -> T_IsEquivalence_26
d_isEquivalence_50 v0
  = case coe v0 of
      C_IsDecEquivalence'46'constructor_2293 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecEquivalence._≟_
d__'8799'__52 ::
  T_IsDecEquivalence_44 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__52 v0
  = case coe v0 of
      C_IsDecEquivalence'46'constructor_2293 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecEquivalence._.isPartialEquivalence
d_isPartialEquivalence_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecEquivalence_44 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_56 ~v0 ~v1 ~v2 ~v3 v4
  = du_isPartialEquivalence_56 v4
du_isPartialEquivalence_56 ::
  T_IsDecEquivalence_44 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_56 v0
  = coe du_isPartialEquivalence_42 (coe d_isEquivalence_50 (coe v0))
-- Relation.Binary.Structures.IsDecEquivalence._.refl
d_refl_58 :: T_IsDecEquivalence_44 -> AgdaAny -> AgdaAny
d_refl_58 v0 = coe d_refl_34 (coe d_isEquivalence_50 (coe v0))
-- Relation.Binary.Structures.IsDecEquivalence._.reflexive
d_reflexive_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecEquivalence_44 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_60 ~v0 ~v1 ~v2 ~v3 v4 = du_reflexive_60 v4
du_reflexive_60 ::
  T_IsDecEquivalence_44 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_60 v0 v1 v2 v3
  = coe du_reflexive_40 (coe d_isEquivalence_50 (coe v0)) v1
-- Relation.Binary.Structures.IsDecEquivalence._.sym
d_sym_62 ::
  T_IsDecEquivalence_44 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_62 v0 = coe d_sym_36 (coe d_isEquivalence_50 (coe v0))
-- Relation.Binary.Structures.IsDecEquivalence._.trans
d_trans_64 ::
  T_IsDecEquivalence_44 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_64 v0 = coe d_trans_38 (coe d_isEquivalence_50 (coe v0))
-- Relation.Binary.Structures.IsPreorder
d_IsPreorder_70 a0 a1 a2 a3 a4 a5 = ()
data T_IsPreorder_70
  = C_IsPreorder'46'constructor_3211 T_IsEquivalence_26
                                     (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                     (AgdaAny ->
                                      AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Relation.Binary.Structures.IsPreorder.isEquivalence
d_isEquivalence_80 :: T_IsPreorder_70 -> T_IsEquivalence_26
d_isEquivalence_80 v0
  = case coe v0 of
      C_IsPreorder'46'constructor_3211 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsPreorder.reflexive
d_reflexive_82 ::
  T_IsPreorder_70 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_82 v0
  = case coe v0 of
      C_IsPreorder'46'constructor_3211 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsPreorder.trans
d_trans_84 ::
  T_IsPreorder_70 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_84 v0
  = case coe v0 of
      C_IsPreorder'46'constructor_3211 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsPreorder.Eq.isPartialEquivalence
d_isPartialEquivalence_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPreorder_70 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_88 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_88 v6
du_isPartialEquivalence_88 ::
  T_IsPreorder_70 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_88 v0
  = coe du_isPartialEquivalence_42 (coe d_isEquivalence_80 (coe v0))
-- Relation.Binary.Structures.IsPreorder.Eq.refl
d_refl_90 :: T_IsPreorder_70 -> AgdaAny -> AgdaAny
d_refl_90 v0 = coe d_refl_34 (coe d_isEquivalence_80 (coe v0))
-- Relation.Binary.Structures.IsPreorder.Eq.reflexive
d_reflexive_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_92 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_92 v6
du_reflexive_92 ::
  T_IsPreorder_70 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_92 v0 v1 v2 v3
  = coe du_reflexive_40 (coe d_isEquivalence_80 (coe v0)) v1
-- Relation.Binary.Structures.IsPreorder.Eq.sym
d_sym_94 ::
  T_IsPreorder_70 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_94 v0 = coe d_sym_36 (coe d_isEquivalence_80 (coe v0))
-- Relation.Binary.Structures.IsPreorder.Eq.trans
d_trans_96 ::
  T_IsPreorder_70 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_96 v0 = coe d_trans_38 (coe d_isEquivalence_80 (coe v0))
-- Relation.Binary.Structures.IsPreorder.refl
d_refl_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) -> T_IsPreorder_70 -> AgdaAny -> AgdaAny
d_refl_98 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 = du_refl_98 v6 v7
du_refl_98 :: T_IsPreorder_70 -> AgdaAny -> AgdaAny
du_refl_98 v0 v1
  = coe
      d_reflexive_82 v0 v1 v1
      (coe d_refl_34 (d_isEquivalence_80 (coe v0)) v1)
-- Relation.Binary.Structures.IsPreorder.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPreorder_70 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_100 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
                                    v8 v9 v10 v11
  = du_'8764''45'resp'737''45''8776'_100 v6 v7 v8 v9 v10 v11
du_'8764''45'resp'737''45''8776'_100 ::
  T_IsPreorder_70 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_100 v0 v1 v2 v3 v4 v5
  = coe
      d_trans_84 v0 v3 v2 v1
      (coe
         d_reflexive_82 v0 v3 v2
         (coe d_sym_36 (d_isEquivalence_80 (coe v0)) v2 v3 v4))
      v5
-- Relation.Binary.Structures.IsPreorder.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPreorder_70 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_106 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7
                                    v8 v9 v10 v11
  = du_'8764''45'resp'691''45''8776'_106 v6 v7 v8 v9 v10 v11
du_'8764''45'resp'691''45''8776'_106 ::
  T_IsPreorder_70 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_106 v0 v1 v2 v3 v4 v5
  = coe d_trans_84 v0 v1 v2 v3 v5 (coe d_reflexive_82 v0 v2 v3 v4)
-- Relation.Binary.Structures.IsPreorder.∼-resp-≈
d_'8764''45'resp'45''8776'_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPreorder_70 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_112 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'45''8776'_112 v6
du_'8764''45'resp'45''8776'_112 ::
  T_IsPreorder_70 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_112 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_'8764''45'resp'691''45''8776'_106 (coe v0))
      (coe du_'8764''45'resp'737''45''8776'_100 (coe v0))
-- Relation.Binary.Structures.IsTotalPreorder
d_IsTotalPreorder_118 a0 a1 a2 a3 a4 a5 = ()
data T_IsTotalPreorder_118
  = C_IsTotalPreorder'46'constructor_7157 T_IsPreorder_70
                                          (AgdaAny ->
                                           AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30)
-- Relation.Binary.Structures.IsTotalPreorder.isPreorder
d_isPreorder_126 :: T_IsTotalPreorder_118 -> T_IsPreorder_70
d_isPreorder_126 v0
  = case coe v0 of
      C_IsTotalPreorder'46'constructor_7157 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsTotalPreorder.total
d_total_128 ::
  T_IsTotalPreorder_118 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_128 v0
  = case coe v0 of
      C_IsTotalPreorder'46'constructor_7157 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsTotalPreorder._.isEquivalence
d_isEquivalence_132 :: T_IsTotalPreorder_118 -> T_IsEquivalence_26
d_isEquivalence_132 v0
  = coe d_isEquivalence_80 (coe d_isPreorder_126 (coe v0))
-- Relation.Binary.Structures.IsTotalPreorder._.refl
d_refl_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalPreorder_118 -> AgdaAny -> AgdaAny
d_refl_134 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_134 v6
du_refl_134 :: T_IsTotalPreorder_118 -> AgdaAny -> AgdaAny
du_refl_134 v0 = coe du_refl_98 (coe d_isPreorder_126 (coe v0))
-- Relation.Binary.Structures.IsTotalPreorder._.reflexive
d_reflexive_136 ::
  T_IsTotalPreorder_118 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_136 v0
  = coe d_reflexive_82 (coe d_isPreorder_126 (coe v0))
-- Relation.Binary.Structures.IsTotalPreorder._.trans
d_trans_138 ::
  T_IsTotalPreorder_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_138 v0 = coe d_trans_84 (coe d_isPreorder_126 (coe v0))
-- Relation.Binary.Structures.IsTotalPreorder._.∼-resp-≈
d_'8764''45'resp'45''8776'_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalPreorder_118 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_140 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'45''8776'_140 v6
du_'8764''45'resp'45''8776'_140 ::
  T_IsTotalPreorder_118 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_140 v0
  = coe
      du_'8764''45'resp'45''8776'_112 (coe d_isPreorder_126 (coe v0))
-- Relation.Binary.Structures.IsTotalPreorder._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalPreorder_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_142 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'691''45''8776'_142 v6
du_'8764''45'resp'691''45''8776'_142 ::
  T_IsTotalPreorder_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_142 v0
  = coe
      du_'8764''45'resp'691''45''8776'_106
      (coe d_isPreorder_126 (coe v0))
-- Relation.Binary.Structures.IsTotalPreorder._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalPreorder_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_144 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'737''45''8776'_144 v6
du_'8764''45'resp'737''45''8776'_144 ::
  T_IsTotalPreorder_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_144 v0
  = coe
      du_'8764''45'resp'737''45''8776'_100
      (coe d_isPreorder_126 (coe v0))
-- Relation.Binary.Structures.IsTotalPreorder._.Eq.isPartialEquivalence
d_isPartialEquivalence_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalPreorder_118 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_148 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_148 v6
du_isPartialEquivalence_148 ::
  T_IsTotalPreorder_118 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_148 v0
  = let v1 = d_isPreorder_126 (coe v0) in
    coe du_isPartialEquivalence_42 (coe d_isEquivalence_80 (coe v1))
-- Relation.Binary.Structures.IsTotalPreorder._.Eq.refl
d_refl_150 :: T_IsTotalPreorder_118 -> AgdaAny -> AgdaAny
d_refl_150 v0
  = coe
      d_refl_34 (coe d_isEquivalence_80 (coe d_isPreorder_126 (coe v0)))
-- Relation.Binary.Structures.IsTotalPreorder._.Eq.reflexive
d_reflexive_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalPreorder_118 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_152 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_152 v6
du_reflexive_152 ::
  T_IsTotalPreorder_118 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_152 v0
  = let v1 = d_isPreorder_126 (coe v0) in
    \ v2 v3 v4 ->
      coe du_reflexive_40 (coe d_isEquivalence_80 (coe v1)) v2
-- Relation.Binary.Structures.IsTotalPreorder._.Eq.sym
d_sym_154 ::
  T_IsTotalPreorder_118 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_154 v0
  = coe
      d_sym_36 (coe d_isEquivalence_80 (coe d_isPreorder_126 (coe v0)))
-- Relation.Binary.Structures.IsTotalPreorder._.Eq.trans
d_trans_156 ::
  T_IsTotalPreorder_118 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_156 v0
  = coe
      d_trans_38 (coe d_isEquivalence_80 (coe d_isPreorder_126 (coe v0)))
-- Relation.Binary.Structures.IsPartialOrder
d_IsPartialOrder_162 a0 a1 a2 a3 a4 a5 = ()
data T_IsPartialOrder_162
  = C_IsPartialOrder'46'constructor_8515 T_IsPreorder_70
                                         (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Relation.Binary.Structures.IsPartialOrder.isPreorder
d_isPreorder_170 :: T_IsPartialOrder_162 -> T_IsPreorder_70
d_isPreorder_170 v0
  = case coe v0 of
      C_IsPartialOrder'46'constructor_8515 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsPartialOrder.antisym
d_antisym_172 ::
  T_IsPartialOrder_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_172 v0
  = case coe v0 of
      C_IsPartialOrder'46'constructor_8515 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsPartialOrder._.isEquivalence
d_isEquivalence_176 :: T_IsPartialOrder_162 -> T_IsEquivalence_26
d_isEquivalence_176 v0
  = coe d_isEquivalence_80 (coe d_isPreorder_170 (coe v0))
-- Relation.Binary.Structures.IsPartialOrder._.refl
d_refl_178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPartialOrder_162 -> AgdaAny -> AgdaAny
d_refl_178 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_178 v6
du_refl_178 :: T_IsPartialOrder_162 -> AgdaAny -> AgdaAny
du_refl_178 v0 = coe du_refl_98 (coe d_isPreorder_170 (coe v0))
-- Relation.Binary.Structures.IsPartialOrder._.reflexive
d_reflexive_180 ::
  T_IsPartialOrder_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_180 v0
  = coe d_reflexive_82 (coe d_isPreorder_170 (coe v0))
-- Relation.Binary.Structures.IsPartialOrder._.trans
d_trans_182 ::
  T_IsPartialOrder_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_182 v0 = coe d_trans_84 (coe d_isPreorder_170 (coe v0))
-- Relation.Binary.Structures.IsPartialOrder._.∼-resp-≈
d_'8764''45'resp'45''8776'_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPartialOrder_162 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_184 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'45''8776'_184 v6
du_'8764''45'resp'45''8776'_184 ::
  T_IsPartialOrder_162 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_184 v0
  = coe
      du_'8764''45'resp'45''8776'_112 (coe d_isPreorder_170 (coe v0))
-- Relation.Binary.Structures.IsPartialOrder._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPartialOrder_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_186 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'691''45''8776'_186 v6
du_'8764''45'resp'691''45''8776'_186 ::
  T_IsPartialOrder_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_186 v0
  = coe
      du_'8764''45'resp'691''45''8776'_106
      (coe d_isPreorder_170 (coe v0))
-- Relation.Binary.Structures.IsPartialOrder._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPartialOrder_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_188 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'737''45''8776'_188 v6
du_'8764''45'resp'737''45''8776'_188 ::
  T_IsPartialOrder_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_188 v0
  = coe
      du_'8764''45'resp'737''45''8776'_100
      (coe d_isPreorder_170 (coe v0))
-- Relation.Binary.Structures.IsPartialOrder._.Eq.isPartialEquivalence
d_isPartialEquivalence_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPartialOrder_162 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_192 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_192 v6
du_isPartialEquivalence_192 ::
  T_IsPartialOrder_162 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_192 v0
  = let v1 = d_isPreorder_170 (coe v0) in
    coe du_isPartialEquivalence_42 (coe d_isEquivalence_80 (coe v1))
-- Relation.Binary.Structures.IsPartialOrder._.Eq.refl
d_refl_194 :: T_IsPartialOrder_162 -> AgdaAny -> AgdaAny
d_refl_194 v0
  = coe
      d_refl_34 (coe d_isEquivalence_80 (coe d_isPreorder_170 (coe v0)))
-- Relation.Binary.Structures.IsPartialOrder._.Eq.reflexive
d_reflexive_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsPartialOrder_162 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_196 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_196 v6
du_reflexive_196 ::
  T_IsPartialOrder_162 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_196 v0
  = let v1 = d_isPreorder_170 (coe v0) in
    \ v2 v3 v4 ->
      coe du_reflexive_40 (coe d_isEquivalence_80 (coe v1)) v2
-- Relation.Binary.Structures.IsPartialOrder._.Eq.sym
d_sym_198 ::
  T_IsPartialOrder_162 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_198 v0
  = coe
      d_sym_36 (coe d_isEquivalence_80 (coe d_isPreorder_170 (coe v0)))
-- Relation.Binary.Structures.IsPartialOrder._.Eq.trans
d_trans_200 ::
  T_IsPartialOrder_162 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_200 v0
  = coe
      d_trans_38 (coe d_isEquivalence_80 (coe d_isPreorder_170 (coe v0)))
-- Relation.Binary.Structures.IsDecPartialOrder
d_IsDecPartialOrder_206 a0 a1 a2 a3 a4 a5 = ()
data T_IsDecPartialOrder_206
  = C_IsDecPartialOrder'46'constructor_10175 T_IsPartialOrder_162
                                             (AgdaAny ->
                                              AgdaAny ->
                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
                                             (AgdaAny ->
                                              AgdaAny ->
                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
-- Relation.Binary.Structures.IsDecPartialOrder.isPartialOrder
d_isPartialOrder_216 ::
  T_IsDecPartialOrder_206 -> T_IsPartialOrder_162
d_isPartialOrder_216 v0
  = case coe v0 of
      C_IsDecPartialOrder'46'constructor_10175 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecPartialOrder._≟_
d__'8799'__218 ::
  T_IsDecPartialOrder_206 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__218 v0
  = case coe v0 of
      C_IsDecPartialOrder'46'constructor_10175 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecPartialOrder._≤?_
d__'8804''63'__220 ::
  T_IsDecPartialOrder_206 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__220 v0
  = case coe v0 of
      C_IsDecPartialOrder'46'constructor_10175 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecPartialOrder._.antisym
d_antisym_224 ::
  T_IsDecPartialOrder_206 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_224 v0
  = coe d_antisym_172 (coe d_isPartialOrder_216 (coe v0))
-- Relation.Binary.Structures.IsDecPartialOrder._.isEquivalence
d_isEquivalence_226 ::
  T_IsDecPartialOrder_206 -> T_IsEquivalence_26
d_isEquivalence_226 v0
  = coe
      d_isEquivalence_80
      (coe d_isPreorder_170 (coe d_isPartialOrder_216 (coe v0)))
-- Relation.Binary.Structures.IsDecPartialOrder._.isPreorder
d_isPreorder_228 :: T_IsDecPartialOrder_206 -> T_IsPreorder_70
d_isPreorder_228 v0
  = coe d_isPreorder_170 (coe d_isPartialOrder_216 (coe v0))
-- Relation.Binary.Structures.IsDecPartialOrder._.refl
d_refl_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 -> AgdaAny -> AgdaAny
d_refl_230 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_230 v6
du_refl_230 :: T_IsDecPartialOrder_206 -> AgdaAny -> AgdaAny
du_refl_230 v0
  = let v1 = d_isPartialOrder_216 (coe v0) in
    coe du_refl_98 (coe d_isPreorder_170 (coe v1))
-- Relation.Binary.Structures.IsDecPartialOrder._.reflexive
d_reflexive_232 ::
  T_IsDecPartialOrder_206 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_232 v0
  = coe
      d_reflexive_82
      (coe d_isPreorder_170 (coe d_isPartialOrder_216 (coe v0)))
-- Relation.Binary.Structures.IsDecPartialOrder._.trans
d_trans_234 ::
  T_IsDecPartialOrder_206 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_234 v0
  = coe
      d_trans_84
      (coe d_isPreorder_170 (coe d_isPartialOrder_216 (coe v0)))
-- Relation.Binary.Structures.IsDecPartialOrder._.∼-resp-≈
d_'8764''45'resp'45''8776'_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_236 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'45''8776'_236 v6
du_'8764''45'resp'45''8776'_236 ::
  T_IsDecPartialOrder_206 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_236 v0
  = let v1 = d_isPartialOrder_216 (coe v0) in
    coe du_'8764''45'resp'45''8776'_112 (coe d_isPreorder_170 (coe v1))
-- Relation.Binary.Structures.IsDecPartialOrder._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_238 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'691''45''8776'_238 v6
du_'8764''45'resp'691''45''8776'_238 ::
  T_IsDecPartialOrder_206 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_238 v0
  = let v1 = d_isPartialOrder_216 (coe v0) in
    coe
      du_'8764''45'resp'691''45''8776'_106
      (coe d_isPreorder_170 (coe v1))
-- Relation.Binary.Structures.IsDecPartialOrder._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_240 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'737''45''8776'_240 v6
du_'8764''45'resp'737''45''8776'_240 ::
  T_IsDecPartialOrder_206 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_240 v0
  = let v1 = d_isPartialOrder_216 (coe v0) in
    coe
      du_'8764''45'resp'737''45''8776'_100
      (coe d_isPreorder_170 (coe v1))
-- Relation.Binary.Structures.IsDecPartialOrder.Eq.isDecEquivalence
d_isDecEquivalence_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 -> T_IsDecEquivalence_44
d_isDecEquivalence_244 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isDecEquivalence_244 v6
du_isDecEquivalence_244 ::
  T_IsDecPartialOrder_206 -> T_IsDecEquivalence_44
du_isDecEquivalence_244 v0
  = coe
      C_IsDecEquivalence'46'constructor_2293
      (coe
         d_isEquivalence_80
         (coe d_isPreorder_170 (coe d_isPartialOrder_216 (coe v0))))
      (coe d__'8799'__218 (coe v0))
-- Relation.Binary.Structures.IsDecPartialOrder.Eq._._≟_
d__'8799'__248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__248 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du__'8799'__248 v6
du__'8799'__248 ::
  T_IsDecPartialOrder_206 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8799'__248 v0 = coe d__'8799'__218 (coe v0)
-- Relation.Binary.Structures.IsDecPartialOrder.Eq._.isEquivalence
d_isEquivalence_250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 -> T_IsEquivalence_26
d_isEquivalence_250 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isEquivalence_250 v6
du_isEquivalence_250 ::
  T_IsDecPartialOrder_206 -> T_IsEquivalence_26
du_isEquivalence_250 v0
  = coe
      d_isEquivalence_80
      (coe d_isPreorder_170 (coe d_isPartialOrder_216 (coe v0)))
-- Relation.Binary.Structures.IsDecPartialOrder.Eq._.isPartialEquivalence
d_isPartialEquivalence_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_252 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_252 v6
du_isPartialEquivalence_252 ::
  T_IsDecPartialOrder_206 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_252 v0
  = let v1 = coe du_isDecEquivalence_244 (coe v0) in
    coe du_isPartialEquivalence_42 (coe d_isEquivalence_50 (coe v1))
-- Relation.Binary.Structures.IsDecPartialOrder.Eq._.refl
d_refl_254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 -> AgdaAny -> AgdaAny
d_refl_254 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_254 v6
du_refl_254 :: T_IsDecPartialOrder_206 -> AgdaAny -> AgdaAny
du_refl_254 v0
  = coe
      d_refl_34
      (coe
         d_isEquivalence_80
         (coe d_isPreorder_170 (coe d_isPartialOrder_216 (coe v0))))
-- Relation.Binary.Structures.IsDecPartialOrder.Eq._.reflexive
d_reflexive_256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_256 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_256 v6
du_reflexive_256 ::
  T_IsDecPartialOrder_206 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_256 v0
  = let v1 = coe du_isDecEquivalence_244 (coe v0) in
    \ v2 v3 v4 ->
      coe du_reflexive_40 (coe d_isEquivalence_50 (coe v1)) v2
-- Relation.Binary.Structures.IsDecPartialOrder.Eq._.sym
d_sym_258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_258 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_sym_258 v6
du_sym_258 ::
  T_IsDecPartialOrder_206 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_258 v0
  = coe
      d_sym_36
      (coe
         d_isEquivalence_80
         (coe d_isPreorder_170 (coe d_isPartialOrder_216 (coe v0))))
-- Relation.Binary.Structures.IsDecPartialOrder.Eq._.trans
d_trans_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecPartialOrder_206 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_260 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_trans_260 v6
du_trans_260 ::
  T_IsDecPartialOrder_206 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_260 v0
  = coe
      d_trans_38
      (coe
         d_isEquivalence_80
         (coe d_isPreorder_170 (coe d_isPartialOrder_216 (coe v0))))
-- Relation.Binary.Structures.IsStrictPartialOrder
d_IsStrictPartialOrder_266 a0 a1 a2 a3 a4 a5 = ()
data T_IsStrictPartialOrder_266
  = C_IsStrictPartialOrder'46'constructor_12363 T_IsEquivalence_26
                                                (AgdaAny ->
                                                 AgdaAny ->
                                                 AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                                MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Relation.Binary.Structures.IsStrictPartialOrder.isEquivalence
d_isEquivalence_278 ::
  T_IsStrictPartialOrder_266 -> T_IsEquivalence_26
d_isEquivalence_278 v0
  = case coe v0 of
      C_IsStrictPartialOrder'46'constructor_12363 v1 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsStrictPartialOrder.irrefl
d_irrefl_280 ::
  T_IsStrictPartialOrder_266 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_280 = erased
-- Relation.Binary.Structures.IsStrictPartialOrder.trans
d_trans_282 ::
  T_IsStrictPartialOrder_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_282 v0
  = case coe v0 of
      C_IsStrictPartialOrder'46'constructor_12363 v1 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsStrictPartialOrder.<-resp-≈
d_'60''45'resp'45''8776'_284 ::
  T_IsStrictPartialOrder_266 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8776'_284 v0
  = case coe v0 of
      C_IsStrictPartialOrder'46'constructor_12363 v1 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsStrictPartialOrder.Eq.isPartialEquivalence
d_isPartialEquivalence_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictPartialOrder_266 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_288 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_288 v6
du_isPartialEquivalence_288 ::
  T_IsStrictPartialOrder_266 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_288 v0
  = coe du_isPartialEquivalence_42 (coe d_isEquivalence_278 (coe v0))
-- Relation.Binary.Structures.IsStrictPartialOrder.Eq.refl
d_refl_290 :: T_IsStrictPartialOrder_266 -> AgdaAny -> AgdaAny
d_refl_290 v0 = coe d_refl_34 (coe d_isEquivalence_278 (coe v0))
-- Relation.Binary.Structures.IsStrictPartialOrder.Eq.reflexive
d_reflexive_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictPartialOrder_266 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_292 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_292 v6
du_reflexive_292 ::
  T_IsStrictPartialOrder_266 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_292 v0 v1 v2 v3
  = coe du_reflexive_40 (coe d_isEquivalence_278 (coe v0)) v1
-- Relation.Binary.Structures.IsStrictPartialOrder.Eq.sym
d_sym_294 ::
  T_IsStrictPartialOrder_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_294 v0 = coe d_sym_36 (coe d_isEquivalence_278 (coe v0))
-- Relation.Binary.Structures.IsStrictPartialOrder.Eq.trans
d_trans_296 ::
  T_IsStrictPartialOrder_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_296 v0 = coe d_trans_38 (coe d_isEquivalence_278 (coe v0))
-- Relation.Binary.Structures.IsStrictPartialOrder.asym
d_asym_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictPartialOrder_266 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asym_298 = erased
-- Relation.Binary.Structures.IsStrictPartialOrder.<-respʳ-≈
d_'60''45'resp'691''45''8776'_304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictPartialOrder_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'691''45''8776'_304 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
                                  v9
  = du_'60''45'resp'691''45''8776'_304 v6 v7 v8 v9
du_'60''45'resp'691''45''8776'_304 ::
  T_IsStrictPartialOrder_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'691''45''8776'_304 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
      (d_'60''45'resp'45''8776'_284 (coe v0)) v1 v2 v3
-- Relation.Binary.Structures.IsStrictPartialOrder.<-respˡ-≈
d_'60''45'resp'737''45''8776'_306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictPartialOrder_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'737''45''8776'_306 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
                                  v9
  = du_'60''45'resp'737''45''8776'_306 v6 v7 v8 v9
du_'60''45'resp'737''45''8776'_306 ::
  T_IsStrictPartialOrder_266 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'737''45''8776'_306 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
      (d_'60''45'resp'45''8776'_284 (coe v0)) v1 v2 v3
-- Relation.Binary.Structures.IsDecStrictPartialOrder
d_IsDecStrictPartialOrder_312 a0 a1 a2 a3 a4 a5 = ()
data T_IsDecStrictPartialOrder_312
  = C_IsDecStrictPartialOrder'46'constructor_16967 T_IsStrictPartialOrder_266
                                                   (AgdaAny ->
                                                    AgdaAny ->
                                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
                                                   (AgdaAny ->
                                                    AgdaAny ->
                                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
-- Relation.Binary.Structures.IsDecStrictPartialOrder.isStrictPartialOrder
d_isStrictPartialOrder_322 ::
  T_IsDecStrictPartialOrder_312 -> T_IsStrictPartialOrder_266
d_isStrictPartialOrder_322 v0
  = case coe v0 of
      C_IsDecStrictPartialOrder'46'constructor_16967 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecStrictPartialOrder._≟_
d__'8799'__324 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__324 v0
  = case coe v0 of
      C_IsDecStrictPartialOrder'46'constructor_16967 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecStrictPartialOrder._<?_
d__'60''63'__326 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__326 v0
  = case coe v0 of
      C_IsDecStrictPartialOrder'46'constructor_16967 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.<-resp-≈
d_'60''45'resp'45''8776'_330 ::
  T_IsDecStrictPartialOrder_312 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8776'_330 v0
  = coe
      d_'60''45'resp'45''8776'_284
      (coe d_isStrictPartialOrder_322 (coe v0))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.<-respʳ-≈
d_'60''45'resp'691''45''8776'_332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'691''45''8776'_332 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'60''45'resp'691''45''8776'_332 v6
du_'60''45'resp'691''45''8776'_332 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'691''45''8776'_332 v0
  = coe
      du_'60''45'resp'691''45''8776'_304
      (coe d_isStrictPartialOrder_322 (coe v0))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.<-respˡ-≈
d_'60''45'resp'737''45''8776'_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'737''45''8776'_334 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'60''45'resp'737''45''8776'_334 v6
du_'60''45'resp'737''45''8776'_334 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'737''45''8776'_334 v0
  = coe
      du_'60''45'resp'737''45''8776'_306
      (coe d_isStrictPartialOrder_322 (coe v0))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.asym
d_asym_336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asym_336 = erased
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.irrefl
d_irrefl_338 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_338 = erased
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.isEquivalence
d_isEquivalence_340 ::
  T_IsDecStrictPartialOrder_312 -> T_IsEquivalence_26
d_isEquivalence_340 v0
  = coe d_isEquivalence_278 (coe d_isStrictPartialOrder_322 (coe v0))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.trans
d_trans_342 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_342 v0
  = coe d_trans_282 (coe d_isStrictPartialOrder_322 (coe v0))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.Eq.isPartialEquivalence
d_isPartialEquivalence_346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_346 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_346 v6
du_isPartialEquivalence_346 ::
  T_IsDecStrictPartialOrder_312 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_346 v0
  = let v1 = d_isStrictPartialOrder_322 (coe v0) in
    coe du_isPartialEquivalence_42 (coe d_isEquivalence_278 (coe v1))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.Eq.refl
d_refl_348 :: T_IsDecStrictPartialOrder_312 -> AgdaAny -> AgdaAny
d_refl_348 v0
  = coe
      d_refl_34
      (coe d_isEquivalence_278 (coe d_isStrictPartialOrder_322 (coe v0)))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.Eq.reflexive
d_reflexive_350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_350 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_350 v6
du_reflexive_350 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_350 v0
  = let v1 = d_isStrictPartialOrder_322 (coe v0) in
    \ v2 v3 v4 ->
      coe du_reflexive_40 (coe d_isEquivalence_278 (coe v1)) v2
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.Eq.sym
d_sym_352 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_352 v0
  = coe
      d_sym_36
      (coe d_isEquivalence_278 (coe d_isStrictPartialOrder_322 (coe v0)))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.SPO.Eq.trans
d_trans_354 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_354 v0
  = coe
      d_trans_38
      (coe d_isEquivalence_278 (coe d_isStrictPartialOrder_322 (coe v0)))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.Eq.isDecEquivalence
d_isDecEquivalence_358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 -> T_IsDecEquivalence_44
d_isDecEquivalence_358 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isDecEquivalence_358 v6
du_isDecEquivalence_358 ::
  T_IsDecStrictPartialOrder_312 -> T_IsDecEquivalence_44
du_isDecEquivalence_358 v0
  = coe
      C_IsDecEquivalence'46'constructor_2293
      (coe d_isEquivalence_278 (coe d_isStrictPartialOrder_322 (coe v0)))
      (coe d__'8799'__324 (coe v0))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.Eq._._≟_
d__'8799'__362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__362 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du__'8799'__362 v6
du__'8799'__362 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8799'__362 v0 = coe d__'8799'__324 (coe v0)
-- Relation.Binary.Structures.IsDecStrictPartialOrder.Eq._.isEquivalence
d_isEquivalence_364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 -> T_IsEquivalence_26
d_isEquivalence_364 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isEquivalence_364 v6
du_isEquivalence_364 ::
  T_IsDecStrictPartialOrder_312 -> T_IsEquivalence_26
du_isEquivalence_364 v0
  = coe d_isEquivalence_278 (coe d_isStrictPartialOrder_322 (coe v0))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.Eq._.isPartialEquivalence
d_isPartialEquivalence_366 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_366 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_366 v6
du_isPartialEquivalence_366 ::
  T_IsDecStrictPartialOrder_312 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_366 v0
  = let v1 = coe du_isDecEquivalence_358 (coe v0) in
    coe du_isPartialEquivalence_42 (coe d_isEquivalence_50 (coe v1))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.Eq._.refl
d_refl_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 -> AgdaAny -> AgdaAny
d_refl_368 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_368 v6
du_refl_368 :: T_IsDecStrictPartialOrder_312 -> AgdaAny -> AgdaAny
du_refl_368 v0
  = coe
      d_refl_34
      (coe d_isEquivalence_278 (coe d_isStrictPartialOrder_322 (coe v0)))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.Eq._.reflexive
d_reflexive_370 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_370 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_370 v6
du_reflexive_370 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_370 v0
  = let v1 = coe du_isDecEquivalence_358 (coe v0) in
    \ v2 v3 v4 ->
      coe du_reflexive_40 (coe d_isEquivalence_50 (coe v1)) v2
-- Relation.Binary.Structures.IsDecStrictPartialOrder.Eq._.sym
d_sym_372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_372 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_sym_372 v6
du_sym_372 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_372 v0
  = coe
      d_sym_36
      (coe d_isEquivalence_278 (coe d_isStrictPartialOrder_322 (coe v0)))
-- Relation.Binary.Structures.IsDecStrictPartialOrder.Eq._.trans
d_trans_374 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_374 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_trans_374 v6
du_trans_374 ::
  T_IsDecStrictPartialOrder_312 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_374 v0
  = coe
      d_trans_38
      (coe d_isEquivalence_278 (coe d_isStrictPartialOrder_322 (coe v0)))
-- Relation.Binary.Structures.IsTotalOrder
d_IsTotalOrder_380 a0 a1 a2 a3 a4 a5 = ()
data T_IsTotalOrder_380
  = C_IsTotalOrder'46'constructor_18851 T_IsPartialOrder_162
                                        (AgdaAny ->
                                         AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30)
-- Relation.Binary.Structures.IsTotalOrder.isPartialOrder
d_isPartialOrder_388 :: T_IsTotalOrder_380 -> T_IsPartialOrder_162
d_isPartialOrder_388 v0
  = case coe v0 of
      C_IsTotalOrder'46'constructor_18851 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsTotalOrder.total
d_total_390 ::
  T_IsTotalOrder_380 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_390 v0
  = case coe v0 of
      C_IsTotalOrder'46'constructor_18851 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsTotalOrder._.antisym
d_antisym_394 ::
  T_IsTotalOrder_380 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_394 v0
  = coe d_antisym_172 (coe d_isPartialOrder_388 (coe v0))
-- Relation.Binary.Structures.IsTotalOrder._.isEquivalence
d_isEquivalence_396 :: T_IsTotalOrder_380 -> T_IsEquivalence_26
d_isEquivalence_396 v0
  = coe
      d_isEquivalence_80
      (coe d_isPreorder_170 (coe d_isPartialOrder_388 (coe v0)))
-- Relation.Binary.Structures.IsTotalOrder._.isPreorder
d_isPreorder_398 :: T_IsTotalOrder_380 -> T_IsPreorder_70
d_isPreorder_398 v0
  = coe d_isPreorder_170 (coe d_isPartialOrder_388 (coe v0))
-- Relation.Binary.Structures.IsTotalOrder._.refl
d_refl_400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalOrder_380 -> AgdaAny -> AgdaAny
d_refl_400 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_400 v6
du_refl_400 :: T_IsTotalOrder_380 -> AgdaAny -> AgdaAny
du_refl_400 v0
  = let v1 = d_isPartialOrder_388 (coe v0) in
    coe du_refl_98 (coe d_isPreorder_170 (coe v1))
-- Relation.Binary.Structures.IsTotalOrder._.reflexive
d_reflexive_402 ::
  T_IsTotalOrder_380 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_402 v0
  = coe
      d_reflexive_82
      (coe d_isPreorder_170 (coe d_isPartialOrder_388 (coe v0)))
-- Relation.Binary.Structures.IsTotalOrder._.trans
d_trans_404 ::
  T_IsTotalOrder_380 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_404 v0
  = coe
      d_trans_84
      (coe d_isPreorder_170 (coe d_isPartialOrder_388 (coe v0)))
-- Relation.Binary.Structures.IsTotalOrder._.∼-resp-≈
d_'8764''45'resp'45''8776'_406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalOrder_380 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_406 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'45''8776'_406 v6
du_'8764''45'resp'45''8776'_406 ::
  T_IsTotalOrder_380 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_406 v0
  = let v1 = d_isPartialOrder_388 (coe v0) in
    coe du_'8764''45'resp'45''8776'_112 (coe d_isPreorder_170 (coe v1))
-- Relation.Binary.Structures.IsTotalOrder._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_408 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalOrder_380 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_408 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'691''45''8776'_408 v6
du_'8764''45'resp'691''45''8776'_408 ::
  T_IsTotalOrder_380 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_408 v0
  = let v1 = d_isPartialOrder_388 (coe v0) in
    coe
      du_'8764''45'resp'691''45''8776'_106
      (coe d_isPreorder_170 (coe v1))
-- Relation.Binary.Structures.IsTotalOrder._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_410 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalOrder_380 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_410 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'737''45''8776'_410 v6
du_'8764''45'resp'737''45''8776'_410 ::
  T_IsTotalOrder_380 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_410 v0
  = let v1 = d_isPartialOrder_388 (coe v0) in
    coe
      du_'8764''45'resp'737''45''8776'_100
      (coe d_isPreorder_170 (coe v1))
-- Relation.Binary.Structures.IsTotalOrder._.Eq.isPartialEquivalence
d_isPartialEquivalence_414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalOrder_380 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_414 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_414 v6
du_isPartialEquivalence_414 ::
  T_IsTotalOrder_380 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_414 v0
  = let v1 = d_isPartialOrder_388 (coe v0) in
    let v2 = d_isPreorder_170 (coe v1) in
    coe du_isPartialEquivalence_42 (coe d_isEquivalence_80 (coe v2))
-- Relation.Binary.Structures.IsTotalOrder._.Eq.refl
d_refl_416 :: T_IsTotalOrder_380 -> AgdaAny -> AgdaAny
d_refl_416 v0
  = coe
      d_refl_34
      (coe
         d_isEquivalence_80
         (coe d_isPreorder_170 (coe d_isPartialOrder_388 (coe v0))))
-- Relation.Binary.Structures.IsTotalOrder._.Eq.reflexive
d_reflexive_418 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalOrder_380 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_418 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_418 v6
du_reflexive_418 ::
  T_IsTotalOrder_380 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_418 v0
  = let v1 = d_isPartialOrder_388 (coe v0) in
    let v2 = d_isPreorder_170 (coe v1) in
    \ v3 v4 v5 ->
      coe du_reflexive_40 (coe d_isEquivalence_80 (coe v2)) v3
-- Relation.Binary.Structures.IsTotalOrder._.Eq.sym
d_sym_420 ::
  T_IsTotalOrder_380 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_420 v0
  = coe
      d_sym_36
      (coe
         d_isEquivalence_80
         (coe d_isPreorder_170 (coe d_isPartialOrder_388 (coe v0))))
-- Relation.Binary.Structures.IsTotalOrder._.Eq.trans
d_trans_422 ::
  T_IsTotalOrder_380 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_422 v0
  = coe
      d_trans_38
      (coe
         d_isEquivalence_80
         (coe d_isPreorder_170 (coe d_isPartialOrder_388 (coe v0))))
-- Relation.Binary.Structures.IsTotalOrder.isTotalPreorder
d_isTotalPreorder_424 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsTotalOrder_380 -> T_IsTotalPreorder_118
d_isTotalPreorder_424 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isTotalPreorder_424 v6
du_isTotalPreorder_424 ::
  T_IsTotalOrder_380 -> T_IsTotalPreorder_118
du_isTotalPreorder_424 v0
  = coe
      C_IsTotalPreorder'46'constructor_7157
      (coe d_isPreorder_170 (coe d_isPartialOrder_388 (coe v0)))
      (coe d_total_390 (coe v0))
-- Relation.Binary.Structures.IsDecTotalOrder
d_IsDecTotalOrder_430 a0 a1 a2 a3 a4 a5 = ()
data T_IsDecTotalOrder_430
  = C_IsDecTotalOrder'46'constructor_20821 T_IsTotalOrder_380
                                           (AgdaAny ->
                                            AgdaAny ->
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
                                           (AgdaAny ->
                                            AgdaAny ->
                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
-- Relation.Binary.Structures.IsDecTotalOrder.isTotalOrder
d_isTotalOrder_440 :: T_IsDecTotalOrder_430 -> T_IsTotalOrder_380
d_isTotalOrder_440 v0
  = case coe v0 of
      C_IsDecTotalOrder'46'constructor_20821 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecTotalOrder._≟_
d__'8799'__442 ::
  T_IsDecTotalOrder_430 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__442 v0
  = case coe v0 of
      C_IsDecTotalOrder'46'constructor_20821 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecTotalOrder._≤?_
d__'8804''63'__444 ::
  T_IsDecTotalOrder_430 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__444 v0
  = case coe v0 of
      C_IsDecTotalOrder'46'constructor_20821 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsDecTotalOrder._.antisym
d_antisym_448 ::
  T_IsDecTotalOrder_430 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_antisym_448 v0
  = coe
      d_antisym_172
      (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0)))
-- Relation.Binary.Structures.IsDecTotalOrder._.isEquivalence
d_isEquivalence_450 :: T_IsDecTotalOrder_430 -> T_IsEquivalence_26
d_isEquivalence_450 v0
  = coe
      d_isEquivalence_80
      (coe
         d_isPreorder_170
         (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0))))
-- Relation.Binary.Structures.IsDecTotalOrder._.isPartialOrder
d_isPartialOrder_452 ::
  T_IsDecTotalOrder_430 -> T_IsPartialOrder_162
d_isPartialOrder_452 v0
  = coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0))
-- Relation.Binary.Structures.IsDecTotalOrder._.isPreorder
d_isPreorder_454 :: T_IsDecTotalOrder_430 -> T_IsPreorder_70
d_isPreorder_454 v0
  = coe
      d_isPreorder_170
      (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0)))
-- Relation.Binary.Structures.IsDecTotalOrder._.isTotalPreorder
d_isTotalPreorder_456 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 -> T_IsTotalPreorder_118
d_isTotalPreorder_456 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isTotalPreorder_456 v6
du_isTotalPreorder_456 ::
  T_IsDecTotalOrder_430 -> T_IsTotalPreorder_118
du_isTotalPreorder_456 v0
  = coe du_isTotalPreorder_424 (coe d_isTotalOrder_440 (coe v0))
-- Relation.Binary.Structures.IsDecTotalOrder._.refl
d_refl_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 -> AgdaAny -> AgdaAny
d_refl_458 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_458 v6
du_refl_458 :: T_IsDecTotalOrder_430 -> AgdaAny -> AgdaAny
du_refl_458 v0
  = let v1 = d_isTotalOrder_440 (coe v0) in
    let v2 = d_isPartialOrder_388 (coe v1) in
    coe du_refl_98 (coe d_isPreorder_170 (coe v2))
-- Relation.Binary.Structures.IsDecTotalOrder._.reflexive
d_reflexive_460 ::
  T_IsDecTotalOrder_430 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_reflexive_460 v0
  = coe
      d_reflexive_82
      (coe
         d_isPreorder_170
         (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0))))
-- Relation.Binary.Structures.IsDecTotalOrder._.total
d_total_462 ::
  T_IsDecTotalOrder_430 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_total_462 v0 = coe d_total_390 (coe d_isTotalOrder_440 (coe v0))
-- Relation.Binary.Structures.IsDecTotalOrder._.trans
d_trans_464 ::
  T_IsDecTotalOrder_430 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_464 v0
  = coe
      d_trans_84
      (coe
         d_isPreorder_170
         (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0))))
-- Relation.Binary.Structures.IsDecTotalOrder._.∼-resp-≈
d_'8764''45'resp'45''8776'_466 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8764''45'resp'45''8776'_466 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'45''8776'_466 v6
du_'8764''45'resp'45''8776'_466 ::
  T_IsDecTotalOrder_430 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8764''45'resp'45''8776'_466 v0
  = let v1 = d_isTotalOrder_440 (coe v0) in
    let v2 = d_isPartialOrder_388 (coe v1) in
    coe du_'8764''45'resp'45''8776'_112 (coe d_isPreorder_170 (coe v2))
-- Relation.Binary.Structures.IsDecTotalOrder._.∼-respʳ-≈
d_'8764''45'resp'691''45''8776'_468 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'691''45''8776'_468 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'691''45''8776'_468 v6
du_'8764''45'resp'691''45''8776'_468 ::
  T_IsDecTotalOrder_430 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'691''45''8776'_468 v0
  = let v1 = d_isTotalOrder_440 (coe v0) in
    let v2 = d_isPartialOrder_388 (coe v1) in
    coe
      du_'8764''45'resp'691''45''8776'_106
      (coe d_isPreorder_170 (coe v2))
-- Relation.Binary.Structures.IsDecTotalOrder._.∼-respˡ-≈
d_'8764''45'resp'737''45''8776'_470 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'8764''45'resp'737''45''8776'_470 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'8764''45'resp'737''45''8776'_470 v6
du_'8764''45'resp'737''45''8776'_470 ::
  T_IsDecTotalOrder_430 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'8764''45'resp'737''45''8776'_470 v0
  = let v1 = d_isTotalOrder_440 (coe v0) in
    let v2 = d_isPartialOrder_388 (coe v1) in
    coe
      du_'8764''45'resp'737''45''8776'_100
      (coe d_isPreorder_170 (coe v2))
-- Relation.Binary.Structures.IsDecTotalOrder.isDecPartialOrder
d_isDecPartialOrder_472 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 -> T_IsDecPartialOrder_206
d_isDecPartialOrder_472 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isDecPartialOrder_472 v6
du_isDecPartialOrder_472 ::
  T_IsDecTotalOrder_430 -> T_IsDecPartialOrder_206
du_isDecPartialOrder_472 v0
  = coe
      C_IsDecPartialOrder'46'constructor_10175
      (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0)))
      (coe d__'8799'__442 (coe v0)) (coe d__'8804''63'__444 (coe v0))
-- Relation.Binary.Structures.IsDecTotalOrder.Eq.isDecEquivalence
d_isDecEquivalence_476 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 -> T_IsDecEquivalence_44
d_isDecEquivalence_476 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isDecEquivalence_476 v6
du_isDecEquivalence_476 ::
  T_IsDecTotalOrder_430 -> T_IsDecEquivalence_44
du_isDecEquivalence_476 v0
  = coe
      C_IsDecEquivalence'46'constructor_2293
      (coe
         d_isEquivalence_80
         (coe
            d_isPreorder_170
            (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0)))))
      (coe d__'8799'__442 (coe v0))
-- Relation.Binary.Structures.IsDecTotalOrder.Eq._._≟_
d__'8799'__480 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__480 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du__'8799'__480 v6
du__'8799'__480 ::
  T_IsDecTotalOrder_430 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8799'__480 v0 = coe d__'8799'__442 (coe v0)
-- Relation.Binary.Structures.IsDecTotalOrder.Eq._.isEquivalence
d_isEquivalence_482 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 -> T_IsEquivalence_26
d_isEquivalence_482 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isEquivalence_482 v6
du_isEquivalence_482 :: T_IsDecTotalOrder_430 -> T_IsEquivalence_26
du_isEquivalence_482 v0
  = coe
      d_isEquivalence_80
      (coe
         d_isPreorder_170
         (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0))))
-- Relation.Binary.Structures.IsDecTotalOrder.Eq._.isPartialEquivalence
d_isPartialEquivalence_484 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_484 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_484 v6
du_isPartialEquivalence_484 ::
  T_IsDecTotalOrder_430 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_484 v0
  = let v1 = coe du_isDecEquivalence_476 (coe v0) in
    coe du_isPartialEquivalence_42 (coe d_isEquivalence_50 (coe v1))
-- Relation.Binary.Structures.IsDecTotalOrder.Eq._.refl
d_refl_486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 -> AgdaAny -> AgdaAny
d_refl_486 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_486 v6
du_refl_486 :: T_IsDecTotalOrder_430 -> AgdaAny -> AgdaAny
du_refl_486 v0
  = coe
      d_refl_34
      (coe
         d_isEquivalence_80
         (coe
            d_isPreorder_170
            (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0)))))
-- Relation.Binary.Structures.IsDecTotalOrder.Eq._.reflexive
d_reflexive_488 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_488 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_488 v6
du_reflexive_488 ::
  T_IsDecTotalOrder_430 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_488 v0
  = let v1 = coe du_isDecEquivalence_476 (coe v0) in
    \ v2 v3 v4 ->
      coe du_reflexive_40 (coe d_isEquivalence_50 (coe v1)) v2
-- Relation.Binary.Structures.IsDecTotalOrder.Eq._.sym
d_sym_490 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_490 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_sym_490 v6
du_sym_490 ::
  T_IsDecTotalOrder_430 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_490 v0
  = coe
      d_sym_36
      (coe
         d_isEquivalence_80
         (coe
            d_isPreorder_170
            (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0)))))
-- Relation.Binary.Structures.IsDecTotalOrder.Eq._.trans
d_trans_492 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsDecTotalOrder_430 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_492 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_trans_492 v6
du_trans_492 ::
  T_IsDecTotalOrder_430 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_492 v0
  = coe
      d_trans_38
      (coe
         d_isEquivalence_80
         (coe
            d_isPreorder_170
            (coe d_isPartialOrder_388 (coe d_isTotalOrder_440 (coe v0)))))
-- Relation.Binary.Structures.IsStrictTotalOrder
d_IsStrictTotalOrder_498 a0 a1 a2 a3 a4 a5 = ()
data T_IsStrictTotalOrder_498
  = C_IsStrictTotalOrder'46'constructor_23035 T_IsEquivalence_26
                                              (AgdaAny ->
                                               AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
                                              (AgdaAny ->
                                               AgdaAny ->
                                               MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136)
-- Relation.Binary.Structures.IsStrictTotalOrder.isEquivalence
d_isEquivalence_508 ::
  T_IsStrictTotalOrder_498 -> T_IsEquivalence_26
d_isEquivalence_508 v0
  = case coe v0 of
      C_IsStrictTotalOrder'46'constructor_23035 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsStrictTotalOrder.trans
d_trans_510 ::
  T_IsStrictTotalOrder_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_510 v0
  = case coe v0 of
      C_IsStrictTotalOrder'46'constructor_23035 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsStrictTotalOrder.compare
d_compare_512 ::
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_compare_512 v0
  = case coe v0 of
      C_IsStrictTotalOrder'46'constructor_23035 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsStrictTotalOrder._≟_
d__'8799'__514 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__514 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du__'8799'__514 v6
du__'8799'__514 ::
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8799'__514 v0
  = coe
      MAlonzo.Code.Relation.Binary.Consequences.du_tri'8658'dec'8776'_492
      (coe d_compare_512 (coe v0))
-- Relation.Binary.Structures.IsStrictTotalOrder._<?_
d__'60''63'__516 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__516 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du__'60''63'__516 v6
du__'60''63'__516 ::
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'60''63'__516 v0
  = coe
      MAlonzo.Code.Relation.Binary.Consequences.du_tri'8658'dec'60'_528
      (coe d_compare_512 (coe v0))
-- Relation.Binary.Structures.IsStrictTotalOrder.isDecEquivalence
d_isDecEquivalence_518 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 -> T_IsDecEquivalence_44
d_isDecEquivalence_518 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isDecEquivalence_518 v6
du_isDecEquivalence_518 ::
  T_IsStrictTotalOrder_498 -> T_IsDecEquivalence_44
du_isDecEquivalence_518 v0
  = coe
      C_IsDecEquivalence'46'constructor_2293
      (coe d_isEquivalence_508 (coe v0)) (coe du__'8799'__514 (coe v0))
-- Relation.Binary.Structures.IsStrictTotalOrder.Eq._≟_
d__'8799'__522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__522 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du__'8799'__522 v6
du__'8799'__522 ::
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8799'__522 v0 = coe du__'8799'__514 (coe v0)
-- Relation.Binary.Structures.IsStrictTotalOrder.Eq.isEquivalence
d_isEquivalence_524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 -> T_IsEquivalence_26
d_isEquivalence_524 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isEquivalence_524 v6
du_isEquivalence_524 ::
  T_IsStrictTotalOrder_498 -> T_IsEquivalence_26
du_isEquivalence_524 v0 = coe d_isEquivalence_508 (coe v0)
-- Relation.Binary.Structures.IsStrictTotalOrder.Eq.isPartialEquivalence
d_isPartialEquivalence_526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 -> T_IsPartialEquivalence_16
d_isPartialEquivalence_526 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isPartialEquivalence_526 v6
du_isPartialEquivalence_526 ::
  T_IsStrictTotalOrder_498 -> T_IsPartialEquivalence_16
du_isPartialEquivalence_526 v0
  = let v1 = coe du_isDecEquivalence_518 (coe v0) in
    coe du_isPartialEquivalence_42 (coe d_isEquivalence_50 (coe v1))
-- Relation.Binary.Structures.IsStrictTotalOrder.Eq.refl
d_refl_528 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 -> AgdaAny -> AgdaAny
d_refl_528 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_refl_528 v6
du_refl_528 :: T_IsStrictTotalOrder_498 -> AgdaAny -> AgdaAny
du_refl_528 v0 = coe d_refl_34 (coe d_isEquivalence_508 (coe v0))
-- Relation.Binary.Structures.IsStrictTotalOrder.Eq.reflexive
d_reflexive_530 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_reflexive_530 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_reflexive_530 v6
du_reflexive_530 ::
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
du_reflexive_530 v0
  = let v1 = coe du_isDecEquivalence_518 (coe v0) in
    \ v2 v3 v4 ->
      coe du_reflexive_40 (coe d_isEquivalence_50 (coe v1)) v2
-- Relation.Binary.Structures.IsStrictTotalOrder.Eq.sym
d_sym_532 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_532 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_sym_532 v6
du_sym_532 ::
  T_IsStrictTotalOrder_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_sym_532 v0 = coe d_sym_36 (coe d_isEquivalence_508 (coe v0))
-- Relation.Binary.Structures.IsStrictTotalOrder.Eq.trans
d_trans_534 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_trans_534 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_trans_534 v6
du_trans_534 ::
  T_IsStrictTotalOrder_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_trans_534 v0 = coe d_trans_38 (coe d_isEquivalence_508 (coe v0))
-- Relation.Binary.Structures.IsStrictTotalOrder.isStrictPartialOrder
d_isStrictPartialOrder_536 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 -> T_IsStrictPartialOrder_266
d_isStrictPartialOrder_536 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isStrictPartialOrder_536 v6
du_isStrictPartialOrder_536 ::
  T_IsStrictTotalOrder_498 -> T_IsStrictPartialOrder_266
du_isStrictPartialOrder_536 v0
  = coe
      C_IsStrictPartialOrder'46'constructor_12363
      (d_isEquivalence_508 (coe v0)) (d_trans_510 (coe v0))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_trans'8743'tri'8658'resp_716
         (coe d_compare_512 (coe v0)))
-- Relation.Binary.Structures.IsStrictTotalOrder.isDecStrictPartialOrder
d_isDecStrictPartialOrder_538 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 -> T_IsDecStrictPartialOrder_312
d_isDecStrictPartialOrder_538 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_isDecStrictPartialOrder_538 v6
du_isDecStrictPartialOrder_538 ::
  T_IsStrictTotalOrder_498 -> T_IsDecStrictPartialOrder_312
du_isDecStrictPartialOrder_538 v0
  = coe
      C_IsDecStrictPartialOrder'46'constructor_16967
      (coe du_isStrictPartialOrder_536 (coe v0))
      (coe du__'8799'__514 (coe v0)) (coe du__'60''63'__516 (coe v0))
-- Relation.Binary.Structures.IsStrictTotalOrder._.<-resp-≈
d_'60''45'resp'45''8776'_542 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'60''45'resp'45''8776'_542 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'60''45'resp'45''8776'_542 v6
du_'60''45'resp'45''8776'_542 ::
  T_IsStrictTotalOrder_498 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'60''45'resp'45''8776'_542 v0
  = coe
      MAlonzo.Code.Relation.Binary.Consequences.du_trans'8743'tri'8658'resp_716
      (coe d_compare_512 (coe v0))
-- Relation.Binary.Structures.IsStrictTotalOrder._.<-respʳ-≈
d_'60''45'resp'691''45''8776'_544 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'691''45''8776'_544 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'60''45'resp'691''45''8776'_544 v6
du_'60''45'resp'691''45''8776'_544 ::
  T_IsStrictTotalOrder_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'691''45''8776'_544 v0
  = coe
      du_'60''45'resp'691''45''8776'_304
      (coe du_isStrictPartialOrder_536 (coe v0))
-- Relation.Binary.Structures.IsStrictTotalOrder._.<-respˡ-≈
d_'60''45'resp'737''45''8776'_546 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_'60''45'resp'737''45''8776'_546 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_'60''45'resp'737''45''8776'_546 v6
du_'60''45'resp'737''45''8776'_546 ::
  T_IsStrictTotalOrder_498 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_'60''45'resp'737''45''8776'_546 v0
  = coe
      du_'60''45'resp'737''45''8776'_306
      (coe du_isStrictPartialOrder_536 (coe v0))
-- Relation.Binary.Structures.IsStrictTotalOrder._.asym
d_asym_548 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_asym_548 = erased
-- Relation.Binary.Structures.IsStrictTotalOrder._.irrefl
d_irrefl_550 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsStrictTotalOrder_498 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_550 = erased
-- Relation.Binary.Structures.IsApartnessRelation
d_IsApartnessRelation_556 a0 a1 a2 a3 a4 a5 = ()
data T_IsApartnessRelation_556
  = C_IsApartnessRelation'46'constructor_28351 (AgdaAny ->
                                                AgdaAny -> AgdaAny -> AgdaAny)
                                               (AgdaAny ->
                                                AgdaAny ->
                                                AgdaAny ->
                                                AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30)
-- Relation.Binary.Structures.IsApartnessRelation.irrefl
d_irrefl_566 ::
  T_IsApartnessRelation_556 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_irrefl_566 = erased
-- Relation.Binary.Structures.IsApartnessRelation.sym
d_sym_568 ::
  T_IsApartnessRelation_556 ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_sym_568 v0
  = case coe v0 of
      C_IsApartnessRelation'46'constructor_28351 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsApartnessRelation.cotrans
d_cotrans_570 ::
  T_IsApartnessRelation_556 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_cotrans_570 v0
  = case coe v0 of
      C_IsApartnessRelation'46'constructor_28351 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Structures.IsApartnessRelation._¬#_
d__'172''35'__572 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  T_IsApartnessRelation_556 -> AgdaAny -> AgdaAny -> ()
d__'172''35'__572 = erased
