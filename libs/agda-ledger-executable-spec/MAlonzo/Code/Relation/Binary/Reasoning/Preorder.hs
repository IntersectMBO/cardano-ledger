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

module MAlonzo.Code.Relation.Binary.Reasoning.Preorder where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Double
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Relation.Binary.Reasoning.Preorder._._IsRelatedTo_
d__IsRelatedTo__66 a0 a1 a2 a3 a4 a5 = ()
-- Relation.Binary.Reasoning.Preorder._._∎
d__'8718'_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
d__'8718'_68 ~v0 ~v1 ~v2 v3 = du__'8718'_68 v3
du__'8718'_68 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
du__'8718'_68 v0
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du__'8718'_234
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0))
-- Relation.Binary.Reasoning.Preorder._._≡⟨⟩_
d__'8801''10216''10217'__70 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
d__'8801''10216''10217'__70 v0 = coe v0
-- Relation.Binary.Reasoning.Preorder._.IsEquality
d_IsEquality_72 a0 a1 a2 a3 a4 a5 a6 = ()
-- Relation.Binary.Reasoning.Preorder._.IsEquality?
d_IsEquality'63'_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_IsEquality'63'_74 ~v0 ~v1 ~v2 ~v3 = du_IsEquality'63'_74
du_IsEquality'63'_74 ::
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_IsEquality'63'_74 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_IsEquality'63'_90
      v2
-- Relation.Binary.Reasoning.Preorder._.begin_
d_begin__76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  AgdaAny
d_begin__76 ~v0 ~v1 ~v2 v3 = du_begin__76 v3
du_begin__76 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  AgdaAny
du_begin__76 v0
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin__110
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0))
-- Relation.Binary.Reasoning.Preorder._.begin-equality_
d_begin'45'equality__78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  AgdaAny -> AgdaAny
d_begin'45'equality__78 ~v0 ~v1 ~v2 ~v3 = du_begin'45'equality__78
du_begin'45'equality__78 ::
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  AgdaAny -> AgdaAny
du_begin'45'equality__78 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_begin'45'equality__124
      v2
-- Relation.Binary.Reasoning.Preorder._.extractEquality
d_extractEquality_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T_IsEquality_74 ->
  AgdaAny
d_extractEquality_82 ~v0 ~v1 ~v2 ~v3 = du_extractEquality_82
du_extractEquality_82 ::
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T_IsEquality_74 ->
  AgdaAny
du_extractEquality_82 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_extractEquality_100
      v2 v3
-- Relation.Binary.Reasoning.Preorder._.step-∼
d_step'45''8764'_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
d_step'45''8764'_88 ~v0 ~v1 ~v2 v3 = du_step'45''8764'_88 v3
du_step'45''8764'_88 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
du_step'45''8764'_88 v0
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8764'_136
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0))
-- Relation.Binary.Reasoning.Preorder._.step-≈
d_step'45''8776'_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
d_step'45''8776'_90 ~v0 ~v1 ~v2 v3 = du_step'45''8776'_90 v3
du_step'45''8776'_90 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
du_step'45''8776'_90 v0
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776'_156
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0))
-- Relation.Binary.Reasoning.Preorder._.step-≈˘
d_step'45''8776''728'_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
d_step'45''8776''728'_92 ~v0 ~v1 ~v2 v3
  = du_step'45''8776''728'_92 v3
du_step'45''8776''728'_92 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
du_step'45''8776''728'_92 v0
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.du_step'45''8776''728'_176
      (coe
         MAlonzo.Code.Relation.Binary.Bundles.d_isPreorder_154 (coe v0))
-- Relation.Binary.Reasoning.Preorder._.step-≡
d_step'45''8801'_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
d_step'45''8801'_94 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 ~v8
  = du_step'45''8801'_94 v7
du_step'45''8801'_94 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
du_step'45''8801'_94 v0 = coe v0
-- Relation.Binary.Reasoning.Preorder._.step-≡˘
d_step'45''8801''728'_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
d_step'45''8801''728'_96 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 ~v8
  = du_step'45''8801''728'_96 v7
du_step'45''8801''728'_96 ::
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56 ->
  MAlonzo.Code.Relation.Binary.Reasoning.Base.Double.T__IsRelatedTo__56
du_step'45''8801''728'_96 v0 = coe v0
