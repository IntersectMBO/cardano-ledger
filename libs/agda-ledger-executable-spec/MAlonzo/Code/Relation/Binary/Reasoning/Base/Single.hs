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

module MAlonzo.Code.Relation.Binary.Reasoning.Base.Single where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive

-- Relation.Binary.Reasoning.Base.Single._IsRelatedTo_
d__IsRelatedTo__26 a0 a1 a2 a3 a4 a5 a6 a7 = ()
newtype T__IsRelatedTo__26 = C_relTo_34 AgdaAny
-- Relation.Binary.Reasoning.Base.Single.begin_
d_begin__40 :: T__IsRelatedTo__26 -> AgdaAny
d_begin__40 v0
  = case coe v0 of
      C_relTo_34 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Single.step-∼
d_step'45''8764'_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__26 -> AgdaAny -> T__IsRelatedTo__26
d_step'45''8764'_50 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8 v9 v10
  = du_step'45''8764'_50 v5 v6 v7 v8 v9 v10
du_step'45''8764'_50 ::
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> T__IsRelatedTo__26 -> AgdaAny -> T__IsRelatedTo__26
du_step'45''8764'_50 v0 v1 v2 v3 v4 v5
  = case coe v4 of
      C_relTo_34 v6 -> coe C_relTo_34 (coe v0 v1 v2 v3 v5 v6)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Binary.Reasoning.Base.Single.step-≡
d_step'45''8801'_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  T__IsRelatedTo__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T__IsRelatedTo__26
d_step'45''8801'_62 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 ~v10
  = du_step'45''8801'_62 v9
du_step'45''8801'_62 :: T__IsRelatedTo__26 -> T__IsRelatedTo__26
du_step'45''8801'_62 v0 = coe v0
-- Relation.Binary.Reasoning.Base.Single.step-≡˘
d_step'45''8801''728'_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  T__IsRelatedTo__26 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T__IsRelatedTo__26
d_step'45''8801''728'_72 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
                         ~v10
  = du_step'45''8801''728'_72 v9
du_step'45''8801''728'_72 ::
  T__IsRelatedTo__26 -> T__IsRelatedTo__26
du_step'45''8801''728'_72 v0 = coe v0
-- Relation.Binary.Reasoning.Base.Single._≡⟨⟩_
d__'8801''10216''10217'__80 ::
  T__IsRelatedTo__26 -> T__IsRelatedTo__26
d__'8801''10216''10217'__80 v0 = coe v0
-- Relation.Binary.Reasoning.Base.Single._∎
d__'8718'_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> T__IsRelatedTo__26
d__'8718'_86 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 = du__'8718'_86 v4 v6
du__'8718'_86 ::
  (AgdaAny -> AgdaAny) -> AgdaAny -> T__IsRelatedTo__26
du__'8718'_86 v0 v1 = coe C_relTo_34 (coe v0 v1)
