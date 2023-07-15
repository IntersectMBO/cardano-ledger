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

module MAlonzo.Code.Relation.Unary where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Relation.Unary.Pred
d_Pred_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Primitive.T_Level_14 -> ()
d_Pred_26 = erased
-- Relation.Unary.∅
d_'8709'_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> ()
d_'8709'_32 = erased
-- Relation.Unary.｛_｝
d_'65371'_'65373'_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> ()
d_'65371'_'65373'_36 = erased
-- Relation.Unary.U
d_U_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> ()
d_U_42 = erased
-- Relation.Unary._∈_
d__'8712'__46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> (AgdaAny -> ()) -> ()
d__'8712'__46 = erased
-- Relation.Unary._∉_
d__'8713'__52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> (AgdaAny -> ()) -> ()
d__'8713'__52 = erased
-- Relation.Unary._⊆_
d__'8838'__58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8838'__58 = erased
-- Relation.Unary._⊇_
d__'8839'__66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8839'__66 = erased
-- Relation.Unary._⊈_
d__'8840'__72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8840'__72 = erased
-- Relation.Unary._⊉_
d__'8841'__78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8841'__78 = erased
-- Relation.Unary._⊂_
d__'8834'__84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8834'__84 = erased
-- Relation.Unary._⊃_
d__'8835'__90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8835'__90 = erased
-- Relation.Unary._⊄_
d__'8836'__96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8836'__96 = erased
-- Relation.Unary._⊅_
d__'8837'__102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8837'__102 = erased
-- Relation.Unary._≐_
d__'8784'__108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8784'__108 = erased
-- Relation.Unary._⊆′_
d__'8838''8242'__114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8838''8242'__114 = erased
-- Relation.Unary._⊇′_
d__'8839''8242'__122 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8839''8242'__122 = erased
-- Relation.Unary._⊈′_
d__'8840''8242'__128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8840''8242'__128 = erased
-- Relation.Unary._⊉′_
d__'8841''8242'__134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8841''8242'__134 = erased
-- Relation.Unary._⊂′_
d__'8834''8242'__140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8834''8242'__140 = erased
-- Relation.Unary._⊃′_
d__'8835''8242'__146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8835''8242'__146 = erased
-- Relation.Unary._⊄′_
d__'8836''8242'__152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8836''8242'__152 = erased
-- Relation.Unary._⊅′_
d__'8837''8242'__158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8837''8242'__158 = erased
-- Relation.Unary._≐′_
d__'8784''8242'__164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8784''8242'__164 = erased
-- Relation.Unary.Empty
d_Empty_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> (AgdaAny -> ()) -> ()
d_Empty_170 = erased
-- Relation.Unary.Satisfiable
d_Satisfiable_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> (AgdaAny -> ()) -> ()
d_Satisfiable_176 = erased
-- Relation.Unary.Universal
d_Universal_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> (AgdaAny -> ()) -> ()
d_Universal_182 = erased
-- Relation.Unary.IUniversal
d_IUniversal_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> (AgdaAny -> ()) -> ()
d_IUniversal_188 = erased
-- Relation.Unary.Decidable
d_Decidable_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> (AgdaAny -> ()) -> ()
d_Decidable_194 = erased
-- Relation.Unary.⌊_⌋
d_'8970'_'8971'_202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny -> ()
d_'8970'_'8971'_202 = erased
-- Relation.Unary.Irrelevant
d_Irrelevant_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> (AgdaAny -> ()) -> ()
d_Irrelevant_208 = erased
-- Relation.Unary.Recomputable
d_Recomputable_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> (AgdaAny -> ()) -> ()
d_Recomputable_218 = erased
-- Relation.Unary.∁
d_'8705'_224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> AgdaAny -> ()
d_'8705'_224 = erased
-- Relation.Unary._⇒_
d__'8658'__230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> AgdaAny -> ()
d__'8658'__230 = erased
-- Relation.Unary._∪_
d__'8746'__238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> AgdaAny -> ()
d__'8746'__238 = erased
-- Relation.Unary._∩_
d__'8745'__246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> AgdaAny -> ()
d__'8745'__246 = erased
-- Relation.Unary._∖_
d__'8726'__254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> AgdaAny -> ()
d__'8726'__254 = erased
-- Relation.Unary.⋃
d_'8899'_266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> ()) -> AgdaAny -> ()
d_'8899'_266 = erased
-- Relation.Unary.⋂
d_'8898'_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny -> ()) -> AgdaAny -> ()
d_'8898'_280 = erased
-- Relation.Unary._≬_
d__'8812'__290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> ()
d__'8812'__290 = erased
-- Relation.Unary._⊢_
d__'8866'__298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> ()) -> AgdaAny -> ()
d__'8866'__298 = erased
-- Relation.Unary._⟨×⟩_
d__'10216''215''10217'__306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d__'10216''215''10217'__306 = erased
-- Relation.Unary._⟨⊎⟩_
d__'10216''8846''10217'__316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> ()
d__'10216''8846''10217'__316 = erased
-- Relation.Unary._⟨⊙⟩_
d__'10216''8857''10217'__322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d__'10216''8857''10217'__322 = erased
-- Relation.Unary._⟨→⟩_
d__'10216''8594''10217'__332 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) -> (AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> ()
d__'10216''8594''10217'__332 = erased
-- Relation.Unary._⟨·⟩_
d__'10216''183''10217'__344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d__'10216''183''10217'__344 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du__'10216''183''10217'__344 v8 v9
du__'10216''183''10217'__344 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du__'10216''183''10217'__344 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3
        -> coe v3 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0)) v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Unary._~
d__'126'_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d__'126'_354 = erased
-- Relation.Unary._⟨∘⟩_
d__'10216''8728''10217'__358 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d__'10216''8728''10217'__358 = erased
-- Relation.Unary._//_
d__'47''47'__370 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d__'47''47'__370 = erased
-- Relation.Unary._\\_
d__'92''92'__384 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d__'92''92'__384 = erased
