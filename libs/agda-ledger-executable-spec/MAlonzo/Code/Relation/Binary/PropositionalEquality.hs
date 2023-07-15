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

module MAlonzo.Code.Relation.Binary.PropositionalEquality where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Function.Equality
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles
import qualified MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Construct.Trivial
import qualified MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Relation.Binary.PropositionalEquality._→-setoid_
d__'8594''45'setoid__26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d__'8594''45'setoid__26 ~v0 ~v1 ~v2 ~v3 = du__'8594''45'setoid__26
du__'8594''45'setoid__26 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du__'8594''45'setoid__26
  = coe
      MAlonzo.Code.Function.Equality.du_'8801''45'setoid_206
      (coe
         MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Construct.Trivial.du_indexedSetoid_100
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402))
-- Relation.Binary.PropositionalEquality._≗_
d__'8791'__36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d__'8791'__36 = erased
-- Relation.Binary.PropositionalEquality.:→-to-Π
d_'58''8594''45'to'45'Π_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.T_IndexedSetoid_18 ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Function.Equality.T_Π_16
d_'58''8594''45'to'45'Π_48 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_'58''8594''45'to'45'Π_48 v4 v5
du_'58''8594''45'to'45'Π_48 ::
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.T_IndexedSetoid_18 ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Function.Equality.T_Π_16
du_'58''8594''45'to'45'Π_48 v0 v1
  = coe
      MAlonzo.Code.Function.Equality.C_Π'46'constructor_1167 (coe v1)
      (coe
         (\ v2 v3 v4 ->
            coe
              MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.d_refl_30
              (MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.d_isEquivalence_38
                 (coe v0))
              v2 (coe v1 v2)))
-- Relation.Binary.PropositionalEquality.→-to-⟶
d_'8594''45'to'45''10230'_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Function.Equality.T_Π_16
d_'8594''45'to'45''10230'_68 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8594''45'to'45''10230'_68 v4
du_'8594''45'to'45''10230'_68 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Function.Equality.T_Π_16
du_'8594''45'to'45''10230'_68 v0
  = coe
      du_'58''8594''45'to'45'Π_48
      (coe
         MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.C_IndexedSetoid'46'constructor_441
         (coe
            MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Construct.Trivial.du_isIndexedEquivalence_32
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))))
-- Relation.Binary.PropositionalEquality.Reveal_·_is_
d_Reveal_'183'_is__86 a0 a1 a2 a3 a4 a5 a6 = ()
data T_Reveal_'183'_is__86 = C_'91'_'93'_102
-- Relation.Binary.PropositionalEquality.Reveal_·_is_.eq
d_eq_100 ::
  T_Reveal_'183'_is__86 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_eq_100 = erased
-- Relation.Binary.PropositionalEquality.inspect
d_inspect_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> T_Reveal_'183'_is__86
d_inspect_114 = erased
-- Relation.Binary.PropositionalEquality.isPropositional
d_isPropositional_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()
d_isPropositional_120 = erased
-- Relation.Binary.PropositionalEquality.naturality
d_naturality_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_naturality_142 = erased
-- Relation.Binary.PropositionalEquality.cong-≡id
d_cong'45''8801'id_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cong'45''8801'id_160 = erased
-- Relation.Binary.PropositionalEquality._.fx≡x
d_fx'8801'x_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fx'8801'x_172 = erased
-- Relation.Binary.PropositionalEquality._.f²x≡x
d_f'178'x'8801'x_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_f'178'x'8801'x_174 = erased
-- Relation.Binary.PropositionalEquality._.≡-≟-identity
d_'8801''45''8799''45'identity_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8801''45''8799''45'identity_194 = erased
-- Relation.Binary.PropositionalEquality._.≢-≟-identity
d_'8802''45''8799''45'identity_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  AgdaAny ->
  AgdaAny ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8802''45''8799''45'identity_200 = erased
