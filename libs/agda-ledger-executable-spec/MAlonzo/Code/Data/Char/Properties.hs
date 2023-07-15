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

module MAlonzo.Code.Data.Char.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Char
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive
import qualified MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.Properties
import qualified MAlonzo.Code.Relation.Binary.Construct.On
import qualified MAlonzo.Code.Relation.Binary.Definitions
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.Char.Properties.≈⇒≡
d_'8776''8658''8801'_6 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8776''8658''8801'_6 = erased
-- Data.Char.Properties.≉⇒≢
d_'8777''8658''8802'_8 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  (MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'8777''8658''8802'_8 = erased
-- Data.Char.Properties.≈-reflexive
d_'8776''45'reflexive_12 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8776''45'reflexive_12 = erased
-- Data.Char.Properties._≟_
d__'8799'__14 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__14 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_map'8242'_154
      erased
      (coe
         MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
         (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28 v0)
         (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28 v1))
-- Data.Char.Properties.setoid
d_setoid_20 :: MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_20
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402
-- Data.Char.Properties.decSetoid
d_decSetoid_22 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_decSetoid_22
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_decSetoid_406
      (coe d__'8799'__14)
-- Data.Char.Properties.isDecEquivalence
d_isDecEquivalence_24 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_isDecEquivalence_24
  = coe
      MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isDecEquivalence_398
      (coe d__'8799'__14)
-- Data.Char.Properties._==_
d__'61''61'__26 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 -> Bool
d__'61''61'__26 v0 v1
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.du_isYes_92
      (coe d__'8799'__14 (coe v0) (coe v1))
-- Data.Char.Properties._<?_
d__'60''63'__44 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__44
  = coe
      MAlonzo.Code.Relation.Binary.Construct.On.du_decidable_102
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28)
      (coe MAlonzo.Code.Data.Nat.Properties.d__'60''63'__2860)
-- Data.Char.Properties.<-cmp
d_'60''45'cmp_46 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Relation.Binary.Definitions.T_Tri_136
d_'60''45'cmp_46 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.d_'60''45'cmp_2828
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28 v0)
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28 v1)
-- Data.Char.Properties.<-irrefl
d_'60''45'irrefl_86 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'irrefl_86 = erased
-- Data.Char.Properties.<-trans
d_'60''45'trans_88 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18
d_'60''45'trans_88 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Construct.On.du_transitive_64
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28)
      (\ v3 v4 v5 v6 v7 ->
         coe
           MAlonzo.Code.Data.Nat.Properties.du_'60''45'trans_2810 v4 v6 v7)
      (coe v0) (coe v1) (coe v2)
-- Data.Char.Properties.<-asym
d_'60''45'asym_96 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Nat.Base.T__'8804'__18 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_'60''45'asym_96 = erased
-- Data.Char.Properties.<-isStrictPartialOrder
d_'60''45'isStrictPartialOrder_102 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder_102
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictPartialOrder'46'constructor_12363
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      d_'60''45'trans_88
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe (\ v0 v1 v2 v3 v4 -> v4)) (coe (\ v0 v1 v2 v3 v4 -> v4)))
-- Data.Char.Properties.<-isStrictTotalOrder
d_'60''45'isStrictTotalOrder_118 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_'60''45'isStrictTotalOrder_118
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsStrictTotalOrder'46'constructor_23035
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (coe d_'60''45'trans_88) (coe d_'60''45'cmp_46)
-- Data.Char.Properties.<-strictPartialOrder
d_'60''45'strictPartialOrder_126 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder_126
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictPartialOrder'46'constructor_8915
      d_'60''45'isStrictPartialOrder_102
-- Data.Char.Properties.<-strictTotalOrder
d_'60''45'strictTotalOrder_128 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_'60''45'strictTotalOrder_128
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_StrictTotalOrder'46'constructor_16593
      d_'60''45'isStrictTotalOrder_118
-- Data.Char.Properties._≤?_
d__'8804''63'__130 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''63'__130
  = coe
      MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.Properties.du_decidable_184
      (coe d_'60''45'cmp_46)
-- Data.Char.Properties.≤-reflexive
d_'8804''45'reflexive_132 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.T_ReflClosure_30
d_'8804''45'reflexive_132 ~v0 ~v1 = du_'8804''45'reflexive_132
du_'8804''45'reflexive_132 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.T_ReflClosure_30
du_'8804''45'reflexive_132 v0
  = coe
      MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.du_reflexive_72
-- Data.Char.Properties.≤-trans
d_'8804''45'trans_134 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.T_ReflClosure_30 ->
  MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.T_ReflClosure_30 ->
  MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.T_ReflClosure_30
d_'8804''45'trans_134 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.Properties.du_trans_94
      (coe d_'60''45'trans_88) (coe v0) (coe v1) (coe v2)
-- Data.Char.Properties.≤-antisym
d_'8804''45'antisym_142 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.T_ReflClosure_30 ->
  MAlonzo.Code.Relation.Binary.Construct.Closure.Reflexive.T_ReflClosure_30 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8804''45'antisym_142 = erased
-- Data.Char.Properties.≤-isPreorder
d_'8804''45'isPreorder_144 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_'8804''45'isPreorder_144
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
      (\ v0 v1 -> coe du_'8804''45'reflexive_132)
      (coe d_'8804''45'trans_134)
-- Data.Char.Properties.≤-isPartialOrder
d_'8804''45'isPartialOrder_146 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialOrder_162
d_'8804''45'isPartialOrder_146
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPartialOrder'46'constructor_8515
      (coe d_'8804''45'isPreorder_144) erased
-- Data.Char.Properties.≤-isDecPartialOrder
d_'8804''45'isDecPartialOrder_148 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecPartialOrder_206
d_'8804''45'isDecPartialOrder_148
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecPartialOrder'46'constructor_10175
      (coe d_'8804''45'isPartialOrder_146) (coe d__'8799'__14)
      (coe d__'8804''63'__130)
-- Data.Char.Properties.≤-preorder
d_'8804''45'preorder_150 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_'8804''45'preorder_150
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      d_'8804''45'isPreorder_144
-- Data.Char.Properties.≤-poset
d_'8804''45'poset_152 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Poset_282
d_'8804''45'poset_152
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Poset'46'constructor_5189
      d_'8804''45'isPartialOrder_146
-- Data.Char.Properties.≤-decPoset
d_'8804''45'decPoset_154 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecPoset_360
d_'8804''45'decPoset_154
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecPoset'46'constructor_6741
      d_'8804''45'isDecPartialOrder_148
-- Data.Char.Properties.≈-refl
d_'8776''45'refl_156 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8776''45'refl_156 = erased
-- Data.Char.Properties.≈-sym
d_'8776''45'sym_158 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8776''45'sym_158 = erased
-- Data.Char.Properties.≈-trans
d_'8776''45'trans_160 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'8776''45'trans_160 = erased
-- Data.Char.Properties.≈-subst
d_'8776''45'subst_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Agda.Builtin.Char.T_Char_6 -> ()) ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  AgdaAny -> AgdaAny
d_'8776''45'subst_164 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du_'8776''45'subst_164 v5
du_'8776''45'subst_164 :: AgdaAny -> AgdaAny
du_'8776''45'subst_164 v0 = coe v0
-- Data.Char.Properties._≈?_
d__'8776''63'__172 ::
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Agda.Builtin.Char.T_Char_6 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8776''63'__172 v0 v1
  = coe
      MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28 v0)
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28 v1)
-- Data.Char.Properties.≈-isEquivalence
d_'8776''45'isEquivalence_178 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8776''45'isEquivalence_178
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      erased erased erased
-- Data.Char.Properties.≈-setoid
d_'8776''45'setoid_180 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8776''45'setoid_180
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      d_'8776''45'isEquivalence_178
-- Data.Char.Properties.≈-isDecEquivalence
d_'8776''45'isDecEquivalence_182 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsDecEquivalence_44
d_'8776''45'isDecEquivalence_182
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsDecEquivalence'46'constructor_2293
      (coe d_'8776''45'isEquivalence_178) (coe d__'8776''63'__172)
-- Data.Char.Properties.≈-decSetoid
d_'8776''45'decSetoid_184 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_'8776''45'decSetoid_184
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_DecSetoid'46'constructor_1373
      d_'8776''45'isDecEquivalence_182
-- Data.Char.Properties.≡-setoid
d_'8801''45'setoid_186 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8801''45'setoid_186 = coe d_setoid_20
-- Data.Char.Properties.≡-decSetoid
d_'8801''45'decSetoid_188 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_DecSetoid_84
d_'8801''45'decSetoid_188 = coe d_decSetoid_22
-- Data.Char.Properties.<-isStrictPartialOrder-≈
d_'60''45'isStrictPartialOrder'45''8776'_190 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictPartialOrder_266
d_'60''45'isStrictPartialOrder'45''8776'_190
  = coe
      MAlonzo.Code.Relation.Binary.Construct.On.du_isStrictPartialOrder_356
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28)
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'60''45'isStrictPartialOrder_2876)
-- Data.Char.Properties.<-isStrictTotalOrder-≈
d_'60''45'isStrictTotalOrder'45''8776'_192 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_'60''45'isStrictTotalOrder'45''8776'_192
  = coe
      MAlonzo.Code.Relation.Binary.Construct.On.du_isStrictTotalOrder_498
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28)
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'60''45'isStrictTotalOrder_2878)
-- Data.Char.Properties.<-strictPartialOrder-≈
d_'60''45'strictPartialOrder'45''8776'_194 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictPartialOrder_472
d_'60''45'strictPartialOrder'45''8776'_194
  = coe
      MAlonzo.Code.Relation.Binary.Construct.On.du_strictPartialOrder_592
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'60''45'strictPartialOrder_2880)
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28)
-- Data.Char.Properties.<-strictTotalOrder-≈
d_'60''45'strictTotalOrder'45''8776'_196 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_StrictTotalOrder_860
d_'60''45'strictTotalOrder'45''8776'_196
  = coe
      MAlonzo.Code.Relation.Binary.Construct.On.du_strictTotalOrder_616
      (coe
         MAlonzo.Code.Data.Nat.Properties.d_'60''45'strictTotalOrder_2882)
      (coe MAlonzo.Code.Agda.Builtin.Char.d_primCharToNat_28)
