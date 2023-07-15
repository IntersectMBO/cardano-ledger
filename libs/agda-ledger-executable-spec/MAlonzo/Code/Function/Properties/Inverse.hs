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

module MAlonzo.Code.Function.Properties.Inverse where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Consequences
import qualified MAlonzo.Code.Function.Construct.Composition
import qualified MAlonzo.Code.Function.Construct.Identity
import qualified MAlonzo.Code.Function.Construct.Symmetry
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Properties.Inverse.isEquivalence
d_isEquivalence_28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_isEquivalence_28 ~v0 ~v1 = du_isEquivalence_28
du_isEquivalence_28 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_isEquivalence_28
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Function.Construct.Identity.du_inverse_738 (coe v0)))
      (\ v0 v1 v2 ->
         coe MAlonzo.Code.Function.Construct.Symmetry.du_inverse_910 v2)
      (\ v0 v1 v2 v3 v4 ->
         coe
           MAlonzo.Code.Function.Construct.Composition.du_inverse_1986 v0 v2
           v3 v4)
-- Function.Properties.Inverse.↔-isEquivalence
d_'8596''45'isEquivalence_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_'8596''45'isEquivalence_32 ~v0 = du_'8596''45'isEquivalence_32
du_'8596''45'isEquivalence_32 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_'8596''45'isEquivalence_32
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Function.Construct.Identity.du_inverse_738
              (coe
                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)))
      (coe
         (\ v0 v1 ->
            coe MAlonzo.Code.Function.Construct.Symmetry.du_inverse_910))
      (coe
         (\ v0 v1 v2 ->
            coe
              MAlonzo.Code.Function.Construct.Composition.du_inverse_1986
              (coe
                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
              (coe
                 MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)))
-- Function.Properties.Inverse.↔.isPartialEquivalence
d_isPartialEquivalence_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
d_isPartialEquivalence_40 ~v0 = du_isPartialEquivalence_40
du_isPartialEquivalence_40 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsPartialEquivalence_16
du_isPartialEquivalence_40
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_isPartialEquivalence_42
      (coe du_'8596''45'isEquivalence_32)
-- Function.Properties.Inverse.↔.refl
d_refl_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_refl_42 ~v0 = du_refl_42
du_refl_42 :: () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_refl_42
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe du_'8596''45'isEquivalence_32)
-- Function.Properties.Inverse.↔.reflexive
d_reflexive_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_reflexive_44 ~v0 = du_reflexive_44
du_reflexive_44 ::
  () ->
  () ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_reflexive_44 v0 v1 v2
  = coe
      MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40
      (coe du_'8596''45'isEquivalence_32) v0
-- Function.Properties.Inverse.↔.sym
d_sym_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_sym_46 ~v0 = du_sym_46
du_sym_46 ::
  () ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_sym_46
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (coe du_'8596''45'isEquivalence_32)
-- Function.Properties.Inverse.↔.trans
d_trans_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_trans_48 ~v0 = du_trans_48
du_trans_48 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_trans_48
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_trans_38
      (coe du_'8596''45'isEquivalence_32)
-- Function.Properties.Inverse.Inverse⇒Injection
d_Inverse'8658'Injection_50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704
d_Inverse'8658'Injection_50 ~v0 ~v1 v2 ~v3 ~v4 ~v5 v6
  = du_Inverse'8658'Injection_50 v2 v6
du_Inverse'8658'Injection_50 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704
du_Inverse'8658'Injection_50 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.C_Injection'46'constructor_7999
      (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v1))
      (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_1068 (coe v1))
      (coe
         MAlonzo.Code.Function.Consequences.du_inverse'691''8658'injective_74
         (coe v0) (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_from_1066 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1070 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.du_inverse'691'_1076 (coe v1)))
-- Function.Properties.Inverse.Inverse⇒Bijection
d_Inverse'8658'Bijection_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_Inverse'8658'Bijection_134 ~v0 ~v1 v2 ~v3 ~v4 ~v5 v6
  = du_Inverse'8658'Bijection_134 v2 v6
du_Inverse'8658'Bijection_134 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_Inverse'8658'Bijection_134 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.C_Bijection'46'constructor_13257
      (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v1))
      (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_1068 (coe v1))
      (coe
         MAlonzo.Code.Function.Consequences.du_inverse'7495''8658'bijective_122
         (coe v0) (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_from_1066 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1070 (coe v1))
         (coe MAlonzo.Code.Function.Bundles.d_inverse_1072 (coe v1)))
-- Function.Properties.Inverse.Inverse⇒Equivalence
d_Inverse'8658'Equivalence_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_Inverse'8658'Equivalence_218 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_Inverse'8658'Equivalence_218 v6
du_Inverse'8658'Equivalence_218 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_Inverse'8658'Equivalence_218 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_Equivalence'46'constructor_17233
      (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_from_1066 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_to'45'cong_1068 (coe v0))
      (coe MAlonzo.Code.Function.Bundles.d_from'45'cong_1070 (coe v0))
-- Function.Properties.Inverse.↔⇒↣
d_'8596''8658''8611'_300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704
d_'8596''8658''8611'_300 ~v0 ~v1 ~v2 ~v3
  = du_'8596''8658''8611'_300
du_'8596''8658''8611'_300 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704
du_'8596''8658''8611'_300
  = coe
      du_Inverse'8658'Injection_50
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Properties.Inverse.↔⇒⤖
d_'8596''8658''10518'_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_'8596''8658''10518'_302 ~v0 ~v1 ~v2 ~v3
  = du_'8596''8658''10518'_302
du_'8596''8658''10518'_302 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_'8596''8658''10518'_302
  = coe
      du_Inverse'8658'Bijection_134
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Properties.Inverse.↔⇒⇔
d_'8596''8658''8660'_304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8596''8658''8660'_304 ~v0 ~v1 ~v2 ~v3
  = du_'8596''8658''8660'_304
du_'8596''8658''8660'_304 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8596''8658''8660'_304 = coe du_Inverse'8658'Equivalence_218
-- Function.Properties.Inverse._.↔-fun
d_'8596''45'fun_316 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
   MAlonzo.Code.Agda.Primitive.T_Level_14 ->
   () ->
   (AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'8596''45'fun_316 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du_'8596''45'fun_316 v9 v10
du_'8596''45'fun_316 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'8596''45'fun_316 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe
         (\ v2 v3 ->
            coe
              MAlonzo.Code.Function.Bundles.d_to_1064 v1
              (coe v2 (coe MAlonzo.Code.Function.Bundles.d_from_1066 v0 v3))))
      (coe
         (\ v2 v3 ->
            coe
              MAlonzo.Code.Function.Bundles.d_from_1066 v1
              (coe v2 (coe MAlonzo.Code.Function.Bundles.d_to_1064 v0 v3))))
      erased erased
