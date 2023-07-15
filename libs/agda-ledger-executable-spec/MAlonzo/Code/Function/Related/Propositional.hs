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

module MAlonzo.Code.Function.Related.Propositional where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Construct.Composition
import qualified MAlonzo.Code.Function.Construct.Identity
import qualified MAlonzo.Code.Function.Construct.Symmetry
import qualified MAlonzo.Code.Function.Properties.Bijection
import qualified MAlonzo.Code.Function.Properties.Inverse
import qualified MAlonzo.Code.Function.Properties.RightInverse
import qualified MAlonzo.Code.Function.Properties.Surjection
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Related.Propositional.Kind
d_Kind_6 = ()
data T_Kind_6
  = C_implication_8 | C_reverseImplication_10 | C_equivalence_12 |
    C_injection_14 | C_reverseInjection_16 | C_leftInverse_18 |
    C_surjection_20 | C_bijection_22
-- Function.Related.Propositional._∼[_]_
d__'8764''91'_'93'__40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Kind_6 -> () -> ()
d__'8764''91'_'93'__40 = erased
-- Function.Related.Propositional.Related
d_Related_74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Kind_6 -> () -> () -> ()
d_Related_74 = erased
-- Function.Related.Propositional.⤖⇒
d_'10518''8658'_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Kind_6 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 -> AgdaAny
d_'10518''8658'_82 ~v0 ~v1 ~v2 ~v3 v4 = du_'10518''8658'_82 v4
du_'10518''8658'_82 ::
  T_Kind_6 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 -> AgdaAny
du_'10518''8658'_82 v0
  = case coe v0 of
      C_implication_8
        -> coe
             (\ v1 ->
                coe
                  MAlonzo.Code.Function.Bundles.du_mk'10230'_1290
                  (coe MAlonzo.Code.Function.Bundles.d_to_852 (coe v1)))
      C_reverseImplication_10
        -> coe
             (\ v1 ->
                coe
                  MAlonzo.Code.Function.Bundles.du_mk'10230'_1290
                  (coe
                     MAlonzo.Code.Function.Bundles.d_from_1066
                     (coe
                        MAlonzo.Code.Function.Properties.Bijection.du_'10518''8658''8596'_154
                        v1)))
      C_equivalence_12
        -> coe
             MAlonzo.Code.Function.Properties.Bijection.du_'10518''8658''8660'_156
      C_injection_14
        -> coe MAlonzo.Code.Function.Bundles.du_injection_862
      C_reverseInjection_16
        -> coe
             (\ v1 ->
                coe
                  MAlonzo.Code.Function.Bundles.du_injection_862
                  (coe
                     MAlonzo.Code.Function.Properties.Inverse.du_'8596''8658''10518'_302
                     (coe
                        MAlonzo.Code.Function.Construct.Symmetry.du_inverse_910
                        (coe
                           MAlonzo.Code.Function.Properties.Bijection.du_'10518''8658''8596'_154
                           v1))))
      C_leftInverse_18
        -> coe
             (\ v1 ->
                coe
                  MAlonzo.Code.Function.Bundles.du_rightInverse_1080
                  (coe
                     MAlonzo.Code.Function.Properties.Bijection.du_'10518''8658''8596'_154
                     v1))
      C_surjection_20
        -> coe MAlonzo.Code.Function.Bundles.du_surjection_864
      C_bijection_22 -> coe (\ v1 -> v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.≡⇒
d_'8801''8658'_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T_Kind_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_'8801''8658'_84 ~v0 ~v1 ~v2 v3 ~v4 = du_'8801''8658'_84 v3
du_'8801''8658'_84 :: T_Kind_6 -> AgdaAny
du_'8801''8658'_84 v0
  = coe
      du_'10518''8658'_82 v0
      (coe MAlonzo.Code.Function.Construct.Identity.du_'10518''45'id_754)
-- Function.Related.Propositional.SymmetricKind
d_SymmetricKind_86 = ()
data T_SymmetricKind_86 = C_equivalence_88 | C_bijection_90
-- Function.Related.Propositional.⌊_⌋
d_'8970'_'8971'_92 :: T_SymmetricKind_86 -> T_Kind_6
d_'8970'_'8971'_92 v0
  = case coe v0 of
      C_equivalence_88 -> coe C_equivalence_12
      C_bijection_90 -> coe C_bijection_22
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.ForwardKind
d_ForwardKind_94 = ()
data T_ForwardKind_94
  = C_implication_96 | C_equivalence_98 | C_injection_100 |
    C_leftInverse_102 | C_surjection_104 | C_bijection_106
-- Function.Related.Propositional.⌊_⌋→
d_'8970'_'8971''8594'_108 :: T_ForwardKind_94 -> T_Kind_6
d_'8970'_'8971''8594'_108 v0
  = case coe v0 of
      C_implication_96 -> coe C_implication_8
      C_equivalence_98 -> coe C_equivalence_12
      C_injection_100 -> coe C_injection_14
      C_leftInverse_102 -> coe C_leftInverse_18
      C_surjection_104 -> coe C_surjection_20
      C_bijection_106 -> coe C_bijection_22
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.⇒→
d_'8658''8594'_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_ForwardKind_94 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8658''8594'_112 ~v0 ~v1 ~v2 ~v3 v4 = du_'8658''8594'_112 v4
du_'8658''8594'_112 ::
  T_ForwardKind_94 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8658''8594'_112 v0
  = case coe v0 of
      C_implication_96
        -> coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_to_648 (coe v1))
      C_equivalence_98
        -> coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_to_938 (coe v1))
      C_injection_100
        -> coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_to_712 (coe v1))
      C_leftInverse_102
        -> coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_to_1036 (coe v1))
      C_surjection_104
        -> coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_to_782 (coe v1))
      C_bijection_106
        -> coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_to_852 (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.BackwardKind
d_BackwardKind_114 = ()
data T_BackwardKind_114
  = C_reverseImplication_116 | C_equivalence_118 |
    C_reverseInjection_120 | C_leftInverse_122 | C_surjection_124 |
    C_bijection_126
-- Function.Related.Propositional.⌊_⌋←
d_'8970'_'8971''8592'_128 :: T_BackwardKind_114 -> T_Kind_6
d_'8970'_'8971''8592'_128 v0
  = case coe v0 of
      C_reverseImplication_116 -> coe C_reverseImplication_10
      C_equivalence_118 -> coe C_equivalence_12
      C_reverseInjection_120 -> coe C_reverseInjection_16
      C_leftInverse_122 -> coe C_leftInverse_18
      C_surjection_124 -> coe C_surjection_20
      C_bijection_126 -> coe C_bijection_22
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.⇒←
d_'8658''8592'_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_BackwardKind_114 -> AgdaAny -> AgdaAny -> AgdaAny
d_'8658''8592'_132 ~v0 ~v1 ~v2 ~v3 v4 = du_'8658''8592'_132 v4
du_'8658''8592'_132 ::
  T_BackwardKind_114 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8658''8592'_132 v0
  = case coe v0 of
      C_reverseImplication_116
        -> coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_to_648 (coe v1))
      C_equivalence_118
        -> coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_from_940 (coe v1))
      C_reverseInjection_120
        -> coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_to_712 (coe v1))
      C_leftInverse_122
        -> coe (\ v1 -> MAlonzo.Code.Function.Bundles.d_from_1038 (coe v1))
      C_surjection_124
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Bundles.d_to_1036
                  (coe
                     MAlonzo.Code.Function.Properties.Surjection.du_'8608''8658''8618'_14
                     (coe v1)))
      C_bijection_126
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Bundles.d_from_1066
                  (coe
                     MAlonzo.Code.Function.Properties.Bijection.du_'10518''8658''8596'_154
                     v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.EquivalenceKind
d_EquivalenceKind_134 = ()
data T_EquivalenceKind_134
  = C_equivalence_136 | C_leftInverse_138 | C_surjection_140 |
    C_bijection_142
-- Function.Related.Propositional.⌊_⌋⇔
d_'8970'_'8971''8660'_144 :: T_EquivalenceKind_134 -> T_Kind_6
d_'8970'_'8971''8660'_144 v0
  = case coe v0 of
      C_equivalence_136 -> coe C_equivalence_12
      C_leftInverse_138 -> coe C_leftInverse_18
      C_surjection_140 -> coe C_surjection_20
      C_bijection_142 -> coe C_bijection_22
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.⇒⇔
d_'8658''8660'_148 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_EquivalenceKind_134 ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8658''8660'_148 ~v0 ~v1 ~v2 ~v3 v4 = du_'8658''8660'_148 v4
du_'8658''8660'_148 ::
  T_EquivalenceKind_134 ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8658''8660'_148 v0
  = case coe v0 of
      C_equivalence_136 -> coe (\ v1 -> v1)
      C_leftInverse_138
        -> coe MAlonzo.Code.Function.Bundles.du_equivalence_1050
      C_surjection_140
        -> coe
             MAlonzo.Code.Function.Properties.Surjection.du_'8608''8658''8660'_84
      C_bijection_142
        -> coe
             MAlonzo.Code.Function.Properties.Bijection.du_'10518''8658''8660'_156
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.⇔⌊_⌋
d_'8660''8970'_'8971'_150 ::
  T_SymmetricKind_86 -> T_EquivalenceKind_134
d_'8660''8970'_'8971'_150 v0
  = case coe v0 of
      C_equivalence_88 -> coe C_equivalence_136
      C_bijection_90 -> coe C_bijection_142
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.→⌊_⌋
d_'8594''8970'_'8971'_152 ::
  T_EquivalenceKind_134 -> T_ForwardKind_94
d_'8594''8970'_'8971'_152 v0
  = case coe v0 of
      C_equivalence_136 -> coe C_equivalence_98
      C_leftInverse_138 -> coe C_leftInverse_102
      C_surjection_140 -> coe C_surjection_104
      C_bijection_142 -> coe C_bijection_106
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.←⌊_⌋
d_'8592''8970'_'8971'_154 ::
  T_EquivalenceKind_134 -> T_BackwardKind_114
d_'8592''8970'_'8971'_154 v0
  = case coe v0 of
      C_equivalence_136 -> coe C_equivalence_118
      C_leftInverse_138 -> coe C_leftInverse_122
      C_surjection_140 -> coe C_surjection_124
      C_bijection_142 -> coe C_bijection_126
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional._op
d__op_156 :: T_Kind_6 -> T_Kind_6
d__op_156 v0
  = case coe v0 of
      C_implication_8 -> coe C_reverseImplication_10
      C_reverseImplication_10 -> coe C_implication_8
      C_equivalence_12 -> coe v0
      C_injection_14 -> coe C_reverseInjection_16
      C_reverseInjection_16 -> coe C_injection_14
      C_leftInverse_18 -> coe C_surjection_20
      C_surjection_20 -> coe C_leftInverse_18
      C_bijection_22 -> coe v0
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.reverse
d_reverse_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_reverse_158 ~v0 ~v1 v2 ~v3 ~v4 = du_reverse_158 v2
du_reverse_158 :: T_Kind_6 -> AgdaAny -> AgdaAny
du_reverse_158 v0
  = case coe v0 of
      C_implication_8 -> coe (\ v1 -> v1)
      C_reverseImplication_10 -> coe (\ v1 -> v1)
      C_equivalence_12
        -> coe
             MAlonzo.Code.Function.Construct.Symmetry.du_'8660''45'sym_996
      C_injection_14 -> coe (\ v1 -> v1)
      C_reverseInjection_16 -> coe (\ v1 -> v1)
      C_leftInverse_18
        -> coe
             MAlonzo.Code.Function.Properties.RightInverse.du_'8618''8658''8608'_50
      C_surjection_20
        -> coe
             MAlonzo.Code.Function.Properties.Surjection.du_'8608''8658''8618'_14
      C_bijection_22
        -> coe
             (\ v1 ->
                coe
                  MAlonzo.Code.Function.Properties.Inverse.du_'8596''8658''10518'_302
                  (coe
                     MAlonzo.Code.Function.Construct.Symmetry.du_'8596''45'sym_1002
                     (coe
                        MAlonzo.Code.Function.Properties.Bijection.du_'10518''8658''8596'_154
                        v1)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.K-refl
d_K'45'refl_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Kind_6 -> () -> AgdaAny
d_K'45'refl_160 ~v0 v1 ~v2 = du_K'45'refl_160 v1
du_K'45'refl_160 :: T_Kind_6 -> AgdaAny
du_K'45'refl_160 v0
  = case coe v0 of
      C_implication_8
        -> coe
             MAlonzo.Code.Function.Construct.Identity.du_'10230''45'id_748
      C_reverseImplication_10
        -> coe
             MAlonzo.Code.Function.Construct.Identity.du_'10230''45'id_748
      C_equivalence_12
        -> coe MAlonzo.Code.Function.Construct.Identity.du_'8660''45'id_756
      C_injection_14
        -> coe MAlonzo.Code.Function.Construct.Identity.du_'8611''45'id_750
      C_reverseInjection_16
        -> coe MAlonzo.Code.Function.Construct.Identity.du_'8611''45'id_750
      C_leftInverse_18
        -> coe MAlonzo.Code.Function.Construct.Identity.du_'8618''45'id_760
      C_surjection_20
        -> coe MAlonzo.Code.Function.Construct.Identity.du_'8608''45'id_752
      C_bijection_22
        -> coe
             MAlonzo.Code.Function.Construct.Identity.du_'10518''45'id_754
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.K-reflexive
d_K'45'reflexive_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Kind_6 ->
  () ->
  () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_K'45'reflexive_162 ~v0 v1 ~v2 ~v3 ~v4 = du_K'45'reflexive_162 v1
du_K'45'reflexive_162 :: T_Kind_6 -> AgdaAny
du_K'45'reflexive_162 v0 = coe du_K'45'refl_160 (coe v0)
-- Function.Related.Propositional.K-trans
d_K'45'trans_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d_K'45'trans_164 ~v0 ~v1 v2 ~v3 ~v4 ~v5 ~v6 = du_K'45'trans_164 v2
du_K'45'trans_164 :: T_Kind_6 -> AgdaAny -> AgdaAny -> AgdaAny
du_K'45'trans_164 v0
  = case coe v0 of
      C_implication_8
        -> coe
             MAlonzo.Code.Function.Construct.Composition.du__'10230''45''8728'__2144
      C_reverseImplication_10
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Function.Construct.Composition.du__'10230''45''8728'__2144
                  v2 v1)
      C_equivalence_12
        -> coe
             MAlonzo.Code.Function.Construct.Composition.du__'8660''45''8728'__2152
      C_injection_14
        -> coe
             MAlonzo.Code.Function.Construct.Composition.du__'8611''45''8728'__2146
      C_reverseInjection_16
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Function.Construct.Composition.du__'8611''45''8728'__2146
                  v2 v1)
      C_leftInverse_18
        -> coe
             MAlonzo.Code.Function.Construct.Composition.du__'8618''45''8728'__2156
      C_surjection_20
        -> coe
             MAlonzo.Code.Function.Construct.Composition.du__'8608''45''8728'__2148
      C_bijection_22
        -> coe
             MAlonzo.Code.Function.Construct.Composition.du__'10518''45''8728'__2150
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.SK-sym
d_SK'45'sym_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SymmetricKind_86 -> () -> () -> AgdaAny -> AgdaAny
d_SK'45'sym_168 ~v0 ~v1 v2 ~v3 ~v4 = du_SK'45'sym_168 v2
du_SK'45'sym_168 :: T_SymmetricKind_86 -> AgdaAny -> AgdaAny
du_SK'45'sym_168 v0
  = case coe v0 of
      C_equivalence_88 -> coe du_reverse_158 (coe C_equivalence_12)
      C_bijection_90 -> coe du_reverse_158 (coe C_bijection_22)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Propositional.SK-isEquivalence
d_SK'45'isEquivalence_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SymmetricKind_86 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_SK'45'isEquivalence_172 ~v0 v1 = du_SK'45'isEquivalence_172 v1
du_SK'45'isEquivalence_172 ::
  T_SymmetricKind_86 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_SK'45'isEquivalence_172 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (\ v1 -> coe du_K'45'refl_160 (coe d_'8970'_'8971'_92 (coe v0)))
      (\ v1 v2 -> coe du_SK'45'sym_168 (coe v0))
      (\ v1 v2 v3 ->
         coe du_K'45'trans_164 (coe d_'8970'_'8971'_92 (coe v0)))
-- Function.Related.Propositional.SK-setoid
d_SK'45'setoid_178 ::
  T_SymmetricKind_86 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_SK'45'setoid_178 v0 ~v1 = du_SK'45'setoid_178 v0
du_SK'45'setoid_178 ::
  T_SymmetricKind_86 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_SK'45'setoid_178 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe du_SK'45'isEquivalence_172 (coe v0))
-- Function.Related.Propositional.K-isPreorder
d_K'45'isPreorder_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Kind_6 -> MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_K'45'isPreorder_186 ~v0 v1 = du_K'45'isPreorder_186 v1
du_K'45'isPreorder_186 ::
  T_Kind_6 -> MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_K'45'isPreorder_186 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe du_SK'45'isEquivalence_172 (coe C_bijection_90))
      (coe (\ v1 v2 -> coe du_'10518''8658'_82 (coe v0)))
      (\ v1 v2 v3 -> coe du_K'45'trans_164 (coe v0))
-- Function.Related.Propositional.K-preorder
d_K'45'preorder_192 ::
  T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_K'45'preorder_192 v0 ~v1 = du_K'45'preorder_192 v0
du_K'45'preorder_192 ::
  T_Kind_6 -> MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_K'45'preorder_192 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe du_K'45'isPreorder_186 (coe v0))
-- Function.Related.Propositional.EquationalReasoning._∼⟨_⟩_
d__'8764''10216'_'10217'__202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8764''10216'_'10217'__202 ~v0 v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8
  = du__'8764''10216'_'10217'__202 v1 v7 v8
du__'8764''10216'_'10217'__202 ::
  T_Kind_6 -> AgdaAny -> AgdaAny -> AgdaAny
du__'8764''10216'_'10217'__202 v0 v1 v2
  = coe du_K'45'trans_164 v0 v1 v2
-- Function.Related.Propositional.EquationalReasoning._⤖⟨_⟩_
d__'10518''10216'_'10217'__210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 -> AgdaAny -> AgdaAny
d__'10518''10216'_'10217'__210 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7 v8
  = du__'10518''10216'_'10217'__210 v3 v7 v8
du__'10518''10216'_'10217'__210 ::
  T_Kind_6 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844 -> AgdaAny -> AgdaAny
du__'10518''10216'_'10217'__210 v0 v1 v2
  = coe
      du__'8764''10216'_'10217'__202 (coe v0)
      (coe du_'10518''8658'_82 v0 v1) (coe v2)
-- Function.Related.Propositional.EquationalReasoning._↔⟨_⟩_
d__'8596''10216'_'10217'__220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 -> AgdaAny -> AgdaAny
d__'8596''10216'_'10217'__220 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7 v8
  = du__'8596''10216'_'10217'__220 v3 v7 v8
du__'8596''10216'_'10217'__220 ::
  T_Kind_6 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 -> AgdaAny -> AgdaAny
du__'8596''10216'_'10217'__220 v0 v1 v2
  = coe
      du__'8764''10216'_'10217'__202 (coe v0)
      (coe
         du_'10518''8658'_82 v0
         (coe
            MAlonzo.Code.Function.Properties.Inverse.du_'8596''8658''10518'_302
            v1))
      (coe v2)
-- Function.Related.Propositional.EquationalReasoning._↔⟨⟩_
d__'8596''10216''10217'__230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> AgdaAny -> AgdaAny
d__'8596''10216''10217'__230 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du__'8596''10216''10217'__230 v5
du__'8596''10216''10217'__230 :: AgdaAny -> AgdaAny
du__'8596''10216''10217'__230 v0 = coe v0
-- Function.Related.Propositional.EquationalReasoning._≡⟨_⟩_
d__'8801''10216'_'10217'__238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  AgdaAny -> AgdaAny
d__'8801''10216'_'10217'__238 ~v0 ~v1 v2 ~v3 ~v4 ~v5 ~v6 v7
  = du__'8801''10216'_'10217'__238 v2 v7
du__'8801''10216'_'10217'__238 :: T_Kind_6 -> AgdaAny -> AgdaAny
du__'8801''10216'_'10217'__238 v0 v1
  = coe
      du__'8764''10216'_'10217'__202 (coe v0)
      (coe du_'8801''8658'_84 (coe v0)) (coe v1)
-- Function.Related.Propositional.EquationalReasoning._∎
d__'8718'_248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Kind_6 -> () -> AgdaAny
d__'8718'_248 ~v0 v1 ~v2 = du__'8718'_248 v1
du__'8718'_248 :: T_Kind_6 -> AgdaAny
du__'8718'_248 v0 = coe du_K'45'refl_160 (coe v0)
-- Function.Related.Propositional.InducedRelation₁
d_InducedRelation'8321'_254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Kind_6 -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> ()
d_InducedRelation'8321'_254 = erased
-- Function.Related.Propositional.InducedPreorder₁
d_InducedPreorder'8321'_266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Kind_6 ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_InducedPreorder'8321'_266 ~v0 ~v1 ~v2 v3 ~v4
  = du_InducedPreorder'8321'_266 v3
du_InducedPreorder'8321'_266 ::
  T_Kind_6 -> MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_InducedPreorder'8321'_266 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
         (coe
            (\ v1 v2 v3 ->
               coe
                 du_'10518''8658'_82 v0
                 (coe du_K'45'reflexive_162 (coe C_bijection_22))))
         (coe (\ v1 v2 v3 -> coe du_K'45'trans_164 (coe v0))))
-- Function.Related.Propositional.InducedEquivalence₁
d_InducedEquivalence'8321'_326 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_SymmetricKind_86 ->
  (AgdaAny -> ()) -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_InducedEquivalence'8321'_326 ~v0 ~v1 ~v2 v3 ~v4
  = du_InducedEquivalence'8321'_326 v3
du_InducedEquivalence'8321'_326 ::
  T_SymmetricKind_86 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_InducedEquivalence'8321'_326 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
         (coe
            (\ v1 -> coe du_K'45'refl_160 (coe d_'8970'_'8971'_92 (coe v0))))
         (coe (\ v1 v2 -> coe du_SK'45'sym_168 (coe v0)))
         (coe
            (\ v1 v2 v3 ->
               coe du_K'45'trans_164 (coe d_'8970'_'8971'_92 (coe v0)))))
-- Function.Related.Propositional.InducedRelation₂
d_InducedRelation'8322'_334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) -> AgdaAny -> AgdaAny -> ()
d_InducedRelation'8322'_334 = erased
-- Function.Related.Propositional.InducedPreorder₂
d_InducedPreorder'8322'_348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_InducedPreorder'8322'_348 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6
  = du_InducedPreorder'8322'_348 v4
du_InducedPreorder'8322'_348 ::
  T_Kind_6 -> MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_InducedPreorder'8322'_348 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
         (coe
            (\ v1 v2 v3 v4 ->
               coe
                 du_'10518''8658'_82 v0
                 (coe du_K'45'reflexive_162 (coe C_bijection_22))))
         (coe
            (\ v1 v2 v3 v4 v5 v6 ->
               coe du_K'45'trans_164 v0 (coe v4 v6) (coe v5 v6))))
-- Function.Related.Propositional.InducedEquivalence₂
d_InducedEquivalence'8322'_416 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_SymmetricKind_86 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_InducedEquivalence'8322'_416 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6
  = du_InducedEquivalence'8322'_416 v4
du_InducedEquivalence'8322'_416 ::
  T_SymmetricKind_86 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_InducedEquivalence'8322'_416 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
         (coe
            (\ v1 v2 ->
               coe du_K'45'refl_160 (coe d_'8970'_'8971'_92 (coe v0))))
         (coe (\ v1 v2 v3 v4 -> coe du_SK'45'sym_168 v0 (coe v3 v4)))
         (coe
            (\ v1 v2 v3 v4 v5 v6 ->
               coe
                 du_K'45'trans_164 (d_'8970'_'8971'_92 (coe v0)) (coe v4 v6)
                 (coe v5 v6))))
