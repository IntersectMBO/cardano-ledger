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

module MAlonzo.Code.Function.LeftInverse where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Equality
import qualified MAlonzo.Code.Function.Equivalence
import qualified MAlonzo.Code.Function.Injection
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.LeftInverse._LeftInverseOf_
d__LeftInverseOf__16 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 -> ()
d__LeftInverseOf__16 = erased
-- Function.LeftInverse._RightInverseOf_
d__RightInverseOf__64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 -> ()
d__RightInverseOf__64 = erased
-- Function.LeftInverse.LeftInverse
d_LeftInverse_82 a0 a1 a2 a3 a4 a5 = ()
data T_LeftInverse_82
  = C_LeftInverse'46'constructor_4525 MAlonzo.Code.Function.Equality.T_Π_16
                                      MAlonzo.Code.Function.Equality.T_Π_16 (AgdaAny -> AgdaAny)
-- Function.LeftInverse.LeftInverse.to
d_to_102 ::
  T_LeftInverse_82 -> MAlonzo.Code.Function.Equality.T_Π_16
d_to_102 v0
  = case coe v0 of
      C_LeftInverse'46'constructor_4525 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.LeftInverse.LeftInverse.from
d_from_104 ::
  T_LeftInverse_82 -> MAlonzo.Code.Function.Equality.T_Π_16
d_from_104 v0
  = case coe v0 of
      C_LeftInverse'46'constructor_4525 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.LeftInverse.LeftInverse.left-inverse-of
d_left'45'inverse'45'of_106 ::
  T_LeftInverse_82 -> AgdaAny -> AgdaAny
d_left'45'inverse'45'of_106 v0
  = case coe v0 of
      C_LeftInverse'46'constructor_4525 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.LeftInverse.LeftInverse.F._≈_
d__'8776'__110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 -> AgdaAny -> AgdaAny -> ()
d__'8776'__110 = erased
-- Function.LeftInverse.LeftInverse.T._≈_
d__'8776'__132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 -> AgdaAny -> AgdaAny -> ()
d__'8776'__132 = erased
-- Function.LeftInverse.LeftInverse.injective
d_injective_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_176 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 v7 v8 v9
  = du_injective_176 v4 v6 v7 v8 v9
du_injective_176 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_injective_176 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0 v2
         (coe
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
            (d_from_104 (coe v1))
            (coe
               MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
               (d_to_102 (coe v1)) v2))
         v3
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0
            (coe
               MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
               (d_from_104 (coe v1))
               (coe
                  MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (d_to_102 (coe v1)) v2))
            (coe
               MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
               (d_from_104 (coe v1))
               (coe
                  MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (d_to_102 (coe v1)) v3))
            v3
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               v0
               (coe
                  MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (d_from_104 (coe v1))
                  (coe
                     MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                     (d_to_102 (coe v1)) v3))
               v3 v3
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                  (coe v3))
               (coe d_left'45'inverse'45'of_106 v1 v3))
            (coe
               MAlonzo.Code.Function.Equality.d_cong_40 (d_from_104 (coe v1))
               (coe
                  MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (d_to_102 (coe v1)) v2)
               (coe
                  MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (d_to_102 (coe v1)) v3)
               v4))
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_sym_36
            (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
            (coe
               MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
               (d_from_104 (coe v1))
               (coe
                  MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (d_to_102 (coe v1)) v2))
            v2 (coe d_left'45'inverse'45'of_106 v1 v2)))
-- Function.LeftInverse.LeftInverse.injection
d_injection_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 -> MAlonzo.Code.Function.Injection.T_Injection_88
d_injection_184 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 = du_injection_184 v4 v6
du_injection_184 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 -> MAlonzo.Code.Function.Injection.T_Injection_88
du_injection_184 v0 v1
  = coe
      MAlonzo.Code.Function.Injection.C_Injection'46'constructor_3039
      (coe d_to_102 (coe v1)) (coe du_injective_176 (coe v0) (coe v1))
-- Function.LeftInverse.LeftInverse.equivalence
d_equivalence_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_equivalence_186 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_equivalence_186 v6
du_equivalence_186 ::
  T_LeftInverse_82 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du_equivalence_186 v0
  = coe
      MAlonzo.Code.Function.Equivalence.C_Equivalence'46'constructor_433
      (coe d_to_102 (coe v0)) (coe d_from_104 (coe v0))
-- Function.LeftInverse.LeftInverse.to-from
d_to'45'from_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_to'45'from_192 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
  = du_to'45'from_192 v4 v5 v6 v7 v8 v9
du_to'45'from_192 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_to'45'from_192 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
      (coe
         MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
         v0
         (coe
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
            (d_from_104 (coe v2)) v4)
         (coe
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
            (d_from_104 (coe v2))
            (coe
               MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
               (d_to_102 (coe v2)) v3))
         v3
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            v0
            (coe
               MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
               (d_from_104 (coe v2))
               (coe
                  MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (d_to_102 (coe v2)) v3))
            v3 v3
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                  (coe
                     MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
               (coe v3))
            (coe d_left'45'inverse'45'of_106 v2 v3))
         (coe
            MAlonzo.Code.Function.Equality.d_cong_40 (d_from_104 (coe v2)) v4
            (coe
               MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
               (d_to_102 (coe v2)) v3)
            (coe
               MAlonzo.Code.Relation.Binary.Structures.d_sym_36
               (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v1))
               (coe
                  MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (d_to_102 (coe v2)) v3)
               v4 v5)))
-- Function.LeftInverse.RightInverse
d_RightInverse_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 -> ()
d_RightInverse_212 = erased
-- Function.LeftInverse._↞_
d__'8606'__222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8606'__222 = erased
-- Function.LeftInverse.leftInverse
d_leftInverse_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_LeftInverse_82
d_leftInverse_242 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_leftInverse_242 v4 v5 v6
du_leftInverse_242 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_LeftInverse_82
du_leftInverse_242 v0 v1 v2
  = coe
      C_LeftInverse'46'constructor_4525
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.du_'8594''45'to'45''10230'_68
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
            (coe
               MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
               erased erased erased))
         v0)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.du_'8594''45'to'45''10230'_68
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
            (coe
               MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
               erased erased erased))
         v1)
      (coe v2)
-- Function.LeftInverse.id
d_id_256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82
d_id_256 ~v0 ~v1 v2 = du_id_256 v2
du_id_256 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82
du_id_256 v0
  = coe
      C_LeftInverse'46'constructor_4525
      (coe MAlonzo.Code.Function.Equality.du_id_62)
      (coe MAlonzo.Code.Function.Equality.du_id_62)
      (coe
         (\ v1 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
              (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
              (coe
                 MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                 (coe MAlonzo.Code.Function.Equality.du_id_62)
                 (coe
                    MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                    (coe MAlonzo.Code.Function.Equality.du_id_62) v1))))
-- Function.LeftInverse._∘_
d__'8728'__280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 -> T_LeftInverse_82 -> T_LeftInverse_82
d__'8728'__280 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7 ~v8 v9 v10
  = du__'8728'__280 v6 v9 v10
du__'8728'__280 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_LeftInverse_82 -> T_LeftInverse_82 -> T_LeftInverse_82
du__'8728'__280 v0 v1 v2
  = coe
      C_LeftInverse'46'constructor_4525
      (coe
         MAlonzo.Code.Function.Equality.du__'8728'__82
         (coe d_to_102 (coe v1)) (coe d_to_102 (coe v2)))
      (coe
         MAlonzo.Code.Function.Equality.du__'8728'__82
         (coe d_from_104 (coe v2)) (coe d_from_104 (coe v1)))
      (coe
         (\ v3 ->
            MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
              (coe
                 MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                 v0
                 (coe
                    MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                    (d_from_104 (coe v2))
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                       (d_from_104 (coe v1))
                       (coe
                          MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                          (d_to_102 (coe v1))
                          (coe
                             MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                             (d_to_102 (coe v2)) v3))))
                 (coe
                    MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                    (d_from_104 (coe v2))
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                       (d_to_102 (coe v2)) v3))
                 v3
                 (coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                    v0
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                       (d_from_104 (coe v2))
                       (coe
                          MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                          (d_to_102 (coe v2)) v3))
                    v3 v3
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                       (coe
                          MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                          (coe
                             MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0)))
                       (coe v3))
                    (coe d_left'45'inverse'45'of_106 v2 v3))
                 (coe
                    MAlonzo.Code.Function.Equality.d_cong_40 (d_from_104 (coe v2))
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                       (d_from_104 (coe v1))
                       (coe
                          MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                          (d_to_102 (coe v1))
                          (coe
                             MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                             (d_to_102 (coe v2)) v3)))
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                       (d_to_102 (coe v2)) v3)
                    (coe
                       d_left'45'inverse'45'of_106 v1
                       (coe
                          MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                          (d_to_102 (coe v2)) v3))))))
