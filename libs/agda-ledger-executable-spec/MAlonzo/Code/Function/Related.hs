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

module MAlonzo.Code.Function.Related where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Function.Bijection
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Equality
import qualified MAlonzo.Code.Function.Equivalence
import qualified MAlonzo.Code.Function.Injection
import qualified MAlonzo.Code.Function.Inverse
import qualified MAlonzo.Code.Function.LeftInverse
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Function.Surjection
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Related._←_
d__'8592'__20 a0 a1 a2 a3 = ()
newtype T__'8592'__20 = C_lam_34 (AgdaAny -> AgdaAny)
-- Function.Related._←_.app-←
d_app'45''8592'_32 :: T__'8592'__20 -> AgdaAny -> AgdaAny
d_app'45''8592'_32 v0
  = case coe v0 of
      C_lam_34 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related._↢_
d__'8610'__44 a0 a1 a2 a3 = ()
newtype T__'8610'__44
  = C_lam_58 MAlonzo.Code.Function.Injection.T_Injection_88
-- Function.Related._↢_.app-↢
d_app'45''8610'_56 ::
  T__'8610'__44 -> MAlonzo.Code.Function.Injection.T_Injection_88
d_app'45''8610'_56 v0
  = case coe v0 of
      C_lam_58 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related._∼[_]_
d__'8764''91'_'93'__64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 -> () -> ()
d__'8764''91'_'93'__64 = erased
-- Function.Related.toRelated
d_toRelated_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny
d_toRelated_100 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_toRelated_100 v4 v5
du_toRelated_100 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny
du_toRelated_100 v0 v1
  = case coe v0 of
      MAlonzo.Code.Function.Related.Propositional.C_implication_8
        -> coe MAlonzo.Code.Function.Bundles.d_to_648 (coe v1)
      MAlonzo.Code.Function.Related.Propositional.C_reverseImplication_10
        -> coe
             C_lam_34 (coe MAlonzo.Code.Function.Bundles.d_to_648 (coe v1))
      MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
        -> coe
             MAlonzo.Code.Function.Equivalence.du_equivalence_56
             (coe MAlonzo.Code.Function.Bundles.d_to_938 (coe v1))
             (coe MAlonzo.Code.Function.Bundles.d_from_940 (coe v1))
      MAlonzo.Code.Function.Related.Propositional.C_injection_14
        -> coe
             MAlonzo.Code.Function.Injection.du_injection_140
             (coe MAlonzo.Code.Function.Bundles.d_to_712 (coe v1)) erased
      MAlonzo.Code.Function.Related.Propositional.C_reverseInjection_16
        -> coe
             C_lam_58
             (coe
                MAlonzo.Code.Function.Injection.du_injection_140
                (coe MAlonzo.Code.Function.Bundles.d_to_712 (coe v1)) erased)
      MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
        -> coe
             MAlonzo.Code.Function.LeftInverse.du_leftInverse_242
             (coe MAlonzo.Code.Function.Bundles.d_to_1036 (coe v1))
             (coe MAlonzo.Code.Function.Bundles.d_from_1038 (coe v1)) erased
      MAlonzo.Code.Function.Related.Propositional.C_surjection_20
        -> let v2
                 = MAlonzo.Code.Function.Bundles.d_surjective_786 (coe v1) in
           coe
             MAlonzo.Code.Function.Surjection.du_surjection_154
             (coe MAlonzo.Code.Function.Bundles.d_to_782 (coe v1))
             (coe
                (\ v3 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v2 v3)))
             erased
      MAlonzo.Code.Function.Related.Propositional.C_bijection_22
        -> let v2
                 = MAlonzo.Code.Function.Bundles.d_bijective_856 (coe v1) in
           case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
               -> coe
                    MAlonzo.Code.Function.Inverse.du_inverse_156
                    (coe MAlonzo.Code.Function.Bundles.d_to_852 (coe v1))
                    (coe
                       (\ v5 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v4 v5)))
                    erased erased
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.fromRelated
d_fromRelated_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny
d_fromRelated_138 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_fromRelated_138 v4 v5
du_fromRelated_138 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny
du_fromRelated_138 v0 v1
  = case coe v0 of
      MAlonzo.Code.Function.Related.Propositional.C_implication_8
        -> coe MAlonzo.Code.Function.Bundles.du_mk'10230'_1290 (coe v1)
      MAlonzo.Code.Function.Related.Propositional.C_reverseImplication_10
        -> coe
             MAlonzo.Code.Function.Bundles.du_mk'10230'_1290
             (coe d_app'45''8592'_32 (coe v1))
      MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
        -> case coe v1 of
             MAlonzo.Code.Function.Equivalence.C_Equivalence'46'constructor_433 v2 v3
               -> coe
                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v2))
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v3))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Function.Related.Propositional.C_injection_14
        -> coe
             MAlonzo.Code.Function.Bundles.du_mk'8611'_1296
             (coe
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                (coe MAlonzo.Code.Function.Injection.d_to_106 (coe v1)))
             erased
      MAlonzo.Code.Function.Related.Propositional.C_reverseInjection_16
        -> case coe v1 of
             C_lam_58 v2
               -> coe
                    MAlonzo.Code.Function.Bundles.du_mk'8611'_1296
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                       (coe MAlonzo.Code.Function.Injection.d_to_106 (coe v2)))
                    erased
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
        -> case coe v1 of
             MAlonzo.Code.Function.LeftInverse.C_LeftInverse'46'constructor_4525 v2 v3 v4
               -> coe
                    MAlonzo.Code.Function.Bundles.du_mk'8618'_1344
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v2))
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v3))
                    erased
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Function.Related.Propositional.C_surjection_20
        -> case coe v1 of
             MAlonzo.Code.Function.Surjection.C_Surjection'46'constructor_2365 v2 v3
               -> case coe v3 of
                    MAlonzo.Code.Function.Surjection.C_Surjective'46'constructor_1227 v4 v5
                      -> coe
                           MAlonzo.Code.Function.Bundles.du_mk'8608'_1304
                           (coe
                              MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v2))
                           (coe
                              MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88
                              (coe
                                 MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v4))
                              (coe v5))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Function.Related.Propositional.C_bijection_22
        -> case coe v1 of
             MAlonzo.Code.Function.Inverse.C_Inverse'46'constructor_3553 v2 v3 v4
               -> case coe v4 of
                    MAlonzo.Code.Function.Inverse.C__InverseOf_'46'constructor_2103 v5 v6
                      -> coe
                           MAlonzo.Code.Function.Bundles.du_mk'10518'_1312
                           (coe
                              MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v2))
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased
                              (coe
                                 MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88
                                 (coe
                                    MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                                    (coe v3))
                                 (coe v6)))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Related
d_Related_220 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d_Related_220 = erased
-- Function.Related.↔⇒
d_'8596''8658'_238 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> MAlonzo.Code.Function.Inverse.T_Inverse_58 -> AgdaAny
d_'8596''8658'_238 v0 ~v1 ~v2 ~v3 ~v4 = du_'8596''8658'_238 v0
du_'8596''8658'_238 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 -> AgdaAny
du_'8596''8658'_238 v0
  = case coe v0 of
      MAlonzo.Code.Function.Related.Propositional.C_implication_8
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe MAlonzo.Code.Function.Inverse.d_to_78 (coe v1)))
      MAlonzo.Code.Function.Related.Propositional.C_reverseImplication_10
        -> coe
             MAlonzo.Code.Function.Base.du__'8728''8242'__216 (coe C_lam_34)
             (coe
                (\ v1 ->
                   MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                     (coe MAlonzo.Code.Function.Inverse.d_from_80 (coe v1))))
      MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
        -> coe
             (\ v1 ->
                let v2
                      = coe
                          MAlonzo.Code.Function.Inverse.du_bijection_98
                          (coe
                             MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                          (coe v1) in
                coe
                  MAlonzo.Code.Function.Surjection.du_equivalence_92
                  (coe MAlonzo.Code.Function.Bijection.du_surjection_100 (coe v2)))
      MAlonzo.Code.Function.Related.Propositional.C_injection_14
        -> coe
             (\ v1 ->
                coe
                  MAlonzo.Code.Function.LeftInverse.du_injection_184
                  (coe
                     MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                  (coe MAlonzo.Code.Function.Inverse.du_left'45'inverse_90 (coe v1)))
      MAlonzo.Code.Function.Related.Propositional.C_reverseInjection_16
        -> coe
             MAlonzo.Code.Function.Base.du__'8728''8242'__216 (coe C_lam_58)
             (coe
                (\ v1 ->
                   coe
                     MAlonzo.Code.Function.LeftInverse.du_injection_184
                     (coe
                        MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                     (coe
                        MAlonzo.Code.Function.Inverse.du_left'45'inverse_90
                        (coe MAlonzo.Code.Function.Inverse.du_sym_226 (coe v1)))))
      MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
        -> coe MAlonzo.Code.Function.Inverse.du_left'45'inverse_90
      MAlonzo.Code.Function.Related.Propositional.C_surjection_20
        -> coe
             (\ v1 ->
                coe
                  MAlonzo.Code.Function.Bijection.du_surjection_100
                  (coe
                     MAlonzo.Code.Function.Inverse.du_bijection_98
                     (coe
                        MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                     (coe v1)))
      MAlonzo.Code.Function.Related.Propositional.C_bijection_22
        -> coe (\ v1 -> v1)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.≡⇒
d_'8801''8658'_248 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_'8801''8658'_248 v0 ~v1 ~v2 ~v3 ~v4 = du_'8801''8658'_248 v0
du_'8801''8658'_248 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 -> AgdaAny
du_'8801''8658'_248 v0
  = coe
      du_'8596''8658'_238 v0
      (coe
         MAlonzo.Code.Function.Inverse.du_id_186
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402))
-- Function.Related.Symmetric-kind
d_Symmetric'45'kind_250 = ()
data T_Symmetric'45'kind_250 = C_equivalence_252 | C_bijection_254
-- Function.Related.⌊_⌋
d_'8970'_'8971'_256 ::
  T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6
d_'8970'_'8971'_256 v0
  = case coe v0 of
      C_equivalence_252
        -> coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
      C_bijection_254
        -> coe MAlonzo.Code.Function.Related.Propositional.C_bijection_22
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Forward-kind
d_Forward'45'kind_258 = ()
data T_Forward'45'kind_258
  = C_implication_260 | C_equivalence_262 | C_injection_264 |
    C_left'45'inverse_266 | C_surjection_268 | C_bijection_270
-- Function.Related.⌊_⌋→
d_'8970'_'8971''8594'_272 ::
  T_Forward'45'kind_258 ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6
d_'8970'_'8971''8594'_272 v0
  = case coe v0 of
      C_implication_260
        -> coe MAlonzo.Code.Function.Related.Propositional.C_implication_8
      C_equivalence_262
        -> coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
      C_injection_264
        -> coe MAlonzo.Code.Function.Related.Propositional.C_injection_14
      C_left'45'inverse_266
        -> coe MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
      C_surjection_268
        -> coe MAlonzo.Code.Function.Related.Propositional.C_surjection_20
      C_bijection_270
        -> coe MAlonzo.Code.Function.Related.Propositional.C_bijection_22
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.⇒→
d_'8658''8594'_284 ::
  T_Forward'45'kind_258 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d_'8658''8594'_284 v0 ~v1 ~v2 ~v3 ~v4 = du_'8658''8594'_284 v0
du_'8658''8594'_284 ::
  T_Forward'45'kind_258 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8658''8594'_284 v0
  = case coe v0 of
      C_implication_260 -> coe (\ v1 -> v1)
      C_equivalence_262
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe MAlonzo.Code.Function.Equivalence.d_to_34 (coe v1)))
      C_injection_264
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe MAlonzo.Code.Function.Injection.d_to_106 (coe v1)))
      C_left'45'inverse_266
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe MAlonzo.Code.Function.LeftInverse.d_to_102 (coe v1)))
      C_surjection_268
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe MAlonzo.Code.Function.Surjection.d_to_72 (coe v1)))
      C_bijection_270
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe MAlonzo.Code.Function.Inverse.d_to_78 (coe v1)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Backward-kind
d_Backward'45'kind_286 = ()
data T_Backward'45'kind_286
  = C_reverse'45'implication_288 | C_equivalence_290 |
    C_reverse'45'injection_292 | C_left'45'inverse_294 |
    C_surjection_296 | C_bijection_298
-- Function.Related.⌊_⌋←
d_'8970'_'8971''8592'_300 ::
  T_Backward'45'kind_286 ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6
d_'8970'_'8971''8592'_300 v0
  = case coe v0 of
      C_reverse'45'implication_288
        -> coe
             MAlonzo.Code.Function.Related.Propositional.C_reverseImplication_10
      C_equivalence_290
        -> coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
      C_reverse'45'injection_292
        -> coe
             MAlonzo.Code.Function.Related.Propositional.C_reverseInjection_16
      C_left'45'inverse_294
        -> coe MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
      C_surjection_296
        -> coe MAlonzo.Code.Function.Related.Propositional.C_surjection_20
      C_bijection_298
        -> coe MAlonzo.Code.Function.Related.Propositional.C_bijection_22
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.⇒←
d_'8658''8592'_312 ::
  T_Backward'45'kind_286 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d_'8658''8592'_312 v0 ~v1 ~v2 ~v3 ~v4 = du_'8658''8592'_312 v0
du_'8658''8592'_312 ::
  T_Backward'45'kind_286 -> AgdaAny -> AgdaAny -> AgdaAny
du_'8658''8592'_312 v0
  = case coe v0 of
      C_reverse'45'implication_288
        -> coe (\ v1 -> d_app'45''8592'_32 (coe v1))
      C_equivalence_290
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe MAlonzo.Code.Function.Equivalence.d_from_36 (coe v1)))
      C_reverse'45'injection_292
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe
                     MAlonzo.Code.Function.Injection.d_to_106
                     (coe d_app'45''8610'_56 (coe v1))))
      C_left'45'inverse_294
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe MAlonzo.Code.Function.LeftInverse.d_from_104 (coe v1)))
      C_surjection_296
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe
                     MAlonzo.Code.Function.Surjection.d_from_38
                     (coe MAlonzo.Code.Function.Surjection.d_surjective_74 (coe v1))))
      C_bijection_298
        -> coe
             (\ v1 ->
                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                  (coe MAlonzo.Code.Function.Inverse.d_from_80 (coe v1)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.Equivalence-kind
d_Equivalence'45'kind_314 = ()
data T_Equivalence'45'kind_314
  = C_equivalence_316 | C_left'45'inverse_318 | C_surjection_320 |
    C_bijection_322
-- Function.Related.⌊_⌋⇔
d_'8970'_'8971''8660'_324 ::
  T_Equivalence'45'kind_314 ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6
d_'8970'_'8971''8660'_324 v0
  = case coe v0 of
      C_equivalence_316
        -> coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
      C_left'45'inverse_318
        -> coe MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
      C_surjection_320
        -> coe MAlonzo.Code.Function.Related.Propositional.C_surjection_20
      C_bijection_322
        -> coe MAlonzo.Code.Function.Related.Propositional.C_bijection_22
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.⇒⇔
d_'8658''8660'_336 ::
  T_Equivalence'45'kind_314 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () -> AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_'8658''8660'_336 v0 ~v1 ~v2 ~v3 ~v4 = du_'8658''8660'_336 v0
du_'8658''8660'_336 ::
  T_Equivalence'45'kind_314 ->
  AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du_'8658''8660'_336 v0
  = case coe v0 of
      C_equivalence_316 -> coe (\ v1 -> v1)
      C_left'45'inverse_318
        -> coe MAlonzo.Code.Function.LeftInverse.du_equivalence_186
      C_surjection_320
        -> coe MAlonzo.Code.Function.Surjection.du_equivalence_92
      C_bijection_322
        -> coe
             (\ v1 ->
                let v2
                      = coe
                          MAlonzo.Code.Function.Inverse.du_bijection_98
                          (coe
                             MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                          (coe v1) in
                coe
                  MAlonzo.Code.Function.Surjection.du_equivalence_92
                  (coe MAlonzo.Code.Function.Bijection.du_surjection_100 (coe v2)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.⇔⌊_⌋
d_'8660''8970'_'8971'_338 ::
  T_Symmetric'45'kind_250 -> T_Equivalence'45'kind_314
d_'8660''8970'_'8971'_338 v0
  = case coe v0 of
      C_equivalence_252 -> coe C_equivalence_316
      C_bijection_254 -> coe C_bijection_322
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.→⌊_⌋
d_'8594''8970'_'8971'_340 ::
  T_Equivalence'45'kind_314 -> T_Forward'45'kind_258
d_'8594''8970'_'8971'_340 v0
  = case coe v0 of
      C_equivalence_316 -> coe C_equivalence_262
      C_left'45'inverse_318 -> coe C_left'45'inverse_266
      C_surjection_320 -> coe C_surjection_268
      C_bijection_322 -> coe C_bijection_270
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.←⌊_⌋
d_'8592''8970'_'8971'_342 ::
  T_Equivalence'45'kind_314 -> T_Backward'45'kind_286
d_'8592''8970'_'8971'_342 v0
  = case coe v0 of
      C_equivalence_316 -> coe C_equivalence_290
      C_left'45'inverse_318 -> coe C_left'45'inverse_294
      C_surjection_320 -> coe C_surjection_296
      C_bijection_322 -> coe C_bijection_298
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related._op
d__op_344 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6
d__op_344 v0
  = case coe v0 of
      MAlonzo.Code.Function.Related.Propositional.C_implication_8
        -> coe
             MAlonzo.Code.Function.Related.Propositional.C_reverseImplication_10
      MAlonzo.Code.Function.Related.Propositional.C_reverseImplication_10
        -> coe MAlonzo.Code.Function.Related.Propositional.C_implication_8
      MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
        -> coe v0
      MAlonzo.Code.Function.Related.Propositional.C_injection_14
        -> coe
             MAlonzo.Code.Function.Related.Propositional.C_reverseInjection_16
      MAlonzo.Code.Function.Related.Propositional.C_reverseInjection_16
        -> coe MAlonzo.Code.Function.Related.Propositional.C_injection_14
      MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
        -> coe MAlonzo.Code.Function.Related.Propositional.C_surjection_20
      MAlonzo.Code.Function.Related.Propositional.C_surjection_20
        -> coe MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
      MAlonzo.Code.Function.Related.Propositional.C_bijection_22
        -> coe v0
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.reverse
d_reverse_356 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> AgdaAny -> AgdaAny
d_reverse_356 v0 ~v1 ~v2 ~v3 ~v4 = du_reverse_356 v0
du_reverse_356 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny
du_reverse_356 v0
  = case coe v0 of
      MAlonzo.Code.Function.Related.Propositional.C_implication_8
        -> coe C_lam_34
      MAlonzo.Code.Function.Related.Propositional.C_reverseImplication_10
        -> coe (\ v1 -> d_app'45''8592'_32 (coe v1))
      MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
        -> coe MAlonzo.Code.Function.Equivalence.du_sym_100
      MAlonzo.Code.Function.Related.Propositional.C_injection_14
        -> coe C_lam_58
      MAlonzo.Code.Function.Related.Propositional.C_reverseInjection_16
        -> coe (\ v1 -> d_app'45''8610'_56 (coe v1))
      MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
        -> coe MAlonzo.Code.Function.Surjection.du_fromRightInverse_106
      MAlonzo.Code.Function.Related.Propositional.C_surjection_20
        -> coe MAlonzo.Code.Function.Surjection.du_right'45'inverse_82
      MAlonzo.Code.Function.Related.Propositional.C_bijection_22
        -> coe MAlonzo.Code.Function.Inverse.du_sym_226
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.K-refl
d_K'45'refl_362 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny
d_K'45'refl_362 v0 ~v1 ~v2 = du_K'45'refl_362 v0
du_K'45'refl_362 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 -> AgdaAny
du_K'45'refl_362 v0
  = case coe v0 of
      MAlonzo.Code.Function.Related.Propositional.C_implication_8
        -> coe (\ v1 -> v1)
      MAlonzo.Code.Function.Related.Propositional.C_reverseImplication_10
        -> coe C_lam_34 (coe (\ v1 -> v1))
      MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
        -> coe MAlonzo.Code.Function.Equivalence.du_id_66
      MAlonzo.Code.Function.Related.Propositional.C_injection_14
        -> coe MAlonzo.Code.Function.Injection.du_id_152
      MAlonzo.Code.Function.Related.Propositional.C_reverseInjection_16
        -> coe C_lam_58 (coe MAlonzo.Code.Function.Injection.du_id_152)
      MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
        -> coe
             MAlonzo.Code.Function.LeftInverse.du_id_256
             (coe
                MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      MAlonzo.Code.Function.Related.Propositional.C_surjection_20
        -> coe
             MAlonzo.Code.Function.Surjection.du_id_168
             (coe
                MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      MAlonzo.Code.Function.Related.Propositional.C_bijection_22
        -> coe
             MAlonzo.Code.Function.Inverse.du_id_186
             (coe
                MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.K-reflexive
d_K'45'reflexive_368 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_K'45'reflexive_368 v0 ~v1 ~v2 ~v3 ~v4 = du_K'45'reflexive_368 v0
du_K'45'reflexive_368 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 -> AgdaAny
du_K'45'reflexive_368 v0 = coe du_K'45'refl_362 (coe v0)
-- Function.Related.K-trans
d_K'45'trans_378 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d_K'45'trans_378 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 = du_K'45'trans_378 v0
du_K'45'trans_378 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_K'45'trans_378 v0
  = case coe v0 of
      MAlonzo.Code.Function.Related.Propositional.C_implication_8
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Function.Base.du__'8728''8242'__216 (coe v2) (coe v1))
      MAlonzo.Code.Function.Related.Propositional.C_reverseImplication_10
        -> coe
             (\ v1 v2 ->
                coe
                  C_lam_34
                  (coe
                     (\ v3 ->
                        coe d_app'45''8592'_32 v1 (coe d_app'45''8592'_32 v2 v3))))
      MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Function.Equivalence.du__'8728'__82 (coe v2) (coe v1))
      MAlonzo.Code.Function.Related.Propositional.C_injection_14
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Function.Injection.du__'8728'__172 (coe v2) (coe v1))
      MAlonzo.Code.Function.Related.Propositional.C_reverseInjection_16
        -> coe
             (\ v1 v2 ->
                coe
                  C_lam_58
                  (coe
                     MAlonzo.Code.Function.Injection.du__'8728'__172
                     (coe d_app'45''8610'_56 (coe v1))
                     (coe d_app'45''8610'_56 (coe v2))))
      MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Function.LeftInverse.du__'8728'__280
                  (coe
                     MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                  (coe v2) (coe v1))
      MAlonzo.Code.Function.Related.Propositional.C_surjection_20
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Function.Surjection.du__'8728'__196
                  (coe
                     MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                  (coe v2) (coe v1))
      MAlonzo.Code.Function.Related.Propositional.C_bijection_22
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Function.Inverse.du__'8728'__208
                  (coe
                     MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                  (coe
                     MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                  (coe v2) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.SK-sym
d_SK'45'sym_394 ::
  T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> AgdaAny -> AgdaAny
d_SK'45'sym_394 v0 ~v1 ~v2 ~v3 ~v4 = du_SK'45'sym_394 v0
du_SK'45'sym_394 :: T_Symmetric'45'kind_250 -> AgdaAny -> AgdaAny
du_SK'45'sym_394 v0
  = case coe v0 of
      C_equivalence_252
        -> coe MAlonzo.Code.Function.Equivalence.du_sym_100
      C_bijection_254 -> coe MAlonzo.Code.Function.Inverse.du_sym_226
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.SK-isEquivalence
d_SK'45'isEquivalence_400 ::
  T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
d_SK'45'isEquivalence_400 v0 ~v1 = du_SK'45'isEquivalence_400 v0
du_SK'45'isEquivalence_400 ::
  T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26
du_SK'45'isEquivalence_400 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
      (\ v1 -> coe du_K'45'refl_362 (coe d_'8970'_'8971'_256 (coe v0)))
      (\ v1 v2 -> coe du_SK'45'sym_394 (coe v0))
      (\ v1 v2 v3 ->
         coe du_K'45'trans_378 (coe d_'8970'_'8971'_256 (coe v0)))
-- Function.Related.SK-setoid
d_SK'45'setoid_408 ::
  T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_SK'45'setoid_408 v0 ~v1 = du_SK'45'setoid_408 v0
du_SK'45'setoid_408 ::
  T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_SK'45'setoid_408 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe du_SK'45'isEquivalence_400 (coe v0))
-- Function.Related.K-isPreorder
d_K'45'isPreorder_418 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
d_K'45'isPreorder_418 v0 ~v1 = du_K'45'isPreorder_418 v0
du_K'45'isPreorder_418 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsPreorder_70
du_K'45'isPreorder_418 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
      (coe du_SK'45'isEquivalence_400 (coe C_bijection_254))
      (\ v1 v2 -> coe du_'8596''8658'_238 (coe v0))
      (\ v1 v2 v3 -> coe du_K'45'trans_378 (coe v0))
-- Function.Related.K-preorder
d_K'45'preorder_426 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_K'45'preorder_426 v0 ~v1 = du_K'45'preorder_426 v0
du_K'45'preorder_426 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_K'45'preorder_426 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe du_K'45'isPreorder_418 (coe v0))
-- Function.Related.EquationalReasoning.begin_
d_begin__444 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> AgdaAny -> AgdaAny
d_begin__444 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_begin__444 v5
du_begin__444 :: AgdaAny -> AgdaAny
du_begin__444 v0 = coe v0
-- Function.Related.EquationalReasoning._∼⟨_⟩_
d__'8764''10216'_'10217'__462 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8764''10216'_'10217'__462 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8
  = du__'8764''10216'_'10217'__462 v0 v7 v8
du__'8764''10216'_'10217'__462 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8764''10216'_'10217'__462 v0 v1 v2
  = coe du_K'45'trans_378 v0 v1 v2
-- Function.Related.EquationalReasoning._↔⟨_⟩_
d__'8596''10216'_'10217'__482 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 -> AgdaAny -> AgdaAny
d__'8596''10216'_'10217'__482 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8
  = du__'8596''10216'_'10217'__482 v0 v7 v8
du__'8596''10216'_'10217'__482 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 -> AgdaAny -> AgdaAny
du__'8596''10216'_'10217'__482 v0 v1 v2
  = coe
      du__'8764''10216'_'10217'__462 (coe v0)
      (coe du_'8596''8658'_238 v0 v1) (coe v2)
-- Function.Related.EquationalReasoning._↔⟨⟩_
d__'8596''10216''10217'__500 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> AgdaAny -> AgdaAny
d__'8596''10216''10217'__500 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du__'8596''10216''10217'__500 v5
du__'8596''10216''10217'__500 :: AgdaAny -> AgdaAny
du__'8596''10216''10217'__500 v0 = coe v0
-- Function.Related.EquationalReasoning._≡˘⟨_⟩_
d__'8801''728''10216'_'10217'__518 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  AgdaAny -> AgdaAny
d__'8801''728''10216'_'10217'__518 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du__'8801''728''10216'_'10217'__518 v0 v7
du__'8801''728''10216'_'10217'__518 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny
du__'8801''728''10216'_'10217'__518 v0 v1
  = coe
      du__'8764''10216'_'10217'__462 (coe v0)
      (coe du_'8801''8658'_248 (coe v0)) (coe v1)
-- Function.Related.EquationalReasoning._≡⟨_⟩_
d__'8801''10216'_'10217'__538 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  AgdaAny -> AgdaAny
d__'8801''10216'_'10217'__538 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7
  = du__'8801''10216'_'10217'__538 v0 v7
du__'8801''10216'_'10217'__538 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny
du__'8801''10216'_'10217'__538 v0 v1
  = coe
      du__'8764''10216'_'10217'__462 (coe v0)
      (coe du_'8801''8658'_248 (coe v0)) (coe v1)
-- Function.Related.EquationalReasoning._∎
d__'8718'_552 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny
d__'8718'_552 v0 ~v1 ~v2 = du__'8718'_552 v0
du__'8718'_552 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 -> AgdaAny
du__'8718'_552 v0 = coe du_K'45'refl_362 (coe v0)
-- Function.Related.InducedRelation₁
d_InducedRelation'8321'_562 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> ()) -> AgdaAny -> AgdaAny -> ()
d_InducedRelation'8321'_562 = erased
-- Function.Related.InducedPreorder₁
d_InducedPreorder'8321'_578 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_InducedPreorder'8321'_578 v0 ~v1 ~v2 ~v3 ~v4
  = du_InducedPreorder'8321'_578 v0
du_InducedPreorder'8321'_578 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_InducedPreorder'8321'_578 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
         (coe
            (\ v1 v2 v3 ->
               coe
                 du_'8596''8658'_238 v0
                 (coe
                    du_K'45'reflexive_368
                    (coe MAlonzo.Code.Function.Related.Propositional.C_bijection_22))))
         (coe (\ v1 v2 v3 -> coe du_K'45'trans_378 (coe v0))))
-- Function.Related.InducedEquivalence₁
d_InducedEquivalence'8321'_642 ::
  T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) -> MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_InducedEquivalence'8321'_642 v0 ~v1 ~v2 ~v3 ~v4
  = du_InducedEquivalence'8321'_642 v0
du_InducedEquivalence'8321'_642 ::
  T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_InducedEquivalence'8321'_642 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
         (coe
            (\ v1 -> coe du_K'45'refl_362 (coe d_'8970'_'8971'_256 (coe v0))))
         (coe (\ v1 v2 -> coe du_SK'45'sym_394 (coe v0)))
         (coe
            (\ v1 v2 v3 ->
               coe du_K'45'trans_378 (coe d_'8970'_'8971'_256 (coe v0)))))
-- Function.Related.InducedRelation₂
d_InducedRelation'8322'_658 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny -> ()) -> AgdaAny -> AgdaAny -> ()
d_InducedRelation'8322'_658 = erased
-- Function.Related.InducedPreorder₂
d_InducedPreorder'8322'_680 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
d_InducedPreorder'8322'_680 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
  = du_InducedPreorder'8322'_680 v0
du_InducedPreorder'8322'_680 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Preorder_132
du_InducedPreorder'8322'_680 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Preorder'46'constructor_2251
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsPreorder'46'constructor_3211
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_isEquivalence_396)
         (coe
            (\ v1 v2 v3 v4 ->
               coe
                 du_'8596''8658'_238 v0
                 (coe
                    du_K'45'reflexive_368
                    (coe MAlonzo.Code.Function.Related.Propositional.C_bijection_22))))
         (coe
            (\ v1 v2 v3 v4 v5 v6 ->
               coe du_K'45'trans_378 v0 (coe v4 v6) (coe v5 v6))))
-- Function.Related.InducedEquivalence₂
d_InducedEquivalence'8322'_756 ::
  T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_InducedEquivalence'8322'_756 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
  = du_InducedEquivalence'8322'_756 v0
du_InducedEquivalence'8322'_756 ::
  T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_InducedEquivalence'8322'_756 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
         (coe
            (\ v1 v2 ->
               coe du_K'45'refl_362 (coe d_'8970'_'8971'_256 (coe v0))))
         (coe (\ v1 v2 v3 v4 -> coe du_SK'45'sym_394 v0 (coe v3 v4)))
         (coe
            (\ v1 v2 v3 v4 v5 v6 ->
               coe
                 du_K'45'trans_378 (d_'8970'_'8971'_256 (coe v0)) (coe v4 v6)
                 (coe v5 v6))))
