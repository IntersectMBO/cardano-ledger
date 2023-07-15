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

module MAlonzo.Code.Data.Product.Function.NonDependent.Propositional where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Product.Function.NonDependent.Setoid
import qualified MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent
import qualified MAlonzo.Code.Function.Bijection
import qualified MAlonzo.Code.Function.Equivalence
import qualified MAlonzo.Code.Function.Injection
import qualified MAlonzo.Code.Function.Inverse
import qualified MAlonzo.Code.Function.LeftInverse
import qualified MAlonzo.Code.Function.Related
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Function.Surjection
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties

-- Data.Product.Function.NonDependent.Propositional._._×-⇔_
d__'215''45''8660'__30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d__'215''45''8660'__30 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du__'215''45''8660'__30 v8 v9
du__'215''45''8660'__30 ::
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du__'215''45''8660'__30 v0 v1
  = coe
      MAlonzo.Code.Function.Equivalence.du__'8728'__82
      (coe
         MAlonzo.Code.Function.Surjection.du_equivalence_92
         (coe
            MAlonzo.Code.Function.Bijection.du_surjection_100
            (coe
               MAlonzo.Code.Function.Inverse.du_bijection_98
               (coe
                  MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du__'215''8347'__494
                  (coe
                     MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                  (coe
                     MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402))
               (coe
                  MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du_Pointwise'45''8801''8596''8801'_512))))
      (coe
         MAlonzo.Code.Function.Equivalence.du__'8728'__82
         (coe
            MAlonzo.Code.Data.Product.Function.NonDependent.Setoid.du__'215''45'equivalence__150
            (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Function.Equivalence.du_sym_100
            (coe
               MAlonzo.Code.Function.Surjection.du_equivalence_92
               (coe
                  MAlonzo.Code.Function.Bijection.du_surjection_100
                  (coe
                     MAlonzo.Code.Function.Inverse.du_bijection_98
                     (coe
                        MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du__'215''8347'__494
                        (coe
                           MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                        (coe
                           MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402))
                     (coe
                        MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du_Pointwise'45''8801''8596''8801'_512))))))
-- Data.Product.Function.NonDependent.Propositional._._×-↣_
d__'215''45''8611'__40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88
d__'215''45''8611'__40 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du__'215''45''8611'__40 v8 v9
du__'215''45''8611'__40 ::
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88
du__'215''45''8611'__40 v0 v1
  = coe
      MAlonzo.Code.Function.Injection.du__'8728'__172
      (coe
         MAlonzo.Code.Function.LeftInverse.du_injection_184
         (coe
            MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du__'215''8347'__494
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402))
         (coe
            MAlonzo.Code.Function.Inverse.du_left'45'inverse_90
            (coe
               MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du_Pointwise'45''8801''8596''8801'_512)))
      (coe
         MAlonzo.Code.Function.Injection.du__'8728'__172
         (coe
            MAlonzo.Code.Data.Product.Function.NonDependent.Setoid.du__'215''45'injection__160
            (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Function.LeftInverse.du_injection_184
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
            (coe
               MAlonzo.Code.Function.Inverse.du_left'45'inverse_90
               (coe
                  MAlonzo.Code.Function.Inverse.du_sym_226
                  (coe
                     MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du_Pointwise'45''8801''8596''8801'_512)))))
-- Data.Product.Function.NonDependent.Propositional._._×-↞_
d__'215''45''8606'__50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d__'215''45''8606'__50 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du__'215''45''8606'__50 v8 v9
du__'215''45''8606'__50 ::
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du__'215''45''8606'__50 v0 v1
  = coe
      MAlonzo.Code.Function.LeftInverse.du__'8728'__280
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      (coe
         MAlonzo.Code.Function.Inverse.du_left'45'inverse_90
         (coe
            MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du_Pointwise'45''8801''8596''8801'_512))
      (coe
         MAlonzo.Code.Function.LeftInverse.du__'8728'__280
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
         (coe
            MAlonzo.Code.Data.Product.Function.NonDependent.Setoid.du__'215''45'left'45'inverse__170
            (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Function.Inverse.du_left'45'inverse_90
            (coe
               MAlonzo.Code.Function.Inverse.du_sym_226
               (coe
                  MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du_Pointwise'45''8801''8596''8801'_512))))
-- Data.Product.Function.NonDependent.Propositional._._×-↠_
d__'215''45''8608'__60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54
d__'215''45''8608'__60 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du__'215''45''8608'__60 v8 v9
du__'215''45''8608'__60 ::
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54
du__'215''45''8608'__60 v0 v1
  = coe
      MAlonzo.Code.Function.Surjection.du__'8728'__196
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      (coe
         MAlonzo.Code.Function.Bijection.du_surjection_100
         (coe
            MAlonzo.Code.Function.Inverse.du_bijection_98
            (coe
               MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du__'215''8347'__494
               (coe
                  MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
               (coe
                  MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402))
            (coe
               MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du_Pointwise'45''8801''8596''8801'_512)))
      (coe
         MAlonzo.Code.Function.Surjection.du__'8728'__196
         (coe
            MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du__'215''8347'__494
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402))
         (coe
            MAlonzo.Code.Data.Product.Function.NonDependent.Setoid.du__'215''45'surjection__216
            (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Function.Bijection.du_surjection_100
            (coe
               MAlonzo.Code.Function.Inverse.du_bijection_98
               (coe
                  MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
               (coe
                  MAlonzo.Code.Function.Inverse.du_sym_226
                  (coe
                     MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du_Pointwise'45''8801''8596''8801'_512)))))
-- Data.Product.Function.NonDependent.Propositional._._×-↔_
d__'215''45''8596'__70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
d__'215''45''8596'__70 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du__'215''45''8596'__70 v8 v9
du__'215''45''8596'__70 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du__'215''45''8596'__70 v0 v1
  = coe
      MAlonzo.Code.Function.Inverse.du__'8728'__208
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
      (coe
         MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du_Pointwise'45''8801''8596''8801'_512)
      (coe
         MAlonzo.Code.Function.Inverse.du__'8728'__208
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
         (coe
            MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du__'215''8347'__494
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402))
         (coe
            MAlonzo.Code.Data.Product.Function.NonDependent.Setoid.du__'215''45'inverse__228
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
            (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Function.Inverse.du_sym_226
            (coe
               MAlonzo.Code.Data.Product.Relation.Binary.Pointwise.NonDependent.du_Pointwise'45''8801''8596''8801'_512)))
-- Data.Product.Function.NonDependent.Propositional._._×-cong_
d__'215''45'cong__102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'215''45'cong__102 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du__'215''45'cong__102 v8
du__'215''45'cong__102 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'215''45'cong__102 v0
  = case coe v0 of
      MAlonzo.Code.Function.Related.Propositional.C_implication_8
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Data.Product.Base.du_map_104 (coe v1)
                  (coe (\ v3 -> v2)))
      MAlonzo.Code.Function.Related.Propositional.C_reverseImplication_10
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Function.Related.C_lam_34
                  (coe
                     MAlonzo.Code.Data.Product.Base.du_map_104
                     (coe MAlonzo.Code.Function.Related.d_app'45''8592'_32 (coe v1))
                     (coe
                        (\ v3 ->
                           MAlonzo.Code.Function.Related.d_app'45''8592'_32 (coe v2)))))
      MAlonzo.Code.Function.Related.Propositional.C_equivalence_12
        -> coe du__'215''45''8660'__30
      MAlonzo.Code.Function.Related.Propositional.C_injection_14
        -> coe du__'215''45''8611'__40
      MAlonzo.Code.Function.Related.Propositional.C_reverseInjection_16
        -> coe
             (\ v1 v2 ->
                coe
                  MAlonzo.Code.Function.Related.C_lam_58
                  (coe
                     du__'215''45''8611'__40
                     (coe MAlonzo.Code.Function.Related.d_app'45''8610'_56 (coe v1))
                     (coe MAlonzo.Code.Function.Related.d_app'45''8610'_56 (coe v2))))
      MAlonzo.Code.Function.Related.Propositional.C_leftInverse_18
        -> coe du__'215''45''8606'__50
      MAlonzo.Code.Function.Related.Propositional.C_surjection_20
        -> coe du__'215''45''8608'__60
      MAlonzo.Code.Function.Related.Propositional.C_bijection_22
        -> coe du__'215''45''8596'__70
      _ -> MAlonzo.RTE.mazUnreachableError
