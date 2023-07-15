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

module MAlonzo.Code.Function.Related.TypeIsomorphisms where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Algebra.Structures.Biased
import qualified MAlonzo.Code.Data.Empty.Polymorphic
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Product.Function.NonDependent.Propositional
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.Sum.Function.Propositional
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Function.Equality
import qualified MAlonzo.Code.Function.Equivalence
import qualified MAlonzo.Code.Function.Inverse
import qualified MAlonzo.Code.Function.Related
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Level
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Function.Related.TypeIsomorphisms.Σ-assoc
d_Σ'45'assoc_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
d_Σ'45'assoc_22 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 = du_Σ'45'assoc_22
du_Σ'45'assoc_22 :: MAlonzo.Code.Function.Inverse.T_Inverse_58
du_Σ'45'assoc_22
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
              (coe
                 MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0)))
              (coe
                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                 (coe
                    MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                    (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0)))
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v0)))))
      (coe
         (\ v0 ->
            case coe v0 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
                -> case coe v2 of
                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
                       -> coe
                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                            (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v3))
                            (coe v4)
                     _ -> MAlonzo.RTE.mazUnreachableError
              _ -> MAlonzo.RTE.mazUnreachableError))
      erased erased
-- Function.Related.TypeIsomorphisms.×-comm
d_'215''45'comm_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'215''45'comm_52 ~v0 ~v1 ~v2 ~v3 = du_'215''45'comm_52
du_'215''45'comm_52 :: MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'215''45'comm_52
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe MAlonzo.Code.Data.Product.Base.du_swap_346)
      (coe MAlonzo.Code.Data.Product.Base.du_swap_346) erased erased
-- Function.Related.TypeIsomorphisms.×-identityˡ
d_'215''45'identity'737'_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'215''45'identity'737'_60 ~v0 ~v1 = du_'215''45'identity'737'_60
du_'215''45'identity'737'_60 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'215''45'identity'737'_60
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v0)))
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
              (coe
                 MAlonzo.Code.Level.C_lift_20
                 (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
              (coe v0)))
      erased erased
-- Function.Related.TypeIsomorphisms.×-identityʳ
d_'215''45'identity'691'_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'215''45'identity'691'_68 ~v0 ~v1 = du_'215''45'identity'691'_68
du_'215''45'identity'691'_68 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'215''45'identity'691'_68
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0)))
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
              (coe
                 MAlonzo.Code.Level.C_lift_20
                 (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))))
      erased erased
-- Function.Related.TypeIsomorphisms.×-identity
d_'215''45'identity_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'215''45'identity_78 ~v0 = du_'215''45'identity_78
du_'215''45'identity_78 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'215''45'identity_78
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 -> coe du_'215''45'identity'737'_60)
      (\ v0 -> coe du_'215''45'identity'691'_68)
-- Function.Related.TypeIsomorphisms.×-zeroˡ
d_'215''45'zero'737'_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'215''45'zero'737'_84 ~v0 ~v1 = du_'215''45'zero'737'_84
du_'215''45'zero'737'_84 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'215''45'zero'737'_84
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0)))
      (coe
         MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88
         (coe (\ v0 -> v0))
         (\ v0 ->
            coe MAlonzo.Code.Data.Empty.Polymorphic.du_'8869''45'elim_20))
      erased erased
-- Function.Related.TypeIsomorphisms.×-zeroʳ
d_'215''45'zero'691'_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'215''45'zero'691'_96 ~v0 ~v1 = du_'215''45'zero'691'_96
du_'215''45'zero'691'_96 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'215''45'zero'691'_96
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v0)))
      (coe
         MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88
         (\ v0 ->
            coe MAlonzo.Code.Data.Empty.Polymorphic.du_'8869''45'elim_20)
         (coe (\ v0 -> v0)))
      erased erased
-- Function.Related.TypeIsomorphisms.×-zero
d_'215''45'zero_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'215''45'zero_108 ~v0 = du_'215''45'zero_108
du_'215''45'zero_108 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'215''45'zero_108
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 -> coe du_'215''45'zero'737'_84)
      (\ v0 -> coe du_'215''45'zero'691'_96)
-- Function.Related.TypeIsomorphisms.⊎-assoc
d_'8846''45'assoc_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'8846''45'assoc_114 ~v0 ~v1 ~v2 ~v3 = du_'8846''45'assoc_114
du_'8846''45'assoc_114 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'8846''45'assoc_114
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
         (coe
            MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
            (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
            (coe
               MAlonzo.Code.Function.Base.du__'8728''8242'__216
               (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42)
               (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)))
         (coe
            MAlonzo.Code.Function.Base.du__'8728''8242'__216
            (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42)
            (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42)))
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
         (coe
            MAlonzo.Code.Function.Base.du__'8728''8242'__216
            (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
            (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38))
         (coe
            MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
            (coe
               MAlonzo.Code.Function.Base.du__'8728''8242'__216
               (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
               (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42))
            (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42)))
      erased erased
-- Function.Related.TypeIsomorphisms.⊎-comm
d_'8846''45'comm_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'8846''45'comm_138 ~v0 ~v1 ~v2 ~v3 = du_'8846''45'comm_138
du_'8846''45'comm_138 :: MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'8846''45'comm_138
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe MAlonzo.Code.Data.Sum.Base.du_swap_78)
      (coe MAlonzo.Code.Data.Sum.Base.du_swap_78) erased erased
-- Function.Related.TypeIsomorphisms.⊎-identityˡ
d_'8846''45'identity'737'_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'8846''45'identity'737'_142 ~v0 ~v1
  = du_'8846''45'identity'737'_142
du_'8846''45'identity'737'_142 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'8846''45'identity'737'_142
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66 erased
         (\ v0 -> v0))
      (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42) erased erased
-- Function.Related.TypeIsomorphisms.⊎-identityʳ
d_'8846''45'identity'691'_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'8846''45'identity'691'_150 ~v0 ~v1
  = du_'8846''45'identity'691'_150
du_'8846''45'identity'691'_150 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'8846''45'identity'691'_150
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66 (\ v0 -> v0)
         erased)
      (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38) erased erased
-- Function.Related.TypeIsomorphisms.⊎-identity
d_'8846''45'identity_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8846''45'identity_158 ~v0 = du_'8846''45'identity_158
du_'8846''45'identity_158 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8846''45'identity_158
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 -> coe du_'8846''45'identity'737'_142)
      (\ v0 -> coe du_'8846''45'identity'691'_150)
-- Function.Related.TypeIsomorphisms.×-distribˡ-⊎
d_'215''45'distrib'737''45''8846'_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'215''45'distrib'737''45''8846'_164 ~v0 ~v1 ~v2 ~v3
  = du_'215''45'distrib'737''45''8846'_164
du_'215''45'distrib'737''45''8846'_164 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'215''45'distrib'737''45''8846'_164
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe
         MAlonzo.Code.Data.Product.Base.du_uncurry_220
         (coe
            (\ v0 ->
               coe
                 MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
                 (coe
                    MAlonzo.Code.Function.Base.du__'8728''8242'__216
                    (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)
                    (coe
                       (\ v1 ->
                          coe
                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0) (coe v1))))
                 (coe
                    MAlonzo.Code.Function.Base.du__'8728''8242'__216
                    (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42)
                    (coe
                       (\ v1 ->
                          coe
                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0) (coe v1)))))))
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
         (coe
            MAlonzo.Code.Data.Product.Base.du_map'8322'_126
            (coe (\ v0 -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38)))
         (coe
            MAlonzo.Code.Data.Product.Base.du_map'8322'_126
            (coe (\ v0 -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42))))
      erased erased
-- Function.Related.TypeIsomorphisms.×-distribʳ-⊎
d_'215''45'distrib'691''45''8846'_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> () -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'215''45'distrib'691''45''8846'_186 ~v0 ~v1 ~v2 ~v3
  = du_'215''45'distrib'691''45''8846'_186
du_'215''45'distrib'691''45''8846'_186 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'215''45'distrib'691''45''8846'_186
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe
         MAlonzo.Code.Data.Product.Base.du_uncurry_220
         (coe
            MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
            (coe
               MAlonzo.Code.Data.Product.Base.du_curry_200
               (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38))
            (coe
               MAlonzo.Code.Data.Product.Base.du_curry_200
               (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42))))
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
         (coe
            MAlonzo.Code.Data.Product.Base.du_map'8321'_114
            (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38))
         (coe
            MAlonzo.Code.Data.Product.Base.du_map'8321'_114
            (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42)))
      erased erased
-- Function.Related.TypeIsomorphisms.×-distrib-⊎
d_'215''45'distrib'45''8846'_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'215''45'distrib'45''8846'_204 ~v0
  = du_'215''45'distrib'45''8846'_204
du_'215''45'distrib'45''8846'_204 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'215''45'distrib'45''8846'_204
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 v1 v2 -> coe du_'215''45'distrib'737''45''8846'_164)
      (\ v0 v1 v2 -> coe du_'215''45'distrib'691''45''8846'_186)
-- Function.Related.TypeIsomorphisms.×-isMagma
d_'215''45'isMagma_212 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'215''45'isMagma_212 v0 ~v1 = du_'215''45'isMagma_212 v0
du_'215''45'isMagma_212 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'215''45'isMagma_212 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Function.Related.du_SK'45'isEquivalence_400 (coe v0))
      (coe
         (\ v1 v2 v3 v4 ->
            coe
              MAlonzo.Code.Data.Product.Function.NonDependent.Propositional.du__'215''45'cong__102
              (coe MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))))
-- Function.Related.TypeIsomorphisms.×-magma
d_'215''45'magma_220 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'215''45'magma_220 v0 ~v1 = du_'215''45'magma_220 v0
du_'215''45'magma_220 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_'215''45'magma_220 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187 erased
      (coe du_'215''45'isMagma_212 (coe v0))
-- Function.Related.TypeIsomorphisms.×-isSemigroup
d_'215''45'isSemigroup_230 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'215''45'isSemigroup_230 v0 ~v1 = du_'215''45'isSemigroup_230 v0
du_'215''45'isSemigroup_230 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'215''45'isSemigroup_230 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'215''45'isMagma_212 (coe v0))
      (coe
         (\ v1 v2 v3 ->
            coe
              MAlonzo.Code.Function.Related.du_'8596''8658'_238
              (MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
              (coe du_Σ'45'assoc_22)))
-- Function.Related.TypeIsomorphisms.×-semigroup
d_'215''45'semigroup_244 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'215''45'semigroup_244 v0 ~v1 = du_'215''45'semigroup_244 v0
du_'215''45'semigroup_244 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'215''45'semigroup_244 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557 erased
      (coe du_'215''45'isSemigroup_230 (coe v0))
-- Function.Related.TypeIsomorphisms.×-isMonoid
d_'215''45'isMonoid_254 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'215''45'isMonoid_254 v0 ~v1 = du_'215''45'isMonoid_254 v0
du_'215''45'isMonoid_254 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'215''45'isMonoid_254 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe du_'215''45'isSemigroup_230 (coe v0))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            (\ v1 ->
               coe
                 MAlonzo.Code.Function.Related.du_'8596''8658'_238
                 (MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
                 (coe du_'215''45'identity'737'_60)))
         (coe
            (\ v1 ->
               coe
                 MAlonzo.Code.Function.Related.du_'8596''8658'_238
                 (MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
                 (coe du_'215''45'identity'691'_68))))
-- Function.Related.TypeIsomorphisms.×-monoid
d_'215''45'monoid_262 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'215''45'monoid_262 v0 ~v1 = du_'215''45'monoid_262 v0
du_'215''45'monoid_262 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'215''45'monoid_262 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309 erased
      erased (coe du_'215''45'isMonoid_254 (coe v0))
-- Function.Related.TypeIsomorphisms.×-isCommutativeMonoid
d_'215''45'isCommutativeMonoid_272 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'215''45'isCommutativeMonoid_272 v0 ~v1
  = du_'215''45'isCommutativeMonoid_272 v0
du_'215''45'isCommutativeMonoid_272 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'215''45'isCommutativeMonoid_272 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe du_'215''45'isMonoid_254 (coe v0))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Function.Related.du_'8596''8658'_238
              (MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
              (coe du_'215''45'comm_52)))
-- Function.Related.TypeIsomorphisms.×-commutativeMonoid
d_'215''45'commutativeMonoid_284 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'215''45'commutativeMonoid_284 v0 ~v1
  = du_'215''45'commutativeMonoid_284 v0
du_'215''45'commutativeMonoid_284 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'215''45'commutativeMonoid_284 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      erased erased (coe du_'215''45'isCommutativeMonoid_272 (coe v0))
-- Function.Related.TypeIsomorphisms.⊎-isMagma
d_'8846''45'isMagma_294 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8846''45'isMagma_294 v0 ~v1 = du_'8846''45'isMagma_294 v0
du_'8846''45'isMagma_294 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8846''45'isMagma_294 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Function.Related.du_SK'45'isEquivalence_400 (coe v0))
      (coe
         (\ v1 v2 v3 v4 ->
            coe
              MAlonzo.Code.Data.Sum.Function.Propositional.du__'8846''45'cong__100
              (coe MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))))
-- Function.Related.TypeIsomorphisms.⊎-magma
d_'8846''45'magma_302 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8846''45'magma_302 v0 ~v1 = du_'8846''45'magma_302 v0
du_'8846''45'magma_302 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_'8846''45'magma_302 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187 erased
      (coe du_'8846''45'isMagma_294 (coe v0))
-- Function.Related.TypeIsomorphisms.⊎-isSemigroup
d_'8846''45'isSemigroup_312 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8846''45'isSemigroup_312 v0 ~v1
  = du_'8846''45'isSemigroup_312 v0
du_'8846''45'isSemigroup_312 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8846''45'isSemigroup_312 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'8846''45'isMagma_294 (coe v0))
      (coe
         (\ v1 v2 v3 ->
            coe
              MAlonzo.Code.Function.Related.du_'8596''8658'_238
              (MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
              (coe du_'8846''45'assoc_114)))
-- Function.Related.TypeIsomorphisms.⊎-semigroup
d_'8846''45'semigroup_326 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8846''45'semigroup_326 v0 ~v1 = du_'8846''45'semigroup_326 v0
du_'8846''45'semigroup_326 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'8846''45'semigroup_326 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557 erased
      (coe du_'8846''45'isSemigroup_312 (coe v0))
-- Function.Related.TypeIsomorphisms.⊎-isMonoid
d_'8846''45'isMonoid_336 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8846''45'isMonoid_336 v0 ~v1 = du_'8846''45'isMonoid_336 v0
du_'8846''45'isMonoid_336 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'8846''45'isMonoid_336 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe du_'8846''45'isSemigroup_312 (coe v0))
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (coe
            (\ v1 ->
               coe
                 MAlonzo.Code.Function.Related.du_'8596''8658'_238
                 (MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
                 (coe du_'8846''45'identity'737'_142)))
         (coe
            (\ v1 ->
               coe
                 MAlonzo.Code.Function.Related.du_'8596''8658'_238
                 (MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
                 (coe du_'8846''45'identity'691'_150))))
-- Function.Related.TypeIsomorphisms.⊎-monoid
d_'8846''45'monoid_344 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'8846''45'monoid_344 v0 ~v1 = du_'8846''45'monoid_344 v0
du_'8846''45'monoid_344 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'8846''45'monoid_344 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309 erased
      erased (coe du_'8846''45'isMonoid_336 (coe v0))
-- Function.Related.TypeIsomorphisms.⊎-isCommutativeMonoid
d_'8846''45'isCommutativeMonoid_354 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'8846''45'isCommutativeMonoid_354 v0 ~v1
  = du_'8846''45'isCommutativeMonoid_354 v0
du_'8846''45'isCommutativeMonoid_354 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'8846''45'isCommutativeMonoid_354 v0
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe du_'8846''45'isMonoid_336 (coe v0))
      (coe
         (\ v1 v2 ->
            coe
              MAlonzo.Code.Function.Related.du_'8596''8658'_238
              (MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
              (coe du_'8846''45'comm_138)))
-- Function.Related.TypeIsomorphisms.⊎-commutativeMonoid
d_'8846''45'commutativeMonoid_366 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'8846''45'commutativeMonoid_366 v0 ~v1
  = du_'8846''45'commutativeMonoid_366 v0
du_'8846''45'commutativeMonoid_366 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'8846''45'commutativeMonoid_366 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      erased erased (coe du_'8846''45'isCommutativeMonoid_354 (coe v0))
-- Function.Related.TypeIsomorphisms.×-⊎-isCommutativeSemiring
d_'215''45''8846''45'isCommutativeSemiring_376 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_'215''45''8846''45'isCommutativeSemiring_376 v0 ~v1
  = du_'215''45''8846''45'isCommutativeSemiring_376 v0
du_'215''45''8846''45'isCommutativeSemiring_376 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_'215''45''8846''45'isCommutativeSemiring_376 v0
  = coe
      MAlonzo.Code.Algebra.Structures.Biased.du_isCommutativeSemiring_2780
      erased erased erased
      (coe
         MAlonzo.Code.Algebra.Structures.Biased.C_IsCommutativeSemiring'737''46'constructor_39845
         (coe du_'8846''45'isCommutativeMonoid_354 (coe v0))
         (coe du_'215''45'isCommutativeMonoid_272 (coe v0))
         (coe
            (\ v1 v2 v3 ->
               coe
                 MAlonzo.Code.Function.Related.du_'8596''8658'_238
                 (MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
                 (coe du_'215''45'distrib'691''45''8846'_186)))
         (coe
            (\ v1 ->
               coe
                 MAlonzo.Code.Function.Related.du_'8596''8658'_238
                 (MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
                 (coe du_'215''45'zero'737'_84))))
-- Function.Related.TypeIsomorphisms.×-⊎-commutativeSemiring
d_'215''45''8846''45'commutativeSemiring_390 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_'215''45''8846''45'commutativeSemiring_390 v0 ~v1
  = du_'215''45''8846''45'commutativeSemiring_390 v0
du_'215''45''8846''45'commutativeSemiring_390 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_'215''45''8846''45'commutativeSemiring_390 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiring'46'constructor_38603
      erased erased erased erased
      (coe du_'215''45''8846''45'isCommutativeSemiring_376 (coe v0))
-- Function.Related.TypeIsomorphisms.ΠΠ↔ΠΠ
d_ΠΠ'8596'ΠΠ_416 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
d_ΠΠ'8596'ΠΠ_416 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 = du_ΠΠ'8596'ΠΠ_416
du_ΠΠ'8596'ΠΠ_416 :: MAlonzo.Code.Function.Inverse.T_Inverse_58
du_ΠΠ'8596'ΠΠ_416
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe (\ v0 v1 v2 -> coe v0 v2 v1))
      (coe (\ v0 v1 v2 -> coe v0 v2 v1)) erased erased
-- Function.Related.TypeIsomorphisms.∃∃↔∃∃
d_'8707''8707''8596''8707''8707'_442 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'8707''8707''8596''8707''8707'_442 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'8707''8707''8596''8707''8707'_442
du_'8707''8707''8596''8707''8707'_442 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'8707''8707''8596''8707''8707'_442
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156 (coe du_to_458)
      (coe du_from_474) erased erased
-- Function.Related.TypeIsomorphisms._.to
d_to_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_to_458 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_to_458 v6
du_to_458 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_to_458 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                    (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.TypeIsomorphisms._.from
d_from_474 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_from_474 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_from_474 v6
du_from_474 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_from_474 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                    (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v4))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.TypeIsomorphisms.Π↔Π
d_Π'8596'Π_498 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> ()) -> MAlonzo.Code.Function.Inverse.T_Inverse_58
d_Π'8596'Π_498 ~v0 ~v1 ~v2 ~v3 = du_Π'8596'Π_498
du_Π'8596'Π_498 :: MAlonzo.Code.Function.Inverse.T_Inverse_58
du_Π'8596'Π_498
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156
      (coe (\ v0 v1 -> coe v0 v1)) (coe (\ v0 v1 -> coe v0 v1)) erased
      erased
-- Function.Related.TypeIsomorphisms._→-cong-⇔_
d__'8594''45'cong'45''8660'__528 ::
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
d__'8594''45'cong'45''8660'__528 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
                                 v9
  = du__'8594''45'cong'45''8660'__528 v8 v9
du__'8594''45'cong'45''8660'__528 ::
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du__'8594''45'cong'45''8660'__528 v0 v1
  = coe
      MAlonzo.Code.Function.Equivalence.du_equivalence_56
      (coe
         (\ v2 v3 ->
            coe
              MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
              (MAlonzo.Code.Function.Equivalence.d_to_34 (coe v1))
              (coe
                 v2
                 (coe
                    MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                    (MAlonzo.Code.Function.Equivalence.d_from_36 (coe v0)) v3))))
      (coe
         (\ v2 v3 ->
            coe
              MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
              (MAlonzo.Code.Function.Equivalence.d_from_36 (coe v1))
              (coe
                 v2
                 (coe
                    MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                    (MAlonzo.Code.Function.Equivalence.d_to_34 (coe v0)) v3))))
-- Function.Related.TypeIsomorphisms.→-cong
d_'8594''45'cong_560 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() ->
   (AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (() ->
   (AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  () -> () -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d_'8594''45'cong_560 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_'8594''45'cong_560 v4 v5 v6
du_'8594''45'cong_560 ::
  (() ->
   (AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (() ->
   (AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  () -> () -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du_'8594''45'cong_560 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Function.Related.C_equivalence_252
        -> coe
             (\ v3 v4 v5 v6 v7 v8 ->
                coe du__'8594''45'cong'45''8660'__528 (coe v7) (coe v8))
      MAlonzo.Code.Function.Related.C_bijection_254
        -> coe
             (\ v3 v4 v5 v6 v7 v8 ->
                coe
                  MAlonzo.Code.Function.Inverse.C_Inverse'46'constructor_3553
                  (coe
                     MAlonzo.Code.Function.Equivalence.d_to_34
                     (coe du_A'8594'C'8660'B'8594'D_582 (coe v7) (coe v8)))
                  (coe
                     MAlonzo.Code.Function.Equivalence.d_from_36
                     (coe du_A'8594'C'8660'B'8594'D_582 (coe v7) (coe v8)))
                  (coe
                     MAlonzo.Code.Function.Inverse.C__InverseOf_'46'constructor_2103
                     (coe
                        (\ v9 ->
                           coe
                             v0 v3 (\ v10 -> v5)
                             (coe
                                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                                (MAlonzo.Code.Function.Equivalence.d_from_36
                                   (coe du_A'8594'C'8660'B'8594'D_582 (coe v7) (coe v8)))
                                (coe
                                   MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                                   (MAlonzo.Code.Function.Equivalence.d_to_34
                                      (coe du_A'8594'C'8660'B'8594'D_582 (coe v7) (coe v8)))
                                   v9))
                             v9 erased))
                     (coe
                        (\ v9 ->
                           coe
                             v1 v4 (\ v10 -> v6)
                             (coe
                                MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                                (MAlonzo.Code.Function.Equivalence.d_to_34
                                   (coe du_A'8594'C'8660'B'8594'D_582 (coe v7) (coe v8)))
                                (coe
                                   MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                                   (MAlonzo.Code.Function.Equivalence.d_from_36
                                      (coe du_A'8594'C'8660'B'8594'D_582 (coe v7) (coe v8)))
                                   v9))
                             v9 erased))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.TypeIsomorphisms._.A→C⇔B→D
d_A'8594'C'8660'B'8594'D_582 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() ->
   (AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (() ->
   (AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  () ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_A'8594'C'8660'B'8594'D_582 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                             ~v9 v10 v11
  = du_A'8594'C'8660'B'8594'D_582 v10 v11
du_A'8594'C'8660'B'8594'D_582 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du_A'8594'C'8660'B'8594'D_582 v0 v1
  = coe
      du__'8594''45'cong'45''8660'__528
      (coe
         MAlonzo.Code.Function.Related.du_'8596''8658'_238
         (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
         v0)
      (coe
         MAlonzo.Code.Function.Related.du_'8596''8658'_238
         (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
         v1)
-- Function.Related.TypeIsomorphisms.¬-cong-⇔
d_'172''45'cong'45''8660'_600 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_'172''45'cong'45''8660'_600 ~v0 ~v1 ~v2 ~v3 v4
  = du_'172''45'cong'45''8660'_600 v4
du_'172''45'cong'45''8660'_600 ::
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du_'172''45'cong'45''8660'_600 v0
  = coe
      du__'8594''45'cong'45''8660'__528 (coe v0)
      (coe MAlonzo.Code.Function.Equivalence.du_id_66)
-- Function.Related.TypeIsomorphisms.¬-cong
d_'172''45'cong_614 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() ->
   (AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (() ->
   (AgdaAny -> ()) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> AgdaAny) ->
   (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  () -> () -> AgdaAny -> AgdaAny
d_'172''45'cong_614 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 v7
  = du_'172''45'cong_614 v4 v7
du_'172''45'cong_614 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  AgdaAny -> AgdaAny
du_'172''45'cong_614 v0 v1
  = coe
      du_'8594''45'cong_560 erased erased v0 erased erased erased erased
      v1
      (coe
         MAlonzo.Code.Function.Related.du_K'45'reflexive_368
         (coe MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0)))
-- Function.Related.TypeIsomorphisms.Related-cong
d_Related'45'cong_640 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_Related'45'cong_640 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du_Related'45'cong_640 v0 v9 v10
du_Related'45'cong_640 ::
  MAlonzo.Code.Function.Related.T_Symmetric'45'kind_250 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du_Related'45'cong_640 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Equivalence.du_equivalence_56
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Function.Related.du__'8764''10216'_'10217'__462
              (coe MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
              (coe MAlonzo.Code.Function.Related.du_SK'45'sym_394 v0 v1)
              (coe
                 MAlonzo.Code.Function.Related.du__'8764''10216'_'10217'__462
                 (coe MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
                 (coe v3)
                 (coe
                    MAlonzo.Code.Function.Related.du__'8764''10216'_'10217'__462
                    (coe MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
                    (coe v2)
                    (coe
                       MAlonzo.Code.Function.Related.du__'8718'_552
                       (coe
                          MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0)))))))
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Function.Related.du__'8764''10216'_'10217'__462
              (coe MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
              (coe v1)
              (coe
                 MAlonzo.Code.Function.Related.du__'8764''10216'_'10217'__462
                 (coe MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
                 (coe v3)
                 (coe
                    MAlonzo.Code.Function.Related.du__'8764''10216'_'10217'__462
                    (coe MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0))
                    (coe MAlonzo.Code.Function.Related.du_SK'45'sym_394 v0 v2)
                    (coe
                       MAlonzo.Code.Function.Related.du__'8718'_552
                       (coe
                          MAlonzo.Code.Function.Related.d_'8970'_'8971'_256 (coe v0)))))))
-- Function.Related.TypeIsomorphisms.True↔
d_True'8596'_672 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  (AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
d_True'8596'_672 ~v0 ~v1 v2 ~v3 = du_True'8596'_672 v2
du_True'8596'_672 ::
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_True'8596'_672 v0
  = case coe v0 of
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v1 v2
        -> if coe v1
             then coe
                    MAlonzo.Code.Function.Inverse.du_inverse_156
                    (coe
                       (\ v3 ->
                          coe MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42 (coe v2)))
                    (coe (\ v3 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)) erased
                    erased
             else coe
                    seq (coe v2)
                    (coe
                       MAlonzo.Code.Function.Inverse.du_inverse_156 erased
                       (coe
                          MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42
                          (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
                       erased erased)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Related.TypeIsomorphisms._.Σ-≡,≡→≡
d_Σ'45''8801''44''8801''8594''8801'_704 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_Σ'45''8801''44''8801''8594''8801'_704 = erased
-- Function.Related.TypeIsomorphisms._.Σ-≡,≡←≡
d_Σ'45''8801''44''8801''8592''8801'_708 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_Σ'45''8801''44''8801''8592''8801'_708 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6
  = du_Σ'45''8801''44''8801''8592''8801'_708
du_Σ'45''8801''44''8801''8592''8801'_708 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_Σ'45''8801''44''8801''8592''8801'_708
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Function.Related.TypeIsomorphisms._.left-inverse-of
d_left'45'inverse'45'of_714 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'inverse'45'of_714 = erased
-- Function.Related.TypeIsomorphisms._.right-inverse-of
d_right'45'inverse'45'of_718 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'inverse'45'of_718 = erased
-- Function.Related.TypeIsomorphisms._.Σ-≡,≡↔≡
d_Σ'45''8801''44''8801''8596''8801'_722 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
d_Σ'45''8801''44''8801''8596''8801'_722 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_Σ'45''8801''44''8801''8596''8801'_722
du_Σ'45''8801''44''8801''8596''8801'_722 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_Σ'45''8801''44''8801''8596''8801'_722
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156 erased
      (\ v0 -> coe du_Σ'45''8801''44''8801''8592''8801'_708) erased
      erased
-- Function.Related.TypeIsomorphisms._.×-≡,≡→≡
d_'215''45''8801''44''8801''8594''8801'_740 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'215''45''8801''44''8801''8594''8801'_740 = erased
-- Function.Related.TypeIsomorphisms._.×-≡,≡←≡
d_'215''45''8801''44''8801''8592''8801'_742 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'215''45''8801''44''8801''8592''8801'_742 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
                                            ~v6
  = du_'215''45''8801''44''8801''8592''8801'_742
du_'215''45''8801''44''8801''8592''8801'_742 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'215''45''8801''44''8801''8592''8801'_742
  = coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 erased erased
-- Function.Related.TypeIsomorphisms._.left-inverse-of
d_left'45'inverse'45'of_746 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'inverse'45'of_746 = erased
-- Function.Related.TypeIsomorphisms._.right-inverse-of
d_right'45'inverse'45'of_750 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'inverse'45'of_750 = erased
-- Function.Related.TypeIsomorphisms._.×-≡,≡↔≡
d_'215''45''8801''44''8801''8596''8801'_752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'215''45''8801''44''8801''8596''8801'_752 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_'215''45''8801''44''8801''8596''8801'_752
du_'215''45''8801''44''8801''8596''8801'_752 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'215''45''8801''44''8801''8596''8801'_752
  = coe
      MAlonzo.Code.Function.Inverse.du_inverse_156 erased
      (\ v0 -> coe du_'215''45''8801''44''8801''8592''8801'_742) erased
      erased
-- Function.Related.TypeIsomorphisms.×-≡×≡↔≡,≡
d_'215''45''8801''215''8801''8596''8801''44''8801'_768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
d_'215''45''8801''215''8801''8596''8801''44''8801'_768 ~v0 ~v1 ~v2
                                                       ~v3 ~v4 ~v5 ~v6
  = du_'215''45''8801''215''8801''8596''8801''44''8801'_768
du_'215''45''8801''215''8801''8596''8801''44''8801'_768 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du_'215''45''8801''215''8801''8596''8801''44''8801'_768
  = coe du_'215''45''8801''44''8801''8596''8801'_752
