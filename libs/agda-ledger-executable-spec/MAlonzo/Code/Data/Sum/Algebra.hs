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

module MAlonzo.Code.Data.Sum.Algebra where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Properties.Inverse
import qualified MAlonzo.Code.Level

-- Data.Sum.Algebra.♯
d_'9839'_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (MAlonzo.Code.Level.T_Lift_8 -> ()) ->
  MAlonzo.Code.Level.T_Lift_8 -> AgdaAny
d_'9839'_54 ~v0 ~v1 ~v2 ~v3 = du_'9839'_54
du_'9839'_54 :: AgdaAny
du_'9839'_54 = MAlonzo.RTE.mazUnreachableError
-- Data.Sum.Algebra.⊎-cong
d_'8846''45'cong_56 ::
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
d_'8846''45'cong_56 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du_'8846''45'cong_56 v8 v9
du_'8846''45'cong_56 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'8846''45'cong_56 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe
         MAlonzo.Code.Data.Sum.Base.du_map_84
         (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v0))
         (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v1)))
      (coe
         MAlonzo.Code.Data.Sum.Base.du_map_84
         (coe MAlonzo.Code.Function.Bundles.d_from_1066 (coe v0))
         (coe MAlonzo.Code.Function.Bundles.d_from_1066 (coe v1)))
      erased erased
-- Data.Sum.Algebra.⊎-comm
d_'8846''45'comm_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'8846''45'comm_218 ~v0 ~v1 ~v2 ~v3 = du_'8846''45'comm_218
du_'8846''45'comm_218 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'8846''45'comm_218
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe MAlonzo.Code.Data.Sum.Base.du_swap_78)
      (coe MAlonzo.Code.Data.Sum.Base.du_swap_78) erased erased
-- Data.Sum.Algebra._.⊎-assoc
d_'8846''45'assoc_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'8846''45'assoc_226 ~v0 ~v1 ~v2 ~v3 = du_'8846''45'assoc_226
du_'8846''45'assoc_226 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'8846''45'assoc_226
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe MAlonzo.Code.Data.Sum.Base.du_assoc'691'_96)
      (coe MAlonzo.Code.Data.Sum.Base.du_assoc'737'_98) erased erased
-- Data.Sum.Algebra._.⊎-identityˡ
d_'8846''45'identity'737'_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'8846''45'identity'737'_228 ~v0 ~v1
  = du_'8846''45'identity'737'_228
du_'8846''45'identity'737'_228 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'8846''45'identity'737'_228
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52
         (\ v0 -> coe du_'9839'_54) (coe (\ v0 -> v0)))
      (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42) erased erased
-- Data.Sum.Algebra._.⊎-identityʳ
d_'8846''45'identity'691'_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'8846''45'identity'691'_232 ~v0 ~v1
  = du_'8846''45'identity'691'_232
du_'8846''45'identity'691'_232 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'8846''45'identity'691'_232
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52 (coe (\ v0 -> v0))
         (\ v0 -> coe du_'9839'_54))
      (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38) erased erased
-- Data.Sum.Algebra._.⊎-identity
d_'8846''45'identity_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'8846''45'identity_234 ~v0 = du_'8846''45'identity_234
du_'8846''45'identity_234 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'8846''45'identity_234
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 -> coe du_'8846''45'identity'737'_228)
      (\ v0 -> coe du_'8846''45'identity'691'_232)
-- Data.Sum.Algebra._.⊎-isMagma
d_'8846''45'isMagma_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'8846''45'isMagma_236 ~v0 = du_'8846''45'isMagma_236
du_'8846''45'isMagma_236 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'8846''45'isMagma_236
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Function.Properties.Inverse.du_'8596''45'isEquivalence_32)
      (coe (\ v0 v1 v2 v3 v4 v5 -> coe du_'8846''45'cong_56 v4 v5))
-- Data.Sum.Algebra._.⊎-isSemigroup
d_'8846''45'isSemigroup_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'8846''45'isSemigroup_238 ~v0 = du_'8846''45'isSemigroup_238
du_'8846''45'isSemigroup_238 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'8846''45'isSemigroup_238
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'8846''45'isMagma_236)
      (\ v0 v1 v2 -> coe du_'8846''45'assoc_226)
-- Data.Sum.Algebra._.⊎-isMonoid
d_'8846''45'isMonoid_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'8846''45'isMonoid_240 ~v0 = du_'8846''45'isMonoid_240
du_'8846''45'isMonoid_240 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'8846''45'isMonoid_240
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe du_'8846''45'isSemigroup_238)
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (\ v0 -> coe du_'8846''45'identity'737'_228)
         (\ v0 -> coe du_'8846''45'identity'691'_232))
-- Data.Sum.Algebra._.⊎-isCommutativeMonoid
d_'8846''45'isCommutativeMonoid_242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'8846''45'isCommutativeMonoid_242 ~v0
  = du_'8846''45'isCommutativeMonoid_242
du_'8846''45'isCommutativeMonoid_242 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'8846''45'isCommutativeMonoid_242
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe du_'8846''45'isMonoid_240)
      (\ v0 v1 -> coe du_'8846''45'comm_218)
-- Data.Sum.Algebra._.⊎-magma
d_'8846''45'magma_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'8846''45'magma_244 ~v0 = du_'8846''45'magma_244
du_'8846''45'magma_244 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_'8846''45'magma_244
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187 erased
      (coe du_'8846''45'isMagma_236)
-- Data.Sum.Algebra._.⊎-semigroup
d_'8846''45'semigroup_246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'8846''45'semigroup_246 ~v0 = du_'8846''45'semigroup_246
du_'8846''45'semigroup_246 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'8846''45'semigroup_246
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557 erased
      (coe du_'8846''45'isSemigroup_238)
-- Data.Sum.Algebra._.⊎-monoid
d_'8846''45'monoid_248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'8846''45'monoid_248 ~v0 = du_'8846''45'monoid_248
du_'8846''45'monoid_248 ::
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'8846''45'monoid_248
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309 erased
      erased (coe du_'8846''45'isMonoid_240)
-- Data.Sum.Algebra._.⊎-commutativeMonoid
d_'8846''45'commutativeMonoid_250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'8846''45'commutativeMonoid_250 ~v0
  = du_'8846''45'commutativeMonoid_250
du_'8846''45'commutativeMonoid_250 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'8846''45'commutativeMonoid_250
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      erased erased (coe du_'8846''45'isCommutativeMonoid_242)
