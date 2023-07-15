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

module MAlonzo.Code.Data.Product.Algebra where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Empty.Polymorphic
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Sum.Algebra
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.Unit.Polymorphic.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Properties.Inverse

-- Data.Product.Algebra.Σ-assoc
d_Σ'45'assoc_60 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_Σ'45'assoc_60 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 = du_Σ'45'assoc_60
du_Σ'45'assoc_60 :: MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_Σ'45'assoc_60
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe MAlonzo.Code.Data.Product.Base.du_assoc'691'_236)
      (coe MAlonzo.Code.Data.Product.Base.du_assoc'737'_252) erased
      erased
-- Data.Product.Algebra.Σ-assoc-alt
d_Σ'45'assoc'45'alt_68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()) ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_Σ'45'assoc'45'alt_68 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_Σ'45'assoc'45'alt_68
du_Σ'45'assoc'45'alt_68 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_Σ'45'assoc'45'alt_68
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe MAlonzo.Code.Data.Product.Base.du_assoc'691''45'curried_266)
      (coe MAlonzo.Code.Data.Product.Base.du_assoc'737''45'curried_280)
      erased erased
-- Data.Product.Algebra.×-cong
d_'215''45'cong_70 ::
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
d_'215''45'cong_70 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du_'215''45'cong_70 v8 v9
du_'215''45'cong_70 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'215''45'cong_70 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe
         MAlonzo.Code.Data.Product.Base.du_map_104
         (coe MAlonzo.Code.Function.Bundles.d_to_1064 (coe v0))
         (coe (\ v2 -> MAlonzo.Code.Function.Bundles.d_to_1064 (coe v1))))
      (coe
         MAlonzo.Code.Data.Product.Base.du_map_104
         (coe MAlonzo.Code.Function.Bundles.d_from_1066 (coe v0))
         (coe (\ v2 -> MAlonzo.Code.Function.Bundles.d_from_1066 (coe v1))))
      erased erased
-- Data.Product.Algebra.×-comm
d_'215''45'comm_244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'215''45'comm_244 ~v0 ~v1 ~v2 ~v3 = du_'215''45'comm_244
du_'215''45'comm_244 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'215''45'comm_244
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe MAlonzo.Code.Data.Product.Base.du_swap_346)
      (coe MAlonzo.Code.Data.Product.Base.du_swap_346) erased erased
-- Data.Product.Algebra._.×-assoc
d_'215''45'assoc_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'215''45'assoc_252 ~v0 ~v1 ~v2 ~v3 = du_'215''45'assoc_252
du_'215''45'assoc_252 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'215''45'assoc_252
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe MAlonzo.Code.Data.Product.Base.du_assoc'691''8242'_364)
      (coe MAlonzo.Code.Data.Product.Base.du_assoc'737''8242'_372) erased
      erased
-- Data.Product.Algebra._.×-identityˡ
d_'215''45'identity'737'_254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'215''45'identity'737'_254 ~v0 ~v1
  = du_'215''45'identity'737'_254
du_'215''45'identity'737'_254 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'215''45'identity'737'_254
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v0)))
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
              (coe MAlonzo.Code.Data.Unit.Polymorphic.Base.du_tt_16) (coe v0)))
      erased erased
-- Data.Product.Algebra._.×-identityʳ
d_'215''45'identity'691'_258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'215''45'identity'691'_258 ~v0 ~v1
  = du_'215''45'identity'691'_258
du_'215''45'identity'691'_258 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'215''45'identity'691'_258
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0)))
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v0)
              (coe MAlonzo.Code.Data.Unit.Polymorphic.Base.du_tt_16)))
      erased erased
-- Data.Product.Algebra._.×-identity
d_'215''45'identity_262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'215''45'identity_262 ~v0 = du_'215''45'identity_262
du_'215''45'identity_262 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'215''45'identity_262
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 -> coe du_'215''45'identity'737'_254)
      (\ v0 -> coe du_'215''45'identity'691'_258)
-- Data.Product.Algebra._.×-zeroˡ
d_'215''45'zero'737'_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'215''45'zero'737'_264 ~v0 ~v1 = du_'215''45'zero'737'_264
du_'215''45'zero'737'_264 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'215''45'zero'737'_264
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0)))
      (\ v0 ->
         coe MAlonzo.Code.Data.Empty.Polymorphic.du_'8869''45'elim_20)
      erased erased
-- Data.Product.Algebra._.×-zeroʳ
d_'215''45'zero'691'_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'215''45'zero'691'_268 ~v0 ~v1 = du_'215''45'zero'691'_268
du_'215''45'zero'691'_268 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'215''45'zero'691'_268
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
      (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v0)))
      (\ v0 ->
         coe MAlonzo.Code.Data.Empty.Polymorphic.du_'8869''45'elim_20)
      erased erased
-- Data.Product.Algebra._.×-zero
d_'215''45'zero_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'215''45'zero_272 ~v0 = du_'215''45'zero_272
du_'215''45'zero_272 :: MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'215''45'zero_272
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 -> coe du_'215''45'zero'737'_264)
      (\ v0 -> coe du_'215''45'zero'691'_268)
-- Data.Product.Algebra._.×-distribˡ-⊎
d_'215''45'distrib'737''45''8846'_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'215''45'distrib'737''45''8846'_274 ~v0 ~v1 ~v2 ~v3
  = du_'215''45'distrib'737''45''8846'_274
du_'215''45'distrib'737''45''8846'_274 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'215''45'distrib'737''45''8846'_274
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
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
-- Data.Product.Algebra._.×-distribʳ-⊎
d_'215''45'distrib'691''45''8846'_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'215''45'distrib'691''45''8846'_284 ~v0 ~v1 ~v2 ~v3
  = du_'215''45'distrib'691''45''8846'_284
du_'215''45'distrib'691''45''8846'_284 ::
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'215''45'distrib'691''45''8846'_284
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8596''8242'_1386
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
-- Data.Product.Algebra._.×-distrib-⊎
d_'215''45'distrib'45''8846'_290 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'215''45'distrib'45''8846'_290 ~v0
  = du_'215''45'distrib'45''8846'_290
du_'215''45'distrib'45''8846'_290 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_'215''45'distrib'45''8846'_290
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (\ v0 v1 v2 -> coe du_'215''45'distrib'737''45''8846'_274)
      (\ v0 v1 v2 -> coe du_'215''45'distrib'691''45''8846'_284)
-- Data.Product.Algebra._.×-isMagma
d_'215''45'isMagma_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_'215''45'isMagma_292 ~v0 = du_'215''45'isMagma_292
du_'215''45'isMagma_292 ::
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_'215''45'isMagma_292
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe
         MAlonzo.Code.Function.Properties.Inverse.du_'8596''45'isEquivalence_32)
      (coe (\ v0 v1 v2 v3 v4 v5 -> coe du_'215''45'cong_70 v4 v5))
-- Data.Product.Algebra._.×-isSemigroup
d_'215''45'isSemigroup_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_'215''45'isSemigroup_294 ~v0 = du_'215''45'isSemigroup_294
du_'215''45'isSemigroup_294 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_'215''45'isSemigroup_294
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe du_'215''45'isMagma_292)
      (coe (\ v0 v1 v2 -> coe du_Σ'45'assoc_60))
-- Data.Product.Algebra._.×-isMonoid
d_'215''45'isMonoid_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
d_'215''45'isMonoid_302 ~v0 = du_'215''45'isMonoid_302
du_'215''45'isMonoid_302 ::
  MAlonzo.Code.Algebra.Structures.T_IsMonoid_600
du_'215''45'isMonoid_302
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMonoid'46'constructor_13559
      (coe du_'215''45'isSemigroup_294)
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
         (\ v0 -> coe du_'215''45'identity'737'_254)
         (\ v0 -> coe du_'215''45'identity'691'_258))
-- Data.Product.Algebra._.×-isCommutativeMonoid
d_'215''45'isCommutativeMonoid_304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
d_'215''45'isCommutativeMonoid_304 ~v0
  = du_'215''45'isCommutativeMonoid_304
du_'215''45'isCommutativeMonoid_304 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeMonoid_650
du_'215''45'isCommutativeMonoid_304
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeMonoid'46'constructor_15379
      (coe du_'215''45'isMonoid_302)
      (\ v0 v1 -> coe du_'215''45'comm_244)
-- Data.Product.Algebra._.⊎-×-isSemiringWithoutAnnihilatingZero
d_'8846''45''215''45'isSemiringWithoutAnnihilatingZero_306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
d_'8846''45''215''45'isSemiringWithoutAnnihilatingZero_306 ~v0
  = du_'8846''45''215''45'isSemiringWithoutAnnihilatingZero_306
du_'8846''45''215''45'isSemiringWithoutAnnihilatingZero_306 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiringWithoutAnnihilatingZero_1270
du_'8846''45''215''45'isSemiringWithoutAnnihilatingZero_306
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiringWithoutAnnihilatingZero'46'constructor_38063
      (coe
         MAlonzo.Code.Data.Sum.Algebra.du_'8846''45'isCommutativeMonoid_242)
      (coe (\ v0 v1 v2 v3 v4 v5 -> coe du_'215''45'cong_70 v4 v5))
      (\ v0 v1 v2 -> coe du_'215''45'assoc_252)
      (coe du_'215''45'identity_262)
      (coe du_'215''45'distrib'45''8846'_290)
-- Data.Product.Algebra._.⊎-×-isSemiring
d_'8846''45''215''45'isSemiring_308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
d_'8846''45''215''45'isSemiring_308 ~v0
  = du_'8846''45''215''45'isSemiring_308
du_'8846''45''215''45'isSemiring_308 ::
  MAlonzo.Code.Algebra.Structures.T_IsSemiring_1372
du_'8846''45''215''45'isSemiring_308
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemiring'46'constructor_42303
      (coe du_'8846''45''215''45'isSemiringWithoutAnnihilatingZero_306)
      (coe du_'215''45'zero_272)
-- Data.Product.Algebra._.⊎-×-isCommutativeSemiring
d_'8846''45''215''45'isCommutativeSemiring_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
d_'8846''45''215''45'isCommutativeSemiring_310 ~v0
  = du_'8846''45''215''45'isCommutativeSemiring_310
du_'8846''45''215''45'isCommutativeSemiring_310 ::
  MAlonzo.Code.Algebra.Structures.T_IsCommutativeSemiring_1480
du_'8846''45''215''45'isCommutativeSemiring_310
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsCommutativeSemiring'46'constructor_46125
      (coe du_'8846''45''215''45'isSemiring_308)
      (\ v0 v1 -> coe du_'215''45'comm_244)
-- Data.Product.Algebra._.×-magma
d_'215''45'magma_312 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Magma_8
d_'215''45'magma_312 ~v0 = du_'215''45'magma_312
du_'215''45'magma_312 :: MAlonzo.Code.Algebra.Bundles.T_Magma_8
du_'215''45'magma_312
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Magma'46'constructor_187 erased
      (coe du_'215''45'isMagma_292)
-- Data.Product.Algebra._.×-semigroup
d_'215''45'semigroup_314 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
d_'215''45'semigroup_314 ~v0 = du_'215''45'semigroup_314
du_'215''45'semigroup_314 ::
  MAlonzo.Code.Algebra.Bundles.T_Semigroup_476
du_'215''45'semigroup_314
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Semigroup'46'constructor_8557 erased
      (coe du_'215''45'isSemigroup_294)
-- Data.Product.Algebra._.×-monoid
d_'215''45'monoid_316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740
d_'215''45'monoid_316 ~v0 = du_'215''45'monoid_316
du_'215''45'monoid_316 :: MAlonzo.Code.Algebra.Bundles.T_Monoid_740
du_'215''45'monoid_316
  = coe
      MAlonzo.Code.Algebra.Bundles.C_Monoid'46'constructor_13309 erased
      erased (coe du_'215''45'isMonoid_302)
-- Data.Product.Algebra._.×-commutativeMonoid
d_'215''45'commutativeMonoid_318 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_'215''45'commutativeMonoid_318 ~v0
  = du_'215''45'commutativeMonoid_318
du_'215''45'commutativeMonoid_318 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
du_'215''45'commutativeMonoid_318
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeMonoid'46'constructor_15055
      erased erased (coe du_'215''45'isCommutativeMonoid_304)
-- Data.Product.Algebra._.×-⊎-commutativeSemiring
d_'215''45''8846''45'commutativeSemiring_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
d_'215''45''8846''45'commutativeSemiring_320 ~v0
  = du_'215''45''8846''45'commutativeSemiring_320
du_'215''45''8846''45'commutativeSemiring_320 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeSemiring_2152
du_'215''45''8846''45'commutativeSemiring_320
  = coe
      MAlonzo.Code.Algebra.Bundles.C_CommutativeSemiring'46'constructor_38603
      erased erased erased erased
      (coe du_'8846''45''215''45'isCommutativeSemiring_310)
