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

module MAlonzo.Code.Data.Product.Function.NonDependent.Setoid where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Function.Bijection
import qualified MAlonzo.Code.Function.Equality
import qualified MAlonzo.Code.Function.Equivalence
import qualified MAlonzo.Code.Function.Injection
import qualified MAlonzo.Code.Function.Inverse
import qualified MAlonzo.Code.Function.LeftInverse
import qualified MAlonzo.Code.Function.Surjection
import qualified MAlonzo.Code.Relation.Binary.Bundles

-- Data.Product.Function.NonDependent.Setoid._._×-⟶_
d__'215''45''10230'__38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16
d__'215''45''10230'__38 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                        ~v10 ~v11 v12 v13
  = du__'215''45''10230'__38 v12 v13
du__'215''45''10230'__38 ::
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16
du__'215''45''10230'__38 v0 v1
  = coe
      MAlonzo.Code.Function.Equality.C_Π'46'constructor_1167
      (coe du_fg_56 (coe v0) (coe v1))
      (coe du_fg'45'cong_62 (coe v0) (coe v1))
-- Data.Product.Function.NonDependent.Setoid._._._._≈_
d__'8776'__50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d__'8776'__50 = erased
-- Data.Product.Function.NonDependent.Setoid._._._._≈_
d__'8776'__54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> ()
d__'8776'__54 = erased
-- Data.Product.Function.NonDependent.Setoid._._.fg
d_fg_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_fg_56 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
  = du_fg_56 v12 v13
du_fg_56 ::
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_fg_56 v0 v1
  = coe
      MAlonzo.Code.Data.Product.Base.du_map_104
      (coe
         MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v0))
      (coe
         (\ v2 ->
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v1)))
-- Data.Product.Function.NonDependent.Setoid._._.fg-cong
d_fg'45'cong_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_fg'45'cong_62 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11
                v12 v13 v14 v15 v16
  = du_fg'45'cong_62 v12 v13 v14 v15 v16
du_fg'45'cong_62 ::
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_fg'45'cong_62 v0 v1 v2 v3 v4
  = case coe v4 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                MAlonzo.Code.Function.Equality.d_cong_40 v0
                (coe
                   MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                   (\ v7 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7))
                   (\ v7 v8 -> v7) v2 v3)
                (coe
                   MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                   (\ v7 v8 -> v8)
                   (\ v7 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7)) v2 v3)
                v5)
             (coe
                MAlonzo.Code.Function.Equality.d_cong_40 v1
                (coe
                   MAlonzo.Code.Function.Base.du__'45''10216'_'8739'_292
                   (\ v7 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v7))
                   (\ v7 v8 -> v7) v2 v3)
                (coe
                   MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                   (\ v7 v8 -> v8)
                   (\ v7 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v7)) v2 v3)
                v6)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Product.Function.NonDependent.Setoid._.<_,_>ₛ
d_'60'_'44'_'62''8347'_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16
d_'60'_'44'_'62''8347'_90 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
                          v10
  = du_'60'_'44'_'62''8347'_90 v9 v10
du_'60'_'44'_'62''8347'_90 ::
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  MAlonzo.Code.Function.Equality.T_Π_16
du_'60'_'44'_'62''8347'_90 v0 v1
  = coe
      MAlonzo.Code.Function.Equality.C_Π'46'constructor_1167
      (coe
         MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88
         (coe
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v0))
         (coe
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 (coe v1)))
      (coe
         (\ v2 v3 ->
            coe
              MAlonzo.Code.Data.Product.Base.du_'60'_'44'_'62'_88
              (coe MAlonzo.Code.Function.Equality.d_cong_40 v0 v2 v3)
              (coe MAlonzo.Code.Function.Equality.d_cong_40 v1 v2 v3)))
-- Data.Product.Function.NonDependent.Setoid._.proj₁ₛ
d_proj'8321''8347'_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16
d_proj'8321''8347'_116 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_proj'8321''8347'_116
du_proj'8321''8347'_116 :: MAlonzo.Code.Function.Equality.T_Π_16
du_proj'8321''8347'_116
  = coe
      MAlonzo.Code.Function.Equality.C_Π'46'constructor_1167
      (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v0)))
      (coe
         (\ v0 v1 v2 -> MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v2)))
-- Data.Product.Function.NonDependent.Setoid._.proj₂ₛ
d_proj'8322''8347'_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16
d_proj'8322''8347'_118 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5
  = du_proj'8322''8347'_118
du_proj'8322''8347'_118 :: MAlonzo.Code.Function.Equality.T_Π_16
du_proj'8322''8347'_118
  = coe
      MAlonzo.Code.Function.Equality.C_Π'46'constructor_1167
      (coe (\ v0 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v0)))
      (coe
         (\ v0 v1 v2 -> MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v2)))
-- Data.Product.Function.NonDependent.Setoid._.swapₛ
d_swap'8347'_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16
d_swap'8347'_120 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 = du_swap'8347'_120
du_swap'8347'_120 :: MAlonzo.Code.Function.Equality.T_Π_16
du_swap'8347'_120
  = coe
      du_'60'_'44'_'62''8347'_90 (coe du_proj'8322''8347'_118)
      (coe du_proj'8321''8347'_116)
-- Data.Product.Function.NonDependent.Setoid._._×-equivalence_
d__'215''45'equivalence__150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d__'215''45'equivalence__150 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8
                             ~v9 ~v10 ~v11 v12 v13
  = du__'215''45'equivalence__150 v12 v13
du__'215''45'equivalence__150 ::
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du__'215''45'equivalence__150 v0 v1
  = coe
      MAlonzo.Code.Function.Equivalence.C_Equivalence'46'constructor_433
      (coe
         du__'215''45''10230'__38
         (coe MAlonzo.Code.Function.Equivalence.d_to_34 (coe v0))
         (coe MAlonzo.Code.Function.Equivalence.d_to_34 (coe v1)))
      (coe
         du__'215''45''10230'__38
         (coe MAlonzo.Code.Function.Equivalence.d_from_36 (coe v0))
         (coe MAlonzo.Code.Function.Equivalence.d_from_36 (coe v1)))
-- Data.Product.Function.NonDependent.Setoid._._×-injection_
d__'215''45'injection__160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88
d__'215''45'injection__160 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                           ~v10 ~v11 v12 v13
  = du__'215''45'injection__160 v12 v13
du__'215''45'injection__160 ::
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88 ->
  MAlonzo.Code.Function.Injection.T_Injection_88
du__'215''45'injection__160 v0 v1
  = coe
      MAlonzo.Code.Function.Injection.C_Injection'46'constructor_3039
      (coe
         du__'215''45''10230'__38
         (coe MAlonzo.Code.Function.Injection.d_to_106 (coe v0))
         (coe MAlonzo.Code.Function.Injection.d_to_106 (coe v1)))
      (coe
         (\ v2 v3 ->
            coe
              MAlonzo.Code.Data.Product.Base.du_map_104
              (coe
                 MAlonzo.Code.Function.Injection.d_injective_108 v0
                 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v2))
                 (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3)))
              (coe
                 (\ v4 ->
                    coe
                      MAlonzo.Code.Function.Injection.d_injective_108 v1
                      (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v2))
                      (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v3))))))
-- Data.Product.Function.NonDependent.Setoid._._×-left-inverse_
d__'215''45'left'45'inverse__170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d__'215''45'left'45'inverse__170 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
                                 ~v8 ~v9 ~v10 ~v11 v12 v13
  = du__'215''45'left'45'inverse__170 v12 v13
du__'215''45'left'45'inverse__170 ::
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du__'215''45'left'45'inverse__170 v0 v1
  = coe
      MAlonzo.Code.Function.LeftInverse.C_LeftInverse'46'constructor_4525
      (coe
         MAlonzo.Code.Function.Equivalence.d_to_34
         (coe du_eq_180 (coe v0) (coe v1)))
      (coe
         MAlonzo.Code.Function.Equivalence.d_from_36
         (coe du_eq_180 (coe v0) (coe v1)))
      (coe du_left_182 (coe v0) (coe v1))
-- Data.Product.Function.NonDependent.Setoid._._.eq
d_eq_180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_eq_180 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
  = du_eq_180 v12 v13
du_eq_180 ::
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du_eq_180 v0 v1
  = coe
      du__'215''45'equivalence__150
      (coe MAlonzo.Code.Function.LeftInverse.du_equivalence_186 (coe v0))
      (coe MAlonzo.Code.Function.LeftInverse.du_equivalence_186 (coe v1))
-- Data.Product.Function.NonDependent.Setoid._._.left
d_left_182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_left_182 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12
           v13 v14
  = du_left_182 v12 v13 v14
du_left_182 ::
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_left_182 v0 v1 v2
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
             (coe
                MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106 v0
                v3)
             (coe
                MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106 v1
                v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Product.Function.NonDependent.Setoid._._×-surjection_
d__'215''45'surjection__216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54
d__'215''45'surjection__216 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
                            ~v10 ~v11 v12 v13
  = du__'215''45'surjection__216 v12 v13
du__'215''45'surjection__216 ::
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54
du__'215''45'surjection__216 v0 v1
  = coe
      MAlonzo.Code.Function.Surjection.C_Surjection'46'constructor_2365
      (coe
         MAlonzo.Code.Function.LeftInverse.d_from_104
         (coe du_inv_226 (coe v0) (coe v1)))
      (coe
         MAlonzo.Code.Function.Surjection.C_Surjective'46'constructor_1227
         (coe
            MAlonzo.Code.Function.LeftInverse.d_to_102
            (coe du_inv_226 (coe v0) (coe v1)))
         (coe
            MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106
            (coe du_inv_226 (coe v0) (coe v1))))
-- Data.Product.Function.NonDependent.Setoid._._.inv
d_inv_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d_inv_226 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
  = du_inv_226 v12 v13
du_inv_226 ::
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du_inv_226 v0 v1
  = coe
      du__'215''45'left'45'inverse__170
      (coe
         MAlonzo.Code.Function.Surjection.du_right'45'inverse_82 (coe v0))
      (coe
         MAlonzo.Code.Function.Surjection.du_right'45'inverse_82 (coe v1))
-- Data.Product.Function.NonDependent.Setoid._._×-inverse_
d__'215''45'inverse__228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
d__'215''45'inverse__228 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9 v10
                         ~v11 v12 v13
  = du__'215''45'inverse__228 v8 v10 v12 v13
du__'215''45'inverse__228 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58
du__'215''45'inverse__228 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.Inverse.C_Inverse'46'constructor_3553
      (coe
         MAlonzo.Code.Function.Surjection.d_to_72
         (coe du_surj_238 (coe v0) (coe v1) (coe v2) (coe v3)))
      (coe
         MAlonzo.Code.Function.Surjection.d_from_38
         (coe
            MAlonzo.Code.Function.Surjection.d_surjective_74
            (coe du_surj_238 (coe v0) (coe v1) (coe v2) (coe v3))))
      (coe
         MAlonzo.Code.Function.Inverse.C__InverseOf_'46'constructor_2103
         (coe
            MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106
            (coe du_inv_240 (coe v2) (coe v3)))
         (coe
            MAlonzo.Code.Function.Surjection.d_right'45'inverse'45'of_40
            (coe
               MAlonzo.Code.Function.Surjection.d_surjective_74
               (coe du_surj_238 (coe v0) (coe v1) (coe v2) (coe v3)))))
-- Data.Product.Function.NonDependent.Setoid._._.surj
d_surj_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54
d_surj_238 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9 v10 ~v11 v12 v13
  = du_surj_238 v8 v10 v12 v13
du_surj_238 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Surjection.T_Surjection_54
du_surj_238 v0 v1 v2 v3
  = coe
      du__'215''45'surjection__216
      (coe
         MAlonzo.Code.Function.Bijection.du_surjection_100
         (coe
            MAlonzo.Code.Function.Inverse.du_bijection_98 (coe v0) (coe v2)))
      (coe
         MAlonzo.Code.Function.Bijection.du_surjection_100
         (coe
            MAlonzo.Code.Function.Inverse.du_bijection_98 (coe v1) (coe v3)))
-- Data.Product.Function.NonDependent.Setoid._._.inv
d_inv_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d_inv_240 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
  = du_inv_240 v12 v13
du_inv_240 ::
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.Inverse.T_Inverse_58 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du_inv_240 v0 v1
  = coe
      du__'215''45'left'45'inverse__170
      (coe MAlonzo.Code.Function.Inverse.du_left'45'inverse_90 (coe v0))
      (coe MAlonzo.Code.Function.Inverse.du_left'45'inverse_90 (coe v1))
