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

module MAlonzo.Code.Function.Construct.Identity where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Structures
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Construct.Identity._._.Bijective
d_Bijective_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> ()
d_Bijective_24 = erased
-- Function.Construct.Identity._._.Congruent
d_Congruent_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> ()
d_Congruent_26 = erased
-- Function.Construct.Identity._._.Injective
d_Injective_28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> ()
d_Injective_28 = erased
-- Function.Construct.Identity._._.Inverseʳ
d_Inverse'691'_30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Inverse'691'_30 = erased
-- Function.Construct.Identity._._.Inverseˡ
d_Inverse'737'_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Inverse'737'_32 = erased
-- Function.Construct.Identity._._.Inverseᵇ
d_Inverse'7495'_34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> ()
d_Inverse'7495'_34 = erased
-- Function.Construct.Identity._._.Surjective
d_Surjective_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) -> (AgdaAny -> AgdaAny) -> ()
d_Surjective_36 = erased
-- Function.Construct.Identity._.congruent
d_congruent_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_congruent_38 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_congruent_38 v6
du_congruent_38 :: AgdaAny -> AgdaAny
du_congruent_38 v0 = coe v0
-- Function.Construct.Identity._.injective
d_injective_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_40 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_injective_40 v6
du_injective_40 :: AgdaAny -> AgdaAny
du_injective_40 v0 = coe v0
-- Function.Construct.Identity._.surjective
d_surjective_42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_surjective_42 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_surjective_42 v4 v5
du_surjective_42 ::
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_surjective_42 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1) (coe v0 v1)
-- Function.Construct.Identity._.bijective
d_bijective_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_bijective_48 ~v0 ~v1 ~v2 ~v3 v4 = du_bijective_48 v4
du_bijective_48 ::
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_bijective_48 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe (\ v1 v2 v3 -> v3)) (coe du_surjective_42 (coe v0))
-- Function.Construct.Identity._.inverseˡ
d_inverse'737'_52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_inverse'737'_52 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_inverse'737'_52 v4 v5
du_inverse'737'_52 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_inverse'737'_52 v0 v1 = coe v0 v1
-- Function.Construct.Identity._.inverseʳ
d_inverse'691'_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_inverse'691'_58 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_inverse'691'_58 v4 v5
du_inverse'691'_58 :: (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_inverse'691'_58 v0 v1 = coe v0 v1
-- Function.Construct.Identity._.inverseᵇ
d_inverse'7495'_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_inverse'7495'_64 ~v0 ~v1 ~v2 ~v3 v4 = du_inverse'7495'_64 v4
du_inverse'7495'_64 ::
  (AgdaAny -> AgdaAny) -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_inverse'7495'_64 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
      (coe du_inverse'737'_52 (coe v0)) (coe du_inverse'691'_58 (coe v0))
-- Function.Construct.Identity._._.IsBijection
d_IsBijection_86 a0 a1 a2 a3 a4 a5 = ()
-- Function.Construct.Identity._._.IsCongruent
d_IsCongruent_88 a0 a1 a2 a3 a4 a5 = ()
-- Function.Construct.Identity._._.IsInjection
d_IsInjection_90 a0 a1 a2 a3 a4 a5 = ()
-- Function.Construct.Identity._._.IsInverse
d_IsInverse_92 a0 a1 a2 a3 a4 a5 a6 = ()
-- Function.Construct.Identity._._.IsLeftInverse
d_IsLeftInverse_94 a0 a1 a2 a3 a4 a5 a6 = ()
-- Function.Construct.Identity._._.IsRightInverse
d_IsRightInverse_96 a0 a1 a2 a3 a4 a5 a6 = ()
-- Function.Construct.Identity._._.IsSurjection
d_IsSurjection_98 a0 a1 a2 a3 a4 a5 = ()
-- Function.Construct.Identity._.isCongruent
d_isCongruent_678 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
d_isCongruent_678 ~v0 ~v1 ~v2 ~v3 v4 = du_isCongruent_678 v4
du_isCongruent_678 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsCongruent_22
du_isCongruent_678 v0
  = coe
      MAlonzo.Code.Function.Structures.C_IsCongruent'46'constructor_985
      (coe (\ v1 v2 v3 -> v3)) (coe v0) (coe v0)
-- Function.Construct.Identity._.isInjection
d_isInjection_680 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92
d_isInjection_680 ~v0 ~v1 ~v2 ~v3 v4 = du_isInjection_680 v4
du_isInjection_680 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsInjection_92
du_isInjection_680 v0
  = coe
      MAlonzo.Code.Function.Structures.C_IsInjection'46'constructor_3991
      (coe du_isCongruent_678 (coe v0)) (coe (\ v1 v2 v3 -> v3))
-- Function.Construct.Identity._.isSurjection
d_isSurjection_682 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162
d_isSurjection_682 ~v0 ~v1 ~v2 ~v3 v4 = du_isSurjection_682 v4
du_isSurjection_682 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsSurjection_162
du_isSurjection_682 v0
  = coe
      MAlonzo.Code.Function.Structures.C_IsSurjection'46'constructor_6455
      (coe du_isCongruent_678 (coe v0))
      (coe
         du_surjective_42
         (coe MAlonzo.Code.Relation.Binary.Structures.d_refl_34 (coe v0)))
-- Function.Construct.Identity._.isBijection
d_isBijection_684 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232
d_isBijection_684 ~v0 ~v1 ~v2 ~v3 v4 = du_isBijection_684 v4
du_isBijection_684 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsBijection_232
du_isBijection_684 v0
  = coe
      MAlonzo.Code.Function.Structures.C_IsBijection'46'constructor_8915
      (coe du_isInjection_680 (coe v0))
      (coe
         du_surjective_42
         (coe MAlonzo.Code.Relation.Binary.Structures.d_refl_34 (coe v0)))
-- Function.Construct.Identity._.isLeftInverse
d_isLeftInverse_686 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312
d_isLeftInverse_686 ~v0 ~v1 ~v2 ~v3 v4 = du_isLeftInverse_686 v4
du_isLeftInverse_686 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsLeftInverse_312
du_isLeftInverse_686 v0
  = coe
      MAlonzo.Code.Function.Structures.C_IsLeftInverse'46'constructor_13035
      (coe du_isCongruent_678 (coe v0)) (coe (\ v1 v2 v3 -> v3))
      (coe
         du_inverse'737'_52
         (coe MAlonzo.Code.Relation.Binary.Structures.d_refl_34 (coe v0)))
-- Function.Construct.Identity._.isRightInverse
d_isRightInverse_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390
d_isRightInverse_688 ~v0 ~v1 ~v2 ~v3 v4 = du_isRightInverse_688 v4
du_isRightInverse_688 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsRightInverse_390
du_isRightInverse_688 v0
  = coe
      MAlonzo.Code.Function.Structures.C_IsRightInverse'46'constructor_16307
      (coe du_isCongruent_678 (coe v0)) (coe (\ v1 v2 v3 -> v3))
      (coe
         du_inverse'691'_58
         (coe MAlonzo.Code.Relation.Binary.Structures.d_refl_34 (coe v0)))
-- Function.Construct.Identity._.isInverse
d_isInverse_690 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468
d_isInverse_690 ~v0 ~v1 ~v2 ~v3 v4 = du_isInverse_690 v4
du_isInverse_690 ::
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Function.Structures.T_IsInverse_468
du_isInverse_690 v0
  = coe
      MAlonzo.Code.Function.Structures.C_IsInverse'46'constructor_19111
      (coe du_isLeftInverse_686 (coe v0))
      (coe
         du_inverse'691'_58
         (coe MAlonzo.Code.Relation.Binary.Structures.d_refl_34 (coe v0)))
-- Function.Construct.Identity._.function
d_function_724 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Func_642
d_function_724 ~v0 ~v1 ~v2 = du_function_724
du_function_724 :: MAlonzo.Code.Function.Bundles.T_Func_642
du_function_724
  = coe
      MAlonzo.Code.Function.Bundles.C_Func'46'constructor_5935
      (coe (\ v0 -> v0)) (coe (\ v0 v1 v2 -> v2))
-- Function.Construct.Identity._.injection
d_injection_726 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Injection_704
d_injection_726 ~v0 ~v1 ~v2 = du_injection_726
du_injection_726 :: MAlonzo.Code.Function.Bundles.T_Injection_704
du_injection_726
  = coe
      MAlonzo.Code.Function.Bundles.C_Injection'46'constructor_7999
      (coe (\ v0 -> v0)) (coe (\ v0 v1 v2 -> v2))
      (coe (\ v0 v1 v2 -> v2))
-- Function.Construct.Identity._.surjection
d_surjection_728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
d_surjection_728 ~v0 ~v1 v2 = du_surjection_728 v2
du_surjection_728 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Surjection_774
du_surjection_728 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_Surjection'46'constructor_10213
      (coe (\ v1 -> v1)) (coe (\ v1 v2 v3 -> v3))
      (coe
         du_surjective_42
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_refl_34
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))))
-- Function.Construct.Identity._.bijection
d_bijection_730 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
d_bijection_730 ~v0 ~v1 v2 = du_bijection_730 v2
du_bijection_730 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_bijection_730 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_Bijection'46'constructor_13257
      (coe (\ v1 -> v1)) (coe (\ v1 v2 v3 -> v3))
      (coe
         du_bijective_48
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_refl_34
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))))
-- Function.Construct.Identity._.equivalence
d_equivalence_732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_equivalence_732 ~v0 ~v1 ~v2 = du_equivalence_732
du_equivalence_732 ::
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_equivalence_732
  = coe
      MAlonzo.Code.Function.Bundles.C_Equivalence'46'constructor_17233
      (coe (\ v0 -> v0)) (coe (\ v0 -> v0)) (coe (\ v0 v1 v2 -> v2))
      (coe (\ v0 v1 v2 -> v2))
-- Function.Construct.Identity._.leftInverse
d_leftInverse_734 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
d_leftInverse_734 ~v0 ~v1 v2 = du_leftInverse_734 v2
du_leftInverse_734 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
du_leftInverse_734 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_LeftInverse'46'constructor_18307
      (coe (\ v1 -> v1)) (coe (\ v1 -> v1)) (coe (\ v1 v2 v3 -> v3))
      (coe (\ v1 v2 v3 -> v3))
      (coe
         du_inverse'737'_52
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_refl_34
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))))
-- Function.Construct.Identity._.rightInverse
d_rightInverse_736 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
d_rightInverse_736 ~v0 ~v1 v2 = du_rightInverse_736 v2
du_rightInverse_736 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
du_rightInverse_736 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_RightInverse'46'constructor_21741
      (coe (\ v1 -> v1)) (coe (\ v1 -> v1)) (coe (\ v1 v2 v3 -> v3))
      (coe (\ v1 v2 v3 -> v3))
      (coe
         du_inverse'691'_58
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_refl_34
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))))
-- Function.Construct.Identity._.inverse
d_inverse_738 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_inverse_738 ~v0 ~v1 v2 = du_inverse_738 v2
du_inverse_738 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_inverse_738 v0
  = coe
      MAlonzo.Code.Function.Bundles.C_Inverse'46'constructor_24077
      (coe (\ v1 -> v1)) (coe (\ v1 -> v1)) (coe (\ v1 v2 v3 -> v3))
      (coe (\ v1 v2 v3 -> v3))
      (coe
         du_inverse'7495'_64
         (coe
            MAlonzo.Code.Relation.Binary.Structures.d_refl_34
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))))
-- Function.Construct.Identity._.⟶-id
d_'10230''45'id_748 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Func_642
d_'10230''45'id_748 ~v0 ~v1 = du_'10230''45'id_748
du_'10230''45'id_748 :: MAlonzo.Code.Function.Bundles.T_Func_642
du_'10230''45'id_748 = coe du_function_724
-- Function.Construct.Identity._.↣-id
d_'8611''45'id_750 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Injection_704
d_'8611''45'id_750 ~v0 ~v1 = du_'8611''45'id_750
du_'8611''45'id_750 ::
  MAlonzo.Code.Function.Bundles.T_Injection_704
du_'8611''45'id_750 = coe du_injection_726
-- Function.Construct.Identity._.↠-id
d_'8608''45'id_752 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Surjection_774
d_'8608''45'id_752 ~v0 ~v1 = du_'8608''45'id_752
du_'8608''45'id_752 ::
  MAlonzo.Code.Function.Bundles.T_Surjection_774
du_'8608''45'id_752
  = coe
      du_surjection_728
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Construct.Identity._.⤖-id
d_'10518''45'id_754 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Bijection_844
d_'10518''45'id_754 ~v0 ~v1 = du_'10518''45'id_754
du_'10518''45'id_754 ::
  MAlonzo.Code.Function.Bundles.T_Bijection_844
du_'10518''45'id_754
  = coe
      du_bijection_730
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Construct.Identity._.⇔-id
d_'8660''45'id_756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8660''45'id_756 ~v0 ~v1 = du_'8660''45'id_756
du_'8660''45'id_756 ::
  MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8660''45'id_756 = coe du_equivalence_732
-- Function.Construct.Identity._.↩-id
d_'8617''45'id_758 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_LeftInverse_946
d_'8617''45'id_758 ~v0 ~v1 = du_'8617''45'id_758
du_'8617''45'id_758 ::
  MAlonzo.Code.Function.Bundles.T_LeftInverse_946
du_'8617''45'id_758
  = coe
      du_leftInverse_734
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Construct.Identity._.↪-id
d_'8618''45'id_760 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_RightInverse_1024
d_'8618''45'id_760 ~v0 ~v1 = du_'8618''45'id_760
du_'8618''45'id_760 ::
  MAlonzo.Code.Function.Bundles.T_RightInverse_1024
du_'8618''45'id_760
  = coe
      du_rightInverse_736
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Construct.Identity._.↔-id
d_'8596''45'id_762 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_'8596''45'id_762 ~v0 ~v1 = du_'8596''45'id_762
du_'8596''45'id_762 :: MAlonzo.Code.Function.Bundles.T_Inverse_1052
du_'8596''45'id_762
  = coe
      du_inverse_738
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
-- Function.Construct.Identity.id-⟶
d_id'45''10230'_764 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Func_642
d_id'45''10230'_764 v0 v1 = coe du_'10230''45'id_748
-- Function.Construct.Identity.id-↣
d_id'45''8611'_766 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Injection_704
d_id'45''8611'_766 v0 v1 = coe du_'8611''45'id_750
-- Function.Construct.Identity.id-↠
d_id'45''8608'_768 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Surjection_774
d_id'45''8608'_768 v0 v1 = coe du_'8608''45'id_752
-- Function.Construct.Identity.id-⤖
d_id'45''10518'_770 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Bijection_844
d_id'45''10518'_770 v0 v1 = coe du_'10518''45'id_754
-- Function.Construct.Identity.id-⇔
d_id'45''8660'_772 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_id'45''8660'_772 v0 v1 = coe du_'8660''45'id_756
-- Function.Construct.Identity.id-↩
d_id'45''8617'_774 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_LeftInverse_946
d_id'45''8617'_774 v0 v1 = coe du_'8617''45'id_758
-- Function.Construct.Identity.id-↪
d_id'45''8618'_776 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_RightInverse_1024
d_id'45''8618'_776 v0 v1 = coe du_'8618''45'id_760
-- Function.Construct.Identity.id-↔
d_id'45''8596'_778 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Function.Bundles.T_Inverse_1052
d_id'45''8596'_778 v0 v1 = coe du_'8596''45'id_762
