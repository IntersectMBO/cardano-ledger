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

module MAlonzo.Code.Function.Surjection where

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
import qualified MAlonzo.Code.Function.LeftInverse
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Surjection.Surjective
d_Surjective_18 a0 a1 a2 a3 a4 a5 a6 = ()
data T_Surjective_18
  = C_Surjective'46'constructor_1227 MAlonzo.Code.Function.Equality.T_Π_16
                                     (AgdaAny -> AgdaAny)
-- Function.Surjection.Surjective.from
d_from_38 ::
  T_Surjective_18 -> MAlonzo.Code.Function.Equality.T_Π_16
d_from_38 v0
  = case coe v0 of
      C_Surjective'46'constructor_1227 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Surjection.Surjective.right-inverse-of
d_right'45'inverse'45'of_40 ::
  T_Surjective_18 -> AgdaAny -> AgdaAny
d_right'45'inverse'45'of_40 v0
  = case coe v0 of
      C_Surjective'46'constructor_1227 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Surjection.Surjection
d_Surjection_54 a0 a1 a2 a3 a4 a5 = ()
data T_Surjection_54
  = C_Surjection'46'constructor_2365 MAlonzo.Code.Function.Equality.T_Π_16
                                     T_Surjective_18
-- Function.Surjection.Surjection.to
d_to_72 :: T_Surjection_54 -> MAlonzo.Code.Function.Equality.T_Π_16
d_to_72 v0
  = case coe v0 of
      C_Surjection'46'constructor_2365 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Surjection.Surjection.surjective
d_surjective_74 :: T_Surjection_54 -> T_Surjective_18
d_surjective_74 v0
  = case coe v0 of
      C_Surjection'46'constructor_2365 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Surjection.Surjection._.from
d_from_78 ::
  T_Surjection_54 -> MAlonzo.Code.Function.Equality.T_Π_16
d_from_78 v0 = coe d_from_38 (coe d_surjective_74 (coe v0))
-- Function.Surjection.Surjection._.right-inverse-of
d_right'45'inverse'45'of_80 ::
  T_Surjection_54 -> AgdaAny -> AgdaAny
d_right'45'inverse'45'of_80 v0
  = coe d_right'45'inverse'45'of_40 (coe d_surjective_74 (coe v0))
-- Function.Surjection.Surjection.right-inverse
d_right'45'inverse_82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d_right'45'inverse_82 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_right'45'inverse_82 v6
du_right'45'inverse_82 ::
  T_Surjection_54 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du_right'45'inverse_82 v0
  = coe
      MAlonzo.Code.Function.LeftInverse.C_LeftInverse'46'constructor_4525
      (coe d_from_38 (coe d_surjective_74 (coe v0)))
      (coe d_to_72 (coe v0))
      (coe d_right'45'inverse'45'of_40 (coe d_surjective_74 (coe v0)))
-- Function.Surjection.Surjection._.to-from
d_to'45'from_86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_to'45'from_86 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_to'45'from_86 v4 v5 v6
du_to'45'from_86 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_to'45'from_86 v0 v1 v2
  = coe
      MAlonzo.Code.Function.LeftInverse.du_to'45'from_192 (coe v1)
      (coe v0) (coe du_right'45'inverse_82 (coe v2))
-- Function.Surjection.Surjection.injective
d_injective_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_88 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8
  = du_injective_88 v5 v6 v7 v8
du_injective_88 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_injective_88 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Function.LeftInverse.du_injective_176 (coe v0)
      (coe du_right'45'inverse_82 (coe v1)) (coe v2) (coe v3)
-- Function.Surjection.Surjection.injection
d_injection_90 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 -> MAlonzo.Code.Function.Injection.T_Injection_88
d_injection_90 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 = du_injection_90 v5 v6
du_injection_90 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 -> MAlonzo.Code.Function.Injection.T_Injection_88
du_injection_90 v0 v1
  = coe
      MAlonzo.Code.Function.LeftInverse.du_injection_184 (coe v0)
      (coe du_right'45'inverse_82 (coe v1))
-- Function.Surjection.Surjection.equivalence
d_equivalence_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_equivalence_92 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_equivalence_92 v6
du_equivalence_92 ::
  T_Surjection_54 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du_equivalence_92 v0
  = coe
      MAlonzo.Code.Function.Equivalence.C_Equivalence'46'constructor_433
      (coe d_to_72 (coe v0))
      (coe d_from_38 (coe d_surjective_74 (coe v0)))
-- Function.Surjection.fromRightInverse
d_fromRightInverse_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  T_Surjection_54
d_fromRightInverse_106 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_fromRightInverse_106 v6
du_fromRightInverse_106 ::
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82 ->
  T_Surjection_54
du_fromRightInverse_106 v0
  = coe
      C_Surjection'46'constructor_2365
      (coe MAlonzo.Code.Function.LeftInverse.d_from_104 (coe v0))
      (coe
         C_Surjective'46'constructor_1227
         (coe MAlonzo.Code.Function.LeftInverse.d_to_102 (coe v0))
         (coe
            MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106
            (coe v0)))
-- Function.Surjection._↠_
d__'8608'__134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8608'__134 = erased
-- Function.Surjection.surjection
d_surjection_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_Surjection_54
d_surjection_154 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_surjection_154 v4 v5 v6
du_surjection_154 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_Surjection_54
du_surjection_154 v0 v1 v2
  = coe
      C_Surjection'46'constructor_2365
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.du_'8594''45'to'45''10230'_68
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
            (coe
               MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
               erased erased erased))
         v0)
      (coe
         C_Surjective'46'constructor_1227
         (coe
            MAlonzo.Code.Relation.Binary.PropositionalEquality.du_'8594''45'to'45''10230'_68
            (coe
               MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
               (coe
                  MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
                  erased erased erased))
            v1)
         (coe v2))
-- Function.Surjection.id
d_id_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 -> T_Surjection_54
d_id_168 ~v0 ~v1 v2 = du_id_168 v2
du_id_168 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 -> T_Surjection_54
du_id_168 v0
  = coe
      C_Surjection'46'constructor_2365
      (coe MAlonzo.Code.Function.Equality.du_id_62)
      (coe
         C_Surjective'46'constructor_1227
         (coe
            MAlonzo.Code.Function.LeftInverse.d_to_102
            (coe du_id'8242'_176 (coe v0)))
         (coe
            MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106
            (coe du_id'8242'_176 (coe v0))))
-- Function.Surjection._.id′
d_id'8242'_176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d_id'8242'_176 ~v0 ~v1 v2 = du_id'8242'_176 v2
du_id'8242'_176 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du_id'8242'_176 v0
  = coe MAlonzo.Code.Function.LeftInverse.du_id_256 (coe v0)
-- Function.Surjection._∘_
d__'8728'__196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 -> T_Surjection_54 -> T_Surjection_54
d__'8728'__196 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du__'8728'__196 v8 v9 v10
du__'8728'__196 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 -> T_Surjection_54 -> T_Surjection_54
du__'8728'__196 v0 v1 v2
  = coe
      C_Surjection'46'constructor_2365
      (coe
         MAlonzo.Code.Function.Equality.du__'8728'__82
         (coe d_to_72 (coe v1)) (coe d_to_72 (coe v2)))
      (coe
         C_Surjective'46'constructor_1227
         (coe
            MAlonzo.Code.Function.LeftInverse.d_to_102
            (coe du_g'8728'f_206 (coe v0) (coe v1) (coe v2)))
         (coe
            MAlonzo.Code.Function.LeftInverse.d_left'45'inverse'45'of_106
            (coe du_g'8728'f_206 (coe v0) (coe v1) (coe v2))))
-- Function.Surjection._.g∘f
d_g'8728'f_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 ->
  T_Surjection_54 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d_g'8728'f_206 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du_g'8728'f_206 v8 v9 v10
du_g'8728'f_206 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Surjection_54 ->
  T_Surjection_54 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du_g'8728'f_206 v0 v1 v2
  = coe
      MAlonzo.Code.Function.LeftInverse.du__'8728'__280 (coe v0)
      (coe du_right'45'inverse_82 (coe v2))
      (coe du_right'45'inverse_82 (coe v1))
