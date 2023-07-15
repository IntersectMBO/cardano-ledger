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

module MAlonzo.Code.Function.Bijection where

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
import qualified MAlonzo.Code.Function.Surjection
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Bijection.Bijective
d_Bijective_18 a0 a1 a2 a3 a4 a5 a6 = ()
data T_Bijective_18
  = C_Bijective'46'constructor_1705 (AgdaAny ->
                                     AgdaAny -> AgdaAny -> AgdaAny)
                                    MAlonzo.Code.Function.Surjection.T_Surjective_18
-- Function.Bijection.Bijective.injective
d_injective_38 ::
  T_Bijective_18 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_38 v0
  = case coe v0 of
      C_Bijective'46'constructor_1705 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bijection.Bijective.surjective
d_surjective_40 ::
  T_Bijective_18 -> MAlonzo.Code.Function.Surjection.T_Surjective_18
d_surjective_40 v0
  = case coe v0 of
      C_Bijective'46'constructor_1705 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bijection.Bijective._.from
d_from_44 ::
  T_Bijective_18 -> MAlonzo.Code.Function.Equality.T_Π_16
d_from_44 v0
  = coe
      MAlonzo.Code.Function.Surjection.d_from_38
      (coe d_surjective_40 (coe v0))
-- Function.Bijection.Bijective._.right-inverse-of
d_right'45'inverse'45'of_46 :: T_Bijective_18 -> AgdaAny -> AgdaAny
d_right'45'inverse'45'of_46 v0
  = coe
      MAlonzo.Code.Function.Surjection.d_right'45'inverse'45'of_40
      (coe d_surjective_40 (coe v0))
-- Function.Bijection.Bijective.left-inverse-of
d_left'45'inverse'45'of_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  T_Bijective_18 -> AgdaAny -> AgdaAny
d_left'45'inverse'45'of_48 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8
  = du_left'45'inverse'45'of_48 v6 v7 v8
du_left'45'inverse'45'of_48 ::
  MAlonzo.Code.Function.Equality.T_Π_16 ->
  T_Bijective_18 -> AgdaAny -> AgdaAny
du_left'45'inverse'45'of_48 v0 v1 v2
  = coe
      d_injective_38 v1
      (coe
         MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
         (MAlonzo.Code.Function.Surjection.d_from_38
            (coe d_surjective_40 (coe v1)))
         (coe
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 v0 v2))
      v2
      (coe
         MAlonzo.Code.Function.Surjection.d_right'45'inverse'45'of_40
         (d_surjective_40 (coe v1))
         (coe
            MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38 v0 v2))
-- Function.Bijection.Bijection
d_Bijection_64 a0 a1 a2 a3 a4 a5 = ()
data T_Bijection_64
  = C_Bijection'46'constructor_4747 MAlonzo.Code.Function.Equality.T_Π_16
                                    T_Bijective_18
-- Function.Bijection.Bijection.to
d_to_82 :: T_Bijection_64 -> MAlonzo.Code.Function.Equality.T_Π_16
d_to_82 v0
  = case coe v0 of
      C_Bijection'46'constructor_4747 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bijection.Bijection.bijective
d_bijective_84 :: T_Bijection_64 -> T_Bijective_18
d_bijective_84 v0
  = case coe v0 of
      C_Bijection'46'constructor_4747 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Bijection.Bijection._.from
d_from_88 ::
  T_Bijection_64 -> MAlonzo.Code.Function.Equality.T_Π_16
d_from_88 v0
  = coe
      MAlonzo.Code.Function.Surjection.d_from_38
      (coe d_surjective_40 (coe d_bijective_84 (coe v0)))
-- Function.Bijection.Bijection._.injective
d_injective_90 ::
  T_Bijection_64 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_90 v0
  = coe d_injective_38 (coe d_bijective_84 (coe v0))
-- Function.Bijection.Bijection._.left-inverse-of
d_left'45'inverse'45'of_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 -> AgdaAny -> AgdaAny
d_left'45'inverse'45'of_92 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_left'45'inverse'45'of_92 v6
du_left'45'inverse'45'of_92 :: T_Bijection_64 -> AgdaAny -> AgdaAny
du_left'45'inverse'45'of_92 v0
  = coe
      du_left'45'inverse'45'of_48 (coe d_to_82 (coe v0))
      (coe d_bijective_84 (coe v0))
-- Function.Bijection.Bijection._.right-inverse-of
d_right'45'inverse'45'of_94 :: T_Bijection_64 -> AgdaAny -> AgdaAny
d_right'45'inverse'45'of_94 v0
  = coe
      MAlonzo.Code.Function.Surjection.d_right'45'inverse'45'of_40
      (coe d_surjective_40 (coe d_bijective_84 (coe v0)))
-- Function.Bijection.Bijection._.surjective
d_surjective_96 ::
  T_Bijection_64 -> MAlonzo.Code.Function.Surjection.T_Surjective_18
d_surjective_96 v0
  = coe d_surjective_40 (coe d_bijective_84 (coe v0))
-- Function.Bijection.Bijection.injection
d_injection_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 -> MAlonzo.Code.Function.Injection.T_Injection_88
d_injection_98 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_injection_98 v6
du_injection_98 ::
  T_Bijection_64 -> MAlonzo.Code.Function.Injection.T_Injection_88
du_injection_98 v0
  = coe
      MAlonzo.Code.Function.Injection.C_Injection'46'constructor_3039
      (coe d_to_82 (coe v0))
      (coe d_injective_38 (coe d_bijective_84 (coe v0)))
-- Function.Bijection.Bijection.surjection
d_surjection_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 -> MAlonzo.Code.Function.Surjection.T_Surjection_54
d_surjection_100 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_surjection_100 v6
du_surjection_100 ::
  T_Bijection_64 -> MAlonzo.Code.Function.Surjection.T_Surjection_54
du_surjection_100 v0
  = coe
      MAlonzo.Code.Function.Surjection.C_Surjection'46'constructor_2365
      (coe d_to_82 (coe v0))
      (coe d_surjective_40 (coe d_bijective_84 (coe v0)))
-- Function.Bijection.Bijection._.equivalence
d_equivalence_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
d_equivalence_104 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_equivalence_104 v6
du_equivalence_104 ::
  T_Bijection_64 ->
  MAlonzo.Code.Function.Equivalence.T_Equivalence_16
du_equivalence_104 v0
  = coe
      MAlonzo.Code.Function.Surjection.du_equivalence_92
      (coe du_surjection_100 (coe v0))
-- Function.Bijection.Bijection._.to-from
d_to'45'from_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_to'45'from_106 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_to'45'from_106 v4 v5 v6
du_to'45'from_106 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_to'45'from_106 v0 v1 v2
  = let v3 = coe du_surjection_100 (coe v2) in
    coe
      MAlonzo.Code.Function.LeftInverse.du_to'45'from_192 (coe v1)
      (coe v0)
      (coe
         MAlonzo.Code.Function.Surjection.du_right'45'inverse_82 (coe v3))
-- Function.Bijection.Bijection._.right-inverse
d_right'45'inverse_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d_right'45'inverse_108 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_right'45'inverse_108 v6
du_right'45'inverse_108 ::
  T_Bijection_64 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du_right'45'inverse_108 v0
  = coe
      MAlonzo.Code.Function.Surjection.du_right'45'inverse_82
      (coe du_surjection_100 (coe v0))
-- Function.Bijection.Bijection.left-inverse
d_left'45'inverse_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
d_left'45'inverse_110 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6
  = du_left'45'inverse_110 v6
du_left'45'inverse_110 ::
  T_Bijection_64 ->
  MAlonzo.Code.Function.LeftInverse.T_LeftInverse_82
du_left'45'inverse_110 v0
  = coe
      MAlonzo.Code.Function.LeftInverse.C_LeftInverse'46'constructor_4525
      (coe d_to_82 (coe v0))
      (coe
         MAlonzo.Code.Function.Surjection.d_from_38
         (coe d_surjective_40 (coe d_bijective_84 (coe v0))))
      (coe
         du_left'45'inverse'45'of_48 (coe d_to_82 (coe v0))
         (coe d_bijective_84 (coe v0)))
-- Function.Bijection.Bijection._.to-from
d_to'45'from_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_to'45'from_114 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_to'45'from_114 v4 v5 v6
du_to'45'from_114 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_to'45'from_114 v0 v1 v2
  = coe
      MAlonzo.Code.Function.LeftInverse.du_to'45'from_192 (coe v0)
      (coe v1) (coe du_left'45'inverse_110 (coe v2))
-- Function.Bijection._⤖_
d__'10518'__120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'10518'__120 = erased
-- Function.Bijection.bijection
d_bijection_144 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_Bijection_64
d_bijection_144 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_bijection_144 v4 v5 v6 v7
du_bijection_144 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_Bijection_64
du_bijection_144 v0 v1 v2 v3
  = coe
      C_Bijection'46'constructor_4747
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.du_'8594''45'to'45''10230'_68
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
            (coe
               MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
               erased erased erased))
         v0)
      (coe
         C_Bijective'46'constructor_1705 (coe v2)
         (coe
            MAlonzo.Code.Function.Surjection.C_Surjective'46'constructor_1227
            (coe
               MAlonzo.Code.Relation.Binary.PropositionalEquality.du_'8594''45'to'45''10230'_68
               (coe
                  MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
                     erased erased erased))
               v1)
            (coe v3)))
-- Function.Bijection.id
d_id_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 -> T_Bijection_64
d_id_160 ~v0 ~v1 v2 = du_id_160 v2
du_id_160 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 -> T_Bijection_64
du_id_160 v0
  = coe
      C_Bijection'46'constructor_4747
      (coe MAlonzo.Code.Function.Equality.du_id_62)
      (coe
         C_Bijective'46'constructor_1705
         (coe
            MAlonzo.Code.Function.Injection.d_injective_108
            (coe MAlonzo.Code.Function.Injection.du_id_152))
         (coe
            MAlonzo.Code.Function.Surjection.d_surjective_74
            (coe MAlonzo.Code.Function.Surjection.du_id_168 (coe v0))))
-- Function.Bijection._∘_
d__'8728'__182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 -> T_Bijection_64 -> T_Bijection_64
d__'8728'__182 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du__'8728'__182 v8 v9 v10
du__'8728'__182 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Bijection_64 -> T_Bijection_64 -> T_Bijection_64
du__'8728'__182 v0 v1 v2
  = coe
      C_Bijection'46'constructor_4747
      (coe
         MAlonzo.Code.Function.Equality.du__'8728'__82
         (coe d_to_82 (coe v1)) (coe d_to_82 (coe v2)))
      (coe
         C_Bijective'46'constructor_1705
         (coe
            (\ v3 v4 v5 ->
               coe
                 d_injective_38 (d_bijective_84 (coe v2)) v3 v4
                 (coe
                    d_injective_38 (d_bijective_84 (coe v1))
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                       (d_to_82 (coe v2)) v3)
                    (coe
                       MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                       (d_to_82 (coe v2)) v4)
                    v5)))
         (coe
            MAlonzo.Code.Function.Surjection.d_surjective_74
            (coe
               MAlonzo.Code.Function.Surjection.du__'8728'__196 (coe v0)
               (coe du_surjection_100 (coe v1))
               (coe du_surjection_100 (coe v2)))))
