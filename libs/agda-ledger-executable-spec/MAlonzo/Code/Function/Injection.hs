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

module MAlonzo.Code.Function.Injection where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Equality
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Injection.Injective
d_Injective_16 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Function.Equality.T_Π_16 -> ()
d_Injective_16 = erased
-- Function.Injection.Injection
d_Injection_88 a0 a1 a2 a3 a4 a5 = ()
data T_Injection_88
  = C_Injection'46'constructor_3039 MAlonzo.Code.Function.Equality.T_Π_16
                                    (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Injection.Injection.to
d_to_106 :: T_Injection_88 -> MAlonzo.Code.Function.Equality.T_Π_16
d_to_106 v0
  = case coe v0 of
      C_Injection'46'constructor_3039 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Injection.Injection.injective
d_injective_108 ::
  T_Injection_88 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_injective_108 v0
  = case coe v0 of
      C_Injection'46'constructor_3039 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Injection.Injection._._⟨$⟩_
d__'10216''36''10217'__112 :: T_Injection_88 -> AgdaAny -> AgdaAny
d__'10216''36''10217'__112 v0
  = coe
      MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
      (coe d_to_106 (coe v0))
-- Function.Injection.Injection._.cong
d_cong_114 ::
  T_Injection_88 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_114 v0
  = coe
      MAlonzo.Code.Function.Equality.d_cong_40 (coe d_to_106 (coe v0))
-- Function.Injection._↣_
d__'8611'__120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8611'__120 = erased
-- Function.Injection.injection
d_injection_140 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_Injection_88
d_injection_140 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_injection_140 v4 v5
du_injection_140 ::
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  T_Injection_88
du_injection_140 v0 v1
  = coe
      C_Injection'46'constructor_3039
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.du_'8594''45'to'45''10230'_68
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
            (coe
               MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
               erased erased erased))
         v0)
      (coe v1)
-- Function.Injection.id
d_id_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 -> T_Injection_88
d_id_152 ~v0 ~v1 ~v2 = du_id_152
du_id_152 :: T_Injection_88
du_id_152
  = coe
      C_Injection'46'constructor_3039
      (coe MAlonzo.Code.Function.Equality.du_id_62)
      (coe (\ v0 v1 v2 -> v2))
-- Function.Injection._∘_
d__'8728'__172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Injection_88 -> T_Injection_88 -> T_Injection_88
d__'8728'__172 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du__'8728'__172 v9 v10
du__'8728'__172 ::
  T_Injection_88 -> T_Injection_88 -> T_Injection_88
du__'8728'__172 v0 v1
  = coe
      C_Injection'46'constructor_3039
      (coe
         MAlonzo.Code.Function.Equality.du__'8728'__82
         (coe d_to_106 (coe v0)) (coe d_to_106 (coe v1)))
      (coe
         (\ v2 v3 v4 ->
            coe
              d_injective_108 v1 v2 v3
              (coe
                 d_injective_108 v0
                 (coe
                    MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                    (d_to_106 (coe v1)) v2)
                 (coe
                    MAlonzo.Code.Function.Equality.d__'10216''36''10217'__38
                    (d_to_106 (coe v1)) v3)
                 v4)))
