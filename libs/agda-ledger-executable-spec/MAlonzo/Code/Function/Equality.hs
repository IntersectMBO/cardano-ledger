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

module MAlonzo.Code.Function.Equality where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles
import qualified MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Construct.Trivial
import qualified MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Equality.Π
d_Π_16 a0 a1 a2 a3 a4 a5 = ()
data T_Π_16
  = C_Π'46'constructor_1167 (AgdaAny -> AgdaAny)
                            (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny)
-- Function.Equality.Π._⟨$⟩_
d__'10216''36''10217'__38 :: T_Π_16 -> AgdaAny -> AgdaAny
d__'10216''36''10217'__38 v0
  = case coe v0 of
      C_Π'46'constructor_1167 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Equality.Π.cong
d_cong_40 :: T_Π_16 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_40 v0
  = case coe v0 of
      C_Π'46'constructor_1167 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Equality._⟶_
d__'10230'__50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 -> ()
d__'10230'__50 = erased
-- Function.Equality.id
d_id_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 -> T_Π_16
d_id_62 ~v0 ~v1 ~v2 = du_id_62
du_id_62 :: T_Π_16
du_id_62
  = coe
      C_Π'46'constructor_1167 (coe (\ v0 -> v0)) (coe (\ v0 v1 v2 -> v2))
-- Function.Equality._∘_
d__'8728'__82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Π_16 -> T_Π_16 -> T_Π_16
d__'8728'__82 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du__'8728'__82 v9 v10
du__'8728'__82 :: T_Π_16 -> T_Π_16 -> T_Π_16
du__'8728'__82 v0 v1
  = coe
      C_Π'46'constructor_1167
      (coe
         (\ v2 ->
            coe
              d__'10216''36''10217'__38 v0
              (coe d__'10216''36''10217'__38 v1 v2)))
      (coe
         (\ v2 v3 v4 ->
            coe
              d_cong_40 v0 (coe d__'10216''36''10217'__38 v1 v2)
              (coe d__'10216''36''10217'__38 v1 v3) (coe d_cong_40 v1 v2 v3 v4)))
-- Function.Equality.const
d_const_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> T_Π_16
d_const_100 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 = du_const_100 v5 v6
du_const_100 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  AgdaAny -> T_Π_16
du_const_100 v0 v1
  = coe
      C_Π'46'constructor_1167 (coe (\ v2 -> v1))
      (coe
         (\ v2 v3 ->
            let v4
                  = coe
                      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                      (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                      v1 in
            \ v5 -> v4))
-- Function.Equality.setoid
d_setoid_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.T_IndexedSetoid_18 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_116 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_setoid_116 v4 v5
du_setoid_116 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.T_IndexedSetoid_18 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_116 v0 v1
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
         (coe (\ v2 -> d_cong_40 (coe v2)))
         (coe
            (\ v2 v3 v4 v5 v6 v7 ->
               coe
                 MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.d_sym_32
                 (MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.d_isEquivalence_38
                    (coe v1))
                 v6 v5 (coe d__'10216''36''10217'__38 v2 v6)
                 (coe d__'10216''36''10217'__38 v3 v5)
                 (coe
                    v4 v6 v5
                    (coe
                       MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                       (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                       v5 v6 v7))))
         (coe
            (\ v2 v3 v4 v5 v6 v7 v8 v9 ->
               coe
                 MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.d_trans_34
                 (MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.d_isEquivalence_38
                    (coe v1))
                 v7 v7 v8 (coe d__'10216''36''10217'__38 v2 v7)
                 (coe d__'10216''36''10217'__38 v3 v7)
                 (coe d__'10216''36''10217'__38 v4 v8)
                 (coe
                    v5 v7 v7
                    (coe
                       MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                       (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                       v7))
                 (coe v6 v7 v8 v9))))
-- Function.Equality._⇨_
d__'8680'__192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d__'8680'__192 ~v0 ~v1 ~v2 ~v3 v4 v5 = du__'8680'__192 v4 v5
du__'8680'__192 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du__'8680'__192 v0 v1
  = coe
      du_setoid_116 (coe v0)
      (coe
         MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Construct.Trivial.du_indexedSetoid_100
         (coe v1))
-- Function.Equality.≡-setoid
d_'8801''45'setoid_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.T_IndexedSetoid_18 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8801''45'setoid_206 ~v0 ~v1 ~v2 ~v3 v4
  = du_'8801''45'setoid_206 v4
du_'8801''45'setoid_206 ::
  MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.T_IndexedSetoid_18 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_'8801''45'setoid_206 v0
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
         (coe
            (\ v1 v2 ->
               coe
                 MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.d_refl_30
                 (MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.d_isEquivalence_38
                    (coe v0))
                 v2 (coe v1 v2)))
         (coe
            (\ v1 v2 v3 v4 ->
               coe
                 MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.d_sym_32
                 (MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.d_isEquivalence_38
                    (coe v0))
                 v4 v4 (coe v1 v4) (coe v2 v4) (coe v3 v4)))
         (coe
            (\ v1 v2 v3 v4 v5 v6 ->
               coe
                 MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Structures.d_trans_34
                 (MAlonzo.Code.Relation.Binary.Indexed.Heterogeneous.Bundles.d_isEquivalence_38
                    (coe v0))
                 v6 v6 v6 (coe v1 v6) (coe v2 v6) (coe v3 v6) (coe v4 v6)
                 (coe v5 v6))))
-- Function.Equality.flip
d_flip_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Π_16 -> T_Π_16
d_flip_272 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 v9
  = du_flip_272 v5 v9
du_flip_272 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Π_16 -> T_Π_16
du_flip_272 v0 v1
  = coe
      C_Π'46'constructor_1167
      (coe
         (\ v2 ->
            coe
              C_Π'46'constructor_1167
              (coe
                 (\ v3 ->
                    coe
                      d__'10216''36''10217'__38 (coe d__'10216''36''10217'__38 v1 v3)
                      v2))
              (coe
                 (\ v3 v4 v5 ->
                    coe
                      d_cong_40 v1 v3 v4 v5 v2 v2
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60 (coe v0))
                         v2)))))
      (coe (\ v2 v3 v4 v5 v6 v7 -> coe d_cong_40 v1 v5 v6 v7 v2 v3 v4))
