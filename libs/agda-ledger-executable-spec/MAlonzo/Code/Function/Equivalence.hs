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

module MAlonzo.Code.Function.Equivalence where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Function.Equality
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Function.Equivalence.Equivalence
d_Equivalence_16 a0 a1 a2 a3 a4 a5 = ()
data T_Equivalence_16
  = C_Equivalence'46'constructor_433 MAlonzo.Code.Function.Equality.T_Π_16
                                     MAlonzo.Code.Function.Equality.T_Π_16
-- Function.Equivalence.Equivalence.to
d_to_34 ::
  T_Equivalence_16 -> MAlonzo.Code.Function.Equality.T_Π_16
d_to_34 v0
  = case coe v0 of
      C_Equivalence'46'constructor_433 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Equivalence.Equivalence.from
d_from_36 ::
  T_Equivalence_16 -> MAlonzo.Code.Function.Equality.T_Π_16
d_from_36 v0
  = case coe v0 of
      C_Equivalence'46'constructor_433 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Function.Equivalence._⇔_
d__'8660'__42 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> () -> ()
d__'8660'__42 = erased
-- Function.Equivalence.equivalence
d_equivalence_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> T_Equivalence_16
d_equivalence_56 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_equivalence_56 v4 v5
du_equivalence_56 ::
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> T_Equivalence_16
du_equivalence_56 v0 v1
  = coe
      C_Equivalence'46'constructor_433
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.du_'8594''45'to'45''10230'_68
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
            (coe
               MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
               erased erased erased))
         v0)
      (coe
         MAlonzo.Code.Relation.Binary.PropositionalEquality.du_'8594''45'to'45''10230'_68
         (coe
            MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
            (coe
               MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
               erased erased erased))
         v1)
-- Function.Equivalence.id
d_id_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Equivalence_16
d_id_66 ~v0 ~v1 ~v2 = du_id_66
du_id_66 :: T_Equivalence_16
du_id_66
  = coe
      C_Equivalence'46'constructor_433
      (coe MAlonzo.Code.Function.Equality.du_id_62)
      (coe MAlonzo.Code.Function.Equality.du_id_62)
-- Function.Equivalence._∘_
d__'8728'__82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Equivalence_16 -> T_Equivalence_16 -> T_Equivalence_16
d__'8728'__82 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du__'8728'__82 v9 v10
du__'8728'__82 ::
  T_Equivalence_16 -> T_Equivalence_16 -> T_Equivalence_16
du__'8728'__82 v0 v1
  = coe
      C_Equivalence'46'constructor_433
      (coe
         MAlonzo.Code.Function.Equality.du__'8728'__82
         (coe d_to_34 (coe v0)) (coe d_to_34 (coe v1)))
      (coe
         MAlonzo.Code.Function.Equality.du__'8728'__82
         (coe d_from_36 (coe v1)) (coe d_from_36 (coe v0)))
-- Function.Equivalence.sym
d_sym_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  T_Equivalence_16 -> T_Equivalence_16
d_sym_100 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 = du_sym_100 v6
du_sym_100 :: T_Equivalence_16 -> T_Equivalence_16
du_sym_100 v0
  = coe
      C_Equivalence'46'constructor_433 (coe d_from_36 (coe v0))
      (coe d_to_34 (coe v0))
-- Function.Equivalence._._.from
d_from_110 ::
  T_Equivalence_16 -> MAlonzo.Code.Function.Equality.T_Π_16
d_from_110 v0 = coe d_from_36 (coe v0)
-- Function.Equivalence._._.to
d_to_112 ::
  T_Equivalence_16 -> MAlonzo.Code.Function.Equality.T_Π_16
d_to_112 v0 = coe d_to_34 (coe v0)
-- Function.Equivalence.setoid
d_setoid_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_setoid_118 ~v0 ~v1 = du_setoid_118
du_setoid_118 :: MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_setoid_118
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
         (\ v0 -> coe du_id_66) (\ v0 v1 v2 -> coe du_sym_100 v2)
         (coe (\ v0 v1 v2 v3 v4 -> coe du__'8728'__82 (coe v4) (coe v3))))
-- Function.Equivalence.⇔-setoid
d_'8660''45'setoid_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
d_'8660''45'setoid_126 ~v0 = du_'8660''45'setoid_126
du_'8660''45'setoid_126 ::
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44
du_'8660''45'setoid_126
  = coe
      MAlonzo.Code.Relation.Binary.Bundles.C_Setoid'46'constructor_719
      (coe
         MAlonzo.Code.Relation.Binary.Structures.C_IsEquivalence'46'constructor_743
         (coe (\ v0 -> coe du_id_66)) (coe (\ v0 v1 -> coe du_sym_100))
         (coe (\ v0 v1 v2 v3 v4 -> coe du__'8728'__82 (coe v4) (coe v3))))
-- Function.Equivalence.map
d_map_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16) ->
  (MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16) ->
  T_Equivalence_16 -> T_Equivalence_16
d_map_154 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 v12 v13
          v14
  = du_map_154 v12 v13 v14
du_map_154 ::
  (MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16) ->
  (MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16) ->
  T_Equivalence_16 -> T_Equivalence_16
du_map_154 v0 v1 v2
  = coe
      C_Equivalence'46'constructor_433 (coe v0 (d_to_34 (coe v2)))
      (coe v1 (d_from_36 (coe v2)))
-- Function.Equivalence._._.from
d_from_168 ::
  T_Equivalence_16 -> MAlonzo.Code.Function.Equality.T_Π_16
d_from_168 v0 = coe d_from_36 (coe v0)
-- Function.Equivalence._._.to
d_to_170 ::
  T_Equivalence_16 -> MAlonzo.Code.Function.Equality.T_Π_16
d_to_170 v0 = coe d_to_34 (coe v0)
-- Function.Equivalence.zip
d_zip_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  MAlonzo.Code.Relation.Binary.Bundles.T_Setoid_44 ->
  (MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16) ->
  (MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16) ->
  T_Equivalence_16 -> T_Equivalence_16 -> T_Equivalence_16
d_zip_208 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 ~v10 ~v11 ~v12
          ~v13 ~v14 ~v15 ~v16 ~v17 v18 v19 v20 v21
  = du_zip_208 v18 v19 v20 v21
du_zip_208 ::
  (MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16) ->
  (MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16 ->
   MAlonzo.Code.Function.Equality.T_Π_16) ->
  T_Equivalence_16 -> T_Equivalence_16 -> T_Equivalence_16
du_zip_208 v0 v1 v2 v3
  = coe
      C_Equivalence'46'constructor_433
      (coe v0 (d_to_34 (coe v2)) (d_to_34 (coe v3)))
      (coe v1 (d_from_36 (coe v2)) (d_from_36 (coe v3)))
