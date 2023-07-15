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

module MAlonzo.Code.Interface.DecEq where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Integer.Properties
import qualified MAlonzo.Code.Data.List.Properties
import qualified MAlonzo.Code.Data.Maybe.Properties
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Properties
import qualified MAlonzo.Code.Data.Sum.Properties
import qualified MAlonzo.Code.Data.Unit.Properties
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Interface.DecEq.DecEq
d_DecEq_14 a0 a1 = ()
newtype T_DecEq_14
  = C_DecEq'46'constructor_63 (AgdaAny ->
                               AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20)
-- Interface.DecEq.DecEq._≟_
d__'8799'__20 ::
  T_DecEq_14 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__20 v0
  = case coe v0 of
      C_DecEq'46'constructor_63 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.DecEq._._≟_
d__'8799'__24 ::
  T_DecEq_14 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8799'__24 v0 = coe d__'8799'__20 (coe v0)
-- Interface.DecEq.DecEq-⊥
d_DecEq'45''8869'_26 :: T_DecEq_14
d_DecEq'45''8869'_26 = coe C_DecEq'46'constructor_63 erased
-- Interface.DecEq.DecEq-⊤
d_DecEq'45''8868'_28 :: T_DecEq_14
d_DecEq'45''8868'_28
  = coe
      C_DecEq'46'constructor_63
      (\ v0 v1 -> coe MAlonzo.Code.Data.Unit.Properties.du__'8799'__8)
-- Interface.DecEq.DecEq-ℕ
d_DecEq'45'ℕ_30 :: T_DecEq_14
d_DecEq'45'ℕ_30
  = coe
      C_DecEq'46'constructor_63
      (coe MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464)
-- Interface.DecEq.DecEq-ℤ
d_DecEq'45'ℤ_32 :: T_DecEq_14
d_DecEq'45'ℤ_32
  = coe
      C_DecEq'46'constructor_63
      (coe MAlonzo.Code.Data.Integer.Properties.d__'8799'__2476)
-- Interface.DecEq.DecEq-Maybe
d_DecEq'45'Maybe_34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_DecEq_14 -> T_DecEq_14
d_DecEq'45'Maybe_34 ~v0 ~v1 v2 = du_DecEq'45'Maybe_34 v2
du_DecEq'45'Maybe_34 :: T_DecEq_14 -> T_DecEq_14
du_DecEq'45'Maybe_34 v0
  = coe
      C_DecEq'46'constructor_63
      (coe
         MAlonzo.Code.Data.Maybe.Properties.du_'8801''45'dec_24
         (coe d__'8799'__20 (coe v0)))
-- Interface.DecEq.DecEq-List
d_DecEq'45'List_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_DecEq_14 -> T_DecEq_14
d_DecEq'45'List_36 ~v0 ~v1 v2 = du_DecEq'45'List_36 v2
du_DecEq'45'List_36 :: T_DecEq_14 -> T_DecEq_14
du_DecEq'45'List_36 v0
  = coe
      C_DecEq'46'constructor_63
      (coe
         MAlonzo.Code.Data.List.Properties.du_'8801''45'dec_54
         (coe d__'8799'__20 (coe v0)))
-- Interface.DecEq.DecEq-Product
d_DecEq'45'Product_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_DecEq_14 -> T_DecEq_14 -> T_DecEq_14
d_DecEq'45'Product_38 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_DecEq'45'Product_38 v4 v5
du_DecEq'45'Product_38 :: T_DecEq_14 -> T_DecEq_14 -> T_DecEq_14
du_DecEq'45'Product_38 v0 v1
  = coe
      C_DecEq'46'constructor_63
      (coe
         MAlonzo.Code.Data.Product.Properties.du_'8801''45'dec_78
         (coe d__'8799'__20 (coe v0))
         (coe (\ v2 -> d__'8799'__20 (coe v1))))
-- Interface.DecEq.DecEq-Sum
d_DecEq'45'Sum_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_DecEq_14 -> T_DecEq_14 -> T_DecEq_14
d_DecEq'45'Sum_40 ~v0 ~v1 ~v2 ~v3 v4 v5 = du_DecEq'45'Sum_40 v4 v5
du_DecEq'45'Sum_40 :: T_DecEq_14 -> T_DecEq_14 -> T_DecEq_14
du_DecEq'45'Sum_40 v0 v1
  = coe
      C_DecEq'46'constructor_63
      (coe
         MAlonzo.Code.Data.Sum.Properties.du_'8801''45'dec_54
         (coe d__'8799'__20 (coe v0)) (coe d__'8799'__20 (coe v1)))
