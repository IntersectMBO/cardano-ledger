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

module MAlonzo.Code.Foreign.Convertible where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Map
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Foreign.Haskell.Coerce
import qualified MAlonzo.Code.Foreign.Haskell.Pair
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Ledger.Prelude

-- Foreign.Convertible.Convertible
d_Convertible_8 a0 a1 = ()
data T_Convertible_8
  = C_Convertible'46'constructor_21 (AgdaAny -> AgdaAny)
                                    (AgdaAny -> AgdaAny)
-- Foreign.Convertible.Convertible.to
d_to_18 :: T_Convertible_8 -> AgdaAny -> AgdaAny
d_to_18 v0
  = case coe v0 of
      C_Convertible'46'constructor_21 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Foreign.Convertible.Convertible.from
d_from_20 :: T_Convertible_8 -> AgdaAny -> AgdaAny
d_from_20 v0
  = case coe v0 of
      C_Convertible'46'constructor_21 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Foreign.Convertible._.from
d_from_24 :: T_Convertible_8 -> AgdaAny -> AgdaAny
d_from_24 v0 = coe d_from_20 (coe v0)
-- Foreign.Convertible._.to
d_to_26 :: T_Convertible_8 -> AgdaAny -> AgdaAny
d_to_26 v0 = coe d_to_18 (coe v0)
-- Foreign.Convertible.Convertible-Refl
d_Convertible'45'Refl_30 :: () -> T_Convertible_8
d_Convertible'45'Refl_30 ~v0 = du_Convertible'45'Refl_30
du_Convertible'45'Refl_30 :: T_Convertible_8
du_Convertible'45'Refl_30
  = coe
      C_Convertible'46'constructor_21 (coe (\ v0 -> v0))
      (coe (\ v0 -> v0))
-- Foreign.Convertible.Coercible⇒Convertible
d_Coercible'8658'Convertible_38 ::
  () ->
  () ->
  MAlonzo.Code.Foreign.Haskell.Coerce.T_Coercible_34
    AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Convertible_8
d_Coercible'8658'Convertible_38 ~v0 ~v1 v2
  = du_Coercible'8658'Convertible_38 v2
du_Coercible'8658'Convertible_38 ::
  MAlonzo.Code.Foreign.Haskell.Coerce.T_Coercible_34
    AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Convertible_8
du_Coercible'8658'Convertible_38 v0
  = coe
      C_Convertible'46'constructor_21
      (coe
         MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
         v0)
      (coe
         MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
         (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
-- Foreign.Convertible.Convertible-Pair
d_Convertible'45'Pair_52 ::
  () ->
  () ->
  () -> () -> T_Convertible_8 -> T_Convertible_8 -> T_Convertible_8
d_Convertible'45'Pair_52 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du_Convertible'45'Pair_52 v4 v5
du_Convertible'45'Pair_52 ::
  T_Convertible_8 -> T_Convertible_8 -> T_Convertible_8
du_Convertible'45'Pair_52 v0 v1
  = coe
      C_Convertible'46'constructor_21
      (coe
         (\ v2 ->
            case coe v2 of
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
                -> coe
                     MAlonzo.Code.Foreign.Haskell.Pair.C__'44'__36 (coe d_to_18 v0 v3)
                     (coe d_to_18 v1 v4)
              _ -> MAlonzo.RTE.mazUnreachableError))
      (coe
         (\ v2 ->
            case coe v2 of
              MAlonzo.Code.Foreign.Haskell.Pair.C__'44'__36 v3 v4
                -> coe
                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe d_from_20 v0 v3)
                     (coe d_from_20 v1 v4)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Foreign.Convertible.Convertible-FinSet
d_Convertible'45'FinSet_68 ::
  () -> () -> T_Convertible_8 -> T_Convertible_8
d_Convertible'45'FinSet_68 ~v0 ~v1 v2
  = du_Convertible'45'FinSet_68 v2
du_Convertible'45'FinSet_68 :: T_Convertible_8 -> T_Convertible_8
du_Convertible'45'FinSet_68 v0
  = coe
      C_Convertible'46'constructor_21
      (coe
         (\ v1 ->
            coe
              MAlonzo.Code.Data.List.Base.du_map_22 (coe d_to_18 (coe v0))
              (coe v1)))
      (coe
         (\ v1 ->
            coe
              MAlonzo.Code.Data.List.Base.du_map_22 (coe d_from_20 (coe v0))
              (coe v1)))
-- Foreign.Convertible.Convertible-Map
d_Convertible'45'Map_88 ::
  () ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_Convertible_8 -> T_Convertible_8 -> T_Convertible_8
d_Convertible'45'Map_88 ~v0 ~v1 ~v2 ~v3 v4 v5 v6
  = du_Convertible'45'Map_88 v4 v5 v6
du_Convertible'45'Map_88 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_Convertible_8 -> T_Convertible_8 -> T_Convertible_8
du_Convertible'45'Map_88 v0 v1 v2
  = coe
      C_Convertible'46'constructor_21
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Data.List.Base.du_map_22
              (coe d_to_18 (coe du_Convertible'45'Pair_52 (coe v1) (coe v2)))
              (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v3))))
      (coe
         (\ v3 ->
            coe
              MAlonzo.Code.Axiom.Set.Map.du_fromList'7504'_492
              (coe
                 MAlonzo.Code.Axiom.Set.d_th_1374
                 (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
              (coe v0)
              (coe
                 MAlonzo.Code.Data.List.Base.du_map_22
                 (coe d_from_20 (coe du_Convertible'45'Pair_52 (coe v1) (coe v2)))
                 (coe v3))))
