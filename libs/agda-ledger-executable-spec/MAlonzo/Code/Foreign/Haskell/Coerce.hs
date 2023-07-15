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

module MAlonzo.Code.Foreign.Haskell.Coerce where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.IO
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.NonEmpty.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Foreign.Haskell.Either
import qualified MAlonzo.Code.Foreign.Haskell.List.NonEmpty
import qualified MAlonzo.Code.Foreign.Haskell.Pair

import Unsafe.Coerce
data AgdaCoercible l1 l2 a b = TrustMe
-- Foreign.Haskell.Coerce.Coercible
d_Coercible_34 a0 a1 a2 a3 = ()
type T_Coercible_34 a0 a1 a2 a3 = AgdaCoercible a0 a1 a2 a3
pattern C_TrustMe_40 = TrustMe
check_TrustMe_40 ::
  forall xa.
    forall xb. forall xA. forall xB. T_Coercible_34 xa xb xA xB
check_TrustMe_40 = TrustMe
cover_Coercible_34 :: AgdaCoercible a1 a2 a3 a4 -> ()
cover_Coercible_34 x
  = case x of
      TrustMe -> ()
-- Foreign.Haskell.Coerce.coerce
d_coerce_44 ::
  forall xA'46'a.
    forall xA.
      forall xB'46'b.
        forall xB.
          MAlonzo.Code.Agda.Primitive.T_Level_14 ->
          () ->
          MAlonzo.Code.Agda.Primitive.T_Level_14 ->
          () -> T_Coercible_34 xA'46'a xB'46'b xA xB -> xA -> xB
d_coerce_44 = \ _ _ _ _ _ -> unsafeCoerce
-- Foreign.Haskell.Coerce.Coercible₁
d_Coercible'8321'_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> (() -> ()) -> ()
d_Coercible'8321'_54 = erased
-- Foreign.Haskell.Coerce.Coercible₂
d_Coercible'8322'_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> () -> ()) -> (() -> () -> ()) -> ()
d_Coercible'8322'_78 = erased
-- Foreign.Haskell.Coerce.nat-toInt
d_nat'45'toInt_90 :: T_Coercible_34 AgdaAny AgdaAny Integer Integer
d_nat'45'toInt_90 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.pair-toFFI
d_pair'45'toFFI_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Coercible_34
    AgdaAny AgdaAny MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
    (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
       AgdaAny AgdaAny AgdaAny AgdaAny)
d_pair'45'toFFI_92 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
  = du_pair'45'toFFI_92
du_pair'45'toFFI_92 ::
  T_Coercible_34
    AgdaAny AgdaAny MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
    (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
       AgdaAny AgdaAny AgdaAny AgdaAny)
du_pair'45'toFFI_92 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.pair-fromFFI
d_pair'45'fromFFI_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Coercible_34
    AgdaAny AgdaAny
    (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
       AgdaAny AgdaAny AgdaAny AgdaAny)
    MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pair'45'fromFFI_94 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
  = du_pair'45'fromFFI_94
du_pair'45'fromFFI_94 ::
  T_Coercible_34
    AgdaAny AgdaAny
    (MAlonzo.Code.Foreign.Haskell.Pair.T_Pair_22
       AgdaAny AgdaAny AgdaAny AgdaAny)
    MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_pair'45'fromFFI_94 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.either-toFFI
d_either'45'toFFI_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Coercible_34
    AgdaAny AgdaAny MAlonzo.Code.Data.Sum.Base.T__'8846'__30
    (MAlonzo.Code.Foreign.Haskell.Either.T_Either_22
       AgdaAny AgdaAny AgdaAny AgdaAny)
d_either'45'toFFI_96 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
  = du_either'45'toFFI_96
du_either'45'toFFI_96 ::
  T_Coercible_34
    AgdaAny AgdaAny MAlonzo.Code.Data.Sum.Base.T__'8846'__30
    (MAlonzo.Code.Foreign.Haskell.Either.T_Either_22
       AgdaAny AgdaAny AgdaAny AgdaAny)
du_either'45'toFFI_96 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.either-fromFFI
d_either'45'fromFFI_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Coercible_34
    AgdaAny AgdaAny
    (MAlonzo.Code.Foreign.Haskell.Either.T_Either_22
       AgdaAny AgdaAny AgdaAny AgdaAny)
    MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_either'45'fromFFI_98 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
  = du_either'45'fromFFI_98
du_either'45'fromFFI_98 ::
  T_Coercible_34
    AgdaAny AgdaAny
    (MAlonzo.Code.Foreign.Haskell.Either.T_Either_22
       AgdaAny AgdaAny AgdaAny AgdaAny)
    MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_either'45'fromFFI_98 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.nonEmpty-toFFI
d_nonEmpty'45'toFFI_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Coercible_34
    AgdaAny AgdaAny
    MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22
    (MAlonzo.Code.Foreign.Haskell.List.NonEmpty.T_NonEmpty_16
       AgdaAny AgdaAny)
d_nonEmpty'45'toFFI_100 ~v0 ~v1 ~v2 ~v3 ~v4
  = du_nonEmpty'45'toFFI_100
du_nonEmpty'45'toFFI_100 ::
  T_Coercible_34
    AgdaAny AgdaAny
    MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22
    (MAlonzo.Code.Foreign.Haskell.List.NonEmpty.T_NonEmpty_16
       AgdaAny AgdaAny)
du_nonEmpty'45'toFFI_100 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.nonEmpty-fromFFI
d_nonEmpty'45'fromFFI_102 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Coercible_34
    AgdaAny AgdaAny
    (MAlonzo.Code.Foreign.Haskell.List.NonEmpty.T_NonEmpty_16
       AgdaAny AgdaAny)
    MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22
d_nonEmpty'45'fromFFI_102 ~v0 ~v1 ~v2 ~v3 ~v4
  = du_nonEmpty'45'fromFFI_102
du_nonEmpty'45'fromFFI_102 ::
  T_Coercible_34
    AgdaAny AgdaAny
    (MAlonzo.Code.Foreign.Haskell.List.NonEmpty.T_NonEmpty_16
       AgdaAny AgdaAny)
    MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22
du_nonEmpty'45'fromFFI_102 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.coerce-maybe
d_coerce'45'maybe_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Coercible_34 AgdaAny AgdaAny (Maybe AgdaAny) (Maybe AgdaAny)
d_coerce'45'maybe_104 ~v0 ~v1 ~v2 ~v3 ~v4 = du_coerce'45'maybe_104
du_coerce'45'maybe_104 ::
  T_Coercible_34 AgdaAny AgdaAny (Maybe AgdaAny) (Maybe AgdaAny)
du_coerce'45'maybe_104 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.coerce-list
d_coerce'45'list_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Coercible_34 AgdaAny AgdaAny [AgdaAny] [AgdaAny]
d_coerce'45'list_106 ~v0 ~v1 ~v2 ~v3 ~v4 = du_coerce'45'list_106
du_coerce'45'list_106 ::
  T_Coercible_34 AgdaAny AgdaAny [AgdaAny] [AgdaAny]
du_coerce'45'list_106 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.coerce-IO
d_coerce'45'IO_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Coercible_34
    AgdaAny AgdaAny
    (MAlonzo.Code.Agda.Builtin.IO.T_IO_8 AgdaAny AgdaAny)
    (MAlonzo.Code.Agda.Builtin.IO.T_IO_8 AgdaAny AgdaAny)
d_coerce'45'IO_108 ~v0 ~v1 ~v2 ~v3 ~v4 = du_coerce'45'IO_108
du_coerce'45'IO_108 ::
  T_Coercible_34
    AgdaAny AgdaAny
    (MAlonzo.Code.Agda.Builtin.IO.T_IO_8 AgdaAny AgdaAny)
    (MAlonzo.Code.Agda.Builtin.IO.T_IO_8 AgdaAny AgdaAny)
du_coerce'45'IO_108 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.coerce-fun
d_coerce'45'fun_116 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  () ->
  () ->
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny ->
  T_Coercible_34
    AgdaAny AgdaAny (AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
d_coerce'45'fun_116 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9
  = du_coerce'45'fun_116
du_coerce'45'fun_116 ::
  T_Coercible_34
    AgdaAny AgdaAny (AgdaAny -> AgdaAny) (AgdaAny -> AgdaAny)
du_coerce'45'fun_116 = coe C_TrustMe_40
-- Foreign.Haskell.Coerce.coerce-refl
d_coerce'45'refl_118 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny
d_coerce'45'refl_118 ~v0 ~v1 = du_coerce'45'refl_118
du_coerce'45'refl_118 ::
  T_Coercible_34 AgdaAny AgdaAny AgdaAny AgdaAny
du_coerce'45'refl_118 = coe C_TrustMe_40
