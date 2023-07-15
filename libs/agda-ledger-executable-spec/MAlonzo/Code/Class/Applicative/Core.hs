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

module MAlonzo.Code.Class.Applicative.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Functor.Core
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.NonEmpty.Base

-- Class.Applicative.Core.Applicative
d_Applicative_20 a0 = ()
data T_Applicative_20
  = C_Applicative'46'constructor_377 MAlonzo.Code.Class.Functor.Core.T_Functor_34
                                     (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                      () -> AgdaAny -> AgdaAny)
                                     (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                      () ->
                                      MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                      () -> AgdaAny -> AgdaAny -> AgdaAny)
-- Class.Applicative.Core.Applicative.super
d_super_30 ::
  T_Applicative_20 -> MAlonzo.Code.Class.Functor.Core.T_Functor_34
d_super_30 v0
  = case coe v0 of
      C_Applicative'46'constructor_377 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Applicative.Core.Applicative.pure
d_pure_32 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_pure_32 v0
  = case coe v0 of
      C_Applicative'46'constructor_377 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Applicative.Core.Applicative._<*>_
d__'60''42''62'__34 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42''62'__34 v0
  = case coe v0 of
      C_Applicative'46'constructor_377 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Applicative.Core.Applicative._⊛_
d__'8859'__36 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859'__36 ~v0 v1 = du__'8859'__36 v1
du__'8859'__36 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859'__36 v0 = coe d__'60''42''62'__34 (coe v0)
-- Class.Applicative.Core.Applicative._<*_
d__'60''42'__38 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42'__38 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'60''42'__38 v1 v2 v4 v6 v7
du__'60''42'__38 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'60''42'__38 v0 v1 v2 v3 v4
  = coe
      du__'8859'__36 v0 v2 erased v1 erased
      (coe
         MAlonzo.Code.Class.Functor.Core.d__'60''36''62'__40
         (d_super_30 (coe v0)) v1 erased () erased (\ v5 v6 -> v5) v3)
      v4
-- Class.Applicative.Core.Applicative._*>_
d__'42''62'__44 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'42''62'__44 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'42''62'__44 v1 v2 v4 v6 v7
du__'42''62'__44 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'42''62'__44 v0 v1 v2 v3 v4
  = coe
      du__'8859'__36 v0 v2 erased v2 erased
      (coe
         MAlonzo.Code.Class.Functor.Core.d__'60''36''62'__40
         (d_super_30 (coe v0)) v1 erased v2 erased (\ v5 v6 -> v6) v3)
      v4
-- Class.Applicative.Core.Applicative._<⊛_
d__'60''8859'__50 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''8859'__50 ~v0 v1 = du__'60''8859'__50 v1
du__'60''8859'__50 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''8859'__50 v0 v1 v2 v3 v4 v5 v6
  = coe du__'60''42'__38 (coe v0) v1 v3 v5 v6
-- Class.Applicative.Core.Applicative._⊛>_
d__'8859''62'__52 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859''62'__52 ~v0 v1 = du__'8859''62'__52 v1
du__'8859''62'__52 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859''62'__52 v0 v1 v2 v3 v4 v5 v6
  = coe du__'42''62'__44 (coe v0) v1 v3 v5 v6
-- Class.Applicative.Core.Applicative._⊗_
d__'8855'__54 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8855'__54 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'8855'__54 v1 v2 v4 v6 v7
du__'8855'__54 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8855'__54 v0 v1 v2 v3 v4
  = coe
      du__'8859'__36 v0 v2 erased () erased
      (coe
         MAlonzo.Code.Class.Functor.Core.d__'60''36''62'__40
         (d_super_30 (coe v0)) v1 erased () erased
         (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32) v3)
      v4
-- Class.Applicative.Core.Applicative.zipWithA
d_zipWithA_60 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_zipWithA_60 ~v0 v1 v2 ~v3 v4 ~v5 v6 ~v7 v8 v9 v10
  = du_zipWithA_60 v1 v2 v4 v6 v8 v9 v10
du_zipWithA_60 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_zipWithA_60 v0 v1 v2 v3 v4 v5 v6
  = coe
      du__'8859'__36 v0 v2 erased v3 erased
      (coe
         MAlonzo.Code.Class.Functor.Core.d__'60''36''62'__40
         (d_super_30 (coe v0)) v1 erased () erased v4 v5)
      v6
-- Class.Applicative.Core.Applicative.zipA
d_zipA_68 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d_zipA_68 ~v0 v1 v2 ~v3 v4 ~v5 = du_zipA_68 v1 v2 v4
du_zipA_68 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_zipA_68 v0 v1 v2
  = coe
      du_zipWithA_60 (coe v0) (coe v1) (coe v2) (coe ())
      (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32)
-- Class.Applicative.Core._._*>_
d__'42''62'__72 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'42''62'__72 ~v0 v1 = du__'42''62'__72 v1
du__'42''62'__72 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'42''62'__72 v0 v1 v2 v3 v4 v5 v6
  = coe du__'42''62'__44 (coe v0) v1 v3 v5 v6
-- Class.Applicative.Core._._<*_
d__'60''42'__74 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42'__74 ~v0 v1 = du__'60''42'__74 v1
du__'60''42'__74 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''42'__74 v0 v1 v2 v3 v4 v5 v6
  = coe du__'60''42'__38 (coe v0) v1 v3 v5 v6
-- Class.Applicative.Core._._<*>_
d__'60''42''62'__76 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42''62'__76 v0 = coe d__'60''42''62'__34 (coe v0)
-- Class.Applicative.Core._._<⊛_
d__'60''8859'__78 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''8859'__78 ~v0 v1 = du__'60''8859'__78 v1
du__'60''8859'__78 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''8859'__78 v0 = coe du__'60''8859'__50 (coe v0)
-- Class.Applicative.Core._._⊗_
d__'8855'__80 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8855'__80 ~v0 v1 = du__'8855'__80 v1
du__'8855'__80 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8855'__80 v0 v1 v2 v3 v4 v5 v6
  = coe du__'8855'__54 (coe v0) v1 v3 v5 v6
-- Class.Applicative.Core._._⊛_
d__'8859'__82 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859'__82 ~v0 v1 = du__'8859'__82 v1
du__'8859'__82 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859'__82 v0 = coe du__'8859'__36 (coe v0)
-- Class.Applicative.Core._._⊛>_
d__'8859''62'__84 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859''62'__84 ~v0 v1 = du__'8859''62'__84 v1
du__'8859''62'__84 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859''62'__84 v0 = coe du__'8859''62'__52 (coe v0)
-- Class.Applicative.Core._.pure
d_pure_86 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_pure_86 v0 = coe d_pure_32 (coe v0)
-- Class.Applicative.Core._.super
d_super_88 ::
  T_Applicative_20 -> MAlonzo.Code.Class.Functor.Core.T_Functor_34
d_super_88 v0 = coe d_super_30 (coe v0)
-- Class.Applicative.Core._.zipA
d_zipA_90 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d_zipA_90 ~v0 v1 = du_zipA_90 v1
du_zipA_90 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du_zipA_90 v0 v1 v2 v3 v4 = coe du_zipA_68 (coe v0) v1 v3
-- Class.Applicative.Core._.zipWithA
d_zipWithA_92 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_zipWithA_92 ~v0 v1 = du_zipWithA_92 v1
du_zipWithA_92 ::
  T_Applicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_zipWithA_92 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe du_zipWithA_60 (coe v0) v1 v3 v5 v7 v8 v9
-- Class.Applicative.Core.Applicative₀
d_Applicative'8320'_96 a0 = ()
data T_Applicative'8320'_96
  = C_Applicative'8320''46'constructor_7629 T_Applicative_20
                                            (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                             () -> AgdaAny)
-- Class.Applicative.Core.Applicative₀.super
d_super_104 :: T_Applicative'8320'_96 -> T_Applicative_20
d_super_104 v0
  = case coe v0 of
      C_Applicative'8320''46'constructor_7629 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Applicative.Core.Applicative₀.ε₀
d_ε'8320'_106 ::
  T_Applicative'8320'_96 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny
d_ε'8320'_106 v0
  = case coe v0 of
      C_Applicative'8320''46'constructor_7629 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Applicative.Core._.super
d_super_110 :: T_Applicative'8320'_96 -> T_Applicative_20
d_super_110 v0 = coe d_super_104 (coe v0)
-- Class.Applicative.Core._.ε₀
d_ε'8320'_112 ::
  T_Applicative'8320'_96 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny
d_ε'8320'_112 v0 = coe d_ε'8320'_106 (coe v0)
-- Class.Applicative.Core.Alternative
d_Alternative_116 a0 = ()
newtype T_Alternative_116
  = C_Alternative'46'constructor_7805 (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                       () -> AgdaAny -> AgdaAny -> AgdaAny)
-- Class.Applicative.Core.Alternative._<|>_
d__'60''124''62'__122 ::
  T_Alternative_116 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''124''62'__122 v0
  = case coe v0 of
      C_Alternative'46'constructor_7805 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Applicative.Core._._<|>_
d__'60''124''62'__126 ::
  T_Alternative_116 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''124''62'__126 v0 = coe d__'60''124''62'__122 (coe v0)
-- Class.Applicative.Core.⋃⁺_
d_'8899''8314'__128 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Alternative_116 ->
  MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22 -> AgdaAny
d_'8899''8314'__128 ~v0 v1 ~v2 v3 = du_'8899''8314'__128 v1 v3
du_'8899''8314'__128 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Alternative_116 ->
  MAlonzo.Code.Data.List.NonEmpty.Base.T_List'8314'_22 -> AgdaAny
du_'8899''8314'__128 v0 v1
  = coe
      MAlonzo.Code.Data.List.NonEmpty.Base.du_foldr'8321'_160
      (coe d__'60''124''62'__122 v1 v0 erased)
-- Class.Applicative.Core.⋃_
d_'8899'__130 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Applicative'8320'_96 -> T_Alternative_116 -> [AgdaAny] -> AgdaAny
d_'8899'__130 ~v0 v1 ~v2 v3 v4 = du_'8899'__130 v1 v3 v4
du_'8899'__130 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Applicative'8320'_96 -> T_Alternative_116 -> [AgdaAny] -> AgdaAny
du_'8899'__130 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Base.du_foldr_242
      (coe d__'60''124''62'__122 v2 v0 erased)
      (coe d_ε'8320'_106 v1 v0 erased)
