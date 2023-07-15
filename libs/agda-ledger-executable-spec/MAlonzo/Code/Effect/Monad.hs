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

module MAlonzo.Code.Effect.Monad where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Effect.Applicative
import qualified MAlonzo.Code.Effect.Choice
import qualified MAlonzo.Code.Effect.Empty
import qualified MAlonzo.Code.Effect.Functor
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Level

-- Effect.Monad.RawMonad
d_RawMonad_24 a0 a1 a2 = ()
data T_RawMonad_24
  = C_RawMonad'46'constructor_319 MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
                                  (() -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny)
-- Effect.Monad.RawMonad.rawApplicative
d_rawApplicative_32 ::
  T_RawMonad_24 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
d_rawApplicative_32 v0
  = case coe v0 of
      C_RawMonad'46'constructor_319 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Effect.Monad.RawMonad._>>=_
d__'62''62''61'__34 ::
  T_RawMonad_24 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61'__34 v0
  = case coe v0 of
      C_RawMonad'46'constructor_319 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Effect.Monad.RawMonad._._*>_
d__'42''62'__38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'42''62'__38 ~v0 ~v1 ~v2 v3 = du__'42''62'__38 v3
du__'42''62'__38 ::
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'42''62'__38 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Effect.Applicative.du__'42''62'__52
      (coe d_rawApplicative_32 (coe v0)) v3 v4
-- Effect.Monad.RawMonad._._<$_
d__'60''36'__40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''36'__40 ~v0 ~v1 ~v2 v3 = du__'60''36'__40 v3
du__'60''36'__40 ::
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''36'__40 v0
  = let v1 = d_rawApplicative_32 (coe v0) in
    \ v2 v3 v4 v5 ->
      coe
        MAlonzo.Code.Effect.Functor.du__'60''36'__32
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v1)) v4
        v5
-- Effect.Monad.RawMonad._._<$>_
d__'60''36''62'__42 ::
  T_RawMonad_24 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''36''62'__42 v0
  = coe
      MAlonzo.Code.Effect.Functor.d__'60''36''62'__30
      (coe
         MAlonzo.Code.Effect.Applicative.d_rawFunctor_30
         (coe d_rawApplicative_32 (coe v0)))
-- Effect.Monad.RawMonad._._<&>_
d__'60''38''62'__44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'60''38''62'__44 ~v0 ~v1 ~v2 v3 = du__'60''38''62'__44 v3
du__'60''38''62'__44 ::
  T_RawMonad_24 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'60''38''62'__44 v0
  = let v1 = d_rawApplicative_32 (coe v0) in
    \ v2 v3 v4 v5 ->
      coe
        MAlonzo.Code.Effect.Functor.du__'60''38''62'__38
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v1)) v4
        v5
-- Effect.Monad.RawMonad._._<*_
d__'60''42'__46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42'__46 ~v0 ~v1 ~v2 v3 = du__'60''42'__46 v3
du__'60''42'__46 ::
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''42'__46 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Effect.Applicative.du__'60''42'__46
      (coe d_rawApplicative_32 (coe v0)) v3 v4
-- Effect.Monad.RawMonad._._<*>_
d__'60''42''62'__48 ::
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42''62'__48 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d__'60''42''62'__34
      (coe d_rawApplicative_32 (coe v0))
-- Effect.Monad.RawMonad._._<⊛_
d__'60''8859'__50 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''8859'__50 ~v0 ~v1 ~v2 v3 = du__'60''8859'__50 v3
du__'60''8859'__50 ::
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''8859'__50 v0 v1 v2
  = coe
      MAlonzo.Code.Effect.Applicative.du__'60''8859'__70
      (coe d_rawApplicative_32 (coe v0))
-- Effect.Monad.RawMonad._._⊗_
d__'8855'__52 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8855'__52 ~v0 ~v1 ~v2 v3 = du__'8855'__52 v3
du__'8855'__52 ::
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8855'__52 v0 v1 v2
  = coe
      MAlonzo.Code.Effect.Applicative.du__'8855'__74
      (coe d_rawApplicative_32 (coe v0))
-- Effect.Monad.RawMonad._._⊛_
d__'8859'__54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859'__54 ~v0 ~v1 ~v2 v3 = du__'8859'__54 v3
du__'8859'__54 ::
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859'__54 v0 v1 v2
  = coe
      MAlonzo.Code.Effect.Applicative.du__'8859'__68
      (coe d_rawApplicative_32 (coe v0))
-- Effect.Monad.RawMonad._._⊛>_
d__'8859''62'__56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859''62'__56 ~v0 ~v1 ~v2 v3 = du__'8859''62'__56 v3
du__'8859''62'__56 ::
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859''62'__56 v0 v1 v2
  = coe
      MAlonzo.Code.Effect.Applicative.du__'8859''62'__72
      (coe d_rawApplicative_32 (coe v0))
-- Effect.Monad.RawMonad._.ignore
d_ignore_58 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonad_24 -> () -> AgdaAny -> AgdaAny
d_ignore_58 ~v0 ~v1 ~v2 v3 = du_ignore_58 v3
du_ignore_58 :: T_RawMonad_24 -> () -> AgdaAny -> AgdaAny
du_ignore_58 v0
  = let v1 = d_rawApplicative_32 (coe v0) in
    \ v2 ->
      coe
        MAlonzo.Code.Effect.Functor.du_ignore_40
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v1))
-- Effect.Monad.RawMonad._.pure
d_pure_60 :: T_RawMonad_24 -> () -> AgdaAny -> AgdaAny
d_pure_60 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d_pure_32
      (coe d_rawApplicative_32 (coe v0))
-- Effect.Monad.RawMonad._.rawFunctor
d_rawFunctor_62 ::
  T_RawMonad_24 -> MAlonzo.Code.Effect.Functor.T_RawFunctor_24
d_rawFunctor_62 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d_rawFunctor_30
      (coe d_rawApplicative_32 (coe v0))
-- Effect.Monad.RawMonad._.zip
d_zip_64 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d_zip_64 ~v0 ~v1 ~v2 v3 = du_zip_64 v3
du_zip_64 ::
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du_zip_64 v0 v1 v2
  = coe
      MAlonzo.Code.Effect.Applicative.du_zip_66
      (coe d_rawApplicative_32 (coe v0))
-- Effect.Monad.RawMonad._.zipWith
d_zipWith_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_zipWith_66 ~v0 ~v1 ~v2 v3 = du_zipWith_66 v3
du_zipWith_66 ::
  T_RawMonad_24 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_zipWith_66 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Effect.Applicative.du_zipWith_58
      (coe d_rawApplicative_32 (coe v0)) v4 v5 v6
-- Effect.Monad.RawMonad._>>_
d__'62''62'__68 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'62''62'__68 ~v0 ~v1 ~v2 v3 ~v4 ~v5 = du__'62''62'__68 v3
du__'62''62'__68 :: T_RawMonad_24 -> AgdaAny -> AgdaAny -> AgdaAny
du__'62''62'__68 v0
  = coe
      MAlonzo.Code.Effect.Applicative.du__'42''62'__52
      (coe d_rawApplicative_32 (coe v0))
-- Effect.Monad.RawMonad._=<<_
d__'61''60''60'__70 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''60''60'__70 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7
  = du__'61''60''60'__70 v3 v6 v7
du__'61''60''60'__70 ::
  T_RawMonad_24 -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''60''60'__70 v0 v1 v2
  = coe d__'62''62''61'__34 v0 erased erased v2 v1
-- Effect.Monad.RawMonad.Kleisli
d_Kleisli_72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonad_24 -> () -> () -> ()
d_Kleisli_72 = erased
-- Effect.Monad.RawMonad._>=>_
d__'62''61''62'__78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'62''61''62'__78 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7 v8 v9
  = du__'62''61''62'__78 v3 v7 v8 v9
du__'62''61''62'__78 ::
  T_RawMonad_24 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'62''61''62'__78 v0 v1 v2 v3
  = coe d__'62''62''61'__34 v0 erased erased (coe v1 v3) v2
-- Effect.Monad.RawMonad._<=<_
d__'60''61''60'__86 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonad_24 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''61''60'__86 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7 v8
  = du__'60''61''60'__86 v3 v7 v8
du__'60''61''60'__86 ::
  T_RawMonad_24 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''61''60'__86 v0 v1 v2
  = coe du__'62''61''62'__78 (coe v0) (coe v2) (coe v1)
-- Effect.Monad.RawMonad.when
d_when_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonad_24 -> Bool -> AgdaAny -> AgdaAny
d_when_88 ~v0 ~v1 ~v2 v3 v4 v5 = du_when_88 v3 v4 v5
du_when_88 :: T_RawMonad_24 -> Bool -> AgdaAny -> AgdaAny
du_when_88 v0 v1 v2
  = if coe v1
      then coe v2
      else coe
             MAlonzo.Code.Effect.Applicative.d_pure_32
             (d_rawApplicative_32 (coe v0)) erased
             (coe
                MAlonzo.Code.Level.C_lift_20
                (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Effect.Monad.RawMonad.unless
d_unless_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonad_24 -> Bool -> AgdaAny -> AgdaAny
d_unless_94 ~v0 ~v1 ~v2 v3 = du_unless_94 v3
du_unless_94 :: T_RawMonad_24 -> Bool -> AgdaAny -> AgdaAny
du_unless_94 v0
  = coe
      MAlonzo.Code.Function.Base.du__'8728''8242'__216
      (coe du_when_88 (coe v0))
      (coe MAlonzo.Code.Data.Bool.Base.d_not_22)
-- Effect.Monad._.mkRawMonad
d_mkRawMonad_112 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> AgdaAny -> AgdaAny) ->
  (() -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny) ->
  T_RawMonad_24
d_mkRawMonad_112 ~v0 ~v1 v2 v3 = du_mkRawMonad_112 v2 v3
du_mkRawMonad_112 ::
  (() -> AgdaAny -> AgdaAny) ->
  (() -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny) ->
  T_RawMonad_24
du_mkRawMonad_112 v0 v1
  = coe
      C_RawMonad'46'constructor_319
      (let v2
             = coe
                 MAlonzo.Code.Effect.Applicative.du_mkRawApplicative_92 (coe v0) in
       coe
         v2
         (\ v3 v4 v5 v6 ->
            coe
              v1 () v4 v5
              (\ v7 -> coe v1 v3 v4 v6 (\ v8 -> coe v0 v4 (coe v7 v8)))))
      (coe (\ v2 v3 -> coe v1 v2 v3))
-- Effect.Monad.RawMonadZero
d_RawMonadZero_140 a0 a1 a2 = ()
data T_RawMonadZero_140
  = C_RawMonadZero'46'constructor_6181 T_RawMonad_24
                                       MAlonzo.Code.Effect.Empty.T_RawEmpty_16
-- Effect.Monad.RawMonadZero.rawMonad
d_rawMonad_148 :: T_RawMonadZero_140 -> T_RawMonad_24
d_rawMonad_148 v0
  = case coe v0 of
      C_RawMonadZero'46'constructor_6181 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Effect.Monad.RawMonadZero.rawEmpty
d_rawEmpty_150 ::
  T_RawMonadZero_140 -> MAlonzo.Code.Effect.Empty.T_RawEmpty_16
d_rawEmpty_150 v0
  = case coe v0 of
      C_RawMonadZero'46'constructor_6181 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Effect.Monad.RawMonadZero._._*>_
d__'42''62'__154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'42''62'__154 ~v0 ~v1 ~v2 v3 = du__'42''62'__154 v3
du__'42''62'__154 ::
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'42''62'__154 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    \ v2 v3 v4 v5 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'42''62'__52
        (coe d_rawApplicative_32 (coe v1)) v4 v5
-- Effect.Monad.RawMonadZero._._<$_
d__'60''36'__156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''36'__156 ~v0 ~v1 ~v2 v3 = du__'60''36'__156 v3
du__'60''36'__156 ::
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''36'__156 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    let v2 = d_rawApplicative_32 (coe v1) in
    \ v3 v4 v5 v6 ->
      coe
        MAlonzo.Code.Effect.Functor.du__'60''36'__32
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v2)) v5
        v6
-- Effect.Monad.RawMonadZero._._<$>_
d__'60''36''62'__158 ::
  T_RawMonadZero_140 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''36''62'__158 v0
  = coe
      MAlonzo.Code.Effect.Functor.d__'60''36''62'__30
      (coe
         MAlonzo.Code.Effect.Applicative.d_rawFunctor_30
         (coe d_rawApplicative_32 (coe d_rawMonad_148 (coe v0))))
-- Effect.Monad.RawMonadZero._._<&>_
d__'60''38''62'__160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'60''38''62'__160 ~v0 ~v1 ~v2 v3 = du__'60''38''62'__160 v3
du__'60''38''62'__160 ::
  T_RawMonadZero_140 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'60''38''62'__160 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    let v2 = d_rawApplicative_32 (coe v1) in
    \ v3 v4 v5 v6 ->
      coe
        MAlonzo.Code.Effect.Functor.du__'60''38''62'__38
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v2)) v5
        v6
-- Effect.Monad.RawMonadZero._._<*_
d__'60''42'__162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42'__162 ~v0 ~v1 ~v2 v3 = du__'60''42'__162 v3
du__'60''42'__162 ::
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''42'__162 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    \ v2 v3 v4 v5 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'60''42'__46
        (coe d_rawApplicative_32 (coe v1)) v4 v5
-- Effect.Monad.RawMonadZero._._<*>_
d__'60''42''62'__164 ::
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42''62'__164 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d__'60''42''62'__34
      (coe d_rawApplicative_32 (coe d_rawMonad_148 (coe v0)))
-- Effect.Monad.RawMonadZero._._<=<_
d__'60''61''60'__166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''61''60'__166 ~v0 ~v1 ~v2 v3 = du__'60''61''60'__166 v3
du__'60''61''60'__166 ::
  T_RawMonadZero_140 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''61''60'__166 v0 v1 v2 v3 v4 v5
  = coe du__'60''61''60'__86 (coe d_rawMonad_148 (coe v0)) v4 v5
-- Effect.Monad.RawMonadZero._._<⊛_
d__'60''8859'__168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''8859'__168 ~v0 ~v1 ~v2 v3 = du__'60''8859'__168 v3
du__'60''8859'__168 ::
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''8859'__168 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    \ v2 v3 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'60''8859'__70
        (coe d_rawApplicative_32 (coe v1))
-- Effect.Monad.RawMonadZero._._=<<_
d__'61''60''60'__170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''60''60'__170 ~v0 ~v1 ~v2 v3 = du__'61''60''60'__170 v3
du__'61''60''60'__170 ::
  T_RawMonadZero_140 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''60''60'__170 v0 v1 v2 v3 v4
  = coe du__'61''60''60'__70 (coe d_rawMonad_148 (coe v0)) v3 v4
-- Effect.Monad.RawMonadZero._._>=>_
d__'62''61''62'__172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'62''61''62'__172 ~v0 ~v1 ~v2 v3 = du__'62''61''62'__172 v3
du__'62''61''62'__172 ::
  T_RawMonadZero_140 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'62''61''62'__172 v0 v1 v2 v3 v4 v5 v6
  = coe du__'62''61''62'__78 (coe d_rawMonad_148 (coe v0)) v4 v5 v6
-- Effect.Monad.RawMonadZero._._>>_
d__'62''62'__174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'62''62'__174 ~v0 ~v1 ~v2 v3 = du__'62''62'__174 v3
du__'62''62'__174 ::
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'62''62'__174 v0 v1 v2
  = coe du__'62''62'__68 (coe d_rawMonad_148 (coe v0))
-- Effect.Monad.RawMonadZero._._>>=_
d__'62''62''61'__176 ::
  T_RawMonadZero_140 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61'__176 v0
  = coe d__'62''62''61'__34 (coe d_rawMonad_148 (coe v0))
-- Effect.Monad.RawMonadZero._._⊗_
d__'8855'__178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8855'__178 ~v0 ~v1 ~v2 v3 = du__'8855'__178 v3
du__'8855'__178 ::
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8855'__178 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    \ v2 v3 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'8855'__74
        (coe d_rawApplicative_32 (coe v1))
-- Effect.Monad.RawMonadZero._._⊛_
d__'8859'__180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859'__180 ~v0 ~v1 ~v2 v3 = du__'8859'__180 v3
du__'8859'__180 ::
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859'__180 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    \ v2 v3 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'8859'__68
        (coe d_rawApplicative_32 (coe v1))
-- Effect.Monad.RawMonadZero._._⊛>_
d__'8859''62'__182 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859''62'__182 ~v0 ~v1 ~v2 v3 = du__'8859''62'__182 v3
du__'8859''62'__182 ::
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859''62'__182 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    \ v2 v3 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'8859''62'__72
        (coe d_rawApplicative_32 (coe v1))
-- Effect.Monad.RawMonadZero._.Kleisli
d_Kleisli_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonadZero_140 -> () -> () -> ()
d_Kleisli_184 = erased
-- Effect.Monad.RawMonadZero._.ignore
d_ignore_186 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonadZero_140 -> () -> AgdaAny -> AgdaAny
d_ignore_186 ~v0 ~v1 ~v2 v3 = du_ignore_186 v3
du_ignore_186 :: T_RawMonadZero_140 -> () -> AgdaAny -> AgdaAny
du_ignore_186 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    let v2 = d_rawApplicative_32 (coe v1) in
    \ v3 ->
      coe
        MAlonzo.Code.Effect.Functor.du_ignore_40
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v2))
-- Effect.Monad.RawMonadZero._.pure
d_pure_188 :: T_RawMonadZero_140 -> () -> AgdaAny -> AgdaAny
d_pure_188 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d_pure_32
      (coe d_rawApplicative_32 (coe d_rawMonad_148 (coe v0)))
-- Effect.Monad.RawMonadZero._.rawApplicative
d_rawApplicative_190 ::
  T_RawMonadZero_140 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
d_rawApplicative_190 v0
  = coe d_rawApplicative_32 (coe d_rawMonad_148 (coe v0))
-- Effect.Monad.RawMonadZero._.rawFunctor
d_rawFunctor_192 ::
  T_RawMonadZero_140 -> MAlonzo.Code.Effect.Functor.T_RawFunctor_24
d_rawFunctor_192 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d_rawFunctor_30
      (coe d_rawApplicative_32 (coe d_rawMonad_148 (coe v0)))
-- Effect.Monad.RawMonadZero._.unless
d_unless_194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonadZero_140 -> Bool -> AgdaAny -> AgdaAny
d_unless_194 ~v0 ~v1 ~v2 v3 = du_unless_194 v3
du_unless_194 :: T_RawMonadZero_140 -> Bool -> AgdaAny -> AgdaAny
du_unless_194 v0 = coe du_unless_94 (coe d_rawMonad_148 (coe v0))
-- Effect.Monad.RawMonadZero._.when
d_when_196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonadZero_140 -> Bool -> AgdaAny -> AgdaAny
d_when_196 ~v0 ~v1 ~v2 v3 = du_when_196 v3
du_when_196 :: T_RawMonadZero_140 -> Bool -> AgdaAny -> AgdaAny
du_when_196 v0 = coe du_when_88 (coe d_rawMonad_148 (coe v0))
-- Effect.Monad.RawMonadZero._.zip
d_zip_198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d_zip_198 ~v0 ~v1 ~v2 v3 = du_zip_198 v3
du_zip_198 ::
  T_RawMonadZero_140 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du_zip_198 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    \ v2 v3 ->
      coe
        MAlonzo.Code.Effect.Applicative.du_zip_66
        (coe d_rawApplicative_32 (coe v1))
-- Effect.Monad.RawMonadZero._.zipWith
d_zipWith_200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_zipWith_200 ~v0 ~v1 ~v2 v3 = du_zipWith_200 v3
du_zipWith_200 ::
  T_RawMonadZero_140 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_zipWith_200 v0
  = let v1 = d_rawMonad_148 (coe v0) in
    \ v2 v3 v4 v5 v6 v7 ->
      coe
        MAlonzo.Code.Effect.Applicative.du_zipWith_58
        (coe d_rawApplicative_32 (coe v1)) v5 v6 v7
-- Effect.Monad.RawMonadZero._.empty
d_empty_204 :: T_RawMonadZero_140 -> () -> AgdaAny
d_empty_204 v0
  = coe
      MAlonzo.Code.Effect.Empty.d_empty_22 (coe d_rawEmpty_150 (coe v0))
-- Effect.Monad.RawMonadZero._.∅
d_'8709'_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonadZero_140 -> () -> AgdaAny
d_'8709'_206 ~v0 ~v1 ~v2 v3 = du_'8709'_206 v3
du_'8709'_206 :: T_RawMonadZero_140 -> () -> AgdaAny
du_'8709'_206 v0 v1
  = coe
      MAlonzo.Code.Effect.Empty.du_'8709'_24
      (coe d_rawEmpty_150 (coe v0))
-- Effect.Monad.RawMonadZero.rawApplicativeZero
d_rawApplicativeZero_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadZero_140 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicativeZero_118
d_rawApplicativeZero_208 ~v0 ~v1 ~v2 v3
  = du_rawApplicativeZero_208 v3
du_rawApplicativeZero_208 ::
  T_RawMonadZero_140 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicativeZero_118
du_rawApplicativeZero_208 v0
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawApplicativeZero'46'constructor_7471
      (coe d_rawApplicative_32 (coe d_rawMonad_148 (coe v0)))
      (coe d_rawEmpty_150 (coe v0))
-- Effect.Monad.RawMonadPlus
d_RawMonadPlus_216 a0 a1 a2 = ()
data T_RawMonadPlus_216
  = C_RawMonadPlus'46'constructor_8031 T_RawMonadZero_140
                                       MAlonzo.Code.Effect.Choice.T_RawChoice_16
-- Effect.Monad.RawMonadPlus.rawMonadZero
d_rawMonadZero_224 :: T_RawMonadPlus_216 -> T_RawMonadZero_140
d_rawMonadZero_224 v0
  = case coe v0 of
      C_RawMonadPlus'46'constructor_8031 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Effect.Monad.RawMonadPlus.rawChoice
d_rawChoice_226 ::
  T_RawMonadPlus_216 -> MAlonzo.Code.Effect.Choice.T_RawChoice_16
d_rawChoice_226 v0
  = case coe v0 of
      C_RawMonadPlus'46'constructor_8031 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Effect.Monad.RawMonadPlus._._*>_
d__'42''62'__230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'42''62'__230 ~v0 ~v1 ~v2 v3 = du__'42''62'__230 v3
du__'42''62'__230 ::
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'42''62'__230 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    \ v3 v4 v5 v6 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'42''62'__52
        (coe d_rawApplicative_32 (coe v2)) v5 v6
-- Effect.Monad.RawMonadPlus._._<$_
d__'60''36'__232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''36'__232 ~v0 ~v1 ~v2 v3 = du__'60''36'__232 v3
du__'60''36'__232 ::
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''36'__232 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    let v3 = d_rawApplicative_32 (coe v2) in
    \ v4 v5 v6 v7 ->
      coe
        MAlonzo.Code.Effect.Functor.du__'60''36'__32
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v3)) v6
        v7
-- Effect.Monad.RawMonadPlus._._<$>_
d__'60''36''62'__234 ::
  T_RawMonadPlus_216 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''36''62'__234 v0
  = coe
      MAlonzo.Code.Effect.Functor.d__'60''36''62'__30
      (coe
         MAlonzo.Code.Effect.Applicative.d_rawFunctor_30
         (coe
            d_rawApplicative_32
            (coe d_rawMonad_148 (coe d_rawMonadZero_224 (coe v0)))))
-- Effect.Monad.RawMonadPlus._._<&>_
d__'60''38''62'__236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'60''38''62'__236 ~v0 ~v1 ~v2 v3 = du__'60''38''62'__236 v3
du__'60''38''62'__236 ::
  T_RawMonadPlus_216 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'60''38''62'__236 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    let v3 = d_rawApplicative_32 (coe v2) in
    \ v4 v5 v6 v7 ->
      coe
        MAlonzo.Code.Effect.Functor.du__'60''38''62'__38
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v3)) v6
        v7
-- Effect.Monad.RawMonadPlus._._<*_
d__'60''42'__238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42'__238 ~v0 ~v1 ~v2 v3 = du__'60''42'__238 v3
du__'60''42'__238 ::
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''42'__238 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    \ v3 v4 v5 v6 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'60''42'__46
        (coe d_rawApplicative_32 (coe v2)) v5 v6
-- Effect.Monad.RawMonadPlus._._<*>_
d__'60''42''62'__240 ::
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42''62'__240 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d__'60''42''62'__34
      (coe
         d_rawApplicative_32
         (coe d_rawMonad_148 (coe d_rawMonadZero_224 (coe v0))))
-- Effect.Monad.RawMonadPlus._._<=<_
d__'60''61''60'__242 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''61''60'__242 ~v0 ~v1 ~v2 v3 = du__'60''61''60'__242 v3
du__'60''61''60'__242 ::
  T_RawMonadPlus_216 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''61''60'__242 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    \ v2 v3 v4 v5 v6 ->
      coe du__'60''61''60'__86 (coe d_rawMonad_148 (coe v1)) v5 v6
-- Effect.Monad.RawMonadPlus._._<⊛_
d__'60''8859'__244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''8859'__244 ~v0 ~v1 ~v2 v3 = du__'60''8859'__244 v3
du__'60''8859'__244 ::
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''8859'__244 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    \ v3 v4 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'60''8859'__70
        (coe d_rawApplicative_32 (coe v2))
-- Effect.Monad.RawMonadPlus._._=<<_
d__'61''60''60'__246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''60''60'__246 ~v0 ~v1 ~v2 v3 = du__'61''60''60'__246 v3
du__'61''60''60'__246 ::
  T_RawMonadPlus_216 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''60''60'__246 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    \ v2 v3 v4 v5 ->
      coe du__'61''60''60'__70 (coe d_rawMonad_148 (coe v1)) v4 v5
-- Effect.Monad.RawMonadPlus._._>=>_
d__'62''61''62'__248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'62''61''62'__248 ~v0 ~v1 ~v2 v3 = du__'62''61''62'__248 v3
du__'62''61''62'__248 ::
  T_RawMonadPlus_216 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'62''61''62'__248 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    \ v2 v3 v4 v5 v6 v7 ->
      coe du__'62''61''62'__78 (coe d_rawMonad_148 (coe v1)) v5 v6 v7
-- Effect.Monad.RawMonadPlus._._>>_
d__'62''62'__250 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'62''62'__250 ~v0 ~v1 ~v2 v3 = du__'62''62'__250 v3
du__'62''62'__250 ::
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'62''62'__250 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    \ v2 v3 -> coe du__'62''62'__68 (coe d_rawMonad_148 (coe v1))
-- Effect.Monad.RawMonadPlus._._>>=_
d__'62''62''61'__252 ::
  T_RawMonadPlus_216 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61'__252 v0
  = coe
      d__'62''62''61'__34
      (coe d_rawMonad_148 (coe d_rawMonadZero_224 (coe v0)))
-- Effect.Monad.RawMonadPlus._._⊗_
d__'8855'__254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8855'__254 ~v0 ~v1 ~v2 v3 = du__'8855'__254 v3
du__'8855'__254 ::
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8855'__254 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    \ v3 v4 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'8855'__74
        (coe d_rawApplicative_32 (coe v2))
-- Effect.Monad.RawMonadPlus._._⊛_
d__'8859'__256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859'__256 ~v0 ~v1 ~v2 v3 = du__'8859'__256 v3
du__'8859'__256 ::
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859'__256 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    \ v3 v4 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'8859'__68
        (coe d_rawApplicative_32 (coe v2))
-- Effect.Monad.RawMonadPlus._._⊛>_
d__'8859''62'__258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859''62'__258 ~v0 ~v1 ~v2 v3 = du__'8859''62'__258 v3
du__'8859''62'__258 ::
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859''62'__258 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    \ v3 v4 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'8859''62'__72
        (coe d_rawApplicative_32 (coe v2))
-- Effect.Monad.RawMonadPlus._.Kleisli
d_Kleisli_260 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonadPlus_216 -> () -> () -> ()
d_Kleisli_260 = erased
-- Effect.Monad.RawMonadPlus._.empty
d_empty_262 :: T_RawMonadPlus_216 -> () -> AgdaAny
d_empty_262 v0
  = coe
      MAlonzo.Code.Effect.Empty.d_empty_22
      (coe d_rawEmpty_150 (coe d_rawMonadZero_224 (coe v0)))
-- Effect.Monad.RawMonadPlus._.ignore
d_ignore_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonadPlus_216 -> () -> AgdaAny -> AgdaAny
d_ignore_264 ~v0 ~v1 ~v2 v3 = du_ignore_264 v3
du_ignore_264 :: T_RawMonadPlus_216 -> () -> AgdaAny -> AgdaAny
du_ignore_264 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    let v3 = d_rawApplicative_32 (coe v2) in
    \ v4 ->
      coe
        MAlonzo.Code.Effect.Functor.du_ignore_40
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v3))
-- Effect.Monad.RawMonadPlus._.pure
d_pure_266 :: T_RawMonadPlus_216 -> () -> AgdaAny -> AgdaAny
d_pure_266 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d_pure_32
      (coe
         d_rawApplicative_32
         (coe d_rawMonad_148 (coe d_rawMonadZero_224 (coe v0))))
-- Effect.Monad.RawMonadPlus._.rawApplicative
d_rawApplicative_268 ::
  T_RawMonadPlus_216 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
d_rawApplicative_268 v0
  = coe
      d_rawApplicative_32
      (coe d_rawMonad_148 (coe d_rawMonadZero_224 (coe v0)))
-- Effect.Monad.RawMonadPlus._.rawApplicativeZero
d_rawApplicativeZero_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicativeZero_118
d_rawApplicativeZero_270 ~v0 ~v1 ~v2 v3
  = du_rawApplicativeZero_270 v3
du_rawApplicativeZero_270 ::
  T_RawMonadPlus_216 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicativeZero_118
du_rawApplicativeZero_270 v0
  = coe du_rawApplicativeZero_208 (coe d_rawMonadZero_224 (coe v0))
-- Effect.Monad.RawMonadPlus._.rawEmpty
d_rawEmpty_272 ::
  T_RawMonadPlus_216 -> MAlonzo.Code.Effect.Empty.T_RawEmpty_16
d_rawEmpty_272 v0
  = coe d_rawEmpty_150 (coe d_rawMonadZero_224 (coe v0))
-- Effect.Monad.RawMonadPlus._.rawFunctor
d_rawFunctor_274 ::
  T_RawMonadPlus_216 -> MAlonzo.Code.Effect.Functor.T_RawFunctor_24
d_rawFunctor_274 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d_rawFunctor_30
      (coe
         d_rawApplicative_32
         (coe d_rawMonad_148 (coe d_rawMonadZero_224 (coe v0))))
-- Effect.Monad.RawMonadPlus._.rawMonad
d_rawMonad_276 :: T_RawMonadPlus_216 -> T_RawMonad_24
d_rawMonad_276 v0
  = coe d_rawMonad_148 (coe d_rawMonadZero_224 (coe v0))
-- Effect.Monad.RawMonadPlus._.unless
d_unless_278 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonadPlus_216 -> Bool -> AgdaAny -> AgdaAny
d_unless_278 ~v0 ~v1 ~v2 v3 = du_unless_278 v3
du_unless_278 :: T_RawMonadPlus_216 -> Bool -> AgdaAny -> AgdaAny
du_unless_278 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    coe du_unless_94 (coe d_rawMonad_148 (coe v1))
-- Effect.Monad.RawMonadPlus._.when
d_when_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonadPlus_216 -> Bool -> AgdaAny -> AgdaAny
d_when_280 ~v0 ~v1 ~v2 v3 = du_when_280 v3
du_when_280 :: T_RawMonadPlus_216 -> Bool -> AgdaAny -> AgdaAny
du_when_280 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    coe du_when_88 (coe d_rawMonad_148 (coe v1))
-- Effect.Monad.RawMonadPlus._.zip
d_zip_282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d_zip_282 ~v0 ~v1 ~v2 v3 = du_zip_282 v3
du_zip_282 ::
  T_RawMonadPlus_216 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du_zip_282 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    \ v3 v4 ->
      coe
        MAlonzo.Code.Effect.Applicative.du_zip_66
        (coe d_rawApplicative_32 (coe v2))
-- Effect.Monad.RawMonadPlus._.zipWith
d_zipWith_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_zipWith_284 ~v0 ~v1 ~v2 v3 = du_zipWith_284 v3
du_zipWith_284 ::
  T_RawMonadPlus_216 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_zipWith_284 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    let v2 = d_rawMonad_148 (coe v1) in
    \ v3 v4 v5 v6 v7 v8 ->
      coe
        MAlonzo.Code.Effect.Applicative.du_zipWith_58
        (coe d_rawApplicative_32 (coe v2)) v6 v7 v8
-- Effect.Monad.RawMonadPlus._.∅
d_'8709'_286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> T_RawMonadPlus_216 -> () -> AgdaAny
d_'8709'_286 ~v0 ~v1 ~v2 v3 = du_'8709'_286 v3
du_'8709'_286 :: T_RawMonadPlus_216 -> () -> AgdaAny
du_'8709'_286 v0
  = let v1 = d_rawMonadZero_224 (coe v0) in
    \ v2 ->
      coe
        MAlonzo.Code.Effect.Empty.du_'8709'_24
        (coe d_rawEmpty_150 (coe v1))
-- Effect.Monad.RawMonadPlus._._<|>_
d__'60''124''62'__290 ::
  T_RawMonadPlus_216 -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''124''62'__290 v0
  = coe
      MAlonzo.Code.Effect.Choice.d__'60''124''62'__22
      (coe d_rawChoice_226 (coe v0))
-- Effect.Monad.RawMonadPlus._._∣_
d__'8739'__292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8739'__292 ~v0 ~v1 ~v2 v3 = du__'8739'__292 v3
du__'8739'__292 ::
  T_RawMonadPlus_216 -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8739'__292 v0 v1
  = coe
      MAlonzo.Code.Effect.Choice.du__'8739'__24
      (coe d_rawChoice_226 (coe v0))
-- Effect.Monad.RawMonadPlus.rawAlternative
d_rawAlternative_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_RawMonadPlus_216 ->
  MAlonzo.Code.Effect.Applicative.T_RawAlternative_176
d_rawAlternative_294 ~v0 ~v1 ~v2 v3 = du_rawAlternative_294 v3
du_rawAlternative_294 ::
  T_RawMonadPlus_216 ->
  MAlonzo.Code.Effect.Applicative.T_RawAlternative_176
du_rawAlternative_294 v0
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawAlternative'46'constructor_9281
      (coe du_rawApplicativeZero_208 (coe d_rawMonadZero_224 (coe v0)))
      (coe d_rawChoice_226 (coe v0))
-- Effect.Monad.RawMonadTd
d_RawMonadTd_306 a0 a1 a2 a3 a4 = ()
data T_RawMonadTd_306
  = C_RawMonadTd'46'constructor_10175 (() -> AgdaAny -> AgdaAny)
                                      T_RawMonad_24
-- Effect.Monad.RawMonadTd.lift
d_lift_316 :: T_RawMonadTd_306 -> () -> AgdaAny -> AgdaAny
d_lift_316 v0
  = case coe v0 of
      C_RawMonadTd'46'constructor_10175 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Effect.Monad.RawMonadTd.rawMonad
d_rawMonad_318 :: T_RawMonadTd_306 -> T_RawMonad_24
d_rawMonad_318 v0
  = case coe v0 of
      C_RawMonadTd'46'constructor_10175 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Effect.Monad.RawMonadTd._._*>_
d__'42''62'__322 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'42''62'__322 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du__'42''62'__322 v5
du__'42''62'__322 ::
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'42''62'__322 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    \ v2 v3 v4 v5 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'42''62'__52
        (coe d_rawApplicative_32 (coe v1)) v4 v5
-- Effect.Monad.RawMonadTd._._<$_
d__'60''36'__324 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''36'__324 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du__'60''36'__324 v5
du__'60''36'__324 ::
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''36'__324 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    let v2 = d_rawApplicative_32 (coe v1) in
    \ v3 v4 v5 v6 ->
      coe
        MAlonzo.Code.Effect.Functor.du__'60''36'__32
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v2)) v5
        v6
-- Effect.Monad.RawMonadTd._._<$>_
d__'60''36''62'__326 ::
  T_RawMonadTd_306 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''36''62'__326 v0
  = coe
      MAlonzo.Code.Effect.Functor.d__'60''36''62'__30
      (coe
         MAlonzo.Code.Effect.Applicative.d_rawFunctor_30
         (coe d_rawApplicative_32 (coe d_rawMonad_318 (coe v0))))
-- Effect.Monad.RawMonadTd._._<&>_
d__'60''38''62'__328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'60''38''62'__328 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du__'60''38''62'__328 v5
du__'60''38''62'__328 ::
  T_RawMonadTd_306 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'60''38''62'__328 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    let v2 = d_rawApplicative_32 (coe v1) in
    \ v3 v4 v5 v6 ->
      coe
        MAlonzo.Code.Effect.Functor.du__'60''38''62'__38
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v2)) v5
        v6
-- Effect.Monad.RawMonadTd._._<*_
d__'60''42'__330 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42'__330 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du__'60''42'__330 v5
du__'60''42'__330 ::
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''42'__330 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    \ v2 v3 v4 v5 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'60''42'__46
        (coe d_rawApplicative_32 (coe v1)) v4 v5
-- Effect.Monad.RawMonadTd._._<*>_
d__'60''42''62'__332 ::
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''42''62'__332 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d__'60''42''62'__34
      (coe d_rawApplicative_32 (coe d_rawMonad_318 (coe v0)))
-- Effect.Monad.RawMonadTd._._<=<_
d__'60''61''60'__334 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''61''60'__334 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du__'60''61''60'__334 v5
du__'60''61''60'__334 ::
  T_RawMonadTd_306 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''61''60'__334 v0 v1 v2 v3 v4 v5
  = coe du__'60''61''60'__86 (coe d_rawMonad_318 (coe v0)) v4 v5
-- Effect.Monad.RawMonadTd._._<⊛_
d__'60''8859'__336 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''8859'__336 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du__'60''8859'__336 v5
du__'60''8859'__336 ::
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''8859'__336 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    \ v2 v3 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'60''8859'__70
        (coe d_rawApplicative_32 (coe v1))
-- Effect.Monad.RawMonadTd._._=<<_
d__'61''60''60'__338 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''60''60'__338 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du__'61''60''60'__338 v5
du__'61''60''60'__338 ::
  T_RawMonadTd_306 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''60''60'__338 v0 v1 v2 v3 v4
  = coe du__'61''60''60'__70 (coe d_rawMonad_318 (coe v0)) v3 v4
-- Effect.Monad.RawMonadTd._._>=>_
d__'62''61''62'__340 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'62''61''62'__340 ~v0 ~v1 ~v2 ~v3 ~v4 v5
  = du__'62''61''62'__340 v5
du__'62''61''62'__340 ::
  T_RawMonadTd_306 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'62''61''62'__340 v0 v1 v2 v3 v4 v5 v6
  = coe du__'62''61''62'__78 (coe d_rawMonad_318 (coe v0)) v4 v5 v6
-- Effect.Monad.RawMonadTd._._>>_
d__'62''62'__342 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'62''62'__342 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du__'62''62'__342 v5
du__'62''62'__342 ::
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'62''62'__342 v0 v1 v2
  = coe du__'62''62'__68 (coe d_rawMonad_318 (coe v0))
-- Effect.Monad.RawMonadTd._._>>=_
d__'62''62''61'__344 ::
  T_RawMonadTd_306 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61'__344 v0
  = coe d__'62''62''61'__34 (coe d_rawMonad_318 (coe v0))
-- Effect.Monad.RawMonadTd._._⊗_
d__'8855'__346 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8855'__346 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du__'8855'__346 v5
du__'8855'__346 ::
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8855'__346 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    \ v2 v3 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'8855'__74
        (coe d_rawApplicative_32 (coe v1))
-- Effect.Monad.RawMonadTd._._⊛_
d__'8859'__348 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859'__348 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du__'8859'__348 v5
du__'8859'__348 ::
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859'__348 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    \ v2 v3 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'8859'__68
        (coe d_rawApplicative_32 (coe v1))
-- Effect.Monad.RawMonadTd._._⊛>_
d__'8859''62'__350 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8859''62'__350 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du__'8859''62'__350 v5
du__'8859''62'__350 ::
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8859''62'__350 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    \ v2 v3 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'8859''62'__72
        (coe d_rawApplicative_32 (coe v1))
-- Effect.Monad.RawMonadTd._.Kleisli
d_Kleisli_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) -> (() -> ()) -> T_RawMonadTd_306 -> () -> () -> ()
d_Kleisli_352 = erased
-- Effect.Monad.RawMonadTd._.ignore
d_ignore_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) -> T_RawMonadTd_306 -> () -> AgdaAny -> AgdaAny
d_ignore_354 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_ignore_354 v5
du_ignore_354 :: T_RawMonadTd_306 -> () -> AgdaAny -> AgdaAny
du_ignore_354 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    let v2 = d_rawApplicative_32 (coe v1) in
    \ v3 ->
      coe
        MAlonzo.Code.Effect.Functor.du_ignore_40
        (coe MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v2))
-- Effect.Monad.RawMonadTd._.pure
d_pure_356 :: T_RawMonadTd_306 -> () -> AgdaAny -> AgdaAny
d_pure_356 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d_pure_32
      (coe d_rawApplicative_32 (coe d_rawMonad_318 (coe v0)))
-- Effect.Monad.RawMonadTd._.rawApplicative
d_rawApplicative_358 ::
  T_RawMonadTd_306 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
d_rawApplicative_358 v0
  = coe d_rawApplicative_32 (coe d_rawMonad_318 (coe v0))
-- Effect.Monad.RawMonadTd._.rawFunctor
d_rawFunctor_360 ::
  T_RawMonadTd_306 -> MAlonzo.Code.Effect.Functor.T_RawFunctor_24
d_rawFunctor_360 v0
  = coe
      MAlonzo.Code.Effect.Applicative.d_rawFunctor_30
      (coe d_rawApplicative_32 (coe d_rawMonad_318 (coe v0)))
-- Effect.Monad.RawMonadTd._.unless
d_unless_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) -> T_RawMonadTd_306 -> Bool -> AgdaAny -> AgdaAny
d_unless_362 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_unless_362 v5
du_unless_362 :: T_RawMonadTd_306 -> Bool -> AgdaAny -> AgdaAny
du_unless_362 v0 = coe du_unless_94 (coe d_rawMonad_318 (coe v0))
-- Effect.Monad.RawMonadTd._.when
d_when_364 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) -> T_RawMonadTd_306 -> Bool -> AgdaAny -> AgdaAny
d_when_364 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_when_364 v5
du_when_364 :: T_RawMonadTd_306 -> Bool -> AgdaAny -> AgdaAny
du_when_364 v0 = coe du_when_88 (coe d_rawMonad_318 (coe v0))
-- Effect.Monad.RawMonadTd._.zip
d_zip_366 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d_zip_366 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_zip_366 v5
du_zip_366 ::
  T_RawMonadTd_306 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du_zip_366 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    \ v2 v3 ->
      coe
        MAlonzo.Code.Effect.Applicative.du_zip_66
        (coe d_rawApplicative_32 (coe v1))
-- Effect.Monad.RawMonadTd._.zipWith
d_zipWith_368 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  (() -> ()) ->
  T_RawMonadTd_306 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_zipWith_368 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_zipWith_368 v5
du_zipWith_368 ::
  T_RawMonadTd_306 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_zipWith_368 v0
  = let v1 = d_rawMonad_318 (coe v0) in
    \ v2 v3 v4 v5 v6 v7 ->
      coe
        MAlonzo.Code.Effect.Applicative.du_zipWith_58
        (coe d_rawApplicative_32 (coe v1)) v5 v6 v7
-- Effect.Monad.RawMonadT
d_RawMonadT_372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  ((() -> ()) -> () -> ()) -> ()
d_RawMonadT_372 = erased
