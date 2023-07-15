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

module MAlonzo.Code.Class.Monad.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Applicative.Core

-- Class.Monad.Core.Monad
d_Monad_22 a0 = ()
data T_Monad_22
  = C_Monad'46'constructor_377 MAlonzo.Code.Class.Applicative.Core.T_Applicative_20
                               (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                () -> AgdaAny -> AgdaAny)
                               (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                () ->
                                MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny)
-- Class.Monad.Core.Monad.super
d_super_32 ::
  T_Monad_22 -> MAlonzo.Code.Class.Applicative.Core.T_Applicative_20
d_super_32 v0
  = case coe v0 of
      C_Monad'46'constructor_377 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Core.Monad.return
d_return_34 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_return_34 v0
  = case coe v0 of
      C_Monad'46'constructor_377 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Core.Monad._>>=_
d__'62''62''61'__36 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61'__36 v0
  = case coe v0 of
      C_Monad'46'constructor_377 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Core.Monad._>>_
d__'62''62'__38 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'62''62'__38 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'62''62'__38 v1 v2 v4 v6 v7
du__'62''62'__38 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'62''62'__38 v0 v1 v2 v3 v4
  = coe d__'62''62''61'__36 v0 v1 erased v2 erased v3 (\ v5 -> v4)
-- Class.Monad.Core.Monad._=<<_
d__'61''60''60'__46 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''60''60'__46 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'61''60''60'__46 v1 v2 v4 v6 v7
du__'61''60''60'__46 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''60''60'__46 v0 v1 v2 v3 v4
  = coe d__'62''62''61'__36 v0 v1 erased v2 erased v4 v3
-- Class.Monad.Core.Monad._≫=_
d__'8811''61'__52 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'8811''61'__52 ~v0 v1 = du__'8811''61'__52 v1
du__'8811''61'__52 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'8811''61'__52 v0 = coe d__'62''62''61'__36 (coe v0)
-- Class.Monad.Core.Monad._≫_
d__'8811'__54 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8811'__54 ~v0 v1 = du__'8811'__54 v1
du__'8811'__54 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8811'__54 v0 v1 v2 v3 v4 v5 v6
  = coe du__'62''62'__38 (coe v0) v1 v3 v5 v6
-- Class.Monad.Core.Monad._=≪_
d__'61''8810'__56 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''8810'__56 ~v0 v1 = du__'61''8810'__56 v1
du__'61''8810'__56 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''8810'__56 v0 v1 v2 v3 v4 v5 v6
  = coe du__'61''60''60'__46 (coe v0) v1 v3 v5 v6
-- Class.Monad.Core.Monad._>=>_
d__'62''61''62'__58 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'62''61''62'__58 ~v0 v1 ~v2 ~v3 v4 ~v5 v6 ~v7 v8 v9 v10
  = du__'62''61''62'__58 v1 v4 v6 v8 v9 v10
du__'62''61''62'__58 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'62''61''62'__58 v0 v1 v2 v3 v4 v5
  = coe
      du__'61''60''60'__46 (coe v0) (coe v1) (coe v2) (coe v4)
      (coe v3 v5)
-- Class.Monad.Core.Monad._<=<_
d__'60''61''60'__64 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''61''60'__64 ~v0 v1 v2 ~v3 v4 ~v5 ~v6 ~v7 v8 v9
  = du__'60''61''60'__64 v1 v2 v4 v8 v9
du__'60''61''60'__64 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''61''60'__64 v0 v1 v2 v3 v4
  = coe
      du__'62''61''62'__58 (coe v0) (coe v1) (coe v2) (coe v4) (coe v3)
-- Class.Monad.Core.Monad.join
d_join_70 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_join_70 ~v0 v1 v2 ~v3 v4 = du_join_70 v1 v2 v4
du_join_70 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> AgdaAny -> AgdaAny
du_join_70 v0 v1 v2
  = coe d__'62''62''61'__36 v0 v1 erased v1 erased v2 (\ v3 -> v3)
-- Class.Monad.Core._._<=<_
d__'60''61''60'__76 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''61''60'__76 ~v0 v1 = du__'60''61''60'__76 v1
du__'60''61''60'__76 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''61''60'__76 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe du__'60''61''60'__64 (coe v0) v1 v3 v7 v8
-- Class.Monad.Core._._=<<_
d__'61''60''60'__78 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''60''60'__78 ~v0 v1 = du__'61''60''60'__78 v1
du__'61''60''60'__78 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''60''60'__78 v0 v1 v2 v3 v4 v5 v6
  = coe du__'61''60''60'__46 (coe v0) v1 v3 v5 v6
-- Class.Monad.Core._._=≪_
d__'61''8810'__80 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''8810'__80 ~v0 v1 = du__'61''8810'__80 v1
du__'61''8810'__80 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''8810'__80 v0 = coe du__'61''8810'__56 (coe v0)
-- Class.Monad.Core._._>=>_
d__'62''61''62'__82 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'62''61''62'__82 ~v0 v1 = du__'62''61''62'__82 v1
du__'62''61''62'__82 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'62''61''62'__82 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe du__'62''61''62'__58 (coe v0) v3 v5 v7 v8 v9
-- Class.Monad.Core._._>>_
d__'62''62'__84 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'62''62'__84 ~v0 v1 = du__'62''62'__84 v1
du__'62''62'__84 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'62''62'__84 v0 v1 v2 v3 v4 v5 v6
  = coe du__'62''62'__38 (coe v0) v1 v3 v5 v6
-- Class.Monad.Core._._>>=_
d__'62''62''61'__86 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61'__86 v0 = coe d__'62''62''61'__36 (coe v0)
-- Class.Monad.Core._._≫_
d__'8811'__88 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8811'__88 ~v0 v1 = du__'8811'__88 v1
du__'8811'__88 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8811'__88 v0 = coe du__'8811'__54 (coe v0)
-- Class.Monad.Core._._≫=_
d__'8811''61'__90 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'8811''61'__90 ~v0 v1 = du__'8811''61'__90 v1
du__'8811''61'__90 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'8811''61'__90 v0 = coe du__'8811''61'__52 (coe v0)
-- Class.Monad.Core._.join
d_join_92 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_join_92 ~v0 v1 = du_join_92 v1
du_join_92 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
du_join_92 v0 v1 v2 v3 = coe du_join_70 (coe v0) v1 v3
-- Class.Monad.Core._.return
d_return_94 ::
  T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_return_94 v0 = coe d_return_34 (coe v0)
-- Class.Monad.Core._.super
d_super_96 ::
  T_Monad_22 -> MAlonzo.Code.Class.Applicative.Core.T_Applicative_20
d_super_96 v0 = coe d_super_32 (coe v0)
-- Class.Monad.Core.Monad-Laws
d_Monad'45'Laws_102 a0 a1 = ()
data T_Monad'45'Laws_102 = C_Monad'45'Laws'46'constructor_18175
-- Class.Monad.Core.Monad-Laws.>>=-identityˡ
d_'62''62''61''45'identity'737'_148 ::
  T_Monad'45'Laws_102 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'62''62''61''45'identity'737'_148 = erased
-- Class.Monad.Core.Monad-Laws.>>=-identityʳ
d_'62''62''61''45'identity'691'_154 ::
  T_Monad'45'Laws_102 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'62''62''61''45'identity'691'_154 = erased
-- Class.Monad.Core.Monad-Laws.>>=-assoc
d_'62''62''61''45'assoc_170 ::
  T_Monad'45'Laws_102 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'62''62''61''45'assoc_170 = erased
-- Class.Monad.Core._.>>=-assoc
d_'62''62''61''45'assoc_174 ::
  T_Monad'45'Laws_102 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'62''62''61''45'assoc_174 = erased
-- Class.Monad.Core._.>>=-identityʳ
d_'62''62''61''45'identity'691'_176 ::
  T_Monad'45'Laws_102 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'62''62''61''45'identity'691'_176 = erased
-- Class.Monad.Core._.>>=-identityˡ
d_'62''62''61''45'identity'737'_178 ::
  T_Monad'45'Laws_102 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_'62''62''61''45'identity'737'_178 = erased
-- Class.Monad.Core.Lawful-Monad
d_Lawful'45'Monad_182 a0 = ()
newtype T_Lawful'45'Monad_182
  = C_Lawful'45'Monad'46'constructor_18631 T_Monad_22
-- Class.Monad.Core.Lawful-Monad.isMonad
d_isMonad_190 :: T_Lawful'45'Monad_182 -> T_Monad_22
d_isMonad_190 v0
  = case coe v0 of
      C_Lawful'45'Monad'46'constructor_18631 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Core.Lawful-Monad.hasMonadLaws
d_hasMonadLaws_192 :: T_Lawful'45'Monad_182 -> T_Monad'45'Laws_102
d_hasMonadLaws_192 = erased
-- Class.Monad.Core.mkLawful-Monad
d_mkLawful'45'Monad_198 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 -> T_Monad'45'Laws_102 -> T_Lawful'45'Monad_182
d_mkLawful'45'Monad_198 ~v0 v1 ~v2 = du_mkLawful'45'Monad_198 v1
du_mkLawful'45'Monad_198 :: T_Monad_22 -> T_Lawful'45'Monad_182
du_mkLawful'45'Monad_198 v0
  = coe C_Lawful'45'Monad'46'constructor_18631 v0
-- Class.Monad.Core.Monad′
d_Monad'8242'_206 a0 a1 a2 = ()
data T_Monad'8242'_206
  = C_Monad'8242''46'constructor_19351 (() -> AgdaAny -> AgdaAny)
                                       (() -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny)
-- Class.Monad.Core.Monad′.return′
d_return'8242'_214 :: T_Monad'8242'_206 -> () -> AgdaAny -> AgdaAny
d_return'8242'_214 v0
  = case coe v0 of
      C_Monad'8242''46'constructor_19351 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Core.Monad′._>>=′_
d__'62''62''61''8242'__216 ::
  T_Monad'8242'_206 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61''8242'__216 v0
  = case coe v0 of
      C_Monad'8242''46'constructor_19351 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Core.Monad′._>>′_
d__'62''62''8242'__218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'62''62''8242'__218 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7
  = du__'62''62''8242'__218 v3 v6 v7
du__'62''62''8242'__218 ::
  T_Monad'8242'_206 -> AgdaAny -> AgdaAny -> AgdaAny
du__'62''62''8242'__218 v0 v1 v2
  = coe d__'62''62''61''8242'__216 v0 erased erased v1 (\ v3 -> v2)
-- Class.Monad.Core.Monad′._=<<′_
d__'61''60''60''8242'__226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''60''60''8242'__226 ~v0 ~v1 ~v2 v3 ~v4 ~v5 v6 v7
  = du__'61''60''60''8242'__226 v3 v6 v7
du__'61''60''60''8242'__226 ::
  T_Monad'8242'_206 -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''60''60''8242'__226 v0 v1 v2
  = coe d__'62''62''61''8242'__216 v0 erased erased v2 v1
-- Class.Monad.Core.Monad′._>=>′_
d__'62''61''62''8242'__232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'62''61''62''8242'__232 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du__'62''61''62''8242'__232 v3 v8 v9 v10
du__'62''61''62''8242'__232 ::
  T_Monad'8242'_206 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'62''61''62''8242'__232 v0 v1 v2 v3
  = coe du__'61''60''60''8242'__226 (coe v0) (coe v2) (coe v1 v3)
-- Class.Monad.Core.Monad′._<=<′_
d__'60''61''60''8242'__238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''61''60''8242'__238 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du__'60''61''60''8242'__238 v3 v8 v9
du__'60''61''60''8242'__238 ::
  T_Monad'8242'_206 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''61''60''8242'__238 v0 v1 v2
  = coe du__'62''61''62''8242'__232 (coe v0) (coe v2) (coe v1)
-- Class.Monad.Core.Monad′._≫=′_
d__'8811''61''8242'__244 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'8811''61''8242'__244 ~v0 ~v1 ~v2 v3
  = du__'8811''61''8242'__244 v3
du__'8811''61''8242'__244 ::
  T_Monad'8242'_206 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'8811''61''8242'__244 v0
  = coe d__'62''62''61''8242'__216 (coe v0)
-- Class.Monad.Core.Monad′._≫′_
d__'8811''8242'__246 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8811''8242'__246 ~v0 ~v1 ~v2 v3 = du__'8811''8242'__246 v3
du__'8811''8242'__246 ::
  T_Monad'8242'_206 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8811''8242'__246 v0 v1 v2 v3 v4
  = coe du__'62''62''8242'__218 (coe v0) v3 v4
-- Class.Monad.Core.Monad′._=≪′_
d__'61''8810''8242'__248 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''8810''8242'__248 ~v0 ~v1 ~v2 v3
  = du__'61''8810''8242'__248 v3
du__'61''8810''8242'__248 ::
  T_Monad'8242'_206 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''8810''8242'__248 v0 v1 v2 v3 v4
  = coe du__'61''60''60''8242'__226 (coe v0) v3 v4
-- Class.Monad.Core._._<=<′_
d__'60''61''60''8242'__252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''61''60''8242'__252 ~v0 ~v1 ~v2 v3
  = du__'60''61''60''8242'__252 v3
du__'60''61''60''8242'__252 ::
  T_Monad'8242'_206 ->
  () ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''61''60''8242'__252 v0 v1 v2 v3 v4 v5 v6
  = coe du__'60''61''60''8242'__238 (coe v0) v5 v6
-- Class.Monad.Core._._=<<′_
d__'61''60''60''8242'__254 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''60''60''8242'__254 ~v0 ~v1 ~v2 v3
  = du__'61''60''60''8242'__254 v3
du__'61''60''60''8242'__254 ::
  T_Monad'8242'_206 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''60''60''8242'__254 v0 v1 v2 v3 v4
  = coe du__'61''60''60''8242'__226 (coe v0) v3 v4
-- Class.Monad.Core._._=≪′_
d__'61''8810''8242'__256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''8810''8242'__256 ~v0 ~v1 ~v2 v3
  = du__'61''8810''8242'__256 v3
du__'61''8810''8242'__256 ::
  T_Monad'8242'_206 ->
  () -> () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''8810''8242'__256 v0
  = coe du__'61''8810''8242'__248 (coe v0)
-- Class.Monad.Core._._>=>′_
d__'62''61''62''8242'__258 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'62''61''62''8242'__258 ~v0 ~v1 ~v2 v3
  = du__'62''61''62''8242'__258 v3
du__'62''61''62''8242'__258 ::
  T_Monad'8242'_206 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'62''61''62''8242'__258 v0 v1 v2 v3 v4 v5 v6 v7
  = coe du__'62''61''62''8242'__232 (coe v0) v5 v6 v7
-- Class.Monad.Core._._>>=′_
d__'62''62''61''8242'__260 ::
  T_Monad'8242'_206 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61''8242'__260 v0
  = coe d__'62''62''61''8242'__216 (coe v0)
-- Class.Monad.Core._._>>′_
d__'62''62''8242'__262 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'62''62''8242'__262 ~v0 ~v1 ~v2 v3 = du__'62''62''8242'__262 v3
du__'62''62''8242'__262 ::
  T_Monad'8242'_206 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'62''62''8242'__262 v0 v1 v2 v3 v4
  = coe du__'62''62''8242'__218 (coe v0) v3 v4
-- Class.Monad.Core._._≫=′_
d__'8811''61''8242'__264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'8811''61''8242'__264 ~v0 ~v1 ~v2 v3
  = du__'8811''61''8242'__264 v3
du__'8811''61''8242'__264 ::
  T_Monad'8242'_206 ->
  () -> () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'8811''61''8242'__264 v0
  = coe du__'8811''61''8242'__244 (coe v0)
-- Class.Monad.Core._._≫′_
d__'8811''8242'__266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  T_Monad'8242'_206 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8811''8242'__266 ~v0 ~v1 ~v2 v3 = du__'8811''8242'__266 v3
du__'8811''8242'__266 ::
  T_Monad'8242'_206 -> () -> () -> AgdaAny -> AgdaAny -> AgdaAny
du__'8811''8242'__266 v0 = coe du__'8811''8242'__246 (coe v0)
-- Class.Monad.Core._.return′
d_return'8242'_268 :: T_Monad'8242'_206 -> () -> AgdaAny -> AgdaAny
d_return'8242'_268 v0 = coe d_return'8242'_214 (coe v0)
-- Class.Monad.Core.Monad₀
d_Monad'8320'_272 a0 = ()
data T_Monad'8320'_272
  = C_Monad'8320''46'constructor_22909 T_Monad_22
                                       MAlonzo.Code.Class.Applicative.Core.T_Applicative'8320'_96
-- Class.Monad.Core.Monad₀.isMonad
d_isMonad_280 :: T_Monad'8320'_272 -> T_Monad_22
d_isMonad_280 v0
  = case coe v0 of
      C_Monad'8320''46'constructor_22909 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Core.Monad₀.isApplicative₀
d_isApplicative'8320'_282 ::
  T_Monad'8320'_272 ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative'8320'_96
d_isApplicative'8320'_282 v0
  = case coe v0 of
      C_Monad'8320''46'constructor_22909 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Core.mkMonad₀
d_mkMonad'8320'_286 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative'8320'_96 ->
  T_Monad'8320'_272
d_mkMonad'8320'_286 ~v0 v1 v2 = du_mkMonad'8320'_286 v1 v2
du_mkMonad'8320'_286 ::
  T_Monad_22 ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative'8320'_96 ->
  T_Monad'8320'_272
du_mkMonad'8320'_286 v0 v1
  = coe C_Monad'8320''46'constructor_22909 (coe v0) (coe v1)
-- Class.Monad.Core.Monad⁺
d_Monad'8314'_290 a0 = ()
data T_Monad'8314'_290
  = C_Monad'8314''46'constructor_23267 T_Monad_22
                                       MAlonzo.Code.Class.Applicative.Core.T_Alternative_116
-- Class.Monad.Core.Monad⁺.isMonad
d_isMonad_298 :: T_Monad'8314'_290 -> T_Monad_22
d_isMonad_298 v0
  = case coe v0 of
      C_Monad'8314''46'constructor_23267 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Core.Monad⁺.isAlternative
d_isAlternative_300 ::
  T_Monad'8314'_290 ->
  MAlonzo.Code.Class.Applicative.Core.T_Alternative_116
d_isAlternative_300 v0
  = case coe v0 of
      C_Monad'8314''46'constructor_23267 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Core.mkMonad⁺
d_mkMonad'8314'_304 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_22 ->
  MAlonzo.Code.Class.Applicative.Core.T_Alternative_116 ->
  T_Monad'8314'_290
d_mkMonad'8314'_304 ~v0 v1 v2 = du_mkMonad'8314'_304 v1 v2
du_mkMonad'8314'_304 ::
  T_Monad_22 ->
  MAlonzo.Code.Class.Applicative.Core.T_Alternative_116 ->
  T_Monad'8314'_290
du_mkMonad'8314'_304 v0 v1
  = coe C_Monad'8314''46'constructor_23267 (coe v0) (coe v1)
