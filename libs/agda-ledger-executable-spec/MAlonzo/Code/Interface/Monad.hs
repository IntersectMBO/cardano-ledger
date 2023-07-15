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

module MAlonzo.Code.Interface.Monad where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Primitive

-- Interface.Monad.Monad
d_Monad_20 a0 = ()
data T_Monad_20
  = C_Monad'46'constructor_273 (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                () -> AgdaAny -> AgdaAny)
                               (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                () ->
                                MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny)
-- Interface.Monad.Monad.return
d_return_28 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_return_28 v0
  = case coe v0 of
      C_Monad'46'constructor_273 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.Monad.Monad._>>=_
d__'62''62''61'__30 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61'__30 v0
  = case coe v0 of
      C_Monad'46'constructor_273 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.Monad.Monad._>>_
d__'62''62'__32 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'62''62'__32 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'62''62'__32 v1 v2 v4 v6 v7
du__'62''62'__32 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'62''62'__32 v0 v1 v2 v3 v4
  = coe d__'62''62''61'__30 v0 v1 erased v2 erased v3 (\ v5 -> v4)
-- Interface.Monad.Monad._=<<_
d__'61''60''60'__40 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''60''60'__40 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'61''60''60'__40 v1 v2 v4 v6 v7
du__'61''60''60'__40 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''60''60'__40 v0 v1 v2 v3 v4
  = coe d__'62''62''61'__30 v0 v1 erased v2 erased v4 v3
-- Interface.Monad.Monad._>=>_
d__'62''61''62'__46 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'62''61''62'__46 ~v0 v1 ~v2 ~v3 v4 ~v5 v6 ~v7 v8 v9 v10
  = du__'62''61''62'__46 v1 v4 v6 v8 v9 v10
du__'62''61''62'__46 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'62''61''62'__46 v0 v1 v2 v3 v4 v5
  = coe
      du__'61''60''60'__40 (coe v0) (coe v1) (coe v2) (coe v4)
      (coe v3 v5)
-- Interface.Monad.Monad._<=<_
d__'60''61''60'__52 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''61''60'__52 ~v0 v1 v2 ~v3 v4 ~v5 ~v6 ~v7 v8 v9
  = du__'60''61''60'__52 v1 v2 v4 v8 v9
du__'60''61''60'__52 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''61''60'__52 v0 v1 v2 v3 v4
  = coe
      du__'62''61''62'__46 (coe v0) (coe v1) (coe v2) (coe v4) (coe v3)
-- Interface.Monad.Monad.join
d_join_58 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_join_58 ~v0 v1 v2 ~v3 v4 = du_join_58 v1 v2 v4
du_join_58 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> AgdaAny -> AgdaAny
du_join_58 v0 v1 v2
  = coe d__'62''62''61'__30 v0 v1 erased v1 erased v2 (\ v3 -> v3)
-- Interface.Monad.Monad._<$>_
d__'60''36''62'__62 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''36''62'__62 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'60''36''62'__62 v1 v2 v4 v6 v7
du__'60''36''62'__62 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''36''62'__62 v0 v1 v2 v3 v4
  = coe
      du__'61''60''60'__40 (coe v0) (coe v1) (coe v2)
      (coe (\ v5 -> coe d_return_28 v0 v2 erased (coe v3 v5))) (coe v4)
-- Interface.Monad.Monad._<&>_
d__'60''38''62'__68 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'60''38''62'__68 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'60''38''62'__68 v1 v2 v4 v6 v7
du__'60''38''62'__68 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'60''38''62'__68 v0 v1 v2 v3 v4
  = coe
      du__'60''36''62'__62 (coe v0) (coe v1) (coe v2) (coe v4) (coe v3)
-- Interface.Monad.Monad.traverseList
d_traverseList_70 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
d_traverseList_70 ~v0 v1 ~v2 ~v3 v4 ~v5 v6 v7
  = du_traverseList_70 v1 v4 v6 v7
du_traverseList_70 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
du_traverseList_70 v0 v1 v2 v3
  = case coe v3 of
      [] -> coe d_return_28 v0 v1 erased v3
      (:) v4 v5
        -> coe
             d__'62''62''61'__30 v0 v1 erased v1 erased (coe v2 v4)
             (\ v6 ->
                coe
                  du__'60''36''62'__62 (coe v0) (coe v1) (coe v1)
                  (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v6))
                  (coe du_traverseList_70 (coe v0) (coe v1) (coe v2) (coe v5)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.Monad.Monad.sequenceList
d_sequenceList_84 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> AgdaAny
d_sequenceList_84 ~v0 v1 v2 ~v3 = du_sequenceList_84 v1 v2
du_sequenceList_84 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> [AgdaAny] -> AgdaAny
du_sequenceList_84 v0 v1
  = coe du_traverseList_70 (coe v0) (coe v1) (coe (\ v2 -> v2))
-- Interface.Monad.Monad.traverseMaybe
d_traverseMaybe_86 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> Maybe AgdaAny -> AgdaAny
d_traverseMaybe_86 ~v0 v1 ~v2 ~v3 v4 ~v5 v6 v7
  = du_traverseMaybe_86 v1 v4 v6 v7
du_traverseMaybe_86 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> Maybe AgdaAny -> AgdaAny
du_traverseMaybe_86 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v4
        -> coe
             du__'60''36''62'__62 (coe v0) (coe v1) (coe v1)
             (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16) (coe v2 v4)
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
        -> coe d_return_28 v0 v1 erased v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.Monad.Monad.sequenceMaybe
d_sequenceMaybe_94 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Maybe AgdaAny -> AgdaAny
d_sequenceMaybe_94 ~v0 v1 v2 ~v3 = du_sequenceMaybe_94 v1 v2
du_sequenceMaybe_94 ::
  T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> Maybe AgdaAny -> AgdaAny
du_sequenceMaybe_94 v0 v1
  = coe du_traverseMaybe_86 (coe v0) (coe v1) (coe (\ v2 -> v2))
