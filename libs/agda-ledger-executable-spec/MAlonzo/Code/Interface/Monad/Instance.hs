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

module MAlonzo.Code.Interface.Monad.Instance where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Interface.Monad

-- Interface.Monad.Instance._._<$>_
d__'60''36''62'__8 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''36''62'__8 ~v0 v1 = du__'60''36''62'__8 v1
du__'60''36''62'__8 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''36''62'__8 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''36''62'__62 (coe v0) v1 v3 v5
      v6
-- Interface.Monad.Instance._._<&>_
d__'60''38''62'__10 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'60''38''62'__10 ~v0 v1 = du__'60''38''62'__10 v1
du__'60''38''62'__10 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'60''38''62'__10 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''38''62'__68 (coe v0) v1 v3 v5
      v6
-- Interface.Monad.Instance._._<=<_
d__'60''61''60'__12 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''61''60'__12 ~v0 v1 = du__'60''61''60'__12 v1
du__'60''61''60'__12 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'60''61''60'__12 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      MAlonzo.Code.Interface.Monad.du__'60''61''60'__52 (coe v0) v1 v3 v7
      v8
-- Interface.Monad.Instance._._=<<_
d__'61''60''60'__14 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'61''60''60'__14 ~v0 v1 = du__'61''60''60'__14 v1
du__'61''60''60'__14 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'61''60''60'__14 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Interface.Monad.du__'61''60''60'__40 (coe v0) v1 v3 v5
      v6
-- Interface.Monad.Instance._._>=>_
d__'62''61''62'__16 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'62''61''62'__16 ~v0 v1 = du__'62''61''62'__16 v1
du__'62''61''62'__16 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du__'62''61''62'__16 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe
      MAlonzo.Code.Interface.Monad.du__'62''61''62'__46 (coe v0) v3 v5 v7
      v8 v9
-- Interface.Monad.Instance._._>>_
d__'62''62'__18 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'62''62'__18 ~v0 v1 = du__'62''62'__18 v1
du__'62''62'__18 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'62''62'__18 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Interface.Monad.du__'62''62'__32 (coe v0) v1 v3 v5 v6
-- Interface.Monad.Instance._._>>=_
d__'62''62''61'__20 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'62''62''61'__20 v0
  = coe MAlonzo.Code.Interface.Monad.d__'62''62''61'__30 (coe v0)
-- Interface.Monad.Instance._.join
d_join_22 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_join_22 ~v0 v1 = du_join_22 v1
du_join_22 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
du_join_22 v0 v1 v2 v3
  = coe MAlonzo.Code.Interface.Monad.du_join_58 (coe v0) v1 v3
-- Interface.Monad.Instance._.return
d_return_24 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_return_24 v0
  = coe MAlonzo.Code.Interface.Monad.d_return_28 (coe v0)
-- Interface.Monad.Instance._.sequenceList
d_sequenceList_26 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> AgdaAny
d_sequenceList_26 ~v0 v1 = du_sequenceList_26 v1
du_sequenceList_26 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> AgdaAny
du_sequenceList_26 v0 v1 v2
  = coe MAlonzo.Code.Interface.Monad.du_sequenceList_84 (coe v0) v1
-- Interface.Monad.Instance._.sequenceMaybe
d_sequenceMaybe_28 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Maybe AgdaAny -> AgdaAny
d_sequenceMaybe_28 ~v0 v1 = du_sequenceMaybe_28 v1
du_sequenceMaybe_28 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> Maybe AgdaAny -> AgdaAny
du_sequenceMaybe_28 v0 v1 v2
  = coe MAlonzo.Code.Interface.Monad.du_sequenceMaybe_94 (coe v0) v1
-- Interface.Monad.Instance._.traverseList
d_traverseList_30 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
d_traverseList_30 ~v0 v1 = du_traverseList_30 v1
du_traverseList_30 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
du_traverseList_30 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Interface.Monad.du_traverseList_70 (coe v0) v3 v5 v6
-- Interface.Monad.Instance._.traverseMaybe
d_traverseMaybe_32 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> Maybe AgdaAny -> AgdaAny
d_traverseMaybe_32 ~v0 v1 = du_traverseMaybe_32 v1
du_traverseMaybe_32 ::
  MAlonzo.Code.Interface.Monad.T_Monad_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> Maybe AgdaAny -> AgdaAny
du_traverseMaybe_32 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Interface.Monad.du_traverseMaybe_86 (coe v0) v3 v5 v6
-- Interface.Monad.Instance.Monad-List
d_Monad'45'List_34 :: MAlonzo.Code.Interface.Monad.T_Monad_20
d_Monad'45'List_34
  = coe
      MAlonzo.Code.Interface.Monad.C_Monad'46'constructor_273
      (coe
         (\ v0 v1 v2 ->
            coe
              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
              (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
      (coe
         (\ v0 v1 v2 v3 v4 v5 ->
            coe
              MAlonzo.Code.Data.List.Base.du_concatMap_272 (coe v5) (coe v4)))
-- Interface.Monad.Instance.Monad-Maybe
d_Monad'45'Maybe_38 :: MAlonzo.Code.Interface.Monad.T_Monad_20
d_Monad'45'Maybe_38
  = coe
      MAlonzo.Code.Interface.Monad.C_Monad'46'constructor_273
      (coe (\ v0 v1 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16))
      (coe
         (\ v0 v1 v2 v3 ->
            coe MAlonzo.Code.Data.Maybe.Base.du__'62''62''61'__76))
