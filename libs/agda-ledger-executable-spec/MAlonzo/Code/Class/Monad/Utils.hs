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

module MAlonzo.Code.Class.Monad.Utils where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Applicative.Core
import qualified MAlonzo.Code.Class.Functor.Core
import qualified MAlonzo.Code.Class.Monad.Core
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.List.Base

-- Class.Monad.Utils.mapM
d_mapM_22 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
d_mapM_22 ~v0 v1 ~v2 ~v3 v4 ~v5 v6 v7 = du_mapM_22 v1 v4 v6 v7
du_mapM_22 ::
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
du_mapM_22 v0 v1 v2 v3
  = case coe v3 of
      [] -> coe MAlonzo.Code.Class.Monad.Core.d_return_34 v0 v1 erased v3
      (:) v4 v5
        -> coe
             MAlonzo.Code.Class.Applicative.Core.d__'60''42''62'__34
             (MAlonzo.Code.Class.Monad.Core.d_super_32 (coe v0)) v1 erased v1
             erased
             (coe
                MAlonzo.Code.Class.Applicative.Core.d__'60''42''62'__34
                (MAlonzo.Code.Class.Monad.Core.d_super_32 (coe v0)) v1 erased v1
                erased
                (coe
                   MAlonzo.Code.Class.Applicative.Core.d_pure_32
                   (MAlonzo.Code.Class.Monad.Core.d_super_32 (coe v0)) v1 erased
                   (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22))
                (coe v2 v4))
             (coe du_mapM_22 (coe v0) (coe v1) (coe v2) (coe v5))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Utils.concatMapM
d_concatMapM_32 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
d_concatMapM_32 ~v0 v1 ~v2 ~v3 v4 ~v5 v6 v7
  = du_concatMapM_32 v1 v4 v6 v7
du_concatMapM_32 ::
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
du_concatMapM_32 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Class.Functor.Core.d__'60''36''62'__40
      (MAlonzo.Code.Class.Applicative.Core.d_super_30
         (coe MAlonzo.Code.Class.Monad.Core.d_super_32 (coe v0)))
      v1 erased v1 erased (coe MAlonzo.Code.Data.List.Base.du_concat_270)
      (coe du_mapM_22 (coe v0) (coe v1) (coe v2) (coe v3))
-- Class.Monad.Utils.forM
d_forM_38 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> (AgdaAny -> AgdaAny) -> AgdaAny
d_forM_38 ~v0 v1 ~v2 ~v3 v4 ~v5 v6 v7 = du_forM_38 v1 v4 v6 v7
du_forM_38 ::
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  [AgdaAny] -> (AgdaAny -> AgdaAny) -> AgdaAny
du_forM_38 v0 v1 v2 v3
  = case coe v2 of
      [] -> coe MAlonzo.Code.Class.Monad.Core.d_return_34 v0 v1 erased v2
      (:) v4 v5
        -> coe
             MAlonzo.Code.Class.Applicative.Core.d__'60''42''62'__34
             (MAlonzo.Code.Class.Monad.Core.d_super_32 (coe v0)) v1 erased v1
             erased
             (coe
                MAlonzo.Code.Class.Applicative.Core.d__'60''42''62'__34
                (MAlonzo.Code.Class.Monad.Core.d_super_32 (coe v0)) v1 erased v1
                erased
                (coe
                   MAlonzo.Code.Class.Applicative.Core.d_pure_32
                   (MAlonzo.Code.Class.Monad.Core.d_super_32 (coe v0)) v1 erased
                   (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22))
                (coe v3 v4))
             (coe du_forM_38 (coe v0) (coe v1) (coe v5) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Utils.concatForM
d_concatForM_46 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> (AgdaAny -> AgdaAny) -> AgdaAny
d_concatForM_46 ~v0 v1 ~v2 ~v3 v4 ~v5 v6 v7
  = du_concatForM_46 v1 v4 v6 v7
du_concatForM_46 ::
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  [AgdaAny] -> (AgdaAny -> AgdaAny) -> AgdaAny
du_concatForM_46 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Class.Functor.Core.d__'60''36''62'__40
      (MAlonzo.Code.Class.Applicative.Core.d_super_30
         (coe MAlonzo.Code.Class.Monad.Core.d_super_32 (coe v0)))
      v1 erased v1 erased (coe MAlonzo.Code.Data.List.Base.du_concat_270)
      (coe du_forM_38 (coe v0) (coe v1) (coe v2) (coe v3))
-- Class.Monad.Utils.return⊤
d_return'8868'_52 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_return'8868'_52 ~v0 v1 v2 ~v3 v4 = du_return'8868'_52 v1 v2 v4
du_return'8868'_52 ::
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> AgdaAny -> AgdaAny
du_return'8868'_52 v0 v1 v2
  = coe
      MAlonzo.Code.Class.Monad.Core.du__'8811'__54 v0 v1 erased () erased
      v2
      (coe
         MAlonzo.Code.Class.Monad.Core.d_return_34 v0 () erased
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
-- Class.Monad.Utils.void
d_void_54 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> AgdaAny -> AgdaAny
d_void_54 ~v0 v1 v2 ~v3 = du_void_54 v1 v2
du_void_54 ::
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> AgdaAny -> AgdaAny
du_void_54 v0 v1 = coe du_return'8868'_52 (coe v0) (coe v1)
-- Class.Monad.Utils.filterM
d_filterM_58 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
d_filterM_58 ~v0 v1 v2 ~v3 v4 v5 = du_filterM_58 v1 v2 v4 v5
du_filterM_58 ::
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
du_filterM_58 v0 v1 v2 v3
  = case coe v3 of
      [] -> coe MAlonzo.Code.Class.Monad.Core.d_return_34 v0 v1 erased v3
      (:) v4 v5
        -> coe
             MAlonzo.Code.Class.Monad.Core.d__'62''62''61'__36 v0 () erased v1
             erased (coe v2 v4)
             (\ v6 ->
                coe
                  MAlonzo.Code.Class.Functor.Core.d__'60''36''62'__40
                  (MAlonzo.Code.Class.Applicative.Core.d_super_30
                     (coe MAlonzo.Code.Class.Monad.Core.d_super_32 (coe v0)))
                  v1 erased v1 erased
                  (coe
                     MAlonzo.Code.Data.List.Base.du__'43''43'__62
                     (coe
                        MAlonzo.Code.Data.Bool.Base.du_if_then_else__42 (coe v6)
                        (coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 (coe v4))
                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                  (coe du_filterM_58 (coe v0) (coe v1) (coe v2) (coe v5)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Monad.Utils.do-pure
d_do'45'pure_78 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  AgdaAny ->
  Maybe AgdaAny ->
  (AgdaAny -> Bool) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_do'45'pure_78 = erased
