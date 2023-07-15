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

module MAlonzo.Code.Class.Traversable.Instances where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Applicative.Core
import qualified MAlonzo.Code.Class.Monad.Core
import qualified MAlonzo.Code.Class.Traversable.Core
import qualified MAlonzo.Code.Data.List.NonEmpty.Base

-- Class.Traversable.Instances.TraversableA-Maybe
d_TraversableA'45'Maybe_10 ::
  MAlonzo.Code.Class.Traversable.Core.T_TraversableA_22
d_TraversableA'45'Maybe_10
  = coe
      MAlonzo.Code.Class.Traversable.Core.C_TraversableA'46'constructor_359
      (coe
         (\ v0 v1 v2 v3 v4 ->
            case coe v4 of
              MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v5
                -> coe
                     MAlonzo.Code.Class.Applicative.Core.d__'60''42''62'__34 v3 v1
                     erased v1 erased
                     (coe
                        MAlonzo.Code.Class.Applicative.Core.d_pure_32 v3 v1 erased
                        (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16))
                     v5
              MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                -> coe
                     MAlonzo.Code.Class.Applicative.Core.d_pure_32 v3 v1 erased v4
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Class.Traversable.Instances.TraversableM-Maybe
d_TraversableM'45'Maybe_14 ::
  MAlonzo.Code.Class.Traversable.Core.T_TraversableM_50
d_TraversableM'45'Maybe_14
  = coe
      MAlonzo.Code.Class.Traversable.Core.C_TraversableM'46'constructor_3383
      (coe
         (\ v0 v1 v2 v3 v4 ->
            case coe v4 of
              MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v5
                -> coe
                     MAlonzo.Code.Class.Monad.Core.d__'62''62''61'__36 v3 v1 erased v1
                     erased v5
                     (\ v6 ->
                        coe
                          MAlonzo.Code.Class.Monad.Core.d_return_34 v3 v1 erased
                          (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v6)))
              MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                -> coe MAlonzo.Code.Class.Monad.Core.d_return_34 v3 v1 erased v4
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Class.Traversable.Instances.TraversableA-List
d_TraversableA'45'List_18 ::
  MAlonzo.Code.Class.Traversable.Core.T_TraversableA_22
d_TraversableA'45'List_18
  = coe
      MAlonzo.Code.Class.Traversable.Core.C_TraversableA'46'constructor_359
      (coe (\ v0 v1 v2 v3 -> coe du_go_24 (coe v1) (coe v3)))
-- Class.Traversable.Instances._.go
d_go_24 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
  [AgdaAny] -> AgdaAny
d_go_24 ~v0 v1 ~v2 v3 = du_go_24 v1 v3
du_go_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
  [AgdaAny] -> AgdaAny
du_go_24 v0 v1 = coe du_'46'extendedlambda0_26 (coe v0) (coe v1)
-- Class.Traversable.Instances._..extendedlambda0
d_'46'extendedlambda0_26 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
  [AgdaAny] -> AgdaAny
d_'46'extendedlambda0_26 ~v0 v1 ~v2 v3 v4
  = du_'46'extendedlambda0_26 v1 v3 v4
du_'46'extendedlambda0_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
  [AgdaAny] -> AgdaAny
du_'46'extendedlambda0_26 v0 v1 v2
  = case coe v2 of
      []
        -> coe
             MAlonzo.Code.Class.Applicative.Core.d_pure_32 v1 v0 erased v2
      (:) v3 v4
        -> coe
             MAlonzo.Code.Class.Applicative.Core.d__'60''42''62'__34 v1 v0
             erased v0 erased
             (coe
                MAlonzo.Code.Class.Applicative.Core.d__'60''42''62'__34 v1 v0
                erased v0 erased
                (coe
                   MAlonzo.Code.Class.Applicative.Core.d_pure_32 v1 v0 erased
                   (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22))
                v3)
             (coe du_go_24 v0 v1 v4)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Traversable.Instances.TraversableM-List
d_TraversableM'45'List_32 ::
  MAlonzo.Code.Class.Traversable.Core.T_TraversableM_50
d_TraversableM'45'List_32
  = coe
      MAlonzo.Code.Class.Traversable.Core.C_TraversableM'46'constructor_3383
      (coe (\ v0 v1 v2 v3 -> coe du_go_38 (coe v1) (coe v3)))
-- Class.Traversable.Instances._.go
d_go_38 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 -> [AgdaAny] -> AgdaAny
d_go_38 ~v0 v1 ~v2 v3 = du_go_38 v1 v3
du_go_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 -> [AgdaAny] -> AgdaAny
du_go_38 v0 v1 = coe du_'46'extendedlambda0_40 (coe v0) (coe v1)
-- Class.Traversable.Instances._..extendedlambda0
d_'46'extendedlambda0_40 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 -> [AgdaAny] -> AgdaAny
d_'46'extendedlambda0_40 ~v0 v1 ~v2 v3 v4
  = du_'46'extendedlambda0_40 v1 v3 v4
du_'46'extendedlambda0_40 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 -> [AgdaAny] -> AgdaAny
du_'46'extendedlambda0_40 v0 v1 v2
  = case coe v2 of
      [] -> coe MAlonzo.Code.Class.Monad.Core.d_return_34 v1 v0 erased v2
      (:) v3 v4
        -> coe
             MAlonzo.Code.Class.Monad.Core.d__'62''62''61'__36 v1 v0 erased v0
             erased v3
             (\ v5 ->
                coe
                  MAlonzo.Code.Class.Monad.Core.d__'62''62''61'__36 v1 v0 erased v0
                  erased (coe du_go_38 v0 v1 v4)
                  (\ v6 ->
                     coe
                       MAlonzo.Code.Class.Monad.Core.d_return_34 v1 v0 erased
                       (coe
                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v5) (coe v6))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Traversable.Instances.TraversableA-List⁺
d_TraversableA'45'List'8314'_50 ::
  MAlonzo.Code.Class.Traversable.Core.T_TraversableA_22
d_TraversableA'45'List'8314'_50
  = coe
      MAlonzo.Code.Class.Traversable.Core.C_TraversableA'46'constructor_359
      (coe
         (\ v0 v1 v2 v3 v4 ->
            case coe v4 of
              MAlonzo.Code.Data.List.NonEmpty.Base.C__'8759'__34 v5 v6
                -> coe
                     MAlonzo.Code.Class.Applicative.Core.d__'60''42''62'__34 v3 v1
                     erased v1 erased
                     (coe
                        MAlonzo.Code.Class.Applicative.Core.d__'60''42''62'__34 v3 v1
                        erased v1 erased
                        (coe
                           MAlonzo.Code.Class.Applicative.Core.d_pure_32 v3 v1 erased
                           (coe MAlonzo.Code.Data.List.NonEmpty.Base.C__'8759'__34))
                        v5)
                     (coe du_go_24 v1 v3 v6)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Class.Traversable.Instances.TraversableM-List⁺
d_TraversableM'45'List'8314'_56 ::
  MAlonzo.Code.Class.Traversable.Core.T_TraversableM_50
d_TraversableM'45'List'8314'_56
  = coe
      MAlonzo.Code.Class.Traversable.Core.C_TraversableM'46'constructor_3383
      (coe
         (\ v0 v1 v2 v3 v4 ->
            case coe v4 of
              MAlonzo.Code.Data.List.NonEmpty.Base.C__'8759'__34 v5 v6
                -> coe
                     MAlonzo.Code.Class.Monad.Core.d__'62''62''61'__36 v3 v1 erased v1
                     erased v5
                     (\ v7 ->
                        coe
                          MAlonzo.Code.Class.Monad.Core.d__'62''62''61'__36 v3 v1 erased v1
                          erased (coe du_go_38 v1 v3 v6)
                          (\ v8 ->
                             coe
                               MAlonzo.Code.Class.Monad.Core.d_return_34 v3 v1 erased
                               (coe
                                  MAlonzo.Code.Data.List.NonEmpty.Base.C__'8759'__34 (coe v7)
                                  (coe v8))))
              _ -> MAlonzo.RTE.mazUnreachableError))
