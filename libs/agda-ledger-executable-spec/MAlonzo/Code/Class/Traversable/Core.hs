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

module MAlonzo.Code.Class.Traversable.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Class.Applicative.Core
import qualified MAlonzo.Code.Class.Foldable.Core
import qualified MAlonzo.Code.Class.Functor.Core
import qualified MAlonzo.Code.Class.Monad.Core

-- Class.Traversable.Core.TraversableA
d_TraversableA_22 a0 a1 a2 = ()
newtype T_TraversableA_22
  = C_TraversableA'46'constructor_359 ((MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                        () -> ()) ->
                                       MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                       () ->
                                       MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
                                       AgdaAny -> AgdaAny)
-- Class.Traversable.Core.TraversableA.sequenceA
d_sequenceA_32 ::
  T_TraversableA_22 ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
  AgdaAny -> AgdaAny
d_sequenceA_32 v0
  = case coe v0 of
      C_TraversableA'46'constructor_359 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Traversable.Core.TraversableA.traverseA
d_traverseA_34 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Functor.Core.T_Functor_34 ->
  MAlonzo.Code.Class.Foldable.Core.T_Foldable_12 ->
  T_TraversableA_22 ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_traverseA_34 ~v0 v1 ~v2 v3 ~v4 v5 ~v6 v7 ~v8 v9 v10 v11
  = du_traverseA_34 v1 v3 v5 v7 v9 v10 v11
du_traverseA_34 ::
  MAlonzo.Code.Class.Functor.Core.T_Functor_34 ->
  T_TraversableA_22 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_traverseA_34 v0 v1 v2 v3 v4 v5 v6
  = coe
      d_sequenceA_32 v1 erased v3 erased v4
      (coe
         MAlonzo.Code.Class.Functor.Core.du_fmap_42 v0 v2 erased v3 erased
         v5 v6)
-- Class.Traversable.Core._.sequenceA
d_sequenceA_40 ::
  T_TraversableA_22 ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
  AgdaAny -> AgdaAny
d_sequenceA_40 v0 = coe d_sequenceA_32 (coe v0)
-- Class.Traversable.Core._.traverseA
d_traverseA_42 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Functor.Core.T_Functor_34 ->
  MAlonzo.Code.Class.Foldable.Core.T_Foldable_12 ->
  T_TraversableA_22 ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_traverseA_42 ~v0 v1 ~v2 v3 = du_traverseA_42 v1 v3
du_traverseA_42 ::
  MAlonzo.Code.Class.Functor.Core.T_Functor_34 ->
  T_TraversableA_22 ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Applicative.Core.T_Applicative_20 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_traverseA_42 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe du_traverseA_34 (coe v0) (coe v1) v3 v5 v7 v8 v9
-- Class.Traversable.Core.TraversableM
d_TraversableM_50 a0 a1 a2 = ()
newtype T_TraversableM_50
  = C_TraversableM'46'constructor_3383 ((MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                         () -> ()) ->
                                        MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                        () ->
                                        MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
                                        AgdaAny -> AgdaAny)
-- Class.Traversable.Core.TraversableM.sequenceM
d_sequenceM_60 ::
  T_TraversableM_50 ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 -> AgdaAny -> AgdaAny
d_sequenceM_60 v0
  = case coe v0 of
      C_TraversableM'46'constructor_3383 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Traversable.Core.TraversableM.traverseM
d_traverseM_62 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Functor.Core.T_Functor_34 ->
  MAlonzo.Code.Class.Foldable.Core.T_Foldable_12 ->
  T_TraversableM_50 ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_traverseM_62 ~v0 v1 ~v2 v3 ~v4 v5 ~v6 v7 ~v8 v9 v10 v11
  = du_traverseM_62 v1 v3 v5 v7 v9 v10 v11
du_traverseM_62 ::
  MAlonzo.Code.Class.Functor.Core.T_Functor_34 ->
  T_TraversableM_50 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_traverseM_62 v0 v1 v2 v3 v4 v5 v6
  = coe
      d_sequenceM_60 v1 erased v3 erased v4
      (coe
         MAlonzo.Code.Class.Functor.Core.du_fmap_42 v0 v2 erased v3 erased
         v5 v6)
-- Class.Traversable.Core._.sequenceM
d_sequenceM_68 ::
  T_TraversableM_50 ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 -> AgdaAny -> AgdaAny
d_sequenceM_68 v0 = coe d_sequenceM_60 (coe v0)
-- Class.Traversable.Core._.traverseM
d_traverseM_70 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Class.Functor.Core.T_Functor_34 ->
  MAlonzo.Code.Class.Foldable.Core.T_Foldable_12 ->
  T_TraversableM_50 ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_traverseM_70 ~v0 v1 ~v2 v3 = du_traverseM_70 v1 v3
du_traverseM_70 ::
  MAlonzo.Code.Class.Functor.Core.T_Functor_34 ->
  T_TraversableM_50 ->
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Class.Monad.Core.T_Monad_22 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_traverseM_70 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe du_traverseM_62 (coe v0) (coe v1) v3 v5 v7 v8 v9
