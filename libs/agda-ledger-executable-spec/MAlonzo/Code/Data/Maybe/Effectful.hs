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

module MAlonzo.Code.Data.Maybe.Effectful where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Effect.Applicative
import qualified MAlonzo.Code.Effect.Choice
import qualified MAlonzo.Code.Effect.Empty
import qualified MAlonzo.Code.Effect.Functor
import qualified MAlonzo.Code.Effect.Monad

-- Data.Maybe.Effectful.functor
d_functor_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Functor.T_RawFunctor_24
d_functor_22 ~v0 = du_functor_22
du_functor_22 :: MAlonzo.Code.Effect.Functor.T_RawFunctor_24
du_functor_22
  = coe
      MAlonzo.Code.Effect.Functor.C_RawFunctor'46'constructor_241
      (coe (\ v0 v1 v2 -> coe MAlonzo.Code.Data.Maybe.Base.du_map_68 v2))
-- Data.Maybe.Effectful.applicative
d_applicative_24 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
d_applicative_24 ~v0 = du_applicative_24
du_applicative_24 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
du_applicative_24
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawApplicative'46'constructor_453
      (coe du_functor_22)
      (coe (\ v0 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16))
      (coe
         (\ v0 v1 ->
            coe
              MAlonzo.Code.Data.Maybe.Base.du_maybe_36
              (coe MAlonzo.Code.Data.Maybe.Base.du_map_68)
              (let v2 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
               coe (\ v3 -> v2))))
-- Data.Maybe.Effectful.empty
d_empty_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Empty.T_RawEmpty_16
d_empty_26 ~v0 = du_empty_26
du_empty_26 :: MAlonzo.Code.Effect.Empty.T_RawEmpty_16
du_empty_26
  = coe
      MAlonzo.Code.Effect.Empty.C_RawEmpty'46'constructor_129
      (coe (\ v0 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18))
-- Data.Maybe.Effectful.choice
d_choice_28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Choice.T_RawChoice_16
d_choice_28 ~v0 = du_choice_28
du_choice_28 :: MAlonzo.Code.Effect.Choice.T_RawChoice_16
du_choice_28
  = coe
      MAlonzo.Code.Effect.Choice.C_RawChoice'46'constructor_149
      (\ v0 v1 v2 ->
         coe MAlonzo.Code.Data.Maybe.Base.du__'60''8739''62'__84 v1 v2)
-- Data.Maybe.Effectful.applicativeZero
d_applicativeZero_30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicativeZero_118
d_applicativeZero_30 ~v0 = du_applicativeZero_30
du_applicativeZero_30 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicativeZero_118
du_applicativeZero_30
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawApplicativeZero'46'constructor_7471
      (coe du_applicative_24) (coe du_empty_26)
-- Data.Maybe.Effectful.alternative
d_alternative_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Applicative.T_RawAlternative_176
d_alternative_32 ~v0 = du_alternative_32
du_alternative_32 ::
  MAlonzo.Code.Effect.Applicative.T_RawAlternative_176
du_alternative_32
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawAlternative'46'constructor_9281
      (coe du_applicativeZero_30) (coe du_choice_28)
-- Data.Maybe.Effectful.monad
d_monad_34 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24
d_monad_34 ~v0 = du_monad_34
du_monad_34 :: MAlonzo.Code.Effect.Monad.T_RawMonad_24
du_monad_34
  = coe
      MAlonzo.Code.Effect.Monad.C_RawMonad'46'constructor_319
      (coe du_applicative_24)
      (coe
         (\ v0 v1 v2 v3 ->
            coe MAlonzo.Code.Data.Maybe.Base.du__'62''62''61'__76 v2 v3))
-- Data.Maybe.Effectful.monadZero
d_monadZero_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Monad.T_RawMonadZero_140
d_monadZero_36 ~v0 = du_monadZero_36
du_monadZero_36 :: MAlonzo.Code.Effect.Monad.T_RawMonadZero_140
du_monadZero_36
  = coe
      MAlonzo.Code.Effect.Monad.C_RawMonadZero'46'constructor_6181
      (coe du_monad_34) (coe du_empty_26)
-- Data.Maybe.Effectful.monadPlus
d_monadPlus_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Monad.T_RawMonadPlus_216
d_monadPlus_38 ~v0 = du_monadPlus_38
du_monadPlus_38 :: MAlonzo.Code.Effect.Monad.T_RawMonadPlus_216
du_monadPlus_38
  = coe
      MAlonzo.Code.Effect.Monad.C_RawMonadPlus'46'constructor_8031
      (coe du_monadZero_36) (coe du_choice_28)
-- Data.Maybe.Effectful.TraversableA.sequenceA
d_sequenceA_84 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  () -> Maybe AgdaAny -> AgdaAny
d_sequenceA_84 ~v0 ~v1 ~v2 v3 ~v4 v5 = du_sequenceA_84 v3 v5
du_sequenceA_84 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  Maybe AgdaAny -> AgdaAny
du_sequenceA_84 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v2
        -> coe
             MAlonzo.Code.Effect.Functor.d__'60''36''62'__30
             (MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v0)) erased
             erased (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16) v2
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
        -> coe MAlonzo.Code.Effect.Applicative.d_pure_32 v0 erased v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Maybe.Effectful.TraversableA.mapA
d_mapA_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> Maybe AgdaAny -> AgdaAny
d_mapA_88 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7 v8 = du_mapA_88 v3 v7 v8
du_mapA_88 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  (AgdaAny -> AgdaAny) -> Maybe AgdaAny -> AgdaAny
du_mapA_88 v0 v1 v2
  = coe
      du_sequenceA_84 (coe v0)
      (coe MAlonzo.Code.Data.Maybe.Base.du_map_68 v1 v2)
-- Data.Maybe.Effectful.TraversableA.forA
d_forA_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> Maybe AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_forA_92 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7 v8 = du_forA_92 v3 v7 v8
du_forA_92 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  Maybe AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du_forA_92 v0 v1 v2 = coe du_mapA_88 (coe v0) (coe v2) (coe v1)
-- Data.Maybe.Effectful.TraversableM._.forA
d_forA_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> Maybe AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d_forA_156 ~v0 ~v1 ~v2 v3 = du_forA_156 v3
du_forA_156 ::
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> Maybe AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du_forA_156 v0 v1 v2 v3 v4 v5
  = coe
      du_forA_92
      (coe MAlonzo.Code.Effect.Monad.d_rawApplicative_32 (coe v0)) v4 v5
-- Data.Maybe.Effectful.TraversableM._.mapA
d_mapA_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> Maybe AgdaAny -> AgdaAny
d_mapA_158 ~v0 ~v1 ~v2 v3 = du_mapA_158 v3
du_mapA_158 ::
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> Maybe AgdaAny -> AgdaAny
du_mapA_158 v0 v1 v2 v3 v4 v5
  = coe
      du_mapA_88
      (coe MAlonzo.Code.Effect.Monad.d_rawApplicative_32 (coe v0)) v4 v5
-- Data.Maybe.Effectful.TraversableM._.sequenceA
d_sequenceA_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  () -> Maybe AgdaAny -> AgdaAny
d_sequenceA_160 ~v0 ~v1 ~v2 v3 = du_sequenceA_160 v3
du_sequenceA_160 ::
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  () -> Maybe AgdaAny -> AgdaAny
du_sequenceA_160 v0 v1 v2
  = coe
      du_sequenceA_84
      (coe MAlonzo.Code.Effect.Monad.d_rawApplicative_32 (coe v0)) v2
