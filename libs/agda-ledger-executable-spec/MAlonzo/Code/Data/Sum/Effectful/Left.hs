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

module MAlonzo.Code.Data.Sum.Effectful.Left where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Effect.Applicative
import qualified MAlonzo.Code.Effect.Choice
import qualified MAlonzo.Code.Effect.Empty
import qualified MAlonzo.Code.Effect.Functor
import qualified MAlonzo.Code.Effect.Monad
import qualified MAlonzo.Code.Function.Base

-- Data.Sum.Effectful.Left.Sumₗ
d_Sum'8343'_14 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()
d_Sum'8343'_14 = erased
-- Data.Sum.Effectful.Left.functor
d_functor_18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Functor.T_RawFunctor_24
d_functor_18 ~v0 ~v1 ~v2 = du_functor_18
du_functor_18 :: MAlonzo.Code.Effect.Functor.T_RawFunctor_24
du_functor_18
  = coe
      MAlonzo.Code.Effect.Functor.C_RawFunctor'46'constructor_241
      (coe (\ v0 v1 -> coe MAlonzo.Code.Data.Sum.Base.du_map'8322'_94))
-- Data.Sum.Effectful.Left.applicative
d_applicative_20 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
d_applicative_20 ~v0 ~v1 ~v2 = du_applicative_20
du_applicative_20 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
du_applicative_20
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawApplicative'46'constructor_453
      (coe du_functor_18)
      (coe (\ v0 -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42))
      (coe
         (\ v0 v1 ->
            coe
              MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
              (\ v2 ->
                 let v3 = coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v2) in
                 \ v4 -> v3)
              (coe MAlonzo.Code.Data.Sum.Base.du_map'8322'_94)))
-- Data.Sum.Effectful.Left.empty
d_empty_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> MAlonzo.Code.Effect.Empty.T_RawEmpty_16
d_empty_22 ~v0 ~v1 ~v2 v3 = du_empty_22 v3
du_empty_22 :: AgdaAny -> MAlonzo.Code.Effect.Empty.T_RawEmpty_16
du_empty_22 v0
  = coe
      MAlonzo.Code.Effect.Empty.C_RawEmpty'46'constructor_129
      (coe
         (\ v1 -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v0)))
-- Data.Sum.Effectful.Left.choice
d_choice_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Choice.T_RawChoice_16
d_choice_26 ~v0 ~v1 ~v2 = du_choice_26
du_choice_26 :: MAlonzo.Code.Effect.Choice.T_RawChoice_16
du_choice_26
  = coe
      MAlonzo.Code.Effect.Choice.C_RawChoice'46'constructor_149
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
              (\ v1 v2 -> v2)
              (\ v1 ->
                 let v2 = coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 (coe v1) in
                 \ v3 -> v2)))
-- Data.Sum.Effectful.Left.applicativeZero
d_applicativeZero_28 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> MAlonzo.Code.Effect.Applicative.T_RawApplicativeZero_118
d_applicativeZero_28 ~v0 ~v1 ~v2 v3 = du_applicativeZero_28 v3
du_applicativeZero_28 ::
  AgdaAny -> MAlonzo.Code.Effect.Applicative.T_RawApplicativeZero_118
du_applicativeZero_28 v0
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawApplicativeZero'46'constructor_7471
      (coe du_applicative_20) (coe du_empty_22 (coe v0))
-- Data.Sum.Effectful.Left.alternative
d_alternative_32 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> MAlonzo.Code.Effect.Applicative.T_RawAlternative_176
d_alternative_32 ~v0 ~v1 ~v2 v3 = du_alternative_32 v3
du_alternative_32 ::
  AgdaAny -> MAlonzo.Code.Effect.Applicative.T_RawAlternative_176
du_alternative_32 v0
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawAlternative'46'constructor_9281
      (coe du_applicativeZero_28 (coe v0)) (coe du_choice_26)
-- Data.Sum.Effectful.Left.monad
d_monad_36 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24
d_monad_36 ~v0 ~v1 ~v2 = du_monad_36
du_monad_36 :: MAlonzo.Code.Effect.Monad.T_RawMonad_24
du_monad_36
  = coe
      MAlonzo.Code.Effect.Monad.C_RawMonad'46'constructor_319
      (coe du_applicative_20)
      (coe
         (\ v0 v1 ->
            coe
              MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93''8242'_66
              (coe
                 MAlonzo.Code.Function.Base.du__'8728''8242'__216
                 (coe (\ v2 v3 -> v2))
                 (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38))
              (coe MAlonzo.Code.Function.Base.du__'124''62''8242'__232)))
-- Data.Sum.Effectful.Left.TraversableA.sequenceA
d_sequenceA_78 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  () -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_sequenceA_78 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 = du_sequenceA_78 v4 v6
du_sequenceA_78 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_sequenceA_78 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v2
        -> coe MAlonzo.Code.Effect.Applicative.d_pure_32 v0 erased v1
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v2
        -> coe
             MAlonzo.Code.Effect.Functor.d__'60''36''62'__30
             (MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v0)) erased
             erased (coe MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42) v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Sum.Effectful.Left.TraversableA.mapA
d_mapA_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_mapA_88 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 v7 v8 = du_mapA_88 v4 v7 v8
du_mapA_88 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_mapA_88 v0 v1 v2
  = coe
      du_sequenceA_78 (coe v0)
      (coe MAlonzo.Code.Data.Sum.Base.du_map'8322'_94 v1 v2)
-- Data.Sum.Effectful.Left.TraversableA.forA
d_forA_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  () ->
  () ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  (AgdaAny -> AgdaAny) -> AgdaAny
d_forA_96 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 v7 v8 = du_forA_96 v4 v7 v8
du_forA_96 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  (AgdaAny -> AgdaAny) -> AgdaAny
du_forA_96 v0 v1 v2 = coe du_mapA_88 (coe v0) (coe v2) (coe v1)
-- Data.Sum.Effectful.Left.TraversableM._.forA
d_forA_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  () ->
  () ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  (AgdaAny -> AgdaAny) -> AgdaAny
d_forA_156 ~v0 ~v1 ~v2 ~v3 v4 = du_forA_156 v4
du_forA_156 ::
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  () ->
  () ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  (AgdaAny -> AgdaAny) -> AgdaAny
du_forA_156 v0 v1 v2 v3 v4
  = coe
      du_forA_96
      (coe MAlonzo.Code.Effect.Monad.d_rawApplicative_32 (coe v0)) v3 v4
-- Data.Sum.Effectful.Left.TraversableM._.mapA
d_mapA_158 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_mapA_158 ~v0 ~v1 ~v2 ~v3 v4 = du_mapA_158 v4
du_mapA_158 ::
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_mapA_158 v0 v1 v2 v3 v4
  = coe
      du_mapA_88
      (coe MAlonzo.Code.Effect.Monad.d_rawApplicative_32 (coe v0)) v3 v4
-- Data.Sum.Effectful.Left.TraversableM._.sequenceA
d_sequenceA_160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  () -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_sequenceA_160 ~v0 ~v1 ~v2 ~v3 v4 = du_sequenceA_160 v4
du_sequenceA_160 ::
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  () -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_sequenceA_160 v0 v1 v2
  = coe
      du_sequenceA_78
      (coe MAlonzo.Code.Effect.Monad.d_rawApplicative_32 (coe v0)) v2
