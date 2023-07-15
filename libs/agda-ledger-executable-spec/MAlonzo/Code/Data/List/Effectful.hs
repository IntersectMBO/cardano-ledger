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

module MAlonzo.Code.Data.List.Effectful where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Effect.Applicative
import qualified MAlonzo.Code.Effect.Choice
import qualified MAlonzo.Code.Effect.Empty
import qualified MAlonzo.Code.Effect.Functor
import qualified MAlonzo.Code.Effect.Monad

-- Data.List.Effectful.functor
d_functor_8 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Functor.T_RawFunctor_24
d_functor_8 ~v0 = du_functor_8
du_functor_8 :: MAlonzo.Code.Effect.Functor.T_RawFunctor_24
du_functor_8
  = coe
      MAlonzo.Code.Effect.Functor.C_RawFunctor'46'constructor_241
      (coe
         (\ v0 v1 v2 v3 -> coe MAlonzo.Code.Data.List.Base.du_map_22 v2 v3))
-- Data.List.Effectful.applicative
d_applicative_10 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
d_applicative_10 ~v0 = du_applicative_10
du_applicative_10 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20
du_applicative_10
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawApplicative'46'constructor_453
      (coe du_functor_8)
      (\ v0 v1 -> coe MAlonzo.Code.Data.List.Base.du_'91'_'93'_306 v1)
      (coe
         (\ v0 v1 v2 v3 -> coe MAlonzo.Code.Data.List.Base.du_ap_276 v2 v3))
-- Data.List.Effectful.empty
d_empty_12 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Empty.T_RawEmpty_16
d_empty_12 ~v0 = du_empty_12
du_empty_12 :: MAlonzo.Code.Effect.Empty.T_RawEmpty_16
du_empty_12
  = coe
      MAlonzo.Code.Effect.Empty.C_RawEmpty'46'constructor_129
      (coe (\ v0 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
-- Data.List.Effectful.choice
d_choice_14 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Choice.T_RawChoice_16
d_choice_14 ~v0 = du_choice_14
du_choice_14 :: MAlonzo.Code.Effect.Choice.T_RawChoice_16
du_choice_14
  = coe
      MAlonzo.Code.Effect.Choice.C_RawChoice'46'constructor_149
      (\ v0 v1 v2 ->
         coe MAlonzo.Code.Data.List.Base.du__'43''43'__62 v1 v2)
-- Data.List.Effectful.applicativeZero
d_applicativeZero_16 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicativeZero_118
d_applicativeZero_16 ~v0 = du_applicativeZero_16
du_applicativeZero_16 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicativeZero_118
du_applicativeZero_16
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawApplicativeZero'46'constructor_7471
      (coe du_applicative_10) (coe du_empty_12)
-- Data.List.Effectful.alternative
d_alternative_18 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Applicative.T_RawAlternative_176
d_alternative_18 ~v0 = du_alternative_18
du_alternative_18 ::
  MAlonzo.Code.Effect.Applicative.T_RawAlternative_176
du_alternative_18
  = coe
      MAlonzo.Code.Effect.Applicative.C_RawAlternative'46'constructor_9281
      (coe du_applicativeZero_16) (coe du_choice_14)
-- Data.List.Effectful.monad
d_monad_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24
d_monad_22 ~v0 = du_monad_22
du_monad_22 :: MAlonzo.Code.Effect.Monad.T_RawMonad_24
du_monad_22
  = coe
      MAlonzo.Code.Effect.Monad.C_RawMonad'46'constructor_319
      (coe du_applicative_10)
      (coe
         (\ v0 v1 v2 v3 ->
            coe
              MAlonzo.Code.Data.List.Base.du_concatMap_272 (coe v3) (coe v2)))
-- Data.List.Effectful.monadZero
d_monadZero_26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Monad.T_RawMonadZero_140
d_monadZero_26 ~v0 = du_monadZero_26
du_monadZero_26 :: MAlonzo.Code.Effect.Monad.T_RawMonadZero_140
du_monadZero_26
  = coe
      MAlonzo.Code.Effect.Monad.C_RawMonadZero'46'constructor_6181
      (coe du_monad_22) (coe du_empty_12)
-- Data.List.Effectful.monadPlus
d_monadPlus_30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Effect.Monad.T_RawMonadPlus_216
d_monadPlus_30 ~v0 = du_monadPlus_30
du_monadPlus_30 :: MAlonzo.Code.Effect.Monad.T_RawMonadPlus_216
du_monadPlus_30
  = coe
      MAlonzo.Code.Effect.Monad.C_RawMonadPlus'46'constructor_8031
      (coe du_monadZero_26) (coe du_choice_14)
-- Data.List.Effectful.TraversableA.sequenceA
d_sequenceA_76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  () -> [AgdaAny] -> AgdaAny
d_sequenceA_76 ~v0 ~v1 ~v2 v3 ~v4 v5 = du_sequenceA_76 v3 v5
du_sequenceA_76 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  [AgdaAny] -> AgdaAny
du_sequenceA_76 v0 v1
  = case coe v1 of
      [] -> coe MAlonzo.Code.Effect.Applicative.d_pure_32 v0 erased v1
      (:) v2 v3
        -> coe
             MAlonzo.Code.Effect.Applicative.d__'60''42''62'__34 v0 erased
             erased
             (coe
                MAlonzo.Code.Effect.Functor.d__'60''36''62'__30
                (MAlonzo.Code.Effect.Applicative.d_rawFunctor_30 (coe v0)) erased
                erased (coe MAlonzo.Code.Agda.Builtin.List.C__'8759'__22) v2)
             (coe du_sequenceA_76 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.List.Effectful.TraversableA.mapA
d_mapA_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
d_mapA_88 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7 v8 = du_mapA_88 v3 v7 v8
du_mapA_88 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
du_mapA_88 v0 v1 v2
  = coe
      du_sequenceA_76 (coe v0)
      (coe MAlonzo.Code.Data.List.Base.du_map_22 (coe v1) (coe v2))
-- Data.List.Effectful.TraversableA.forA
d_forA_98 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> [AgdaAny] -> (AgdaAny -> AgdaAny) -> AgdaAny
d_forA_98 ~v0 ~v1 ~v2 v3 ~v4 ~v5 ~v6 v7 v8 = du_forA_98 v3 v7 v8
du_forA_98 ::
  MAlonzo.Code.Effect.Applicative.T_RawApplicative_20 ->
  [AgdaAny] -> (AgdaAny -> AgdaAny) -> AgdaAny
du_forA_98 v0 v1 v2 = coe du_mapA_88 (coe v0) (coe v2) (coe v1)
-- Data.List.Effectful.TraversableM._.forA
d_forA_162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> [AgdaAny] -> (AgdaAny -> AgdaAny) -> AgdaAny
d_forA_162 ~v0 ~v1 ~v2 v3 = du_forA_162 v3
du_forA_162 ::
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> [AgdaAny] -> (AgdaAny -> AgdaAny) -> AgdaAny
du_forA_162 v0 v1 v2 v3 v4 v5
  = coe
      du_forA_98
      (coe MAlonzo.Code.Effect.Monad.d_rawApplicative_32 (coe v0)) v4 v5
-- Data.List.Effectful.TraversableM._.mapA
d_mapA_164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
d_mapA_164 ~v0 ~v1 ~v2 v3 = du_mapA_164 v3
du_mapA_164 ::
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> AgdaAny
du_mapA_164 v0 v1 v2 v3 v4 v5
  = coe
      du_mapA_88
      (coe MAlonzo.Code.Effect.Monad.d_rawApplicative_32 (coe v0)) v4 v5
-- Data.List.Effectful.TraversableM._.sequenceA
d_sequenceA_166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (() -> ()) ->
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  () -> [AgdaAny] -> AgdaAny
d_sequenceA_166 ~v0 ~v1 ~v2 v3 = du_sequenceA_166 v3
du_sequenceA_166 ::
  MAlonzo.Code.Effect.Monad.T_RawMonad_24 ->
  () -> [AgdaAny] -> AgdaAny
du_sequenceA_166 v0 v1 v2
  = coe
      du_sequenceA_76
      (coe MAlonzo.Code.Effect.Monad.d_rawApplicative_32 (coe v0)) v2
-- Data.List.Effectful.LMP._<$>_
d__'60''36''62'__176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> [AgdaAny]
d__'60''36''62'__176 ~v0 = du__'60''36''62'__176
du__'60''36''62'__176 ::
  () -> () -> (AgdaAny -> AgdaAny) -> [AgdaAny] -> [AgdaAny]
du__'60''36''62'__176
  = coe
      MAlonzo.Code.Effect.Functor.d__'60''36''62'__30
      (coe
         MAlonzo.Code.Effect.Applicative.d_rawFunctor_30
         (coe
            MAlonzo.Code.Effect.Monad.d_rawApplicative_32
            (coe
               MAlonzo.Code.Effect.Monad.d_rawMonad_148
               (coe
                  MAlonzo.Code.Effect.Monad.d_rawMonadZero_224
                  (coe du_monadPlus_30)))))
-- Data.List.Effectful.LMP._>>=_
d__'62''62''61'__196 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> [AgdaAny] -> (AgdaAny -> [AgdaAny]) -> [AgdaAny]
d__'62''62''61'__196 ~v0 = du__'62''62''61'__196
du__'62''62''61'__196 ::
  () -> () -> [AgdaAny] -> (AgdaAny -> [AgdaAny]) -> [AgdaAny]
du__'62''62''61'__196
  = coe
      MAlonzo.Code.Effect.Monad.d__'62''62''61'__34
      (coe
         MAlonzo.Code.Effect.Monad.d_rawMonad_148
         (coe
            MAlonzo.Code.Effect.Monad.d_rawMonadZero_224
            (coe du_monadPlus_30)))
-- Data.List.Effectful.LMP._∣_
d__'8739'__198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> [AgdaAny] -> [AgdaAny]
d__'8739'__198 ~v0 = du__'8739'__198
du__'8739'__198 :: () -> [AgdaAny] -> [AgdaAny] -> [AgdaAny]
du__'8739'__198
  = let v0 = coe du_monadPlus_30 in
    \ v1 ->
      coe
        MAlonzo.Code.Effect.Choice.du__'8739'__24
        (coe MAlonzo.Code.Effect.Monad.d_rawChoice_226 (coe v0))
-- Data.List.Effectful.LMP._⊛_
d__'8859'__202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> [AgdaAny -> AgdaAny] -> [AgdaAny] -> [AgdaAny]
d__'8859'__202 ~v0 = du__'8859'__202
du__'8859'__202 ::
  () -> () -> [AgdaAny -> AgdaAny] -> [AgdaAny] -> [AgdaAny]
du__'8859'__202
  = let v0 = coe du_monadPlus_30 in
    let v1 = MAlonzo.Code.Effect.Monad.d_rawMonadZero_224 (coe v0) in
    let v2 = MAlonzo.Code.Effect.Monad.d_rawMonad_148 (coe v1) in
    \ v3 v4 ->
      coe
        MAlonzo.Code.Effect.Applicative.du__'8859'__68
        (coe MAlonzo.Code.Effect.Monad.d_rawApplicative_32 (coe v2))
-- Data.List.Effectful.LMP.pure
d_pure_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> [AgdaAny]
d_pure_212 ~v0 = du_pure_212
du_pure_212 :: () -> AgdaAny -> [AgdaAny]
du_pure_212
  = coe
      MAlonzo.Code.Effect.Applicative.d_pure_32
      (coe
         MAlonzo.Code.Effect.Monad.d_rawApplicative_32
         (coe
            MAlonzo.Code.Effect.Monad.d_rawMonad_148
            (coe
               MAlonzo.Code.Effect.Monad.d_rawMonadZero_224
               (coe du_monadPlus_30))))
-- Data.List.Effectful.LMP.∅
d_'8709'_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> [AgdaAny]
d_'8709'_238 ~v0 = du_'8709'_238
du_'8709'_238 :: () -> [AgdaAny]
du_'8709'_238
  = let v0 = coe du_monadPlus_30 in
    let v1 = MAlonzo.Code.Effect.Monad.d_rawMonadZero_224 (coe v0) in
    \ v2 ->
      coe
        MAlonzo.Code.Effect.Empty.du_'8709'_24
        (coe MAlonzo.Code.Effect.Monad.d_rawEmpty_150 (coe v1))
-- Data.List.Effectful.MonadProperties.left-identity
d_left'45'identity_252 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'identity_252 = erased
-- Data.List.Effectful.MonadProperties.right-identity
d_right'45'identity_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'identity_264 = erased
-- Data.List.Effectful.MonadProperties.left-zero
d_left'45'zero_280 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'zero_280 = erased
-- Data.List.Effectful.MonadProperties.right-zero
d_right'45'zero_292 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'zero_292 = erased
-- Data.List.Effectful.MonadProperties.right-distributive
d_right'45'distributive_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'distributive_320 = erased
-- Data.List.Effectful.MonadProperties.associative
d_associative_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  [AgdaAny] ->
  (AgdaAny -> [AgdaAny]) ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_associative_352 = erased
-- Data.List.Effectful.MonadProperties.cong
d_cong_384 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  (AgdaAny -> [AgdaAny]) ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cong_384 = erased
-- Data.List.Effectful.Applicative.MP.associative
d_associative_394 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  [AgdaAny] ->
  (AgdaAny -> [AgdaAny]) ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_associative_394 = erased
-- Data.List.Effectful.Applicative.MP.cong
d_cong_396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  (AgdaAny -> [AgdaAny]) ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  (AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cong_396 = erased
-- Data.List.Effectful.Applicative.MP.left-identity
d_left'45'identity_398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  AgdaAny ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'identity_398 = erased
-- Data.List.Effectful.Applicative.MP.left-zero
d_left'45'zero_400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'zero_400 = erased
-- Data.List.Effectful.Applicative.MP.right-distributive
d_right'45'distributive_402 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  [AgdaAny] ->
  [AgdaAny] ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'distributive_402 = erased
-- Data.List.Effectful.Applicative.MP.right-identity
d_right'45'identity_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'identity_404 = erased
-- Data.List.Effectful.Applicative.MP.right-zero
d_right'45'zero_406 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'zero_406 = erased
-- Data.List.Effectful.Applicative.pam
d_pam_414 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> () -> [AgdaAny] -> (AgdaAny -> AgdaAny) -> [AgdaAny]
d_pam_414 ~v0 ~v1 ~v2 v3 v4 = du_pam_414 v3 v4
du_pam_414 :: [AgdaAny] -> (AgdaAny -> AgdaAny) -> [AgdaAny]
du_pam_414 v0 v1
  = coe
      MAlonzo.Code.Effect.Monad.d__'62''62''61'__34
      (MAlonzo.Code.Effect.Monad.d_rawMonad_148
         (coe
            MAlonzo.Code.Effect.Monad.d_rawMonadZero_224
            (coe du_monadPlus_30)))
      erased erased v0
      (\ v2 ->
         coe
           MAlonzo.Code.Effect.Applicative.d_pure_32
           (MAlonzo.Code.Effect.Monad.d_rawApplicative_32
              (coe
                 MAlonzo.Code.Effect.Monad.d_rawMonad_148
                 (coe
                    MAlonzo.Code.Effect.Monad.d_rawMonadZero_224
                    (coe du_monadPlus_30))))
           erased (coe v1 v2))
-- Data.List.Effectful.Applicative.left-zero
d_left'45'zero_428 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_left'45'zero_428 = erased
-- Data.List.Effectful.Applicative.right-zero
d_right'45'zero_440 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  [AgdaAny -> AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'zero_440 = erased
-- Data.List.Effectful.Applicative.unfold-<$>
d_unfold'45''60''36''62'_460 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unfold'45''60''36''62'_460 = erased
-- Data.List.Effectful.Applicative.unfold-⊛
d_unfold'45''8859'_476 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  [AgdaAny -> AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_unfold'45''8859'_476 = erased
-- Data.List.Effectful.Applicative.right-distributive
d_right'45'distributive_504 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  [AgdaAny -> AgdaAny] ->
  [AgdaAny -> AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_right'45'distributive_504 = erased
-- Data.List.Effectful.Applicative.identity
d_identity_526 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_identity_526 = erased
-- Data.List.Effectful.Applicative.pam-lemma
d_pam'45'lemma_546 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  [AgdaAny] ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> [AgdaAny]) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pam'45'lemma_546 = erased
-- Data.List.Effectful.Applicative.composition
d_composition_574 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  [AgdaAny -> AgdaAny] ->
  [AgdaAny -> AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_composition_574 = erased
-- Data.List.Effectful.Applicative.homomorphism
d_homomorphism_634 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_homomorphism_634 = erased
-- Data.List.Effectful.Applicative.interchange
d_interchange_652 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  [AgdaAny -> AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_interchange_652 = erased
