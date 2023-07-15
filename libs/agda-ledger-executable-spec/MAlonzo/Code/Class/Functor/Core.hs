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

module MAlonzo.Code.Class.Functor.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Primitive

-- Class.Functor.Core.Type[_↝_]
d_Type'91'_'8605'_'93'_22 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> ()
d_Type'91'_'8605'_'93'_22 = erased
-- Class.Functor.Core.Type↑
d_Type'8593'_28 :: ()
d_Type'8593'_28 = erased
-- Class.Functor.Core.Functor
d_Functor_34 a0 = ()
newtype T_Functor_34
  = C_Functor'46'constructor_219 (MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                  () ->
                                  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
                                  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny)
-- Class.Functor.Core.Functor._<$>_
d__'60''36''62'__40 ::
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''36''62'__40 v0
  = case coe v0 of
      C_Functor'46'constructor_219 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Class.Functor.Core.Functor.fmap
d_fmap_42 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_fmap_42 ~v0 v1 = du_fmap_42 v1
du_fmap_42 ::
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_fmap_42 v0 = coe d__'60''36''62'__40 (coe v0)
-- Class.Functor.Core.Functor._<$_
d__'60''36'__44 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''36'__44 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'60''36'__44 v1 v2 v4 v6 v7
du__'60''36'__44 ::
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'60''36'__44 v0 v1 v2 v3 v4
  = coe d__'60''36''62'__40 v0 v2 erased v1 erased (\ v5 -> v3) v4
-- Class.Functor.Core.Functor._<&>_
d__'60''38''62'__50 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'60''38''62'__50 ~v0 v1 v2 ~v3 v4 ~v5 v6 v7
  = du__'60''38''62'__50 v1 v2 v4 v6 v7
du__'60''38''62'__50 ::
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'60''38''62'__50 v0 v1 v2 v3 v4
  = coe d__'60''36''62'__40 v0 v1 erased v2 erased v4 v3
-- Class.Functor.Core._._<$_
d__'60''36'__54 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'60''36'__54 ~v0 v1 = du__'60''36'__54 v1
du__'60''36'__54 ::
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
du__'60''36'__54 v0 v1 v2 v3 v4 v5 v6
  = coe du__'60''36'__44 (coe v0) v1 v3 v5 v6
-- Class.Functor.Core._._<$>_
d__'60''36''62'__56 ::
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d__'60''36''62'__56 v0 = coe d__'60''36''62'__40 (coe v0)
-- Class.Functor.Core._._<&>_
d__'60''38''62'__58 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
d__'60''38''62'__58 ~v0 v1 = du__'60''38''62'__58 v1
du__'60''38''62'__58 ::
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> (AgdaAny -> AgdaAny) -> AgdaAny
du__'60''38''62'__58 v0 v1 v2 v3 v4 v5 v6
  = coe du__'60''38''62'__50 (coe v0) v1 v3 v5 v6
-- Class.Functor.Core._.fmap
d_fmap_60 ::
  (MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> ()) ->
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
d_fmap_60 ~v0 v1 = du_fmap_60 v1
du_fmap_60 ::
  T_Functor_34 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny
du_fmap_60 v0 = coe du_fmap_42 (coe v0)
-- Class.Functor.Core.FunctorLaws
d_FunctorLaws_66 a0 a1 = ()
data T_FunctorLaws_66 = C_FunctorLaws'46'constructor_8579
-- Class.Functor.Core.FunctorLaws.fmap-id
d_fmap'45'id_96 ::
  T_FunctorLaws_66 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fmap'45'id_96 = erased
-- Class.Functor.Core.FunctorLaws.fmap-∘
d_fmap'45''8728'_110 ::
  T_FunctorLaws_66 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fmap'45''8728'_110 = erased
-- Class.Functor.Core._.fmap-id
d_fmap'45'id_114 ::
  T_FunctorLaws_66 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fmap'45'id_114 = erased
-- Class.Functor.Core._.fmap-∘
d_fmap'45''8728'_116 ::
  T_FunctorLaws_66 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_fmap'45''8728'_116 = erased
