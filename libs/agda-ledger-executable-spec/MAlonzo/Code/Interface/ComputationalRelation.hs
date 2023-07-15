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

module MAlonzo.Code.Interface.ComputationalRelation where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Interface.ComputationalRelation._.Computational
d_Computational_32 a0 a1 a2 a3 = ()
data T_Computational_32
  = C_MkComputational_42 (AgdaAny ->
                          AgdaAny -> AgdaAny -> Maybe AgdaAny)
                         (AgdaAny ->
                          AgdaAny ->
                          AgdaAny ->
                          AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928)
-- Interface.ComputationalRelation._.Computational.compute
d_compute_38 ::
  T_Computational_32 ->
  AgdaAny -> AgdaAny -> AgdaAny -> Maybe AgdaAny
d_compute_38 v0
  = case coe v0 of
      C_MkComputational_42 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.ComputationalRelation._.Computational.≡-just⇔STS
d_'8801''45'just'8660'STS_40 ::
  T_Computational_32 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8801''45'just'8660'STS_40 v0
  = case coe v0 of
      C_MkComputational_42 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.ComputationalRelation._.ExtendedRel
d_ExtendedRel_44 ::
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  AgdaAny -> AgdaAny -> AgdaAny -> Maybe AgdaAny -> ()
d_ExtendedRel_44 = erased
-- Interface.ComputationalRelation._._.compute
d_compute_78 ::
  T_Computational_32 ->
  AgdaAny -> AgdaAny -> AgdaAny -> Maybe AgdaAny
d_compute_78 v0 = coe d_compute_38 (coe v0)
-- Interface.ComputationalRelation._._.≡-just⇔STS
d_'8801''45'just'8660'STS_80 ::
  T_Computational_32 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8801''45'just'8660'STS_80 v0
  = coe d_'8801''45'just'8660'STS_40 (coe v0)
-- Interface.ComputationalRelation._.ExtendedRelSTS
d_ExtendedRelSTS_82 ::
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  T_Computational_32 ->
  AgdaAny -> AgdaAny -> AgdaAny -> Maybe AgdaAny -> ()
d_ExtendedRelSTS_82 = erased
-- Interface.ComputationalRelation._.ExtendedRel-compute
d_ExtendedRel'45'compute_84 ::
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  T_Computational_32 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_ExtendedRel'45'compute_84 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7
  = du_ExtendedRel'45'compute_84 v4 v5 v6 v7
du_ExtendedRel'45'compute_84 ::
  T_Computational_32 -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_ExtendedRel'45'compute_84 v0 v1 v2 v3
  = let v4 = coe d_compute_38 v0 v1 v2 v3 in
    case coe v4 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v5
        -> coe
             MAlonzo.Code.Function.Bundles.d_to_938
             (coe d_'8801''45'just'8660'STS_40 v0 v1 v2 v3 v5) erased
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> erased
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.ComputationalRelation._.ExtendedRel-rightUnique
d_ExtendedRel'45'rightUnique_118 ::
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  T_Computational_32 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  Maybe AgdaAny ->
  Maybe AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_ExtendedRel'45'rightUnique_118 = erased
-- Interface.ComputationalRelation._.computational⇒rightUnique
d_computational'8658'rightUnique_144 ::
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  T_Computational_32 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_computational'8658'rightUnique_144 = erased
-- Interface.ComputationalRelation._.Computational⇒Dec
d_Computational'8658'Dec_160 ::
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  T_Computational_32 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_Computational'8658'Dec_160 ~v0 ~v1 ~v2 ~v3 v4 v5 v6 v7 v8 v9
  = du_Computational'8658'Dec_160 v4 v5 v6 v7 v8 v9
du_Computational'8658'Dec_160 ::
  T_Computational_32 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_Computational'8658'Dec_160 v0 v1 v2 v3 v4 v5
  = let v6 = coe d_compute_38 v0 v1 v2 v3 in
    let v7
          = coe
              du_ExtendedRel'45'compute_84 (coe v0) (coe v1) (coe v2) (coe v3) in
    case coe v6 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v8
        -> let v9
                 = coe MAlonzo.Code.Interface.DecEq.d__'8799'__20 v5 v8 v4 in
           case coe v9 of
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v10 v11
               -> if coe v10
                    then coe
                           seq (coe v11)
                           (coe
                              MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                              (coe v10)
                              (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 (coe v7)))
                    else coe
                           seq (coe v11)
                           (coe
                              MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                              (coe v10)
                              (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Interface.ComputationalRelation._._.compute
d_compute_246 ::
  T_Computational_32 ->
  T_Computational_32 ->
  AgdaAny -> AgdaAny -> AgdaAny -> Maybe AgdaAny
d_compute_246 v0 ~v1 = du_compute_246 v0
du_compute_246 ::
  T_Computational_32 ->
  AgdaAny -> AgdaAny -> AgdaAny -> Maybe AgdaAny
du_compute_246 v0 = coe d_compute_38 (coe v0)
-- Interface.ComputationalRelation._._.≡-just⇔STS
d_'8801''45'just'8660'STS_248 ::
  T_Computational_32 ->
  T_Computational_32 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8801''45'just'8660'STS_248 v0 ~v1
  = du_'8801''45'just'8660'STS_248 v0
du_'8801''45'just'8660'STS_248 ::
  T_Computational_32 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8801''45'just'8660'STS_248 v0
  = coe d_'8801''45'just'8660'STS_40 (coe v0)
-- Interface.ComputationalRelation._._.compute
d_compute_252 ::
  T_Computational_32 ->
  AgdaAny -> AgdaAny -> AgdaAny -> Maybe AgdaAny
d_compute_252 v0 = coe d_compute_38 (coe v0)
-- Interface.ComputationalRelation._._.≡-just⇔STS
d_'8801''45'just'8660'STS_254 ::
  T_Computational_32 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8801''45'just'8660'STS_254 v0
  = coe d_'8801''45'just'8660'STS_40 (coe v0)
-- Interface.ComputationalRelation._.compute-ext≡
d_compute'45'ext'8801'_256 ::
  () ->
  () ->
  () ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  T_Computational_32 ->
  T_Computational_32 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_compute'45'ext'8801'_256 = erased
-- Interface.ComputationalRelation.Computational⇒Dec'
d_Computational'8658'Dec''_264 ::
  () ->
  () ->
  () ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> ()) ->
  T_Computational_32 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_Computational'8658'Dec''_264 ~v0 ~v1 ~v2 v3 v4 v5 v6 v7 ~v8 v9
  = du_Computational'8658'Dec''_264 v3 v4 v5 v6 v7 v9
du_Computational'8658'Dec''_264 ::
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_Computational_32 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_Computational'8658'Dec''_264 v0 v1 v2 v3 v4 v5
  = coe
      du_Computational'8658'Dec_160 (coe v5) (coe v0) (coe v1) (coe v2)
      (coe v3) (coe v4)
