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

module MAlonzo.Code.Algebra.Construct.LiftedChoice where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Consequences.Base
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Construct.LiftedChoice._.Lift
d_Lift_30 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_Lift_30 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8 v9 v10
  = du_Lift_30 v7 v8 v9 v10
du_Lift_30 ::
  (AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_Lift_30 v0 v1 v2 v3
  = let v4 = coe v0 (coe v1 v2) (coe v1 v3) in
    case coe v4 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5 -> coe v2
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.LiftedChoice._._._◦_
d__'9702'__132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'9702'__132 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 v8
  = du__'9702'__132 v5 v8
du__'9702'__132 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'9702'__132 v0 v1
  = coe
      du_Lift_30 (coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v0))
      (coe v1)
-- Algebra.Construct.LiftedChoice._._.sel-≡
d_sel'45''8801'_134 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel'45''8801'_134 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 v8 v9 v10
  = du_sel'45''8801'_134 v5 v8 v9 v10
du_sel'45''8801'_134 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_sel'45''8801'_134 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Structures.d_sel_410 v0 (coe v1 v2)
      (coe v1 v3)
-- Algebra.Construct.LiftedChoice._._.distrib
d_distrib_156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_distrib_156 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 v8 v9 v10
  = du_distrib_156 v5 v8 v9 v10
du_distrib_156 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_distrib_156 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Structures.d_sel_410 v0 (coe v1 v2)
              (coe v1 v3) in
    case coe v4 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5 -> coe v5
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.LiftedChoice._._._◦_
d__'9702'__190 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'9702'__190 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 v8 ~v9 ~v10
  = du__'9702'__190 v5 v8
du__'9702'__190 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'9702'__190 v0 v1
  = coe
      du_Lift_30 (coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v0))
      (coe v1)
-- Algebra.Construct.LiftedChoice._._.sel
d_sel_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_sel_192 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 v8 ~v9 v10 v11 v12
  = du_sel_192 v5 v8 v10 v11 v12
du_sel_192 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny) ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_sel_192 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Data.Sum.Base.du_map_84
      (coe v2 (coe du__'9702'__190 v0 v1 v3 v4) v3)
      (coe v2 (coe du__'9702'__190 v0 v1 v3 v4) v4)
      (coe du_sel'45''8801'_134 (coe v0) (coe v1) (coe v3) (coe v4))
-- Algebra.Construct.LiftedChoice._._.idem
d_idem_198 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny) ->
  AgdaAny -> AgdaAny
d_idem_198 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 v8 ~v9 v10
  = du_idem_198 v5 v8 v10
du_idem_198 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny) ->
  AgdaAny -> AgdaAny
du_idem_198 v0 v1 v2
  = coe
      MAlonzo.Code.Algebra.Consequences.Base.du_sel'8658'idem_16
      (coe du_sel_192 (coe v0) (coe v1) (coe v2))
-- Algebra.Construct.LiftedChoice._._._◦_
d__'9702'__216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'9702'__216 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 v8 ~v9 ~v10
  = du__'9702'__216 v5 v8
du__'9702'__216 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'9702'__216 v0 v1
  = coe
      du_Lift_30 (coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v0))
      (coe v1)
-- Algebra.Construct.LiftedChoice._._.cong
d_cong_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_cong_218 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 ~v7 v8 ~v9 v10 v11 v12 v13 v14
           v15 v16 v17
  = du_cong_218 v4 v5 v8 v10 v11 v12 v13 v14 v15 v16 v17
du_cong_218 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_cong_218 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = let v11
          = coe
              MAlonzo.Code.Algebra.Structures.d_sel_410 v1 (coe v2 v5)
              (coe v2 v7) in
    let v12
          = coe
              MAlonzo.Code.Algebra.Structures.d_sel_410 v1 (coe v2 v6)
              (coe v2 v8) in
    case coe v11 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v13
        -> case coe v12 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v14 -> coe v9
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v14
               -> coe
                    v3 v5 v8
                    (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                          (coe v2 v5) (coe v0 (coe v2 v5) (coe v2 v7)) (coe v2 v8)
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                             (coe
                                MAlonzo.Code.Algebra.Structures.du_setoid_164
                                (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                             (coe v0 (coe v2 v5) (coe v2 v7)) (coe v0 (coe v2 v6) (coe v2 v8))
                             (coe v2 v8)
                             (coe
                                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                (coe
                                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                                (coe v0 (coe v2 v6) (coe v2 v8)) (coe v2 v8) (coe v2 v8)
                                (coe
                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                   (coe
                                      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMagma_408
                                            (coe v1))))
                                   (coe v2 v8))
                                v14)
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                (MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1))
                                (coe v2 v5) (coe v2 v6) (coe v2 v7) (coe v2 v8) (coe v4 v5 v6 v9)
                                (coe v4 v7 v8 v10)))
                          (coe
                             MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                             (coe v0 (coe v2 v5) (coe v2 v7)) (coe v2 v5) v13)))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v13
        -> case coe v12 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v14
               -> coe
                    v3 v7 v6
                    (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                          (coe v2 v7) (coe v0 (coe v2 v5) (coe v2 v7)) (coe v2 v6)
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                             (coe
                                MAlonzo.Code.Algebra.Structures.du_setoid_164
                                (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                             (coe v0 (coe v2 v5) (coe v2 v7)) (coe v0 (coe v2 v6) (coe v2 v8))
                             (coe v2 v6)
                             (coe
                                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                (coe
                                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                                (coe v0 (coe v2 v6) (coe v2 v8)) (coe v2 v6) (coe v2 v6)
                                (coe
                                   MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                   (coe
                                      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMagma_408
                                            (coe v1))))
                                   (coe v2 v6))
                                v14)
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                (MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1))
                                (coe v2 v5) (coe v2 v6) (coe v2 v7) (coe v2 v8) (coe v4 v5 v6 v9)
                                (coe v4 v7 v8 v10)))
                          (coe
                             MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                             (coe v0 (coe v2 v5) (coe v2 v7)) (coe v2 v7) v13)))
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v14 -> coe v10
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.LiftedChoice._._.assoc
d_assoc_310 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_assoc_310 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 ~v7 v8 ~v9 v10 v11 v12 v13
            v14
  = du_assoc_310 v4 v5 v8 v10 v11 v12 v13 v14
du_assoc_310 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_assoc_310 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      v3 (coe du__'9702'__216 v1 v2 (coe du__'9702'__216 v1 v2 v5 v6) v7)
      (coe du__'9702'__216 v1 v2 v5 (coe du__'9702'__216 v1 v2 v6 v7))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
            (coe
               v2
               (coe du__'9702'__216 v1 v2 (coe du__'9702'__216 v1 v2 v5 v6) v7))
            (coe v0 (coe v2 (coe du__'9702'__216 v1 v2 v5 v6)) (coe v2 v7))
            (coe
               v2
               (coe du__'9702'__216 v1 v2 v5 (coe du__'9702'__216 v1 v2 v6 v7)))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
               (coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
               (coe v0 (coe v2 (coe du__'9702'__216 v1 v2 v5 v6)) (coe v2 v7))
               (coe v0 (coe v0 (coe v2 v5) (coe v2 v6)) (coe v2 v7))
               (coe
                  v2
                  (coe du__'9702'__216 v1 v2 v5 (coe du__'9702'__216 v1 v2 v6 v7)))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                  (coe v0 (coe v0 (coe v2 v5) (coe v2 v6)) (coe v2 v7))
                  (coe v0 (coe v2 v5) (coe v0 (coe v2 v6) (coe v2 v7)))
                  (coe
                     v2
                     (coe du__'9702'__216 v1 v2 v5 (coe du__'9702'__216 v1 v2 v6 v7)))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                     (coe
                        MAlonzo.Code.Algebra.Structures.du_setoid_164
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                     (coe v0 (coe v2 v5) (coe v0 (coe v2 v6) (coe v2 v7)))
                     (coe v0 (coe v2 v5) (coe v2 (coe du__'9702'__216 v1 v2 v6 v7)))
                     (coe
                        v2
                        (coe du__'9702'__216 v1 v2 v5 (coe du__'9702'__216 v1 v2 v6 v7)))
                     (coe
                        MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                        (coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                        (coe v0 (coe v2 v5) (coe v2 (coe du__'9702'__216 v1 v2 v6 v7)))
                        (coe
                           v2
                           (coe du__'9702'__216 v1 v2 v5 (coe du__'9702'__216 v1 v2 v6 v7)))
                        (coe
                           v2
                           (coe du__'9702'__216 v1 v2 v5 (coe du__'9702'__216 v1 v2 v6 v7)))
                        (coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                           (coe
                              MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                              (coe
                                 MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                 (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1))))
                           (coe
                              v2
                              (coe du__'9702'__216 v1 v2 v5 (coe du__'9702'__216 v1 v2 v6 v7))))
                        (coe
                           du_distrib_156 (coe v1) (coe v2) (coe v5)
                           (coe du__'9702'__216 v1 v2 v6 v7)))
                     (coe
                        MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'737'_166
                        (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1))
                        (coe v2 v5) (coe v0 (coe v2 v6) (coe v2 v7))
                        (coe v2 (coe du__'9702'__132 v1 v2 v6 v7))
                        (coe du_distrib_156 (coe v1) (coe v2) (coe v6) (coe v7))))
                  (coe v4 (coe v2 v5) (coe v2 v6) (coe v2 v7)))
               (coe
                  MAlonzo.Code.Algebra.Structures.du_'8729''45'cong'691'_170
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1))
                  (coe v2 v7) (coe v0 (coe v2 v5) (coe v2 v6))
                  (coe v2 (coe du__'9702'__132 v1 v2 v5 v6))
                  (coe du_distrib_156 (coe v1) (coe v2) (coe v5) (coe v6))))
            (coe
               du_distrib_156 (coe v1) (coe v2) (coe du__'9702'__216 v1 v2 v5 v6)
               (coe v7))))
-- Algebra.Construct.LiftedChoice._._.comm
d_comm_320 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d_comm_320 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 ~v7 v8 ~v9 v10 v11 v12 v13
  = du_comm_320 v4 v5 v8 v10 v11 v12 v13
du_comm_320 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du_comm_320 v0 v1 v2 v3 v4 v5 v6
  = coe
      v3 (coe du__'9702'__216 v1 v2 v5 v6)
      (coe du__'9702'__216 v1 v2 v6 v5)
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776''728'_66
            (coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
            (coe v2 (coe du__'9702'__216 v1 v2 v5 v6))
            (coe v0 (coe v2 v5) (coe v2 v6))
            (coe v2 (coe du__'9702'__216 v1 v2 v6 v5))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
               (coe v0 (coe v2 v5) (coe v2 v6)) (coe v0 (coe v2 v6) (coe v2 v5))
               (coe v2 (coe du__'9702'__216 v1 v2 v6 v5))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                  (coe
                     MAlonzo.Code.Algebra.Structures.du_setoid_164
                     (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1)))
                  (coe v0 (coe v2 v6) (coe v2 v5))
                  (coe v2 (coe du__'9702'__216 v1 v2 v6 v5))
                  (coe v2 (coe du__'9702'__216 v1 v2 v6 v5))
                  (coe
                     MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                     (coe
                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_408 (coe v1))))
                     (coe v2 (coe du__'9702'__216 v1 v2 v6 v5)))
                  (coe du_distrib_156 (coe v1) (coe v2) (coe v6) (coe v5)))
               (coe v4 (coe v2 v5) (coe v2 v6)))
            (coe du_distrib_156 (coe v1) (coe v2) (coe v5) (coe v6))))
-- Algebra.Construct.LiftedChoice._._._◦_
d__'9702'__360 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'9702'__360 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 v9 ~v10 ~v11 ~v12
  = du__'9702'__360 v5 v9
du__'9702'__360 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'9702'__360 v0 v1
  = coe
      du_Lift_30 (coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v0))
      (coe v1)
-- Algebra.Construct.LiftedChoice._._.isMagma
d_isMagma_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
d_isMagma_362 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 ~v7 ~v8 v9 v10 v11 v12
  = du_isMagma_362 v4 v5 v9 v10 v11 v12
du_isMagma_362 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsMagma_140
du_isMagma_362 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsMagma'46'constructor_769
      (coe v5)
      (coe du_cong_218 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
-- Algebra.Construct.LiftedChoice._._.isSemigroup
d_isSemigroup_366 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
d_isSemigroup_366 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 ~v7 ~v8 v9 v10 v11 v12
                  v13
  = du_isSemigroup_366 v4 v5 v9 v10 v11 v12 v13
du_isSemigroup_366 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSemigroup_436
du_isSemigroup_366 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSemigroup'46'constructor_9303
      (coe
         du_isMagma_362 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
         (coe v5))
      (coe du_assoc_310 (coe v0) (coe v1) (coe v2) (coe v3) (coe v6))
-- Algebra.Construct.LiftedChoice._._.isBand
d_isBand_372 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
d_isBand_372 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 ~v7 ~v8 v9 v10 v11 v12 v13
  = du_isBand_372 v4 v5 v9 v10 v11 v12 v13
du_isBand_372 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsBand_472
du_isBand_372 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsBand'46'constructor_10089
      (coe
         du_isSemigroup_366 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
         (coe v5) (coe v6))
      (coe
         du_idem_198 (coe v1) (coe v2)
         (\ v7 v8 v9 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40 (coe v5)
              v7))
-- Algebra.Construct.LiftedChoice._._.isSelectiveMagma
d_isSelectiveMagma_376 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
d_isSelectiveMagma_376 ~v0 ~v1 ~v2 ~v3 v4 v5 ~v6 ~v7 ~v8 v9 v10 v11
                       v12
  = du_isSelectiveMagma_376 v4 v5 v9 v10 v11 v12
du_isSelectiveMagma_376 ::
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsEquivalence_26 ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400
du_isSelectiveMagma_376 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Algebra.Structures.C_IsSelectiveMagma'46'constructor_8519
      (coe
         du_isMagma_362 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
         (coe v5))
      (coe
         du_sel_192 (coe v1) (coe v2)
         (\ v6 v7 v8 ->
            coe
              MAlonzo.Code.Relation.Binary.Structures.du_reflexive_40 (coe v5)
              v6))
-- Algebra.Construct.LiftedChoice._._._◦_
d__'9702'__390 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
d__'9702'__390 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 ~v9 v10
  = du__'9702'__390 v5 v10
du__'9702'__390 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) -> AgdaAny -> AgdaAny -> AgdaAny
du__'9702'__390 v0 v1
  = coe
      du_Lift_30 (coe MAlonzo.Code.Algebra.Structures.d_sel_410 (coe v0))
      (coe v1)
-- Algebra.Construct.LiftedChoice._._.preservesᵒ
d_preserves'7506'_404 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_preserves'7506'_404 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 ~v9 v10
                      v11 v12 v13 v14 v15
  = du_preserves'7506'_404 v5 v10 v11 v12 v13 v14 v15
du_preserves'7506'_404 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_preserves'7506'_404 v0 v1 v2 v3 v4 v5 v6
  = case coe v6 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7
        -> let v8
                 = coe
                     MAlonzo.Code.Algebra.Structures.d_sel_410 v0 (coe v1 v4)
                     (coe v1 v5) in
           case coe v8 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v9 -> coe v7
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v9 -> coe v2 v4 v5 v7 v9
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v7
        -> let v8
                 = coe
                     MAlonzo.Code.Algebra.Structures.d_sel_410 v0 (coe v1 v4)
                     (coe v1 v5) in
           case coe v8 of
             MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v9 -> coe v3 v4 v5 v7 v9
             MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v9 -> coe v7
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.LiftedChoice._._.preservesʳ
d_preserves'691'_486 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_preserves'691'_486 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 ~v9 v10 v11
                     v12 v13 v14
  = du_preserves'691'_486 v5 v10 v11 v12 v13 v14
du_preserves'691'_486 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_preserves'691'_486 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Algebra.Structures.d_sel_410 v0 (coe v1 v3)
              (coe v1 v4) in
    case coe v6 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7 -> coe v2 v3 v4 v5 v7
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.LiftedChoice._._.preservesᵇ
d_preserves'7495'_524 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
d_preserves'7495'_524 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 ~v9 v10
                      v11 v12 v13 v14
  = du_preserves'7495'_524 v5 v10 v11 v12 v13 v14
du_preserves'7495'_524 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny
du_preserves'7495'_524 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Algebra.Structures.d_sel_410 v0 (coe v1 v2)
              (coe v1 v3) in
    case coe v6 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v7 -> coe v4
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Construct.LiftedChoice._._.forcesᵇ
d_forces'7495'_566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_forces'7495'_566 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 ~v8 ~v9 v10 v11
                   v12 v13 v14 v15
  = du_forces'7495'_566 v5 v10 v11 v12 v13 v14 v15
du_forces'7495'_566 ::
  MAlonzo.Code.Algebra.Structures.T_IsSelectiveMagma_400 ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny -> AgdaAny) ->
  AgdaAny ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_forces'7495'_566 v0 v1 v2 v3 v4 v5 v6
  = let v7
          = coe
              MAlonzo.Code.Algebra.Structures.d_sel_410 v0 (coe v1 v4)
              (coe v1 v5) in
    case coe v7 of
      MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v8
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v6)
             (coe v2 v4 v5 v6 v8)
      MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v8
        -> coe
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3 v4 v5 v6 v8)
             (coe v6)
      _ -> MAlonzo.RTE.mazUnreachableError
