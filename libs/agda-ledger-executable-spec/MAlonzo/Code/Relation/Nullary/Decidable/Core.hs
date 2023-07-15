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

module MAlonzo.Code.Relation.Nullary.Decidable.Core where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.Empty.Irrelevant
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Level
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Relation.Nullary.Decidable.Core.Dec
d_Dec_20 a0 a1 = ()
data T_Dec_20
  = C__because__34 Bool
                   MAlonzo.Code.Relation.Nullary.Reflects.T_Reflects_18
-- Relation.Nullary.Decidable.Core.Dec.does
d_does_30 :: T_Dec_20 -> Bool
d_does_30 v0
  = case coe v0 of
      C__because__34 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Nullary.Decidable.Core.Dec.proof
d_proof_32 ::
  T_Dec_20 -> MAlonzo.Code.Relation.Nullary.Reflects.T_Reflects_18
d_proof_32 v0
  = case coe v0 of
      C__because__34 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Nullary.Decidable.Core.recompute
d_recompute_48 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Dec_20 -> AgdaAny -> AgdaAny
d_recompute_48 ~v0 ~v1 v2 ~v3 = du_recompute_48 v2
du_recompute_48 :: T_Dec_20 -> AgdaAny
du_recompute_48 v0
  = case coe v0 of
      C__because__34 v1 v2
        -> if coe v1
             then case coe v2 of
                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v3 -> coe v3
                    _ -> MAlonzo.RTE.mazUnreachableError
             else coe
                    seq (coe v2)
                    (coe MAlonzo.Code.Data.Empty.Irrelevant.du_'8869''45'elim_10)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Nullary.Decidable.Core.¬?
d_'172''63'_56 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Dec_20 -> T_Dec_20
d_'172''63'_56 ~v0 ~v1 v2 = du_'172''63'_56 v2
du_'172''63'_56 :: T_Dec_20 -> T_Dec_20
du_'172''63'_56 v0
  = coe
      C__because__34
      (coe MAlonzo.Code.Data.Bool.Base.d_not_22 (coe d_does_30 (coe v0)))
      (coe
         MAlonzo.Code.Relation.Nullary.Reflects.du_'172''45'reflects_50
         (coe d_proof_32 (coe v0)))
-- Relation.Nullary.Decidable.Core._×-dec_
d__'215''45'dec__62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Dec_20 -> T_Dec_20 -> T_Dec_20
d__'215''45'dec__62 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du__'215''45'dec__62 v4 v5
du__'215''45'dec__62 :: T_Dec_20 -> T_Dec_20 -> T_Dec_20
du__'215''45'dec__62 v0 v1
  = coe
      C__because__34
      (coe
         MAlonzo.Code.Data.Bool.Base.d__'8743'__24 (coe d_does_30 (coe v0))
         (coe d_does_30 (coe v1)))
      (coe
         MAlonzo.Code.Relation.Nullary.Reflects.du__'215''45'reflects__62
         (coe d_proof_32 (coe v0)) (coe d_proof_32 (coe v1)))
-- Relation.Nullary.Decidable.Core._⊎-dec_
d__'8846''45'dec__72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Dec_20 -> T_Dec_20 -> T_Dec_20
d__'8846''45'dec__72 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du__'8846''45'dec__72 v4 v5
du__'8846''45'dec__72 :: T_Dec_20 -> T_Dec_20 -> T_Dec_20
du__'8846''45'dec__72 v0 v1
  = coe
      C__because__34
      (coe
         MAlonzo.Code.Data.Bool.Base.d__'8744'__30 (coe d_does_30 (coe v0))
         (coe d_does_30 (coe v1)))
      (coe
         MAlonzo.Code.Relation.Nullary.Reflects.du__'8846''45'reflects__78
         (coe d_proof_32 (coe v0)) (coe d_proof_32 (coe v1)))
-- Relation.Nullary.Decidable.Core._→-dec_
d__'8594''45'dec__82 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Dec_20 -> T_Dec_20 -> T_Dec_20
d__'8594''45'dec__82 ~v0 ~v1 ~v2 ~v3 v4 v5
  = du__'8594''45'dec__82 v4 v5
du__'8594''45'dec__82 :: T_Dec_20 -> T_Dec_20 -> T_Dec_20
du__'8594''45'dec__82 v0 v1
  = coe
      C__because__34
      (coe
         MAlonzo.Code.Data.Bool.Base.d__'8744'__30
         (coe MAlonzo.Code.Data.Bool.Base.d_not_22 (coe d_does_30 (coe v0)))
         (coe d_does_30 (coe v1)))
      (coe
         MAlonzo.Code.Relation.Nullary.Reflects.du__'8594''45'reflects__94
         (coe d_proof_32 (coe v0)) (coe d_proof_32 (coe v1)))
-- Relation.Nullary.Decidable.Core.isYes
d_isYes_92 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> T_Dec_20 -> Bool
d_isYes_92 ~v0 ~v1 v2 = du_isYes_92 v2
du_isYes_92 :: T_Dec_20 -> Bool
du_isYes_92 v0
  = case coe v0 of
      C__because__34 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Nullary.Decidable.Core.isNo
d_isNo_94 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> T_Dec_20 -> Bool
d_isNo_94 ~v0 ~v1 v2 = du_isNo_94 v2
du_isNo_94 :: T_Dec_20 -> Bool
du_isNo_94 v0
  = coe
      MAlonzo.Code.Data.Bool.Base.d_not_22 (coe du_isYes_92 (coe v0))
-- Relation.Nullary.Decidable.Core.True
d_True_96 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> T_Dec_20 -> ()
d_True_96 = erased
-- Relation.Nullary.Decidable.Core.False
d_False_100 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> T_Dec_20 -> ()
d_False_100 = erased
-- Relation.Nullary.Decidable.Core.⌊_⌋
d_'8970'_'8971'_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> T_Dec_20 -> Bool
d_'8970'_'8971'_104 v0 v1 v2 = coe du_isYes_92 v2
-- Relation.Nullary.Decidable.Core.toWitness
d_toWitness_108 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Dec_20 -> AgdaAny -> AgdaAny
d_toWitness_108 ~v0 ~v1 v2 ~v3 = du_toWitness_108 v2
du_toWitness_108 :: T_Dec_20 -> AgdaAny
du_toWitness_108 v0
  = case coe v0 of
      C__because__34 v1 v2
        -> coe
             seq (coe v1)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Nullary.Decidable.Core.fromWitness
d_fromWitness_114 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Dec_20 -> AgdaAny -> AgdaAny
d_fromWitness_114 ~v0 ~v1 v2 = du_fromWitness_114 v2
du_fromWitness_114 :: T_Dec_20 -> AgdaAny -> AgdaAny
du_fromWitness_114 v0
  = case coe v0 of
      C__because__34 v1 v2
        -> if coe v1
             then let v3 = coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8 in
                  coe (\ v4 -> v3)
             else coe
                    MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42 (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Nullary.Decidable.Core.toWitnessFalse
d_toWitnessFalse_120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Dec_20 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_toWitnessFalse_120 = erased
-- Relation.Nullary.Decidable.Core.fromWitnessFalse
d_fromWitnessFalse_126 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Dec_20 ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny
d_fromWitnessFalse_126 ~v0 ~v1 v2 = du_fromWitnessFalse_126 v2
du_fromWitnessFalse_126 ::
  T_Dec_20 ->
  (AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny
du_fromWitnessFalse_126 v0
  = case coe v0 of
      C__because__34 v1 v2
        -> if coe v1
             then coe
                    (\ v3 ->
                       coe
                         v3
                         (coe MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42 (coe v2)))
             else (let v3 = coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8 in
                   coe (\ v4 -> v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Nullary.Decidable.Core._.From-yes
d_From'45'yes_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> T_Dec_20 -> ()
d_From'45'yes_138 = erased
-- Relation.Nullary.Decidable.Core._.from-yes
d_from'45'yes_142 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> T_Dec_20 -> AgdaAny
d_from'45'yes_142 ~v0 ~v1 v2 = du_from'45'yes_142 v2
du_from'45'yes_142 :: T_Dec_20 -> AgdaAny
du_from'45'yes_142 v0
  = case coe v0 of
      C__because__34 v1 v2
        -> if coe v1
             then coe
                    MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42 (coe v2)
             else coe
                    MAlonzo.Code.Level.C_lift_20
                    (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Nullary.Decidable.Core._.From-no
d_From'45'no_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> T_Dec_20 -> ()
d_From'45'no_146 = erased
-- Relation.Nullary.Decidable.Core._.from-no
d_from'45'no_150 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> () -> T_Dec_20 -> AgdaAny
d_from'45'no_150 ~v0 ~v1 v2 = du_from'45'no_150 v2
du_from'45'no_150 :: T_Dec_20 -> AgdaAny
du_from'45'no_150 v0
  = case coe v0 of
      C__because__34 v1 v2
        -> if coe v1
             then coe
                    MAlonzo.Code.Level.C_lift_20
                    (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
             else coe
                    MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42 (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Nullary.Decidable.Core.map′
d_map'8242'_154 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  (AgdaAny -> AgdaAny) -> T_Dec_20 -> T_Dec_20
d_map'8242'_154 ~v0 ~v1 ~v2 ~v3 v4 ~v5 v6 = du_map'8242'_154 v4 v6
du_map'8242'_154 :: (AgdaAny -> AgdaAny) -> T_Dec_20 -> T_Dec_20
du_map'8242'_154 v0 v1
  = coe
      C__because__34 (coe d_does_30 (coe v1))
      (case coe v1 of
         C__because__34 v2 v3
           -> if coe v2
                then coe
                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                       (coe
                          v0
                          (coe MAlonzo.Code.Relation.Nullary.Reflects.du_invert_42 (coe v3)))
                else coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30
         _ -> MAlonzo.RTE.mazUnreachableError)
-- Relation.Nullary.Decidable.Core.decidable-stable
d_decidable'45'stable_174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  T_Dec_20 ->
  ((AgdaAny -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny
d_decidable'45'stable_174 ~v0 ~v1 v2 ~v3
  = du_decidable'45'stable_174 v2
du_decidable'45'stable_174 :: T_Dec_20 -> AgdaAny
du_decidable'45'stable_174 v0
  = case coe v0 of
      C__because__34 v1 v2
        -> if coe v1
             then case coe v2 of
                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v3 -> coe v3
                    _ -> MAlonzo.RTE.mazUnreachableError
             else coe
                    seq (coe v2)
                    (coe MAlonzo.Code.Data.Empty.Irrelevant.du_'8869''45'elim_10)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Relation.Nullary.Decidable.Core.¬-drop-Dec
d_'172''45'drop'45'Dec_184 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> T_Dec_20 -> T_Dec_20
d_'172''45'drop'45'Dec_184 ~v0 ~v1 v2
  = du_'172''45'drop'45'Dec_184 v2
du_'172''45'drop'45'Dec_184 :: T_Dec_20 -> T_Dec_20
du_'172''45'drop'45'Dec_184 v0
  = coe du_map'8242'_154 erased (coe du_'172''63'_56 (coe v0))
-- Relation.Nullary.Decidable.Core.excluded-middle
d_excluded'45'middle_188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (T_Dec_20 -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_excluded'45'middle_188 = erased
