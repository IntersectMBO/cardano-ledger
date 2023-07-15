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

module MAlonzo.Code.Algebra.Solver.Ring.Simple where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Solver.Ring
import qualified MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Relation.Binary.Consequences
import qualified MAlonzo.Code.Relation.Binary.Reflection
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Algebra.Solver.Ring.Simple._._*H_
d__'42'H__162 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486
d__'42'H__162 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'42'H__778 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._._*HN_
d__'42'HN__164 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486
d__'42'HN__164 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'42'HN__774 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._._*N_
d__'42'N__166 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488
d__'42'N__166 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'42'N__782 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._._*NH_
d__'42'NH__168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486
d__'42'NH__168 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'42'NH__770 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._._*x+H_
d__'42'x'43'H__174 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486
d__'42'x'43'H__174 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'42'x'43'H__756 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._._*x+HN_
d__'42'x'43'HN__176 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486
d__'42'x'43'HN__176 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'42'x'43'HN__706 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._._+H_
d__'43'H__178 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486
d__'43'H__178 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'43'H__728 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._._+N_
d__'43'N__180 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488
d__'43'N__180 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'43'N__732 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._._:*_
d__'58''42'__182 ::
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398
d__'58''42'__182
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.C_op_408
      (coe MAlonzo.Code.Algebra.Solver.Ring.C_'91''42''93'_394)
-- Algebra.Solver.Ring.Simple._._:+_
d__'58''43'__184 ::
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398
d__'58''43'__184
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.C_op_408
      (coe MAlonzo.Code.Algebra.Solver.Ring.C_'91''43''93'_392)
-- Algebra.Solver.Ring.Simple._._:-_
d__'58''45'__186 ::
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398
d__'58''45'__186 v0 v1
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.C_op_408
      (coe MAlonzo.Code.Algebra.Solver.Ring.C_'91''43''93'_392) (coe v0)
      (coe MAlonzo.Code.Algebra.Solver.Ring.C_'58''45'__426 (coe v1))
-- Algebra.Solver.Ring.Simple._._⊜_
d__'8860'__188 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d__'8860'__188 ~v0 ~v1 ~v2 ~v3 = du__'8860'__188
du__'8860'__188 ::
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du__'8860'__188 v0
  = coe MAlonzo.Code.Relation.Binary.Reflection.du__'8860'__142
-- Algebra.Solver.Ring.Simple._._:×_
d__'58''215'__192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398
d__'58''215'__192 ~v0 ~v1 v2 ~v3 = du__'58''215'__192 v2
du__'58''215'__192 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398
du__'58''215'__192 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.du__'58''215'__446
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v0))
      v2 v3
-- Algebra.Solver.Ring.Simple._._^N_
d__'94'N__194 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  Integer -> MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488
d__'94'N__194 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'94'N__854 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._._≈H_
d__'8776'H__196 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Solver.Ring.Simple._._≈N_
d__'8776'N__198 a0 a1 a2 a3 a4 a5 a6 = ()
-- Algebra.Solver.Ring.Simple._._≟H_
d__'8799'H__200 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  Maybe MAlonzo.Code.Algebra.Solver.Ring.T__'8776'H__528
d__'8799'H__200 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'8799'H__566 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._._≟N_
d__'8799'N__202 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  Maybe MAlonzo.Code.Algebra.Solver.Ring.T__'8776'N__532
d__'8799'N__202 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d__'8799'N__570 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.*H-homo
d_'42'H'45'homo_204 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'H'45'homo_204 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'42'H'45'homo_1146 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.*HN-homo
d_'42'HN'45'homo_206 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'HN'45'homo_206 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'42'HN'45'homo_1136 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.*N-homo
d_'42'N'45'homo_208 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'N'45'homo_208 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'42'N'45'homo_1156 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.*NH-homo
d_'42'NH'45'homo_210 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'NH'45'homo_210 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'42'NH'45'homo_1124 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.*x+H-homo
d_'42'x'43'H'45'homo_212 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'x'43'H'45'homo_212 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'42'x'43'H'45'homo_1094 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.*x+HN≈*x+
d_'42'x'43'HN'8776''42'x'43'_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'x'43'HN'8776''42'x'43'_214 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'42'x'43'HN'8776''42'x'43'_964
      (coe v0) (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.+H-homo
d_'43'H'45'homo_216 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'43'H'45'homo_216 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'43'H'45'homo_1040 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.+N-homo
d_'43'N'45'homo_218 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'43'N'45'homo_218 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'43'N'45'homo_1050 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.-H_
d_'45'H__220 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486
d_'45'H__220 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'45'H__864 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.-H‿-homo
d_'45'H'8255''45'homo_222 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'45'H'8255''45'homo_222 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'45'H'8255''45'homo_1300
      (coe v0) (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.-N_
d_'45'N__224 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488
d_'45'N__224 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'45'N__868 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.-N‿-homo
d_'45'N'8255''45'homo_226 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'45'N'8255''45'homo_226 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'45'N'8255''45'homo_1308
      (coe v0) (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.0H
d_0H_228 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486
d_0H_228 ~v0 ~v1 ~v2 ~v3 ~v4 = du_0H_228
du_0H_228 :: MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486
du_0H_228 = coe MAlonzo.Code.Algebra.Solver.Ring.C_'8709'_492
-- Algebra.Solver.Ring.Simple._.0N
d_0N_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488
d_0N_230 ~v0 ~v1 v2 ~v3 = du_0N_230 v2
du_0N_230 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer -> MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488
du_0N_230 v0
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.du_0N_688
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v0))
-- Algebra.Solver.Ring.Simple._.0N-homo
d_0N'45'homo_232 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_0N'45'homo_232 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_0N'45'homo_926 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.0≈⟦0⟧
d_0'8776''10214'0'10215'_234 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T__'8776'N__532 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_0'8776''10214'0'10215'_234 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_0'8776''10214'0'10215'_938
      (coe v0) (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.1H
d_1H_236 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486
d_1H_236 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_1H_694 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.1N
d_1N_238 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488
d_1N_238 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_1N_698 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.1N-homo
d_1N'45'homo_240 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_1N'45'homo_240 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_1N'45'homo_950 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.HNF
d_HNF_244 a0 a1 a2 a3 a4 = ()
-- Algebra.Solver.Ring.Simple._.Normal
d_Normal_246 a0 a1 a2 a3 a4 = ()
-- Algebra.Solver.Ring.Simple._.Op
d_Op_248 a0 a1 a2 a3 = ()
-- Algebra.Solver.Ring.Simple._.Polynomial
d_Polynomial_250 a0 a1 a2 a3 a4 = ()
-- Algebra.Solver.Ring.Simple._.^N-homo
d_'94'N'45'homo_256 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'94'N'45'homo_256 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'94'N'45'homo_1282 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.correct
d_correct_264 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_correct_264 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_correct_1362 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.correct-con
d_correct'45'con_266 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_correct'45'con_266 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_correct'45'con_1328 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.correct-var
d_correct'45'var_268 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_correct'45'var_268 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_correct'45'var_1344 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.normalise
d_normalise_270 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488
d_normalise_270 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_normalise_894 (coe v0) (coe v0)
      (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.normalise-con
d_normalise'45'con_272 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> AgdaAny -> MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488
d_normalise'45'con_272 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_normalise'45'con_878 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.normalise-var
d_normalise'45'var_274 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488
d_normalise'45'var_274 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_normalise'45'var_888 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.prove
d_prove_282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  AgdaAny -> AgdaAny
d_prove_282 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
              (coe v2) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
              (coe v2) in
    let v6
          = coe
              MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Reflection.du_prove_90
      (let v7
             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                 (coe v2) in
       let v8
             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                 (coe v7) in
       let v9
             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v8) in
       let v10
             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                 (coe v9) in
       let v11
             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                 (coe v10) in
       let v12
             = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v11) in
       let v13
             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v12) in
       coe
         MAlonzo.Code.Algebra.Structures.du_setoid_164
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v13)))
      (\ v7 v8 v9 ->
         coe
           MAlonzo.Code.Algebra.Solver.Ring.du_'10214'_'10215'_458 (coe v2)
           (coe v5) v8 v9)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.d_'10214'_'10215''8595'_916
         (coe v0) (coe v0) (coe v1) (coe v1) (coe v4) (coe v2) (coe v5)
         (coe v6))
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.d_correct_1362 (coe v0) (coe v0)
         (coe v1) (coe v1) (coe v4) (coe v2) (coe v5) (coe v6))
-- Algebra.Solver.Ring.Simple._.sem
d_sem_284 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Op_390 ->
  AgdaAny -> AgdaAny -> AgdaAny
d_sem_284 ~v0 ~v1 v2 ~v3 = du_sem_284 v2
du_sem_284 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Op_390 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_sem_284 v0
  = coe MAlonzo.Code.Algebra.Solver.Ring.du_sem_454 (coe v0)
-- Algebra.Solver.Ring.Simple._.solve
d_solve_286 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny
d_solve_286 v0 v1 v2 v3
  = let v4
          = coe
              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
              (coe v2) in
    let v5
          = coe
              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
              (coe v2) in
    let v6
          = coe
              MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
              (coe v3) in
    coe
      MAlonzo.Code.Relation.Binary.Reflection.du_solve_114
      (let v7
             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                 (coe v2) in
       let v8
             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                 (coe v7) in
       let v9
             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v8) in
       let v10
             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                 (coe v9) in
       let v11
             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                 (coe v10) in
       let v12
             = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v11) in
       let v13
             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v12) in
       coe
         MAlonzo.Code.Algebra.Structures.du_setoid_164
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v13)))
      (coe (\ v7 -> coe MAlonzo.Code.Algebra.Solver.Ring.C_var_416))
      (\ v7 v8 v9 ->
         coe
           MAlonzo.Code.Algebra.Solver.Ring.du_'10214'_'10215'_458 (coe v2)
           (coe v5) v8 v9)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.d_'10214'_'10215''8595'_916
         (coe v0) (coe v0) (coe v1) (coe v1) (coe v4) (coe v2) (coe v5)
         (coe v6))
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.d_correct_1362 (coe v0) (coe v0)
         (coe v1) (coe v1) (coe v4) (coe v2) (coe v5) (coe v6))
-- Algebra.Solver.Ring.Simple._.∅*x+HN-homo
d_'8709''42'x'43'HN'45'homo_294 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8709''42'x'43'HN'45'homo_294 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'8709''42'x'43'HN'45'homo_1006
      (coe v0) (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.⟦_⟧
d_'10214'_'10215'_296 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'_296 ~v0 ~v1 v2 ~v3 = du_'10214'_'10215'_296 v2
du_'10214'_'10215'_296 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'10214'_'10215'_296 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.du_'10214'_'10215'_458 (coe v0)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v0))
      v2 v3
-- Algebra.Solver.Ring.Simple._.⟦_⟧H
d_'10214'_'10215'H_298 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'H_298 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'10214'_'10215'H_506 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.⟦_⟧H-cong
d_'10214'_'10215'H'45'cong_300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_HNF_486 ->
  MAlonzo.Code.Algebra.Solver.Ring.T__'8776'H__528 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'H'45'cong_300 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'10214'_'10215'H'45'cong_656
      (coe v0) (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.⟦_⟧N
d_'10214'_'10215'N_302 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'N_302 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'10214'_'10215'N_510 (coe v0)
      (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.⟦_⟧N-cong
d_'10214'_'10215'N'45'cong_304 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Normal_488 ->
  MAlonzo.Code.Algebra.Solver.Ring.T__'8776'N__532 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'N'45'cong_304 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'10214'_'10215'N'45'cong_666
      (coe v0) (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
-- Algebra.Solver.Ring.Simple._.⟦_⟧↓
d_'10214'_'10215''8595'_306 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  Integer ->
  MAlonzo.Code.Algebra.Solver.Ring.T_Polynomial_398 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215''8595'_306 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.d_'10214'_'10215''8595'_916
      (coe v0) (coe v0) (coe v1) (coe v1)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_rawRing_344
         (coe v2))
      (coe v2)
      (coe
         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_'45'raw'45'almostCommutative'10230'_770
         (coe v2))
      (coe
         MAlonzo.Code.Relation.Binary.Consequences.du_dec'8658'weaklyDec_800
         (coe v3))
