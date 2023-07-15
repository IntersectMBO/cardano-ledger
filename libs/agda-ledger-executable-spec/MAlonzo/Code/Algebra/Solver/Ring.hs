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

module MAlonzo.Code.Algebra.Solver.Ring where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Bundles.Raw
import qualified MAlonzo.Code.Algebra.Definitions.RawSemiring
import qualified MAlonzo.Code.Algebra.Properties.Semiring.Exp
import qualified MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing
import qualified MAlonzo.Code.Algebra.Solver.Ring.Lemmas
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.Fin.Base
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Function.Base
import qualified MAlonzo.Code.Relation.Binary.Bundles
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Base.Single
import qualified MAlonzo.Code.Relation.Binary.Reasoning.Setoid
import qualified MAlonzo.Code.Relation.Binary.Reflection
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Algebra.Solver.Ring.C.Carrier
d_Carrier_66 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> ()
d_Carrier_66 = erased
-- Algebra.Solver.Ring._._*_
d__'42'__72 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'42'__72 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du__'42'__72 v5
du__'42'__72 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'42'__72 v0
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
      (coe v0)
-- Algebra.Solver.Ring._._+_
d__'43'__74 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'43'__74 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du__'43'__74 v5
du__'43'__74 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'43'__74 v0
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
      (coe v0)
-- Algebra.Solver.Ring._._≈_
d__'8776'__76 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny -> ()
d__'8776'__76 = erased
-- Algebra.Solver.Ring._.-_
d_'45'__160 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_'45'__160 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du_'45'__160 v5
du_'45'__160 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> AgdaAny
du_'45'__160 v0
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
      (coe v0)
-- Algebra.Solver.Ring._.0#
d_0'35'_168 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> AgdaAny
d_0'35'_168 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du_0'35'_168 v5
du_0'35'_168 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny
du_0'35'_168 v0
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
      (coe v0)
-- Algebra.Solver.Ring._.1#
d_1'35'_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> AgdaAny
d_1'35'_170 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du_1'35'_170 v5
du_1'35'_170 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny
du_1'35'_170 v0
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
      (coe v0)
-- Algebra.Solver.Ring._.Carrier
d_Carrier_172 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> ()
d_Carrier_172 = erased
-- Algebra.Solver.Ring._.⟦_⟧
d_'10214'_'10215'_352 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> AgdaAny -> AgdaAny
d_'10214'_'10215'_352 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7
  = du_'10214'_'10215'_352 v6
du_'10214'_'10215'_352 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  AgdaAny -> AgdaAny
du_'10214'_'10215'_352 v0
  = coe
      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'10214'_'10215'_752
      (coe v0)
-- Algebra.Solver.Ring._._^_
d__'94'__356 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  AgdaAny -> Integer -> AgdaAny
d__'94'__356 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 = du__'94'__356 v5
du__'94'__356 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  AgdaAny -> Integer -> AgdaAny
du__'94'__356 v0
  = let v1
          = coe
              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
              (coe
                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                 (coe v0)) in
    coe
      MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
      (coe
         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
         (coe
            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
            (coe v1)))
-- Algebra.Solver.Ring.Op
d_Op_390 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_Op_390 = C_'91''43''93'_392 | C_'91''42''93'_394
-- Algebra.Solver.Ring.Polynomial
d_Polynomial_398 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_Polynomial_398
  = C_op_408 T_Op_390 T_Polynomial_398 T_Polynomial_398 |
    C_con_412 AgdaAny | C_var_416 MAlonzo.Code.Data.Fin.Base.T_Fin_10 |
    C__'58''94'__422 T_Polynomial_398 Integer |
    C_'58''45'__426 T_Polynomial_398
-- Algebra.Solver.Ring._:+_
d__'58''43'__430 ::
  T_Polynomial_398 -> T_Polynomial_398 -> T_Polynomial_398
d__'58''43'__430 = coe C_op_408 (coe C_'91''43''93'_392)
-- Algebra.Solver.Ring._:*_
d__'58''42'__434 ::
  T_Polynomial_398 -> T_Polynomial_398 -> T_Polynomial_398
d__'58''42'__434 = coe C_op_408 (coe C_'91''42''93'_394)
-- Algebra.Solver.Ring._:-_
d__'58''45'__438 ::
  T_Polynomial_398 -> T_Polynomial_398 -> T_Polynomial_398
d__'58''45'__438 v0 v1
  = coe
      C_op_408 (coe C_'91''43''93'_392) (coe v0)
      (coe C_'58''45'__426 (coe v1))
-- Algebra.Solver.Ring._:×_
d__'58''215'__446 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> Integer -> T_Polynomial_398 -> T_Polynomial_398
d__'58''215'__446 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 ~v8 v9 v10
  = du__'58''215'__446 v4 v9 v10
du__'58''215'__446 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  Integer -> T_Polynomial_398 -> T_Polynomial_398
du__'58''215'__446 v0 v1 v2
  = case coe v1 of
      0 -> coe
             C_con_412
             (coe MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0))
      _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
           coe
             C_op_408 (coe C_'91''43''93'_392) (coe v2)
             (coe du__'58''215'__446 (coe v0) (coe v3) (coe v2))
-- Algebra.Solver.Ring.sem
d_sem_454 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  T_Op_390 -> AgdaAny -> AgdaAny -> AgdaAny
d_sem_454 ~v0 ~v1 ~v2 ~v3 ~v4 v5 ~v6 ~v7 v8 = du_sem_454 v5 v8
du_sem_454 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  T_Op_390 -> AgdaAny -> AgdaAny -> AgdaAny
du_sem_454 v0 v1
  = case coe v1 of
      C_'91''43''93'_392
        -> coe
             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
             (coe v0)
      C_'91''42''93'_394
        -> coe
             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
             (coe v0)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.⟦_⟧
d_'10214'_'10215'_458 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Polynomial_398 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'_458 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 ~v7 ~v8 v9 v10
  = du_'10214'_'10215'_458 v5 v6 v9 v10
du_'10214'_'10215'_458 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  T_Polynomial_398 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
du_'10214'_'10215'_458 v0 v1 v2 v3
  = case coe v2 of
      C_op_408 v4 v5 v6
        -> coe
             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
             (coe du_'10214'_'10215'_458 (coe v0) (coe v1) (coe v5) (coe v3))
             (coe du_sem_454 (coe v0) (coe v4))
             (coe du_'10214'_'10215'_458 (coe v0) (coe v1) (coe v6) (coe v3))
      C_con_412 v4
        -> coe
             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'10214'_'10215'_752
             v1 v4
      C_var_416 v4
        -> coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v3) (coe v4)
      C__'58''94'__422 v4 v5
        -> coe
             MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
             (coe
                MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                (coe
                   MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                   (coe
                      MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                         (coe v0)))))
             (coe du_'10214'_'10215'_458 (coe v0) (coe v1) (coe v4) (coe v3))
             (coe v5)
      C_'58''45'__426 v4
        -> coe
             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
             v0 (coe du_'10214'_'10215'_458 (coe v0) (coe v1) (coe v4) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.HNF
d_HNF_486 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_HNF_486
  = C_'8709'_492 | C__'42'x'43'__496 T_HNF_486 T_Normal_488
-- Algebra.Solver.Ring.Normal
d_Normal_488 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
data T_Normal_488 = C_con_498 AgdaAny | C_poly_502 T_HNF_486
-- Algebra.Solver.Ring.⟦_⟧H
d_'10214'_'10215'H_506 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_HNF_486 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'H_506 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_'8709'_492
        -> coe
             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
             (coe v5)
      C__'42'x'43'__496 v12 v13
        -> case coe v10 of
             MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v15 v16
               -> coe
                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                    v5
                    (coe
                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                       v5
                       (d_'10214'_'10215'H_506
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8) (coe v12)
                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v15 v16))
                       v15)
                    (d_'10214'_'10215'N_510
                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                       (coe v7) (coe v8) (coe v13) (coe v16))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.⟦_⟧N
d_'10214'_'10215'N_510 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Normal_488 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'N_510 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_con_498 v11
        -> coe
             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'10214'_'10215'_752
             v6 v11
      C_poly_502 v12
        -> let v13 = subInt (coe v8) (coe (1 :: Integer)) in
           coe
             d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
             (coe v5) (coe v6) (coe v7) (coe v13) (coe v12) (coe v10)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._≈H_
d__'8776'H__528 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = ()
data T__'8776'H__528
  = C_'8709'_536 | C__'42'x'43'__548 T__'8776'H__528 T__'8776'N__532
-- Algebra.Solver.Ring._≈N_
d__'8776'N__532 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = ()
data T__'8776'N__532
  = C_con_554 AgdaAny | C_poly_562 T__'8776'H__528
-- Algebra.Solver.Ring._≟H_
d__'8799'H__566 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_HNF_486 -> T_HNF_486 -> Maybe T__'8776'H__528
d__'8799'H__566 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_'8709'_492
        -> case coe v10 of
             C_'8709'_492
               -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe C_'8709'_536)
             C__'42'x'43'__496 v13 v14
               -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
             _ -> MAlonzo.RTE.mazUnreachableError
      C__'42'x'43'__496 v12 v13
        -> let v14 = subInt (coe v8) (coe (1 :: Integer)) in
           case coe v10 of
             C_'8709'_492 -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
             C__'42'x'43'__496 v16 v17
               -> let v18
                        = d__'8799'H__566
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v12) (coe v16) in
                  let v19
                        = d__'8799'N__570
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v14) (coe v13) (coe v17) in
                  case coe v18 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v20
                      -> case coe v19 of
                           MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v21
                             -> coe
                                  MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                                  (coe C__'42'x'43'__548 v20 v21)
                           MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v19
                           _ -> MAlonzo.RTE.mazUnreachableError
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                      -> case coe v19 of
                           MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v19
                           _ -> coe v18
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._≟N_
d__'8799'N__570 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_Normal_488 -> T_Normal_488 -> Maybe T__'8776'N__532
d__'8799'N__570 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_con_498 v11
        -> case coe v10 of
             C_con_498 v12
               -> let v13 = coe v7 v11 v12 in
                  case coe v13 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v14
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe C_con_554 v14)
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v13
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C_poly_502 v12
        -> case coe v10 of
             C_poly_502 v14
               -> let v15
                        = d__'8799'H__566
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v12) (coe v14) in
                  case coe v15 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v16
                      -> coe
                           MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe C_poly_562 v16)
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v15
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.⟦_⟧H-cong
d_'10214'_'10215'H'45'cong_656 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_HNF_486 ->
  T_HNF_486 ->
  T__'8776'H__528 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'H'45'cong_656 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
                               v11 v12
  = case coe v11 of
      C_'8709'_536
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                        (coe v5))))))))))
             (d_'10214'_'10215'H_506
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                (coe v7) (coe v8) (coe C_'8709'_492) (coe v12))
      C__'42'x'43'__548 v18 v19
        -> case coe v9 of
             C__'42'x'43'__496 v21 v22
               -> case coe v10 of
                    C__'42'x'43'__496 v24 v25
                      -> case coe v12 of
                           MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v27 v28
                             -> coe
                                  MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                  (coe
                                     MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                     (coe
                                        d_'10214'_'10215'H'45'cong_656 (coe v0) (coe v1) (coe v2)
                                        (coe v3) (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                        (coe v21) (coe v24) (coe v18)
                                        (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v27 v28))
                                     (coe
                                        MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                        (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                           (coe
                                              MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe
                                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                 (coe
                                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                    (coe v5)))))
                                        (d_'10214'_'10215'H_506
                                           (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                           (coe v6) (coe v7) (coe v8) (coe v21)
                                           (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v27 v28))
                                        (d_'10214'_'10215'H_506
                                           (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                           (coe v6) (coe v7) (coe v8) (coe v24)
                                           (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v27 v28))
                                        v27 v27)
                                     (coe
                                        MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                        (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                           (coe
                                              MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                              (coe
                                                 MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                 (coe
                                                    MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                    (coe
                                                       MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                       (coe
                                                          MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                          (coe
                                                             MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                             (coe
                                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                (coe
                                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                   (coe v5))))))))))
                                        v27))
                                  (coe
                                     MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                     (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                        (coe
                                           MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                           (coe
                                              MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                              (coe
                                                 MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                 (coe
                                                    MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                    (coe
                                                       MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                       (coe
                                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                          (coe
                                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                             (coe v5)))))))))
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                        v5
                                        (d_'10214'_'10215'H_506
                                           (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                           (coe v6) (coe v7) (coe v8) (coe v21)
                                           (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v27 v28))
                                        v27)
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                        v5
                                        (d_'10214'_'10215'H_506
                                           (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                           (coe v6) (coe v7) (coe v8) (coe v24)
                                           (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v27 v28))
                                        v27)
                                     (d_'10214'_'10215'N_510
                                        (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                        (coe v6) (coe v7) (coe v8) (coe v22) (coe v28))
                                     (d_'10214'_'10215'N_510
                                        (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                        (coe v6) (coe v7) (coe v8) (coe v25) (coe v28)))
                                  (coe
                                     d_'10214'_'10215'N'45'cong_666 (coe v0) (coe v1) (coe v2)
                                     (coe v3) (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v22)
                                     (coe v25) (coe v19) (coe v28))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.⟦_⟧N-cong
d_'10214'_'10215'N'45'cong_666 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Normal_488 ->
  T_Normal_488 ->
  T__'8776'N__532 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215'N'45'cong_666 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
                               v11 v12
  = case coe v11 of
      C_con_554 v15 -> coe v15
      C_poly_562 v16
        -> let v17 = subInt (coe v8) (coe (1 :: Integer)) in
           case coe v9 of
             C_poly_502 v19
               -> case coe v10 of
                    C_poly_502 v21
                      -> coe
                           d_'10214'_'10215'H'45'cong_656 (coe v0) (coe v1) (coe v2) (coe v3)
                           (coe v4) (coe v5) (coe v6) (coe v7) (coe v17) (coe v19) (coe v21)
                           (coe v16) (coe v12)
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.0H
d_0H_684 ::
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> Integer -> T_HNF_486
d_0H_684 ~v0 ~v1 ~v2 = du_0H_684
du_0H_684 :: T_HNF_486
du_0H_684 = coe C_'8709'_492
-- Algebra.Solver.Ring.0N
d_0N_688 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> Integer -> T_Normal_488
d_0N_688 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 v8 = du_0N_688 v4 v8
du_0N_688 ::
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  Integer -> T_Normal_488
du_0N_688 v0 v1
  = case coe v1 of
      0 -> coe
             C_con_498
             (coe MAlonzo.Code.Algebra.Bundles.Raw.d_0'35'_272 (coe v0))
      _ -> coe C_poly_502 (coe C_'8709'_492)
-- Algebra.Solver.Ring.1H
d_1H_694 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> Integer -> T_HNF_486
d_1H_694 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      C__'42'x'43'__496 (coe C_'8709'_492)
      (d_1N_698
         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
         (coe v7) (coe v8))
-- Algebra.Solver.Ring.1N
d_1N_698 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) -> Integer -> T_Normal_488
d_1N_698 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = case coe v8 of
      0 -> coe
             C_con_498
             (coe MAlonzo.Code.Algebra.Bundles.Raw.d_1'35'_274 (coe v4))
      _ -> let v9 = subInt (coe v8) (coe (1 :: Integer)) in
           coe
             C_poly_502
             (d_1H_694
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                (coe v7) (coe v9))
-- Algebra.Solver.Ring._*x+HN_
d__'42'x'43'HN__706 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_HNF_486 -> T_Normal_488 -> T_HNF_486
d__'42'x'43'HN__706 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_'8709'_492
        -> let v12
                 = d__'8799'N__570
                     (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                     (coe v7) (coe v8) (coe v10) (coe du_0N_688 (coe v4) (coe v8)) in
           case coe v12 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v13 -> coe C_'8709'_492
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe C__'42'x'43'__496 (coe C_'8709'_492) v10
             _ -> MAlonzo.RTE.mazUnreachableError
      C__'42'x'43'__496 v12 v13
        -> coe C__'42'x'43'__496 (coe C__'42'x'43'__496 v12 v13) v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._+H_
d__'43'H__728 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_HNF_486 -> T_HNF_486 -> T_HNF_486
d__'43'H__728 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_'8709'_492 -> coe v10
      C__'42'x'43'__496 v12 v13
        -> case coe v10 of
             C_'8709'_492 -> coe C__'42'x'43'__496 v12 v13
             C__'42'x'43'__496 v15 v16
               -> coe
                    d__'42'x'43'HN__706 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                    (coe v5) (coe v6) (coe v7) (coe v8)
                    (coe
                       d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                       (coe v6) (coe v7) (coe v8) (coe v12) (coe v15))
                    (coe
                       d__'43'N__732 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                       (coe v6) (coe v7) (coe v8) (coe v13) (coe v16))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._+N_
d__'43'N__732 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_Normal_488 -> T_Normal_488 -> T_Normal_488
d__'43'N__732 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_con_498 v11
        -> case coe v10 of
             C_con_498 v12
               -> coe
                    C_con_498
                    (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'43'__266 v4 v11 v12)
             _ -> MAlonzo.RTE.mazUnreachableError
      C_poly_502 v12
        -> let v13 = subInt (coe v8) (coe (1 :: Integer)) in
           case coe v10 of
             C_poly_502 v15
               -> coe
                    C_poly_502
                    (d__'43'H__728
                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                       (coe v7) (coe v13) (coe v12) (coe v15))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._*x+H_
d__'42'x'43'H__756 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_HNF_486 -> T_HNF_486 -> T_HNF_486
d__'42'x'43'H__756 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v10 of
      C_'8709'_492
        -> case coe v9 of
             C_'8709'_492 -> coe C_'8709'_492
             C__'42'x'43'__496 v13 v14
               -> coe
                    C__'42'x'43'__496 (coe C__'42'x'43'__496 v13 v14)
                    (coe du_0N_688 (coe v4) (coe v8))
             _ -> MAlonzo.RTE.mazUnreachableError
      C__'42'x'43'__496 v12 v13
        -> coe
             d__'42'x'43'HN__706 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
             (coe v5) (coe v6) (coe v7) (coe v8)
             (coe
                d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                (coe v6) (coe v7) (coe v8) (coe v9) (coe v12))
             (coe v13)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._*NH_
d__'42'NH__770 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_Normal_488 -> T_HNF_486 -> T_HNF_486
d__'42'NH__770 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v10 of
      C_'8709'_492 -> coe C_'8709'_492
      C__'42'x'43'__496 v12 v13
        -> let v14
                 = d__'8799'N__570
                     (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                     (coe v7) (coe v8) (coe v9) (coe du_0N_688 (coe v4) (coe v8)) in
           case coe v14 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v15 -> coe C_'8709'_492
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe
                    C__'42'x'43'__496
                    (d__'42'NH__770
                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                       (coe v7) (coe v8) (coe v9) (coe v12))
                    (d__'42'N__782
                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                       (coe v7) (coe v8) (coe v9) (coe v13))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._*HN_
d__'42'HN__774 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_HNF_486 -> T_Normal_488 -> T_HNF_486
d__'42'HN__774 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_'8709'_492 -> coe C_'8709'_492
      C__'42'x'43'__496 v12 v13
        -> let v14
                 = d__'8799'N__570
                     (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                     (coe v7) (coe v8) (coe v10) (coe du_0N_688 (coe v4) (coe v8)) in
           case coe v14 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v15 -> coe C_'8709'_492
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe
                    C__'42'x'43'__496
                    (d__'42'HN__774
                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                       (coe v7) (coe v8) (coe v12) (coe v10))
                    (d__'42'N__782
                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                       (coe v7) (coe v8) (coe v13) (coe v10))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._*H_
d__'42'H__778 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_HNF_486 -> T_HNF_486 -> T_HNF_486
d__'42'H__778 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_'8709'_492 -> coe C_'8709'_492
      C__'42'x'43'__496 v12 v13
        -> case coe v10 of
             C_'8709'_492 -> coe C_'8709'_492
             C__'42'x'43'__496 v15 v16
               -> coe
                    d__'42'x'43'HN__706 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                    (coe v5) (coe v6) (coe v7) (coe v8)
                    (coe
                       d__'42'x'43'H__756 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                       (coe v5) (coe v6) (coe v7) (coe v8)
                       (coe
                          d__'42'H__778 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                          (coe v6) (coe v7) (coe v8) (coe v12) (coe v15))
                       (coe
                          d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                          (coe v6) (coe v7) (coe v8)
                          (coe
                             d__'42'HN__774 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe v12) (coe v16))
                          (coe
                             d__'42'NH__770 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe v13) (coe v15))))
                    (coe
                       d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                       (coe v6) (coe v7) (coe v8) (coe v13) (coe v16))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._*N_
d__'42'N__782 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_Normal_488 -> T_Normal_488 -> T_Normal_488
d__'42'N__782 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_con_498 v11
        -> case coe v10 of
             C_con_498 v12
               -> coe
                    C_con_498
                    (coe MAlonzo.Code.Algebra.Bundles.Raw.d__'42'__268 v4 v11 v12)
             _ -> MAlonzo.RTE.mazUnreachableError
      C_poly_502 v12
        -> let v13 = subInt (coe v8) (coe (1 :: Integer)) in
           case coe v10 of
             C_poly_502 v15
               -> coe
                    C_poly_502
                    (d__'42'H__778
                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                       (coe v7) (coe v13) (coe v12) (coe v15))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._^N_
d__'94'N__854 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_Normal_488 -> Integer -> T_Normal_488
d__'94'N__854 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v10 of
      0 -> coe
             d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
             (coe v6) (coe v7) (coe v8)
      _ -> let v11 = subInt (coe v10) (coe (1 :: Integer)) in
           coe
             d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
             (coe v6) (coe v7) (coe v8) (coe v9)
             (coe
                d__'94'N__854 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                (coe v6) (coe v7) (coe v8) (coe v9) (coe v11))
-- Algebra.Solver.Ring.-H_
d_'45'H__864 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_HNF_486 -> T_HNF_486
d_'45'H__864 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = coe
      d__'42'NH__770 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
      (coe v5) (coe v6) (coe v7) (coe v8)
      (coe
         d_'45'N__868 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
         (coe v6) (coe v7) (coe v8)
         (coe
            d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
            (coe v6) (coe v7) (coe v8)))
      (coe v9)
-- Algebra.Solver.Ring.-N_
d_'45'N__868 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_Normal_488 -> T_Normal_488
d_'45'N__868 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = case coe v9 of
      C_con_498 v10
        -> coe
             C_con_498 (coe MAlonzo.Code.Algebra.Bundles.Raw.d_'45'__270 v4 v10)
      C_poly_502 v11
        -> let v12 = subInt (coe v8) (coe (1 :: Integer)) in
           coe
             C_poly_502
             (d_'45'H__864
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                (coe v7) (coe v12) (coe v11))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.normalise-con
d_normalise'45'con_878 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> AgdaAny -> T_Normal_488
d_normalise'45'con_878 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = case coe v8 of
      0 -> coe C_con_498 (coe v9)
      _ -> let v10 = subInt (coe v8) (coe (1 :: Integer)) in
           coe
             C_poly_502
             (d__'42'x'43'HN__706
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                (coe v7) (coe v10) (coe C_'8709'_492)
                (coe
                   d_normalise'45'con_878 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                   (coe v5) (coe v6) (coe v7) (coe v10) (coe v9)))
-- Algebra.Solver.Ring.normalise-var
d_normalise'45'var_888 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> MAlonzo.Code.Data.Fin.Base.T_Fin_10 -> T_Normal_488
d_normalise'45'var_888 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = case coe v9 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> let v11 = subInt (coe v8) (coe (1 :: Integer)) in
           coe
             C_poly_502
             (coe
                C__'42'x'43'__496
                (coe
                   C__'42'x'43'__496 (coe C_'8709'_492)
                   (d_1N_698
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v11)))
                (coe du_0N_688 (coe v4) (coe v11)))
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v11
        -> let v12 = subInt (coe v8) (coe (1 :: Integer)) in
           coe
             C_poly_502
             (d__'42'x'43'HN__706
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                (coe v7) (coe v12) (coe C_'8709'_492)
                (coe
                   d_normalise'45'var_888 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                   (coe v5) (coe v6) (coe v7) (coe v12) (coe v11)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.normalise
d_normalise_894 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> T_Polynomial_398 -> T_Normal_488
d_normalise_894 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = case coe v9 of
      C_op_408 v10 v11 v12
        -> case coe v10 of
             C_'91''43''93'_392
               -> coe
                    d__'43'N__732 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                    (coe v6) (coe v7) (coe v8)
                    (coe
                       d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                       (coe v5) (coe v6) (coe v7) (coe v8) (coe v11))
                    (coe
                       d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                       (coe v5) (coe v6) (coe v7) (coe v8) (coe v12))
             C_'91''42''93'_394
               -> coe
                    d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                    (coe v6) (coe v7) (coe v8)
                    (coe
                       d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                       (coe v5) (coe v6) (coe v7) (coe v8) (coe v11))
                    (coe
                       d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                       (coe v5) (coe v6) (coe v7) (coe v8) (coe v12))
             _ -> MAlonzo.RTE.mazUnreachableError
      C_con_412 v10
        -> coe
             d_normalise'45'con_878 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
             (coe v5) (coe v6) (coe v7) (coe v8) (coe v10)
      C_var_416 v10
        -> coe
             d_normalise'45'var_888 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
             (coe v5) (coe v6) (coe v7) (coe v8) (coe v10)
      C__'58''94'__422 v10 v11
        -> coe
             d__'94'N__854 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
             (coe v6) (coe v7) (coe v8)
             (coe
                d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                (coe v5) (coe v6) (coe v7) (coe v8) (coe v10))
             (coe v11)
      C_'58''45'__426 v10
        -> coe
             d_'45'N__868 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
             (coe v6) (coe v7) (coe v8)
             (coe
                d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                (coe v5) (coe v6) (coe v7) (coe v8) (coe v10))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.⟦_⟧↓
d_'10214'_'10215''8595'_916 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Polynomial_398 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'10214'_'10215''8595'_916 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = coe
      d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
      (coe v5) (coe v6) (coe v7) (coe v8)
      (coe
         d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
         (coe v5) (coe v6) (coe v7) (coe v8) (coe v9))
      (coe v10)
-- Algebra.Solver.Ring.0N-homo
d_0N'45'homo_926 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_0N'45'homo_926 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = case coe v9 of
      MAlonzo.Code.Data.Vec.Base.C_'91''93'_32
        -> coe
             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'45'homo_760
             (coe v6)
      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                        (coe v5))))))))))
             (d_'10214'_'10215'N_510
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                (coe v7) (coe v8) (coe du_0N_688 (coe v4) (coe v8))
                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.0≈⟦0⟧
d_0'8776''10214'0'10215'_938 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Normal_488 ->
  T__'8776'N__532 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_0'8776''10214'0'10215'_938 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_sym_36
      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                     (coe
                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe
                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                           (coe
                              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe
                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                 (coe v5))))))))))
      (d_'10214'_'10215'N_510
         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
         (coe v7) (coe v8) (coe v9) (coe v11))
      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
         (coe v5))
      (MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
         (coe
            MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
            (let v12
                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                       (coe v5) in
             let v13
                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                       (coe v12) in
             let v14
                   = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v13) in
             let v15
                   = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                       (coe v14) in
             let v16
                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                       (coe v15) in
             let v17
                   = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v16) in
             let v18
                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v17) in
             coe
               MAlonzo.Code.Algebra.Structures.du_setoid_164
               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v18)))
            (d_'10214'_'10215'N_510
               (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
               (coe v7) (coe v8) (coe v9) (coe v11))
            (d_'10214'_'10215'N_510
               (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
               (coe v7) (coe v8) (coe du_0N_688 (coe v4) (coe v8)) (coe v11))
            (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
               (coe v5))
            (coe
               MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
               (let v12
                      = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                          (coe v5) in
                let v13
                      = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                          (coe v12) in
                let v14
                      = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v13) in
                let v15
                      = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                          (coe v14) in
                let v16
                      = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                          (coe v15) in
                let v17
                      = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v16) in
                let v18
                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v17) in
                coe
                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v18)))
               (d_'10214'_'10215'N_510
                  (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                  (coe v7) (coe v8) (coe du_0N_688 (coe v4) (coe v8)) (coe v11))
               (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                  (coe v5))
               (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                  (coe v5))
               (coe
                  MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                  (coe
                     MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                     (coe
                        MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                        (let v12
                               = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                   (coe v5) in
                         let v13
                               = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                   (coe v12) in
                         let v14
                               = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v13) in
                         let v15
                               = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                   (coe v14) in
                         let v16
                               = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                   (coe v15) in
                         let v17
                               = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v16) in
                         let v18
                               = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v17) in
                         coe
                           MAlonzo.Code.Algebra.Structures.du_setoid_164
                           (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v18)))))
                  (coe
                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                     (coe v5)))
               (d_0N'45'homo_926
                  (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                  (coe v7) (coe v8) (coe v11)))
            (d_'10214'_'10215'N'45'cong_666
               (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
               (coe v7) (coe v8) (coe v9) (coe du_0N_688 (coe v4) (coe v8))
               (coe v10) (coe v11))))
-- Algebra.Solver.Ring.1N-homo
d_1N'45'homo_950 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_1N'45'homo_950 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  = case coe v9 of
      MAlonzo.Code.Data.Vec.Base.C_'91''93'_32
        -> coe
             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'45'homo_762
             (coe v6)
      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12
        -> let v13 = subInt (coe v8) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v14
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                           (coe v5) in
                 let v15
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe v14) in
                 let v16
                       = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
                 let v17
                       = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                           (coe v16) in
                 let v18
                       = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                           (coe v17) in
                 let v19
                       = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                 let v20
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v19) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))
                (coe
                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                   v5
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                      v5
                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                         (coe v5))
                      v11)
                   (d_'10214'_'10215'N_510
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v13)
                      (coe
                         d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                         (coe v6) (coe v7) (coe v13))
                      (coe v12)))
                (coe
                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                   v5
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                      v5
                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                         (coe v5))
                      v11)
                   (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                      (coe v5)))
                (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                   (coe v5))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   (let v14
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                              (coe v5) in
                    let v15
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe v14) in
                    let v16
                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
                    let v17
                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                              (coe v16) in
                    let v18
                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                              (coe v17) in
                    let v19
                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                    let v20
                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v19) in
                    coe
                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                      v5
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                         v5
                         (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                            (coe v5))
                         v11)
                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                         (coe v5)))
                   (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                      (coe v5))
                   (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                      (coe v5))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                            (let v14
                                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                       (coe v5) in
                             let v15
                                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                       (coe v14) in
                             let v16
                                   = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
                             let v17
                                   = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                       (coe v16) in
                             let v18
                                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                       (coe v17) in
                             let v19
                                   = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                             let v20
                                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v19) in
                             coe
                               MAlonzo.Code.Algebra.Structures.du_setoid_164
                               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))))
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                         (coe v5)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8326'_358 (coe v5)
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                         (coe v5))
                      (coe v11)))
                (coe
                   MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                   (coe
                      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                      (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_isMagma_444
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                  (coe
                                     MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe
                                        MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                        (coe
                                           MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                           (coe
                                              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe
                                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                 (coe v5))))))))))
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                         v5
                         (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                            (coe v5))
                         v11))
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                      (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe
                                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe
                                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                        (coe
                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                           (coe
                                              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                              (coe v5)))))))))
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                         v5
                         (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                            (coe v5))
                         v11)
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                         v5
                         (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                            (coe v5))
                         v11)
                      (d_'10214'_'10215'N_510
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v13)
                         (coe
                            d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                            (coe v6) (coe v7) (coe v13))
                         (coe v12))
                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                         (coe v5)))
                   (coe
                      d_1N'45'homo_950 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                      (coe v5) (coe v6) (coe v7) (coe v13) (coe v12))))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.*x+HN≈*x+
d_'42'x'43'HN'8776''42'x'43'_964 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_HNF_486 ->
  T_Normal_488 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'x'43'HN'8776''42'x'43'_964 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
                                 v11
  = case coe v9 of
      C_'8709'_492
        -> case coe v11 of
             MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v14 v15
               -> let v16
                        = d__'8799'N__570
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v10) (coe du_0N_688 (coe v4) (coe v8)) in
                  case coe v16 of
                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v17
                      -> coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                              (let v18
                                     = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                         (coe v5) in
                               let v19
                                     = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                         (coe v18) in
                               let v20
                                     = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                         (coe v19) in
                               let v21
                                     = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                         (coe v20) in
                               let v22
                                     = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                         (coe v21) in
                               let v23
                                     = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v22) in
                               let v24
                                     = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe v23) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_setoid_164
                                 (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v24)))
                              (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                 (coe v5))
                              (d_'10214'_'10215'N_510
                                 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                 (coe v7) (coe v8) (coe v10) (coe v15))
                              (coe
                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                 v5
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                    v5
                                    (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                       (coe v5))
                                    v14)
                                 (d_'10214'_'10215'N_510
                                    (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                    (coe v7) (coe v8) (coe v10) (coe v15)))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                 (let v18
                                        = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                            (coe v5) in
                                  let v19
                                        = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                            (coe v18) in
                                  let v20
                                        = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                            (coe v19) in
                                  let v21
                                        = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                            (coe v20) in
                                  let v22
                                        = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                            (coe v21) in
                                  let v23
                                        = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe v22) in
                                  let v24
                                        = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                            (coe v23) in
                                  coe
                                    MAlonzo.Code.Algebra.Structures.du_setoid_164
                                    (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v24)))
                                 (d_'10214'_'10215'N_510
                                    (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                    (coe v7) (coe v8) (coe v10) (coe v15))
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                          (coe v5))
                                       v14)
                                    (d_'10214'_'10215'N_510
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8) (coe v10) (coe v15)))
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                          (coe v5))
                                       v14)
                                    (d_'10214'_'10215'N_510
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8) (coe v10) (coe v15)))
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                       (coe
                                          MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                          (let v18
                                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                     (coe v5) in
                                           let v19
                                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                     (coe v18) in
                                           let v20
                                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                     (coe v19) in
                                           let v21
                                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                     (coe v20) in
                                           let v22
                                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                     (coe v21) in
                                           let v23
                                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                     (coe v22) in
                                           let v24
                                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                     (coe v23) in
                                           coe
                                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                (coe v24)))))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                             (coe v5))
                                          v14)
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v10) (coe v15))))
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                                    (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                       (coe
                                          MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                          (coe
                                             MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                         (coe
                                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                            (coe
                                                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                               (coe v5))))))))))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                             (coe v5))
                                          v14)
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v10) (coe v15)))
                                    (d_'10214'_'10215'N_510
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8) (coe v10) (coe v15))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8326'_358
                                       (coe v5)
                                       (coe
                                          d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3)
                                          (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v10)
                                          (coe v15))
                                       (coe v14))))
                              (d_0'8776''10214'0'10215'_938
                                 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                 (coe v7) (coe v8) (coe v10) (coe v17) (coe v15)))
                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                      -> coe
                           MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                           (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                              (coe
                                 MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                 (coe
                                    MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                    (coe
                                       MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                       (coe
                                          MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                          (coe
                                             MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                      (coe v5))))))))))
                           (d_'10214'_'10215'H_506
                              (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                              (coe v7) (coe v8) (coe C__'42'x'43'__496 (coe C_'8709'_492) v10)
                              (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v14 v15))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      C__'42'x'43'__496 v13 v14
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                        (coe v5))))))))))
             (d_'10214'_'10215'H_506
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                (coe v7) (coe v8)
                (coe
                   d__'42'x'43'HN__706 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                   (coe v5) (coe v6) (coe v7) (coe v8) (coe C__'42'x'43'__496 v13 v14)
                   (coe v10))
                (coe v11))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.∅*x+HN-homo
d_'8709''42'x'43'HN'45'homo_1006 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Normal_488 ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'8709''42'x'43'HN'45'homo_1006 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
                                 v11
  = let v12
          = d__'8799'N__570
              (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
              (coe v7) (coe v8) (coe v9) (coe du_0N_688 (coe v4) (coe v8)) in
    case coe v12 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v13
        -> coe
             d_0'8776''10214'0'10215'_938 (coe v0) (coe v1) (coe v2) (coe v3)
             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v13)
             (coe v11)
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
        -> coe
             MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8326'_358 (coe v5)
             (coe
                d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v11))
             (coe v10)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.+H-homo
d_'43'H'45'homo_1040 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_HNF_486 ->
  T_HNF_486 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'43'H'45'homo_1040 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  = case coe v9 of
      C_'8709'_492
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_sym_36
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                        (coe v5))))))))))
             (coe
                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                v5
                (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                   (coe v5))
                (d_'10214'_'10215'H_506
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8)
                   (coe
                      d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                      (coe v6) (coe v7) (coe v8) (coe C_'8709'_492) (coe v10))
                   (coe v11)))
             (d_'10214'_'10215'H_506
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                (coe v7) (coe v8)
                (coe
                   d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                   (coe v6) (coe v7) (coe v8) (coe C_'8709'_492) (coe v10))
                (coe v11))
             (let v13
                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                        (coe v5) in
              let v14
                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                        (coe v13) in
              let v15
                    = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v14) in
              let v16
                    = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                        (coe v15) in
              let v17
                    = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                        (coe v16) in
              coe
                MAlonzo.Code.Algebra.Structures.du_identity'737'_640
                (MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v17))
                (d_'10214'_'10215'H_506
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8)
                   (coe
                      d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                      (coe v6) (coe v7) (coe v8) (coe C_'8709'_492) (coe v10))
                   (coe v11)))
      C__'42'x'43'__496 v13 v14
        -> case coe v10 of
             C_'8709'_492
               -> coe
                    MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                    (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                       (coe
                          MAlonzo.Code.Algebra.Structures.d_isMagma_444
                          (coe
                             MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                            (coe
                                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                               (coe v5))))))))))
                    (coe
                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                       v5
                       (d_'10214'_'10215'H_506
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                             (coe v6) (coe v7) (coe v8) (coe C__'42'x'43'__496 v13 v14)
                             (coe C_'8709'_492))
                          (coe v11))
                       (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                          (coe v5)))
                    (d_'10214'_'10215'H_506
                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                       (coe v7) (coe v8)
                       (coe
                          d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                          (coe v6) (coe v7) (coe v8) (coe C__'42'x'43'__496 v13 v14)
                          (coe C_'8709'_492))
                       (coe v11))
                    (let v16
                           = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                               (coe v5) in
                     let v17
                           = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                               (coe v16) in
                     let v18
                           = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v17) in
                     let v19
                           = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                               (coe v18) in
                     let v20
                           = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                               (coe v19) in
                     coe
                       MAlonzo.Code.Algebra.Structures.du_identity'691'_642
                       (MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v20))
                       (d_'10214'_'10215'H_506
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                             (coe v6) (coe v7) (coe v8) (coe C__'42'x'43'__496 v13 v14)
                             (coe C_'8709'_492))
                          (coe v11)))
             C__'42'x'43'__496 v16 v17
               -> case coe v11 of
                    MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20
                      -> coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                              (let v21
                                     = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                         (coe v5) in
                               let v22
                                     = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                         (coe v21) in
                               let v23
                                     = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                         (coe v22) in
                               let v24
                                     = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                         (coe v23) in
                               let v25
                                     = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                         (coe v24) in
                               let v26
                                     = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v25) in
                               let v27
                                     = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe v26) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_setoid_164
                                 (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v27)))
                              (d_'10214'_'10215'H_506
                                 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                 (coe v7) (coe v8)
                                 (coe
                                    d__'42'x'43'HN__706 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                    (coe v5) (coe v6) (coe v7) (coe v8)
                                    (coe
                                       d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                       (coe v5) (coe v6) (coe v7) (coe v8) (coe v13) (coe v16))
                                    (coe
                                       d__'43'N__732 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                       (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v17)))
                                 (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                              (coe
                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                 v5
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                    v5
                                    (d_'10214'_'10215'H_506
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8)
                                       (coe
                                          d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                          (coe v5) (coe v6) (coe v7) (coe v8) (coe v13) (coe v16))
                                       (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                    v19)
                                 (d_'10214'_'10215'N_510
                                    (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                    (coe v7) (coe v8)
                                    (coe
                                       d__'43'N__732 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                       (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v17))
                                    (coe v20)))
                              (coe
                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                 v5
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (d_'10214'_'10215'H_506
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v13)
                                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                       v19)
                                    (d_'10214'_'10215'N_510
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8) (coe v14) (coe v20)))
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (d_'10214'_'10215'H_506
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v16)
                                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                       v19)
                                    (d_'10214'_'10215'N_510
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                 (let v21
                                        = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                            (coe v5) in
                                  let v22
                                        = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                            (coe v21) in
                                  let v23
                                        = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                            (coe v22) in
                                  let v24
                                        = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                            (coe v23) in
                                  let v25
                                        = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                            (coe v24) in
                                  let v26
                                        = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe v25) in
                                  let v27
                                        = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                            (coe v26) in
                                  coe
                                    MAlonzo.Code.Algebra.Structures.du_setoid_164
                                    (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v27)))
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (d_'10214'_'10215'H_506
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8)
                                          (coe
                                             d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                             (coe v16))
                                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                       v19)
                                    (d_'10214'_'10215'N_510
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8)
                                       (coe
                                          d__'43'N__732 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                          (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v17))
                                       (coe v20)))
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v13)
                                             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v16)
                                             (coe
                                                MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20)))
                                       v19)
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v14) (coe v20))
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v13)
                                             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                          v19)
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v14) (coe v20)))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v16)
                                             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                          v19)
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                    (let v21
                                           = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                               (coe v5) in
                                     let v22
                                           = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                               (coe v21) in
                                     let v23
                                           = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                               (coe v22) in
                                     let v24
                                           = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                               (coe v23) in
                                     let v25
                                           = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                               (coe v24) in
                                     let v26
                                           = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                               (coe v25) in
                                     let v27
                                           = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                               (coe v26) in
                                     coe
                                       MAlonzo.Code.Algebra.Structures.du_setoid_164
                                       (coe
                                          MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v27)))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20)))
                                          v19)
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v14) (coe v20))
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             v19)
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v14) (coe v20)))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             v19)
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             v19)
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v14) (coe v20)))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             v19)
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                       (coe
                                          MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                          (coe
                                             MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                             (let v21
                                                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                        (coe v5) in
                                              let v22
                                                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                        (coe v21) in
                                              let v23
                                                    = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                        (coe v22) in
                                              let v24
                                                    = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                        (coe v23) in
                                              let v25
                                                    = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                        (coe v24) in
                                              let v26
                                                    = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                        (coe v25) in
                                              let v27
                                                    = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                        (coe v26) in
                                              coe
                                                MAlonzo.Code.Algebra.Structures.du_setoid_164
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                   (coe v27)))))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))
                                                v19)
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                (coe v20)))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))
                                                v19)
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                (coe v20)))))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8321'_262
                                       (coe v5)
                                       (coe
                                          d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3)
                                          (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                       (coe
                                          d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3)
                                          (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                       (coe
                                          d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3)
                                          (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                          (coe v20))
                                       (coe
                                          d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3)
                                          (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                          (coe v20))
                                       (coe v19)))
                                 (coe
                                    MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                    (coe
                                       MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                       (coe
                                          d_'43'H'45'homo_1040 (coe v0) (coe v1) (coe v2) (coe v3)
                                          (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                          (coe v16)
                                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                       (coe
                                          MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                          (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                      (coe v5)))))
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8)
                                             (coe
                                                d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe v13) (coe v16))
                                             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20)))
                                          v19 v19)
                                       (coe
                                          MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                          (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                     (coe v5))))))))))
                                          v19))
                                    (coe
                                       MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                       (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                          (coe
                                             MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                         (coe
                                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                            (coe
                                                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                               (coe v5)))))))))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8)
                                             (coe
                                                d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe v13) (coe v16))
                                             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                          v19)
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20)))
                                          v19)
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8)
                                          (coe
                                             d__'43'N__732 (coe v0) (coe v1) (coe v2) (coe v3)
                                             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                             (coe v17))
                                          (coe v20))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v14) (coe v20))
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                    (coe
                                       d_'43'N'45'homo_1050 (coe v0) (coe v1) (coe v2) (coe v3)
                                       (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                       (coe v17) (coe v20))))
                              (d_'42'x'43'HN'8776''42'x'43'_964
                                 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                 (coe v7) (coe v8)
                                 (coe
                                    d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                    (coe v5) (coe v6) (coe v7) (coe v8) (coe v13) (coe v16))
                                 (coe
                                    d__'43'N__732 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                    (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v17))
                                 (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20)))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.+N-homo
d_'43'N'45'homo_1050 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Normal_488 ->
  T_Normal_488 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'43'N'45'homo_1050 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  = case coe v9 of
      C_con_498 v12
        -> case coe v10 of
             C_con_498 v13
               -> coe
                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'43''45'homo_754
                    v6 v12 v13
             _ -> MAlonzo.RTE.mazUnreachableError
      C_poly_502 v13
        -> let v14 = subInt (coe v8) (coe (1 :: Integer)) in
           case coe v10 of
             C_poly_502 v16
               -> coe
                    d_'43'H'45'homo_1040 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                    (coe v5) (coe v6) (coe v7) (coe v14) (coe v13) (coe v16) (coe v11)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.*x+H-homo
d_'42'x'43'H'45'homo_1094 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_HNF_486 ->
  T_HNF_486 ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'x'43'H'45'homo_1094 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12
  = case coe v9 of
      C_'8709'_492
        -> case coe v10 of
             C_'8709'_492
               -> coe
                    MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                    (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                       (coe
                          MAlonzo.Code.Algebra.Structures.d_isMagma_444
                          (coe
                             MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                            (coe
                                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                               (coe v5))))))))))
                    (coe
                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                       v5
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                          v5
                          (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                             (coe v5))
                          v11)
                       (d_'10214'_'10215'H_506
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d__'42'x'43'H__756 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                             (coe C_'8709'_492))
                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                    (d_'10214'_'10215'H_506
                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                       (coe v7) (coe v8)
                       (coe
                          d__'42'x'43'H__756 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                          (coe v5) (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                          (coe C_'8709'_492))
                       (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                    (coe
                       MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8326'_358 (coe v5)
                       (coe
                          d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                          (coe v5) (coe v6) (coe v7) (coe v8)
                          (coe
                             d__'42'x'43'H__756 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                             (coe C_'8709'_492))
                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                       (coe v11))
             C__'42'x'43'__496 v15 v16
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v17
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v18
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v17) in
                        let v19
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v18) in
                        let v20
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v19) in
                        let v21
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v20) in
                        let v22
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                        let v23
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v22) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))
                       (d_'10214'_'10215'H_506
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d__'42'x'43'HN__706 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8)
                             (coe
                                d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                (coe v6) (coe v7) (coe v8) (coe C_'8709'_492) (coe v15))
                             (coe v16))
                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'H_506
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8)
                                (coe
                                   d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe C_'8709'_492) (coe v15))
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             v11)
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe v16) (coe v12)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'H_506
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe C_'8709'_492)
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             v11)
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v16) (coe v12))))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v17
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v18
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v17) in
                           let v19
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v19) in
                           let v21
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v20) in
                           let v22
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                           let v23
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v22) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8)
                                   (coe
                                      d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                                      (coe v15))
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v16) (coe v12)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe C_'8709'_492)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v16) (coe v12)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe C_'8709'_492)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v16) (coe v12))))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                             (let v17
                                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                        (coe v5) in
                              let v18
                                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                        (coe v17) in
                              let v19
                                    = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v18) in
                              let v20
                                    = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                        (coe v19) in
                              let v21
                                    = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                        (coe v20) in
                              let v22
                                    = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                              let v23
                                    = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v22) in
                              coe
                                MAlonzo.Code.Algebra.Structures.du_setoid_164
                                (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v15)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v16) (coe v12)))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe C_'8709'_492)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v15)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v16) (coe v12))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe C_'8709'_492)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v15)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v16) (coe v12))))
                             (coe
                                MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                (coe
                                   MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                   (coe
                                      MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                      (let v17
                                             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                 (coe v5) in
                                       let v18
                                             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                 (coe v17) in
                                       let v19
                                             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                 (coe v18) in
                                       let v20
                                             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                 (coe v19) in
                                       let v21
                                             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                 (coe v20) in
                                       let v22
                                             = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                 (coe v21) in
                                       let v23
                                             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                 (coe v22) in
                                       coe
                                         MAlonzo.Code.Algebra.Structures.du_setoid_164
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                            (coe v23)))))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                      v5
                                      (coe
                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                         v5
                                         (d_'10214'_'10215'H_506
                                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                            (coe v6) (coe v7) (coe v8) (coe v15)
                                            (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                         v11)
                                      (d_'10214'_'10215'N_510
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v16) (coe v12)))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8320'_242 (coe v5)
                                (coe
                                   d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3)
                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                (coe
                                   d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3)
                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v15)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                (coe
                                   d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3)
                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v16) (coe v12))
                                (coe v11)))
                          (coe
                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                             (coe
                                MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                (coe
                                   d_'43'H'45'homo_1040 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe C_'8709'_492) (coe v15)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                   (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                            (coe
                                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                               (coe v5)))))
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8)
                                      (coe
                                         d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                         (coe v5) (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                                         (coe v15))
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v15)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                   v11 v11)
                                (coe
                                   MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                   (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                     (coe
                                                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                           (coe
                                                              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                              (coe v5))))))))))
                                   v11))
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                  (coe
                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                        (coe v5)))))))))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8)
                                      (coe
                                         d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                         (coe v5) (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                                         (coe v15))
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe C_'8709'_492)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v15)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v16) (coe v12))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v16) (coe v12)))
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                           (coe v5))))))))))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v16) (coe v12)))))
                       (d_'42'x'43'HN'8776''42'x'43'_964
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                             (coe v6) (coe v7) (coe v8) (coe C_'8709'_492) (coe v15))
                          (coe v16) (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
             _ -> MAlonzo.RTE.mazUnreachableError
      C__'42'x'43'__496 v14 v15
        -> case coe v10 of
             C_'8709'_492
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v17
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v18
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v17) in
                        let v19
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v18) in
                        let v20
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v19) in
                        let v21
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v20) in
                        let v22
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                        let v23
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v22) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'H_506
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             v11)
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe du_0N_688 (coe v4) (coe v8)) (coe v12)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'H_506
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             v11)
                          (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                             (coe v5)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'H_506
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             v11)
                          (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                             (coe v5)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                          (coe
                             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                             (coe
                                MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                (let v17
                                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                           (coe v5) in
                                 let v18
                                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                           (coe v17) in
                                 let v19
                                       = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                           (coe v18) in
                                 let v20
                                       = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                           (coe v19) in
                                 let v21
                                       = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                           (coe v20) in
                                 let v22
                                       = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                                 let v23
                                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                           (coe v22) in
                                 coe
                                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                (coe v5))))
                       (coe
                          MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                          (coe
                             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                  (coe
                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                        (coe v5))))))))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11))
                          (coe
                             MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                             (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                               (coe
                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                  (coe
                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                     (coe v5)))))))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe du_0N_688 (coe v4) (coe v8)) (coe v12))
                             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                (coe v5)))
                          (coe
                             d_0N'45'homo_926 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe v12))))
             C__'42'x'43'__496 v17 v18
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v19
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v20
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v19) in
                        let v21
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v20) in
                        let v22
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v21) in
                        let v23
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v22) in
                        let v24
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v23) in
                        let v25
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v24) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v25)))
                       (d_'10214'_'10215'H_506
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d__'42'x'43'HN__706 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8)
                             (coe
                                d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                (coe v6) (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                (coe v17))
                             (coe v18))
                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'H_506
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8)
                                (coe
                                   d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                   (coe v5) (coe v6) (coe v7) (coe v8)
                                   (coe C__'42'x'43'__496 v14 v15) (coe v17))
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             v11)
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe v18) (coe v12)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'H_506
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             v11)
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v17)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v18) (coe v12))))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v19
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v20
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v19) in
                           let v21
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v20) in
                           let v22
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v21) in
                           let v23
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v22) in
                           let v24
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v23) in
                           let v25
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v24) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v25)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8)
                                   (coe
                                      d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                      (coe C__'42'x'43'__496 v14 v15) (coe v17))
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v18) (coe v12)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v17)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v18) (coe v12)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v17)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v18) (coe v12))))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                             (let v19
                                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                        (coe v5) in
                              let v20
                                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                        (coe v19) in
                              let v21
                                    = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v20) in
                              let v22
                                    = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                        (coe v21) in
                              let v23
                                    = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                        (coe v22) in
                              let v24
                                    = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v23) in
                              let v25
                                    = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v24) in
                              coe
                                MAlonzo.Code.Algebra.Structures.du_setoid_164
                                (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v25)))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v17)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v18) (coe v12)))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v17)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v18) (coe v12))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v17)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v18) (coe v12))))
                             (coe
                                MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                (coe
                                   MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                   (coe
                                      MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                      (let v19
                                             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                 (coe v5) in
                                       let v20
                                             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                 (coe v19) in
                                       let v21
                                             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                 (coe v20) in
                                       let v22
                                             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                 (coe v21) in
                                       let v23
                                             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                 (coe v22) in
                                       let v24
                                             = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                 (coe v23) in
                                       let v25
                                             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                 (coe v24) in
                                       coe
                                         MAlonzo.Code.Algebra.Structures.du_setoid_164
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                            (coe v25)))))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                      v5
                                      (coe
                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                         v5
                                         (d_'10214'_'10215'H_506
                                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                            (coe v6) (coe v7) (coe v8) (coe v17)
                                            (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                         v11)
                                      (d_'10214'_'10215'N_510
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v18) (coe v12)))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8320'_242 (coe v5)
                                (coe
                                   d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3)
                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                   (coe C__'42'x'43'__496 v14 v15)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                (coe
                                   d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3)
                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                (coe
                                   d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3)
                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v18) (coe v12))
                                (coe v11)))
                          (coe
                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                             (coe
                                MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                (coe
                                   d_'43'H'45'homo_1040 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                   (coe v5) (coe v6) (coe v7) (coe v8)
                                   (coe C__'42'x'43'__496 v14 v15) (coe v17)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                   (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                            (coe
                                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                               (coe v5)))))
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8)
                                      (coe
                                         d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                         (coe v5) (coe v6) (coe v7) (coe v8)
                                         (coe C__'42'x'43'__496 v14 v15) (coe v17))
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v17)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                   v11 v11)
                                (coe
                                   MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                   (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                     (coe
                                                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                           (coe
                                                              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                              (coe v5))))))))))
                                   v11))
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                  (coe
                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                        (coe v5)))))))))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8)
                                      (coe
                                         d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                         (coe v5) (coe v6) (coe v7) (coe v8)
                                         (coe C__'42'x'43'__496 v14 v15) (coe v17))
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v17)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v18) (coe v12))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v18) (coe v12)))
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                           (coe v5))))))))))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v18) (coe v12)))))
                       (d_'42'x'43'HN'8776''42'x'43'_964
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                             (coe v6) (coe v7) (coe v8) (coe C__'42'x'43'__496 v14 v15)
                             (coe v17))
                          (coe v18) (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.*NH-homo
d_'42'NH'45'homo_1124 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Normal_488 ->
  T_HNF_486 ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'NH'45'homo_1124 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12
  = case coe v10 of
      C_'8709'_492
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_sym_36
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                        (coe v5))))))))))
             (coe
                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                v5
                (d_'10214'_'10215'N_510
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8) (coe v9) (coe v12))
                (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                   (coe v5)))
             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                (coe v5))
             (let v14
                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                        (coe v5) in
              let v15
                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                        (coe v14) in
              let v16
                    = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
              coe
                MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
                (coe
                   MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
                   (coe v16))
                (d_'10214'_'10215'N_510
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8) (coe v9) (coe v12)))
      C__'42'x'43'__496 v14 v15
        -> let v16
                 = d__'8799'N__570
                     (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                     (coe v7) (coe v8) (coe v9) (coe du_0N_688 (coe v4) (coe v8)) in
           case coe v16 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v17
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v18
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v19
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v18) in
                        let v20
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v19) in
                        let v21
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v20) in
                        let v22
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v21) in
                        let v23
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v22) in
                        let v24
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v23) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v24)))
                       (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                          (coe v5))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                          v5
                          (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                             (coe v5))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v14)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v15) (coe v12))))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                          v5
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe v9) (coe v12))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v14)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v15) (coe v12))))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v18
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v19
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v19) in
                           let v21
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v20) in
                           let v22
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v21) in
                           let v23
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v22) in
                           let v24
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v23) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v24)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                (coe v5))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v9) (coe v12))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v9) (coe v12))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                   (let v18
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                              (coe v5) in
                                    let v19
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe v18) in
                                    let v20
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe v19) in
                                    let v21
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                              (coe v20) in
                                    let v22
                                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                              (coe v21) in
                                    let v23
                                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                              (coe v22) in
                                    let v24
                                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                              (coe v23) in
                                    coe
                                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v24)))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v9) (coe v12))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v14)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15) (coe v12)))))
                          (coe
                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                             (coe
                                d_0'8776''10214'0'10215'_938 (coe v0) (coe v1) (coe v2) (coe v3)
                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v17)
                                (coe v12))
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                      (coe
                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                            (coe v5)))))
                                (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                   (coe v5))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v9) (coe v12))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v14)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15) (coe v12)))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v14)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15) (coe v12))))
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                           (coe v5))))))))))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v14)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15) (coe v12))))))
                       (coe
                          MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                          (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                               (coe
                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                  (coe
                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                     (coe v5))))))))))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                (coe v5))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))))
                          (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                             (coe v5))
                          (let v18
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v19
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v19) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
                             (coe
                                MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
                                (coe v20))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))))))
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v17
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v18
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v17) in
                        let v19
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v18) in
                        let v20
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v19) in
                        let v21
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v20) in
                        let v22
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                        let v23
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v22) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'H_506
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8)
                                (coe
                                   d__'42'NH__770 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v14))
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             v11)
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8)
                             (coe
                                d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                (coe v6) (coe v7) (coe v8) (coe v9) (coe v15))
                             (coe v12)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v9) (coe v12))
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v14)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                             v11)
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v9) (coe v12))
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v15) (coe v12))))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                          v5
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe v9) (coe v12))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v14)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v15) (coe v12))))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v17
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v18
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v17) in
                           let v19
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v19) in
                           let v21
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v20) in
                           let v22
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                           let v23
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v22) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v9) (coe v12))
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                v11)
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v9) (coe v12))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v9) (coe v12))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v9) (coe v12))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                   (let v17
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                              (coe v5) in
                                    let v18
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe v17) in
                                    let v19
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe v18) in
                                    let v20
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                              (coe v19) in
                                    let v21
                                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                              (coe v20) in
                                    let v22
                                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                              (coe v21) in
                                    let v23
                                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                              (coe v22) in
                                    coe
                                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v9) (coe v12))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v14)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15) (coe v12)))))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8323'_306 (coe v5)
                             (coe
                                d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v12))
                             (coe
                                d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             (coe
                                d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v15) (coe v12))
                             (coe v11)))
                       (coe
                          MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                          (coe
                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                             (coe
                                d_'42'NH'45'homo_1124 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v14) (coe v11)
                                (coe v12))
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                      (coe
                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                            (coe v5)))))
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8)
                                   (coe
                                      d__'42'NH__770 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v14))
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v9) (coe v12))
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                v11 v11)
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                           (coe v5))))))))))
                                v11))
                          (coe
                             MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                             (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                               (coe
                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                  (coe
                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                     (coe v5)))))))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8)
                                   (coe
                                      d__'42'NH__770 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v14))
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v9) (coe v12))
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12)))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8)
                                (coe
                                   d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v15))
                                (coe v12))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v9) (coe v12))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))))
                          (coe
                             d_'42'N'45'homo_1156 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v15) (coe v12))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.*HN-homo
d_'42'HN'45'homo_1136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_HNF_486 ->
  T_Normal_488 ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'HN'45'homo_1136 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12
  = case coe v9 of
      C_'8709'_492
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_sym_36
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                        (coe v5))))))))))
             (coe
                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                v5
                (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                   (coe v5))
                (d_'10214'_'10215'N_510
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8) (coe v10) (coe v12)))
             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                (coe v5))
             (let v14
                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                        (coe v5) in
              let v15
                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                        (coe v14) in
              let v16
                    = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
              coe
                MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
                (coe
                   MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
                   (coe v16))
                (d_'10214'_'10215'N_510
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8) (coe v10) (coe v12)))
      C__'42'x'43'__496 v14 v15
        -> let v16
                 = d__'8799'N__570
                     (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                     (coe v7) (coe v8) (coe v10) (coe du_0N_688 (coe v4) (coe v8)) in
           case coe v16 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v17
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v18
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v19
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v18) in
                        let v20
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v19) in
                        let v21
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v20) in
                        let v22
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v21) in
                        let v23
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v22) in
                        let v24
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v23) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v24)))
                       (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                          (coe v5))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v14)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v15) (coe v12)))
                          (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                             (coe v5)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v14)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v15) (coe v12)))
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe v10) (coe v12)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v18
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v19
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v19) in
                           let v21
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v20) in
                           let v22
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v21) in
                           let v23
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v22) in
                           let v24
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v23) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v24)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12)))
                             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                (coe v5)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12)))
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v10) (coe v12)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12)))
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v10) (coe v12)))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                   (let v18
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                              (coe v5) in
                                    let v19
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe v18) in
                                    let v20
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe v19) in
                                    let v21
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                              (coe v20) in
                                    let v22
                                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                              (coe v21) in
                                    let v23
                                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                              (coe v22) in
                                    let v24
                                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                              (coe v23) in
                                    coe
                                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v24)))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v14)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15) (coe v12)))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v10) (coe v12))))
                          (coe
                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                           (coe v5))))))))))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v14)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15) (coe v12))))
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                      (coe
                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                            (coe v5)))))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v14)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15) (coe v12)))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v14)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15) (coe v12)))
                                (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                   (coe v5))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v10) (coe v12)))
                             (coe
                                d_0'8776''10214'0'10215'_938 (coe v0) (coe v1) (coe v2) (coe v3)
                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v10) (coe v17)
                                (coe v12))))
                       (coe
                          MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                          (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                               (coe
                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                  (coe
                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                     (coe v5))))))))))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12)))
                             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                (coe v5)))
                          (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                             (coe v5))
                          (let v18
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v19
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v19) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
                             (coe
                                MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
                                (coe v20))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))))))
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v17
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v18
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v17) in
                        let v19
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v18) in
                        let v20
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v19) in
                        let v21
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v20) in
                        let v22
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                        let v23
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v22) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'H_506
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8)
                                (coe
                                   d__'42'HN__774 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v10))
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             v11)
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8)
                             (coe
                                d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                (coe v6) (coe v7) (coe v8) (coe v15) (coe v10))
                             (coe v12)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v14)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v10) (coe v12)))
                             v11)
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v15) (coe v12))
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v10) (coe v12))))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v14)
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v15) (coe v12)))
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe v10) (coe v12)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v17
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v18
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v17) in
                           let v19
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v19) in
                           let v21
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v20) in
                           let v22
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                           let v23
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v22) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v10) (coe v12)))
                                v11)
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v10) (coe v12))))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12)))
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v10) (coe v12)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   v11)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12)))
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v10) (coe v12)))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                   (let v17
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                              (coe v5) in
                                    let v18
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe v17) in
                                    let v19
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe v18) in
                                    let v20
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                              (coe v19) in
                                    let v21
                                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                              (coe v20) in
                                    let v22
                                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                              (coe v21) in
                                    let v23
                                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                              (coe v22) in
                                    coe
                                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (d_'10214'_'10215'H_506
                                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                         (coe v6) (coe v7) (coe v8) (coe v14)
                                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                      v11)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v15) (coe v12)))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v10) (coe v12))))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8322'_282 (coe v5)
                             (coe
                                d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                             (coe
                                d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v15) (coe v12))
                             (coe
                                d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v10) (coe v12))
                             (coe v11)))
                       (coe
                          MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                          (coe
                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                             (coe
                                d_'42'HN'45'homo_1136 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v10) (coe v11)
                                (coe v12))
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                      (coe
                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                            (coe v5)))))
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8)
                                   (coe
                                      d__'42'HN__774 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v10))
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v10) (coe v12)))
                                v11 v11)
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                           (coe v5))))))))))
                                v11))
                          (coe
                             MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                             (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                               (coe
                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                  (coe
                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                     (coe v5)))))))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'H_506
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8)
                                   (coe
                                      d__'42'HN__774 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v10))
                                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                v11)
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (d_'10214'_'10215'H_506
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v14)
                                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v11 v12))
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v8) (coe v10) (coe v12)))
                                v11)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8)
                                (coe
                                   d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v15) (coe v10))
                                (coe v12))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v15) (coe v12))
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v10) (coe v12))))
                          (coe
                             d_'42'N'45'homo_1156 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe v15) (coe v10)
                             (coe v12))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.*H-homo
d_'42'H'45'homo_1146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_HNF_486 ->
  T_HNF_486 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'H'45'homo_1146 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  = case coe v9 of
      C_'8709'_492
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_sym_36
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                        (coe v5))))))))))
             (coe
                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                v5
                (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                   (coe v5))
                (d_'10214'_'10215'H_506
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8) (coe v10) (coe v11)))
             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                (coe v5))
             (let v13
                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                        (coe v5) in
              let v14
                    = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                        (coe v13) in
              let v15
                    = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v14) in
              coe
                MAlonzo.Code.Algebra.Structures.du_zero'737'_1192
                (coe
                   MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
                   (coe v15))
                (d_'10214'_'10215'H_506
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8) (coe v10) (coe v11)))
      C__'42'x'43'__496 v13 v14
        -> case coe v10 of
             C_'8709'_492
               -> coe
                    MAlonzo.Code.Relation.Binary.Structures.d_sym_36
                    (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                       (coe
                          MAlonzo.Code.Algebra.Structures.d_isMagma_444
                          (coe
                             MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                            (coe
                                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                               (coe v5))))))))))
                    (coe
                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                       v5
                       (d_'10214'_'10215'H_506
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8) (coe C__'42'x'43'__496 v13 v14) (coe v11))
                       (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                          (coe v5)))
                    (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                       (coe v5))
                    (let v16
                           = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                               (coe v5) in
                     let v17
                           = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                               (coe v16) in
                     let v18
                           = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v17) in
                     coe
                       MAlonzo.Code.Algebra.Structures.du_zero'691'_1194
                       (coe
                          MAlonzo.Code.Algebra.Structures.du_isSemiringWithoutOne_1462
                          (coe v18))
                       (d_'10214'_'10215'H_506
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8) (coe C__'42'x'43'__496 v13 v14) (coe v11)))
             C__'42'x'43'__496 v16 v17
               -> case coe v11 of
                    MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20
                      -> coe
                           MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                           (coe
                              MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                              (let v21
                                     = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                         (coe v5) in
                               let v22
                                     = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                         (coe v21) in
                               let v23
                                     = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                         (coe v22) in
                               let v24
                                     = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                         (coe v23) in
                               let v25
                                     = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                         (coe v24) in
                               let v26
                                     = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v25) in
                               let v27
                                     = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe v26) in
                               coe
                                 MAlonzo.Code.Algebra.Structures.du_setoid_164
                                 (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v27)))
                              (d_'10214'_'10215'H_506
                                 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                 (coe v7) (coe v8)
                                 (coe
                                    d__'42'x'43'HN__706 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                    (coe v5) (coe v6) (coe v7) (coe v8)
                                    (coe
                                       d__'42'x'43'H__756 (coe v0) (coe v1) (coe v2) (coe v3)
                                       (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                       (coe
                                          d__'42'H__778 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                          (coe v5) (coe v6) (coe v7) (coe v8) (coe v13) (coe v16))
                                       (coe
                                          d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                          (coe v5) (coe v6) (coe v7) (coe v8)
                                          (coe
                                             d__'42'HN__774 (coe v0) (coe v1) (coe v2) (coe v3)
                                             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                             (coe v17))
                                          (coe
                                             d__'42'NH__770 (coe v0) (coe v1) (coe v2) (coe v3)
                                             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                             (coe v16))))
                                    (coe
                                       d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                       (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v17)))
                                 (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                              (coe
                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                 v5
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                    v5
                                    (d_'10214'_'10215'H_506
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8)
                                       (coe
                                          d__'42'x'43'H__756 (coe v0) (coe v1) (coe v2) (coe v3)
                                          (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                          (coe
                                             d__'42'H__778 (coe v0) (coe v1) (coe v2) (coe v3)
                                             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                             (coe v16))
                                          (coe
                                             d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                             (coe
                                                d__'42'HN__774 (coe v0) (coe v1) (coe v2) (coe v3)
                                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe v13) (coe v17))
                                             (coe
                                                d__'42'NH__770 (coe v0) (coe v1) (coe v2) (coe v3)
                                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe v14) (coe v16))))
                                       (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                    v19)
                                 (d_'10214'_'10215'N_510
                                    (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                    (coe v7) (coe v8)
                                    (coe
                                       d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                       (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v17))
                                    (coe v20)))
                              (coe
                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                 v5
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (d_'10214'_'10215'H_506
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v13)
                                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                       v19)
                                    (d_'10214'_'10215'N_510
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8) (coe v14) (coe v20)))
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (d_'10214'_'10215'H_506
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v16)
                                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                       v19)
                                    (d_'10214'_'10215'N_510
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                              (coe
                                 MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                 (let v21
                                        = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                            (coe v5) in
                                  let v22
                                        = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                            (coe v21) in
                                  let v23
                                        = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                            (coe v22) in
                                  let v24
                                        = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                            (coe v23) in
                                  let v25
                                        = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                            (coe v24) in
                                  let v26
                                        = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe v25) in
                                  let v27
                                        = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                            (coe v26) in
                                  coe
                                    MAlonzo.Code.Algebra.Structures.du_setoid_164
                                    (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v27)))
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (d_'10214'_'10215'H_506
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8)
                                          (coe
                                             d__'42'x'43'H__756 (coe v0) (coe v1) (coe v2) (coe v3)
                                             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                             (coe
                                                d__'42'H__778 (coe v0) (coe v1) (coe v2) (coe v3)
                                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe v13) (coe v16))
                                             (coe
                                                d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe
                                                   d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                   (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                   (coe v8) (coe v13) (coe v17))
                                                (coe
                                                   d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                   (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                   (coe v8) (coe v14) (coe v16))))
                                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                       v19)
                                    (d_'10214'_'10215'N_510
                                       (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                       (coe v6) (coe v7) (coe v8)
                                       (coe
                                          d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                          (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v17))
                                       (coe v20)))
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe
                                                   d__'42'H__778 (coe v0) (coe v1) (coe v2) (coe v3)
                                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe v13) (coe v16))
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             v19)
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8)
                                             (coe
                                                d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe
                                                   d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                   (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                   (coe v8) (coe v13) (coe v17))
                                                (coe
                                                   d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                   (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                   (coe v8) (coe v14) (coe v16)))
                                             (coe
                                                MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20)))
                                       v19)
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v14) (coe v20))
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                 (coe
                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                    v5
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v13)
                                             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                          v19)
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v14) (coe v20)))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v16)
                                             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                          v19)
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                 (coe
                                    MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                    (let v21
                                           = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                               (coe v5) in
                                     let v22
                                           = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                               (coe v21) in
                                     let v23
                                           = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                               (coe v22) in
                                     let v24
                                           = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                               (coe v23) in
                                     let v25
                                           = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                               (coe v24) in
                                     let v26
                                           = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                               (coe v25) in
                                     let v27
                                           = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                               (coe v26) in
                                     coe
                                       MAlonzo.Code.Algebra.Structures.du_setoid_164
                                       (coe
                                          MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v27)))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'42'H__778 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v13) (coe v16))
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))
                                                v19)
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe
                                                   d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v13) (coe v17))
                                                   (coe
                                                      d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v14) (coe v16)))
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20)))
                                          v19)
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v14) (coe v20))
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20)))
                                                v19)
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v13) (coe v17))
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v14) (coe v16))
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))))
                                          v19)
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v14) (coe v20))
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                       v5
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             v19)
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v14) (coe v20)))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             v19)
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                    (coe
                                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                       (let v21
                                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                  (coe v5) in
                                        let v22
                                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                  (coe v21) in
                                        let v23
                                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                  (coe v22) in
                                        let v24
                                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                  (coe v23) in
                                        let v25
                                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                  (coe v24) in
                                        let v26
                                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                  (coe v25) in
                                        let v27
                                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                  (coe v26) in
                                        coe
                                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                                          (coe
                                             MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                             (coe v27)))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v13)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v16)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20)))
                                                   v19)
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v17))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v14) (coe v16))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))))
                                             v19)
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                (coe v20))
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                (coe v20))))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v13)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v16)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20)))
                                                   v19)
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v13)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (d_'10214'_'10215'N_510
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v17) (coe v20)))
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (d_'10214'_'10215'N_510
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v14) (coe v20))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v16)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20)))))
                                             v19)
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                (coe v20))
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                (coe v20))))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))
                                                v19)
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                (coe v20)))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))
                                                v19)
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                (coe v20))))
                                       (coe
                                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                                          (let v21
                                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                     (coe v5) in
                                           let v22
                                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                     (coe v21) in
                                           let v23
                                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                     (coe v22) in
                                           let v24
                                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                     (coe v23) in
                                           let v25
                                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                     (coe v24) in
                                           let v26
                                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                     (coe v25) in
                                           let v27
                                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                     (coe v26) in
                                           coe
                                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                (coe v27)))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))
                                                      v19)
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'N_510
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v17) (coe v20)))
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'N_510
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v14) (coe v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))))
                                                v19)
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                   (coe v20))
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                   (coe v20))))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   v19)
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                   (coe v20)))
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   v19)
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                   (coe v20))))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   v19)
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                   (coe v20)))
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v16)
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   v19)
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                   (coe v20))))
                                          (coe
                                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                                             (coe
                                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                (coe
                                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                                   (let v21
                                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                              (coe v5) in
                                                    let v22
                                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                              (coe v21) in
                                                    let v23
                                                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                              (coe v22) in
                                                    let v24
                                                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                              (coe v23) in
                                                    let v25
                                                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                              (coe v24) in
                                                    let v26
                                                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                              (coe v25) in
                                                    let v27
                                                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                              (coe v26) in
                                                    coe
                                                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                         (coe v27)))))
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v13)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      v19)
                                                   (d_'10214'_'10215'N_510
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                      (coe v20)))
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v16)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      v19)
                                                   (d_'10214'_'10215'N_510
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                      (coe v20)))))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8324'_326
                                             (coe v5)
                                             (coe
                                                d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2)
                                                (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                (coe v8) (coe v13)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             (coe
                                                d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2)
                                                (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                (coe v8) (coe v14) (coe v20))
                                             (coe
                                                d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2)
                                                (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                (coe v8) (coe v16)
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20))
                                             (coe
                                                d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2)
                                                (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                (coe v8) (coe v17) (coe v20))
                                             (coe v19)))
                                       (coe
                                          MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                          (coe
                                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                             (coe
                                                MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                (coe
                                                   MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                   (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                                     (coe
                                                                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                        (coe
                                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                           (coe
                                                                              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                              (coe v5))))))))))
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))
                                                      v19))
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                                   (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                     (coe
                                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                        (coe
                                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                           (coe v5)))))))))
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))
                                                      v19)
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))
                                                      v19)
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8)
                                                         (coe
                                                            d__'42'HN__774 (coe v0) (coe v1)
                                                            (coe v2) (coe v3) (coe v4) (coe v5)
                                                            (coe v6) (coe v7) (coe v8) (coe v13)
                                                            (coe v17))
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8)
                                                         (coe
                                                            d__'42'NH__770 (coe v0) (coe v1)
                                                            (coe v2) (coe v3) (coe v4) (coe v5)
                                                            (coe v6) (coe v7) (coe v8) (coe v14)
                                                            (coe v16))
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20)))
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'N_510
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v17) (coe v20)))
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'N_510
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v14) (coe v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))))
                                                (coe
                                                   MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                   (coe
                                                      d_'42'HN'45'homo_1136 (coe v0) (coe v1)
                                                      (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                                      (coe v7) (coe v8) (coe v13) (coe v17)
                                                      (coe v19) (coe v20))
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                                      (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                                     (coe
                                                                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                        (coe
                                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                           (coe
                                                                              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                              (coe v5)))))))))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8)
                                                         (coe
                                                            d__'42'HN__774 (coe v0) (coe v1)
                                                            (coe v2) (coe v3) (coe v4) (coe v5)
                                                            (coe v6) (coe v7) (coe v8) (coe v13)
                                                            (coe v17))
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'N_510
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v17) (coe v20)))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8)
                                                         (coe
                                                            d__'42'NH__770 (coe v0) (coe v1)
                                                            (coe v2) (coe v3) (coe v4) (coe v5)
                                                            (coe v6) (coe v7) (coe v8) (coe v14)
                                                            (coe v16))
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'N_510
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v14) (coe v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))))
                                                   (coe
                                                      d_'42'NH'45'homo_1124 (coe v0) (coe v1)
                                                      (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                                      (coe v7) (coe v8) (coe v14) (coe v16)
                                                      (coe v19) (coe v20))))
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                                (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                         (coe
                                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                            (coe v5)))))
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))
                                                      v19)
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8)
                                                         (coe
                                                            d__'42'HN__774 (coe v0) (coe v1)
                                                            (coe v2) (coe v3) (coe v4) (coe v5)
                                                            (coe v6) (coe v7) (coe v8) (coe v13)
                                                            (coe v17))
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8)
                                                         (coe
                                                            d__'42'NH__770 (coe v0) (coe v1)
                                                            (coe v2) (coe v3) (coe v4) (coe v5)
                                                            (coe v6) (coe v7) (coe v8) (coe v14)
                                                            (coe v16))
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))))
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))
                                                      v19)
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'N_510
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v17) (coe v20)))
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'N_510
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v14) (coe v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))))
                                                v19 v19)
                                             (coe
                                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                     (coe
                                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                        (coe
                                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                           (coe v5))))))))))
                                                v19))
                                          (coe
                                             MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                             (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                     (coe v5)))))))))
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))
                                                      v19)
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8)
                                                         (coe
                                                            d__'42'HN__774 (coe v0) (coe v1)
                                                            (coe v2) (coe v3) (coe v4) (coe v5)
                                                            (coe v6) (coe v7) (coe v8) (coe v13)
                                                            (coe v17))
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8)
                                                         (coe
                                                            d__'42'NH__770 (coe v0) (coe v1)
                                                            (coe v2) (coe v3) (coe v4) (coe v5)
                                                            (coe v6) (coe v7) (coe v8) (coe v14)
                                                            (coe v16))
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))))
                                                v19)
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))
                                                      v19)
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                      v5
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v13)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20))
                                                         (d_'10214'_'10215'N_510
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v17) (coe v20)))
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                         v5
                                                         (d_'10214'_'10215'N_510
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v14) (coe v20))
                                                         (d_'10214'_'10215'H_506
                                                            (coe v0) (coe v1) (coe v2) (coe v3)
                                                            (coe v4) (coe v5) (coe v6) (coe v7)
                                                            (coe v8) (coe v16)
                                                            (coe
                                                               MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                               v19 v20)))))
                                                v19)
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                   (coe v20))
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                   (coe v20)))
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                   (coe v20))
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                   (coe v20))))
                                          (coe
                                             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                     (coe
                                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                        (coe v5))))))))))
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                   (coe v20))
                                                (d_'10214'_'10215'N_510
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                   (coe v20))))))
                                    (coe
                                       MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                       (coe
                                          MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                          (coe
                                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                             (coe
                                                MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                                (coe
                                                   d_'42'H'45'homo_1146 (coe v0) (coe v1) (coe v2)
                                                   (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                   (coe v8) (coe v13) (coe v16)
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                                   (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                         (coe
                                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                            (coe
                                                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                               (coe v5)))))
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'H__778 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v16))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v13)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v16)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20)))
                                                   v19 v19)
                                                (coe
                                                   MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                                   (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                                     (coe
                                                                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                        (coe
                                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                           (coe
                                                                              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                              (coe v5))))))))))
                                                   v19))
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                                (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                     (coe
                                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                        (coe v5)))))))))
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'H__778 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v16))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   v19)
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v13)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v16)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20)))
                                                   v19)
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'43'H__728 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8)
                                                      (coe
                                                         d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v17))
                                                      (coe
                                                         d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v14) (coe v16)))
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v17))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v14) (coe v16))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))))
                                             (coe
                                                d_'43'H'45'homo_1040 (coe v0) (coe v1) (coe v2)
                                                (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                (coe v8)
                                                (coe
                                                   d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                   (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                   (coe v8) (coe v13) (coe v17))
                                                (coe
                                                   d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                   (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                   (coe v8) (coe v14) (coe v16))
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20)))
                                          (coe
                                             MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                             (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                      (coe
                                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                         (coe v5)))))
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'H__778 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v16))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   v19)
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'43'H__728 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8)
                                                      (coe
                                                         d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v17))
                                                      (coe
                                                         d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v14) (coe v16)))
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20)))
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v13)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v16)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20)))
                                                   v19)
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v17))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v14) (coe v16))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))))
                                             v19 v19)
                                          (coe
                                             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                     (coe
                                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                        (coe v5))))))))))
                                             v19))
                                       (coe
                                          MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                          (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                            (coe
                                                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                  (coe v5)))))))))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'H__778 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v16))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   v19)
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'43'H__728 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8)
                                                      (coe
                                                         d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v17))
                                                      (coe
                                                         d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v14) (coe v16)))
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20)))
                                             v19)
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                v5
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                   v5
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                      v5
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v13)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20))
                                                      (d_'10214'_'10215'H_506
                                                         (coe v0) (coe v1) (coe v2) (coe v3)
                                                         (coe v4) (coe v5) (coe v6) (coe v7)
                                                         (coe v8) (coe v16)
                                                         (coe
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                            v19 v20)))
                                                   v19)
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                                   v5
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v13) (coe v17))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))
                                                   (d_'10214'_'10215'H_506
                                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                      (coe v5) (coe v6) (coe v7) (coe v8)
                                                      (coe
                                                         d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                         (coe v3) (coe v4) (coe v5) (coe v6)
                                                         (coe v7) (coe v8) (coe v14) (coe v16))
                                                      (coe
                                                         MAlonzo.Code.Data.Vec.Base.C__'8759'__38
                                                         v19 v20))))
                                             v19)
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                (coe v20))
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                (coe v20)))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                (coe v20))
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                (coe v20))))
                                       (coe
                                          MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                          (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                     (coe v5))))))))))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                             v5
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                                (coe v20))
                                             (d_'10214'_'10215'N_510
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v17)
                                                (coe v20))))))
                                 (coe
                                    MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                    (coe
                                       MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                       (coe
                                          d_'42'x'43'H'45'homo_1094 (coe v0) (coe v1) (coe v2)
                                          (coe v3) (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                          (coe
                                             d__'42'H__778 (coe v0) (coe v1) (coe v2) (coe v3)
                                             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                             (coe v16))
                                          (coe
                                             d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                             (coe
                                                d__'42'HN__774 (coe v0) (coe v1) (coe v2) (coe v3)
                                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe v13) (coe v17))
                                             (coe
                                                d__'42'NH__770 (coe v0) (coe v1) (coe v2) (coe v3)
                                                (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe v14) (coe v16)))
                                          (coe v19) (coe v20))
                                       (coe
                                          MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                          (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                (coe
                                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                   (coe
                                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                      (coe v5)))))
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8)
                                             (coe
                                                d__'42'x'43'H__756 (coe v0) (coe v1) (coe v2)
                                                (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                (coe v8)
                                                (coe
                                                   d__'42'H__778 (coe v0) (coe v1) (coe v2) (coe v3)
                                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe v13) (coe v16))
                                                (coe
                                                   d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v13) (coe v17))
                                                   (coe
                                                      d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v14) (coe v16))))
                                             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'42'H__778 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v13) (coe v16))
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))
                                                v19)
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe
                                                   d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v13) (coe v17))
                                                   (coe
                                                      d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v14) (coe v16)))
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20)))
                                          v19 v19)
                                       (coe
                                          MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                          (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                         (coe
                                                            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                            (coe
                                                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                               (coe
                                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                                  (coe
                                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                                     (coe v5))))))))))
                                          v19))
                                    (coe
                                       MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                       (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                          (coe
                                             MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                             (coe
                                                MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                                (coe
                                                   MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                   (coe
                                                      MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                      (coe
                                                         MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                         (coe
                                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                            (coe
                                                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                               (coe v5)))))))))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (d_'10214'_'10215'H_506
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8)
                                             (coe
                                                d__'42'x'43'H__756 (coe v0) (coe v1) (coe v2)
                                                (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                (coe v8)
                                                (coe
                                                   d__'42'H__778 (coe v0) (coe v1) (coe v2) (coe v3)
                                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe v13) (coe v16))
                                                (coe
                                                   d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v13) (coe v17))
                                                   (coe
                                                      d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v14) (coe v16))))
                                             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20))
                                          v19)
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (coe
                                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                             v5
                                             (coe
                                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                                v5
                                                (d_'10214'_'10215'H_506
                                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                   (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'42'H__778 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v13) (coe v16))
                                                   (coe
                                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                      v20))
                                                v19)
                                             (d_'10214'_'10215'H_506
                                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                                (coe v5) (coe v6) (coe v7) (coe v8)
                                                (coe
                                                   d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3)
                                                   (coe v4) (coe v5) (coe v6) (coe v7) (coe v8)
                                                   (coe
                                                      d__'42'HN__774 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v13) (coe v17))
                                                   (coe
                                                      d__'42'NH__770 (coe v0) (coe v1) (coe v2)
                                                      (coe v3) (coe v4) (coe v5) (coe v6) (coe v7)
                                                      (coe v8) (coe v14) (coe v16)))
                                                (coe
                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19
                                                   v20)))
                                          v19)
                                       (d_'10214'_'10215'N_510
                                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                          (coe v6) (coe v7) (coe v8)
                                          (coe
                                             d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3)
                                             (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                             (coe v17))
                                          (coe v20))
                                       (coe
                                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                          v5
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v14) (coe v20))
                                          (d_'10214'_'10215'N_510
                                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                             (coe v6) (coe v7) (coe v8) (coe v17) (coe v20))))
                                    (coe
                                       d_'42'N'45'homo_1156 (coe v0) (coe v1) (coe v2) (coe v3)
                                       (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                       (coe v17) (coe v20))))
                              (d_'42'x'43'HN'8776''42'x'43'_964
                                 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                 (coe v7) (coe v8)
                                 (coe
                                    d__'42'x'43'H__756 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                    (coe v5) (coe v6) (coe v7) (coe v8)
                                    (coe
                                       d__'42'H__778 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                       (coe v5) (coe v6) (coe v7) (coe v8) (coe v13) (coe v16))
                                    (coe
                                       d__'43'H__728 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                       (coe v5) (coe v6) (coe v7) (coe v8)
                                       (coe
                                          d__'42'HN__774 (coe v0) (coe v1) (coe v2) (coe v3)
                                          (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)
                                          (coe v17))
                                       (coe
                                          d__'42'NH__770 (coe v0) (coe v1) (coe v2) (coe v3)
                                          (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v14)
                                          (coe v16))))
                                 (coe
                                    d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                    (coe v5) (coe v6) (coe v7) (coe v8) (coe v14) (coe v17))
                                 (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v19 v20)))
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.*N-homo
d_'42'N'45'homo_1156 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Normal_488 ->
  T_Normal_488 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'42'N'45'homo_1156 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  = case coe v9 of
      C_con_498 v12
        -> case coe v10 of
             C_con_498 v13
               -> coe
                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'42''45'homo_756
                    v6 v12 v13
             _ -> MAlonzo.RTE.mazUnreachableError
      C_poly_502 v13
        -> let v14 = subInt (coe v8) (coe (1 :: Integer)) in
           case coe v10 of
             C_poly_502 v16
               -> coe
                    d_'42'H'45'homo_1146 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                    (coe v5) (coe v6) (coe v7) (coe v14) (coe v13) (coe v16) (coe v11)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.^N-homo
d_'94'N'45'homo_1282 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Normal_488 ->
  Integer -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'94'N'45'homo_1282 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  = case coe v10 of
      0 -> coe
             d_1N'45'homo_950 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
             (coe v5) (coe v6) (coe v7) (coe v8) (coe v11)
      _ -> let v12 = subInt (coe v10) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v13
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                           (coe v5) in
                 let v14
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe v13) in
                 let v15
                       = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v14) in
                 let v16
                       = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                           (coe v15) in
                 let v17
                       = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                           (coe v16) in
                 let v18
                       = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v17) in
                 let v19
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v18) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v19)))
                (d_'10214'_'10215'N_510
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8)
                   (coe
                      d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                      (coe v6) (coe v7) (coe v8) (coe v9)
                      (coe
                         d__'94'N__854 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                         (coe v6) (coe v7) (coe v8) (coe v9) (coe v12)))
                   (coe v11))
                (coe
                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                   v5
                   (d_'10214'_'10215'N_510
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v8) (coe v9) (coe v11))
                   (d_'10214'_'10215'N_510
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v8)
                      (coe
                         d__'94'N__854 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                         (coe v6) (coe v7) (coe v8) (coe v9) (coe v12))
                      (coe v11)))
                (coe
                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                   v5
                   (d_'10214'_'10215'N_510
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v8) (coe v9) (coe v11))
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                      (coe
                         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                               (coe
                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                                  (coe v5)))))
                      (coe
                         d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                         (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v11))
                      (coe v12)))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   (let v13
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                              (coe v5) in
                    let v14
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe v13) in
                    let v15
                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v14) in
                    let v16
                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                              (coe v15) in
                    let v17
                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                              (coe v16) in
                    let v18
                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v17) in
                    let v19
                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v18) in
                    coe
                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v19)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                      v5
                      (d_'10214'_'10215'N_510
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8) (coe v9) (coe v11))
                      (d_'10214'_'10215'N_510
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8)
                         (coe
                            d__'94'N__854 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                            (coe v6) (coe v7) (coe v8) (coe v9) (coe v12))
                         (coe v11)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                      v5
                      (d_'10214'_'10215'N_510
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8) (coe v9) (coe v11))
                      (coe
                         MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                                     (coe v5)))))
                         (coe
                            d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                            (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v11))
                         (coe v12)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                      v5
                      (d_'10214'_'10215'N_510
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8) (coe v9) (coe v11))
                      (coe
                         MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                                     (coe v5)))))
                         (coe
                            d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                            (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v11))
                         (coe v12)))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                            (let v13
                                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                       (coe v5) in
                             let v14
                                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                       (coe v13) in
                             let v15
                                   = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v14) in
                             let v16
                                   = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                       (coe v15) in
                             let v17
                                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                       (coe v16) in
                             let v18
                                   = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v17) in
                             let v19
                                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v18) in
                             coe
                               MAlonzo.Code.Algebra.Structures.du_setoid_164
                               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v19)))))
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                         v5
                         (d_'10214'_'10215'N_510
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v9) (coe v11))
                         (coe
                            MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                  (coe
                                     MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                                        (coe v5)))))
                            (coe
                               d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                               (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v11))
                            (coe v12))))
                   (coe
                      MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isMagma_444
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                  (coe
                                     MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                     (coe
                                        MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                        (coe
                                           MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                           (coe
                                              MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe
                                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                 (coe
                                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                    (coe v5))))))))))
                         (d_'10214'_'10215'N_510
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v9) (coe v11)))
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                         (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                               (coe
                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5)))))
                         (d_'10214'_'10215'N_510
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v9) (coe v11))
                         (d_'10214'_'10215'N_510
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v9) (coe v11))
                         (d_'10214'_'10215'N_510
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8)
                            (coe
                               d__'94'N__854 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                               (coe v6) (coe v7) (coe v8) (coe v9) (coe v12))
                            (coe v11))
                         (let v13
                                = coe
                                    MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                    (coe
                                       MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                                       (coe v5)) in
                          coe
                            MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                                  (coe v13)))
                            (coe
                               d_'10214'_'10215'N_510 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                               (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v11))
                            (coe v12)))
                      (coe
                         d_'94'N'45'homo_1282 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                         (coe v5) (coe v6) (coe v7) (coe v8) (coe v9) (coe v12) (coe v11))))
                (d_'42'N'45'homo_1156
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8) (coe v9)
                   (coe
                      d__'94'N__854 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                      (coe v6) (coe v7) (coe v8) (coe v9) (coe v12))
                   (coe v11)))
-- Algebra.Solver.Ring.-H‿-homo
d_'45'H'8255''45'homo_1300 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_HNF_486 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'45'H'8255''45'homo_1300 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v10 of
      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v14
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                           (coe v5) in
                 let v15
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe v14) in
                 let v16
                       = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
                 let v17
                       = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                           (coe v16) in
                 let v18
                       = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                           (coe v17) in
                 let v19
                       = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                 let v20
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v19) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))
                (d_'10214'_'10215'H_506
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8)
                   (coe
                      d__'42'NH__770 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                      (coe v5) (coe v6) (coe v7) (coe v8)
                      (coe
                         d_'45'N__868 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                         (coe v6) (coe v7) (coe v8)
                         (coe
                            d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                            (coe v6) (coe v7) (coe v8)))
                      (coe v9))
                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13))
                (coe
                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                   v5
                   (d_'10214'_'10215'N_510
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v8)
                      (coe
                         d_'45'N__868 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                         (coe v6) (coe v7) (coe v8)
                         (coe
                            d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                            (coe v6) (coe v7) (coe v8)))
                      (coe v13))
                   (d_'10214'_'10215'H_506
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v8) (coe v9)
                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13)))
                (coe
                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                   v5
                   (d_'10214'_'10215'H_506
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v8) (coe v9)
                      (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13)))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   (let v14
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                              (coe v5) in
                    let v15
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe v14) in
                    let v16
                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
                    let v17
                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                              (coe v16) in
                    let v18
                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                              (coe v17) in
                    let v19
                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                    let v20
                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v19) in
                    coe
                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                      v5
                      (d_'10214'_'10215'N_510
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8)
                         (coe
                            d_'45'N__868 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                            (coe v6) (coe v7) (coe v8)
                            (coe
                               d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                               (coe v6) (coe v7) (coe v8)))
                         (coe v13))
                      (d_'10214'_'10215'H_506
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8) (coe v9)
                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                      v5
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                         v5
                         (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                            (coe v5)))
                      (d_'10214'_'10215'H_506
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8) (coe v9)
                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                      v5
                      (d_'10214'_'10215'H_506
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8) (coe v9)
                         (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13)))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                      (let v14
                             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                 (coe v5) in
                       let v15
                             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                 (coe v14) in
                       let v16
                             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
                       let v17
                             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                 (coe v16) in
                       let v18
                             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                 (coe v17) in
                       let v19
                             = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                       let v20
                             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v19) in
                       coe
                         MAlonzo.Code.Algebra.Structures.du_setoid_164
                         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                         v5
                         (coe
                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                            v5
                            (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                               (coe v5)))
                         (d_'10214'_'10215'H_506
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v9)
                            (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13)))
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                         v5
                         (d_'10214'_'10215'H_506
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v9)
                            (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13)))
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                         v5
                         (d_'10214'_'10215'H_506
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v9)
                            (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13)))
                      (coe
                         MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                         (coe
                            MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                            (coe
                               MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                               (let v14
                                      = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                          (coe v5) in
                                let v15
                                      = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                          (coe v14) in
                                let v16
                                      = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                          (coe v15) in
                                let v17
                                      = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                          (coe v16) in
                                let v18
                                      = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                          (coe v17) in
                                let v19
                                      = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                                let v20
                                      = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                          (coe v19) in
                                coe
                                  MAlonzo.Code.Algebra.Structures.du_setoid_164
                                  (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))))
                         (coe
                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                            v5
                            (d_'10214'_'10215'H_506
                               (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                               (coe v7) (coe v8) (coe v9)
                               (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13))))
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8327'_366 (coe v5)
                         (coe
                            d_'10214'_'10215'H_506 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                            (coe v5) (coe v6) (coe v7) (coe v8) (coe v9)
                            (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13))))
                   (coe
                      MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_trans_38
                         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isMagma_444
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                  (coe
                                     MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                     (coe
                                        MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                        (coe
                                           MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                           (coe
                                              MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe
                                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                 (coe
                                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                    (coe v5))))))))))
                         (d_'10214'_'10215'N_510
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8)
                            (coe
                               d_'45'N__868 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                               (coe v6) (coe v7) (coe v8)
                               (coe
                                  d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                  (coe v6) (coe v7) (coe v8)))
                            (coe v13))
                         (coe
                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                            v5
                            (d_'10214'_'10215'N_510
                               (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                               (coe v7) (coe v8)
                               (coe
                                  d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                  (coe v6) (coe v7) (coe v8))
                               (coe v13)))
                         (coe
                            MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                            (\ v14 v15 -> v15)
                            (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                               (coe v5))
                            (d_'10214'_'10215'N_510
                               (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                               (coe v7) (coe v8)
                               (coe
                                  d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                  (coe v6) (coe v7) (coe v8))
                               (coe v13))
                            (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                               (coe v5)))
                         (d_'45'N'8255''45'homo_1308
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8)
                            (coe
                               d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                               (coe v6) (coe v7) (coe v8))
                            (coe v13))
                         (coe
                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45''8255'cong_64
                            (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                               (coe v5))
                            (d_'10214'_'10215'N_510
                               (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                               (coe v7) (coe v8)
                               (coe
                                  d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                  (coe v6) (coe v7) (coe v8))
                               (coe v13))
                            (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                               (coe v5))
                            (d_1N'45'homo_950
                               (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                               (coe v7) (coe v8) (coe v13))))
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                         (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                               (coe
                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5)))))
                         (d_'10214'_'10215'N_510
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8)
                            (coe
                               d_'45'N__868 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                               (coe v6) (coe v7) (coe v8)
                               (coe
                                  d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                  (coe v6) (coe v7) (coe v8)))
                            (coe v13))
                         (coe
                            MAlonzo.Code.Function.Base.du_'8739'_'10217''45'__298
                            (\ v14 v15 -> v15)
                            (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                               (coe v5))
                            (d_'10214'_'10215'N_510
                               (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                               (coe v7) (coe v8)
                               (coe
                                  d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                  (coe v6) (coe v7) (coe v8))
                               (coe v13))
                            (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                               (coe v5)))
                         (d_'10214'_'10215'H_506
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v9)
                            (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13))
                         (d_'10214'_'10215'H_506
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v9)
                            (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13)))
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isMagma_444
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                  (coe
                                     MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                     (coe
                                        MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                        (coe
                                           MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                           (coe
                                              MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe
                                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                 (coe
                                                    MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                    (coe v5))))))))))
                         (d_'10214'_'10215'H_506
                            (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                            (coe v7) (coe v8) (coe v9)
                            (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13)))))
                (d_'42'NH'45'homo_1124
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8)
                   (coe
                      d_'45'N__868 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                      (coe v6) (coe v7) (coe v8)
                      (coe
                         d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                         (coe v6) (coe v7) (coe v8)))
                   (coe v9) (coe v12) (coe v13)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.-N‿-homo
d_'45'N'8255''45'homo_1308 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Normal_488 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_'45'N'8255''45'homo_1308 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_con_498 v11
        -> coe
             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45''8255'homo_758
             v6 v11
      C_poly_502 v12
        -> let v13 = subInt (coe v8) (coe (1 :: Integer)) in
           coe
             d_'45'H'8255''45'homo_1300 (coe v0) (coe v1) (coe v2) (coe v3)
             (coe v4) (coe v5) (coe v6) (coe v7) (coe v13) (coe v12) (coe v10)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.correct-con
d_correct'45'con_1328 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  AgdaAny -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_correct'45'con_1328 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v10 of
      MAlonzo.Code.Data.Vec.Base.C_'91''93'_32
        -> coe
             MAlonzo.Code.Relation.Binary.Structures.d_refl_34
             (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                (coe
                   MAlonzo.Code.Algebra.Structures.d_isMagma_444
                   (coe
                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                      (coe
                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                         (coe
                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                            (coe
                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                               (coe
                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe
                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                        (coe v5))))))))))
             (d_'10214'_'10215'N_510
                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                (coe v7) (coe (0 :: Integer))
                (coe
                   d_normalise'45'con_878 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                   (coe v5) (coe v6) (coe v7) (coe (0 :: Integer)) (coe v9))
                (coe v10))
      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13
        -> let v14 = subInt (coe v8) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v15
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                           (coe v5) in
                 let v16
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe v15) in
                 let v17
                       = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v16) in
                 let v18
                       = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                           (coe v17) in
                 let v19
                       = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                           (coe v18) in
                 let v20
                       = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v19) in
                 let v21
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v20) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v21)))
                (d_'10214'_'10215'H_506
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v14)
                   (coe
                      d__'42'x'43'HN__706 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                      (coe v5) (coe v6) (coe v7) (coe v14) (coe C_'8709'_492)
                      (coe
                         d_normalise'45'con_878 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                         (coe v5) (coe v6) (coe v7) (coe v14) (coe v9)))
                   (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13))
                (d_'10214'_'10215'N_510
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v14)
                   (coe
                      d_normalise'45'con_878 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                      (coe v5) (coe v6) (coe v7) (coe v14) (coe v9))
                   (coe v13))
                (coe
                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'10214'_'10215'_752
                   v6 v9)
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   (let v15
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                              (coe v5) in
                    let v16
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe v15) in
                    let v17
                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v16) in
                    let v18
                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                              (coe v17) in
                    let v19
                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                              (coe v18) in
                    let v20
                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v19) in
                    let v21
                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v20) in
                    coe
                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v21)))
                   (d_'10214'_'10215'N_510
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v14)
                      (coe
                         d_normalise'45'con_878 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                         (coe v5) (coe v6) (coe v7) (coe v14) (coe v9))
                      (coe v13))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'10214'_'10215'_752
                      v6 v9)
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'10214'_'10215'_752
                      v6 v9)
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                            (let v15
                                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                       (coe v5) in
                             let v16
                                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                       (coe v15) in
                             let v17
                                   = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v16) in
                             let v18
                                   = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                       (coe v17) in
                             let v19
                                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                       (coe v18) in
                             let v20
                                   = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v19) in
                             let v21
                                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v20) in
                             coe
                               MAlonzo.Code.Algebra.Structures.du_setoid_164
                               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v21)))))
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'10214'_'10215'_752
                         v6 v9))
                   (d_correct'45'con_1328
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v14) (coe v9) (coe v13)))
                (d_'8709''42'x'43'HN'45'homo_1006
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v14)
                   (coe
                      d_normalise'45'con_878 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                      (coe v5) (coe v6) (coe v7) (coe v14) (coe v9))
                   (coe v12) (coe v13)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.correct-var
d_correct'45'var_1344 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  MAlonzo.Code.Data.Fin.Base.T_Fin_10 ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_correct'45'var_1344 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      MAlonzo.Code.Data.Fin.Base.C_zero_12
        -> let v12 = subInt (coe v8) (coe (1 :: Integer)) in
           case coe v10 of
             MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v14 v15
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v16
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v17
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v16) in
                        let v18
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v17) in
                        let v19
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v18) in
                        let v20
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v19) in
                        let v21
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v20) in
                        let v22
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v21) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v22)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                      (coe v5))
                                   v14)
                                (d_'10214'_'10215'N_510
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v12)
                                   (coe
                                      d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                                      (coe v6) (coe v7) (coe v12))
                                   (coe v15)))
                             v14)
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v12) (coe du_0N_688 (coe v4) (coe v12)) (coe v15)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                   v5
                                   (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                      (coe v5))
                                   v14)
                                (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                                   (coe v5)))
                             v14)
                          (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                             (coe v5)))
                       v14
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v16
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v17
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v16) in
                           let v18
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v17) in
                           let v19
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v19) in
                           let v21
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v20) in
                           let v22
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v21) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v22)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                         (coe v5))
                                      v14)
                                   (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                                      (coe v5)))
                                v14)
                             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                (coe v5)))
                          v14 v14
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                   (let v16
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                              (coe v5) in
                                    let v17
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe v16) in
                                    let v18
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe v17) in
                                    let v19
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                              (coe v18) in
                                    let v20
                                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                              (coe v19) in
                                    let v21
                                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                              (coe v20) in
                                    let v22
                                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                              (coe v21) in
                                    coe
                                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v22)))))
                             (coe v14))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.Lemmas.du_lemma'8325'_350 (coe v5)
                             (coe v14)))
                       (coe
                          MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                          (coe
                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                             (coe
                                MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                                (coe
                                   MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                   (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                     (coe
                                                        MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                           (coe
                                                              MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                              (coe v5))))))))))
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                         (coe v5))
                                      v14))
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                   (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                           (coe v5)))))))))
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                         (coe v5))
                                      v14)
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                         (coe v5))
                                      v14)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v12)
                                      (coe
                                         d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                         (coe v5) (coe v6) (coe v7) (coe v12))
                                      (coe v15))
                                   (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                                      (coe v5)))
                                (coe
                                   d_1N'45'homo_950 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                   (coe v5) (coe v6) (coe v7) (coe v12) (coe v15)))
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                      (coe
                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                            (coe v5)))))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                         (coe v5))
                                      v14)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v12)
                                      (coe
                                         d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                         (coe v5) (coe v6) (coe v7) (coe v12))
                                      (coe v15)))
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                         (coe v5))
                                      v14)
                                   (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                                      (coe v5)))
                                v14 v14)
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                                  (coe
                                                     MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                        (coe
                                                           MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                           (coe v5))))))))))
                                v14))
                          (coe
                             MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                             (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                (coe
                                   MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                               (coe
                                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                  (coe
                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                     (coe v5)))))))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                         (coe v5))
                                      v14)
                                   (d_'10214'_'10215'N_510
                                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                      (coe v7) (coe v12)
                                      (coe
                                         d_1N_698 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                         (coe v5) (coe v6) (coe v7) (coe v12))
                                      (coe v15)))
                                v14)
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe
                                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                   v5
                                   (coe
                                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                      v5
                                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                         (coe v5))
                                      v14)
                                   (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_1'35'_212
                                      (coe v5)))
                                v14)
                             (d_'10214'_'10215'N_510
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v12) (coe du_0N_688 (coe v4) (coe v12)) (coe v15))
                             (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_0'35'_210
                                (coe v5)))
                          (coe
                             d_0N'45'homo_926 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v12) (coe v15))))
             _ -> MAlonzo.RTE.mazUnreachableError
      MAlonzo.Code.Data.Fin.Base.C_suc_16 v12
        -> let v13 = subInt (coe v8) (coe (1 :: Integer)) in
           case coe v10 of
             MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v15 v16
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v17
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v18
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v17) in
                        let v19
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v18) in
                        let v20
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v19) in
                        let v21
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v20) in
                        let v22
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                        let v23
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v22) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))
                       (d_'10214'_'10215'H_506
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v13)
                          (coe
                             d__'42'x'43'HN__706 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v13) (coe C_'8709'_492)
                             (coe
                                d_normalise'45'var_888 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v13) (coe v12)))
                          (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v15 v16))
                       (d_'10214'_'10215'N_510
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v13)
                          (coe
                             d_normalise'45'var_888 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v13) (coe v12))
                          (coe v16))
                       (coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v16) (coe v12))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v17
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v18
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v17) in
                           let v19
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v19) in
                           let v21
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v20) in
                           let v22
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v21) in
                           let v23
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v22) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))
                          (d_'10214'_'10215'N_510
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v13)
                             (coe
                                d_normalise'45'var_888 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v13) (coe v12))
                             (coe v16))
                          (coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v16) (coe v12))
                          (coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v16) (coe v12))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                   (let v17
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                              (coe v5) in
                                    let v18
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe v17) in
                                    let v19
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe v18) in
                                    let v20
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                              (coe v19) in
                                    let v21
                                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                              (coe v20) in
                                    let v22
                                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                              (coe v21) in
                                    let v23
                                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                              (coe v22) in
                                    coe
                                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v23)))))
                             (coe MAlonzo.Code.Data.Vec.Base.du_lookup_82 (coe v16) (coe v12)))
                          (d_correct'45'var_1344
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v13) (coe v12) (coe v16)))
                       (d_'8709''42'x'43'HN'45'homo_1006
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v13)
                          (coe
                             d_normalise'45'var_888 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v13) (coe v12))
                          (coe v15) (coe v16)))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring.correct
d_correct_1362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Polynomial_398 -> MAlonzo.Code.Data.Vec.Base.T_Vec_28 -> AgdaAny
d_correct_1362 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  = case coe v9 of
      C_op_408 v11 v12 v13
        -> case coe v11 of
             C_'91''43''93'_392
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v14
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v15
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v14) in
                        let v16
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
                        let v17
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v16) in
                        let v18
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v17) in
                        let v19
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                        let v20
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v19) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))
                       (d_'10214'_'10215'N_510
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d__'43'N__732 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                             (coe v6) (coe v7) (coe v8)
                             (coe
                                d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v12))
                             (coe
                                d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)))
                          (coe v10))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (d_'10214'_'10215''8595'_916
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe v12) (coe v10))
                          (d_'10214'_'10215''8595'_916
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe v13) (coe v10)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                          v5
                          (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v12) (coe v10))
                          (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v13) (coe v10)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v14
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v15
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v14) in
                           let v16
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
                           let v17
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v16) in
                           let v18
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v17) in
                           let v19
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v19) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (d_'10214'_'10215''8595'_916
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v12) (coe v10))
                             (d_'10214'_'10215''8595'_916
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v13) (coe v10)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v12) (coe v10))
                             (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v13) (coe v10)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                             v5
                             (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v12) (coe v10))
                             (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v13) (coe v10)))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                   (let v14
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                              (coe v5) in
                                    let v15
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe v14) in
                                    let v16
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe v15) in
                                    let v17
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                              (coe v16) in
                                    let v18
                                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                              (coe v17) in
                                    let v19
                                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                              (coe v18) in
                                    let v20
                                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                              (coe v19) in
                                    coe
                                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'43'__204
                                v5
                                (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v12) (coe v10))
                                (coe
                                   du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v13) (coe v10))))
                          (coe
                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                             (coe
                                d_correct_1362 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v12) (coe v10))
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'8729''45'cong_150
                                (MAlonzo.Code.Algebra.Structures.d_isMagma_444
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                         (coe
                                            MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                            (coe
                                               MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                               (coe
                                                  MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                                  (coe
                                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                                     (coe
                                                        MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                                        (coe v5)))))))))
                                (d_'10214'_'10215''8595'_916
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v12) (coe v10))
                                (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v12) (coe v10))
                                (d_'10214'_'10215''8595'_916
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v13) (coe v10))
                                (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v13) (coe v10)))
                             (coe
                                d_correct_1362 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v13) (coe v10))))
                       (d_'43'N'45'homo_1050
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe v12))
                          (coe
                             d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe v13))
                          (coe v10)))
             C_'91''42''93'_394
               -> coe
                    MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
                    (coe
                       MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                       (let v14
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                  (coe v5) in
                        let v15
                              = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                  (coe v14) in
                        let v16
                              = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
                        let v17
                              = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                  (coe v16) in
                        let v18
                              = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                  (coe v17) in
                        let v19
                              = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                        let v20
                              = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v19) in
                        coe
                          MAlonzo.Code.Algebra.Structures.du_setoid_164
                          (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))
                       (d_'10214'_'10215'N_510
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d__'42'N__782 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                             (coe v6) (coe v7) (coe v8)
                             (coe
                                d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v12))
                             (coe
                                d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v13)))
                          (coe v10))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                          v5
                          (d_'10214'_'10215''8595'_916
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe v12) (coe v10))
                          (d_'10214'_'10215''8595'_916
                             (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                             (coe v7) (coe v8) (coe v13) (coe v10)))
                       (coe
                          MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                          v5
                          (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v12) (coe v10))
                          (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v13) (coe v10)))
                       (coe
                          MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                          (let v14
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                     (coe v5) in
                           let v15
                                 = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                     (coe v14) in
                           let v16
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v15) in
                           let v17
                                 = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                     (coe v16) in
                           let v18
                                 = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                     (coe v17) in
                           let v19
                                 = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v18) in
                           let v20
                                 = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v19) in
                           coe
                             MAlonzo.Code.Algebra.Structures.du_setoid_164
                             (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (d_'10214'_'10215''8595'_916
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v12) (coe v10))
                             (d_'10214'_'10215''8595'_916
                                (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                (coe v7) (coe v8) (coe v13) (coe v10)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v12) (coe v10))
                             (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v13) (coe v10)))
                          (coe
                             MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                             v5
                             (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v12) (coe v10))
                             (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v13) (coe v10)))
                          (coe
                             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                             (coe
                                MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                                (coe
                                   MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                                   (let v14
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                              (coe v5) in
                                    let v15
                                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                              (coe v14) in
                                    let v16
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                              (coe v15) in
                                    let v17
                                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                              (coe v16) in
                                    let v18
                                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                              (coe v17) in
                                    let v19
                                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                                              (coe v18) in
                                    let v20
                                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
                                              (coe v19) in
                                    coe
                                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                                      (coe
                                         MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v20)))))
                             (coe
                                MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d__'42'__206
                                v5
                                (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v12) (coe v10))
                                (coe
                                   du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v13) (coe v10))))
                          (coe
                             MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                             (coe
                                d_correct_1362 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v12) (coe v10))
                             (coe
                                MAlonzo.Code.Algebra.Structures.d_'42''45'cong_1292
                                (MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                   (coe
                                      MAlonzo.Code.Algebra.Structures.d_isSemiring_1494
                                      (coe
                                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                         (coe
                                            MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                            (coe v5)))))
                                (d_'10214'_'10215''8595'_916
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v12) (coe v10))
                                (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v12) (coe v10))
                                (d_'10214'_'10215''8595'_916
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                                   (coe v7) (coe v8) (coe v13) (coe v10))
                                (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v13) (coe v10)))
                             (coe
                                d_correct_1362 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                                (coe v5) (coe v6) (coe v7) (coe v8) (coe v13) (coe v10))))
                       (d_'42'N'45'homo_1156
                          (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                          (coe v7) (coe v8)
                          (coe
                             d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe v12))
                          (coe
                             d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                             (coe v5) (coe v6) (coe v7) (coe v8) (coe v13))
                          (coe v10)))
             _ -> MAlonzo.RTE.mazUnreachableError
      C_con_412 v11
        -> coe
             d_correct'45'con_1328 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
             (coe v5) (coe v6) (coe v7) (coe v8) (coe v11) (coe v10)
      C_var_416 v11
        -> coe
             d_correct'45'var_1344 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
             (coe v5) (coe v6) (coe v7) (coe v8) (coe v11) (coe v10)
      C__'58''94'__422 v11 v12
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v13
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                           (coe v5) in
                 let v14
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe v13) in
                 let v15
                       = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v14) in
                 let v16
                       = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                           (coe v15) in
                 let v17
                       = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                           (coe v16) in
                 let v18
                       = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v17) in
                 let v19
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v18) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v19)))
                (d_'10214'_'10215'N_510
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8)
                   (coe
                      d__'94'N__854 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                      (coe v6) (coe v7) (coe v8)
                      (coe
                         d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                         (coe v5) (coe v6) (coe v7) (coe v8) (coe v11))
                      (coe v12))
                   (coe v10))
                (coe
                   MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                   (coe
                      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                      (coe
                         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                            (coe
                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                               (coe v5)))))
                   (coe
                      d_'10214'_'10215''8595'_916 (coe v0) (coe v1) (coe v2) (coe v3)
                      (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v11) (coe v10))
                   (coe v12))
                (coe
                   MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                   (coe
                      MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                      (coe
                         MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                            (coe
                               MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                               (coe v5)))))
                   (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v11) (coe v10))
                   (coe v12))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   (let v13
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                              (coe v5) in
                    let v14
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe v13) in
                    let v15
                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v14) in
                    let v16
                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                              (coe v15) in
                    let v17
                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                              (coe v16) in
                    let v18
                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v17) in
                    let v19
                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v18) in
                    coe
                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v19)))
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                      (coe
                         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                               (coe
                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                                  (coe v5)))))
                      (coe
                         d_'10214'_'10215''8595'_916 (coe v0) (coe v1) (coe v2) (coe v3)
                         (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v11) (coe v10))
                      (coe v12))
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                      (coe
                         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                               (coe
                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                                  (coe v5)))))
                      (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v11) (coe v10))
                      (coe v12))
                   (coe
                      MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                      (coe
                         MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                               (coe
                                  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                                  (coe v5)))))
                      (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v11) (coe v10))
                      (coe v12))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                            (let v13
                                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                       (coe v5) in
                             let v14
                                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                       (coe v13) in
                             let v15
                                   = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v14) in
                             let v16
                                   = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                       (coe v15) in
                             let v17
                                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                       (coe v16) in
                             let v18
                                   = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v17) in
                             let v19
                                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v18) in
                             coe
                               MAlonzo.Code.Algebra.Structures.du_setoid_164
                               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v19)))))
                      (coe
                         MAlonzo.Code.Algebra.Definitions.RawSemiring.du__'94'__70
                         (coe
                            MAlonzo.Code.Algebra.Bundles.du_rawSemiring_1942
                            (coe
                               MAlonzo.Code.Algebra.Bundles.du_semiringWithoutAnnihilatingZero_2104
                               (coe
                                  MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                                  (coe
                                     MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                                     (coe v5)))))
                         (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v11) (coe v10))
                         (coe v12)))
                   (coe
                      MAlonzo.Code.Function.Base.du__'10216'_'10217'__240
                      (coe
                         d_correct_1362 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                         (coe v5) (coe v6) (coe v7) (coe v8) (coe v11) (coe v10))
                      (\ v13 v14 ->
                         coe
                           MAlonzo.Code.Algebra.Properties.Semiring.Exp.du_'94''45'cong_200
                           (coe
                              MAlonzo.Code.Algebra.Bundles.du_semiring_2282
                              (coe
                                 MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.du_commutativeSemiring_320
                                 (coe v5)))
                           (coe
                              d_'10214'_'10215''8595'_916 (coe v0) (coe v1) (coe v2) (coe v3)
                              (coe v4) (coe v5) (coe v6) (coe v7) (coe v8) (coe v11) (coe v10))
                           (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v11) (coe v10))
                           (coe v12) v13)
                      erased))
                (d_'94'N'45'homo_1282
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8)
                   (coe
                      d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v11))
                   (coe v12) (coe v10)))
      C_'58''45'__426 v11
        -> coe
             MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.d_begin__40
             (coe
                MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                (let v12
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                           (coe v5) in
                 let v13
                       = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                           (coe v12) in
                 let v14
                       = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v13) in
                 let v15
                       = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                           (coe v14) in
                 let v16
                       = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                           (coe v15) in
                 let v17
                       = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v16) in
                 let v18
                       = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v17) in
                 coe
                   MAlonzo.Code.Algebra.Structures.du_setoid_164
                   (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v18)))
                (d_'10214'_'10215'N_510
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8)
                   (coe
                      d_'45'N__868 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
                      (coe v6) (coe v7) (coe v8)
                      (coe
                         d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                         (coe v5) (coe v6) (coe v7) (coe v8) (coe v11)))
                   (coe v10))
                (coe
                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                   v5
                   (d_'10214'_'10215''8595'_916
                      (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                      (coe v7) (coe v8) (coe v11) (coe v10)))
                (coe
                   MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                   v5
                   (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v11) (coe v10)))
                (coe
                   MAlonzo.Code.Relation.Binary.Reasoning.Setoid.du_step'45''8776'_58
                   (let v12
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                              (coe v5) in
                    let v13
                          = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                              (coe v12) in
                    let v14
                          = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v13) in
                    let v15
                          = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                              (coe v14) in
                    let v16
                          = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                              (coe v15) in
                    let v17
                          = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v16) in
                    let v18
                          = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v17) in
                    coe
                      MAlonzo.Code.Algebra.Structures.du_setoid_164
                      (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v18)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                      v5
                      (d_'10214'_'10215''8595'_916
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8) (coe v11) (coe v10)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                      v5
                      (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v11) (coe v10)))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                      v5
                      (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v11) (coe v10)))
                   (coe
                      MAlonzo.Code.Relation.Binary.Reasoning.Base.Single.du__'8718'_86
                      (coe
                         MAlonzo.Code.Relation.Binary.Structures.d_refl_34
                         (coe
                            MAlonzo.Code.Relation.Binary.Bundles.d_isEquivalence_60
                            (let v12
                                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                                       (coe v5) in
                             let v13
                                   = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                                       (coe v12) in
                             let v14
                                   = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v13) in
                             let v15
                                   = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                                       (coe v14) in
                             let v16
                                   = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                                       (coe v15) in
                             let v17
                                   = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v16) in
                             let v18
                                   = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v17) in
                             coe
                               MAlonzo.Code.Algebra.Structures.du_setoid_164
                               (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v18)))))
                      (coe
                         MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45'__208
                         v5
                         (coe
                            du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v11) (coe v10))))
                   (coe
                      MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_'45''8255'cong_64
                      (MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                         (coe v5))
                      (d_'10214'_'10215''8595'_916
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8) (coe v11) (coe v10))
                      (coe du_'10214'_'10215'_458 (coe v5) (coe v6) (coe v11) (coe v10))
                      (d_correct_1362
                         (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                         (coe v7) (coe v8) (coe v11) (coe v10))))
                (d_'45'N'8255''45'homo_1308
                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v5) (coe v6)
                   (coe v7) (coe v8)
                   (coe
                      d_normalise_894 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
                      (coe v5) (coe v6) (coe v7) (coe v8) (coe v11))
                   (coe v10)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Algebra.Solver.Ring._._⊜_
d__'8860'__1396 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  T_Polynomial_398 ->
  T_Polynomial_398 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d__'8860'__1396 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 = du__'8860'__1396
du__'8860'__1396 ::
  Integer ->
  T_Polynomial_398 ->
  T_Polynomial_398 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du__'8860'__1396 v0
  = coe MAlonzo.Code.Relation.Binary.Reflection.du__'8860'__142
-- Algebra.Solver.Ring._.prove
d_prove_1398 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer ->
  MAlonzo.Code.Data.Vec.Base.T_Vec_28 ->
  T_Polynomial_398 -> T_Polynomial_398 -> AgdaAny -> AgdaAny
d_prove_1398 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      MAlonzo.Code.Relation.Binary.Reflection.du_prove_90
      (let v8
             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                 (coe v5) in
       let v9
             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                 (coe v8) in
       let v10
             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v9) in
       let v11
             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                 (coe v10) in
       let v12
             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                 (coe v11) in
       let v13
             = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v12) in
       let v14
             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v13) in
       coe
         MAlonzo.Code.Algebra.Structures.du_setoid_164
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v14)))
      (\ v8 v9 v10 ->
         coe du_'10214'_'10215'_458 (coe v5) (coe v6) v9 v10)
      (coe
         d_'10214'_'10215''8595'_916 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe v4) (coe v5) (coe v6) (coe v7))
      (coe
         d_correct_1362 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
         (coe v5) (coe v6) (coe v7))
-- Algebra.Solver.Ring._.solve
d_solve_1400 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Algebra.Bundles.Raw.T_RawRing_242 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T_AlmostCommutativeRing_178 ->
  MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.T__'45'Raw'45'AlmostCommutative'10230'__358 ->
  (AgdaAny -> AgdaAny -> Maybe AgdaAny) ->
  Integer -> AgdaAny -> AgdaAny -> AgdaAny
d_solve_1400 v0 v1 v2 v3 v4 v5 v6 v7
  = coe
      MAlonzo.Code.Relation.Binary.Reflection.du_solve_114
      (let v8
             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isAlmostCommutativeRing_214
                 (coe v5) in
       let v9
             = MAlonzo.Code.Algebra.Solver.Ring.AlmostCommutativeRing.d_isCommutativeSemiring_62
                 (coe v8) in
       let v10
             = MAlonzo.Code.Algebra.Structures.d_isSemiring_1494 (coe v9) in
       let v11
             = MAlonzo.Code.Algebra.Structures.d_isSemiringWithoutAnnihilatingZero_1386
                 (coe v10) in
       let v12
             = MAlonzo.Code.Algebra.Structures.d_'43''45'isCommutativeMonoid_1290
                 (coe v11) in
       let v13
             = MAlonzo.Code.Algebra.Structures.d_isMonoid_660 (coe v12) in
       let v14
             = MAlonzo.Code.Algebra.Structures.d_isSemigroup_610 (coe v13) in
       coe
         MAlonzo.Code.Algebra.Structures.du_setoid_164
         (coe MAlonzo.Code.Algebra.Structures.d_isMagma_444 (coe v14)))
      (coe (\ v8 -> coe C_var_416))
      (\ v8 v9 v10 ->
         coe du_'10214'_'10215'_458 (coe v5) (coe v6) v9 v10)
      (coe
         d_'10214'_'10215''8595'_916 (coe v0) (coe v1) (coe v2) (coe v3)
         (coe v4) (coe v5) (coe v6) (coe v7))
      (coe
         d_correct_1362 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4)
         (coe v5) (coe v6) (coe v7))
