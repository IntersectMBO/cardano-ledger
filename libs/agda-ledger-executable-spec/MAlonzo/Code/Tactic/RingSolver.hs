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

module MAlonzo.Code.Tactic.RingSolver where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Bool.Base
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Reflection
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Nat.Reflection
import qualified MAlonzo.Code.Data.Vec.Base
import qualified MAlonzo.Code.Data.Vec.Reflection
import qualified MAlonzo.Code.Reflection.AST.AlphaEquality
import qualified MAlonzo.Code.Reflection.AST.Term
import qualified MAlonzo.Code.Tactic.RingSolver.Core.NatSet

-- Tactic.RingSolver.VarMap
d_VarMap_4 :: ()
d_VarMap_4 = erased
-- Tactic.RingSolver.getVisible
d_getVisible_6 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88 ->
  Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_getVisible_6 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v1 v2
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v3 v4
               -> let v5 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
                  case coe v3 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                      -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v2)
                    _ -> coe v5
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.getVisibleArgs
d_getVisibleArgs_12 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Maybe MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_getVisibleArgs_12 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v3 v4
        -> coe
             MAlonzo.Code.Data.Maybe.Base.du_map_68
             (coe MAlonzo.Code.Data.Vec.Base.du_reverse_654)
             (coe
                MAlonzo.Code.Data.List.Base.du_foldl_256 (coe du_f_26)
                (coe du_c_42)
                (coe
                   MAlonzo.Code.Data.List.Base.du_mapMaybe_32 (coe d_getVisible_6)
                   (coe v4))
                v0)
      _ -> coe v2
-- Tactic.RingSolver._.f
d_f_26 ::
  Integer ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  (Integer -> Maybe MAlonzo.Code.Data.Vec.Base.T_Vec_28) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer -> Maybe MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_f_26 ~v0 ~v1 ~v2 v3 v4 v5 = du_f_26 v3 v4 v5
du_f_26 ::
  (Integer -> Maybe MAlonzo.Code.Data.Vec.Base.T_Vec_28) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer -> Maybe MAlonzo.Code.Data.Vec.Base.T_Vec_28
du_f_26 v0 v1 v2
  = case coe v2 of
      0 -> coe
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
             (coe MAlonzo.Code.Data.Vec.Base.C_'91''93'_32)
      _ -> let v3 = subInt (coe v2) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Data.Maybe.Base.du_map_68
             (coe MAlonzo.Code.Data.Vec.Base.C__'8759'__38 (coe v1)) (coe v0 v3)
-- Tactic.RingSolver._.c
d_c_42 ::
  Integer ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  Integer -> Maybe MAlonzo.Code.Data.Vec.Base.T_Vec_28
d_c_42 ~v0 ~v1 ~v2 v3 = du_c_42 v3
du_c_42 :: Integer -> Maybe MAlonzo.Code.Data.Vec.Base.T_Vec_28
du_c_42 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
             (coe MAlonzo.Code.Data.Vec.Base.C_'91''93'_32)
      _ -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
-- Tactic.RingSolver.curriedTerm
d_curriedTerm_44 ::
  [Integer] -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_curriedTerm_44 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_foldr_242 (coe d_go_50)
      (coe MAlonzo.Code.Data.Vec.Reflection.d_'96''91''93'_12)
      (coe
         MAlonzo.Code.Tactic.RingSolver.Core.NatSet.d_toList_218 (coe v0))
-- Tactic.RingSolver._.go
d_go_50 ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_go_50 v0 v1
  = coe
      MAlonzo.Code.Data.Vec.Reflection.d__'96''8759'__14
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v0)
         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
      (coe v1)
-- Tactic.RingSolver.`AlmostCommutativeRing
d_'96'AlmostCommutativeRing_56 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'AlmostCommutativeRing_56
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
      (coe
         (MAlonzo.RTE.QName
            (178 :: Integer) (11528919355260747559 :: Integer)
            "Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
      (let v0 = 2 :: Integer in
       let v1 = coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16 in
       case coe v0 of
         0 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
         _ -> let v2 = 1 :: Integer in
              coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                (coe
                   MAlonzo.Code.Reflection.AST.Term.d__'8943''10216''8759''10217'__70
                   (coe v2) (coe v1)))
-- Tactic.RingSolver.RingOperatorTerms
d_RingOperatorTerms_58 = ()
data T_RingOperatorTerms_58
  = C_add'8658'_mul'8658'_pow'8658'_neg'8658'_sub'8658'__80 MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
                                                            MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
                                                            MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
                                                            MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
                                                            MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
-- Tactic.RingSolver.RingOperatorTerms.add
d_add_70 ::
  T_RingOperatorTerms_58 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_add_70 v0
  = case coe v0 of
      C_add'8658'_mul'8658'_pow'8658'_neg'8658'_sub'8658'__80 v1 v2 v3 v4 v5
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.RingOperatorTerms.mul
d_mul_72 ::
  T_RingOperatorTerms_58 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_mul_72 v0
  = case coe v0 of
      C_add'8658'_mul'8658'_pow'8658'_neg'8658'_sub'8658'__80 v1 v2 v3 v4 v5
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.RingOperatorTerms.pow
d_pow_74 ::
  T_RingOperatorTerms_58 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_pow_74 v0
  = case coe v0 of
      C_add'8658'_mul'8658'_pow'8658'_neg'8658'_sub'8658'__80 v1 v2 v3 v4 v5
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.RingOperatorTerms.neg
d_neg_76 ::
  T_RingOperatorTerms_58 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_neg_76 v0
  = case coe v0 of
      C_add'8658'_mul'8658'_pow'8658'_neg'8658'_sub'8658'__80 v1 v2 v3 v4 v5
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.RingOperatorTerms.sub
d_sub_78 ::
  T_RingOperatorTerms_58 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_sub_78 v0
  = case coe v0 of
      C_add'8658'_mul'8658'_pow'8658'_neg'8658'_sub'8658'__80 v1 v2 v3 v4 v5
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Tactic.RingSolver.checkIsRing
d_checkIsRing_82 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_checkIsRing_82 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_checkType_338 v0
      d_'96'AlmostCommutativeRing_56
-- Tactic.RingSolver.RingReflection._$ʳ_
d__'36''691'__90 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d__'36''691'__90 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v1)
      (let v3 = 2 :: Integer in
       let v4
             = coe
                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                       (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                    (coe v0))
                 (coe v2) in
       case coe v3 of
         0 -> coe v4
         _ -> let v5 = 1 :: Integer in
              coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                (coe
                   MAlonzo.Code.Reflection.AST.Term.d__'8943''10181''8759''10182'__78
                   (coe v5) (coe v4)))
-- Tactic.RingSolver.RingReflection.`Carrier
d_'96'Carrier_96 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'Carrier_96 v0
  = coe
      d__'36''691'__90 (coe v0)
      (coe
         (MAlonzo.RTE.QName
            (204 :: Integer) (11528919355260747559 :: Integer)
            "Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.Carrier"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Tactic.RingSolver.RingReflection.`refl
d_'96'refl_98 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'refl_98 v0
  = coe
      d__'36''691'__90 (coe v0)
      (coe
         (MAlonzo.RTE.QName
            (358 :: Integer) (11528919355260747559 :: Integer)
            "Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.refl"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
      (let v1 = 1 :: Integer in
       let v2 = coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16 in
       case coe v1 of
         0 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
         _ -> let v3 = 0 :: Integer in
              coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                (coe
                   MAlonzo.Code.Reflection.AST.Term.d__'8943''10181''8759''10182'__78
                   (coe v3) (coe v2)))
-- Tactic.RingSolver.RingReflection.`sym
d_'96'sym_100 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'sym_100 v0 v1
  = coe
      d__'36''691'__90 (coe v0)
      (coe
         (MAlonzo.RTE.QName
            (316 :: Integer) (11528919355260747559 :: Integer)
            "Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.sym"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
      (let v2 = 2 :: Integer in
       let v3
             = coe
                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                       (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                    (coe v1))
                 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) in
       case coe v2 of
         0 -> coe v3
         _ -> let v4 = 1 :: Integer in
              coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                (coe
                   MAlonzo.Code.Reflection.AST.Term.d__'8943''10181''8759''10182'__78
                   (coe v4) (coe v3)))
-- Tactic.RingSolver.RingReflection.`trans
d_'96'trans_104 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'trans_104 v0 v1 v2
  = coe
      d__'36''691'__90 (coe v0)
      (coe
         (MAlonzo.RTE.QName
            (318 :: Integer) (11528919355260747559 :: Integer)
            "Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._.trans"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
      (let v3 = 3 :: Integer in
       let v4
             = coe
                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                       (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                    (coe v1))
                 (coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                       (coe v2))
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)) in
       case coe v3 of
         0 -> coe v4
         _ -> let v5 = 2 :: Integer in
              coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                (coe
                   MAlonzo.Code.Reflection.AST.Term.d__'8943''10181''8759''10182'__78
                   (coe v5) (coe v4)))
-- Tactic.RingSolver.RingReflection.getRingOperatorTerms
d_getRingOperatorTerms_110 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_getRingOperatorTerms_110 v0
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
         erased
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
            erased
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
               erased
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                  erased
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                     (coe C_add'8658'_mul'8658'_pow'8658'_neg'8658'_sub'8658'__80))
                  (\ v1 ->
                     coe
                       MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                       erased
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340
                          (d__'36''691'__90
                             (coe v0)
                             (coe
                                (MAlonzo.RTE.QName
                                   (208 :: Integer) (11528919355260747559 :: Integer)
                                   "Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._+_"
                                   (MAlonzo.RTE.Fixity
                                      MAlonzo.RTE.LeftAssoc (MAlonzo.RTE.Related (6.0 :: Double)))))
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                       (\ v2 ->
                          coe
                            MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                            (coe v1 v2))))
               (\ v1 ->
                  coe
                    MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                    erased
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340
                       (d__'36''691'__90
                          (coe v0)
                          (coe
                             (MAlonzo.RTE.QName
                                (210 :: Integer) (11528919355260747559 :: Integer)
                                "Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._*_"
                                (MAlonzo.RTE.Fixity
                                   MAlonzo.RTE.LeftAssoc (MAlonzo.RTE.Related (7.0 :: Double)))))
                          (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                    (\ v2 ->
                       coe
                         MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                         (coe v1 v2))))
            (\ v1 ->
               coe
                 MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                 erased
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340
                    (d__'36''691'__90
                       (coe v0)
                       (coe
                          (MAlonzo.RTE.QName
                             (348 :: Integer) (11528919355260747559 :: Integer)
                             "Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._^_"
                             (MAlonzo.RTE.Fixity
                                MAlonzo.RTE.RightAssoc (MAlonzo.RTE.Related (8.0 :: Double)))))
                       (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                 (\ v2 ->
                    coe
                      MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                      (coe v1 v2))))
         (\ v1 ->
            coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
              erased
              (coe
                 MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340
                 (d__'36''691'__90
                    (coe v0)
                    (coe
                       (MAlonzo.RTE.QName
                          (212 :: Integer) (11528919355260747559 :: Integer)
                          "Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing.-_"
                          (MAlonzo.RTE.Fixity
                             MAlonzo.RTE.NonAssoc (MAlonzo.RTE.Related (8.0 :: Double)))))
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
              (\ v2 ->
                 coe
                   MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                   (coe v1 v2))))
      (\ v1 ->
         coe
           MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
           erased
           (coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340
              (d__'36''691'__90
                 (coe v0)
                 (coe
                    (MAlonzo.RTE.QName
                       (350 :: Integer) (11528919355260747559 :: Integer)
                       "Tactic.RingSolver.Core.AlmostCommutativeRing.AlmostCommutativeRing._-_"
                       (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
                 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
           (\ v2 ->
              coe
                MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                (coe v1 v2)))
-- Tactic.RingSolver.RingSolverReflection._._$ʳ_
d__'36''691'__120 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d__'36''691'__120 v0 ~v1 = du__'36''691'__120 v0
du__'36''691'__120 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du__'36''691'__120 v0 = coe d__'36''691'__90 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._.`Carrier
d_'96'Carrier_122 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'Carrier_122 v0 ~v1 = du_'96'Carrier_122 v0
du_'96'Carrier_122 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_'96'Carrier_122 v0 = coe d_'96'Carrier_96 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._.`refl
d_'96'refl_124 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'refl_124 v0 ~v1 = du_'96'refl_124 v0
du_'96'refl_124 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_'96'refl_124 v0 = coe d_'96'refl_98 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._.`sym
d_'96'sym_126 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'sym_126 v0 ~v1 = du_'96'sym_126 v0
du_'96'sym_126 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_'96'sym_126 v0 = coe d_'96'sym_100 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._.`trans
d_'96'trans_128 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'trans_128 v0 ~v1 = du_'96'trans_128 v0
du_'96'trans_128 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_'96'trans_128 v0 = coe d_'96'trans_104 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._.getRingOperatorTerms
d_getRingOperatorTerms_130 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer -> AgdaAny
d_getRingOperatorTerms_130 v0 ~v1 = du_getRingOperatorTerms_130 v0
du_getRingOperatorTerms_130 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_getRingOperatorTerms_130 v0
  = coe d_getRingOperatorTerms_110 (coe v0)
-- Tactic.RingSolver.RingSolverReflection.`numberOfVariables
d_'96'numberOfVariables_132 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'numberOfVariables_132 ~v0 v1
  = du_'96'numberOfVariables_132 v1
du_'96'numberOfVariables_132 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_'96'numberOfVariables_132 v0
  = coe MAlonzo.Code.Data.Nat.Reflection.d_toTerm_14 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._$ᵉ_
d__'36''7497'__134 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d__'36''7497'__134 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 (coe v2)
      (let v4 = 1 :: Integer in
       let v5
             = coe
                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                       (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                    (coe d_'96'Carrier_96 (coe v0)))
                 (coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                       (coe du_'96'numberOfVariables_132 (coe v1)))
                    (coe v3)) in
       case coe v4 of
         0 -> coe v5
         _ -> let v6 = 0 :: Integer in
              coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                (coe
                   MAlonzo.Code.Reflection.AST.Term.d__'8943''10181''8759''10182'__78
                   (coe v6) (coe v5)))
-- Tactic.RingSolver.RingSolverReflection.`Κ
d_'96'Κ_140 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'Κ_140 v0 v1 v2
  = coe
      d__'36''7497'__134 (coe v0) (coe v1)
      (coe
         (MAlonzo.RTE.QName
            (22 :: Integer) (4058621091251856968 :: Integer)
            "Tactic.RingSolver.Core.Expression.Expr.\922"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
      (coe
         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
               (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
            (coe v2))
         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
-- Tactic.RingSolver.RingSolverReflection.`I
d_'96'I_144 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'I_144 v0 v1 v2
  = coe
      d__'36''7497'__134 (coe v0) (coe v1)
      (coe
         (MAlonzo.RTE.QName
            (24 :: Integer) (4058621091251856968 :: Integer)
            "Tactic.RingSolver.Core.Expression.Expr.\921"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
      (coe
         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
               (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
            (coe v2))
         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
-- Tactic.RingSolver.RingSolverReflection._`⊜_
d__'96''8860'__148 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d__'96''8860'__148 v0 v1 v2 v3
  = coe
      d__'36''691'__90 (coe v0)
      (coe
         (MAlonzo.RTE.QName
            (480 :: Integer) (13355546136547469431 :: Integer)
            "Tactic.RingSolver.NonReflective._\8860_"
            (MAlonzo.RTE.Fixity
               MAlonzo.RTE.LeftAssoc (MAlonzo.RTE.Related (6.0 :: Double)))))
      (coe
         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
               (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
            (coe du_'96'numberOfVariables_132 (coe v1)))
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
               (coe v2))
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                     (coe
                        MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                        (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                        (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                  (coe v3))
               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
-- Tactic.RingSolver.RingSolverReflection.`correct
d_'96'correct_154 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'correct_154 v0 ~v1 v2 v3 = du_'96'correct_154 v0 v2 v3
du_'96'correct_154 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_'96'correct_154 v0 v1 v2
  = coe
      d__'36''691'__90 (coe v0)
      (coe
         (MAlonzo.RTE.QName
            (402 :: Integer) (13355546136547469431 :: Integer)
            "Tactic.RingSolver.NonReflective.Ops.correct"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
      (let v3 = 1 :: Integer in
       let v4
             = coe
                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                       (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                    (coe v1))
                 (coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                       (coe v2))
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)) in
       case coe v3 of
         0 -> coe v4
         _ -> let v5 = 0 :: Integer in
              coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                (coe
                   MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                   (coe
                      MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                (coe
                   MAlonzo.Code.Reflection.AST.Term.d__'8943''10181''8759''10182'__78
                   (coe v5) (coe v4)))
-- Tactic.RingSolver.RingSolverReflection.`solver
d_'96'solver_160 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'solver_160 v0 v1 v2 v3
  = coe
      d__'36''691'__90 (coe v0)
      (coe
         (MAlonzo.RTE.QName
            (476 :: Integer) (13355546136547469431 :: Integer)
            "Tactic.RingSolver.NonReflective.solve"
            (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
      (coe
         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
               (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
            (coe du_'96'numberOfVariables_132 (coe v1)))
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
               (coe v2))
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                  (coe
                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                     (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                     (coe
                        MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                        (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                        (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                  (coe v3))
               (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
-- Tactic.RingSolver.RingSolverReflection.convertTerm
d_convertTerm_166 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_convertTerm_166 v0 v1 v2 v3
  = coe d_convert_188 (coe v0) (coe v1) (coe v2) (coe v3)
-- Tactic.RingSolver.RingSolverReflection._._.add
d_add_178 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_add_178 ~v0 ~v1 v2 ~v3 = du_add_178 v2
du_add_178 ::
  T_RingOperatorTerms_58 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_add_178 v0 = coe d_add_70 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._._.mul
d_mul_180 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_mul_180 ~v0 ~v1 v2 ~v3 = du_mul_180 v2
du_mul_180 ::
  T_RingOperatorTerms_58 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_mul_180 v0 = coe d_mul_72 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._._.neg
d_neg_182 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_neg_182 ~v0 ~v1 v2 ~v3 = du_neg_182 v2
du_neg_182 ::
  T_RingOperatorTerms_58 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_neg_182 v0 = coe d_neg_76 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._._.pow
d_pow_184 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_pow_184 ~v0 ~v1 v2 ~v3 = du_pow_184 v2
du_pow_184 ::
  T_RingOperatorTerms_58 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_pow_184 v0 = coe d_pow_74 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._._.sub
d_sub_186 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_sub_186 ~v0 ~v1 v2 ~v3 = du_sub_186 v2
du_sub_186 ::
  T_RingOperatorTerms_58 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_sub_186 v0 = coe d_sub_78 (coe v0)
-- Tactic.RingSolver.RingSolverReflection._.convert
d_convert_188 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_convert_188 v0 v1 v2 v3 v4
  = let v5
          = coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
              (d_'96'Κ_140 (coe v0) (coe v1) (coe v4)) in
    case coe v4 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v6 v7
        -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
             (coe
                MAlonzo.Code.Data.Maybe.Base.du_fromMaybe_50
                (d_'96'Κ_140 (coe v0) (coe v1) (coe v4)) (coe v3 v6))
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v6 v7
        -> case coe v6 of
             MAlonzo.RTE.QName 12 13537827747504913145 _ _
               -> case coe v7 of
                    (:) v8 v9
                      -> case coe v8 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v10 v11
                             -> case coe v10 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v12 v13
                                    -> case coe v12 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                           -> case coe v13 of
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v14 v15
                                                  -> case coe v14 of
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                         -> case coe v15 of
                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                -> case coe v9 of
                                                                     []
                                                                       -> coe
                                                                            d_convertSuc_200
                                                                            (coe v0) (coe v1)
                                                                            (coe v2) (coe v3)
                                                                            (coe v11)
                                                                     _ -> coe v5
                                                              _ -> coe v5
                                                       _ -> coe v5
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> coe v5
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> coe v5
             _ -> coe v5
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 v6 v7
        -> let v8
                 = d_convertUnknownName_198
                     (coe v0) (coe v1) (coe v2) (coe v3) (coe v6) (coe v7) in
           case coe v6 of
             MAlonzo.RTE.QName 208 11528919355260747559 _ _
               -> coe
                    d_convertOp'8322'_190 (coe v0) (coe v1) (coe v2) (coe v3)
                    (coe
                       (MAlonzo.RTE.QName
                          (26 :: Integer) (4058621091251856968 :: Integer)
                          "Tactic.RingSolver.Core.Expression.Expr._\8853_"
                          (MAlonzo.RTE.Fixity
                             MAlonzo.RTE.LeftAssoc (MAlonzo.RTE.Related (6.0 :: Double)))))
                    (coe v7)
             MAlonzo.RTE.QName 210 11528919355260747559 _ _
               -> coe
                    d_convertOp'8322'_190 (coe v0) (coe v1) (coe v2) (coe v3)
                    (coe
                       (MAlonzo.RTE.QName
                          (28 :: Integer) (4058621091251856968 :: Integer)
                          "Tactic.RingSolver.Core.Expression.Expr._\8855_"
                          (MAlonzo.RTE.Fixity
                             MAlonzo.RTE.LeftAssoc (MAlonzo.RTE.Related (7.0 :: Double)))))
                    (coe v7)
             MAlonzo.RTE.QName 212 11528919355260747559 _ _
               -> coe
                    d_convertOp'8321'_192 (coe v0) (coe v1) (coe v2) (coe v3)
                    (coe
                       (MAlonzo.RTE.QName
                          (32 :: Integer) (4058621091251856968 :: Integer)
                          "Tactic.RingSolver.Core.Expression.Expr.\8861_"
                          (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
                    (coe v7)
             MAlonzo.RTE.QName 348 11528919355260747559 _ _
               -> coe
                    d_convertExp_194 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7)
             MAlonzo.RTE.QName 350 11528919355260747559 _ _
               -> coe
                    d_convertSub_196 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7)
             _ -> coe v8
      _ -> coe v5
-- Tactic.RingSolver.RingSolverReflection._.convertOp₂
d_convertOp'8322'_190 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_convertOp'8322'_190 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208) in
    case coe v5 of
      (:) v7 v8
        -> case coe v7 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v9 v10
               -> case coe v9 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v11 v12
                      -> let v13
                               = d_convertOp'8322'_190
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v8) in
                         case coe v11 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                             -> case coe v12 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v14 v15
                                    -> case coe v14 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                           -> case coe v15 of
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                  -> case coe v8 of
                                                       (:) v16 v17
                                                         -> case coe v16 of
                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v18 v19
                                                                -> case coe v18 of
                                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v20 v21
                                                                       -> case coe v20 of
                                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                                                              -> case coe v21 of
                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v22 v23
                                                                                     -> case coe
                                                                                               v22 of
                                                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                                                            -> case coe
                                                                                                      v23 of
                                                                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                                                   -> case coe
                                                                                                             v17 of
                                                                                                        []
                                                                                                          -> coe
                                                                                                               MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                                                                               ()
                                                                                                               ()
                                                                                                               erased
                                                                                                               erased
                                                                                                               (d_convert_188
                                                                                                                  (coe
                                                                                                                     v0)
                                                                                                                  (coe
                                                                                                                     v1)
                                                                                                                  (coe
                                                                                                                     v2)
                                                                                                                  (coe
                                                                                                                     v3)
                                                                                                                  (coe
                                                                                                                     v10))
                                                                                                               (\ v24 ->
                                                                                                                  coe
                                                                                                                    MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                                                                                    ()
                                                                                                                    ()
                                                                                                                    erased
                                                                                                                    erased
                                                                                                                    (d_convert_188
                                                                                                                       (coe
                                                                                                                          v0)
                                                                                                                       (coe
                                                                                                                          v1)
                                                                                                                       (coe
                                                                                                                          v2)
                                                                                                                       (coe
                                                                                                                          v3)
                                                                                                                       (coe
                                                                                                                          v19))
                                                                                                                    (\ v25 ->
                                                                                                                       coe
                                                                                                                         MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316
                                                                                                                         ()
                                                                                                                         erased
                                                                                                                         (d__'36''7497'__134
                                                                                                                            (coe
                                                                                                                               v0)
                                                                                                                            (coe
                                                                                                                               v1)
                                                                                                                            (coe
                                                                                                                               v4)
                                                                                                                            (coe
                                                                                                                               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                               (coe
                                                                                                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                  (coe
                                                                                                                                     v18)
                                                                                                                                  (coe
                                                                                                                                     v24))
                                                                                                                               (coe
                                                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                                  (coe
                                                                                                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                     (coe
                                                                                                                                        v18)
                                                                                                                                     (coe
                                                                                                                                        v25))
                                                                                                                                  (coe
                                                                                                                                     v17))))))
                                                                                                        _ -> coe
                                                                                                               v13
                                                                                                 _ -> coe
                                                                                                        v13
                                                                                          _ -> coe
                                                                                                 v13
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            _ -> coe v13
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> coe v13
                                                _ -> coe v13
                                         _ -> coe v13
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> coe v13
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> coe v6
-- Tactic.RingSolver.RingSolverReflection._.convertOp₁
d_convertOp'8321'_192 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_convertOp'8321'_192 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208) in
    case coe v5 of
      (:) v7 v8
        -> case coe v7 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v9 v10
               -> case coe v9 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v11 v12
                      -> let v13
                               = d_convertOp'8321'_192
                                   (coe v0) (coe v1) (coe v2) (coe v3) (coe v4) (coe v8) in
                         case coe v11 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                             -> case coe v12 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v14 v15
                                    -> case coe v14 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                           -> case coe v15 of
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                  -> case coe v8 of
                                                       []
                                                         -> coe
                                                              MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                              () () erased erased
                                                              (d_convert_188
                                                                 (coe v0) (coe v1) (coe v2) (coe v3)
                                                                 (coe v10))
                                                              (\ v16 ->
                                                                 coe
                                                                   MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316
                                                                   () erased
                                                                   (d__'36''7497'__134
                                                                      (coe v0) (coe v1) (coe v4)
                                                                      (coe
                                                                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                         (coe
                                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                            (coe v9) (coe v16))
                                                                         (coe v8))))
                                                       _ -> coe v13
                                                _ -> coe v13
                                         _ -> coe v13
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> coe v13
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> coe v6
-- Tactic.RingSolver.RingSolverReflection._.convertExp
d_convertExp_194 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_convertExp_194 v0 v1 v2 v3 v4
  = let v5
          = coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208) in
    case coe v4 of
      (:) v6 v7
        -> case coe v6 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v8 v9
               -> case coe v8 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v10 v11
                      -> let v12
                               = d_convertExp_194 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7) in
                         case coe v10 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                             -> case coe v11 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v13 v14
                                    -> case coe v13 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                           -> case coe v14 of
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                  -> case coe v7 of
                                                       (:) v15 v16
                                                         -> case coe v15 of
                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v17 v18
                                                                -> case coe v17 of
                                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v19 v20
                                                                       -> case coe v19 of
                                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                                                              -> case coe v20 of
                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v21 v22
                                                                                     -> case coe
                                                                                               v21 of
                                                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                                                            -> case coe
                                                                                                      v22 of
                                                                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                                                   -> case coe
                                                                                                             v16 of
                                                                                                        []
                                                                                                          -> coe
                                                                                                               MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                                                                               ()
                                                                                                               ()
                                                                                                               erased
                                                                                                               erased
                                                                                                               (d_convert_188
                                                                                                                  (coe
                                                                                                                     v0)
                                                                                                                  (coe
                                                                                                                     v1)
                                                                                                                  (coe
                                                                                                                     v2)
                                                                                                                  (coe
                                                                                                                     v3)
                                                                                                                  (coe
                                                                                                                     v9))
                                                                                                               (\ v23 ->
                                                                                                                  coe
                                                                                                                    MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316
                                                                                                                    ()
                                                                                                                    erased
                                                                                                                    (d__'36''7497'__134
                                                                                                                       (coe
                                                                                                                          v0)
                                                                                                                       (coe
                                                                                                                          v1)
                                                                                                                       (coe
                                                                                                                          (MAlonzo.RTE.QName
                                                                                                                             (30 ::
                                                                                                                                Integer)
                                                                                                                             (4058621091251856968 ::
                                                                                                                                Integer)
                                                                                                                             "Tactic.RingSolver.Core.Expression.Expr._\8859_"
                                                                                                                             (MAlonzo.RTE.Fixity
                                                                                                                                MAlonzo.RTE.RightAssoc
                                                                                                                                (MAlonzo.RTE.Related
                                                                                                                                   (8.0 ::
                                                                                                                                      Double)))))
                                                                                                                       (coe
                                                                                                                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                          (coe
                                                                                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                             (coe
                                                                                                                                v17)
                                                                                                                             (coe
                                                                                                                                v23))
                                                                                                                          (coe
                                                                                                                             v7))))
                                                                                                        _ -> coe
                                                                                                               v12
                                                                                                 _ -> coe
                                                                                                        v12
                                                                                          _ -> coe
                                                                                                 v12
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            _ -> coe v12
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> coe v12
                                                _ -> coe v12
                                         _ -> coe v12
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> coe v12
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> coe v5
-- Tactic.RingSolver.RingSolverReflection._.convertSub
d_convertSub_196 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_convertSub_196 v0 v1 v2 v3 v4
  = let v5
          = coe
              MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
              (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208) in
    case coe v4 of
      (:) v6 v7
        -> case coe v6 of
             MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v8 v9
               -> case coe v8 of
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v10 v11
                      -> let v12
                               = d_convertSub_196 (coe v0) (coe v1) (coe v2) (coe v3) (coe v7) in
                         case coe v10 of
                           MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                             -> case coe v11 of
                                  MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v13 v14
                                    -> case coe v13 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                           -> case coe v14 of
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                  -> case coe v7 of
                                                       (:) v15 v16
                                                         -> case coe v15 of
                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v17 v18
                                                                -> case coe v17 of
                                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v19 v20
                                                                       -> case coe v19 of
                                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                                                              -> case coe v20 of
                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v21 v22
                                                                                     -> case coe
                                                                                               v21 of
                                                                                          MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                                                            -> case coe
                                                                                                      v22 of
                                                                                                 MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                                                   -> case coe
                                                                                                             v16 of
                                                                                                        []
                                                                                                          -> coe
                                                                                                               MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                                                                               ()
                                                                                                               ()
                                                                                                               erased
                                                                                                               erased
                                                                                                               (d_convert_188
                                                                                                                  (coe
                                                                                                                     v0)
                                                                                                                  (coe
                                                                                                                     v1)
                                                                                                                  (coe
                                                                                                                     v2)
                                                                                                                  (coe
                                                                                                                     v3)
                                                                                                                  (coe
                                                                                                                     v9))
                                                                                                               (\ v23 ->
                                                                                                                  coe
                                                                                                                    MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                                                                                    ()
                                                                                                                    ()
                                                                                                                    erased
                                                                                                                    erased
                                                                                                                    (d_convertOp'8321'_192
                                                                                                                       (coe
                                                                                                                          v0)
                                                                                                                       (coe
                                                                                                                          v1)
                                                                                                                       (coe
                                                                                                                          v2)
                                                                                                                       (coe
                                                                                                                          v3)
                                                                                                                       (coe
                                                                                                                          (MAlonzo.RTE.QName
                                                                                                                             (32 ::
                                                                                                                                Integer)
                                                                                                                             (4058621091251856968 ::
                                                                                                                                Integer)
                                                                                                                             "Tactic.RingSolver.Core.Expression.Expr.\8861_"
                                                                                                                             (MAlonzo.RTE.Fixity
                                                                                                                                MAlonzo.RTE.NonAssoc
                                                                                                                                MAlonzo.RTE.Unrelated)))
                                                                                                                       (coe
                                                                                                                          v7))
                                                                                                                    (\ v24 ->
                                                                                                                       coe
                                                                                                                         MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316
                                                                                                                         ()
                                                                                                                         erased
                                                                                                                         (d__'36''7497'__134
                                                                                                                            (coe
                                                                                                                               v0)
                                                                                                                            (coe
                                                                                                                               v1)
                                                                                                                            (coe
                                                                                                                               (MAlonzo.RTE.QName
                                                                                                                                  (26 ::
                                                                                                                                     Integer)
                                                                                                                                  (4058621091251856968 ::
                                                                                                                                     Integer)
                                                                                                                                  "Tactic.RingSolver.Core.Expression.Expr._\8853_"
                                                                                                                                  (MAlonzo.RTE.Fixity
                                                                                                                                     MAlonzo.RTE.LeftAssoc
                                                                                                                                     (MAlonzo.RTE.Related
                                                                                                                                        (6.0 ::
                                                                                                                                           Double)))))
                                                                                                                            (coe
                                                                                                                               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                               (coe
                                                                                                                                  MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                  (coe
                                                                                                                                     v17)
                                                                                                                                  (coe
                                                                                                                                     v23))
                                                                                                                               (coe
                                                                                                                                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                                                                                                                  (coe
                                                                                                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                                                                                                                                     (coe
                                                                                                                                        v17)
                                                                                                                                     (coe
                                                                                                                                        v24))
                                                                                                                                  (coe
                                                                                                                                     v16))))))
                                                                                                        _ -> coe
                                                                                                               v12
                                                                                                 _ -> coe
                                                                                                        v12
                                                                                          _ -> coe
                                                                                                 v12
                                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                                            _ -> coe v12
                                                                     _ -> MAlonzo.RTE.mazUnreachableError
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> coe v12
                                                _ -> coe v12
                                         _ -> coe v12
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> coe v12
                    _ -> MAlonzo.RTE.mazUnreachableError
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> coe v5
-- Tactic.RingSolver.RingSolverReflection._.convertUnknownName
d_convertUnknownName_198 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  AgdaAny ->
  [MAlonzo.Code.Agda.Builtin.Reflection.T_Arg_88] -> AgdaAny
d_convertUnknownName_198 v0 v1 v2 v3 v4 v5
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v4)
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
      (\ v6 ->
         coe
           MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
           (coe
              MAlonzo.Code.Reflection.AST.AlphaEquality.d__'61'α'61''45'Term__54
              (coe v6) (coe d_add_70 (coe v2)))
           (coe
              d_convertOp'8322'_190 (coe v0) (coe v1) (coe v2) (coe v3)
              (coe
                 (MAlonzo.RTE.QName
                    (26 :: Integer) (4058621091251856968 :: Integer)
                    "Tactic.RingSolver.Core.Expression.Expr._\8853_"
                    (MAlonzo.RTE.Fixity
                       MAlonzo.RTE.LeftAssoc (MAlonzo.RTE.Related (6.0 :: Double)))))
              (coe v5))
           (coe
              MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
              (coe
                 MAlonzo.Code.Reflection.AST.AlphaEquality.d__'61'α'61''45'Term__54
                 (coe v6) (coe d_mul_72 (coe v2)))
              (coe
                 d_convertOp'8322'_190 (coe v0) (coe v1) (coe v2) (coe v3)
                 (coe
                    (MAlonzo.RTE.QName
                       (28 :: Integer) (4058621091251856968 :: Integer)
                       "Tactic.RingSolver.Core.Expression.Expr._\8855_"
                       (MAlonzo.RTE.Fixity
                          MAlonzo.RTE.LeftAssoc (MAlonzo.RTE.Related (7.0 :: Double)))))
                 (coe v5))
              (coe
                 MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                 (coe
                    MAlonzo.Code.Reflection.AST.AlphaEquality.d__'61'α'61''45'Term__54
                    (coe v6) (coe d_neg_76 (coe v2)))
                 (coe
                    d_convertOp'8321'_192 (coe v0) (coe v1) (coe v2) (coe v3)
                    (coe
                       (MAlonzo.RTE.QName
                          (32 :: Integer) (4058621091251856968 :: Integer)
                          "Tactic.RingSolver.Core.Expression.Expr.\8861_"
                          (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
                    (coe v5))
                 (coe
                    MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                    (coe
                       MAlonzo.Code.Reflection.AST.AlphaEquality.d__'61'α'61''45'Term__54
                       (coe v6) (coe d_pow_74 (coe v2)))
                    (coe d_convertExp_194 (coe v0) (coe v1) (coe v2) (coe v3) (coe v5))
                    (coe
                       MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
                       (coe
                          MAlonzo.Code.Reflection.AST.AlphaEquality.d__'61'α'61''45'Term__54
                          (coe v6) (coe d_sub_78 (coe v2)))
                       (coe d_convertSub_196 (coe v0) (coe v1) (coe v2) (coe v3) (coe v5))
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                          (d_'96'Κ_140
                             (coe v0) (coe v1)
                             (coe
                                MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v4)
                                (coe v5)))))))))
-- Tactic.RingSolver.RingSolverReflection._.convertSuc
d_convertSuc_200 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer ->
  T_RingOperatorTerms_58 ->
  (Integer ->
   Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146) ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_convertSuc_200 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased (d_convert_188 (coe v0) (coe v1) (coe v2) (coe v3) (coe v4))
      (\ v5 ->
         coe
           MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
           (d__'36''7497'__134
              (coe v0) (coe v1)
              (coe
                 (MAlonzo.RTE.QName
                    (26 :: Integer) (4058621091251856968 :: Integer)
                    "Tactic.RingSolver.Core.Expression.Expr._\8853_"
                    (MAlonzo.RTE.Fixity
                       MAlonzo.RTE.LeftAssoc (MAlonzo.RTE.Related (6.0 :: Double)))))
              (coe
                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                 (coe
                    MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                       (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                    (coe
                       d_'96'Κ_140 (coe v0) (coe v1)
                       (coe
                          MAlonzo.Code.Data.Nat.Reflection.d_toTerm_14
                          (coe (1 :: Integer)))))
                 (coe
                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                    (coe
                       MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98
                       (coe
                          MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82
                          (coe MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50)
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                             (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                       (coe v5))
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
-- Tactic.RingSolver.malformedForallTypeError
d_malformedForallTypeError_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_malformedForallTypeError_288 v0 ~v1 v2
  = du_malformedForallTypeError_288 v0 v2
du_malformedForallTypeError_288 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_malformedForallTypeError_288 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334 v0 erased
      (coe
         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
            (coe ("Malformed call to solve." :: Data.Text.Text)))
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
               (coe
                  ("Expected target type to be like: \8704 x y \8594 x + y \8776 y + x."
                   ::
                   Data.Text.Text)))
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
                  (coe ("Instead: " :: Data.Text.Text)))
               (coe
                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_termErr_302 (coe v1))
                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
-- Tactic.RingSolver.quantifiedVarMap
d_quantifiedVarMap_292 ::
  Integer ->
  Integer -> Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_quantifiedVarMap_292 v0 v1
  = coe
      MAlonzo.Code.Data.Bool.Base.du_if_then_else__42
      (coe ltInt (coe v1) (coe v0))
      (coe
         MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 (coe v1)
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
      (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
-- Tactic.RingSolver.constructCallToSolver
d_constructCallToSolver_298 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_RingOperatorTerms_58 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_constructCallToSolver_298 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased (coe du_conv_316 v0 v1 v2 v3)
      (\ v5 ->
         coe
           MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
           erased (coe du_conv_316 v0 v1 v2 v4)
           (\ v6 ->
              coe
                MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                (d_'96'solver_160
                   (coe v0) (coe du_numVars_314 (coe v2))
                   (coe
                      MAlonzo.Code.Reflection.AST.Term.d_prependVLams_118 v2
                      (d__'96''8860'__148
                         (coe v0) (coe du_numVars_314 (coe v2)) (coe v5) (coe v6)))
                   (coe
                      MAlonzo.Code.Reflection.AST.Term.d_prependHLams_112 v2
                      (d_'96'refl_98 (coe v0))))))
-- Tactic.RingSolver._.numVars
d_numVars_314 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_RingOperatorTerms_58 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Integer
d_numVars_314 ~v0 ~v1 v2 ~v3 ~v4 = du_numVars_314 v2
du_numVars_314 ::
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] -> Integer
du_numVars_314 v0
  = coe MAlonzo.Code.Data.List.Base.du_length_304 v0
-- Tactic.RingSolver._.conv
d_conv_316 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_RingOperatorTerms_58 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_conv_316 v0 v1 v2 ~v3 ~v4 = du_conv_316 v0 v1 v2
du_conv_316 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_RingOperatorTerms_58 ->
  [MAlonzo.Code.Agda.Builtin.String.T_String_6] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_conv_316 v0 v1 v2
  = coe
      d_convertTerm_166 (coe v0) (coe du_numVars_314 (coe v2)) (coe v1)
      (coe d_quantifiedVarMap_292 (coe du_numVars_314 (coe v2)))
-- Tactic.RingSolver.solve-∀-macro
d_solve'45''8704''45'macro_322 ::
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_solve'45''8704''45'macro_322 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (d_checkIsRing_82
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v0)
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
      (\ v2 ->
         coe
           MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
           erased MAlonzo.Code.Agda.Builtin.Reflection.d_commitTC_404
           (\ v3 ->
              coe
                MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                erased (d_getRingOperatorTerms_110 (coe v2))
                (\ v4 ->
                   coe
                     MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                     erased
                     (coe
                        MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                        erased
                        (coe MAlonzo.Code.Agda.Builtin.Reflection.d_inferType_336 v1)
                        MAlonzo.Code.Agda.Builtin.Reflection.d_reduce_342)
                     (\ v5 ->
                        coe
                          MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                          erased
                          (coe
                             MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                             (d_getVisibleArgs_12
                                (coe (2 :: Integer))
                                (coe
                                   MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30
                                   (coe MAlonzo.Code.Reflection.AST.Term.d_stripPis_86 (coe v5)))))
                          (\ v6 ->
                             case coe v6 of
                               MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v7
                                 -> case coe v7 of
                                      MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v9 v10
                                        -> case coe v10 of
                                             MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v12 v13
                                               -> coe
                                                    seq (coe v13)
                                                    (coe
                                                       MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                       () () erased erased
                                                       (d_constructCallToSolver_298
                                                          (coe v2) (coe v4)
                                                          (coe
                                                             MAlonzo.Code.Data.List.Base.du_map_22
                                                             (coe
                                                                (\ v14 ->
                                                                   MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                     (coe v14)))
                                                             (coe
                                                                MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                                (coe
                                                                   MAlonzo.Code.Reflection.AST.Term.d_stripPis_86
                                                                   (coe v5))))
                                                          (coe v9) (coe v12))
                                                       (coe
                                                          MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328
                                                          v1))
                                             _ -> MAlonzo.RTE.mazUnreachableError
                                      _ -> MAlonzo.RTE.mazUnreachableError
                               MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                                 -> coe du_malformedForallTypeError_288 (coe ()) (coe v5)
                               _ -> MAlonzo.RTE.mazUnreachableError)))))
-- Tactic.RingSolver.solve-∀
d_solve'45''8704'_348 ::
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_solve'45''8704'_348 = coe d_solve'45''8704''45'macro_322
-- Tactic.RingSolver.malformedArgumentListError
d_malformedArgumentListError_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_malformedArgumentListError_354 v0 ~v1 v2
  = du_malformedArgumentListError_354 v0 v2
du_malformedArgumentListError_354 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_malformedArgumentListError_354 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334 v0 erased
      (coe
         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
            (coe ("Malformed call to solve." :: Data.Text.Text)))
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
               (coe
                  ("First argument should be a list of free variables."
                   ::
                   Data.Text.Text)))
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
                  (coe ("Instead: " :: Data.Text.Text)))
               (coe
                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_termErr_302 (coe v1))
                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
-- Tactic.RingSolver.malformedGoalError
d_malformedGoalError_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_malformedGoalError_362 v0 ~v1 v2
  = du_malformedGoalError_362 v0 v2
du_malformedGoalError_362 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_malformedGoalError_362 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_typeError_334 v0 erased
      (coe
         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
            (coe ("Malformed call to solve." :: Data.Text.Text)))
         (coe
            MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
            (coe
               MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
               (coe
                  ("Goal type should be of the form: LHS \8776 RHS"
                   ::
                   Data.Text.Text)))
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
               (coe
                  MAlonzo.Code.Agda.Builtin.Reflection.C_strErr_300
                  (coe ("Instead: " :: Data.Text.Text)))
               (coe
                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                  (coe MAlonzo.Code.Agda.Builtin.Reflection.C_termErr_302 (coe v1))
                  (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))))
-- Tactic.RingSolver.checkIsListOfVariables
d_checkIsListOfVariables_366 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_checkIsListOfVariables_366 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (coe
         MAlonzo.Code.Agda.Builtin.Reflection.d_checkType_338 v1
         (MAlonzo.Code.Data.List.Reflection.d_'96'List_6
            (coe d_'96'Carrier_96 (coe v0))))
      MAlonzo.Code.Agda.Builtin.Reflection.d_normalise_340
-- Tactic.RingSolver.getVariableIndices
d_getVariableIndices_372 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Maybe [Integer]
d_getVariableIndices_372
  = coe d_go_378 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Tactic.RingSolver._.go
d_go_378 ::
  [Integer] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Maybe [Integer]
d_go_378 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
    case coe v1 of
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170 v3 v4
        -> case coe v3 of
             MAlonzo.RTE.QName 16 15090436609435731260 _ _
               -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v0)
             MAlonzo.RTE.QName 22 15090436609435731260 _ _
               -> case coe v4 of
                    (:) v5 v6
                      -> case coe v6 of
                           (:) v7 v8
                             -> case coe v8 of
                                  (:) v9 v10
                                    -> case coe v9 of
                                         MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v11 v12
                                           -> case coe v11 of
                                                MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v13 v14
                                                  -> case coe v13 of
                                                       MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                                         -> case coe v14 of
                                                              MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v15 v16
                                                                -> case coe v15 of
                                                                     MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                                       -> case coe v16 of
                                                                            MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                              -> case coe v12 of
                                                                                   MAlonzo.Code.Agda.Builtin.Reflection.C_var_164 v17 v18
                                                                                     -> case coe
                                                                                               v18 of
                                                                                          []
                                                                                            -> case coe
                                                                                                      v10 of
                                                                                                 (:) v19 v20
                                                                                                   -> case coe
                                                                                                             v19 of
                                                                                                        MAlonzo.Code.Agda.Builtin.Reflection.C_arg_98 v21 v22
                                                                                                          -> case coe
                                                                                                                    v21 of
                                                                                                               MAlonzo.Code.Agda.Builtin.Reflection.C_arg'45'info_82 v23 v24
                                                                                                                 -> case coe
                                                                                                                           v23 of
                                                                                                                      MAlonzo.Code.Agda.Builtin.Reflection.C_visible_50
                                                                                                                        -> case coe
                                                                                                                                  v24 of
                                                                                                                             MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74 v25 v26
                                                                                                                               -> case coe
                                                                                                                                         v25 of
                                                                                                                                    MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58
                                                                                                                                      -> case coe
                                                                                                                                                v26 of
                                                                                                                                           MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66
                                                                                                                                             -> coe
                                                                                                                                                  d_go_378
                                                                                                                                                  (coe
                                                                                                                                                     MAlonzo.Code.Tactic.RingSolver.Core.NatSet.d_insert_32
                                                                                                                                                     (coe
                                                                                                                                                        v17)
                                                                                                                                                     (coe
                                                                                                                                                        v0))
                                                                                                                                                  (coe
                                                                                                                                                     v22)
                                                                                                                                           _ -> coe
                                                                                                                                                  v2
                                                                                                                                    _ -> coe
                                                                                                                                           v2
                                                                                                                             _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                                      _ -> coe
                                                                                                                             v2
                                                                                                               _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                        _ -> MAlonzo.RTE.mazUnreachableError
                                                                                                 _ -> coe
                                                                                                        v2
                                                                                          _ -> coe
                                                                                                 v2
                                                                                   _ -> coe v2
                                                                            _ -> coe v2
                                                                     _ -> coe v2
                                                              _ -> MAlonzo.RTE.mazUnreachableError
                                                       _ -> coe v2
                                                _ -> MAlonzo.RTE.mazUnreachableError
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  _ -> coe v2
                           _ -> coe v2
                    _ -> coe v2
             _ -> coe v2
      _ -> coe v2
-- Tactic.RingSolver.constructSolution
d_constructSolution_388 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_RingOperatorTerms_58 ->
  [Integer] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_constructSolution_388 v0 v1 v2 v3 v4
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased (coe du_conv_414 (coe v0) (coe v1) (coe v2) (coe v3))
      (\ v5 ->
         coe
           MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
           erased (coe du_conv_414 (coe v0) (coe v1) (coe v2) (coe v4))
           (\ v6 ->
              coe
                MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                (d_'96'trans_104
                   (coe v0) (coe d_'96'sym_100 (coe v0) (coe v5)) (coe v6))))
-- Tactic.RingSolver._.numVars
d_numVars_404 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_RingOperatorTerms_58 ->
  [Integer] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> Integer
d_numVars_404 ~v0 ~v1 v2 ~v3 ~v4 = du_numVars_404 v2
du_numVars_404 :: [Integer] -> Integer
du_numVars_404 v0
  = coe MAlonzo.Code.Data.List.Base.du_length_304 v0
-- Tactic.RingSolver._.varMap
d_varMap_406 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_RingOperatorTerms_58 ->
  [Integer] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  Integer -> Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_varMap_406 v0 ~v1 v2 ~v3 ~v4 v5 = du_varMap_406 v0 v2 v5
du_varMap_406 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  [Integer] ->
  Integer -> Maybe MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_varMap_406 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Maybe.Base.du_map_68
      (\ v3 ->
         d_'96'I_144
           (coe v0) (coe du_numVars_404 (coe v1))
           (coe MAlonzo.Code.Data.Nat.Reflection.d_toFinTerm_18 (coe v3)))
      (MAlonzo.Code.Tactic.RingSolver.Core.NatSet.d_lookup_152
         (coe v1) (coe v2))
-- Tactic.RingSolver._.ρ
d_ρ_412 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_RingOperatorTerms_58 ->
  [Integer] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_ρ_412 ~v0 ~v1 v2 ~v3 ~v4 = du_ρ_412 v2
du_ρ_412 ::
  [Integer] -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
du_ρ_412 v0 = coe d_curriedTerm_44 (coe v0)
-- Tactic.RingSolver._.conv
d_conv_414 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_RingOperatorTerms_58 ->
  [Integer] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_conv_414 v0 v1 v2 ~v3 ~v4 v5 = du_conv_414 v0 v1 v2 v5
du_conv_414 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  T_RingOperatorTerms_58 ->
  [Integer] ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
du_conv_414 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (coe
         d_convertTerm_166 v0 (coe du_numVars_404 (coe v2)) v1
         (coe du_varMap_406 (coe v0) (coe v2)) v3)
      (\ v4 ->
         coe
           MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
           (coe du_'96'correct_154 (coe v0) (coe v4) (coe du_ρ_412 (coe v2))))
-- Tactic.RingSolver.solve-macro
d_solve'45'macro_424 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_solve'45'macro_424 v0 v1 v2
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
      erased
      (d_checkIsRing_82
         (coe
            MAlonzo.Code.Agda.Builtin.Reflection.C_def_176 (coe v1)
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
      (\ v3 ->
         coe
           MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
           erased MAlonzo.Code.Agda.Builtin.Reflection.d_commitTC_404
           (\ v4 ->
              coe
                MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                erased (d_getRingOperatorTerms_110 (coe v3))
                (\ v5 ->
                   coe
                     MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                     erased (d_checkIsListOfVariables_366 (coe v3) (coe v0))
                     (\ v6 ->
                        coe
                          MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                          erased MAlonzo.Code.Agda.Builtin.Reflection.d_commitTC_404
                          (\ v7 ->
                             coe
                               MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () () erased
                               erased
                               (coe
                                  MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316 () erased
                                  (coe d_getVariableIndices_372 v6))
                               (\ v8 ->
                                  case coe v8 of
                                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v9
                                      -> coe
                                           MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 () ()
                                           erased erased
                                           (coe
                                              MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 ()
                                              () erased erased
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Reflection.d_inferType_336
                                                 v2)
                                              MAlonzo.Code.Agda.Builtin.Reflection.d_reduce_342)
                                           (\ v10 ->
                                              coe
                                                MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326 ()
                                                () erased erased
                                                (coe
                                                   MAlonzo.Code.Agda.Builtin.Reflection.d_returnTC_316
                                                   () erased
                                                   (d_getVisibleArgs_12
                                                      (coe (2 :: Integer)) (coe v10)))
                                                (\ v11 ->
                                                   case coe v11 of
                                                     MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v12
                                                       -> case coe v12 of
                                                            MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v14 v15
                                                              -> case coe v15 of
                                                                   MAlonzo.Code.Data.Vec.Base.C__'8759'__38 v17 v18
                                                                     -> coe
                                                                          seq (coe v18)
                                                                          (coe
                                                                             MAlonzo.Code.Agda.Builtin.Reflection.d_bindTC_326
                                                                             () () erased erased
                                                                             (d_constructSolution_388
                                                                                (coe v3) (coe v5)
                                                                                (coe v9) (coe v14)
                                                                                (coe v17))
                                                                             (coe
                                                                                MAlonzo.Code.Agda.Builtin.Reflection.d_unify_328
                                                                                v2))
                                                                   _ -> MAlonzo.RTE.mazUnreachableError
                                                            _ -> MAlonzo.RTE.mazUnreachableError
                                                     MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                                                       -> coe
                                                            du_malformedGoalError_362 (coe ())
                                                            (coe v10)
                                                     _ -> MAlonzo.RTE.mazUnreachableError))
                                    MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
                                      -> coe du_malformedArgumentListError_354 (coe ()) (coe v6)
                                    _ -> MAlonzo.RTE.mazUnreachableError))))))
-- Tactic.RingSolver.solve
d_solve_452 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 -> AgdaAny
d_solve_452 = coe d_solve'45'macro_424
