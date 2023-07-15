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

module MAlonzo.Code.Data.Nat.Reflection where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Reflection.AST.Term

-- Data.Nat.Reflection.toTerm
d_toTerm_14 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_toTerm_14 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170
             (coe
                (MAlonzo.RTE.QName
                   (8 :: Integer) (13537827747504913145 :: Integer)
                   "Agda.Builtin.Nat.Nat.zero"
                   (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170
             (coe
                (MAlonzo.RTE.QName
                   (12 :: Integer) (13537827747504913145 :: Integer)
                   "Agda.Builtin.Nat.Nat.suc"
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
                   (coe d_toTerm_14 (coe v1)))
                (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
-- Data.Nat.Reflection.toFinTerm
d_toFinTerm_18 ::
  Integer -> MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_toFinTerm_18 v0
  = case coe v0 of
      0 -> coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170
             (coe
                (MAlonzo.RTE.QName
                   (12 :: Integer) (2085323462298651273 :: Integer)
                   "Data.Fin.Base.Fin.zero"
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
      _ -> let v1 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             MAlonzo.Code.Agda.Builtin.Reflection.C_con_170
             (coe
                (MAlonzo.RTE.QName
                   (16 :: Integer) (2085323462298651273 :: Integer)
                   "Data.Fin.Base.Fin.suc"
                   (MAlonzo.RTE.Fixity MAlonzo.RTE.NonAssoc MAlonzo.RTE.Unrelated)))
             (let v2 = 1 :: Integer in
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
                           (coe d_toFinTerm_18 (coe v1)))
                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16) in
              case coe v2 of
                0 -> coe v3
                _ -> let v4 = 0 :: Integer in
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
