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

module MAlonzo.Code.Data.Vec.Reflection where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Reflection
import qualified MAlonzo.Code.Reflection.AST.Term

-- Data.Vec.Reflection.`Vector
d_'96'Vector_6 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96'Vector_6 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_def_176
      (coe
         (MAlonzo.RTE.QName
            (28 :: Integer) (3315087417907161601 :: Integer)
            "Data.Vec.Base.Vec"
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
                    (coe v0))
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
                       (coe v1))
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)) in
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
-- Data.Vec.Reflection.`[]
d_'96''91''93'_12 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d_'96''91''93'_12
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170
      (coe
         (MAlonzo.RTE.QName
            (32 :: Integer) (3315087417907161601 :: Integer)
            "Data.Vec.Base.Vec.[]"
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
                      (coe MAlonzo.Code.Agda.Builtin.Reflection.C_hidden_52)
                      (coe
                         MAlonzo.Code.Agda.Builtin.Reflection.C_modality_74
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_relevant_58)
                         (coe MAlonzo.Code.Agda.Builtin.Reflection.C_quantity'45'ω_66)))
                   (coe MAlonzo.Code.Agda.Builtin.Reflection.C_unknown_208))
                (coe
                   MAlonzo.Code.Reflection.AST.Term.d__'8943''10181''8759''10182'__78
                   (coe v2) (coe v1)))
-- Data.Vec.Reflection._`∷_
d__'96''8759'__14 ::
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146 ->
  MAlonzo.Code.Agda.Builtin.Reflection.T_Term_146
d__'96''8759'__14 v0 v1
  = coe
      MAlonzo.Code.Agda.Builtin.Reflection.C_con_170
      (coe
         (MAlonzo.RTE.QName
            (38 :: Integer) (3315087417907161601 :: Integer)
            "Data.Vec.Base.Vec._\8759_"
            (MAlonzo.RTE.Fixity
               MAlonzo.RTE.RightAssoc (MAlonzo.RTE.Related (5.0 :: Double)))))
      (let v2 = 3 :: Integer in
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
                    (coe v0))
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
                       (coe v1))
                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)) in
       case coe v2 of
         0 -> coe v3
         _ -> let v4 = 2 :: Integer in
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
