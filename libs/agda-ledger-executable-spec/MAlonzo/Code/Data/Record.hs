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

module MAlonzo.Code.Data.Record where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Data.Record.Manifest-Σ
d_Manifest'45'Σ_22 a0 a1 a2 a3 a4 a5 a6 a7 = ()
newtype T_Manifest'45'Σ_22 = C__'44'_40 AgdaAny
-- Data.Record.Manifest-Σ.proj₁
d_proj'8321'_36 :: T_Manifest'45'Σ_22 -> AgdaAny
d_proj'8321'_36 v0
  = case coe v0 of
      C__'44'_40 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Record.Manifest-Σ.proj₂
d_proj'8322'_38 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> ()) ->
  (AgdaAny -> AgdaAny) -> T_Manifest'45'Σ_22 -> AgdaAny
d_proj'8322'_38 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 v7 v8
  = du_proj'8322'_38 v7 v8
du_proj'8322'_38 ::
  (AgdaAny -> AgdaAny) -> T_Manifest'45'Σ_22 -> AgdaAny
du_proj'8322'_38 v0 v1 = coe v0 (d_proj'8321'_36 (coe v1))
-- Data.Record.Signature
d_Signature_44 a0 a1 a2 a3 = ()
data T_Signature_44
  = C_'8709'_58 | C__'44'_'8758'__66 T_Signature_44 AgdaAny |
    C__'44'_'8788'__78 T_Signature_44 AgdaAny (T_Record_50 -> AgdaAny)
-- Data.Record.Record
d_Record_50 a0 a1 a2 a3 a4 = ()
newtype T_Record_50 = C_rec_88 AgdaAny
-- Data.Record.Record-fun
d_Record'45'fun_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 -> T_Signature_44 -> ()
d_Record'45'fun_54 = erased
-- Data.Record.Record.fun
d_fun_86 :: T_Record_50 -> AgdaAny
d_fun_86 v0
  = case coe v0 of
      C_rec_88 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Record.labels
d_labels_104 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Signature_44 -> [AgdaAny]
d_labels_104 ~v0 ~v1 ~v2 ~v3 v4 = du_labels_104 v4
du_labels_104 :: T_Signature_44 -> [AgdaAny]
du_labels_104 v0
  = case coe v0 of
      C_'8709'_58 -> coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16
      C__'44'_'8758'__66 v1 v2
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
             (coe du_labels_104 (coe v1))
      C__'44'_'8788'__78 v1 v2 v4
        -> coe
             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
             (coe du_labels_104 (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Record._∈_
d__'8712'__120 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  AgdaAny -> T_Signature_44 -> ()
d__'8712'__120 = erased
-- Data.Record.Restrict
d_Restrict_136 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Signature_44 -> AgdaAny -> AgdaAny -> T_Signature_44
d_Restrict_136 ~v0 ~v1 v2 ~v3 v4 v5 ~v6 = du_Restrict_136 v2 v4 v5
du_Restrict_136 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  T_Signature_44 -> AgdaAny -> T_Signature_44
du_Restrict_136 v0 v1 v2
  = case coe v1 of
      C__'44'_'8758'__66 v3 v4
        -> let v6
                 = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                     (coe v0 v2 v4) in
           if coe v6
             then coe v3
             else coe du_Restrict_136 (coe v0) (coe v3) (coe v2)
      C__'44'_'8788'__78 v3 v4 v6
        -> let v7
                 = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                     (coe v0 v2 v4) in
           if coe v7
             then coe v3
             else coe du_Restrict_136 (coe v0) (coe v3) (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Record.Restricted
d_Restricted_214 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Signature_44 -> AgdaAny -> AgdaAny -> ()
d_Restricted_214 = erased
-- Data.Record.Proj
d_Proj_230 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Signature_44 -> AgdaAny -> AgdaAny -> T_Record_50 -> ()
d_Proj_230 = erased
-- Data.Record._∣_
d__'8739'__316 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Signature_44 -> T_Record_50 -> AgdaAny -> AgdaAny -> T_Record_50
d__'8739'__316 ~v0 ~v1 v2 ~v3 v4 v5 v6 ~v7
  = du__'8739'__316 v2 v4 v5 v6
du__'8739'__316 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  T_Signature_44 -> T_Record_50 -> AgdaAny -> T_Record_50
du__'8739'__316 v0 v1 v2 v3
  = case coe v1 of
      C__'44'_'8758'__66 v4 v5
        -> case coe v2 of
             C_rec_88 v7
               -> let v8
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v0 v3 v5) in
                  if coe v8
                    then coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7)
                    else coe
                           du__'8739'__316 (coe v0) (coe v4)
                           (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7)) (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      C__'44'_'8788'__78 v4 v5 v7
        -> case coe v2 of
             C_rec_88 v8
               -> let v9
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v0 v3 v5) in
                  if coe v9
                    then coe d_proj'8321'_36 (coe v8)
                    else coe
                           du__'8739'__316 (coe v0) (coe v4) (coe d_proj'8321'_36 (coe v8))
                           (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Record._·_
d__'183'__412 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Signature_44 -> T_Record_50 -> AgdaAny -> AgdaAny -> AgdaAny
d__'183'__412 ~v0 ~v1 v2 ~v3 v4 v5 v6 ~v7
  = du__'183'__412 v2 v4 v5 v6
du__'183'__412 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  T_Signature_44 -> T_Record_50 -> AgdaAny -> AgdaAny
du__'183'__412 v0 v1 v2 v3
  = case coe v1 of
      C__'44'_'8758'__66 v4 v5
        -> case coe v2 of
             C_rec_88 v7
               -> let v8
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v0 v3 v5) in
                  if coe v8
                    then coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v7)
                    else coe
                           du__'183'__412 (coe v0) (coe v4)
                           (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7)) (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      C__'44'_'8788'__78 v4 v5 v7
        -> case coe v2 of
             C_rec_88 v8
               -> let v9
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v0 v3 v5) in
                  if coe v9
                    then coe du_proj'8322'_38 (coe v7) (coe v8)
                    else coe
                           du__'183'__412 (coe v0) (coe v4) (coe d_proj'8321'_36 (coe v8))
                           (coe v3)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Record._With_≔_
d__With_'8788'__508 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Signature_44 ->
  AgdaAny -> AgdaAny -> (T_Record_50 -> AgdaAny) -> T_Signature_44
d__With_'8788'__508 ~v0 ~v1 v2 ~v3 v4 v5 ~v6 v7
  = du__With_'8788'__508 v2 v4 v5 v7
du__With_'8788'__508 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  T_Signature_44 ->
  AgdaAny -> (T_Record_50 -> AgdaAny) -> T_Signature_44
du__With_'8788'__508 v0 v1 v2 v3
  = case coe v1 of
      C__'44'_'8758'__66 v4 v5
        -> let v7
                 = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                     (coe v0 v2 v5) in
           if coe v7
             then coe C__'44'_'8788'__78 v4 v5 v3
             else coe
                    C__'44'_'8758'__66
                    (coe du__With_'8788'__508 (coe v0) (coe v4) (coe v2) (coe v3)) v5
      C__'44'_'8788'__78 v4 v5 v7
        -> let v8
                 = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                     (coe v0 v2 v5) in
           if coe v8
             then coe C__'44'_'8788'__78 v4 v5 v3
             else coe
                    C__'44'_'8788'__78
                    (coe du__With_'8788'__508 (coe v0) (coe v4) (coe v2) (coe v3)) v5
                    (\ v9 ->
                       coe
                         v7
                         (coe
                            du_drop'45'With_522 (coe v0) (coe v4) (coe v2) (coe v3) (coe v9)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.Record.drop-With
d_drop'45'With_522 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  T_Signature_44 ->
  AgdaAny ->
  AgdaAny -> (T_Record_50 -> AgdaAny) -> T_Record_50 -> T_Record_50
d_drop'45'With_522 ~v0 ~v1 v2 ~v3 v4 v5 ~v6 v7 v8
  = du_drop'45'With_522 v2 v4 v5 v7 v8
du_drop'45'With_522 ::
  (AgdaAny ->
   AgdaAny ->
   MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20) ->
  T_Signature_44 ->
  AgdaAny -> (T_Record_50 -> AgdaAny) -> T_Record_50 -> T_Record_50
du_drop'45'With_522 v0 v1 v2 v3 v4
  = case coe v1 of
      C__'44'_'8758'__66 v5 v6
        -> case coe v4 of
             C_rec_88 v8
               -> let v9
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v0 v2 v6) in
                  if coe v9
                    then coe
                           C_rec_88
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                              (coe d_proj'8321'_36 (coe v8))
                              (coe du_proj'8322'_38 (coe v3) (coe v8)))
                    else coe
                           C_rec_88
                           (coe
                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                              (coe
                                 du_drop'45'With_522 (coe v0) (coe v5) (coe v2) (coe v3)
                                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v8)))
                              (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v8)))
             _ -> MAlonzo.RTE.mazUnreachableError
      C__'44'_'8788'__78 v5 v6 v8
        -> case coe v4 of
             C_rec_88 v9
               -> let v10
                        = MAlonzo.Code.Relation.Nullary.Decidable.Core.d_does_30
                            (coe v0 v2 v6) in
                  if coe v10
                    then coe v4
                    else coe
                           C_rec_88
                           (coe
                              C__'44'_40
                              (coe
                                 du_drop'45'With_522 (coe v0) (coe v5) (coe v2) (coe v3)
                                 (coe d_proj'8321'_36 (coe v9))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
