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

module MAlonzo.Code.Data.List.Ext.Properties where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Membership.Propositional.Properties
import qualified MAlonzo.Code.Data.List.Relation.Binary.BagAndSetEquality
import qualified MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional
import qualified MAlonzo.Code.Data.List.Relation.Unary.All
import qualified MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.List.Relation.Unary.Unique.Propositional.Properties.WithK
import qualified MAlonzo.Code.Data.Product.Function.NonDependent.Propositional
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.Sum.Function.Propositional
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Related
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Interface.DecEq

-- Data.List.Ext.Properties._×-cong_
d__'215''45'cong__26 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'215''45'cong__26 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du__'215''45'cong__26 v8 v9 v10
du__'215''45'cong__26 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'215''45'cong__26 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Related.du_fromRelated_138 (coe v0)
      (coe
         MAlonzo.Code.Data.Product.Function.NonDependent.Propositional.du__'215''45'cong__102
         v0
         (coe
            MAlonzo.Code.Function.Related.du_toRelated_100 (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Function.Related.du_toRelated_100 (coe v0) (coe v2)))
-- Data.List.Ext.Properties._⊎-cong_
d__'8846''45'cong__54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'8846''45'cong__54 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du__'8846''45'cong__54 v8 v9 v10
du__'8846''45'cong__54 ::
  MAlonzo.Code.Function.Related.Propositional.T_Kind_6 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'8846''45'cong__54 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Related.du_fromRelated_138 (coe v0)
      (coe
         MAlonzo.Code.Data.Sum.Function.Propositional.du__'8846''45'cong__100
         v0
         (coe
            MAlonzo.Code.Function.Related.du_toRelated_100 (coe v0) (coe v1))
         (coe
            MAlonzo.Code.Function.Related.du_toRelated_100 (coe v0) (coe v2)))
-- Data.List.Ext.Properties._.deduplicate≡
d_deduplicate'8801'_132 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> [AgdaAny] -> [AgdaAny]
d_deduplicate'8801'_132 ~v0 ~v1 v2 = du_deduplicate'8801'_132 v2
du_deduplicate'8801'_132 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> [AgdaAny] -> [AgdaAny]
du_deduplicate'8801'_132 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_deduplicate_834
      (coe MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0))
-- Data.List.Ext.Properties._.disj-on-dedup
d_disj'45'on'45'dedup_138 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] ->
  [AgdaAny] ->
  (AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_disj'45'on'45'dedup_138 = erased
-- Data.List.Ext.Properties._.∈-dedup
d_'8712''45'dedup_146 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_'8712''45'dedup_146 ~v0 ~v1 v2 v3 v4
  = du_'8712''45'dedup_146 v2 v3 v4
du_'8712''45'dedup_146 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_'8712''45'dedup_146 v0 v1 v2
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
      (coe
         MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45'deduplicate'8314'_582
         (coe MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) (coe v1)
         (coe v2))
      (coe
         MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45'deduplicate'8315'_556
         (coe MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) (coe v1))
-- Data.List.Ext.Properties._.dedup-++-↭
d_dedup'45''43''43''45''8621'_152 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] ->
  [AgdaAny] ->
  (AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
d_dedup'45''43''43''45''8621'_152 ~v0 ~v1 v2 v3 v4 ~v5
  = du_dedup'45''43''43''45''8621'_152 v2 v3 v4
du_dedup'45''43''43''45''8621'_152 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Binary.Permutation.Propositional.T__'8621'__16
du_dedup'45''43''43''45''8621'_152 v0 v1 v2
  = coe
      MAlonzo.Code.Data.List.Relation.Binary.BagAndSetEquality.du_'8764'bag'8658''8621'_1500
      (coe
         MAlonzo.Code.Data.List.Base.du_deduplicate_834
         (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0))
         (coe
            MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2)))
      (coe
         MAlonzo.Code.Data.List.Base.du__'43''43'__62
         (coe
            MAlonzo.Code.Data.List.Base.du_deduplicate_834
            (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) v1)
         (coe
            MAlonzo.Code.Data.List.Base.du_deduplicate_834
            (MAlonzo.Code.Interface.DecEq.d__'8799'__20 (coe v0)) v2))
      (coe
         MAlonzo.Code.Data.List.Relation.Unary.Unique.Propositional.Properties.WithK.du_unique'8743'set'8658'bag_64
         (coe
            (\ v3 ->
               coe
                 MAlonzo.Code.Function.Related.du_toRelated_100
                 (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                 (coe
                    MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                    (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                    (coe
                       MAlonzo.Code.Function.Related.Propositional.du_SK'45'sym_168
                       (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_88)
                       (coe
                          du_'8712''45'dedup_146 (coe v0)
                          (coe
                             MAlonzo.Code.Data.List.Base.du__'43''43'__62 (coe v1) (coe v2))
                          (coe v3)))
                    (coe
                       MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                       (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                       (coe du_helper_170 (coe v1) (coe v2))
                       (coe
                          MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                          (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                          (coe
                             du__'8846''45'cong__54
                             (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                             (coe du_'8712''45'dedup_146 (coe v0) (coe v1) (coe v3))
                             (coe du_'8712''45'dedup_146 (coe v0) (coe v2) (coe v3)))
                          (coe
                             MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                             (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                             (coe
                                MAlonzo.Code.Function.Related.Propositional.du_SK'45'sym_168
                                (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_88)
                                (coe
                                   du_helper_170 (coe du_deduplicate'8801'_132 v0 v1)
                                   (coe du_deduplicate'8801'_132 v0 v2)))
                             (coe
                                MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
                                (coe
                                   MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)))))))))
-- Data.List.Ext.Properties._._.helper
d_helper_170 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] ->
  [AgdaAny] ->
  (AgdaAny ->
   MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20) ->
  [AgdaAny] ->
  [AgdaAny] ->
  AgdaAny -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
d_helper_170 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 ~v8
  = du_helper_170 v6 v7
du_helper_170 ::
  [AgdaAny] ->
  [AgdaAny] -> MAlonzo.Code.Function.Bundles.T_Equivalence_928
du_helper_170 v0 v1
  = coe
      MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
      (coe
         MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45''43''43''8315'_212
         v0 v1)
      (coe
         MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52
         (coe
            MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45''43''43''8314''737'_200
            (coe v0))
         (coe
            MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45''43''43''8314''691'_206
            v0 v1))
-- Data.List.Ext.Properties.AllPairs⇒≡∨R∨Rᵒᵖ
d_AllPairs'8658''8801''8744'R'8744'R'7506''7510'_192 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  () ->
  (AgdaAny -> AgdaAny -> ()) ->
  AgdaAny ->
  AgdaAny ->
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_AllPairs'8658''8801''8744'R'8744'R'7506''7510'_192 ~v0 ~v1 ~v2
                                                     ~v3 ~v4 ~v5 v6 v7
  = du_AllPairs'8658''8801''8744'R'8744'R'7506''7510'_192 v6 v7
du_AllPairs'8658''8801''8744'R'8744'R'7506''7510'_192 ::
  [AgdaAny] ->
  MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.T_AllPairs_20 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_AllPairs'8658''8801''8744'R'8744'R'7506''7510'_192 v0 v1
  = case coe v1 of
      MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C_'91''93'_22
        -> erased
      MAlonzo.Code.Data.List.Relation.Unary.AllPairs.Core.C__'8759'__28 v4 v5
        -> case coe v0 of
             (:) v6 v7
               -> coe
                    (\ v8 ->
                       case coe v8 of
                         MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v11
                           -> coe
                                (\ v12 ->
                                   case coe v12 of
                                     MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v15
                                       -> coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 erased
                                     MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v15
                                       -> coe
                                            MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                                            (coe
                                               MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
                                               (coe
                                                  MAlonzo.Code.Data.List.Relation.Unary.All.du_lookup_440
                                                  v7 v4 v15))
                                     _ -> MAlonzo.RTE.mazUnreachableError)
                         MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v11
                           -> coe
                                (\ v12 ->
                                   case coe v12 of
                                     MAlonzo.Code.Data.List.Relation.Unary.Any.C_here_46 v15
                                       -> coe
                                            MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                                            (coe
                                               MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42
                                               (coe
                                                  MAlonzo.Code.Data.List.Relation.Unary.All.du_lookup_440
                                                  v7 v4 v11))
                                     MAlonzo.Code.Data.List.Relation.Unary.Any.C_there_54 v15
                                       -> coe
                                            du_AllPairs'8658''8801''8744'R'8744'R'7506''7510'_192 v7
                                            v5 v11 v15
                                     _ -> MAlonzo.RTE.mazUnreachableError)
                         _ -> MAlonzo.RTE.mazUnreachableError)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
