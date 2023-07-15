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

module MAlonzo.Code.Axiom.Set.List where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.List.Membership.Propositional
import qualified MAlonzo.Code.Data.List.Membership.Propositional.Properties
import qualified MAlonzo.Code.Data.List.Membership.Setoid
import qualified MAlonzo.Code.Data.List.Relation.Unary.All
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Product.Algebra
import qualified MAlonzo.Code.Data.Product.Base
import qualified MAlonzo.Code.Data.Product.Properties.Ext
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Function.Properties.Inverse
import qualified MAlonzo.Code.Function.Related
import qualified MAlonzo.Code.Function.Related.Propositional
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties
import qualified MAlonzo.Code.Relation.Nullary.Decidable
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Axiom.Set.List.List-Model
d_List'45'Model_6 :: MAlonzo.Code.Axiom.Set.T_Theory_82
d_List'45'Model_6 = coe d_'46'extendedlambda0_12
-- Axiom.Set.List._..extendedlambda0
d_'46'extendedlambda0_12 :: MAlonzo.Code.Axiom.Set.T_Theory_82
d_'46'extendedlambda0_12
  = coe
      MAlonzo.Code.Axiom.Set.C_Theory'46'constructor_6769
      MAlonzo.Code.Axiom.Set.d_Dec'45'SpecProperty_72
      (\ v0 v1 v2 v3 ->
         coe
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
           (coe MAlonzo.Code.Data.List.Base.du_filter_792 v3 v2)
           (coe
              (\ v4 ->
                 coe
                   MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                   (coe
                      (\ v5 ->
                         case coe v5 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                             -> coe
                                  MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45'filter'8314'_522
                                  v3 v2 v7 v6
                           _ -> MAlonzo.RTE.mazUnreachableError))
                   (coe
                      (\ v5 ->
                         coe
                           MAlonzo.Code.Data.Product.Base.du_swap_346
                           (coe
                              MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45'filter'8315'_528
                              v3 v4 v2 v5))))))
      (\ v0 v1 ->
         coe
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
           (coe MAlonzo.Code.Data.List.Base.du_concat_270 v1)
           (coe
              (\ v2 ->
                 coe
                   MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                   (coe
                      (\ v3 ->
                         case coe v3 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
                             -> case coe v5 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
                                    -> coe
                                         MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45'concat'8314''8242'_268
                                         (coe v2) (coe v4) (coe v1) (coe v7) (coe v6)
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError))
                   (coe
                      (\ v3 ->
                         let v4
                               = coe
                                   MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_'8712''45'concat'8315''8242'_278
                                   (coe v1) (coe v3) in
                         case coe v4 of
                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
                             -> case coe v6 of
                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
                                    -> coe
                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v5)
                                         (coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v8)
                                            (coe v7))
                                  _ -> MAlonzo.RTE.mazUnreachableError
                           _ -> MAlonzo.RTE.mazUnreachableError)))))
      (\ v0 v1 v2 v3 ->
         coe
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
           (coe MAlonzo.Code.Data.List.Base.du_map_22 (coe v2) (coe v3))
           (coe
              (\ v4 ->
                 coe
                   MAlonzo.Code.Function.Related.Propositional.du__'8764''10216'_'10217'__202
                   (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                   (coe
                      MAlonzo.Code.Data.Product.Properties.Ext.du_'8707''45'cong'8242'_46
                      (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                      (coe
                         (\ v5 ->
                            coe
                              MAlonzo.Code.Function.Related.Propositional.du_'10518''8658'_82
                              (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                              (coe
                                 MAlonzo.Code.Function.Properties.Inverse.du_'8596''8658''10518'_302
                                 (coe MAlonzo.Code.Data.Product.Algebra.du_'215''45'comm_244)))))
                   (coe
                      MAlonzo.Code.Function.Related.Propositional.du__'10518''10216'_'10217'__210
                      (coe MAlonzo.Code.Function.Related.Propositional.C_equivalence_12)
                      (coe
                         MAlonzo.Code.Function.Related.du_fromRelated_138
                         (coe MAlonzo.Code.Function.Related.Propositional.C_bijection_22)
                         (coe
                            MAlonzo.Code.Data.List.Membership.Propositional.Properties.du_map'45''8712''8596'_174
                            (coe v3)))
                      (coe
                         MAlonzo.Code.Function.Related.Propositional.du__'8718'_248
                         (coe
                            MAlonzo.Code.Function.Related.Propositional.C_equivalence_12))))))
      (\ v0 v1 ->
         coe
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1)
           (coe
              (\ v2 ->
                 coe
                   MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 (coe (\ v3 -> v3))
                   (coe (\ v3 -> v3)))))
-- Axiom.Set.List.List-Modelᶠ
d_List'45'Model'7584'_58 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7584'_708
d_List'45'Model'7584'_58
  = coe
      MAlonzo.Code.Axiom.Set.C_Theory'7584''46'constructor_98831
      (coe d_List'45'Model_6)
      (coe
         (\ v0 v1 ->
            coe
              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v1)
              (coe
                 (\ v2 ->
                    coe
                      MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 (coe (\ v3 -> v3))
                      (coe (\ v3 -> v3))))))
-- Axiom.Set.List.Decˡ._.Set
d_Set_100 ::
  () -> MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> () -> ()
d_Set_100 = erased
-- Axiom.Set.List.Decˡ._.∅
d_'8709'_156 ::
  () -> MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> () -> [AgdaAny]
d_'8709'_156 ~v0 ~v1 = du_'8709'_156
du_'8709'_156 :: () -> [AgdaAny]
du_'8709'_156 v0
  = coe MAlonzo.Code.Axiom.Set.du_'8709'_404 (coe d_List'45'Model_6)
-- Axiom.Set.List.Decˡ._∈?_
d__'8712''63'__194 ::
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8712''63'__194 ~v0 v1 v2 = du__'8712''63'__194 v1 v2
du__'8712''63'__194 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  [AgdaAny] -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du__'8712''63'__194 v0 v1
  = coe
      MAlonzo.Code.Data.List.Relation.Unary.Any.du_any'63'_138
      (coe MAlonzo.Code.Interface.DecEq.d__'8799'__20 v0 v1)
-- Axiom.Set.List.Decˡ.≟-∅
d_'8799''45''8709'_202 ::
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [AgdaAny] -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_'8799''45''8709'_202 ~v0 ~v1 v2 = du_'8799''45''8709'_202 v2
du_'8799''45''8709'_202 ::
  [AgdaAny] -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_'8799''45''8709'_202 v0
  = case coe v0 of
      []
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 erased)
      (:) v1 v2
        -> coe
             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
             (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.List.List-Modelᵈ
d_List'45'Model'7496'_208 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230
d_List'45'Model'7496'_208
  = coe
      MAlonzo.Code.Axiom.Set.C_Theory'7496''46'constructor_105429
      (coe d_List'45'Model_6)
      (coe (\ v0 v1 v2 v3 -> coe du__'8712''63'__194 v1 v3 v2))
      (\ v0 v1 v2 -> coe du__'8712''63'__194 v1 v2)
      (coe
         (\ v0 v1 v2 v3 v4 ->
            coe
              MAlonzo.Code.Relation.Nullary.Decidable.du_map_18
              (coe
                 MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                 (\ v5 v6 ->
                    coe
                      MAlonzo.Code.Data.List.Relation.Unary.All.du_lookup_440 (coe v4)
                      v5)
                 (coe
                    MAlonzo.Code.Data.List.Relation.Unary.All.du_tabulate_272
                    (coe v4)))
              (coe
                 MAlonzo.Code.Data.List.Relation.Unary.All.du_all'63'_514 (coe v3)
                 (coe v4))))
      (coe
         (\ v0 v1 v2 v3 v4 ->
            coe
              MAlonzo.Code.Relation.Nullary.Decidable.du_map_18
              (coe
                 MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                 (coe
                    MAlonzo.Code.Data.List.Membership.Setoid.du_find_80
                    (coe
                       MAlonzo.Code.Relation.Binary.PropositionalEquality.Properties.du_setoid_402)
                    (coe v4))
                 (coe
                    (\ v5 ->
                       coe
                         MAlonzo.Code.Data.Product.Base.du_uncurry_220
                         (coe
                            MAlonzo.Code.Data.List.Membership.Propositional.du_lose_52
                            (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v5)) (coe v4))
                         (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v5)))))
              (coe
                 MAlonzo.Code.Data.List.Relation.Unary.Any.du_any'63'_138 (coe v3)
                 (coe v4))))
