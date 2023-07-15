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

module MAlonzo.Code.Axiom.Set.Map.Dec where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Map
import qualified MAlonzo.Code.Axiom.Set.Rel
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.These.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Negation.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects

-- Axiom.Set.Map.Dec._._∈_
d__'8712'__28 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () -> AgdaAny -> AgdaAny -> ()
d__'8712'__28 = erased
-- Axiom.Set.Map.Dec._._∪_
d__'8746'__32 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () -> AgdaAny -> AgdaAny -> AgdaAny
d__'8746'__32 v0 v1 v2 v3
  = coe
      MAlonzo.Code.Axiom.Set.du__'8746'__642
      (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0)) v2 v3
-- Axiom.Set.Map.Dec._.FinSet
d_FinSet_46 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 -> () -> ()
d_FinSet_46 = erased
-- Axiom.Set.Map.Dec._.spec-∈
d_spec'45''8712'_90 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 -> () -> ()
d_spec'45''8712'_90 = erased
-- Axiom.Set.Map.Dec._.dom
d_dom_148 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () -> () -> AgdaAny -> AgdaAny
d_dom_148 v0 v1 v2
  = coe
      MAlonzo.Code.Axiom.Set.Rel.du_dom_290
      (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
-- Axiom.Set.Map.Dec._._ˢ
d__'738'_238 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () -> () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d__'738'_238 ~v0 = du__'738'_238
du__'738'_238 ::
  () -> () -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du__'738'_238 v0 v1 v2
  = coe MAlonzo.Code.Axiom.Set.Map.du__'738'_458 v2
-- Axiom.Set.Map.Dec._.Map
d_Map_256 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 -> () -> () -> ()
d_Map_256 = erased
-- Axiom.Set.Map.Dec._.left-unique
d_left'45'unique_270 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () -> () -> AgdaAny -> ()
d_left'45'unique_270 = erased
-- Axiom.Set.Map.Dec.Lookupᵐᵈ.unionThese
d_unionThese_392 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38
d_unionThese_392 v0 ~v1 ~v2 ~v3 ~v4 v5 v6 v7 v8 v9
  = du_unionThese_392 v0 v5 v6 v7 v8 v9
du_unionThese_392 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> AgdaAny -> MAlonzo.Code.Data.These.Base.T_These_38
du_unionThese_392 v0 v1 v2 v3 v4 v5
  = let v6
          = coe
              MAlonzo.Code.Axiom.Set.d__'8712''63'__1498 v0 erased v1 v4
              (coe
                 MAlonzo.Code.Axiom.Set.Rel.du_dom_290
                 (MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
                 (coe MAlonzo.Code.Axiom.Set.Map.du__'738'_458 (coe v2))) in
    let v7
          = coe
              MAlonzo.Code.Axiom.Set.d__'8712''63'__1498 v0 erased v1 v4
              (coe
                 MAlonzo.Code.Axiom.Set.Rel.du_dom_290
                 (MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
                 (coe MAlonzo.Code.Axiom.Set.Map.du__'738'_458 (coe v3))) in
    case coe v6 of
      MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v8 v9
        -> if coe v8
             then case coe v9 of
                    MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v10
                      -> case coe v7 of
                           MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v11 v12
                             -> if coe v11
                                  then case coe v12 of
                                         MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v13
                                           -> coe
                                                MAlonzo.Code.Data.These.Base.C_these_52
                                                (coe
                                                   MAlonzo.Code.Axiom.Set.Map.du_lookup'7504'_1332
                                                   (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
                                                   (coe v2) (coe v4) (coe v10))
                                                (coe
                                                   MAlonzo.Code.Axiom.Set.Map.du_lookup'7504'_1332
                                                   (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
                                                   (coe v3) (coe v4) (coe v13))
                                         _ -> MAlonzo.RTE.mazUnreachableError
                                  else coe
                                         seq (coe v12)
                                         (coe
                                            MAlonzo.Code.Data.These.Base.C_this_48
                                            (coe
                                               MAlonzo.Code.Axiom.Set.Map.du_lookup'7504'_1332
                                               (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
                                               (coe v2) (coe v4) (coe v10)))
                           _ -> MAlonzo.RTE.mazUnreachableError
                    _ -> MAlonzo.RTE.mazUnreachableError
             else coe
                    seq (coe v9)
                    (case coe v7 of
                       MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v10 v11
                         -> if coe v10
                              then case coe v11 of
                                     MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26 v12
                                       -> coe
                                            MAlonzo.Code.Data.These.Base.C_that_50
                                            (coe
                                               MAlonzo.Code.Axiom.Set.Map.du_lookup'7504'_1332
                                               (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
                                               (coe v3) (coe v4) (coe v12))
                                     _ -> MAlonzo.RTE.mazUnreachableError
                              else coe
                                     seq (coe v11)
                                     (coe
                                        MAlonzo.Code.Data.Sum.Base.du_'91'_'44'_'93'_52
                                        (coe
                                           (\ v12 ->
                                              coe
                                                MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38))
                                        (coe
                                           (\ v12 ->
                                              coe
                                                MAlonzo.Code.Relation.Nullary.Negation.Core.du_contradiction_38))
                                        (coe
                                           MAlonzo.Code.Function.Bundles.d_from_940
                                           (coe
                                              MAlonzo.Code.Axiom.Set.du_'8712''45''8746'_650
                                              (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                 (coe
                                                    MAlonzo.Code.Axiom.Set.d_replacement_196
                                                    (MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
                                                    erased erased
                                                    (\ v12 ->
                                                       MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                         (coe v12))
                                                    (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                       (coe v2))))
                                              (coe
                                                 MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                 (coe
                                                    MAlonzo.Code.Axiom.Set.d_replacement_196
                                                    (MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
                                                    erased erased
                                                    (\ v12 ->
                                                       MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                         (coe v12))
                                                    (MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                                                       (coe v3))))
                                              (coe v4))
                                           v5))
                       _ -> MAlonzo.RTE.mazUnreachableError)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Map.Dec.Lookupᵐᵈ.unionWith
d_unionWith_454 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_unionWith_454 v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9
  = du_unionWith_454 v0 v6 v7 v8 v9
du_unionWith_454 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_unionWith_454 v0 v1 v2 v3 v4
  = case coe v3 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v5 v6
        -> case coe v4 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v7 v8
               -> coe
                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                    (coe
                       du_m''''_476 (coe v0) (coe v1) (coe v2) (coe v5) erased (coe v7)
                       erased)
                    erased
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Axiom.Set.Map.Dec.Lookupᵐᵈ._.d
d_d_474 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny
d_d_474 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9 v10 ~v11
  = du_d_474 v0 v8 v10
du_d_474 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  AgdaAny -> AgdaAny -> AgdaAny
du_d_474 v0 v1 v2
  = coe
      MAlonzo.Code.Axiom.Set.du__'8746'__642
      (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
      (coe
         MAlonzo.Code.Axiom.Set.Rel.du_dom_290
         (MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0)) v1)
      (coe
         MAlonzo.Code.Axiom.Set.Rel.du_dom_290
         (MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0)) v2)
-- Axiom.Set.Map.Dec.Lookupᵐᵈ._.m''
d_m''''_476 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny
d_m''''_476 v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 v8 v9 v10 v11
  = du_m''''_476 v0 v6 v7 v8 v9 v10 v11
du_m''''_476 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny
du_m''''_476 v0 v1 v2 v3 v4 v5 v6
  = coe
      MAlonzo.Code.Axiom.Set.du_map_360
      (MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0))
      (\ v7 ->
         coe
           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
           (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7))
           (coe
              v2
              (coe
                 du_unionThese_392 (coe v0) (coe v1)
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3) (coe v4))
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v5) (coe v6))
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v7))
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v7)))))
      (coe
         MAlonzo.Code.Axiom.Set.du_incl'45'set_1576 (coe v0) (coe v1)
         (coe du_d_474 (coe v0) (coe v3) (coe v5)))
-- Axiom.Set.Map.Dec.Lookupᵐᵈ._.helper
d_helper_484 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  (MAlonzo.Code.Data.These.Base.T_These_38 -> AgdaAny) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny ->
  (AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny ->
   AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12) ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_helper_484 = erased
-- Axiom.Set.Map.Dec.Lookupᵐᵈ._._.Carrier
d_Carrier_554 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Carrier_554 = erased
-- Axiom.Set.Map.Dec.Lookupᵐᵈ._._∪⁺_
d__'8746''8314'__604 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d__'8746''8314'__604 v0 ~v1 ~v2 v3 v4
  = du__'8746''8314'__604 v0 v3 v4
du__'8746''8314'__604 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du__'8746''8314'__604 v0 v1 v2
  = coe
      du_unionWith_454 (coe v0) (coe v2)
      (coe
         MAlonzo.Code.Data.These.Base.du_fold_92 (coe (\ v3 -> v3))
         (coe (\ v3 -> v3))
         (coe MAlonzo.Code.Algebra.Bundles.d__'8729'__760 (coe v1)))
-- Axiom.Set.Map.Dec.Lookupᵐᵈ._.aggregate₊
d_aggregate'8330'_606 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  () ->
  (AgdaAny -> AgdaAny) ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_aggregate'8330'_606 v0 ~v1 ~v2 v3 v4 v5
  = du_aggregate'8330'_606 v0 v3 v4 v5
du_aggregate'8330'_606 ::
  MAlonzo.Code.Axiom.Set.T_Theory'7496'_1230 ->
  MAlonzo.Code.Algebra.Bundles.T_Monoid_740 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_aggregate'8330'_606 v0 v1 v2 v3
  = case coe v3 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v4 v5
        -> case coe v5 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v6 v7
               -> coe
                    MAlonzo.Code.Data.List.Base.du_foldl_256
                    (coe
                       (\ v8 v9 ->
                          coe
                            du__'8746''8314'__604 v0 v1 v2 v8
                            (coe
                               MAlonzo.Code.Axiom.Set.Map.du_'10100'_'10101''7504'_648
                               (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0)) (coe v9))))
                    (coe
                       MAlonzo.Code.Axiom.Set.Map.du_'8709''7504'_488
                       (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v0)))
                    (coe v6)
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
