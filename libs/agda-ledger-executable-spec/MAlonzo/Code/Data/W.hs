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

module MAlonzo.Code.Data.W where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Primitive
import qualified MAlonzo.Code.Data.Container.Core
import qualified MAlonzo.Code.Data.Container.Relation.Unary.All
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.Product.Base

-- Data.W.W
d_W_30 a0 a1 a2 = ()
newtype T_W_30 = C_sup_34 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Data.W.sup-injective₁
d_sup'45'injective'8321'_44 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  AgdaAny ->
  AgdaAny ->
  (AgdaAny -> T_W_30) ->
  (AgdaAny -> T_W_30) ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_sup'45'injective'8321'_44 = erased
-- Data.W.head
d_head_46 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  T_W_30 -> AgdaAny
d_head_46 ~v0 ~v1 ~v2 v3 = du_head_46 v3
du_head_46 :: T_W_30 -> AgdaAny
du_head_46 v0
  = case coe v0 of
      C_sup_34 v1
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3 -> coe v2
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.W.tail
d_tail_54 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  T_W_30 -> AgdaAny -> T_W_30
d_tail_54 ~v0 ~v1 ~v2 v3 = du_tail_54 v3
du_tail_54 :: T_W_30 -> AgdaAny -> T_W_30
du_tail_54 v0
  = case coe v0 of
      C_sup_34 v1
        -> case coe v1 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v2 v3 -> coe v3
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.W.map
d_map_62 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 -> T_W_30 -> T_W_30
d_map_62 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 v7 = du_map_62 v6 v7
du_map_62 ::
  MAlonzo.Code.Data.Container.Core.T__'8658'__74 -> T_W_30 -> T_W_30
du_map_62 v0 v1
  = case coe v1 of
      C_sup_34 v2
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
               -> coe
                    C_sup_34
                    (coe
                       MAlonzo.Code.Data.Container.Core.du_'10218'_'10219'_104 v0
                       (coe
                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 (coe v3)
                          (coe (\ v5 -> coe du_map_62 (coe v0) (coe v4 v5)))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.W._.induction
d_induction_88 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  (T_W_30 -> ()) ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.Container.Relation.Unary.All.T_'9633'_26 ->
   AgdaAny) ->
  T_W_30 -> AgdaAny
d_induction_88 ~v0 ~v1 ~v2 ~v3 ~v4 v5 v6 = du_induction_88 v5 v6
du_induction_88 ::
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
   MAlonzo.Code.Data.Container.Relation.Unary.All.T_'9633'_26 ->
   AgdaAny) ->
  T_W_30 -> AgdaAny
du_induction_88 v0 v1
  = case coe v1 of
      C_sup_34 v2
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v3 v4
               -> coe
                    v0 v2
                    (coe
                       MAlonzo.Code.Data.Container.Relation.Unary.All.C_all_52
                       (coe (\ v5 -> coe du_induction_88 (coe v0) (coe v4 v5))))
             _ -> MAlonzo.RTE.mazUnreachableError
      _ -> MAlonzo.RTE.mazUnreachableError
-- Data.W._.foldr
d_foldr_106 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  () ->
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  T_W_30 -> AgdaAny
d_foldr_106 ~v0 ~v1 ~v2 ~v3 ~v4 v5 = du_foldr_106 v5
du_foldr_106 ::
  (MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny) ->
  T_W_30 -> AgdaAny
du_foldr_106 v0
  = coe
      du_induction_88
      (coe
         (\ v1 v2 ->
            coe
              v0
              (coe
                 MAlonzo.Code.Data.Product.Base.du_'45''44'__68
                 (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v1))
                 (coe
                    MAlonzo.Code.Data.Container.Relation.Unary.All.d_proof_50
                    (coe v2)))))
-- Data.W.inhabited⇒empty
d_inhabited'8658'empty_110 ::
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Agda.Primitive.T_Level_14 ->
  MAlonzo.Code.Data.Container.Core.T_Container_10 ->
  (AgdaAny -> AgdaAny) ->
  T_W_30 -> MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20
d_inhabited'8658'empty_110 = erased
