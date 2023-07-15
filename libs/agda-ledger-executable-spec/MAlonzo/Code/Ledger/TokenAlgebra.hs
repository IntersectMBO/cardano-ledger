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

module MAlonzo.Code.Ledger.TokenAlgebra where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Morphism
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Relation.Binary.Structures

-- Ledger.TokenAlgebra.TokenAlgebra
d_TokenAlgebra_4 = ()
data T_TokenAlgebra_4
  = C_TokenAlgebra'46'constructor_989 MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
                                      (AgdaAny -> Integer) (Integer -> AgdaAny)
                                      (AgdaAny -> [AgdaAny]) (AgdaAny -> Integer)
                                      MAlonzo.Code.Algebra.Morphism.T_IsCommutativeMonoidMorphism_498
                                      MAlonzo.Code.Interface.DecEq.T_DecEq_14
-- Ledger.TokenAlgebra.TokenAlgebra.PolicyId
d_PolicyId_40 :: T_TokenAlgebra_4 -> ()
d_PolicyId_40 = erased
-- Ledger.TokenAlgebra.TokenAlgebra.Value-CommutativeMonoid
d_Value'45'CommutativeMonoid_42 ::
  T_TokenAlgebra_4 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_Value'45'CommutativeMonoid_42 v0
  = case coe v0 of
      C_TokenAlgebra'46'constructor_989 v2 v3 v4 v5 v6 v9 v10 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.TokenAlgebra.TokenAlgebra.MemoryEstimate
d_MemoryEstimate_44 :: T_TokenAlgebra_4 -> ()
d_MemoryEstimate_44 = erased
-- Ledger.TokenAlgebra.TokenAlgebra._._∙_
d__'8729'__48 :: T_TokenAlgebra_4 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__48 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.d__'8729'__840
      (coe d_Value'45'CommutativeMonoid_42 (coe v0))
-- Ledger.TokenAlgebra.TokenAlgebra._._≈_
d__'8776'__50 :: T_TokenAlgebra_4 -> AgdaAny -> AgdaAny -> ()
d__'8776'__50 = erased
-- Ledger.TokenAlgebra.TokenAlgebra._.Carrier
d_Carrier_52 :: T_TokenAlgebra_4 -> ()
d_Carrier_52 = erased
-- Ledger.TokenAlgebra.TokenAlgebra._.refl
d_refl_54 :: T_TokenAlgebra_4 -> AgdaAny -> AgdaAny
d_refl_54 v0
  = coe
      MAlonzo.Code.Relation.Binary.Structures.d_refl_34
      (coe
         MAlonzo.Code.Algebra.Structures.d_isEquivalence_148
         (coe
            MAlonzo.Code.Algebra.Structures.d_isMagma_444
            (coe
               MAlonzo.Code.Algebra.Structures.d_isSemigroup_610
               (coe
                  MAlonzo.Code.Algebra.Structures.d_isMonoid_660
                  (coe
                     MAlonzo.Code.Algebra.Bundles.d_isCommutativeMonoid_844
                     (coe d_Value'45'CommutativeMonoid_42 (coe v0)))))))
-- Ledger.TokenAlgebra.TokenAlgebra._.ε
d_ε_56 :: T_TokenAlgebra_4 -> AgdaAny
d_ε_56 v0
  = coe
      MAlonzo.Code.Algebra.Bundles.d_ε_842
      (coe d_Value'45'CommutativeMonoid_42 (coe v0))
-- Ledger.TokenAlgebra.TokenAlgebra.coin
d_coin_58 :: T_TokenAlgebra_4 -> AgdaAny -> Integer
d_coin_58 v0
  = case coe v0 of
      C_TokenAlgebra'46'constructor_989 v2 v3 v4 v5 v6 v9 v10 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.TokenAlgebra.TokenAlgebra.inject
d_inject_60 :: T_TokenAlgebra_4 -> Integer -> AgdaAny
d_inject_60 v0
  = case coe v0 of
      C_TokenAlgebra'46'constructor_989 v2 v3 v4 v5 v6 v9 v10 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.TokenAlgebra.TokenAlgebra.policies
d_policies_62 :: T_TokenAlgebra_4 -> AgdaAny -> [AgdaAny]
d_policies_62 v0
  = case coe v0 of
      C_TokenAlgebra'46'constructor_989 v2 v3 v4 v5 v6 v9 v10 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.TokenAlgebra.TokenAlgebra.size
d_size_64 :: T_TokenAlgebra_4 -> AgdaAny -> Integer
d_size_64 v0
  = case coe v0 of
      C_TokenAlgebra'46'constructor_989 v2 v3 v4 v5 v6 v9 v10 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.TokenAlgebra.TokenAlgebra._≤ᵗ_
d__'8804''7511'__66 :: T_TokenAlgebra_4 -> AgdaAny -> AgdaAny -> ()
d__'8804''7511'__66 = erased
-- Ledger.TokenAlgebra.TokenAlgebra.property
d_property_68 ::
  T_TokenAlgebra_4 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_property_68 = erased
-- Ledger.TokenAlgebra.TokenAlgebra.coinIsMonoidMorphism
d_coinIsMonoidMorphism_70 ::
  T_TokenAlgebra_4 ->
  MAlonzo.Code.Algebra.Morphism.T_IsCommutativeMonoidMorphism_498
d_coinIsMonoidMorphism_70 v0
  = case coe v0 of
      C_TokenAlgebra'46'constructor_989 v2 v3 v4 v5 v6 v9 v10 -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.TokenAlgebra.TokenAlgebra.DecEq-Value
d_DecEq'45'Value_72 ::
  T_TokenAlgebra_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'Value_72 v0
  = case coe v0 of
      C_TokenAlgebra'46'constructor_989 v2 v3 v4 v5 v6 v9 v10 -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.TokenAlgebra.TokenAlgebra.sumᵛ
d_sum'7515'_74 :: T_TokenAlgebra_4 -> [AgdaAny] -> AgdaAny
d_sum'7515'_74 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_foldr_242
      (coe
         MAlonzo.Code.Algebra.Bundles.d__'8729'__840
         (coe d_Value'45'CommutativeMonoid_42 (coe v0)))
      (coe d_inject_60 v0 (0 :: Integer))
