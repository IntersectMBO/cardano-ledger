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

module MAlonzo.Code.Ledger.PParams where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Product.Properties
import qualified MAlonzo.Code.Data.Rational.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Ledger.Epoch
import qualified MAlonzo.Code.Ledger.Prelude
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects
import qualified MAlonzo.Code.Tactic.Derive.DecEq

-- Ledger.PParams.ProtVer
d_ProtVer_46 :: MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 -> ()
d_ProtVer_46 = erased
-- Ledger.PParams.Acnt
d_Acnt_48 a0 = ()
data T_Acnt_48 = C_Acnt'46'constructor_71 Integer Integer
-- Ledger.PParams.Acnt.treasury
d_treasury_54 :: T_Acnt_48 -> Integer
d_treasury_54 v0
  = case coe v0 of
      C_Acnt'46'constructor_71 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.Acnt.reserves
d_reserves_56 :: T_Acnt_48 -> Integer
d_reserves_56 v0
  = case coe v0 of
      C_Acnt'46'constructor_71 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParamGroup
d_PParamGroup_58 a0 = ()
data T_PParamGroup_58
  = C_NetworkGroup_60 | C_EconomicGroup_62 | C_TechnicalGroup_64 |
    C_GovernanceGroup_66
-- Ledger.PParams.DrepThresholds
d_DrepThresholds_68 a0 = ()
data T_DrepThresholds_68
  = C_DrepThresholds'46'constructor_377 MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
-- Ledger.PParams.DrepThresholds.P1
d_P1_90 ::
  T_DrepThresholds_68 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P1_90 v0
  = case coe v0 of
      C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.DrepThresholds.P2a
d_P2a_92 ::
  T_DrepThresholds_68 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P2a_92 v0
  = case coe v0 of
      C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.DrepThresholds.P2b
d_P2b_94 ::
  T_DrepThresholds_68 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P2b_94 v0
  = case coe v0 of
      C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.DrepThresholds.P3
d_P3_96 ::
  T_DrepThresholds_68 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P3_96 v0
  = case coe v0 of
      C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.DrepThresholds.P4
d_P4_98 ::
  T_DrepThresholds_68 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P4_98 v0
  = case coe v0 of
      C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.DrepThresholds.P5a
d_P5a_100 ::
  T_DrepThresholds_68 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P5a_100 v0
  = case coe v0 of
      C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.DrepThresholds.P5b
d_P5b_102 ::
  T_DrepThresholds_68 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P5b_102 v0
  = case coe v0 of
      C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.DrepThresholds.P5c
d_P5c_104 ::
  T_DrepThresholds_68 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P5c_104 v0
  = case coe v0 of
      C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.DrepThresholds.P5d
d_P5d_106 ::
  T_DrepThresholds_68 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P5d_106 v0
  = case coe v0 of
      C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.DrepThresholds.P6
d_P6_108 ::
  T_DrepThresholds_68 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P6_108 v0
  = case coe v0 of
      C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
        -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PoolThresholds
d_PoolThresholds_110 a0 = ()
data T_PoolThresholds_110
  = C_PoolThresholds'46'constructor_715 MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
                                        MAlonzo.Code.Data.Rational.Base.T_ℚ_6
-- Ledger.PParams.PoolThresholds.Q1
d_Q1_120 ::
  T_PoolThresholds_110 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_Q1_120 v0
  = case coe v0 of
      C_PoolThresholds'46'constructor_715 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PoolThresholds.Q2a
d_Q2a_122 ::
  T_PoolThresholds_110 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_Q2a_122 v0
  = case coe v0 of
      C_PoolThresholds'46'constructor_715 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PoolThresholds.Q2b
d_Q2b_124 ::
  T_PoolThresholds_110 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_Q2b_124 v0
  = case coe v0 of
      C_PoolThresholds'46'constructor_715 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PoolThresholds.Q4
d_Q4_126 ::
  T_PoolThresholds_110 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_Q4_126 v0
  = case coe v0 of
      C_PoolThresholds'46'constructor_715 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams
d_PParams_128 a0 = ()
data T_PParams_128
  = C_PParams'46'constructor_1197 Integer Integer Integer Integer
                                  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 Integer Integer Integer
                                  Integer AgdaAny Integer T_DrepThresholds_68 T_PoolThresholds_110
                                  Integer Integer Integer Integer Integer AgdaAny
-- Ledger.PParams.PParams.maxBlockSize
d_maxBlockSize_168 :: T_PParams_128 -> Integer
d_maxBlockSize_168 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.maxTxSize
d_maxTxSize_170 :: T_PParams_128 -> Integer
d_maxTxSize_170 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.maxHeaderSize
d_maxHeaderSize_172 :: T_PParams_128 -> Integer
d_maxHeaderSize_172 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.maxValSize
d_maxValSize_174 :: T_PParams_128 -> Integer
d_maxValSize_174 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.pv
d_pv_176 :: T_PParams_128 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pv_176 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.a
d_a_178 :: T_PParams_128 -> Integer
d_a_178 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.b
d_b_180 :: T_PParams_128 -> Integer
d_b_180 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.minUTxOValue
d_minUTxOValue_182 :: T_PParams_128 -> Integer
d_minUTxOValue_182 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.poolDeposit
d_poolDeposit_184 :: T_PParams_128 -> Integer
d_poolDeposit_184 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.Emax
d_Emax_186 :: T_PParams_128 -> AgdaAny
d_Emax_186 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.collateralPercent
d_collateralPercent_188 :: T_PParams_128 -> Integer
d_collateralPercent_188 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v11
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.drepThresholds
d_drepThresholds_190 :: T_PParams_128 -> T_DrepThresholds_68
d_drepThresholds_190 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v12
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.poolThresholds
d_poolThresholds_192 :: T_PParams_128 -> T_PoolThresholds_110
d_poolThresholds_192 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v13
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.minCCSize
d_minCCSize_194 :: T_PParams_128 -> Integer
d_minCCSize_194 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v14
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.ccTermLimit
d_ccTermLimit_196 :: T_PParams_128 -> Integer
d_ccTermLimit_196 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v15
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.govExpiration
d_govExpiration_198 :: T_PParams_128 -> Integer
d_govExpiration_198 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v16
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.govDeposit
d_govDeposit_200 :: T_PParams_128 -> Integer
d_govDeposit_200 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v17
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.drepDeposit
d_drepDeposit_202 :: T_PParams_128 -> Integer
d_drepDeposit_202 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v18
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParams.drepActivity
d_drepActivity_204 :: T_PParams_128 -> AgdaAny
d_drepActivity_204 v0
  = case coe v0 of
      C_PParams'46'constructor_1197 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
        -> coe v19
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.paramsWellFormed
d_paramsWellFormed_206 ::
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  T_PParams_128 -> Bool
d_paramsWellFormed_206 ~v0 v1 = du_paramsWellFormed_206 v1
du_paramsWellFormed_206 :: T_PParams_128 -> Bool
du_paramsWellFormed_206 v0
  = coe
      MAlonzo.Code.Relation.Nullary.Decidable.Core.d_'8970'_'8971'_104 ()
      erased
      (coe
         MAlonzo.Code.Relation.Nullary.Decidable.Core.du_'172''63'_56
         (coe
            MAlonzo.Code.Axiom.Set.d__'8712''63'__1498
            MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12 erased
            MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30 (0 :: Integer)
            (coe
               MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
               (coe d_maxBlockSize_168 (coe v0))
               (coe
                  MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                  (coe d_maxTxSize_170 (coe v0))
                  (coe
                     MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                     (coe d_maxHeaderSize_172 (coe v0))
                     (coe
                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                        (coe d_maxValSize_174 (coe v0))
                        (coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                           (coe d_minUTxOValue_182 (coe v0))
                           (coe
                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                              (coe d_poolDeposit_184 (coe v0))
                              (coe
                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                 (coe d_collateralPercent_188 (coe v0))
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                    (coe d_ccTermLimit_196 (coe v0))
                                    (coe
                                       MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                       (coe d_govExpiration_198 (coe v0))
                                       (coe
                                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                          (coe d_govDeposit_200 (coe v0))
                                          (coe
                                             MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                             (coe d_drepDeposit_202 (coe v0))
                                             (coe
                                                MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))))))))))
-- Ledger.PParams._._.Emax
d_Emax_216 :: T_PParams_128 -> AgdaAny
d_Emax_216 v0 = coe d_Emax_186 (coe v0)
-- Ledger.PParams._._.a
d_a_218 :: T_PParams_128 -> Integer
d_a_218 v0 = coe d_a_178 (coe v0)
-- Ledger.PParams._._.b
d_b_220 :: T_PParams_128 -> Integer
d_b_220 v0 = coe d_b_180 (coe v0)
-- Ledger.PParams._._.ccTermLimit
d_ccTermLimit_222 :: T_PParams_128 -> Integer
d_ccTermLimit_222 v0 = coe d_ccTermLimit_196 (coe v0)
-- Ledger.PParams._._.collateralPercent
d_collateralPercent_224 :: T_PParams_128 -> Integer
d_collateralPercent_224 v0 = coe d_collateralPercent_188 (coe v0)
-- Ledger.PParams._._.drepActivity
d_drepActivity_226 :: T_PParams_128 -> AgdaAny
d_drepActivity_226 v0 = coe d_drepActivity_204 (coe v0)
-- Ledger.PParams._._.drepDeposit
d_drepDeposit_228 :: T_PParams_128 -> Integer
d_drepDeposit_228 v0 = coe d_drepDeposit_202 (coe v0)
-- Ledger.PParams._._.drepThresholds
d_drepThresholds_230 :: T_PParams_128 -> T_DrepThresholds_68
d_drepThresholds_230 v0 = coe d_drepThresholds_190 (coe v0)
-- Ledger.PParams._._.govDeposit
d_govDeposit_232 :: T_PParams_128 -> Integer
d_govDeposit_232 v0 = coe d_govDeposit_200 (coe v0)
-- Ledger.PParams._._.govExpiration
d_govExpiration_234 :: T_PParams_128 -> Integer
d_govExpiration_234 v0 = coe d_govExpiration_198 (coe v0)
-- Ledger.PParams._._.maxBlockSize
d_maxBlockSize_236 :: T_PParams_128 -> Integer
d_maxBlockSize_236 v0 = coe d_maxBlockSize_168 (coe v0)
-- Ledger.PParams._._.maxHeaderSize
d_maxHeaderSize_238 :: T_PParams_128 -> Integer
d_maxHeaderSize_238 v0 = coe d_maxHeaderSize_172 (coe v0)
-- Ledger.PParams._._.maxTxSize
d_maxTxSize_240 :: T_PParams_128 -> Integer
d_maxTxSize_240 v0 = coe d_maxTxSize_170 (coe v0)
-- Ledger.PParams._._.maxValSize
d_maxValSize_242 :: T_PParams_128 -> Integer
d_maxValSize_242 v0 = coe d_maxValSize_174 (coe v0)
-- Ledger.PParams._._.minCCSize
d_minCCSize_244 :: T_PParams_128 -> Integer
d_minCCSize_244 v0 = coe d_minCCSize_194 (coe v0)
-- Ledger.PParams._._.minUTxOValue
d_minUTxOValue_246 :: T_PParams_128 -> Integer
d_minUTxOValue_246 v0 = coe d_minUTxOValue_182 (coe v0)
-- Ledger.PParams._._.poolDeposit
d_poolDeposit_248 :: T_PParams_128 -> Integer
d_poolDeposit_248 v0 = coe d_poolDeposit_184 (coe v0)
-- Ledger.PParams._._.poolThresholds
d_poolThresholds_250 :: T_PParams_128 -> T_PoolThresholds_110
d_poolThresholds_250 v0 = coe d_poolThresholds_192 (coe v0)
-- Ledger.PParams._._.pv
d_pv_252 :: T_PParams_128 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pv_252 v0 = coe d_pv_176 (coe v0)
-- Ledger.PParams.DecEq-DrepThresholds
d_DecEq'45'DrepThresholds_254 ::
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'DrepThresholds_254 ~v0 = du_DecEq'45'DrepThresholds_254
du_DecEq'45'DrepThresholds_254 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'DrepThresholds_254
  = coe
      MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63
      (coe
         (\ v0 ->
            case coe v0 of
              C_DrepThresholds'46'constructor_377 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
                -> coe
                     (\ v11 ->
                        case coe v11 of
                          C_DrepThresholds'46'constructor_377 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe
                                       (\ v22 ->
                                          coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                        (coe
                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                           (coe
                                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                              (coe
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                 (coe
                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                    (coe
                                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                       (coe
                                                                          MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
                                                                       erased)
                                                                    erased)
                                                                 erased)
                                                              erased)
                                                           erased)
                                                        erased)
                                                     erased)
                                                  erased)
                                               erased)
                                            erased)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                       (coe
                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                          (coe
                                             MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                             (coe
                                                MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                (coe
                                                   MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                   (coe
                                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                      (coe
                                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                         (coe
                                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                            (coe
                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                               (coe
                                                                  MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                  (coe
                                                                     MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                                                  (coe
                                                                     MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                                                     (coe
                                                                        MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                                               (coe
                                                                  MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                  MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                  v21 v10))
                                                            (coe
                                                               MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                               MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                               v20 v9))
                                                         (coe
                                                            MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                            MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                            v19 v8))
                                                      (coe
                                                         MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                         MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                         v18 v7))
                                                   (coe
                                                      MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                      MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6 v17
                                                      v6))
                                                (coe
                                                   MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                   MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6 v16
                                                   v5))
                                             (coe
                                                MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6 v15 v4))
                                          (coe
                                             MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                             MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6 v14 v3))
                                       (coe
                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                          MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6 v13 v2))
                                    (coe
                                       MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                       MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6 v12 v1))
                          _ -> MAlonzo.RTE.mazUnreachableError)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Ledger.PParams.DecEq-PoolThresholds
d_DecEq'45'PoolThresholds_256 ::
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'PoolThresholds_256 ~v0 = du_DecEq'45'PoolThresholds_256
du_DecEq'45'PoolThresholds_256 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'PoolThresholds_256
  = coe
      MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63
      (coe
         (\ v0 ->
            case coe v0 of
              C_PoolThresholds'46'constructor_715 v1 v2 v3 v4
                -> coe
                     (\ v5 ->
                        case coe v5 of
                          C_PoolThresholds'46'constructor_715 v6 v7 v8 v9
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe
                                       (\ v10 ->
                                          coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                     (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
                                                     erased)
                                                  erased)
                                               erased)
                                            erased)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                       (coe
                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                          (coe
                                             MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                             (coe
                                                MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                                (coe
                                                   MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                                   (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                             (coe
                                                MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6 v9 v4))
                                          (coe
                                             MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                             MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6 v8 v3))
                                       (coe
                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                          MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6 v7 v2))
                                    (coe
                                       MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                       MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6 v6 v1))
                          _ -> MAlonzo.RTE.mazUnreachableError)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Ledger.PParams.DecEq-PParams
d_DecEq'45'PParams_258 ::
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'PParams_258 v0
  = coe
      MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63
      (coe
         (\ v1 ->
            case coe v1 of
              C_PParams'46'constructor_1197 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
                -> coe
                     (\ v21 ->
                        case coe v21 of
                          C_PParams'46'constructor_1197 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe
                                       (\ v41 ->
                                          coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                               (coe
                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                  (coe
                                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                     (coe
                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                        (coe
                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                           (coe
                                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                              (coe
                                                                 MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                 (coe
                                                                    MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                    (coe
                                                                       MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                       (coe
                                                                          MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                          (coe
                                                                             MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                             (coe
                                                                                MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                (coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe
                                                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                            (coe
                                                                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
                                                                                                  erased)
                                                                                               erased)
                                                                                            erased)
                                                                                         erased)
                                                                                      erased)
                                                                                   erased)
                                                                                erased)
                                                                             erased)
                                                                          erased)
                                                                       erased)
                                                                    erased)
                                                                 erased)
                                                              erased)
                                                           erased)
                                                        erased)
                                                     erased)
                                                  erased)
                                               erased)
                                            erased)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                       (coe
                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                          (coe
                                             MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                             (coe
                                                MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                (coe
                                                   MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                   (coe
                                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                      (coe
                                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                         (coe
                                                            MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                            (coe
                                                               MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                               (coe
                                                                  MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                  (coe
                                                                     MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                     (coe
                                                                        MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                        (coe
                                                                           MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                           (coe
                                                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                              (coe
                                                                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                 (coe
                                                                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                    (coe
                                                                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                       (coe
                                                                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                          (coe
                                                                                             MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                                             (coe
                                                                                                MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                                                                             (coe
                                                                                                MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                                                                                (coe
                                                                                                   MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                                                                          (coe
                                                                                             MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                             (MAlonzo.Code.Ledger.Epoch.d_DecEq'45'Epoch_70
                                                                                                (coe
                                                                                                   v0))
                                                                                             v40
                                                                                             v20))
                                                                                       (coe
                                                                                          MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                                                          (coe v39)
                                                                                          (coe
                                                                                             v19)))
                                                                                    (coe
                                                                                       MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                                                       (coe v38)
                                                                                       (coe v18)))
                                                                                 (coe
                                                                                    MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                                                    (coe v37)
                                                                                    (coe v17)))
                                                                              (coe
                                                                                 MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                                                 (coe v36)
                                                                                 (coe v16)))
                                                                           (coe
                                                                              MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                                              (coe v35) (coe v15)))
                                                                        (coe
                                                                           MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                                                           (coe
                                                                              MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                                                                              erased
                                                                              (coe
                                                                                 (\ v41 ->
                                                                                    coe
                                                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                            (coe
                                                                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
                                                                                               erased)
                                                                                            erased)
                                                                                         erased)
                                                                                      erased)))
                                                                           (coe
                                                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                              (coe
                                                                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                 (coe
                                                                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                    (coe
                                                                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                       (coe
                                                                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                                          (coe
                                                                                             MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                                                                          (coe
                                                                                             MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                                                                             (coe
                                                                                                MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                                                                       (coe
                                                                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                          MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                          (d_Q4_126
                                                                                             (coe
                                                                                                v14))
                                                                                          (d_Q4_126
                                                                                             (coe
                                                                                                v34))))
                                                                                    (coe
                                                                                       MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                       MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                       (d_Q2b_124
                                                                                          (coe v14))
                                                                                       (d_Q2b_124
                                                                                          (coe
                                                                                             v34))))
                                                                                 (coe
                                                                                    MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                    MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                    (d_Q2a_122
                                                                                       (coe v14))
                                                                                    (d_Q2a_122
                                                                                       (coe v34))))
                                                                              (coe
                                                                                 MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                 MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                 (d_Q1_120
                                                                                    (coe v14))
                                                                                 (d_Q1_120
                                                                                    (coe v34))))))
                                                                     (coe
                                                                        MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                                                        (coe
                                                                           MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                                                                           erased
                                                                           (coe
                                                                              (\ v41 ->
                                                                                 coe
                                                                                   MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                   (coe
                                                                                      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                      (coe
                                                                                         MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                         (coe
                                                                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                            (coe
                                                                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                               (coe
                                                                                                  MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                  (coe
                                                                                                     MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                     (coe
                                                                                                        MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                        (coe
                                                                                                           MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                           (coe
                                                                                                              MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                                                                                              (coe
                                                                                                                 MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
                                                                                                              erased)
                                                                                                           erased)
                                                                                                        erased)
                                                                                                     erased)
                                                                                                  erased)
                                                                                               erased)
                                                                                            erased)
                                                                                         erased)
                                                                                      erased)
                                                                                   erased)))
                                                                        (coe
                                                                           MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                           (coe
                                                                              MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                              (coe
                                                                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                 (coe
                                                                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                    (coe
                                                                                       MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                       (coe
                                                                                          MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                          (coe
                                                                                             MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                             (coe
                                                                                                MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                                (coe
                                                                                                   MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                                   (coe
                                                                                                      MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                                                                                                      (coe
                                                                                                         MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                                                                                         (coe
                                                                                                            MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                                                                                         (coe
                                                                                                            MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                                                                                            (coe
                                                                                                               MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                                                                                      (coe
                                                                                                         MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                                         MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                                         (d_P6_108
                                                                                                            (coe
                                                                                                               v13))
                                                                                                         (d_P6_108
                                                                                                            (coe
                                                                                                               v33))))
                                                                                                   (coe
                                                                                                      MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                                      MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                                      (d_P5d_106
                                                                                                         (coe
                                                                                                            v13))
                                                                                                      (d_P5d_106
                                                                                                         (coe
                                                                                                            v33))))
                                                                                                (coe
                                                                                                   MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                                   MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                                   (d_P5c_104
                                                                                                      (coe
                                                                                                         v13))
                                                                                                   (d_P5c_104
                                                                                                      (coe
                                                                                                         v33))))
                                                                                             (coe
                                                                                                MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                                MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                                (d_P5b_102
                                                                                                   (coe
                                                                                                      v13))
                                                                                                (d_P5b_102
                                                                                                   (coe
                                                                                                      v33))))
                                                                                          (coe
                                                                                             MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                             MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                             (d_P5a_100
                                                                                                (coe
                                                                                                   v13))
                                                                                             (d_P5a_100
                                                                                                (coe
                                                                                                   v33))))
                                                                                       (coe
                                                                                          MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                          MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                          (d_P4_98
                                                                                             (coe
                                                                                                v13))
                                                                                          (d_P4_98
                                                                                             (coe
                                                                                                v33))))
                                                                                    (coe
                                                                                       MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                       MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                       (d_P3_96
                                                                                          (coe v13))
                                                                                       (d_P3_96
                                                                                          (coe
                                                                                             v33))))
                                                                                 (coe
                                                                                    MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                    MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                    (d_P2b_94
                                                                                       (coe v13))
                                                                                    (d_P2b_94
                                                                                       (coe v33))))
                                                                              (coe
                                                                                 MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                                 MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                                 (d_P2a_92
                                                                                    (coe v13))
                                                                                 (d_P2a_92
                                                                                    (coe v33))))
                                                                           (coe
                                                                              MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                              MAlonzo.Code.Ledger.Prelude.d_DecEq'45'ℚ_6
                                                                              (d_P1_90 (coe v13))
                                                                              (d_P1_90
                                                                                 (coe v33))))))
                                                                  (coe
                                                                     MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                                     (coe v32) (coe v12)))
                                                               (coe
                                                                  MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                                                  (MAlonzo.Code.Ledger.Epoch.d_DecEq'45'Epoch_70
                                                                     (coe v0))
                                                                  v31 v11))
                                                            (coe
                                                               MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                               (coe v30) (coe v10)))
                                                         (coe
                                                            MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                            (coe v29) (coe v9)))
                                                      (coe
                                                         MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                         (coe v28) (coe v8)))
                                                   (coe
                                                      MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                      (coe v27) (coe v7)))
                                                (coe
                                                   MAlonzo.Code.Data.Product.Properties.du_'8801''45'dec_78
                                                   (coe
                                                      MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464)
                                                   (coe
                                                      (\ v41 ->
                                                         MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464))
                                                   (coe v26) (coe v6)))
                                             (coe
                                                MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                                (coe v25) (coe v5)))
                                          (coe
                                             MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                                             (coe v24) (coe v4)))
                                       (coe
                                          MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464 (coe v23)
                                          (coe v3)))
                                    (coe
                                       MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464 (coe v22)
                                       (coe v2)))
                          _ -> MAlonzo.RTE.mazUnreachableError)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Ledger.PParams.PParamsDiff
d_PParamsDiff_260 a0 = ()
data T_PParamsDiff_260
  = C_PParamsDiff'46'constructor_246881 (AgdaAny ->
                                         [T_PParamGroup_58])
                                        (T_PParams_128 -> AgdaAny -> T_PParams_128)
                                        (AgdaAny -> Bool)
-- Ledger.PParams.PParamsDiff.UpdateT
d_UpdateT_276 :: T_PParamsDiff_260 -> ()
d_UpdateT_276 = erased
-- Ledger.PParams.PParamsDiff.updateGroups
d_updateGroups_278 ::
  T_PParamsDiff_260 -> AgdaAny -> [T_PParamGroup_58]
d_updateGroups_278 v0
  = case coe v0 of
      C_PParamsDiff'46'constructor_246881 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParamsDiff.applyUpdate
d_applyUpdate_280 ::
  T_PParamsDiff_260 -> T_PParams_128 -> AgdaAny -> T_PParams_128
d_applyUpdate_280 v0
  = case coe v0 of
      C_PParamsDiff'46'constructor_246881 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParamsDiff.ppdWellFormed
d_ppdWellFormed_282 :: T_PParamsDiff_260 -> AgdaAny -> Bool
d_ppdWellFormed_282 v0
  = case coe v0 of
      C_PParamsDiff'46'constructor_246881 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.PParams.PParamsDiff.ppdWellFormed⇒WF
d_ppdWellFormed'8658'WF_288 ::
  T_PParamsDiff_260 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_ppdWellFormed'8658'WF_288 = erased
