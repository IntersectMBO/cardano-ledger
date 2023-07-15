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

module MAlonzo.Code.Ledger.Foreign.HSLedger where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Morphism
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Map
import qualified MAlonzo.Code.Axiom.Set.Rel
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Maybe.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Rational.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Foreign.Convertible
import qualified MAlonzo.Code.Foreign.Haskell.Coerce
import qualified MAlonzo.Code.Interface.ComputationalRelation
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Interface.Decidable.Instance
import qualified MAlonzo.Code.Interface.Hashable
import qualified MAlonzo.Code.Ledger.Address
import qualified MAlonzo.Code.Ledger.Crypto
import qualified MAlonzo.Code.Ledger.Deleg
import qualified MAlonzo.Code.Ledger.Epoch
import qualified MAlonzo.Code.Ledger.Foreign.LedgerTypes
import qualified MAlonzo.Code.Ledger.GovernanceActions
import qualified MAlonzo.Code.Ledger.PParams
import qualified MAlonzo.Code.Ledger.Prelude
import qualified MAlonzo.Code.Ledger.Script
import qualified MAlonzo.Code.Ledger.TokenAlgebra
import qualified MAlonzo.Code.Ledger.Transaction
import qualified MAlonzo.Code.Ledger.Utxo
import qualified MAlonzo.Code.Ledger.Utxo.Properties
import qualified MAlonzo.Code.Ledger.Utxow
import qualified MAlonzo.Code.Ledger.Utxow.Properties
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Ledger.Foreign.HSLedger.HSGlobalConstants
d_HSGlobalConstants_6 ::
  MAlonzo.Code.Ledger.Epoch.T_GlobalConstants_4
d_HSGlobalConstants_6
  = coe
      MAlonzo.Code.Ledger.Epoch.C_GlobalConstants'46'constructor_33
      (100 :: Integer)
      (coe
         MAlonzo.Code.Data.Nat.Base.C_NonZero'46'constructor_563
         (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8))
      (10 :: Integer) (1 :: Integer)
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
-- Ledger.Foreign.HSLedger.HSEpochStructure
d_HSEpochStructure_8 ::
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30
d_HSEpochStructure_8
  = coe
      MAlonzo.Code.Ledger.Epoch.d_ℕEpochStructure_288
      (coe d_HSGlobalConstants_6)
-- Ledger.Foreign.HSLedger._.DecEq-PParams
d_DecEq'45'PParams_16 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'PParams_16
  = coe
      MAlonzo.Code.Ledger.PParams.d_DecEq'45'PParams_258
      (coe d_HSEpochStructure_8)
-- Ledger.Foreign.HSLedger._.DrepThresholds
d_DrepThresholds_20 = ()
-- Ledger.Foreign.HSLedger._.PParamGroup
d_PParamGroup_28 = ()
-- Ledger.Foreign.HSLedger._.PParams
d_PParams_30 = ()
-- Ledger.Foreign.HSLedger._.PParamsDiff
d_PParamsDiff_32 = ()
-- Ledger.Foreign.HSLedger._.PoolThresholds
d_PoolThresholds_34 = ()
-- Ledger.Foreign.HSLedger._.ProtVer
d_ProtVer_36 :: ()
d_ProtVer_36 = erased
-- Ledger.Foreign.HSLedger._.paramsWellFormed
d_paramsWellFormed_40 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Bool
d_paramsWellFormed_40
  = coe MAlonzo.Code.Ledger.PParams.du_paramsWellFormed_206
-- Ledger.Foreign.HSLedger._.DrepThresholds.P1
d_P1_50 ::
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P1_50 v0 = coe MAlonzo.Code.Ledger.PParams.d_P1_90 (coe v0)
-- Ledger.Foreign.HSLedger._.DrepThresholds.P2a
d_P2a_52 ::
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P2a_52 v0 = coe MAlonzo.Code.Ledger.PParams.d_P2a_92 (coe v0)
-- Ledger.Foreign.HSLedger._.DrepThresholds.P2b
d_P2b_54 ::
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P2b_54 v0 = coe MAlonzo.Code.Ledger.PParams.d_P2b_94 (coe v0)
-- Ledger.Foreign.HSLedger._.DrepThresholds.P3
d_P3_56 ::
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P3_56 v0 = coe MAlonzo.Code.Ledger.PParams.d_P3_96 (coe v0)
-- Ledger.Foreign.HSLedger._.DrepThresholds.P4
d_P4_58 ::
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P4_58 v0 = coe MAlonzo.Code.Ledger.PParams.d_P4_98 (coe v0)
-- Ledger.Foreign.HSLedger._.DrepThresholds.P5a
d_P5a_60 ::
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P5a_60 v0 = coe MAlonzo.Code.Ledger.PParams.d_P5a_100 (coe v0)
-- Ledger.Foreign.HSLedger._.DrepThresholds.P5b
d_P5b_62 ::
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P5b_62 v0 = coe MAlonzo.Code.Ledger.PParams.d_P5b_102 (coe v0)
-- Ledger.Foreign.HSLedger._.DrepThresholds.P5c
d_P5c_64 ::
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P5c_64 v0 = coe MAlonzo.Code.Ledger.PParams.d_P5c_104 (coe v0)
-- Ledger.Foreign.HSLedger._.DrepThresholds.P5d
d_P5d_66 ::
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P5d_66 v0 = coe MAlonzo.Code.Ledger.PParams.d_P5d_106 (coe v0)
-- Ledger.Foreign.HSLedger._.DrepThresholds.P6
d_P6_68 ::
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P6_68 v0 = coe MAlonzo.Code.Ledger.PParams.d_P6_108 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.Emax
d_Emax_82 :: MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_Emax_82 v0 = coe MAlonzo.Code.Ledger.PParams.d_Emax_186 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.a
d_a_84 :: MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_a_84 v0 = coe MAlonzo.Code.Ledger.PParams.d_a_178 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.b
d_b_86 :: MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_b_86 v0 = coe MAlonzo.Code.Ledger.PParams.d_b_180 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.ccTermLimit
d_ccTermLimit_88 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_ccTermLimit_88 v0
  = coe MAlonzo.Code.Ledger.PParams.d_ccTermLimit_196 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.collateralPercent
d_collateralPercent_90 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_collateralPercent_90 v0
  = coe MAlonzo.Code.Ledger.PParams.d_collateralPercent_188 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.drepActivity
d_drepActivity_92 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_drepActivity_92 v0
  = coe MAlonzo.Code.Ledger.PParams.d_drepActivity_204 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.drepDeposit
d_drepDeposit_94 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_drepDeposit_94 v0
  = coe MAlonzo.Code.Ledger.PParams.d_drepDeposit_202 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.drepThresholds
d_drepThresholds_96 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.PParams.T_DrepThresholds_68
d_drepThresholds_96 v0
  = coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.govDeposit
d_govDeposit_98 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_govDeposit_98 v0
  = coe MAlonzo.Code.Ledger.PParams.d_govDeposit_200 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.govExpiration
d_govExpiration_100 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_govExpiration_100 v0
  = coe MAlonzo.Code.Ledger.PParams.d_govExpiration_198 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.maxBlockSize
d_maxBlockSize_102 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_maxBlockSize_102 v0
  = coe MAlonzo.Code.Ledger.PParams.d_maxBlockSize_168 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.maxHeaderSize
d_maxHeaderSize_104 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_maxHeaderSize_104 v0
  = coe MAlonzo.Code.Ledger.PParams.d_maxHeaderSize_172 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.maxTxSize
d_maxTxSize_106 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_maxTxSize_106 v0
  = coe MAlonzo.Code.Ledger.PParams.d_maxTxSize_170 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.maxValSize
d_maxValSize_108 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_maxValSize_108 v0
  = coe MAlonzo.Code.Ledger.PParams.d_maxValSize_174 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.minCCSize
d_minCCSize_110 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_minCCSize_110 v0
  = coe MAlonzo.Code.Ledger.PParams.d_minCCSize_194 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.minUTxOValue
d_minUTxOValue_112 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_minUTxOValue_112 v0
  = coe MAlonzo.Code.Ledger.PParams.d_minUTxOValue_182 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.poolDeposit
d_poolDeposit_114 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 -> Integer
d_poolDeposit_114 v0
  = coe MAlonzo.Code.Ledger.PParams.d_poolDeposit_184 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.poolThresholds
d_poolThresholds_116 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.PParams.T_PoolThresholds_110
d_poolThresholds_116 v0
  = coe MAlonzo.Code.Ledger.PParams.d_poolThresholds_192 (coe v0)
-- Ledger.Foreign.HSLedger._.PParams.pv
d_pv_118 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pv_118 v0 = coe MAlonzo.Code.Ledger.PParams.d_pv_176 (coe v0)
-- Ledger.Foreign.HSLedger._.PParamsDiff.UpdateT
d_UpdateT_122 ::
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 -> ()
d_UpdateT_122 = erased
-- Ledger.Foreign.HSLedger._.PParamsDiff.applyUpdate
d_applyUpdate_124 ::
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  AgdaAny -> MAlonzo.Code.Ledger.PParams.T_PParams_128
d_applyUpdate_124 v0
  = coe MAlonzo.Code.Ledger.PParams.d_applyUpdate_280 (coe v0)
-- Ledger.Foreign.HSLedger._.PParamsDiff.ppdWellFormed
d_ppdWellFormed_126 ::
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 -> AgdaAny -> Bool
d_ppdWellFormed_126 v0
  = coe MAlonzo.Code.Ledger.PParams.d_ppdWellFormed_282 (coe v0)
-- Ledger.Foreign.HSLedger._.PParamsDiff.ppdWellFormed⇒WF
d_ppdWellFormed'8658'WF_128 ::
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_ppdWellFormed'8658'WF_128 = erased
-- Ledger.Foreign.HSLedger._.PParamsDiff.updateGroups
d_updateGroups_130 ::
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  AgdaAny -> [MAlonzo.Code.Ledger.PParams.T_PParamGroup_58]
d_updateGroups_130 v0
  = coe MAlonzo.Code.Ledger.PParams.d_updateGroups_278 (coe v0)
-- Ledger.Foreign.HSLedger._.PoolThresholds.Q1
d_Q1_134 ::
  MAlonzo.Code.Ledger.PParams.T_PoolThresholds_110 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_Q1_134 v0 = coe MAlonzo.Code.Ledger.PParams.d_Q1_120 (coe v0)
-- Ledger.Foreign.HSLedger._.PoolThresholds.Q2a
d_Q2a_136 ::
  MAlonzo.Code.Ledger.PParams.T_PoolThresholds_110 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_Q2a_136 v0 = coe MAlonzo.Code.Ledger.PParams.d_Q2a_122 (coe v0)
-- Ledger.Foreign.HSLedger._.PoolThresholds.Q2b
d_Q2b_138 ::
  MAlonzo.Code.Ledger.PParams.T_PoolThresholds_110 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_Q2b_138 v0 = coe MAlonzo.Code.Ledger.PParams.d_Q2b_124 (coe v0)
-- Ledger.Foreign.HSLedger._.PoolThresholds.Q4
d_Q4_140 ::
  MAlonzo.Code.Ledger.PParams.T_PoolThresholds_110 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_Q4_140 v0 = coe MAlonzo.Code.Ledger.PParams.d_Q4_126 (coe v0)
-- Ledger.Foreign.HSLedger.isHashableSelf
d_isHashableSelf_144 ::
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_isHashableSelf_144 ~v0 v1 = du_isHashableSelf_144 v1
du_isHashableSelf_144 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
du_isHashableSelf_144 v0
  = coe
      MAlonzo.Code.Ledger.Crypto.du_mkIsHashableSet_26 (coe (\ v1 -> v1))
      (coe v0)
-- Ledger.Foreign.HSLedger.isHashableSet-⊤
d_isHashableSet'45''8868'_150 ::
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_isHashableSet'45''8868'_150
  = coe
      du_isHashableSelf_144
      (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45''8868'_28)
-- Ledger.Foreign.HSLedger.isHashableSet-ℕ
d_isHashableSet'45'ℕ_152 ::
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_isHashableSet'45'ℕ_152
  = coe
      du_isHashableSelf_144
      (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
-- Ledger.Foreign.HSLedger.HSPKKScheme
d_HSPKKScheme_154 :: MAlonzo.Code.Ledger.Crypto.T_PKKScheme_70
d_HSPKKScheme_154
  = coe
      MAlonzo.Code.Ledger.Crypto.C_PKKScheme'46'constructor_2217 addInt
      (\ v0 v1 ->
         MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
           (coe addInt (coe v0) (coe v1)))
      erased MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30
      MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30
      MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30
-- Ledger.Foreign.HSLedger.HSCrypto
d_HSCrypto_174 :: MAlonzo.Code.Ledger.Crypto.T_Crypto_164
d_HSCrypto_174
  = coe
      MAlonzo.Code.Ledger.Crypto.C_Crypto'46'constructor_2767
      d_HSPKKScheme_154 d_isHashableSet'45'ℕ_152
      MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30
-- Ledger.Foreign.HSLedger.HSP1ScriptStructure
d_HSP1ScriptStructure_176 ::
  MAlonzo.Code.Ledger.Script.T_P1ScriptStructure_12
d_HSP1ScriptStructure_176
  = coe
      MAlonzo.Code.Ledger.Script.C_P1ScriptStructure'46'constructor_519
      erased
      (coe
         MAlonzo.Code.Interface.Hashable.C_Hashable'46'constructor_9 erased)
      MAlonzo.Code.Interface.DecEq.d_DecEq'45''8869'_26
-- Ledger.Foreign.HSLedger.HSP2ScriptStructure
d_HSP2ScriptStructure_186 ::
  MAlonzo.Code.Ledger.Script.T_PlutusStructure_46
d_HSP2ScriptStructure_186
  = coe
      MAlonzo.Code.Ledger.Script.C_PlutusStructure'46'constructor_1575
      (coe
         MAlonzo.Code.Ledger.Crypto.C_HashableSet'46'constructor_535
         (coe
            du_isHashableSelf_144
            (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45''8869'_26)))
      (coe
         MAlonzo.Code.Interface.Hashable.C_Hashable'46'constructor_9 erased)
      MAlonzo.Code.Interface.DecEq.d_DecEq'45''8869'_26 erased
-- Ledger.Foreign.HSLedger.HSScriptStructure
d_HSScriptStructure_188 ::
  MAlonzo.Code.Ledger.Script.T_ScriptStructure_120
d_HSScriptStructure_188
  = coe
      MAlonzo.Code.Ledger.Script.C_ScriptStructure'46'constructor_2427
      (coe d_HSP1ScriptStructure_176) (coe d_HSP2ScriptStructure_186)
-- Ledger.Foreign.HSLedger.coinTokenAlgebra
d_coinTokenAlgebra_190 ::
  MAlonzo.Code.Ledger.TokenAlgebra.T_TokenAlgebra_4
d_coinTokenAlgebra_190
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.C_TokenAlgebra'46'constructor_989
      MAlonzo.Code.Data.Nat.Properties.d_'43''45'0'45'commutativeMonoid_3206
      (\ v0 -> v0) (\ v0 -> v0)
      (\ v0 ->
         coe
           MAlonzo.Code.Axiom.Set.du_'8709'_404
           (coe
              MAlonzo.Code.Axiom.Set.d_th_1374
              (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12)))
      (\ v0 -> 1 :: Integer)
      (coe
         MAlonzo.Code.Algebra.Morphism.C_IsCommutativeMonoidMorphism'46'constructor_3701
         (coe
            MAlonzo.Code.Algebra.Morphism.C_IsMonoidMorphism'46'constructor_2137
            (coe
               MAlonzo.Code.Algebra.Morphism.C_IsSemigroupMorphism'46'constructor_1081
               (coe (\ v0 v1 v2 -> v2)) erased)
            erased))
      (coe
         MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63
         (coe MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464))
-- Ledger.Foreign.HSLedger._.HSTransactionStructure
d_HSTransactionStructure_208 ::
  MAlonzo.Code.Ledger.Transaction.T_TransactionStructure_4
d_HSTransactionStructure_208
  = coe
      MAlonzo.Code.Ledger.Transaction.C_TransactionStructure'46'constructor_1497
      d_HSEpochStructure_8 d_HSGlobalConstants_6 d_HSCrypto_174
      d_isHashableSet'45''8868'_150
      (coe
         du_isHashableSelf_144
         (coe
            MAlonzo.Code.Ledger.PParams.d_DecEq'45'PParams_258
            (coe d_HSEpochStructure_8)))
      (coe
         MAlonzo.Code.Ledger.PParams.C_PParamsDiff'46'constructor_246881
         (\ v0 ->
            coe
              MAlonzo.Code.Axiom.Set.du_'8709'_404
              (coe
                 MAlonzo.Code.Axiom.Set.d_th_1374
                 (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12)))
         (\ v0 v1 -> v0)
         (\ v0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10))
      (\ v0 -> v0) (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      d_coinTokenAlgebra_190 MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30
      MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30
      MAlonzo.Code.Interface.DecEq.d_DecEq'45''8868'_28
      MAlonzo.Code.Interface.DecEq.d_DecEq'45''8868'_28
      d_HSScriptStructure_188
-- Ledger.Foreign.HSLedger._.DepositPurpose
d_DepositPurpose_238 = ()
-- Ledger.Foreign.HSLedger._.Deposits
d_Deposits_240 :: ()
d_Deposits_240 = erased
-- Ledger.Foreign.HSLedger._.UTxOEnv
d_UTxOEnv_254 = ()
-- Ledger.Foreign.HSLedger._.UTxOState
d_UTxOState_256 = ()
-- Ledger.Foreign.HSLedger._.UTxOEnv.pparams
d_pparams_334 ::
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128
d_pparams_334 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_pparams_1040 (coe v0)
-- Ledger.Foreign.HSLedger._.UTxOEnv.slot
d_slot_336 :: MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 -> Integer
d_slot_336 v0 = coe MAlonzo.Code.Ledger.Utxo.d_slot_1038 (coe v0)
-- Ledger.Foreign.HSLedger._.UTxOState.deposits
d_deposits_340 ::
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_deposits_340 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_deposits_1056 (coe v0)
-- Ledger.Foreign.HSLedger._.UTxOState.donations
d_donations_342 ::
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 -> Integer
d_donations_342 v0
  = coe MAlonzo.Code.Ledger.Utxo.d_donations_1058 (coe v0)
-- Ledger.Foreign.HSLedger._.UTxOState.fees
d_fees_344 :: MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 -> Integer
d_fees_344 v0 = coe MAlonzo.Code.Ledger.Utxo.d_fees_1054 (coe v0)
-- Ledger.Foreign.HSLedger._.UTxOState.utxo
d_utxo_346 ::
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_utxo_346 v0 = coe MAlonzo.Code.Ledger.Utxo.d_utxo_1052 (coe v0)
-- Ledger.Foreign.HSLedger._.UTXO-step
d_UTXO'45'step_362 ::
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  Maybe MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042
d_UTXO'45'step_362
  = coe
      MAlonzo.Code.Ledger.Utxo.Properties.d_UTXO'45'step_1296
      (coe d_HSTransactionStructure_208)
-- Ledger.Foreign.HSLedger._.Computational-UTXOW
d_Computational'45'UTXOW_412 ::
  MAlonzo.Code.Interface.ComputationalRelation.T_Computational_32
d_Computational'45'UTXOW_412
  = coe
      MAlonzo.Code.Ledger.Utxow.Properties.d_Computational'45'UTXOW_940
      (coe d_HSTransactionStructure_208)
-- Ledger.Foreign.HSLedger._.THash
d_THash_488 :: ()
d_THash_488 = erased
-- Ledger.Foreign.HSLedger._.Addr
d_Addr_490 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Addr_490 = erased
-- Ledger.Foreign.HSLedger._.Anchor
d_Anchor_492 a0 = ()
-- Ledger.Foreign.HSLedger._.AuxiliaryData
d_AuxiliaryData_494 :: ()
d_AuxiliaryData_494 = erased
-- Ledger.Foreign.HSLedger._.BaseAddr
d_BaseAddr_496 a0 a1 a2 = ()
-- Ledger.Foreign.HSLedger._.BootstrapAddr
d_BootstrapAddr_498 a0 a1 a2 = ()
-- Ledger.Foreign.HSLedger._.CostModel
d_CostModel_518 :: ()
d_CostModel_518 = erased
-- Ledger.Foreign.HSLedger._.Credential
d_Credential_520 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Credential_520 = erased
-- Ledger.Foreign.HSLedger._.DCert
d_DCert_522 a0 = ()
-- Ledger.Foreign.HSLedger._.T
d_T_530 :: ()
d_T_530 = erased
-- Ledger.Foreign.HSLedger._.THash
d_THash_532 :: ()
d_THash_532 = erased
-- Ledger.Foreign.HSLedger._.Dataʰ
d_Data'688'_534 :: MAlonzo.Code.Ledger.Crypto.T_HashableSet_52
d_Data'688'_534
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Script.d_Data'688'_84
      (coe
         MAlonzo.Code.Ledger.Script.d_ps_128
         (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0)))
-- Ledger.Foreign.HSLedger._.DecEq-Ix
d_DecEq'45'Ix_544 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'Ix_544
  = coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30
-- Ledger.Foreign.HSLedger._.DecEq-Netw
d_DecEq'45'Netw_546 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'Netw_546
  = coe MAlonzo.Code.Interface.DecEq.d_DecEq'45''8868'_28
-- Ledger.Foreign.HSLedger._.DecEq-P1Script
d_DecEq'45'P1Script_548 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'P1Script_548
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Script.d_DecEq'45'P1Script_44
      (coe
         MAlonzo.Code.Ledger.Script.d_p1s_126
         (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0)))
-- Ledger.Foreign.HSLedger._.DecEq-PlutusScript
d_DecEq'45'PlutusScript_550 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'PlutusScript_550
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Script.d_DecEq'45'PlutusScript_94
      (coe
         MAlonzo.Code.Ledger.Script.d_ps_128
         (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0)))
-- Ledger.Foreign.HSLedger._.DecEq-THash
d_DecEq'45'THash_554 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'THash_554
  = let v0 = d_HSTransactionStructure_208 in
    let v1 = MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
      (coe MAlonzo.Code.Ledger.Crypto.d_khs_240 (coe v1))
-- Ledger.Foreign.HSLedger._.DecEq-THash
d_DecEq'45'THash_556 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'THash_556
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
      (coe
         MAlonzo.Code.Ledger.Transaction.d_adHashingScheme_246 (coe v0))
-- Ledger.Foreign.HSLedger._.DecEq-THash
d_DecEq'45'THash_558 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'THash_558
  = let v0 = d_HSTransactionStructure_208 in
    let v1
          = MAlonzo.Code.Ledger.Script.d_Data'688'_84
              (coe
                 MAlonzo.Code.Ledger.Script.d_ps_128
                 (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0))) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
      (coe MAlonzo.Code.Ledger.Crypto.d_T'45'isHashable_60 (coe v1))
-- Ledger.Foreign.HSLedger._.DecEq-TxId
d_DecEq'45'TxId_560 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'TxId_560
  = coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30
-- Ledger.Foreign.HSLedger._.DecEq-UpdT
d_DecEq'45'UpdT_562 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'UpdT_562
  = coe MAlonzo.Code.Interface.DecEq.d_DecEq'45''8868'_28
-- Ledger.Foreign.HSLedger._.Epoch
d_Epoch_590 :: ()
d_Epoch_590 = erased
-- Ledger.Foreign.HSLedger._.ExUnits
d_ExUnits_592 :: ()
d_ExUnits_592 = erased
-- Ledger.Foreign.HSLedger._.GovAction
d_GovAction_602 a0 = ()
-- Ledger.Foreign.HSLedger._.GovActionID
d_GovActionID_604 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_GovActionID_604 = erased
-- Ledger.Foreign.HSLedger._.GovProposal
d_GovProposal_608 a0 = ()
-- Ledger.Foreign.HSLedger._.GovRole
d_GovRole_610 a0 = ()
-- Ledger.Foreign.HSLedger._.GovVote
d_GovVote_612 a0 = ()
-- Ledger.Foreign.HSLedger._.Hashable-P1Script
d_Hashable'45'P1Script_618 ::
  MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_Hashable'45'P1Script_618
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Script.d_Hashable'45'P1Script_42
      (coe
         MAlonzo.Code.Ledger.Script.d_p1s_126
         (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0)))
-- Ledger.Foreign.HSLedger._.Hashable-PlutusScript
d_Hashable'45'PlutusScript_620 ::
  MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_Hashable'45'PlutusScript_620
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Script.d_Hashable'45'PlutusScript_92
      (coe
         MAlonzo.Code.Ledger.Script.d_ps_128
         (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0)))
-- Ledger.Foreign.HSLedger._.Ix
d_Ix_626 :: ()
d_Ix_626 = erased
-- Ledger.Foreign.HSLedger._.THash
d_THash_628 :: ()
d_THash_628 = erased
-- Ledger.Foreign.HSLedger._.NeedsHash
d_NeedsHash_634 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovAction_362 -> ()
d_NeedsHash_634 = erased
-- Ledger.Foreign.HSLedger._.Network
d_Network_636 :: ()
d_Network_636 = erased
-- Ledger.Foreign.HSLedger._.NetworkId
d_NetworkId_638 :: MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_NetworkId_638
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Epoch.d_NetworkId_28
      (coe
         MAlonzo.Code.Ledger.Transaction.d_globalConstants_188 (coe v0))
-- Ledger.Foreign.HSLedger._.NonZero-SlotsPerEpoch
d_NonZero'45'SlotsPerEpoch_646 ::
  MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_NonZero'45'SlotsPerEpoch_646
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Epoch.d_NonZero'45'SlotsPerEpoch_22
      (coe
         MAlonzo.Code.Ledger.Transaction.d_globalConstants_188 (coe v0))
-- Ledger.Foreign.HSLedger._.P1Script
d_P1Script_648 :: ()
d_P1Script_648 = erased
-- Ledger.Foreign.HSLedger._.PlutusScript
d_PlutusScript_650 :: ()
d_PlutusScript_650 = erased
-- Ledger.Foreign.HSLedger._.UpdateT
d_UpdateT_656 :: ()
d_UpdateT_656 = erased
-- Ledger.Foreign.HSLedger._.PoolParams
d_PoolParams_664 a0 = ()
-- Ledger.Foreign.HSLedger._.Quorum
d_Quorum_668 :: Integer
d_Quorum_668
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Epoch.d_Quorum_26
      (coe
         MAlonzo.Code.Ledger.Transaction.d_globalConstants_188 (coe v0))
-- Ledger.Foreign.HSLedger._.RwdAddr
d_RwdAddr_670 a0 a1 a2 = ()
-- Ledger.Foreign.HSLedger._.SKey
d_SKey_674 :: ()
d_SKey_674 = erased
-- Ledger.Foreign.HSLedger._.Script
d_Script_678 :: ()
d_Script_678 = erased
-- Ledger.Foreign.HSLedger._.ScriptHash
d_ScriptHash_686 :: ()
d_ScriptHash_686 = erased
-- Ledger.Foreign.HSLedger._.Ser
d_Ser_688 :: ()
d_Ser_688 = erased
-- Ledger.Foreign.HSLedger._.Sig
d_Sig_690 :: ()
d_Sig_690 = erased
-- Ledger.Foreign.HSLedger._.Slot
d_Slot_692 :: ()
d_Slot_692 = erased
-- Ledger.Foreign.HSLedger._.SlotsPerEpochᶜ
d_SlotsPerEpoch'7580'_696 :: Integer
d_SlotsPerEpoch'7580'_696
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Epoch.d_SlotsPerEpoch'7580'_20
      (coe
         MAlonzo.Code.Ledger.Transaction.d_globalConstants_188 (coe v0))
-- Ledger.Foreign.HSLedger._.StabilityWindowᶜ
d_StabilityWindow'7580'_702 :: Integer
d_StabilityWindow'7580'_702
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Epoch.d_StabilityWindow'7580'_24
      (coe
         MAlonzo.Code.Ledger.Transaction.d_globalConstants_188 (coe v0))
-- Ledger.Foreign.HSLedger._.T-isHashable
d_T'45'isHashable_710 ::
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_T'45'isHashable_710
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Crypto.d_T'45'isHashable_60
      (coe
         MAlonzo.Code.Ledger.Script.d_Data'688'_84
         (coe
            MAlonzo.Code.Ledger.Script.d_ps_128
            (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0))))
-- Ledger.Foreign.HSLedger._.Tx
d_Tx_716 = ()
-- Ledger.Foreign.HSLedger._.TxBody
d_TxBody_718 = ()
-- Ledger.Foreign.HSLedger._.TxId
d_TxId_720 :: ()
d_TxId_720 = erased
-- Ledger.Foreign.HSLedger._.TxIn
d_TxIn_722 :: ()
d_TxIn_722 = erased
-- Ledger.Foreign.HSLedger._.TxOut
d_TxOut_724 :: ()
d_TxOut_724 = erased
-- Ledger.Foreign.HSLedger._.TxWitnesses
d_TxWitnesses_726 = ()
-- Ledger.Foreign.HSLedger._.Update
d_Update_730 :: ()
d_Update_730 = erased
-- Ledger.Foreign.HSLedger._.VKey
d_VKey_734 :: ()
d_VKey_734 = erased
-- Ledger.Foreign.HSLedger._.Carrier
d_Carrier_744 :: ()
d_Carrier_744 = erased
-- Ledger.Foreign.HSLedger._.Value-CommutativeMonoid
d_Value'45'CommutativeMonoid_746 ::
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_Value'45'CommutativeMonoid_746
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
      (coe MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0))
-- Ledger.Foreign.HSLedger._.Vote
d_Vote_748 a0 = ()
-- Ledger.Foreign.HSLedger._.Wdrl
d_Wdrl_750 :: ()
d_Wdrl_750 = erased
-- Ledger.Foreign.HSLedger._.adHashingScheme
d_adHashingScheme_758 ::
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_adHashingScheme_758 = coe d_isHashableSet'45''8868'_150
-- Ledger.Foreign.HSLedger._.applyUpdate
d_applyUpdate_760 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128
d_applyUpdate_760
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.PParams.d_applyUpdate_280
      (coe MAlonzo.Code.Ledger.Transaction.d_ppUpd_250 (coe v0))
-- Ledger.Foreign.HSLedger._.crypto
d_crypto_770 :: MAlonzo.Code.Ledger.Crypto.T_Crypto_164
d_crypto_770 = coe d_HSCrypto_174
-- Ledger.Foreign.HSLedger._.decEq-ScriptHash
d_decEq'45'ScriptHash_772 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'ScriptHash_772
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
      (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0))
-- Ledger.Foreign.HSLedger._.decEq-Ser
d_decEq'45'Ser_774 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'Ser_774
  = let v0 = d_HSTransactionStructure_208 in
    let v1 = MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_decEq'45'Ser_162
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Foreign.HSLedger._.decEq-Sig
d_decEq'45'Sig_776 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'Sig_776
  = let v0 = d_HSTransactionStructure_208 in
    let v1 = MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_decEq'45'Sig_160
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Foreign.HSLedger._.decEq-VKey
d_decEq'45'VKey_778 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'VKey_778
  = let v0 = d_HSTransactionStructure_208 in
    let v1 = MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_decEq'45'VKey_158
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Foreign.HSLedger._.epochStructure
d_epochStructure_786 ::
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30
d_epochStructure_786 = coe d_HSEpochStructure_8
-- Ledger.Foreign.HSLedger._.globalConstants
d_globalConstants_792 ::
  MAlonzo.Code.Ledger.Epoch.T_GlobalConstants_4
d_globalConstants_792 = coe d_HSGlobalConstants_6
-- Ledger.Foreign.HSLedger._.isKeyPair
d_isKeyPair_798 :: Integer -> Integer -> ()
d_isKeyPair_798 = erased
-- Ledger.Foreign.HSLedger._.isSigned
d_isSigned_802 :: Integer -> Integer -> Integer -> ()
d_isSigned_802 = erased
-- Ledger.Foreign.HSLedger._.isSigned-correct
d_isSigned'45'correct_804 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_isSigned'45'correct_804 = erased
-- Ledger.Foreign.HSLedger._.isSigned?
d_isSigned'63'_806 ::
  Integer ->
  Integer ->
  Integer -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isSigned'63'_806
  = let v0 = d_HSTransactionStructure_208 in
    let v1 = MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_isSigned'63'_144
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Foreign.HSLedger._.khs
d_khs_816 :: MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_khs_816
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Crypto.d_khs_240
      (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0))
-- Ledger.Foreign.HSLedger._.networkId
d_networkId_822 :: MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_networkId_822 = coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8
-- Ledger.Foreign.HSLedger._.p1s
d_p1s_826 :: MAlonzo.Code.Ledger.Script.T_P1ScriptStructure_12
d_p1s_826
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Script.d_p1s_126
      (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0))
-- Ledger.Foreign.HSLedger._.pkk
d_pkk_830 :: MAlonzo.Code.Ledger.Crypto.T_PKKScheme_70
d_pkk_830
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Crypto.d_pkk_210
      (coe MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0))
-- Ledger.Foreign.HSLedger._.ppHashingScheme
d_ppHashingScheme_834 ::
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_ppHashingScheme_834
  = coe
      du_isHashableSelf_144
      (coe
         MAlonzo.Code.Ledger.PParams.d_DecEq'45'PParams_258
         (coe d_HSEpochStructure_8))
-- Ledger.Foreign.HSLedger._.ppUpd
d_ppUpd_836 :: MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260
d_ppUpd_836
  = coe
      MAlonzo.Code.Ledger.Transaction.d_ppUpd_250
      (coe d_HSTransactionStructure_208)
-- Ledger.Foreign.HSLedger._.ppdWellFormed
d_ppdWellFormed_838 ::
  MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6 -> Bool
d_ppdWellFormed_838
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.PParams.d_ppdWellFormed_282
      (coe MAlonzo.Code.Ledger.Transaction.d_ppUpd_250 (coe v0))
-- Ledger.Foreign.HSLedger._.ppdWellFormed⇒WF
d_ppdWellFormed'8658'WF_840 ::
  MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_ppdWellFormed'8658'WF_840 = erased
-- Ledger.Foreign.HSLedger._.ps
d_ps_844 :: MAlonzo.Code.Ledger.Script.T_PlutusStructure_46
d_ps_844
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Script.d_ps_128
      (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0))
-- Ledger.Foreign.HSLedger._.sign
d_sign_858 :: Integer -> Integer -> Integer
d_sign_858
  = let v0 = d_HSTransactionStructure_208 in
    let v1 = MAlonzo.Code.Ledger.Transaction.d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_sign_130
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Foreign.HSLedger._.ss
d_ss_862 :: MAlonzo.Code.Ledger.Script.T_ScriptStructure_120
d_ss_862 = coe d_HSScriptStructure_188
-- Ledger.Foreign.HSLedger._.tokenAlgebra
d_tokenAlgebra_870 ::
  MAlonzo.Code.Ledger.TokenAlgebra.T_TokenAlgebra_4
d_tokenAlgebra_870 = coe d_coinTokenAlgebra_190
-- Ledger.Foreign.HSLedger._.txidBytes
d_txidBytes_872 :: Integer -> Integer
d_txidBytes_872 v0 = coe v0
-- Ledger.Foreign.HSLedger._.updateGroups
d_updateGroups_876 ::
  MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6 ->
  [MAlonzo.Code.Ledger.PParams.T_PParamGroup_58]
d_updateGroups_876
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.PParams.d_updateGroups_278
      (coe MAlonzo.Code.Ledger.Transaction.d_ppUpd_250 (coe v0))
-- Ledger.Foreign.HSLedger._.validP1Script
d_validP1Script_878 ::
  [Integer] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20 -> ()
d_validP1Script_878 = erased
-- Ledger.Foreign.HSLedger._.validP1Script?
d_validP1Script'63'_880 ::
  [Integer] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_validP1Script'63'_880
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Script.d_validP1Script'63'_40
      (coe
         MAlonzo.Code.Ledger.Script.d_p1s_126
         (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0)))
-- Ledger.Foreign.HSLedger._.validPlutusScript
d_validPlutusScript_882 ::
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20 ->
  [MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20] ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20 -> ()
d_validPlutusScript_882 = erased
-- Ledger.Foreign.HSLedger._.validPlutusScript?
d_validPlutusScript'63'_884 ::
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20 ->
  [MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20] ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20 ->
  MAlonzo.Code.Data.Irrelevant.T_Irrelevant_20 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_validPlutusScript'63'_884
  = let v0 = d_HSTransactionStructure_208 in
    coe
      MAlonzo.Code.Ledger.Script.d_validPlutusScript'63'_118
      (coe
         MAlonzo.Code.Ledger.Script.d_ps_128
         (coe MAlonzo.Code.Ledger.Transaction.d_ss_352 (coe v0)))
-- Ledger.Foreign.HSLedger._.ε
d_ε_886 :: Integer
d_ε_886
  = let v0 = d_HSTransactionStructure_208 in
    let v1
          = MAlonzo.Code.Ledger.Transaction.d_tokenAlgebra_256 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.d_ε_842
      (coe
         MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
         (coe v1))
-- Ledger.Foreign.HSLedger._.Anchor.hash
d_hash_946 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352 ->
  MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_hash_946 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_hash_360 (coe v0)
-- Ledger.Foreign.HSLedger._.Anchor.url
d_url_948 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_url_948 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_url_358 (coe v0)
-- Ledger.Foreign.HSLedger._.BaseAddr.net
d_net_952 ::
  MAlonzo.Code.Ledger.Address.T_BaseAddr_30 ->
  MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_net_952 v0 = coe MAlonzo.Code.Ledger.Address.d_net_38 (coe v0)
-- Ledger.Foreign.HSLedger._.BaseAddr.pay
d_pay_954 ::
  MAlonzo.Code.Ledger.Address.T_BaseAddr_30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_pay_954 v0 = coe MAlonzo.Code.Ledger.Address.d_pay_40 (coe v0)
-- Ledger.Foreign.HSLedger._.BaseAddr.stake
d_stake_956 ::
  MAlonzo.Code.Ledger.Address.T_BaseAddr_30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_stake_956 v0
  = coe MAlonzo.Code.Ledger.Address.d_stake_42 (coe v0)
-- Ledger.Foreign.HSLedger._.BootstrapAddr.attrsSize
d_attrsSize_960 ::
  MAlonzo.Code.Ledger.Address.T_BootstrapAddr_44 -> Integer
d_attrsSize_960 v0
  = coe MAlonzo.Code.Ledger.Address.d_attrsSize_56 (coe v0)
-- Ledger.Foreign.HSLedger._.BootstrapAddr.net
d_net_962 ::
  MAlonzo.Code.Ledger.Address.T_BootstrapAddr_44 ->
  MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_net_962 v0 = coe MAlonzo.Code.Ledger.Address.d_net_52 (coe v0)
-- Ledger.Foreign.HSLedger._.BootstrapAddr.pay
d_pay_964 ::
  MAlonzo.Code.Ledger.Address.T_BootstrapAddr_44 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_pay_964 v0 = coe MAlonzo.Code.Ledger.Address.d_pay_54 (coe v0)
-- Ledger.Foreign.HSLedger._.GovProposal.action
d_action_1046 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovAction_362
d_action_1046 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_action_544 (coe v0)
-- Ledger.Foreign.HSLedger._.GovProposal.anchor
d_anchor_1048 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352
d_anchor_1048 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_anchor_548 (coe v0)
-- Ledger.Foreign.HSLedger._.GovProposal.prevAction
d_prevAction_1050 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532 -> AgdaAny
d_prevAction_1050 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.d_prevAction_546 (coe v0)
-- Ledger.Foreign.HSLedger._.GovProposal.returnAddr
d_returnAddr_1052 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532 ->
  MAlonzo.Code.Ledger.Address.T_RwdAddr_58
d_returnAddr_1052 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.d_returnAddr_542 (coe v0)
-- Ledger.Foreign.HSLedger._.GovVote.anchor
d_anchor_1064 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510 ->
  Maybe MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352
d_anchor_1064 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_anchor_530 (coe v0)
-- Ledger.Foreign.HSLedger._.GovVote.credential
d_credential_1066 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_credential_1066 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.d_credential_526 (coe v0)
-- Ledger.Foreign.HSLedger._.GovVote.gid
d_gid_1068 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_gid_1068 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_gid_522 (coe v0)
-- Ledger.Foreign.HSLedger._.GovVote.role
d_role_1070 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovRole_336
d_role_1070 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_role_524 (coe v0)
-- Ledger.Foreign.HSLedger._.GovVote.vote
d_vote_1072 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_Vote_502
d_vote_1072 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_vote_528 (coe v0)
-- Ledger.Foreign.HSLedger._.PoolParams.rewardAddr
d_rewardAddr_1082 ::
  MAlonzo.Code.Ledger.Deleg.T_PoolParams_268 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_rewardAddr_1082 v0
  = coe MAlonzo.Code.Ledger.Deleg.d_rewardAddr_272 (coe v0)
-- Ledger.Foreign.HSLedger._.RwdAddr.net
d_net_1086 ::
  MAlonzo.Code.Ledger.Address.T_RwdAddr_58 ->
  MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_net_1086 v0 = coe MAlonzo.Code.Ledger.Address.d_net_64 (coe v0)
-- Ledger.Foreign.HSLedger._.RwdAddr.stake
d_stake_1088 ::
  MAlonzo.Code.Ledger.Address.T_RwdAddr_58 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_stake_1088 v0
  = coe MAlonzo.Code.Ledger.Address.d_stake_66 (coe v0)
-- Ledger.Foreign.HSLedger._.Tx.body
d_body_1092 ::
  MAlonzo.Code.Ledger.Transaction.T_Tx_1060 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988
d_body_1092 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v0)
-- Ledger.Foreign.HSLedger._.Tx.txAD
d_txAD_1094 ::
  MAlonzo.Code.Ledger.Transaction.T_Tx_1060 ->
  Maybe MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_txAD_1094 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txAD_1072 (coe v0)
-- Ledger.Foreign.HSLedger._.Tx.wits
d_wits_1096 ::
  MAlonzo.Code.Ledger.Transaction.T_Tx_1060 ->
  MAlonzo.Code.Ledger.Transaction.T_TxWitnesses_1050
d_wits_1096 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_wits_1070 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.mint
d_mint_1100 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_mint_1100 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_mint_1026 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.netwrk
d_netwrk_1102 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  Maybe MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_netwrk_1102 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_netwrk_1044 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txADhash
d_txADhash_1104 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  Maybe MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
d_txADhash_1104 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txADhash_1042 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txcerts
d_txcerts_1106 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  [MAlonzo.Code.Ledger.Deleg.T_DCert_274]
d_txcerts_1106 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txcerts_1030 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txdonation
d_txdonation_1108 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_txdonation_1108 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txdonation_1038 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txfee
d_txfee_1110 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_txfee_1110 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txfee_1024 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txid
d_txid_1112 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_txid_1112 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txid_1048 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txins
d_txins_1114 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_txins_1114 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txins_1020 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txouts
d_txouts_1116 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_txouts_1116 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txouts_1022 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txprop
d_txprop_1118 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  [MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532]
d_txprop_1118 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txprop_1036 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txsize
d_txsize_1120 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 -> Integer
d_txsize_1120 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txsize_1046 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txup
d_txup_1122 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_txup_1122 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txup_1040 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txvldt
d_txvldt_1124 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_txvldt_1124 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txvldt_1028 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txvote
d_txvote_1126 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  [MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510]
d_txvote_1126 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txvote_1034 (coe v0)
-- Ledger.Foreign.HSLedger._.TxBody.txwdrls
d_txwdrls_1128 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_txwdrls_1128 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_txwdrls_1032 (coe v0)
-- Ledger.Foreign.HSLedger._.TxWitnesses.scripts
d_scripts_1132 ::
  MAlonzo.Code.Ledger.Transaction.T_TxWitnesses_1050 ->
  [MAlonzo.Code.Data.Sum.Base.T__'8846'__30]
d_scripts_1132 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_scripts_1058 (coe v0)
-- Ledger.Foreign.HSLedger._.TxWitnesses.vkSigs
d_vkSigs_1134 ::
  MAlonzo.Code.Ledger.Transaction.T_TxWitnesses_1050 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_vkSigs_1134 v0
  = coe MAlonzo.Code.Ledger.Transaction.d_vkSigs_1056 (coe v0)
-- Ledger.Foreign.HSLedger.Convertible-Addr
d_Convertible'45'Addr_1160 ::
  MAlonzo.Code.Foreign.Convertible.T_Convertible_8
d_Convertible'45'Addr_1160
  = coe
      MAlonzo.Code.Foreign.Convertible.C_Convertible'46'constructor_21
      (coe
         (\ v0 ->
            case coe v0 of
              MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v1
                -> case coe v1 of
                     MAlonzo.Code.Ledger.Address.C_BaseAddr'46'constructor_549 v2 v3 v4
                       -> case coe v3 of
                            MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5 -> coe v5
                            MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5 -> coe v5
                            _ -> MAlonzo.RTE.mazUnreachableError
                     _ -> MAlonzo.RTE.mazUnreachableError
              MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v1
                -> case coe v1 of
                     MAlonzo.Code.Ledger.Address.C_BootstrapAddr'46'constructor_811 v2 v3 v4
                       -> case coe v3 of
                            MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 v5 -> coe v5
                            MAlonzo.Code.Data.Sum.Base.C_inj'8322'_42 v5 -> coe v5
                            _ -> MAlonzo.RTE.mazUnreachableError
                     _ -> MAlonzo.RTE.mazUnreachableError
              _ -> MAlonzo.RTE.mazUnreachableError))
      (coe
         (\ v0 ->
            coe
              MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38
              (coe
                 MAlonzo.Code.Ledger.Address.C_BaseAddr'46'constructor_549
                 (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
                 (coe MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe v0))
                 (coe
                    MAlonzo.Code.Data.Sum.Base.C_inj'8321'_38 (coe (0 :: Integer))))))
-- Ledger.Foreign.HSLedger.Convertible-TxBody
d_Convertible'45'TxBody_1172 ::
  MAlonzo.Code.Foreign.Convertible.T_Convertible_8
d_Convertible'45'TxBody_1172
  = coe
      MAlonzo.Code.Foreign.Convertible.C_Convertible'46'constructor_21
      (coe d_to''_1178) (coe d_from''_1214)
-- Ledger.Foreign.HSLedger._.to'
d_to''_1178 ::
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_TxBody_34
d_to''_1178 v0
  = coe
      MAlonzo.Code.Ledger.Foreign.LedgerTypes.C_TxBody'46'constructor_203
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe
            MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
            (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
         (coe MAlonzo.Code.Ledger.Transaction.d_txins_1020 (coe v0)))
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe
            MAlonzo.Code.Foreign.Convertible.d_to_18
            (coe
               MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
               (coe
                  MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                  (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
               (coe
                  MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                  (coe d_Convertible'45'Addr_1160)
                  (coe
                     MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                     (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)))))
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
            (coe MAlonzo.Code.Ledger.Transaction.d_txouts_1022 (coe v0))))
      (coe MAlonzo.Code.Ledger.Transaction.d_txfee_1024 (coe v0))
      (coe
         MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
         (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)
         (MAlonzo.Code.Ledger.Transaction.d_txvldt_1028 (coe v0)))
      (coe MAlonzo.Code.Ledger.Transaction.d_txsize_1046 (coe v0))
      (coe MAlonzo.Code.Ledger.Transaction.d_txid_1048 (coe v0))
-- Ledger.Foreign.HSLedger._.from'
d_from''_1214 ::
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_TxBody_34 ->
  MAlonzo.Code.Ledger.Transaction.T_TxBody_988
d_from''_1214 v0
  = coe
      MAlonzo.Code.Ledger.Transaction.C_TxBody'46'constructor_3787
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe
            MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
            (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
         (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_txins_48 (coe v0)))
      (coe
         MAlonzo.Code.Axiom.Set.Map.du_fromList'7504'_492
         (coe
            MAlonzo.Code.Axiom.Set.d_th_1374
            (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
         (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
         (coe
            MAlonzo.Code.Data.List.Base.du_map_22
            (coe
               MAlonzo.Code.Foreign.Convertible.d_from_20
               (coe
                  MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                  (coe
                     MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                     (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
                  (coe
                     MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                     (coe d_Convertible'45'Addr_1160)
                     (coe
                        MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                        (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)))))
            (coe
               MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_txouts_50 (coe v0))))
      (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_txfee_52 (coe v0))
      (let v1 = d_coinTokenAlgebra_190 in
       coe
         MAlonzo.Code.Algebra.Bundles.d_ε_842
         (coe
            MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
            (coe v1)))
      (coe
         MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
         (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)
         (MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_txvldt_54 (coe v0)))
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe
         MAlonzo.Code.Axiom.Set.Map.du_'8709''7504'_488
         (coe
            MAlonzo.Code.Axiom.Set.d_th_1374
            (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12)))
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      (let v1 = d_coinTokenAlgebra_190 in
       coe
         MAlonzo.Code.Algebra.Bundles.d_ε_842
         (coe
            MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
            (coe v1)))
      (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
      (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
      (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
      (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_txsize_56 (coe v0))
      (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_txid_58 (coe v0))
-- Ledger.Foreign.HSLedger.Convertible-TxWitnesses
d_Convertible'45'TxWitnesses_1232 ::
  MAlonzo.Code.Foreign.Convertible.T_Convertible_8
d_Convertible'45'TxWitnesses_1232
  = coe
      MAlonzo.Code.Foreign.Convertible.C_Convertible'46'constructor_21
      (coe d_to''_1238) (coe d_from''_1248)
-- Ledger.Foreign.HSLedger._.to'
d_to''_1238 ::
  MAlonzo.Code.Ledger.Transaction.T_TxWitnesses_1050 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_TxWitnesses_60
d_to''_1238 v0
  = coe
      MAlonzo.Code.Ledger.Foreign.LedgerTypes.C_TxWitnesses'46'constructor_331
      (coe
         MAlonzo.Code.Data.List.Base.du_map_22
         (coe
            MAlonzo.Code.Foreign.Convertible.d_to_18
            (coe
               MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
               (coe
                  MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                  (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
               (coe
                  MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                  (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))))
         (coe
            MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
            (coe MAlonzo.Code.Ledger.Transaction.d_vkSigs_1056 (coe v0))))
      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
-- Ledger.Foreign.HSLedger._.from'
d_from''_1248 ::
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_TxWitnesses_60 ->
  MAlonzo.Code.Ledger.Transaction.T_TxWitnesses_1050
d_from''_1248 v0
  = coe
      MAlonzo.Code.Ledger.Transaction.C_TxWitnesses'46'constructor_3891
      (coe
         MAlonzo.Code.Axiom.Set.Map.du_fromList'7504'_492
         (coe
            MAlonzo.Code.Axiom.Set.d_th_1374
            (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
         (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
         (coe
            MAlonzo.Code.Data.List.Base.du_map_22
            (coe
               MAlonzo.Code.Foreign.Convertible.d_from_20
               (coe
                  MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                  (coe
                     MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                     (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
                  (coe
                     MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                     (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))))
            (coe
               MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_vkSigs_66 (coe v0))))
      (coe
         MAlonzo.Code.Axiom.Set.du_'8709'_404
         (coe
            MAlonzo.Code.Axiom.Set.d_th_1374
            (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12)))
-- Ledger.Foreign.HSLedger.Convertible-Tx
d_Convertible'45'Tx_1258 ::
  MAlonzo.Code.Foreign.Convertible.T_Convertible_8
d_Convertible'45'Tx_1258
  = coe
      MAlonzo.Code.Foreign.Convertible.C_Convertible'46'constructor_21
      (coe d_to''_1264) (coe d_from''_1276)
-- Ledger.Foreign.HSLedger._.to'
d_to''_1264 ::
  MAlonzo.Code.Ledger.Transaction.T_Tx_1060 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_Tx_70
d_to''_1264 v0
  = coe
      MAlonzo.Code.Ledger.Foreign.LedgerTypes.C_Tx'46'constructor_391
      (coe
         d_to''_1178
         (coe MAlonzo.Code.Ledger.Transaction.d_body_1068 (coe v0)))
      (coe
         d_to''_1238
         (coe MAlonzo.Code.Ledger.Transaction.d_wits_1070 (coe v0)))
      (coe
         MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
         (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)
         (MAlonzo.Code.Ledger.Transaction.d_txAD_1072 (coe v0)))
-- Ledger.Foreign.HSLedger._.from'
d_from''_1276 ::
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_Tx_70 ->
  MAlonzo.Code.Ledger.Transaction.T_Tx_1060
d_from''_1276 v0
  = coe
      MAlonzo.Code.Ledger.Transaction.C_Tx'46'constructor_3963
      (coe
         d_from''_1214
         (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_body_78 (coe v0)))
      (coe
         d_from''_1248
         (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_wits_80 (coe v0)))
      (coe
         MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
         (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)
         (MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_txAD_82 (coe v0)))
-- Ledger.Foreign.HSLedger.Convertible-PParams
d_Convertible'45'PParams_1288 ::
  MAlonzo.Code.Foreign.Convertible.T_Convertible_8
d_Convertible'45'PParams_1288
  = coe
      MAlonzo.Code.Foreign.Convertible.C_Convertible'46'constructor_21
      (coe d_to''_1294) (coe d_from''_1338)
-- Ledger.Foreign.HSLedger._.to'
d_to''_1294 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_PParams_84
d_to''_1294 v0
  = coe
      MAlonzo.Code.Ledger.Foreign.LedgerTypes.C_PParams'46'constructor_827
      (coe MAlonzo.Code.Ledger.PParams.d_a_178 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_b_180 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_maxBlockSize_168 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_maxTxSize_170 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_maxHeaderSize_172 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_maxValSize_174 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_minUTxOValue_182 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_poolDeposit_184 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_Emax_186 (coe v0))
      (coe
         MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
         (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)
         (MAlonzo.Code.Ledger.PParams.d_pv_176 (coe v0)))
      (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)
      (coe MAlonzo.Code.Ledger.PParams.d_minCCSize_194 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_ccTermLimit_196 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_govExpiration_198 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_govDeposit_200 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_drepDeposit_202 (coe v0))
      (coe MAlonzo.Code.Ledger.PParams.d_drepActivity_204 (coe v0))
-- Ledger.Foreign.HSLedger._.from'
d_from''_1338 ::
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_PParams_84 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128
d_from''_1338 v0
  = coe
      MAlonzo.Code.Ledger.PParams.C_PParams'46'constructor_1197
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_maxBlockSize_124
         (coe v0))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_maxTxSize_126 (coe v0))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_maxHeaderSize_128
         (coe v0))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_maxValSize_130 (coe v0))
      (coe
         MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
         (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)
         (MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_pv_138 (coe v0)))
      (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_a_120 (coe v0))
      (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_b_122 (coe v0))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_minUTxOValue_132
         (coe v0))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_poolDeposit_134 (coe v0))
      (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_Emax_136 (coe v0))
      (coe (0 :: Integer))
      (coe
         MAlonzo.Code.Ledger.PParams.C_DrepThresholds'46'constructor_377
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174))
      (coe
         MAlonzo.Code.Ledger.PParams.C_PoolThresholds'46'constructor_715
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174)
         (coe MAlonzo.Code.Data.Rational.Base.d_'189'_174))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_minCCSize_142 (coe v0))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_ccTermLimit_144 (coe v0))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_govExpiration_146
         (coe v0))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_govDeposit_148 (coe v0))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_drepDeposit_150 (coe v0))
      (coe
         MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_drepActivity_152
         (coe v0))
-- Ledger.Foreign.HSLedger.Convertible-UTxOEnv
d_Convertible'45'UTxOEnv_1378 ::
  MAlonzo.Code.Foreign.Convertible.T_Convertible_8
d_Convertible'45'UTxOEnv_1378
  = coe
      MAlonzo.Code.Foreign.Convertible.C_Convertible'46'constructor_21
      (coe d_to''_1384) (coe d_from''_1390)
-- Ledger.Foreign.HSLedger._.to'
d_to''_1384 ::
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOEnv_154
d_to''_1384 v0
  = case coe v0 of
      MAlonzo.Code.Ledger.Utxo.C_UTxOEnv'46'constructor_24005 v1 v2
        -> coe
             MAlonzo.Code.Ledger.Foreign.LedgerTypes.C_UTxOEnv'46'constructor_1469
             (coe v1) (coe d_to''_1294 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.HSLedger._.from'
d_from''_1390 ::
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOEnv_154 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOEnv_1032
d_from''_1390 v0
  = coe
      MAlonzo.Code.Ledger.Utxo.C_UTxOEnv'46'constructor_24005
      (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_slot_160 (coe v0))
      (coe
         d_from''_1338
         (coe
            MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_pparams_162 (coe v0)))
-- Ledger.Foreign.HSLedger.Convertible-UTxOState
d_Convertible'45'UTxOState_1400 ::
  MAlonzo.Code.Foreign.Convertible.T_Convertible_8
d_Convertible'45'UTxOState_1400
  = coe
      MAlonzo.Code.Foreign.Convertible.C_Convertible'46'constructor_21
      (coe d_to''_1406) (coe d_from''_1412)
-- Ledger.Foreign.HSLedger._.to'
d_to''_1406 ::
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOState_164
d_to''_1406 v0
  = case coe v0 of
      MAlonzo.Code.Ledger.Utxo.C_'10214'_'44'_'44'_'44'_'10215''7512'_1060 v1 v2 v3 v4
        -> coe
             MAlonzo.Code.Ledger.Foreign.LedgerTypes.C_UTxOState'46'constructor_1511
             (coe
                MAlonzo.Code.Data.List.Base.du_map_22
                (coe
                   MAlonzo.Code.Foreign.Convertible.d_to_18
                   (coe
                      MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                      (coe
                         MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                         (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
                      (coe
                         MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                         (coe d_Convertible'45'Addr_1160)
                         (coe
                            MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                            (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)))))
                (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v1)))
             (coe v2)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Foreign.HSLedger._.from'
d_from''_1412 ::
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOState_164 ->
  MAlonzo.Code.Ledger.Utxo.T_UTxOState_1042
d_from''_1412 v0
  = coe
      MAlonzo.Code.Ledger.Utxo.C_'10214'_'44'_'44'_'44'_'10215''7512'_1060
      (coe
         MAlonzo.Code.Axiom.Set.Map.du_fromList'7504'_492
         (coe
            MAlonzo.Code.Axiom.Set.d_th_1374
            (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
         (coe
            MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
            (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
            (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30))
         (coe
            MAlonzo.Code.Data.List.Base.du_map_22
            (coe
               MAlonzo.Code.Foreign.Convertible.d_from_20
               (coe
                  MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                  (coe
                     MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                     (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
                  (coe
                     MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                     (coe d_Convertible'45'Addr_1160)
                     (coe
                        MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                        (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)))))
            (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_utxo_170 (coe v0))))
      (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_fees_172 (coe v0))
      (coe
         MAlonzo.Code.Axiom.Set.Map.du_'8709''7504'_488
         (coe
            MAlonzo.Code.Axiom.Set.d_th_1374
            (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12)))
      (let v1 = d_coinTokenAlgebra_190 in
       coe
         MAlonzo.Code.Algebra.Bundles.d_ε_842
         (coe
            MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
            (coe v1)))
-- Ledger.Foreign.HSLedger.utxo-step
utxoStep ::
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOEnv_154 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOState_164 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_TxBody_34 ->
  MAlonzo.Code.Agda.Builtin.Maybe.T_Maybe_10
    () MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOState_164
utxoStep = coe d_utxo'45'step_1416
d_utxo'45'step_1416 ::
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOEnv_154 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOState_164 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_TxBody_34 ->
  Maybe MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOState_164
d_utxo'45'step_1416 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Maybe.Base.du_map_68 d_to''_1406
      (coe
         MAlonzo.Code.Ledger.Utxo.Properties.d_UTXO'45'step_1296
         d_HSTransactionStructure_208 (d_from''_1390 (coe v0))
         (d_from''_1412 (coe v1)) (d_from''_1214 (coe v2)))
-- Ledger.Foreign.HSLedger.utxow-step
utxowStep ::
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOEnv_154 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOState_164 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_Tx_70 ->
  MAlonzo.Code.Agda.Builtin.Maybe.T_Maybe_10
    () MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOState_164
utxowStep = coe d_utxow'45'step_1424
d_utxow'45'step_1424 ::
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOEnv_154 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOState_164 ->
  MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_Tx_70 ->
  Maybe MAlonzo.Code.Ledger.Foreign.LedgerTypes.T_UTxOState_164
d_utxow'45'step_1424 v0 v1 v2
  = coe
      MAlonzo.Code.Data.Maybe.Base.du_map_68 d_to''_1406
      (coe
         MAlonzo.Code.Interface.Decidable.Instance.du_if'7496'_then_else__44
         (coe ())
         (coe
            MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
            (coe
               MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
               (coe
                  MAlonzo.Code.Ledger.Utxow.Properties.d_sig'45'inst_898
                  (coe d_HSTransactionStructure_208)
                  (coe
                     MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_txid_58
                     (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_body_78 (coe v2))))
               (coe
                  MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                  (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                  (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30))
               (coe
                  MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
                  (coe
                     MAlonzo.Code.Axiom.Set.Map.du_fromList'7504'_492
                     (coe
                        MAlonzo.Code.Axiom.Set.d_th_1374
                        (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                     (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                     (coe
                        MAlonzo.Code.Data.List.Base.du_map_22
                        (coe
                           MAlonzo.Code.Foreign.Convertible.d_from_20
                           (coe
                              MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                              (coe
                                 MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                 (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
                              (coe
                                 MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                 (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))))
                        (coe
                           MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_vkSigs_66
                           (coe
                              MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_wits_80 (coe v2)))))))
            (coe
               MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
               (coe
                  MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                  (coe
                     MAlonzo.Code.Ledger.Utxow.Properties.d_valid'45'inst_912
                     (coe d_HSTransactionStructure_208)
                     (coe
                        MAlonzo.Code.Axiom.Set.du_map_360
                        (MAlonzo.Code.Axiom.Set.d_th_1374
                           (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                        (MAlonzo.Code.Interface.Hashable.d_hash_16
                           (coe
                              MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
                              (coe d_isHashableSet'45'ℕ_152)))
                        (coe
                           MAlonzo.Code.Axiom.Set.Rel.du_dom_290
                           (MAlonzo.Code.Axiom.Set.d_th_1374
                              (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                           (coe
                              MAlonzo.Code.Axiom.Set.Map.du__'738'_458
                              (coe
                                 MAlonzo.Code.Axiom.Set.Map.du_fromList'7504'_492
                                 (coe
                                    MAlonzo.Code.Axiom.Set.d_th_1374
                                    (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                 (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du_map_22
                                    (coe
                                       MAlonzo.Code.Foreign.Convertible.d_from_20
                                       (coe
                                          MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                                          (coe
                                             MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                             (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
                                          (coe
                                             MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                             (coe
                                                MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))))
                                    (coe
                                       MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_vkSigs_66
                                       (coe
                                          MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_wits_80
                                          (coe v2))))))))
                     (coe
                        MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
                        (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)
                        (MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_txvldt_54
                           (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_body_78 (coe v2)))))
                  (coe
                     MAlonzo.Code.Ledger.Script.d_DecEq'45'P1Script_44
                     (coe
                        MAlonzo.Code.Ledger.Script.d_p1s_126
                        (coe d_HSScriptStructure_188)))
                  (coe
                     MAlonzo.Code.Ledger.Utxow.du_scriptsP1_872
                     (coe
                        d_from''_1248
                        (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_wits_80 (coe v2)))))
               (coe
                  MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                  (coe
                     MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                     (coe
                        MAlonzo.Code.Ledger.Utxo.du_'8712''45'inst_1076
                        (coe
                           MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                           (coe d_isHashableSet'45'ℕ_152))
                        (coe
                           MAlonzo.Code.Axiom.Set.du_map_360
                           (MAlonzo.Code.Axiom.Set.d_th_1374
                              (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                           (MAlonzo.Code.Interface.Hashable.d_hash_16
                              (coe
                                 MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
                                 (coe d_isHashableSet'45'ℕ_152)))
                           (coe
                              MAlonzo.Code.Axiom.Set.Rel.du_dom_290
                              (MAlonzo.Code.Axiom.Set.d_th_1374
                                 (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                              (coe
                                 MAlonzo.Code.Axiom.Set.Map.du__'738'_458
                                 (coe
                                    MAlonzo.Code.Axiom.Set.Map.du_fromList'7504'_492
                                    (coe
                                       MAlonzo.Code.Axiom.Set.d_th_1374
                                       (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                    (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                                    (coe
                                       MAlonzo.Code.Data.List.Base.du_map_22
                                       (coe
                                          MAlonzo.Code.Foreign.Convertible.d_from_20
                                          (coe
                                             MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                                             (coe
                                                MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                                (coe
                                                   MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
                                             (coe
                                                MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                                (coe
                                                   MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))))
                                       (coe
                                          MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_vkSigs_66
                                          (coe
                                             MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_wits_80
                                             (coe v2)))))))))
                     (coe
                        MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                        (coe d_isHashableSet'45'ℕ_152))
                     (coe
                        MAlonzo.Code.Ledger.Utxow.d_witsVKeyNeeded_860
                        (coe d_HSTransactionStructure_208)
                        (coe
                           MAlonzo.Code.Axiom.Set.Map.du_fromList'7504'_492
                           (coe
                              MAlonzo.Code.Axiom.Set.d_th_1374
                              (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                           (coe
                              MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                              (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                              (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30))
                           (coe
                              MAlonzo.Code.Data.List.Base.du_map_22
                              (coe
                                 MAlonzo.Code.Foreign.Convertible.d_from_20
                                 (coe
                                    MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                                    (coe
                                       MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                       (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
                                    (coe
                                       MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                                       (coe d_Convertible'45'Addr_1160)
                                       (coe
                                          MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                          (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)))))
                              (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_utxo_170 (coe v1))))
                        (coe
                           d_from''_1214
                           (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_body_78 (coe v2)))))
                  (coe
                     MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                     (coe
                        MAlonzo.Code.Relation.Nullary.Decidable.Core.du__'215''45'dec__62
                        (coe
                           MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                           (coe
                              MAlonzo.Code.Ledger.Utxo.du_'8712''45'inst_1076
                              (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                              (coe
                                 MAlonzo.Code.Axiom.Set.du_map_360
                                 (MAlonzo.Code.Axiom.Set.d_th_1374
                                    (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                 (MAlonzo.Code.Interface.Hashable.d_hash_16
                                    (coe
                                       MAlonzo.Code.Ledger.Script.du_Hashable'45'Script_172
                                       (coe d_HSScriptStructure_188)))
                                 (coe
                                    MAlonzo.Code.Axiom.Set.du_'8709'_404
                                    (coe
                                       MAlonzo.Code.Axiom.Set.d_th_1374
                                       (coe
                                          MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12)))))
                           (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                           (coe
                              MAlonzo.Code.Ledger.Utxow.d_scriptsNeeded_866
                              (coe d_HSTransactionStructure_208)
                              (coe
                                 MAlonzo.Code.Axiom.Set.Map.du_fromList'7504'_492
                                 (coe
                                    MAlonzo.Code.Axiom.Set.d_th_1374
                                    (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                 (coe
                                    MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                                    (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                                    (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30))
                                 (coe
                                    MAlonzo.Code.Data.List.Base.du_map_22
                                    (coe
                                       MAlonzo.Code.Foreign.Convertible.d_from_20
                                       (coe
                                          MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                                          (coe
                                             MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                             (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
                                          (coe
                                             MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                                             (coe d_Convertible'45'Addr_1160)
                                             (coe
                                                MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                                (coe
                                                   MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)))))
                                    (coe
                                       MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_utxo_170
                                       (coe v1))))
                              (coe
                                 d_from''_1214
                                 (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_body_78 (coe v2)))))
                        (coe
                           MAlonzo.Code.Ledger.Utxo.du_all'63'''_1092
                           (coe
                              MAlonzo.Code.Ledger.Utxo.du_'8712''45'inst_1076
                              (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                              (coe
                                 MAlonzo.Code.Ledger.Utxow.d_scriptsNeeded_866
                                 (coe d_HSTransactionStructure_208)
                                 (coe
                                    MAlonzo.Code.Axiom.Set.Map.du_fromList'7504'_492
                                    (coe
                                       MAlonzo.Code.Axiom.Set.d_th_1374
                                       (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                                    (coe
                                       MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
                                       (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                                       (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30))
                                    (coe
                                       MAlonzo.Code.Data.List.Base.du_map_22
                                       (coe
                                          MAlonzo.Code.Foreign.Convertible.d_from_20
                                          (coe
                                             MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                                             (coe
                                                MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                                (coe
                                                   MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40))
                                             (coe
                                                MAlonzo.Code.Foreign.Convertible.du_Convertible'45'Pair_52
                                                (coe d_Convertible'45'Addr_1160)
                                                (coe
                                                   MAlonzo.Code.Foreign.Convertible.du_Coercible'8658'Convertible_38
                                                   (coe
                                                      MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)))))
                                       (coe
                                          MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_utxo_170
                                          (coe v1))))
                                 (coe
                                    d_from''_1214
                                    (coe
                                       MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_body_78
                                       (coe v2)))))
                           (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30)
                           (coe
                              MAlonzo.Code.Axiom.Set.du_map_360
                              (MAlonzo.Code.Axiom.Set.d_th_1374
                                 (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
                              (MAlonzo.Code.Interface.Hashable.d_hash_16
                                 (coe
                                    MAlonzo.Code.Ledger.Script.du_Hashable'45'Script_172
                                    (coe d_HSScriptStructure_188)))
                              (coe
                                 MAlonzo.Code.Axiom.Set.du_'8709'_404
                                 (coe
                                    MAlonzo.Code.Axiom.Set.d_th_1374
                                    (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))))))
                     (coe
                        MAlonzo.Code.Interface.Decidable.Instance.du_DecEq'8658'Dec_88
                        (coe
                           MAlonzo.Code.Interface.DecEq.du_DecEq'45'Maybe_34
                           (coe
                              MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                              (coe d_isHashableSet'45''8868'_150)))
                        (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
                        (coe
                           MAlonzo.Code.Data.Maybe.Base.du_map_68
                           (MAlonzo.Code.Interface.Hashable.d_hash_16
                              (coe
                                 MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
                                 (coe d_isHashableSet'45''8868'_150)))
                           (coe
                              MAlonzo.Code.Foreign.Haskell.Coerce.d_coerce_44 () erased () erased
                              (coe MAlonzo.Code.Foreign.Haskell.Coerce.C_TrustMe_40)
                              (MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_txAD_82 (coe v2)))))))))
         (coe
            MAlonzo.Code.Interface.ComputationalRelation.d_compute_38
            (MAlonzo.Code.Ledger.Utxo.d_Computational'45'UTXO_1348
               (coe d_HSTransactionStructure_208))
            (d_from''_1390 (coe v0)) (d_from''_1412 (coe v1))
            (d_from''_1214
               (coe MAlonzo.Code.Ledger.Foreign.LedgerTypes.d_body_78 (coe v2))))
         (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18))
