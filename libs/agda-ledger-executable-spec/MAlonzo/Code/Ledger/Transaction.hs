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

module MAlonzo.Code.Ledger.Transaction where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Algebra.Bundles
import qualified MAlonzo.Code.Algebra.Morphism
import qualified MAlonzo.Code.Algebra.Structures
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Map
import qualified MAlonzo.Code.Axiom.Set.Rel
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Rational.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Interface.ComputationalRelation
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Interface.Hashable
import qualified MAlonzo.Code.Ledger.Address
import qualified MAlonzo.Code.Ledger.Crypto
import qualified MAlonzo.Code.Ledger.Deleg
import qualified MAlonzo.Code.Ledger.Epoch
import qualified MAlonzo.Code.Ledger.GovernanceActions
import qualified MAlonzo.Code.Ledger.Interface.HasCoin
import qualified MAlonzo.Code.Ledger.PParams
import qualified MAlonzo.Code.Ledger.Prelude
import qualified MAlonzo.Code.Ledger.Script
import qualified MAlonzo.Code.Ledger.TokenAlgebra
import qualified MAlonzo.Code.Relation.Binary.Structures
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Ledger.Transaction.TransactionStructure
d_TransactionStructure_4 = ()
data T_TransactionStructure_4
  = C_TransactionStructure'46'constructor_1497 MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30
                                               MAlonzo.Code.Ledger.Epoch.T_GlobalConstants_4
                                               MAlonzo.Code.Ledger.Crypto.T_Crypto_164
                                               MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
                                               MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
                                               MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260
                                               (AgdaAny -> AgdaAny) AgdaAny
                                               MAlonzo.Code.Ledger.TokenAlgebra.T_TokenAlgebra_4
                                               MAlonzo.Code.Interface.DecEq.T_DecEq_14
                                               MAlonzo.Code.Interface.DecEq.T_DecEq_14
                                               MAlonzo.Code.Interface.DecEq.T_DecEq_14
                                               MAlonzo.Code.Interface.DecEq.T_DecEq_14
                                               MAlonzo.Code.Ledger.Script.T_ScriptStructure_120
-- Ledger.Transaction._.Slot
d_Slot_42 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.Epoch.T_GlobalConstants_4 -> ()
d_Slot_42 = erased
-- Ledger.Transaction._.THash
d_THash_96 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.Epoch.T_GlobalConstants_4 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Ledger.TokenAlgebra.T_TokenAlgebra_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_THash_96 = erased
-- Ledger.Transaction._.ScriptHash
d_ScriptHash_102 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.Epoch.T_GlobalConstants_4 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  (AgdaAny -> AgdaAny) ->
  AgdaAny ->
  MAlonzo.Code.Ledger.TokenAlgebra.T_TokenAlgebra_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_ScriptHash_102 = erased
-- Ledger.Transaction.TransactionStructure.Ix
d_Ix_180 :: T_TransactionStructure_4 -> ()
d_Ix_180 = erased
-- Ledger.Transaction.TransactionStructure.TxId
d_TxId_182 :: T_TransactionStructure_4 -> ()
d_TxId_182 = erased
-- Ledger.Transaction.TransactionStructure.AuxiliaryData
d_AuxiliaryData_184 :: T_TransactionStructure_4 -> ()
d_AuxiliaryData_184 = erased
-- Ledger.Transaction.TransactionStructure.epochStructure
d_epochStructure_186 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30
d_epochStructure_186 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.globalConstants
d_globalConstants_188 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Epoch.T_GlobalConstants_4
d_globalConstants_188 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure._._+ᵉ_
d__'43''7497'__192 ::
  T_TransactionStructure_4 -> Integer -> AgdaAny -> AgdaAny
d__'43''7497'__192 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d__'43''7497'__90
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._._+ᵉ'_
d__'43''7497'''__194 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> AgdaAny
d__'43''7497'''__194 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d__'43''7497'''__98
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._._<ˢ_
d__'60''738'__196 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> ()
d__'60''738'__196 = erased
-- Ledger.Transaction.TransactionStructure._._<?_
d__'60''63'__198 ::
  T_TransactionStructure_4 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'60''63'__198 v0
  = let v1 = d_epochStructure_186 (coe v0) in
    coe
      MAlonzo.Code.Relation.Binary.Structures.du__'60''63'__516
      (coe MAlonzo.Code.Ledger.Epoch.d_Slot'45'STO_64 (coe v1))
-- Ledger.Transaction.TransactionStructure._._<ᵉ_
d__'60''7497'__200 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> ()
d__'60''7497'__200 = erased
-- Ledger.Transaction.TransactionStructure._._≤ˢ_
d__'8804''738'__202 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> ()
d__'8804''738'__202 = erased
-- Ledger.Transaction.TransactionStructure._._≤ˢ?_
d__'8804''738''63'__204 ::
  T_TransactionStructure_4 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''738''63'__204 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d__'8804''738''63'__84
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._._≤ᵉ_
d__'8804''7497'__206 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> ()
d__'8804''7497'__206 = erased
-- Ledger.Transaction.TransactionStructure._._≤ᵉ?_
d__'8804''7497''63'__208 ::
  T_TransactionStructure_4 ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d__'8804''7497''63'__208 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d__'8804''7497''63'__262
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._._≥ˢ_
d__'8805''738'__210 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> ()
d__'8805''738'__210 = erased
-- Ledger.Transaction.TransactionStructure._.DecEq-Epoch
d_DecEq'45'Epoch_212 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'Epoch_212 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_DecEq'45'Epoch_70
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._.Epoch
d_Epoch_214 :: T_TransactionStructure_4 -> ()
d_Epoch_214 = erased
-- Ledger.Transaction.TransactionStructure._.Slot
d_Slot_216 :: T_TransactionStructure_4 -> ()
d_Slot_216 = erased
-- Ledger.Transaction.TransactionStructure._.Slot-STO
d_Slot'45'STO_218 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Relation.Binary.Structures.T_IsStrictTotalOrder_498
d_Slot'45'STO_218 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_Slot'45'STO_64
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._.Slotʳ
d_Slot'691'_220 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Algebra.Bundles.T_Semiring_1986
d_Slot'691'_220 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_Slot'691'_52
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._.StabilityWindow
d_StabilityWindow_222 :: T_TransactionStructure_4 -> AgdaAny
d_StabilityWindow_222 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_StabilityWindow_66
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._.epoch
d_epoch_224 :: T_TransactionStructure_4 -> AgdaAny -> AgdaAny
d_epoch_224 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_epoch_58
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._.firstSlot
d_firstSlot_226 :: T_TransactionStructure_4 -> AgdaAny -> AgdaAny
d_firstSlot_226 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_firstSlot_60
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._.sucᵉ
d_suc'7497'_228 :: T_TransactionStructure_4 -> AgdaAny -> AgdaAny
d_suc'7497'_228 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_suc'7497'_68
      (coe d_epochStructure_186 (coe v0))
-- Ledger.Transaction.TransactionStructure._.Network
d_Network_232 :: T_TransactionStructure_4 -> ()
d_Network_232 = erased
-- Ledger.Transaction.TransactionStructure._.NetworkId
d_NetworkId_234 :: T_TransactionStructure_4 -> AgdaAny
d_NetworkId_234 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_NetworkId_28
      (coe d_globalConstants_188 (coe v0))
-- Ledger.Transaction.TransactionStructure._.NonZero-SlotsPerEpoch
d_NonZero'45'SlotsPerEpoch_236 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Data.Nat.Base.T_NonZero_88
d_NonZero'45'SlotsPerEpoch_236 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_NonZero'45'SlotsPerEpoch_22
      (coe d_globalConstants_188 (coe v0))
-- Ledger.Transaction.TransactionStructure._.Quorum
d_Quorum_238 :: T_TransactionStructure_4 -> Integer
d_Quorum_238 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_Quorum_26
      (coe d_globalConstants_188 (coe v0))
-- Ledger.Transaction.TransactionStructure._.SlotsPerEpochᶜ
d_SlotsPerEpoch'7580'_240 :: T_TransactionStructure_4 -> Integer
d_SlotsPerEpoch'7580'_240 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_SlotsPerEpoch'7580'_20
      (coe d_globalConstants_188 (coe v0))
-- Ledger.Transaction.TransactionStructure._.StabilityWindowᶜ
d_StabilityWindow'7580'_242 :: T_TransactionStructure_4 -> Integer
d_StabilityWindow'7580'_242 v0
  = coe
      MAlonzo.Code.Ledger.Epoch.d_StabilityWindow'7580'_24
      (coe d_globalConstants_188 (coe v0))
-- Ledger.Transaction.TransactionStructure.crypto
d_crypto_244 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Ledger.Crypto.T_Crypto_164
d_crypto_244 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.adHashingScheme
d_adHashingScheme_246 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_adHashingScheme_246 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.ppHashingScheme
d_ppHashingScheme_248 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_ppHashingScheme_248 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.ppUpd
d_ppUpd_250 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260
d_ppUpd_250 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.txidBytes
d_txidBytes_252 :: T_TransactionStructure_4 -> AgdaAny -> AgdaAny
d_txidBytes_252 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.networkId
d_networkId_254 :: T_TransactionStructure_4 -> AgdaAny
d_networkId_254 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v11
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.tokenAlgebra
d_tokenAlgebra_256 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.TokenAlgebra.T_TokenAlgebra_4
d_tokenAlgebra_256 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v12
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.DecEq-TxId
d_DecEq'45'TxId_258 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'TxId_258 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v13
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.DecEq-Ix
d_DecEq'45'Ix_260 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'Ix_260 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v14
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.DecEq-Netw
d_DecEq'45'Netw_262 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'Netw_262 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v15
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.DecEq-UpdT
d_DecEq'45'UpdT_264 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'UpdT_264 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v16
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure._.DecEq-THash
d_DecEq'45'THash_268 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'THash_268 v0
  = let v1 = d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
      (coe MAlonzo.Code.Ledger.Crypto.d_khs_240 (coe v1))
-- Ledger.Transaction.TransactionStructure._.THash
d_THash_270 :: T_TransactionStructure_4 -> ()
d_THash_270 = erased
-- Ledger.Transaction.TransactionStructure._.KeyPair
d_KeyPair_272 :: T_TransactionStructure_4 -> ()
d_KeyPair_272 = erased
-- Ledger.Transaction.TransactionStructure._.SKey
d_SKey_274 :: T_TransactionStructure_4 -> ()
d_SKey_274 = erased
-- Ledger.Transaction.TransactionStructure._.ScriptHash
d_ScriptHash_276 :: T_TransactionStructure_4 -> ()
d_ScriptHash_276 = erased
-- Ledger.Transaction.TransactionStructure._.Ser
d_Ser_278 :: T_TransactionStructure_4 -> ()
d_Ser_278 = erased
-- Ledger.Transaction.TransactionStructure._.Sig
d_Sig_280 :: T_TransactionStructure_4 -> ()
d_Sig_280 = erased
-- Ledger.Transaction.TransactionStructure._.T-Hashable
d_T'45'Hashable_282 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_T'45'Hashable_282 v0
  = let v1 = d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
      (coe MAlonzo.Code.Ledger.Crypto.d_khs_240 (coe v1))
-- Ledger.Transaction.TransactionStructure._.VKey
d_VKey_284 :: T_TransactionStructure_4 -> ()
d_VKey_284 = erased
-- Ledger.Transaction.TransactionStructure._.decEq-ScriptHash
d_decEq'45'ScriptHash_286 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'ScriptHash_286 v0
  = coe
      MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
      (coe d_crypto_244 (coe v0))
-- Ledger.Transaction.TransactionStructure._.decEq-Ser
d_decEq'45'Ser_288 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'Ser_288 v0
  = let v1 = d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_decEq'45'Ser_162
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Transaction.TransactionStructure._.decEq-Sig
d_decEq'45'Sig_290 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'Sig_290 v0
  = let v1 = d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_decEq'45'Sig_160
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Transaction.TransactionStructure._.decEq-VKey
d_decEq'45'VKey_292 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_decEq'45'VKey_292 v0
  = let v1 = d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_decEq'45'VKey_158
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Transaction.TransactionStructure._.isKeyPair
d_isKeyPair_294 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> ()
d_isKeyPair_294 = erased
-- Ledger.Transaction.TransactionStructure._.isSigned
d_isSigned_296 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> AgdaAny -> ()
d_isSigned_296 = erased
-- Ledger.Transaction.TransactionStructure._.isSigned-correct
d_isSigned'45'correct_298 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 -> AgdaAny
d_isSigned'45'correct_298 v0
  = let v1 = d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_isSigned'45'correct_156
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Transaction.TransactionStructure._.isSigned?
d_isSigned'63'_300 ::
  T_TransactionStructure_4 ->
  AgdaAny ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isSigned'63'_300 v0
  = let v1 = d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_isSigned'63'_144
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Transaction.TransactionStructure._.khs
d_khs_302 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_khs_302 v0
  = coe
      MAlonzo.Code.Ledger.Crypto.d_khs_240 (coe d_crypto_244 (coe v0))
-- Ledger.Transaction.TransactionStructure._.pkk
d_pkk_304 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Crypto.T_PKKScheme_70
d_pkk_304 v0
  = coe
      MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe d_crypto_244 (coe v0))
-- Ledger.Transaction.TransactionStructure._.sign
d_sign_306 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> AgdaAny
d_sign_306 v0
  = let v1 = d_crypto_244 (coe v0) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_sign_130
      (coe MAlonzo.Code.Ledger.Crypto.d_pkk_210 (coe v1))
-- Ledger.Transaction.TransactionStructure._._∙_
d__'8729'__310 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> AgdaAny
d__'8729'__310 v0
  = let v1 = d_tokenAlgebra_256 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.d__'8729'__840
      (coe
         MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
         (coe v1))
-- Ledger.Transaction.TransactionStructure._._≈_
d__'8776'__312 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> ()
d__'8776'__312 = erased
-- Ledger.Transaction.TransactionStructure._._≤ᵗ_
d__'8804''7511'__314 ::
  T_TransactionStructure_4 -> AgdaAny -> AgdaAny -> ()
d__'8804''7511'__314 = erased
-- Ledger.Transaction.TransactionStructure._.DecEq-Value
d_DecEq'45'Value_316 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'Value_316 v0
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_DecEq'45'Value_72
      (coe d_tokenAlgebra_256 (coe v0))
-- Ledger.Transaction.TransactionStructure._.MemoryEstimate
d_MemoryEstimate_318 :: T_TransactionStructure_4 -> ()
d_MemoryEstimate_318 = erased
-- Ledger.Transaction.TransactionStructure._.PolicyId
d_PolicyId_320 :: T_TransactionStructure_4 -> ()
d_PolicyId_320 = erased
-- Ledger.Transaction.TransactionStructure._.Carrier
d_Carrier_322 :: T_TransactionStructure_4 -> ()
d_Carrier_322 = erased
-- Ledger.Transaction.TransactionStructure._.Value-CommutativeMonoid
d_Value'45'CommutativeMonoid_324 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Algebra.Bundles.T_CommutativeMonoid_820
d_Value'45'CommutativeMonoid_324 v0
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
      (coe d_tokenAlgebra_256 (coe v0))
-- Ledger.Transaction.TransactionStructure._.coin
d_coin_326 :: T_TransactionStructure_4 -> AgdaAny -> Integer
d_coin_326 v0
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_coin_58
      (coe d_tokenAlgebra_256 (coe v0))
-- Ledger.Transaction.TransactionStructure._.coinIsMonoidMorphism
d_coinIsMonoidMorphism_328 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Algebra.Morphism.T_IsCommutativeMonoidMorphism_498
d_coinIsMonoidMorphism_328 v0
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_coinIsMonoidMorphism_70
      (coe d_tokenAlgebra_256 (coe v0))
-- Ledger.Transaction.TransactionStructure._.inject
d_inject_330 :: T_TransactionStructure_4 -> Integer -> AgdaAny
d_inject_330 v0
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_inject_60
      (coe d_tokenAlgebra_256 (coe v0))
-- Ledger.Transaction.TransactionStructure._.policies
d_policies_332 :: T_TransactionStructure_4 -> AgdaAny -> [AgdaAny]
d_policies_332 v0
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_policies_62
      (coe d_tokenAlgebra_256 (coe v0))
-- Ledger.Transaction.TransactionStructure._.property
d_property_334 ::
  T_TransactionStructure_4 ->
  Integer -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_property_334 = erased
-- Ledger.Transaction.TransactionStructure._.refl
d_refl_336 :: T_TransactionStructure_4 -> AgdaAny -> AgdaAny
d_refl_336 v0
  = let v1 = d_tokenAlgebra_256 (coe v0) in
    coe
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
                     (coe
                        MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
                        (coe v1)))))))
-- Ledger.Transaction.TransactionStructure._.size
d_size_338 :: T_TransactionStructure_4 -> AgdaAny -> Integer
d_size_338 v0
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_size_64
      (coe d_tokenAlgebra_256 (coe v0))
-- Ledger.Transaction.TransactionStructure._.sumᵛ
d_sum'7515'_340 :: T_TransactionStructure_4 -> [AgdaAny] -> AgdaAny
d_sum'7515'_340 v0
  = coe
      MAlonzo.Code.Ledger.TokenAlgebra.d_sum'7515'_74
      (coe d_tokenAlgebra_256 (coe v0))
-- Ledger.Transaction.TransactionStructure._.ε
d_ε_342 :: T_TransactionStructure_4 -> AgdaAny
d_ε_342 v0
  = let v1 = d_tokenAlgebra_256 (coe v0) in
    coe
      MAlonzo.Code.Algebra.Bundles.d_ε_842
      (coe
         MAlonzo.Code.Ledger.TokenAlgebra.d_Value'45'CommutativeMonoid_42
         (coe v1))
-- Ledger.Transaction.TransactionStructure._.THash
d_THash_346 :: T_TransactionStructure_4 -> ()
d_THash_346 = erased
-- Ledger.Transaction.TransactionStructure._.DecEq-THash
d_DecEq'45'THash_348 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'THash_348 v0
  = coe
      MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
      (coe d_adHashingScheme_246 (coe v0))
-- Ledger.Transaction.TransactionStructure._.T-Hashable
d_T'45'Hashable_350 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_T'45'Hashable_350 v0
  = coe
      MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
      (coe d_adHashingScheme_246 (coe v0))
-- Ledger.Transaction.TransactionStructure.ss
d_ss_352 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Script.T_ScriptStructure_120
d_ss_352 v0
  = case coe v0 of
      C_TransactionStructure'46'constructor_1497 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
        -> coe v17
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure._.CostModel
d_CostModel_356 :: T_TransactionStructure_4 -> ()
d_CostModel_356 = erased
-- Ledger.Transaction.TransactionStructure._.T
d_T_358 :: T_TransactionStructure_4 -> ()
d_T_358 = erased
-- Ledger.Transaction.TransactionStructure._.THash
d_THash_360 :: T_TransactionStructure_4 -> ()
d_THash_360 = erased
-- Ledger.Transaction.TransactionStructure._.Dataʰ
d_Data'688'_362 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Crypto.T_HashableSet_52
d_Data'688'_362 v0
  = coe
      MAlonzo.Code.Ledger.Script.d_Data'688'_84
      (coe MAlonzo.Code.Ledger.Script.d_ps_128 (coe d_ss_352 (coe v0)))
-- Ledger.Transaction.TransactionStructure._.DecEq-P1Script
d_DecEq'45'P1Script_364 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'P1Script_364 v0
  = coe
      MAlonzo.Code.Ledger.Script.d_DecEq'45'P1Script_44
      (coe MAlonzo.Code.Ledger.Script.d_p1s_126 (coe d_ss_352 (coe v0)))
-- Ledger.Transaction.TransactionStructure._.DecEq-PlutusScript
d_DecEq'45'PlutusScript_366 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'PlutusScript_366 v0
  = coe
      MAlonzo.Code.Ledger.Script.d_DecEq'45'PlutusScript_94
      (coe MAlonzo.Code.Ledger.Script.d_ps_128 (coe d_ss_352 (coe v0)))
-- Ledger.Transaction.TransactionStructure._.DecEq-THash
d_DecEq'45'THash_368 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'THash_368 v0
  = let v1
          = MAlonzo.Code.Ledger.Script.d_Data'688'_84
              (coe
                 MAlonzo.Code.Ledger.Script.d_ps_128 (coe d_ss_352 (coe v0))) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
      (coe MAlonzo.Code.Ledger.Crypto.d_T'45'isHashable_60 (coe v1))
-- Ledger.Transaction.TransactionStructure._.ExUnits
d_ExUnits_370 :: T_TransactionStructure_4 -> ()
d_ExUnits_370 = erased
-- Ledger.Transaction.TransactionStructure._.Hashable-P1Script
d_Hashable'45'P1Script_372 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_Hashable'45'P1Script_372 v0
  = coe
      MAlonzo.Code.Ledger.Script.d_Hashable'45'P1Script_42
      (coe MAlonzo.Code.Ledger.Script.d_p1s_126 (coe d_ss_352 (coe v0)))
-- Ledger.Transaction.TransactionStructure._.Hashable-PlutusScript
d_Hashable'45'PlutusScript_374 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_Hashable'45'PlutusScript_374 v0
  = coe
      MAlonzo.Code.Ledger.Script.d_Hashable'45'PlutusScript_92
      (coe MAlonzo.Code.Ledger.Script.d_ps_128 (coe d_ss_352 (coe v0)))
-- Ledger.Transaction.TransactionStructure._.Hashable-Script
d_Hashable'45'Script_376 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_Hashable'45'Script_376 v0
  = coe
      MAlonzo.Code.Ledger.Script.du_Hashable'45'Script_172
      (coe d_ss_352 (coe v0))
-- Ledger.Transaction.TransactionStructure._.P1Script
d_P1Script_378 :: T_TransactionStructure_4 -> ()
d_P1Script_378 = erased
-- Ledger.Transaction.TransactionStructure._.PlutusScript
d_PlutusScript_380 :: T_TransactionStructure_4 -> ()
d_PlutusScript_380 = erased
-- Ledger.Transaction.TransactionStructure._.Script
d_Script_382 :: T_TransactionStructure_4 -> ()
d_Script_382 = erased
-- Ledger.Transaction.TransactionStructure._.T-Hashable
d_T'45'Hashable_384 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.Hashable.T_Hashable_8
d_T'45'Hashable_384 v0
  = let v1
          = MAlonzo.Code.Ledger.Script.d_Data'688'_84
              (coe
                 MAlonzo.Code.Ledger.Script.d_ps_128 (coe d_ss_352 (coe v0))) in
    coe
      MAlonzo.Code.Ledger.Crypto.d_T'45'Hashable_18
      (coe MAlonzo.Code.Ledger.Crypto.d_T'45'isHashable_60 (coe v1))
-- Ledger.Transaction.TransactionStructure._.T-isHashable
d_T'45'isHashable_386 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6
d_T'45'isHashable_386 v0
  = coe
      MAlonzo.Code.Ledger.Crypto.d_T'45'isHashable_60
      (coe
         MAlonzo.Code.Ledger.Script.d_Data'688'_84
         (coe MAlonzo.Code.Ledger.Script.d_ps_128 (coe d_ss_352 (coe v0))))
-- Ledger.Transaction.TransactionStructure._.p1s
d_p1s_388 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Script.T_P1ScriptStructure_12
d_p1s_388 v0
  = coe MAlonzo.Code.Ledger.Script.d_p1s_126 (coe d_ss_352 (coe v0))
-- Ledger.Transaction.TransactionStructure._.ps
d_ps_390 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Script.T_PlutusStructure_46
d_ps_390 v0
  = coe MAlonzo.Code.Ledger.Script.d_ps_128 (coe d_ss_352 (coe v0))
-- Ledger.Transaction.TransactionStructure._.validP1Script
d_validP1Script_392 ::
  T_TransactionStructure_4 ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny -> ()
d_validP1Script_392 = erased
-- Ledger.Transaction.TransactionStructure._.validP1Script?
d_validP1Script'63'_394 ::
  T_TransactionStructure_4 ->
  [AgdaAny] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_validP1Script'63'_394 v0
  = coe
      MAlonzo.Code.Ledger.Script.d_validP1Script'63'_40
      (coe MAlonzo.Code.Ledger.Script.d_p1s_126 (coe d_ss_352 (coe v0)))
-- Ledger.Transaction.TransactionStructure._.validPlutusScript
d_validPlutusScript_396 ::
  T_TransactionStructure_4 ->
  AgdaAny -> [AgdaAny] -> AgdaAny -> AgdaAny -> ()
d_validPlutusScript_396 = erased
-- Ledger.Transaction.TransactionStructure._.validPlutusScript?
d_validPlutusScript'63'_398 ::
  T_TransactionStructure_4 ->
  AgdaAny ->
  [AgdaAny] ->
  AgdaAny ->
  AgdaAny -> MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_validPlutusScript'63'_398 v0
  = coe
      MAlonzo.Code.Ledger.Script.d_validPlutusScript'63'_118
      (coe MAlonzo.Code.Ledger.Script.d_ps_128 (coe d_ss_352 (coe v0)))
-- Ledger.Transaction.TransactionStructure._.UpdateT
d_UpdateT_534 :: T_TransactionStructure_4 -> ()
d_UpdateT_534 = erased
-- Ledger.Transaction.TransactionStructure._.applyUpdate
d_applyUpdate_536 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  AgdaAny -> MAlonzo.Code.Ledger.PParams.T_PParams_128
d_applyUpdate_536 v0
  = coe
      MAlonzo.Code.Ledger.PParams.d_applyUpdate_280
      (coe d_ppUpd_250 (coe v0))
-- Ledger.Transaction.TransactionStructure._.ppdWellFormed
d_ppdWellFormed_538 :: T_TransactionStructure_4 -> AgdaAny -> Bool
d_ppdWellFormed_538 v0
  = coe
      MAlonzo.Code.Ledger.PParams.d_ppdWellFormed_282
      (coe d_ppUpd_250 (coe v0))
-- Ledger.Transaction.TransactionStructure._.ppdWellFormed⇒WF
d_ppdWellFormed'8658'WF_540 ::
  T_TransactionStructure_4 ->
  AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_ppdWellFormed'8658'WF_540 = erased
-- Ledger.Transaction.TransactionStructure._.updateGroups
d_updateGroups_542 ::
  T_TransactionStructure_4 ->
  AgdaAny -> [MAlonzo.Code.Ledger.PParams.T_PParamGroup_58]
d_updateGroups_542 v0
  = coe
      MAlonzo.Code.Ledger.PParams.d_updateGroups_278
      (coe d_ppUpd_250 (coe v0))
-- Ledger.Transaction.TransactionStructure._._⊢_⇀⦇_,ENACT⦈_
d__'8866'_'8640''10631'_'44'ENACT'10632'__546 a0 a1 a2 a3 a4 a5
  = ()
-- Ledger.Transaction.TransactionStructure._.2ℚ
d_2ℚ_548 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_2ℚ_548 ~v0 ~v1 = du_2ℚ_548
du_2ℚ_548 :: MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_2ℚ_548 = coe MAlonzo.Code.Ledger.GovernanceActions.du_2ℚ_332
-- Ledger.Transaction.TransactionStructure._.Anchor
d_Anchor_550 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.DecEq-GovRole
d_DecEq'45'GovRole_558 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'GovRole_558 ~v0 ~v1 = du_DecEq'45'GovRole_558
du_DecEq'45'GovRole_558 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'GovRole_558
  = coe MAlonzo.Code.Ledger.GovernanceActions.du_DecEq'45'GovRole_552
-- Ledger.Transaction.TransactionStructure._.DecEq-VDeleg
d_DecEq'45'VDeleg_560 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'VDeleg_560 v0 ~v1 = du_DecEq'45'VDeleg_560 v0
du_DecEq'45'VDeleg_560 ::
  T_TransactionStructure_4 -> MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'VDeleg_560 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.du_DecEq'45'VDeleg_556
      (coe d_crypto_244 (coe v0))
-- Ledger.Transaction.TransactionStructure._.DecEq-Vote
d_DecEq'45'Vote_562 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'Vote_562 ~v0 ~v1 = du_DecEq'45'Vote_562
du_DecEq'45'Vote_562 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'Vote_562
  = coe MAlonzo.Code.Ledger.GovernanceActions.du_DecEq'45'Vote_554
-- Ledger.Transaction.TransactionStructure._.EnactEnv
d_EnactEnv_578 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.EnactState
d_EnactState_580 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.GovAction
d_GovAction_582 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.GovActionID
d_GovActionID_584 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_GovActionID_584 = erased
-- Ledger.Transaction.TransactionStructure._.GovProposal
d_GovProposal_586 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.GovRole
d_GovRole_588 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.GovVote
d_GovVote_590 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.HashProtected
d_HashProtected_592 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> () -> ()
d_HashProtected_592 = erased
-- Ledger.Transaction.TransactionStructure._.NeedsHash
d_NeedsHash_596 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovAction_362 -> ()
d_NeedsHash_596 = erased
-- Ledger.Transaction.TransactionStructure._.VDeleg
d_VDeleg_610 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.Vote
d_Vote_612 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.actionWellFormed
d_actionWellFormed_618 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovAction_362 -> Bool
d_actionWellFormed_618 v0 ~v1 = du_actionWellFormed_618 v0
du_actionWellFormed_618 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovAction_362 -> Bool
du_actionWellFormed_618 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.du_actionWellFormed_378
      (coe d_ppUpd_250 (coe v0))
-- Ledger.Transaction.TransactionStructure._.maximum
d_maximum_622 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [MAlonzo.Code.Data.Rational.Base.T_ℚ_6] ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_maximum_622 ~v0 ~v1 = du_maximum_622
du_maximum_622 ::
  [MAlonzo.Code.Data.Rational.Base.T_ℚ_6] ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_maximum_622
  = coe MAlonzo.Code.Ledger.GovernanceActions.du_maximum_382
-- Ledger.Transaction.TransactionStructure._.threshold
d_threshold_626 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovAction_362 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovRole_336 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_threshold_626 v0 ~v1 = du_threshold_626 v0
du_threshold_626 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovAction_362 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovRole_336 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_threshold_626 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.du_threshold_478
      (coe d_ppUpd_250 (coe v0))
-- Ledger.Transaction.TransactionStructure._.Anchor.hash
d_hash_648 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352 -> AgdaAny
d_hash_648 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_hash_360 (coe v0)
-- Ledger.Transaction.TransactionStructure._.Anchor.url
d_url_650 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352 ->
  MAlonzo.Code.Agda.Builtin.String.T_String_6
d_url_650 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_url_358 (coe v0)
-- Ledger.Transaction.TransactionStructure._.EnactEnv.gid
d_gid_654 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_EnactEnv_562 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_gid_654 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_gid_566 (coe v0)
-- Ledger.Transaction.TransactionStructure._.EnactState.cc
d_cc_658 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_EnactState_570 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cc_658 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_cc_584 (coe v0)
-- Ledger.Transaction.TransactionStructure._.EnactState.constitution
d_constitution_660 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_EnactState_570 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_constitution_660 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.d_constitution_586 (coe v0)
-- Ledger.Transaction.TransactionStructure._.EnactState.pparams
d_pparams_662 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_EnactState_570 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pparams_662 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_pparams_590 (coe v0)
-- Ledger.Transaction.TransactionStructure._.EnactState.pv
d_pv_664 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_EnactState_570 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pv_664 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_pv_588 (coe v0)
-- Ledger.Transaction.TransactionStructure._.EnactState.treasury
d_treasury_666 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_EnactState_570 -> Integer
d_treasury_666 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_treasury_594 (coe v0)
-- Ledger.Transaction.TransactionStructure._.EnactState.withdrawals
d_withdrawals_668 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_EnactState_570 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_withdrawals_668 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.d_withdrawals_592 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GovProposal.action
d_action_688 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovAction_362
d_action_688 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_action_544 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GovProposal.anchor
d_anchor_690 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352
d_anchor_690 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_anchor_548 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GovProposal.prevAction
d_prevAction_692 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532 -> AgdaAny
d_prevAction_692 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.d_prevAction_546 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GovProposal.returnAddr
d_returnAddr_694 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532 ->
  MAlonzo.Code.Ledger.Address.T_RwdAddr_58
d_returnAddr_694 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.d_returnAddr_542 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GovVote.anchor
d_anchor_706 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510 ->
  Maybe MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352
d_anchor_706 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_anchor_530 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GovVote.credential
d_credential_708 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_credential_708 v0
  = coe
      MAlonzo.Code.Ledger.GovernanceActions.d_credential_526 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GovVote.gid
d_gid_710 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_gid_710 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_gid_522 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GovVote.role
d_role_712 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_GovRole_336
d_role_712 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_role_524 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GovVote.vote
d_vote_714 ::
  MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_Vote_502
d_vote_714 v0
  = coe MAlonzo.Code.Ledger.GovernanceActions.d_vote_528 (coe v0)
-- Ledger.Transaction.TransactionStructure._.Addr
d_Addr_734 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Addr_734 = erased
-- Ledger.Transaction.TransactionStructure._.BaseAddr
d_BaseAddr_736 a0 a1 a2 a3 = ()
-- Ledger.Transaction.TransactionStructure._.BootstrapAddr
d_BootstrapAddr_738 a0 a1 a2 a3 = ()
-- Ledger.Transaction.TransactionStructure._.Credential
d_Credential_740 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Credential_740 = erased
-- Ledger.Transaction.TransactionStructure._.DecEq-BaseAddr
d_DecEq'45'BaseAddr_742 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'BaseAddr_742 ~v0 v1 v2 v3
  = du_DecEq'45'BaseAddr_742 v1 v2 v3
du_DecEq'45'BaseAddr_742 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'BaseAddr_742 v0 v1 v2
  = coe
      MAlonzo.Code.Ledger.Address.du_DecEq'45'BaseAddr_114 (coe v0)
      (coe v1) (coe v2)
-- Ledger.Transaction.TransactionStructure._.DecEq-BootstrapAddr
d_DecEq'45'BootstrapAddr_744 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'BootstrapAddr_744 ~v0 v1 v2 v3
  = du_DecEq'45'BootstrapAddr_744 v1 v2 v3
du_DecEq'45'BootstrapAddr_744 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'BootstrapAddr_744 v0 v1 v2
  = coe
      MAlonzo.Code.Ledger.Address.du_DecEq'45'BootstrapAddr_116 (coe v0)
      (coe v1) (coe v2)
-- Ledger.Transaction.TransactionStructure._.DecEq-RwdAddr
d_DecEq'45'RwdAddr_746 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'RwdAddr_746 ~v0 v1 v2 v3
  = du_DecEq'45'RwdAddr_746 v1 v2 v3
du_DecEq'45'RwdAddr_746 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'RwdAddr_746 v0 v1 v2
  = coe
      MAlonzo.Code.Ledger.Address.du_DecEq'45'RwdAddr_118 (coe v0)
      (coe v1) (coe v2)
-- Ledger.Transaction.TransactionStructure._.RwdAddr
d_RwdAddr_748 a0 a1 a2 a3 = ()
-- Ledger.Transaction.TransactionStructure._.ScriptAddr
d_ScriptAddr_752 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_ScriptAddr_752 = erased
-- Ledger.Transaction.TransactionStructure._.ScriptBaseAddr
d_ScriptBaseAddr_754 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_ScriptBaseAddr_754 = erased
-- Ledger.Transaction.TransactionStructure._.ScriptBootstrapAddr
d_ScriptBootstrapAddr_756 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_ScriptBootstrapAddr_756 = erased
-- Ledger.Transaction.TransactionStructure._.VKeyAddr
d_VKeyAddr_758 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_VKeyAddr_758 = erased
-- Ledger.Transaction.TransactionStructure._.VKeyBaseAddr
d_VKeyBaseAddr_760 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_VKeyBaseAddr_760 = erased
-- Ledger.Transaction.TransactionStructure._.VKeyBootstrapAddr
d_VKeyBootstrapAddr_762 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_VKeyBootstrapAddr_762 = erased
-- Ledger.Transaction.TransactionStructure._.isScript
d_isScript_766 a0 a1 a2 a3 a4 = ()
-- Ledger.Transaction.TransactionStructure._.isVKey
d_isVKey_768 a0 a1 a2 a3 a4 = ()
-- Ledger.Transaction.TransactionStructure._.isVKey?
d_isVKey'63'_770 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isVKey'63'_770 ~v0 ~v1 ~v2 ~v3 = du_isVKey'63'_770
du_isVKey'63'_770 ::
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_isVKey'63'_770
  = coe MAlonzo.Code.Ledger.Address.du_isVKey'63'_106
-- Ledger.Transaction.TransactionStructure._.isVKeyAddr
d_isVKeyAddr_772 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> ()
d_isVKeyAddr_772 = erased
-- Ledger.Transaction.TransactionStructure._.isVKeyAddr?
d_isVKeyAddr'63'_774 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
d_isVKeyAddr'63'_774 ~v0 ~v1 ~v2 ~v3 = du_isVKeyAddr'63'_774
du_isVKeyAddr'63'_774 ::
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Relation.Nullary.Decidable.Core.T_Dec_20
du_isVKeyAddr'63'_774
  = coe MAlonzo.Code.Ledger.Address.du_isVKeyAddr'63'_112
-- Ledger.Transaction.TransactionStructure._.netId
d_netId_776 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
d_netId_776 ~v0 ~v1 ~v2 ~v3 = du_netId_776
du_netId_776 :: MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> AgdaAny
du_netId_776 = coe MAlonzo.Code.Ledger.Address.du_netId_92
-- Ledger.Transaction.TransactionStructure._.payCred
d_payCred_778 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_payCred_778 ~v0 ~v1 ~v2 ~v3 = du_payCred_778
du_payCred_778 ::
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_payCred_778 = coe MAlonzo.Code.Ledger.Address.du_payCred_90
-- Ledger.Transaction.TransactionStructure._.BaseAddr.net
d_net_782 :: MAlonzo.Code.Ledger.Address.T_BaseAddr_30 -> AgdaAny
d_net_782 v0 = coe MAlonzo.Code.Ledger.Address.d_net_38 (coe v0)
-- Ledger.Transaction.TransactionStructure._.BaseAddr.pay
d_pay_784 ::
  MAlonzo.Code.Ledger.Address.T_BaseAddr_30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_pay_784 v0 = coe MAlonzo.Code.Ledger.Address.d_pay_40 (coe v0)
-- Ledger.Transaction.TransactionStructure._.BaseAddr.stake
d_stake_786 ::
  MAlonzo.Code.Ledger.Address.T_BaseAddr_30 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_stake_786 v0
  = coe MAlonzo.Code.Ledger.Address.d_stake_42 (coe v0)
-- Ledger.Transaction.TransactionStructure._.BootstrapAddr.attrsSize
d_attrsSize_790 ::
  MAlonzo.Code.Ledger.Address.T_BootstrapAddr_44 -> Integer
d_attrsSize_790 v0
  = coe MAlonzo.Code.Ledger.Address.d_attrsSize_56 (coe v0)
-- Ledger.Transaction.TransactionStructure._.BootstrapAddr.net
d_net_792 ::
  MAlonzo.Code.Ledger.Address.T_BootstrapAddr_44 -> AgdaAny
d_net_792 v0 = coe MAlonzo.Code.Ledger.Address.d_net_52 (coe v0)
-- Ledger.Transaction.TransactionStructure._.BootstrapAddr.pay
d_pay_794 ::
  MAlonzo.Code.Ledger.Address.T_BootstrapAddr_44 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_pay_794 v0 = coe MAlonzo.Code.Ledger.Address.d_pay_54 (coe v0)
-- Ledger.Transaction.TransactionStructure._.RwdAddr.net
d_net_798 :: MAlonzo.Code.Ledger.Address.T_RwdAddr_58 -> AgdaAny
d_net_798 v0 = coe MAlonzo.Code.Ledger.Address.d_net_64 (coe v0)
-- Ledger.Transaction.TransactionStructure._.RwdAddr.stake
d_stake_800 ::
  MAlonzo.Code.Ledger.Address.T_RwdAddr_58 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_stake_800 v0
  = coe MAlonzo.Code.Ledger.Address.d_stake_66 (coe v0)
-- Ledger.Transaction.TransactionStructure._._⊢_⇀⦇_,CERTBASE⦈_
d__'8866'_'8640''10631'_'44'CERTBASE'10632'__812 a0 a1 a2 a3 a4 a5
  = ()
-- Ledger.Transaction.TransactionStructure._._⊢_⇀⦇_,CERTS⦈_
d__'8866'_'8640''10631'_'44'CERTS'10632'__814 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.Deleg.T_CertEnv_288 ->
  MAlonzo.Code.Ledger.Deleg.T_CertState_350 ->
  [MAlonzo.Code.Ledger.Deleg.T_DCert_274] ->
  MAlonzo.Code.Ledger.Deleg.T_CertState_350 -> ()
d__'8866'_'8640''10631'_'44'CERTS'10632'__814 = erased
-- Ledger.Transaction.TransactionStructure._._⊢_⇀⦇_,CERT⦈_
d__'8866'_'8640''10631'_'44'CERT'10632'__816 a0 a1 a2 a3 a4 a5 = ()
-- Ledger.Transaction.TransactionStructure._._⊢_⇀⦇_,DELEG⦈_
d__'8866'_'8640''10631'_'44'DELEG'10632'__818 a0 a1 a2 a3 a4 a5
  = ()
-- Ledger.Transaction.TransactionStructure._._⊢_⇀⦇_,GOVCERT⦈_
d__'8866'_'8640''10631'_'44'GOVCERT'10632'__820 a0 a1 a2 a3 a4 a5
  = ()
-- Ledger.Transaction.TransactionStructure._._⊢_⇀⦇_,POOL⦈_
d__'8866'_'8640''10631'_'44'POOL'10632'__822 a0 a1 a2 a3 a4 a5 = ()
-- Ledger.Transaction.TransactionStructure._.CertEnv
d_CertEnv_832 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.CertState
d_CertState_834 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.Computational-DELEG
d_Computational'45'DELEG_836 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.ComputationalRelation.T_Computational_32
d_Computational'45'DELEG_836 v0 ~v1
  = du_Computational'45'DELEG_836 v0
du_Computational'45'DELEG_836 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.ComputationalRelation.T_Computational_32
du_Computational'45'DELEG_836 v0
  = coe
      MAlonzo.Code.Ledger.Deleg.du_Computational'45'DELEG_660
      (coe d_crypto_244 (coe v0))
-- Ledger.Transaction.TransactionStructure._.DCert
d_DCert_838 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.DState
d_DState_842 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.DelegEnv
d_DelegEnv_844 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_DelegEnv_844 = erased
-- Ledger.Transaction.TransactionStructure._.GState
d_GState_852 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.GovCertEnv
d_GovCertEnv_854 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_GovCertEnv_854 = erased
-- Ledger.Transaction.TransactionStructure._.PState
d_PState_860 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.PoolEnv
d_PoolEnv_862 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_PoolEnv_862 = erased
-- Ledger.Transaction.TransactionStructure._.PoolParams
d_PoolParams_864 a0 a1 = ()
-- Ledger.Transaction.TransactionStructure._.insertIfPresent
d_insertIfPresent_872 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_insertIfPresent_872 ~v0 ~v1 = du_insertIfPresent_872
du_insertIfPresent_872 ::
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_insertIfPresent_872 v0 v1 v2 v3 v4 v5
  = coe MAlonzo.Code.Ledger.Deleg.du_insertIfPresent_446 v2 v3 v4 v5
-- Ledger.Transaction.TransactionStructure._.requiredDeposit
d_requiredDeposit_878 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> Integer
d_requiredDeposit_878 ~v0 ~v1 = du_requiredDeposit_878
du_requiredDeposit_878 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> Integer
du_requiredDeposit_878
  = coe MAlonzo.Code.Ledger.Deleg.du_requiredDeposit_428
-- Ledger.Transaction.TransactionStructure._.requiredVDelegDeposit
d_requiredVDelegDeposit_880 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Ledger.GovernanceActions.T_VDeleg_344 -> Integer
d_requiredVDelegDeposit_880 ~v0 ~v1 = du_requiredVDelegDeposit_880
du_requiredVDelegDeposit_880 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Ledger.GovernanceActions.T_VDeleg_344 -> Integer
du_requiredVDelegDeposit_880
  = coe MAlonzo.Code.Ledger.Deleg.du_requiredVDelegDeposit_436
-- Ledger.Transaction.TransactionStructure._.CertEnv.epoch
d_epoch_924 :: MAlonzo.Code.Ledger.Deleg.T_CertEnv_288 -> AgdaAny
d_epoch_924 v0 = coe MAlonzo.Code.Ledger.Deleg.d_epoch_296 (coe v0)
-- Ledger.Transaction.TransactionStructure._.CertEnv.pp
d_pp_926 ::
  MAlonzo.Code.Ledger.Deleg.T_CertEnv_288 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128
d_pp_926 v0 = coe MAlonzo.Code.Ledger.Deleg.d_pp_298 (coe v0)
-- Ledger.Transaction.TransactionStructure._.CertEnv.votes
d_votes_928 ::
  MAlonzo.Code.Ledger.Deleg.T_CertEnv_288 ->
  [MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510]
d_votes_928 v0 = coe MAlonzo.Code.Ledger.Deleg.d_votes_300 (coe v0)
-- Ledger.Transaction.TransactionStructure._.CertState.dState
d_dState_932 ::
  MAlonzo.Code.Ledger.Deleg.T_CertState_350 ->
  MAlonzo.Code.Ledger.Deleg.T_DState_310
d_dState_932 v0
  = coe MAlonzo.Code.Ledger.Deleg.d_dState_358 (coe v0)
-- Ledger.Transaction.TransactionStructure._.CertState.gState
d_gState_934 ::
  MAlonzo.Code.Ledger.Deleg.T_CertState_350 ->
  MAlonzo.Code.Ledger.Deleg.T_GState_338
d_gState_934 v0
  = coe MAlonzo.Code.Ledger.Deleg.d_gState_362 (coe v0)
-- Ledger.Transaction.TransactionStructure._.CertState.pState
d_pState_936 ::
  MAlonzo.Code.Ledger.Deleg.T_CertState_350 ->
  MAlonzo.Code.Ledger.Deleg.T_PState_326
d_pState_936 v0
  = coe MAlonzo.Code.Ledger.Deleg.d_pState_360 (coe v0)
-- Ledger.Transaction.TransactionStructure._.DState.rewards
d_rewards_954 ::
  MAlonzo.Code.Ledger.Deleg.T_DState_310 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rewards_954 v0
  = coe MAlonzo.Code.Ledger.Deleg.d_rewards_322 (coe v0)
-- Ledger.Transaction.TransactionStructure._.DState.stakeDelegs
d_stakeDelegs_956 ::
  MAlonzo.Code.Ledger.Deleg.T_DState_310 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_stakeDelegs_956 v0
  = coe MAlonzo.Code.Ledger.Deleg.d_stakeDelegs_320 (coe v0)
-- Ledger.Transaction.TransactionStructure._.DState.voteDelegs
d_voteDelegs_958 ::
  MAlonzo.Code.Ledger.Deleg.T_DState_310 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_voteDelegs_958 v0
  = coe MAlonzo.Code.Ledger.Deleg.d_voteDelegs_318 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GState.ccHotKeys
d_ccHotKeys_962 ::
  MAlonzo.Code.Ledger.Deleg.T_GState_338 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_ccHotKeys_962 v0
  = coe MAlonzo.Code.Ledger.Deleg.d_ccHotKeys_346 (coe v0)
-- Ledger.Transaction.TransactionStructure._.GState.dreps
d_dreps_964 ::
  MAlonzo.Code.Ledger.Deleg.T_GState_338 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_dreps_964 v0 = coe MAlonzo.Code.Ledger.Deleg.d_dreps_344 (coe v0)
-- Ledger.Transaction.TransactionStructure._.PState.pools
d_pools_968 ::
  MAlonzo.Code.Ledger.Deleg.T_PState_326 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pools_968 v0 = coe MAlonzo.Code.Ledger.Deleg.d_pools_332 (coe v0)
-- Ledger.Transaction.TransactionStructure._.PState.retiring
d_retiring_970 ::
  MAlonzo.Code.Ledger.Deleg.T_PState_326 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_retiring_970 v0
  = coe MAlonzo.Code.Ledger.Deleg.d_retiring_334 (coe v0)
-- Ledger.Transaction.TransactionStructure._.PoolParams.rewardAddr
d_rewardAddr_974 ::
  MAlonzo.Code.Ledger.Deleg.T_PoolParams_268 ->
  MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_rewardAddr_974 v0
  = coe MAlonzo.Code.Ledger.Deleg.d_rewardAddr_272 (coe v0)
-- Ledger.Transaction.TransactionStructure.TxIn
d_TxIn_976 :: T_TransactionStructure_4 -> ()
d_TxIn_976 = erased
-- Ledger.Transaction.TransactionStructure.TxOut
d_TxOut_978 :: T_TransactionStructure_4 -> ()
d_TxOut_978 = erased
-- Ledger.Transaction.TransactionStructure.UTxO
d_UTxO_980 :: T_TransactionStructure_4 -> ()
d_UTxO_980 = erased
-- Ledger.Transaction.TransactionStructure.Wdrl
d_Wdrl_982 :: T_TransactionStructure_4 -> ()
d_Wdrl_982 = erased
-- Ledger.Transaction.TransactionStructure.ProposedPPUpdates
d_ProposedPPUpdates_984 :: T_TransactionStructure_4 -> ()
d_ProposedPPUpdates_984 = erased
-- Ledger.Transaction.TransactionStructure.Update
d_Update_986 :: T_TransactionStructure_4 -> ()
d_Update_986 = erased
-- Ledger.Transaction.TransactionStructure.TxBody
d_TxBody_988 a0 = ()
data T_TxBody_988
  = C_TxBody'46'constructor_3787 [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
                                 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 Integer AgdaAny
                                 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                 [MAlonzo.Code.Ledger.Deleg.T_DCert_274]
                                 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                 [MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510]
                                 [MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532] Integer
                                 (Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14) (Maybe AgdaAny)
                                 (Maybe AgdaAny) Integer AgdaAny
-- Ledger.Transaction.TransactionStructure.TxBody.txins
d_txins_1020 ::
  T_TxBody_988 -> [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_txins_1020 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txouts
d_txouts_1022 ::
  T_TxBody_988 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_txouts_1022 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txfee
d_txfee_1024 :: T_TxBody_988 -> Integer
d_txfee_1024 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.mint
d_mint_1026 :: T_TxBody_988 -> AgdaAny
d_mint_1026 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txvldt
d_txvldt_1028 ::
  T_TxBody_988 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_txvldt_1028 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txcerts
d_txcerts_1030 ::
  T_TxBody_988 -> [MAlonzo.Code.Ledger.Deleg.T_DCert_274]
d_txcerts_1030 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txwdrls
d_txwdrls_1032 ::
  T_TxBody_988 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_txwdrls_1032 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txvote
d_txvote_1034 ::
  T_TxBody_988 ->
  [MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510]
d_txvote_1034 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txprop
d_txprop_1036 ::
  T_TxBody_988 ->
  [MAlonzo.Code.Ledger.GovernanceActions.T_GovProposal_532]
d_txprop_1036 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txdonation
d_txdonation_1038 :: T_TxBody_988 -> Integer
d_txdonation_1038 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txup
d_txup_1040 ::
  T_TxBody_988 -> Maybe MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_txup_1040 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v11
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txADhash
d_txADhash_1042 :: T_TxBody_988 -> Maybe AgdaAny
d_txADhash_1042 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v12
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.netwrk
d_netwrk_1044 :: T_TxBody_988 -> Maybe AgdaAny
d_netwrk_1044 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v13
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txsize
d_txsize_1046 :: T_TxBody_988 -> Integer
d_txsize_1046 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v14
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxBody.txid
d_txid_1048 :: T_TxBody_988 -> AgdaAny
d_txid_1048 v0
  = case coe v0 of
      C_TxBody'46'constructor_3787 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
        -> coe v15
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxWitnesses
d_TxWitnesses_1050 a0 = ()
data T_TxWitnesses_1050
  = C_TxWitnesses'46'constructor_3891 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                      [MAlonzo.Code.Data.Sum.Base.T__'8846'__30]
-- Ledger.Transaction.TransactionStructure.TxWitnesses.vkSigs
d_vkSigs_1056 ::
  T_TxWitnesses_1050 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_vkSigs_1056 v0
  = case coe v0 of
      C_TxWitnesses'46'constructor_3891 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.TxWitnesses.scripts
d_scripts_1058 ::
  T_TxWitnesses_1050 -> [MAlonzo.Code.Data.Sum.Base.T__'8846'__30]
d_scripts_1058 v0
  = case coe v0 of
      C_TxWitnesses'46'constructor_3891 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.Tx
d_Tx_1060 a0 = ()
data T_Tx_1060
  = C_Tx'46'constructor_3963 T_TxBody_988 T_TxWitnesses_1050
                             (Maybe AgdaAny)
-- Ledger.Transaction.TransactionStructure.Tx.body
d_body_1068 :: T_Tx_1060 -> T_TxBody_988
d_body_1068 v0
  = case coe v0 of
      C_Tx'46'constructor_3963 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.Tx.wits
d_wits_1070 :: T_Tx_1060 -> T_TxWitnesses_1050
d_wits_1070 v0
  = case coe v0 of
      C_Tx'46'constructor_3963 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.Tx.txAD
d_txAD_1072 :: T_Tx_1060 -> Maybe AgdaAny
d_txAD_1072 v0
  = case coe v0 of
      C_Tx'46'constructor_3963 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.getValue
d_getValue_1074 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
d_getValue_1074 ~v0 v1 = du_getValue_1074 v1
du_getValue_1074 ::
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 -> AgdaAny
du_getValue_1074 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Transaction.TransactionStructure.txinsVKey
d_txinsVKey_1078 ::
  T_TransactionStructure_4 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14] ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  [MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14]
d_txinsVKey_1078 v0 v1 v2
  = let v3 = MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12 in
    coe
      MAlonzo.Code.Axiom.Set.du__'8745'__666
      (coe MAlonzo.Code.Axiom.Set.d_th_1374 (coe v3))
      (coe
         MAlonzo.Code.Axiom.Set.d_'8712''45'sp_1496
         MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12 erased
         (coe
            MAlonzo.Code.Interface.DecEq.du_DecEq'45'Product_38
            (coe d_DecEq'45'TxId_258 (coe v0))
            (coe d_DecEq'45'Ix_260 (coe v0))))
      (coe v1)
      (coe
         MAlonzo.Code.Axiom.Set.Rel.du_dom_290
         (MAlonzo.Code.Axiom.Set.d_th_1374
            (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
         (coe
            MAlonzo.Code.Axiom.Set.Map.du__'738'_458
            (coe
               MAlonzo.Code.Axiom.Set.Map.du__'8638'''__910
               (coe
                  MAlonzo.Code.Axiom.Set.d_th_1374
                  (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
               (coe v2)
               (coe
                  (\ v4 ->
                     coe
                       MAlonzo.Code.Ledger.Address.du_isVKeyAddr'63'_112
                       (coe MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28 (coe v4)))))))
-- Ledger.Transaction.TransactionStructure.HasCoin-TxOut
d_HasCoin'45'TxOut_1084 ::
  T_TransactionStructure_4 ->
  MAlonzo.Code.Ledger.Interface.HasCoin.T_HasCoin_10
d_HasCoin'45'TxOut_1084 v0
  = coe
      MAlonzo.Code.Ledger.Interface.HasCoin.C_HasCoin'46'constructor_9
      (coe
         (\ v1 ->
            coe
              MAlonzo.Code.Ledger.TokenAlgebra.d_coin_58
              (d_tokenAlgebra_256 (coe v0))
              (MAlonzo.Code.Agda.Builtin.Sigma.d_snd_30 (coe v1))))
