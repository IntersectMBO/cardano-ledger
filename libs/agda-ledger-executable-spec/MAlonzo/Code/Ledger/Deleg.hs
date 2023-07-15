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

module MAlonzo.Code.Ledger.Deleg where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Axiom.Set.Map
import qualified MAlonzo.Code.Data.Irrelevant
import qualified MAlonzo.Code.Data.List.Relation.Unary.Any
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Nat.Properties
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Interface.ComputationalRelation
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Interface.Decidable.Instance
import qualified MAlonzo.Code.Ledger.Address
import qualified MAlonzo.Code.Ledger.Crypto
import qualified MAlonzo.Code.Ledger.Epoch
import qualified MAlonzo.Code.Ledger.GovernanceActions
import qualified MAlonzo.Code.Ledger.PParams
import qualified MAlonzo.Code.Ledger.Prelude
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core

-- Ledger.Deleg._.THash
d_THash_24 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_THash_24 = erased
-- Ledger.Deleg._.Credential
d_Credential_70 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Credential_70 = erased
-- Ledger.Deleg._.RwdAddr
d_RwdAddr_78 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = ()
-- Ledger.Deleg._.PParams
d_PParams_142 a0 a1 a2 a3 a4 a5 a6 a7 = ()
-- Ledger.Deleg._.Anchor
d_Anchor_186 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
-- Ledger.Deleg._.GovVote
d_GovVote_190 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
-- Ledger.Deleg._.VDeleg
d_VDeleg_192 a0 a1 a2 a3 a4 a5 a6 a7 a8 = ()
-- Ledger.Deleg._._+ᵉ'_
d__'43''7497'''__232 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny -> AgdaAny -> AgdaAny
d__'43''7497'''__232 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7
  = du__'43''7497'''__232 v4
du__'43''7497'''__232 ::
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  AgdaAny -> AgdaAny -> AgdaAny
du__'43''7497'''__232 v0
  = coe MAlonzo.Code.Ledger.Epoch.d__'43''7497'''__98 (coe v0)
-- Ledger.Deleg._.Epoch
d_Epoch_252 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Epoch_252 = erased
-- Ledger.Deleg.PoolParams
d_PoolParams_268 a0 a1 a2 a3 a4 a5 a6 a7 = ()
newtype T_PoolParams_268
  = C_PoolParams'46'constructor_1745 MAlonzo.Code.Data.Sum.Base.T__'8846'__30
-- Ledger.Deleg.PoolParams.rewardAddr
d_rewardAddr_272 ::
  T_PoolParams_268 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_rewardAddr_272 v0
  = case coe v0 of
      C_PoolParams'46'constructor_1745 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.DCert
d_DCert_274 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_DCert_274
  = C_delegate_276 MAlonzo.Code.Data.Sum.Base.T__'8846'__30
                   (Maybe MAlonzo.Code.Ledger.GovernanceActions.T_VDeleg_344)
                   (Maybe MAlonzo.Code.Data.Sum.Base.T__'8846'__30) Integer |
    C_regpool_278 MAlonzo.Code.Data.Sum.Base.T__'8846'__30
                  T_PoolParams_268 |
    C_retirepool_280 MAlonzo.Code.Data.Sum.Base.T__'8846'__30 AgdaAny |
    C_regdrep_282 MAlonzo.Code.Data.Sum.Base.T__'8846'__30 Integer
                  MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352 |
    C_deregdrep_284 MAlonzo.Code.Data.Sum.Base.T__'8846'__30 |
    C_ccreghot_286 MAlonzo.Code.Data.Sum.Base.T__'8846'__30
                   (Maybe AgdaAny)
-- Ledger.Deleg.CertEnv
d_CertEnv_288 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_CertEnv_288
  = C_'10214'_'44'_'44'_'10215''7580'_302 AgdaAny
                                          MAlonzo.Code.Ledger.PParams.T_PParams_128
                                          [MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510]
-- Ledger.Deleg.CertEnv.epoch
d_epoch_296 :: T_CertEnv_288 -> AgdaAny
d_epoch_296 v0
  = case coe v0 of
      C_'10214'_'44'_'44'_'10215''7580'_302 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.CertEnv.pp
d_pp_298 ::
  T_CertEnv_288 -> MAlonzo.Code.Ledger.PParams.T_PParams_128
d_pp_298 v0
  = case coe v0 of
      C_'10214'_'44'_'44'_'10215''7580'_302 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.CertEnv.votes
d_votes_300 ::
  T_CertEnv_288 ->
  [MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510]
d_votes_300 v0
  = case coe v0 of
      C_'10214'_'44'_'44'_'10215''7580'_302 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.GovCertEnv
d_GovCertEnv_304 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_GovCertEnv_304 = erased
-- Ledger.Deleg.DelegEnv
d_DelegEnv_306 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_DelegEnv_306 = erased
-- Ledger.Deleg.PoolEnv
d_PoolEnv_308 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_PoolEnv_308 = erased
-- Ledger.Deleg.DState
d_DState_310 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_DState_310
  = C_'10214'_'44'_'44'_'10215''7496'_324 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                          MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                          MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Ledger.Deleg.DState.voteDelegs
d_voteDelegs_318 ::
  T_DState_310 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_voteDelegs_318 v0
  = case coe v0 of
      C_'10214'_'44'_'44'_'10215''7496'_324 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.DState.stakeDelegs
d_stakeDelegs_320 ::
  T_DState_310 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_stakeDelegs_320 v0
  = case coe v0 of
      C_'10214'_'44'_'44'_'10215''7496'_324 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.DState.rewards
d_rewards_322 ::
  T_DState_310 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_rewards_322 v0
  = case coe v0 of
      C_'10214'_'44'_'44'_'10215''7496'_324 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.PState
d_PState_326 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_PState_326
  = C_'10214'_'44'_'10215''7510'_336 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                     MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Ledger.Deleg.PState.pools
d_pools_332 ::
  T_PState_326 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pools_332 v0
  = case coe v0 of
      C_'10214'_'44'_'10215''7510'_336 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.PState.retiring
d_retiring_334 ::
  T_PState_326 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_retiring_334 v0
  = case coe v0 of
      C_'10214'_'44'_'10215''7510'_336 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.GState
d_GState_338 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_GState_338
  = C_'10214'_'44'_'10215''7515'_348 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                     MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Ledger.Deleg.GState.dreps
d_dreps_344 ::
  T_GState_338 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_dreps_344 v0
  = case coe v0 of
      C_'10214'_'44'_'10215''7515'_348 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.GState.ccHotKeys
d_ccHotKeys_346 ::
  T_GState_338 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_ccHotKeys_346 v0
  = case coe v0 of
      C_'10214'_'44'_'10215''7515'_348 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.CertState
d_CertState_350 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_CertState_350
  = C_CertState'46'constructor_8207 T_DState_310 T_PState_326
                                    T_GState_338
-- Ledger.Deleg.CertState.dState
d_dState_358 :: T_CertState_350 -> T_DState_310
d_dState_358 v0
  = case coe v0 of
      C_CertState'46'constructor_8207 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.CertState.pState
d_pState_360 :: T_CertState_350 -> T_PState_326
d_pState_360 v0
  = case coe v0 of
      C_CertState'46'constructor_8207 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.CertState.gState
d_gState_362 :: T_CertState_350 -> T_GState_338
d_gState_362 v0
  = case coe v0 of
      C_CertState'46'constructor_8207 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.requiredDeposit
d_requiredDeposit_428 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> Integer
d_requiredDeposit_428 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du_requiredDeposit_428 v8 v9
du_requiredDeposit_428 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Sum.Base.T__'8846'__30 -> Integer
du_requiredDeposit_428 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v2
        -> coe MAlonzo.Code.Ledger.PParams.d_poolDeposit_184 (coe v0)
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe (0 :: Integer)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.requiredVDelegDeposit
d_requiredVDelegDeposit_436 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Ledger.GovernanceActions.T_VDeleg_344 -> Integer
d_requiredVDelegDeposit_436 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 v9
  = du_requiredVDelegDeposit_436 v8 v9
du_requiredVDelegDeposit_436 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Ledger.GovernanceActions.T_VDeleg_344 -> Integer
du_requiredVDelegDeposit_436 v0 v1
  = case coe v1 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v2
        -> coe MAlonzo.Code.Ledger.PParams.d_poolDeposit_184 (coe v0)
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe (0 :: Integer)
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.insertIfPresent
d_insertIfPresent_446 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  () ->
  () ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_insertIfPresent_446 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 ~v9 v10
                      v11 v12 v13
  = du_insertIfPresent_446 v10 v11 v12 v13
du_insertIfPresent_446 ::
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  AgdaAny ->
  Maybe AgdaAny ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_insertIfPresent_446 v0 v1 v2 v3
  = case coe v2 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v4
        -> coe
             MAlonzo.Code.Axiom.Set.Map.du_insert_688
             (coe
                MAlonzo.Code.Axiom.Set.d_th_1374
                (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
             (coe
                MAlonzo.Code.Axiom.Set.d_'8712''45'sp_1496
                MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12 erased v0)
             (coe v3) (coe v1) (coe v4)
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg._⊢_⇀⦇_,DELEG⦈_
d__'8866'_'8640''10631'_'44'DELEG'10632'__458 a0 a1 a2 a3 a4 a5 a6
                                              a7 a8 a9 a10 a11
  = ()
data T__'8866'_'8640''10631'_'44'DELEG'10632'__458
  = C_DELEG'45'delegate_460
-- Ledger.Deleg._⊢_⇀⦇_,POOL⦈_
d__'8866'_'8640''10631'_'44'POOL'10632'__462 a0 a1 a2 a3 a4 a5 a6
                                             a7 a8 a9 a10 a11
  = ()
data T__'8866'_'8640''10631'_'44'POOL'10632'__462
  = C_POOL'45'regpool_508 | C_POOL'45'retirepool_514 T_PoolParams_268
-- Ledger.Deleg._.rewardAddr
d_rewardAddr_506 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_GeneralizeTel_24343 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_rewardAddr_506 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rewardAddr_506 v8
du_rewardAddr_506 ::
  T_GeneralizeTel_24343 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_rewardAddr_506 v0
  = coe
      d_rewardAddr_272
      (coe d_'46'generalizedField'45'poolParams_24335 v0)
-- Ledger.Deleg._.rewardAddr
d_rewardAddr_512 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_GeneralizeTel_25543 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_rewardAddr_512 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_rewardAddr_512 v8
du_rewardAddr_512 ::
  T_GeneralizeTel_25543 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
du_rewardAddr_512 v0
  = coe
      d_rewardAddr_272
      (coe d_'46'generalizedField'45'poolParams_25531 v0)
-- Ledger.Deleg._⊢_⇀⦇_,GOVCERT⦈_
d__'8866'_'8640''10631'_'44'GOVCERT'10632'__516 a0 a1 a2 a3 a4 a5
                                                a6 a7 a8 a9 a10 a11
  = ()
data T__'8866'_'8640''10631'_'44'GOVCERT'10632'__516
  = C_GOVCERT'45'regdrep_558 MAlonzo.Code.Data.Sum.Base.T__'8846'__30 |
    C_GOVCERT'45'deregdrep_560 MAlonzo.Code.Data.List.Relation.Unary.Any.T_Any_34 |
    C_GOVCERT'45'ccreghot_562
-- Ledger.Deleg._.drepActivity
d_drepActivity_530 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_GeneralizeTel_29219 -> AgdaAny
d_drepActivity_530 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_drepActivity_530 v8
du_drepActivity_530 :: T_GeneralizeTel_29219 -> AgdaAny
du_drepActivity_530 v0
  = coe
      MAlonzo.Code.Ledger.PParams.d_drepActivity_204
      (coe d_'46'generalizedField'45'pp_29203 v0)
-- Ledger.Deleg._.drepDeposit
d_drepDeposit_532 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_GeneralizeTel_29219 -> Integer
d_drepDeposit_532 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_drepDeposit_532 v8
du_drepDeposit_532 :: T_GeneralizeTel_29219 -> Integer
du_drepDeposit_532 v0
  = coe
      MAlonzo.Code.Ledger.PParams.d_drepDeposit_202
      (coe d_'46'generalizedField'45'pp_29203 v0)
-- Ledger.Deleg._⊢_⇀⦇_,CERT⦈_
d__'8866'_'8640''10631'_'44'CERT'10632'__564 a0 a1 a2 a3 a4 a5 a6
                                             a7 a8 a9 a10 a11
  = ()
data T__'8866'_'8640''10631'_'44'CERT'10632'__564
  = C_CERT'45'deleg_566 T_DState_310
                        T__'8866'_'8640''10631'_'44'DELEG'10632'__458 |
    C_CERT'45'vdel_568 T_GState_338
                       T__'8866'_'8640''10631'_'44'GOVCERT'10632'__516 |
    C_CERT'45'pool_570 T_PState_326
                       T__'8866'_'8640''10631'_'44'POOL'10632'__462
-- Ledger.Deleg._⊢_⇀⦇_,CERTBASE⦈_
d__'8866'_'8640''10631'_'44'CERTBASE'10632'__572 a0 a1 a2 a3 a4 a5
                                                 a6 a7 a8 a9 a10 a11
  = ()
newtype T__'8866'_'8640''10631'_'44'CERTBASE'10632'__572
  = C_CERT'45'base_646 MAlonzo.Code.Agda.Builtin.Unit.T_'8868'_6
-- Ledger.Deleg._.drepActivity
d_drepActivity_586 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_GeneralizeTel_38199 -> AgdaAny
d_drepActivity_586 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_drepActivity_586 v8
du_drepActivity_586 :: T_GeneralizeTel_38199 -> AgdaAny
du_drepActivity_586 v0
  = coe
      MAlonzo.Code.Ledger.PParams.d_drepActivity_204
      (coe d_'46'generalizedField'45'pp_38191 v0)
-- Ledger.Deleg._.dState
d_dState_616 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_GeneralizeTel_38199 -> T_DState_310
d_dState_616 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_dState_616 v8
du_dState_616 :: T_GeneralizeTel_38199 -> T_DState_310
du_dState_616 v0
  = coe d_dState_358 (coe d_'46'generalizedField'45'st_38193 v0)
-- Ledger.Deleg._.gState
d_gState_618 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_GeneralizeTel_38199 -> T_GState_338
d_gState_618 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_gState_618 v8
du_gState_618 :: T_GeneralizeTel_38199 -> T_GState_338
du_gState_618 v0
  = coe d_gState_362 (coe d_'46'generalizedField'45'st_38193 v0)
-- Ledger.Deleg._.pState
d_pState_620 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_GeneralizeTel_38199 -> T_PState_326
d_pState_620 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_pState_620 v8
du_pState_620 :: T_GeneralizeTel_38199 -> T_PState_326
du_pState_620 v0
  = coe d_pState_360 (coe d_'46'generalizedField'45'st_38193 v0)
-- Ledger.Deleg._.ccHotKeys
d_ccHotKeys_624 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_GeneralizeTel_38199 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_ccHotKeys_624 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_ccHotKeys_624 v8
du_ccHotKeys_624 ::
  T_GeneralizeTel_38199 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_ccHotKeys_624 v0
  = coe
      d_ccHotKeys_346
      (coe d_gState_362 (coe d_'46'generalizedField'45'st_38193 v0))
-- Ledger.Deleg._.dreps
d_dreps_626 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_GeneralizeTel_38199 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_dreps_626 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 = du_dreps_626 v8
du_dreps_626 ::
  T_GeneralizeTel_38199 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
du_dreps_626 v0
  = coe
      d_dreps_344
      (coe d_gState_362 (coe d_'46'generalizedField'45'st_38193 v0))
-- Ledger.Deleg._⊢_⇀⦇_,CERTS⦈_
d__'8866'_'8640''10631'_'44'CERTS'10632'__648 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  T_CertEnv_288 ->
  T_CertState_350 -> [T_DCert_274] -> T_CertState_350 -> ()
d__'8866'_'8640''10631'_'44'CERTS'10632'__648 = erased
-- Ledger.Deleg.Computational-DELEG
d_Computational'45'DELEG_660 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.ComputationalRelation.T_Computational_32
d_Computational'45'DELEG_660 v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
  = du_Computational'45'DELEG_660 v0
du_Computational'45'DELEG_660 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.ComputationalRelation.T_Computational_32
du_Computational'45'DELEG_660 v0
  = coe
      MAlonzo.Code.Interface.ComputationalRelation.C_MkComputational_42
      (coe
         (\ v1 v2 ->
            case coe v2 of
              C_'10214'_'44'_'44'_'10215''7496'_324 v3 v4 v5
                -> coe
                     (\ v6 ->
                        let v7 = coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 in
                        case coe v6 of
                          C_delegate_276 v8 v9 v10 v11
                            -> coe
                                 MAlonzo.Code.Interface.Decidable.Instance.du_if'7496'_then_else__44
                                 (coe ())
                                 (coe
                                    MAlonzo.Code.Interface.Decidable.Instance.du_DecEq'8658'Dec_88
                                    (coe MAlonzo.Code.Interface.DecEq.d_DecEq'45'ℕ_30) (coe v11)
                                    (coe
                                       MAlonzo.Code.Data.Nat.Base.d__'8852'__138
                                       (coe du_requiredVDelegDeposit_436 (coe v1) (coe v9))
                                       (coe du_requiredDeposit_428 (coe v1) (coe v10))))
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.Maybe.C_just_16
                                    (coe
                                       C_'10214'_'44'_'44'_'10215''7496'_324
                                       (coe
                                          du_insertIfPresent_446
                                          (coe
                                             MAlonzo.Code.Interface.DecEq.du_DecEq'45'Sum_40
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                                (coe MAlonzo.Code.Ledger.Crypto.d_khs_240 (coe v0)))
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                                (coe v0)))
                                          (coe v8) (coe v9) (coe v3))
                                       (coe
                                          du_insertIfPresent_446
                                          (coe
                                             MAlonzo.Code.Interface.DecEq.du_DecEq'45'Sum_40
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                                (coe MAlonzo.Code.Ledger.Crypto.d_khs_240 (coe v0)))
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                                (coe v0)))
                                          (coe v8) (coe v10) (coe v4))
                                       (coe v5)))
                                 (coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18)
                          _ -> coe v7)
              _ -> MAlonzo.RTE.mazUnreachableError))
      (coe
         (\ v1 v2 ->
            seq
              (coe v2)
              (coe
                 (\ v3 v4 ->
                    coe
                      MAlonzo.Code.Function.Bundles.du_mk'8660'_1322
                      (coe du_'46'extendedlambda0_696 (coe v1) (coe v3)) erased))))
-- Ledger.Deleg..extendedlambda0
d_'46'extendedlambda0_696 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 ->
  T_DCert_274 ->
  T_DState_310 ->
  T_DCert_274 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T__'8866'_'8640''10631'_'44'DELEG'10632'__458
d_'46'extendedlambda0_696 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9
                          ~v10 ~v11 ~v12 ~v13 v14
  = du_'46'extendedlambda0_696 v8 v14
du_'46'extendedlambda0_696 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  T_DCert_274 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12 ->
  T__'8866'_'8640''10631'_'44'DELEG'10632'__458
du_'46'extendedlambda0_696 v0 v1
  = case coe v1 of
      C_delegate_276 v2 v3 v4 v5
        -> coe
             (\ v6 ->
                let v7
                      = MAlonzo.Code.Data.Nat.Properties.d__'8799'__2464
                          (coe v5)
                          (coe
                             MAlonzo.Code.Data.Nat.Base.d__'8852'__138
                             (coe du_requiredVDelegDeposit_436 (coe v0) (coe v3))
                             (coe du_requiredDeposit_428 (coe v0) (coe v4))) in
                case coe v7 of
                  MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34 v8 v9
                    -> if coe v8
                         then coe seq (coe v9) (coe C_DELEG'45'delegate_460)
                         else erased
                  _ -> MAlonzo.RTE.mazUnreachableError)
      C_regpool_278 v2 v3 -> erased
      C_retirepool_280 v2 v3 -> erased
      C_regdrep_282 v2 v3 v4 -> erased
      C_deregdrep_284 v2 -> erased
      C_ccreghot_286 v2 v3 -> erased
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-pp
d_'46'generalizedField'45'pp_24333 ::
  T_GeneralizeTel_24343 -> MAlonzo.Code.Ledger.PParams.T_PParams_128
d_'46'generalizedField'45'pp_24333
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-poolParams
d_'46'generalizedField'45'poolParams_24335 ::
  T_GeneralizeTel_24343 -> T_PoolParams_268
d_'46'generalizedField'45'poolParams_24335
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-c
d_'46'generalizedField'45'c_24337 ::
  T_GeneralizeTel_24343 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'46'generalizedField'45'c_24337 = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-pools
d_'46'generalizedField'45'pools_24339 ::
  T_GeneralizeTel_24343 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'46'generalizedField'45'pools_24339
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-retiring
d_'46'generalizedField'45'retiring_24341 ::
  T_GeneralizeTel_24343 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'46'generalizedField'45'retiring_24341
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.GeneralizeTel
d_GeneralizeTel_24343 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_GeneralizeTel_24343
  = C_mkGeneralizeTel_24345 MAlonzo.Code.Ledger.PParams.T_PParams_128
                            T_PoolParams_268 MAlonzo.Code.Data.Sum.Base.T__'8846'__30
                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Ledger.Deleg..generalizedField-poolParams
d_'46'generalizedField'45'poolParams_25531 ::
  T_GeneralizeTel_25543 -> T_PoolParams_268
d_'46'generalizedField'45'poolParams_25531
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-pp
d_'46'generalizedField'45'pp_25533 ::
  T_GeneralizeTel_25543 -> MAlonzo.Code.Ledger.PParams.T_PParams_128
d_'46'generalizedField'45'pp_25533
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-pools
d_'46'generalizedField'45'pools_25535 ::
  T_GeneralizeTel_25543 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'46'generalizedField'45'pools_25535
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-retiring
d_'46'generalizedField'45'retiring_25537 ::
  T_GeneralizeTel_25543 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'46'generalizedField'45'retiring_25537
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-c
d_'46'generalizedField'45'c_25539 ::
  T_GeneralizeTel_25543 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'46'generalizedField'45'c_25539 = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-e
d_'46'generalizedField'45'e_25541 ::
  T_GeneralizeTel_25543 -> AgdaAny
d_'46'generalizedField'45'e_25541 = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.GeneralizeTel
d_GeneralizeTel_25543 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_GeneralizeTel_25543
  = C_mkGeneralizeTel_25545 T_PoolParams_268
                            MAlonzo.Code.Ledger.PParams.T_PParams_128
                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                            MAlonzo.Code.Data.Sum.Base.T__'8846'__30 AgdaAny
-- Ledger.Deleg..generalizedField-pp
d_'46'generalizedField'45'pp_29203 ::
  T_GeneralizeTel_29219 -> MAlonzo.Code.Ledger.PParams.T_PParams_128
d_'46'generalizedField'45'pp_29203
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-d
d_'46'generalizedField'45'd_29205 ::
  T_GeneralizeTel_29219 -> Integer
d_'46'generalizedField'45'd_29205 = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-c
d_'46'generalizedField'45'c_29207 ::
  T_GeneralizeTel_29219 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_'46'generalizedField'45'c_29207 = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-dReps
d_'46'generalizedField'45'dReps_29209 ::
  T_GeneralizeTel_29219 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'46'generalizedField'45'dReps_29209
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-e
d_'46'generalizedField'45'e_29211 ::
  T_GeneralizeTel_29219 -> AgdaAny
d_'46'generalizedField'45'e_29211 = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-vs
d_'46'generalizedField'45'vs_29213 ::
  T_GeneralizeTel_29219 ->
  [MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510]
d_'46'generalizedField'45'vs_29213
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-ccKeys
d_'46'generalizedField'45'ccKeys_29215 ::
  T_GeneralizeTel_29219 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_'46'generalizedField'45'ccKeys_29215
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-an
d_'46'generalizedField'45'an_29217 ::
  T_GeneralizeTel_29219 ->
  MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352
d_'46'generalizedField'45'an_29217
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.GeneralizeTel
d_GeneralizeTel_29219 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_GeneralizeTel_29219
  = C_mkGeneralizeTel_29221 MAlonzo.Code.Ledger.PParams.T_PParams_128
                            Integer MAlonzo.Code.Data.Sum.Base.T__'8846'__30
                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 AgdaAny
                            [MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510]
                            MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                            MAlonzo.Code.Ledger.GovernanceActions.T_Anchor_352
-- Ledger.Deleg..generalizedField-pp
d_'46'generalizedField'45'pp_38191 ::
  T_GeneralizeTel_38199 -> MAlonzo.Code.Ledger.PParams.T_PParams_128
d_'46'generalizedField'45'pp_38191
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-st
d_'46'generalizedField'45'st_38193 ::
  T_GeneralizeTel_38199 -> T_CertState_350
d_'46'generalizedField'45'st_38193
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-vs
d_'46'generalizedField'45'vs_38195 ::
  T_GeneralizeTel_38199 ->
  [MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510]
d_'46'generalizedField'45'vs_38195
  = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg..generalizedField-e
d_'46'generalizedField'45'e_38197 ::
  T_GeneralizeTel_38199 -> AgdaAny
d_'46'generalizedField'45'e_38197 = MAlonzo.RTE.mazUnreachableError
-- Ledger.Deleg.GeneralizeTel
d_GeneralizeTel_38199 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_GeneralizeTel_38199
  = C_mkGeneralizeTel_38201 MAlonzo.Code.Ledger.PParams.T_PParams_128
                            T_CertState_350
                            [MAlonzo.Code.Ledger.GovernanceActions.T_GovVote_510] AgdaAny
