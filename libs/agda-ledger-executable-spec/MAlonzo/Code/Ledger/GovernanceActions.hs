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

module MAlonzo.Code.Ledger.GovernanceActions where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Sigma
import qualified MAlonzo.Code.Agda.Builtin.String
import qualified MAlonzo.Code.Agda.Builtin.Unit
import qualified MAlonzo.Code.Axiom.Set
import qualified MAlonzo.Code.Data.List.Base
import qualified MAlonzo.Code.Data.Nat.Base
import qualified MAlonzo.Code.Data.Rational.Base
import qualified MAlonzo.Code.Data.Sum.Base
import qualified MAlonzo.Code.Data.Sum.Properties
import qualified MAlonzo.Code.Function.Bundles
import qualified MAlonzo.Code.Interface.DecEq
import qualified MAlonzo.Code.Ledger.Address
import qualified MAlonzo.Code.Ledger.Crypto
import qualified MAlonzo.Code.Ledger.Epoch
import qualified MAlonzo.Code.Ledger.PParams
import qualified MAlonzo.Code.Ledger.Prelude
import qualified MAlonzo.Code.Relation.Nullary.Decidable.Core
import qualified MAlonzo.Code.Relation.Nullary.Reflects
import qualified MAlonzo.Code.Tactic.Derive.DecEq

-- Ledger.GovernanceActions._.Epoch
d_Epoch_44 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Epoch_44 = erased
-- Ledger.GovernanceActions._.THash
d_THash_64 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_THash_64 = erased
-- Ledger.GovernanceActions._.Credential
d_Credential_110 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_Credential_110 = erased
-- Ledger.GovernanceActions._.RwdAddr
d_RwdAddr_118 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = ()
-- Ledger.GovernanceActions._.PParamGroup
d_PParamGroup_198 a0 a1 a2 a3 a4 a5 a6 a7 = ()
-- Ledger.GovernanceActions._.PParams
d_PParams_200 a0 a1 a2 a3 a4 a5 a6 a7 = ()
-- Ledger.GovernanceActions._.ProtVer
d_ProtVer_206 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_ProtVer_206 = erased
-- Ledger.GovernanceActions._.UpdateT
d_UpdateT_314 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_UpdateT_314 = erased
-- Ledger.GovernanceActions._.applyUpdate
d_applyUpdate_316 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  AgdaAny -> MAlonzo.Code.Ledger.PParams.T_PParams_128
d_applyUpdate_316 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7
  = du_applyUpdate_316 v4
du_applyUpdate_316 ::
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  AgdaAny -> MAlonzo.Code.Ledger.PParams.T_PParams_128
du_applyUpdate_316 v0
  = coe MAlonzo.Code.Ledger.PParams.d_applyUpdate_280 (coe v0)
-- Ledger.GovernanceActions._.THash
d_THash_328 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_THash_328 = erased
-- Ledger.GovernanceActions.2ℚ
d_2ℚ_332 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_2ℚ_332 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 = du_2ℚ_332
du_2ℚ_332 :: MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_2ℚ_332
  = coe
      MAlonzo.Code.Data.Rational.Base.d__'43'__260
      (coe MAlonzo.Code.Data.Rational.Base.d_1ℚ_172)
      (coe MAlonzo.Code.Data.Rational.Base.d_1ℚ_172)
-- Ledger.GovernanceActions.GovActionID
d_GovActionID_334 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> ()
d_GovActionID_334 = erased
-- Ledger.GovernanceActions.GovRole
d_GovRole_336 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_GovRole_336 = C_CC_338 | C_DRep_340 | C_SPO_342
-- Ledger.GovernanceActions.VDeleg
d_VDeleg_344 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_VDeleg_344
  = C_credVoter_346 T_GovRole_336
                    MAlonzo.Code.Data.Sum.Base.T__'8846'__30 |
    C_abstainRep_348 | C_noConfidenceRep_350
-- Ledger.GovernanceActions.Anchor
d_Anchor_352 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_Anchor_352
  = C_Anchor'46'constructor_3267 MAlonzo.Code.Agda.Builtin.String.T_String_6
                                 AgdaAny
-- Ledger.GovernanceActions.Anchor.url
d_url_358 ::
  T_Anchor_352 -> MAlonzo.Code.Agda.Builtin.String.T_String_6
d_url_358 v0
  = case coe v0 of
      C_Anchor'46'constructor_3267 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.Anchor.hash
d_hash_360 :: T_Anchor_352 -> AgdaAny
d_hash_360 v0
  = case coe v0 of
      C_Anchor'46'constructor_3267 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.GovAction
d_GovAction_362 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_GovAction_362
  = C_NoConfidence_364 |
    C_NewCommittee_366 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 [AgdaAny]
                       MAlonzo.Code.Data.Rational.Base.T_ℚ_6 |
    C_NewConstitution_368 AgdaAny |
    C_TriggerHF_370 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 |
    C_ChangePParams_372 AgdaAny AgdaAny |
    C_TreasuryWdrl_374 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 |
    C_Info_376
-- Ledger.GovernanceActions.actionWellFormed
d_actionWellFormed_378 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> T_GovAction_362 -> Bool
d_actionWellFormed_378 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 v8
  = du_actionWellFormed_378 v4 v8
du_actionWellFormed_378 ::
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  T_GovAction_362 -> Bool
du_actionWellFormed_378 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10 in
    case coe v1 of
      C_ChangePParams_372 v3 v4
        -> coe MAlonzo.Code.Ledger.PParams.d_ppdWellFormed_282 v0 v3
      _ -> coe v2
-- Ledger.GovernanceActions.maximum
d_maximum_382 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  [MAlonzo.Code.Data.Rational.Base.T_ℚ_6] ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_maximum_382 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8
  = du_maximum_382 v8
du_maximum_382 ::
  [MAlonzo.Code.Data.Rational.Base.T_ℚ_6] ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_maximum_382 v0
  = coe
      MAlonzo.Code.Data.List.Base.du_foldl_256
      (coe MAlonzo.Code.Data.Rational.Base.d__'8852'__312)
      (coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170)
      (coe
         MAlonzo.Code.Agda.Builtin.Sigma.d_fst_28
         (coe MAlonzo.Code.Ledger.Prelude.du_finiteness_174 v0))
-- Ledger.GovernanceActions._.ccThreshold
d_ccThreshold_466 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_ccThreshold_466 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 ~v8 v9
  = du_ccThreshold_466 v9
du_ccThreshold_466 ::
  Maybe MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_ccThreshold_466 v0
  = case coe v0 of
      MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v1 -> coe v1
      MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18 -> coe du_2ℚ_332
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions._.pparamThreshold
d_pparamThreshold_472 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  MAlonzo.Code.Ledger.PParams.T_PParamGroup_58 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_pparamThreshold_472 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7 v8 ~v9 v10
  = du_pparamThreshold_472 v8 v10
du_pparamThreshold_472 ::
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  MAlonzo.Code.Ledger.PParams.T_PParamGroup_58 ->
  MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_pparamThreshold_472 v0 v1
  = case coe v1 of
      MAlonzo.Code.Ledger.PParams.C_NetworkGroup_60
        -> coe
             MAlonzo.Code.Ledger.PParams.d_P5a_100
             (coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v0))
      MAlonzo.Code.Ledger.PParams.C_EconomicGroup_62
        -> coe
             MAlonzo.Code.Ledger.PParams.d_P5b_102
             (coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v0))
      MAlonzo.Code.Ledger.PParams.C_TechnicalGroup_64
        -> coe
             MAlonzo.Code.Ledger.PParams.d_P5c_104
             (coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v0))
      MAlonzo.Code.Ledger.PParams.C_GovernanceGroup_66
        -> coe
             MAlonzo.Code.Ledger.PParams.d_P5d_106
             (coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v0))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions._.P5
d_P5_474 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  AgdaAny -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_P5_474 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 v8 ~v9 v10
  = du_P5_474 v4 v8 v10
du_P5_474 ::
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  AgdaAny -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_P5_474 v0 v1 v2
  = coe
      du_maximum_382
      (coe
         MAlonzo.Code.Axiom.Set.du_map_360
         (MAlonzo.Code.Axiom.Set.d_th_1374
            (coe MAlonzo.Code.Ledger.Prelude.d_List'45'Model'7496'_12))
         (coe du_pparamThreshold_472 (coe v1))
         (coe MAlonzo.Code.Ledger.PParams.d_updateGroups_278 v0 v2))
-- Ledger.GovernanceActions._.threshold
d_threshold_478 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  T_GovAction_362 ->
  T_GovRole_336 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
d_threshold_478 ~v0 ~v1 ~v2 ~v3 v4 ~v5 ~v6 ~v7 v8 v9 v10
  = du_threshold_478 v4 v8 v9 v10
du_threshold_478 ::
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.PParams.T_PParams_128 ->
  Maybe MAlonzo.Code.Data.Rational.Base.T_ℚ_6 ->
  T_GovAction_362 ->
  T_GovRole_336 -> MAlonzo.Code.Data.Rational.Base.T_ℚ_6
du_threshold_478 v0 v1 v2 v3
  = case coe v3 of
      C_NoConfidence_364
        -> coe
             (\ v4 ->
                case coe v4 of
                  C_CC_338 -> coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
                  C_DRep_340
                    -> coe
                         MAlonzo.Code.Ledger.PParams.d_P1_90
                         (coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v1))
                  C_SPO_342
                    -> coe
                         MAlonzo.Code.Ledger.PParams.d_Q1_120
                         (coe MAlonzo.Code.Ledger.PParams.d_poolThresholds_192 (coe v1))
                  _ -> MAlonzo.RTE.mazUnreachableError)
      C_NewCommittee_366 v4 v5 v6
        -> case coe v2 of
             MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 v7
               -> coe
                    (\ v8 ->
                       case coe v8 of
                         C_CC_338 -> coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
                         C_DRep_340
                           -> coe
                                MAlonzo.Code.Ledger.PParams.d_P2a_92
                                (coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v1))
                         C_SPO_342
                           -> coe
                                MAlonzo.Code.Ledger.PParams.d_Q2a_122
                                (coe MAlonzo.Code.Ledger.PParams.d_poolThresholds_192 (coe v1))
                         _ -> MAlonzo.RTE.mazUnreachableError)
             MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
               -> coe
                    (\ v7 ->
                       case coe v7 of
                         C_CC_338 -> coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
                         C_DRep_340
                           -> coe
                                MAlonzo.Code.Ledger.PParams.d_P2b_94
                                (coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v1))
                         C_SPO_342
                           -> coe
                                MAlonzo.Code.Ledger.PParams.d_Q2b_124
                                (coe MAlonzo.Code.Ledger.PParams.d_poolThresholds_192 (coe v1))
                         _ -> MAlonzo.RTE.mazUnreachableError)
             _ -> MAlonzo.RTE.mazUnreachableError
      C_NewConstitution_368 v4
        -> coe
             (\ v5 ->
                case coe v5 of
                  C_CC_338 -> coe du_ccThreshold_466 (coe v2)
                  C_DRep_340
                    -> coe
                         MAlonzo.Code.Ledger.PParams.d_P3_96
                         (coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v1))
                  C_SPO_342 -> coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
                  _ -> MAlonzo.RTE.mazUnreachableError)
      C_TriggerHF_370 v4
        -> coe
             (\ v5 ->
                case coe v5 of
                  C_CC_338 -> coe du_ccThreshold_466 (coe v2)
                  C_DRep_340
                    -> coe
                         MAlonzo.Code.Ledger.PParams.d_P4_98
                         (coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v1))
                  C_SPO_342
                    -> coe
                         MAlonzo.Code.Ledger.PParams.d_Q4_126
                         (coe MAlonzo.Code.Ledger.PParams.d_poolThresholds_192 (coe v1))
                  _ -> MAlonzo.RTE.mazUnreachableError)
      C_ChangePParams_372 v4 v5
        -> coe
             (\ v6 ->
                case coe v6 of
                  C_CC_338 -> coe du_ccThreshold_466 (coe v2)
                  C_DRep_340 -> coe du_P5_474 (coe v0) (coe v1) (coe v4)
                  C_SPO_342 -> coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
                  _ -> MAlonzo.RTE.mazUnreachableError)
      C_TreasuryWdrl_374 v4
        -> coe
             (\ v5 ->
                case coe v5 of
                  C_CC_338 -> coe du_ccThreshold_466 (coe v2)
                  C_DRep_340
                    -> coe
                         MAlonzo.Code.Ledger.PParams.d_P6_108
                         (coe MAlonzo.Code.Ledger.PParams.d_drepThresholds_190 (coe v1))
                  C_SPO_342 -> coe MAlonzo.Code.Data.Rational.Base.d_0ℚ_170
                  _ -> MAlonzo.RTE.mazUnreachableError)
      C_Info_376 -> coe (\ v4 -> seq (coe v4) (coe du_2ℚ_332))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.NeedsHash
d_NeedsHash_500 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> T_GovAction_362 -> ()
d_NeedsHash_500 = erased
-- Ledger.GovernanceActions.Vote
d_Vote_502 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_Vote_502 = C_yes_504 | C_no_506 | C_abstain_508
-- Ledger.GovernanceActions.GovVote
d_GovVote_510 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_GovVote_510
  = C_GovVote'46'constructor_34849 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                   T_GovRole_336 MAlonzo.Code.Data.Sum.Base.T__'8846'__30 T_Vote_502
                                   (Maybe T_Anchor_352)
-- Ledger.GovernanceActions.GovVote.gid
d_gid_522 ::
  T_GovVote_510 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_gid_522 v0
  = case coe v0 of
      C_GovVote'46'constructor_34849 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.GovVote.role
d_role_524 :: T_GovVote_510 -> T_GovRole_336
d_role_524 v0
  = case coe v0 of
      C_GovVote'46'constructor_34849 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.GovVote.credential
d_credential_526 ::
  T_GovVote_510 -> MAlonzo.Code.Data.Sum.Base.T__'8846'__30
d_credential_526 v0
  = case coe v0 of
      C_GovVote'46'constructor_34849 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.GovVote.vote
d_vote_528 :: T_GovVote_510 -> T_Vote_502
d_vote_528 v0
  = case coe v0 of
      C_GovVote'46'constructor_34849 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.GovVote.anchor
d_anchor_530 :: T_GovVote_510 -> Maybe T_Anchor_352
d_anchor_530 v0
  = case coe v0 of
      C_GovVote'46'constructor_34849 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.GovProposal
d_GovProposal_532 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_GovProposal_532
  = C_GovProposal'46'constructor_35551 MAlonzo.Code.Ledger.Address.T_RwdAddr_58
                                       T_GovAction_362 AgdaAny T_Anchor_352
-- Ledger.GovernanceActions.GovProposal.returnAddr
d_returnAddr_542 ::
  T_GovProposal_532 -> MAlonzo.Code.Ledger.Address.T_RwdAddr_58
d_returnAddr_542 v0
  = case coe v0 of
      C_GovProposal'46'constructor_35551 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.GovProposal.action
d_action_544 :: T_GovProposal_532 -> T_GovAction_362
d_action_544 v0
  = case coe v0 of
      C_GovProposal'46'constructor_35551 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.GovProposal.prevAction
d_prevAction_546 :: T_GovProposal_532 -> AgdaAny
d_prevAction_546 v0
  = case coe v0 of
      C_GovProposal'46'constructor_35551 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.GovProposal.anchor
d_anchor_548 :: T_GovProposal_532 -> T_Anchor_352
d_anchor_548 v0
  = case coe v0 of
      C_GovProposal'46'constructor_35551 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.DecEq-GovRole
d_DecEq'45'GovRole_552 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'GovRole_552 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
  = du_DecEq'45'GovRole_552
du_DecEq'45'GovRole_552 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'GovRole_552
  = coe
      MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63
      (coe
         (\ v0 ->
            case coe v0 of
              C_CC_338
                -> coe
                     (\ v1 ->
                        case coe v1 of
                          C_CC_338
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe (\ v2 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                       (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                          C_DRep_340
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_SPO_342
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          _ -> MAlonzo.RTE.mazUnreachableError)
              C_DRep_340
                -> coe
                     (\ v1 ->
                        case coe v1 of
                          C_CC_338
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_DRep_340
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe (\ v2 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                       (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                          C_SPO_342
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          _ -> MAlonzo.RTE.mazUnreachableError)
              C_SPO_342
                -> coe
                     (\ v1 ->
                        case coe v1 of
                          C_CC_338
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_DRep_340
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_SPO_342
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe (\ v2 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                       (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                          _ -> MAlonzo.RTE.mazUnreachableError)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Ledger.GovernanceActions.DecEq-Vote
d_DecEq'45'Vote_554 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'Vote_554 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 ~v6 ~v7
  = du_DecEq'45'Vote_554
du_DecEq'45'Vote_554 :: MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'Vote_554
  = coe
      MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63
      (coe
         (\ v0 ->
            case coe v0 of
              C_yes_504
                -> coe
                     (\ v1 ->
                        case coe v1 of
                          C_yes_504
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe (\ v2 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                       (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                          C_no_506
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_abstain_508
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          _ -> MAlonzo.RTE.mazUnreachableError)
              C_no_506
                -> coe
                     (\ v1 ->
                        case coe v1 of
                          C_yes_504
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_no_506
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe (\ v2 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                       (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                          C_abstain_508
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          _ -> MAlonzo.RTE.mazUnreachableError)
              C_abstain_508
                -> coe
                     (\ v1 ->
                        case coe v1 of
                          C_yes_504
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_no_506
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_abstain_508
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe (\ v2 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                       (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                          _ -> MAlonzo.RTE.mazUnreachableError)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Ledger.GovernanceActions.DecEq-VDeleg
d_DecEq'45'VDeleg_556 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
d_DecEq'45'VDeleg_556 ~v0 ~v1 ~v2 ~v3 ~v4 ~v5 v6 ~v7
  = du_DecEq'45'VDeleg_556 v6
du_DecEq'45'VDeleg_556 ::
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14
du_DecEq'45'VDeleg_556 v0
  = coe
      MAlonzo.Code.Interface.DecEq.C_DecEq'46'constructor_63
      (coe
         (\ v1 ->
            case coe v1 of
              C_credVoter_346 v2 v3
                -> coe
                     (\ v4 ->
                        case coe v4 of
                          C_credVoter_346 v5 v6
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe
                                       (\ v7 ->
                                          coe
                                            MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                            (coe
                                               MAlonzo.Code.Agda.Builtin.Sigma.C__'44'__32
                                               (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8) erased)
                                            erased)))
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
                                          MAlonzo.Code.Data.Sum.Properties.du_'8801''45'dec_54
                                          (coe
                                             MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_DecEq'45'THash_20
                                                (coe
                                                   MAlonzo.Code.Ledger.Crypto.d_khs_240 (coe v0))))
                                          (coe
                                             MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                             (coe
                                                MAlonzo.Code.Ledger.Crypto.d_decEq'45'ScriptHash_252
                                                (coe v0)))
                                          (coe v6) (coe v3)))
                                    (coe
                                       MAlonzo.Code.Interface.DecEq.d__'8799'__20
                                       (coe du_DecEq'45'GovRole_552) v5 v2))
                          C_abstainRep_348
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_noConfidenceRep_350
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          _ -> MAlonzo.RTE.mazUnreachableError)
              C_abstainRep_348
                -> coe
                     (\ v2 ->
                        case coe v2 of
                          C_credVoter_346 v3 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_abstainRep_348
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe (\ v3 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                       (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                          C_noConfidenceRep_350
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          _ -> MAlonzo.RTE.mazUnreachableError)
              C_noConfidenceRep_350
                -> coe
                     (\ v2 ->
                        case coe v2 of
                          C_credVoter_346 v3 v4
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_abstainRep_348
                            -> coe
                                 MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8)
                                 (coe MAlonzo.Code.Relation.Nullary.Reflects.C_of'8319'_30)
                          C_noConfidenceRep_350
                            -> coe
                                 MAlonzo.Code.Tactic.Derive.DecEq.du_map''_42
                                 (coe
                                    MAlonzo.Code.Function.Bundles.du_mk'8660'_1322 erased
                                    (coe (\ v3 -> coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                                 (coe
                                    MAlonzo.Code.Relation.Nullary.Decidable.Core.C__because__34
                                    (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
                                    (coe
                                       MAlonzo.Code.Relation.Nullary.Reflects.C_of'696'_26
                                       (coe MAlonzo.Code.Agda.Builtin.Unit.C_tt_8)))
                          _ -> MAlonzo.RTE.mazUnreachableError)
              _ -> MAlonzo.RTE.mazUnreachableError))
-- Ledger.GovernanceActions.HashProtected
d_HashProtected_558 ::
  () ->
  () ->
  () ->
  MAlonzo.Code.Ledger.Epoch.T_EpochStructure_30 ->
  MAlonzo.Code.Ledger.PParams.T_PParamsDiff_260 ->
  MAlonzo.Code.Ledger.Crypto.T_isHashableSet_6 ->
  MAlonzo.Code.Ledger.Crypto.T_Crypto_164 ->
  MAlonzo.Code.Interface.DecEq.T_DecEq_14 -> () -> ()
d_HashProtected_558 = erased
-- Ledger.GovernanceActions.EnactEnv
d_EnactEnv_562 a0 a1 a2 a3 a4 a5 a6 a7 = ()
newtype T_EnactEnv_562
  = C_'10214'_'10215''7497'_568 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
-- Ledger.GovernanceActions.EnactEnv.gid
d_gid_566 ::
  T_EnactEnv_562 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_gid_566 v0
  = case coe v0 of
      C_'10214'_'10215''7497'_568 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.EnactState
d_EnactState_570 a0 a1 a2 a3 a4 a5 a6 a7 = ()
data T_EnactState_570
  = C_EnactState'46'constructor_86417 MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                      MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                      MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                      MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
                                      MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14 Integer
-- Ledger.GovernanceActions.EnactState.cc
d_cc_584 ::
  T_EnactState_570 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_cc_584 v0
  = case coe v0 of
      C_EnactState'46'constructor_86417 v1 v2 v3 v4 v5 v6 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.EnactState.constitution
d_constitution_586 ::
  T_EnactState_570 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_constitution_586 v0
  = case coe v0 of
      C_EnactState'46'constructor_86417 v1 v2 v3 v4 v5 v6 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.EnactState.pv
d_pv_588 ::
  T_EnactState_570 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pv_588 v0
  = case coe v0 of
      C_EnactState'46'constructor_86417 v1 v2 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.EnactState.pparams
d_pparams_590 ::
  T_EnactState_570 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_pparams_590 v0
  = case coe v0 of
      C_EnactState'46'constructor_86417 v1 v2 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.EnactState.withdrawals
d_withdrawals_592 ::
  T_EnactState_570 -> MAlonzo.Code.Agda.Builtin.Sigma.T_Σ_14
d_withdrawals_592 v0
  = case coe v0 of
      C_EnactState'46'constructor_86417 v1 v2 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions.EnactState.treasury
d_treasury_594 :: T_EnactState_570 -> Integer
d_treasury_594 v0
  = case coe v0 of
      C_EnactState'46'constructor_86417 v1 v2 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Ledger.GovernanceActions._⊢_⇀⦇_,ENACT⦈_
d__'8866'_'8640''10631'_'44'ENACT'10632'__620 a0 a1 a2 a3 a4 a5 a6
                                              a7 a8 a9 a10 a11
  = ()
data T__'8866'_'8640''10631'_'44'ENACT'10632'__620
  = C_Enact'45'NoConf_622 | C_Enact'45'NewComm_626 |
    C_Enact'45'NewConst_628 | C_Enact'45'HF_630 |
    C_Enact'45'PParams_632 |
    C_Enact'45'Wdrl_638 MAlonzo.Code.Data.Nat.Base.T__'8804'__18 |
    C_Enact'45'Info_640
