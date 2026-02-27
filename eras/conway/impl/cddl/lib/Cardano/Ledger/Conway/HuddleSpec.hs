{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.HuddleSpec (
  module Cardano.Ledger.Babbage.HuddleSpec,
  ConwayEra,
  conwayCDDL,
  conwayMintRule,
  conwayWithdrawalsRule,
  conwayRedeemer,
  conwayRedeemerTag,
  anchorRule,
  drepRule,
  voterRule,
  dnsNameRule,
  urlRule,
  voteRule,
  drepCredentialRule,
  committeeHotCredentialRule,
  committeeColdCredentialRule,
  accountRegistrationDepositCertGroup,
  accountUnregistrationDepositCertGroup,
  delegationToDrepCertGroup,
  delegationToStakePoolAndDrepCertGroup,
  accountRegistrationDelegationToStakePoolCertGroup,
  accountRegistrationDelegationToDrepCertGroup,
  accountRegistrationDelegationToStakePoolAndDrepCertGroup,
  committeeAuthorizationCertGroup,
  committeeResignationCertGroup,
  drepRegistrationCertGroup,
  drepUnregistrationCertGroup,
  drepUpdateCertGroup,
  votingProcedureRule,
  votingProceduresRule,
  constitutionRule,
  parameterChangeActionGroup,
  hardForkInitiationActionGroup,
  treasuryWithdrawalsActionGroup,
  noConfidenceGroup,
  updateCommitteeGroup,
  newConstitutionGroup,
  infoActionRule,
  govActionRule,
  proposalProcedureRule,
  proposalProceduresRule,
  poolVotingThresholdsRule,
  drepVotingThresholdsRule,
  guardrailsScriptHashRule,
  potentialLanguagesRule,
  certificatesRule,
  mkMaybeTaggedSet,
  maybeTaggedSet,
  maybeTaggedNonemptySet,
  maybeTaggedNonemptyOset,
) where

import Cardano.Ledger.Babbage.HuddleSpec hiding (
  alonzoRedeemer,
  alonzoRedeemerTag,
  dnsNameRule,
  maryMintRule,
  maryMultiasset,
  maryValueRule,
  shelleyWithdrawalsRule,
  urlRule,
 )
import Cardano.Ledger.Conway (ConwayEra)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.TypeLits (KnownSymbol)
import Text.Heredoc
import Prelude hiding ((/))

conwayCDDL :: Huddle
conwayCDDL =
  collectFromInit
    [ HIRule $ huddleRule @"block" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"transaction" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"kes_signature" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"language" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"potential_languages" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"signkey_kes" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"certificate" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"policy_id" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"asset_name" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"redeemer" (Proxy @ConwayEra)
    ]

anchorRule ::
  forall era.
  (HuddleRule "url" era, HuddleRule "hash32" era) => Proxy "anchor" -> Proxy era -> Rule
anchorRule pname p =
  pname
    =.= arr
      [ "anchor_url" ==> huddleRule @"url" p
      , "anchor_data_hash" ==> huddleRule @"hash32" p
      ]

drepRule ::
  forall era.
  (HuddleRule "addr_keyhash" era, HuddleRule "script_hash" era) =>
  Proxy "drep" -> Proxy era -> Rule
drepRule pname p =
  pname
    =.= arr [0, a (huddleRule @"addr_keyhash" p)]
    / arr [1, a (huddleRule @"script_hash" p)]
    / arr [2]
    / arr [3]

voterRule ::
  forall era.
  (HuddleRule "addr_keyhash" era, HuddleRule "script_hash" era) =>
  Proxy "voter" -> Proxy era -> Rule
voterRule pname p =
  pname
    =.= arr [0, a (huddleRule @"addr_keyhash" p)]
    / arr [1, a (huddleRule @"script_hash" p)]
    / arr [2, a (huddleRule @"addr_keyhash" p)]
    / arr [3, a (huddleRule @"script_hash" p)]
    / arr [4, a (huddleRule @"addr_keyhash" p)]

dnsNameRule :: Proxy "dns_name" -> Rule
dnsNameRule pname = pname =.= VText `sized` (0 :: Word64, 128 :: Word64)

urlRule :: Proxy "url" -> Rule
urlRule pname = pname =.= VText `sized` (0 :: Word64, 128 :: Word64)

voteRule :: Proxy "vote" -> Rule
voteRule pname = pname =.= (0 :: Integer) ... (2 :: Integer)

drepCredentialRule ::
  forall era. HuddleRule "credential" era => Proxy "drep_credential" -> Proxy era -> Rule
drepCredentialRule pname p = pname =.= huddleRule @"credential" p

committeeHotCredentialRule ::
  forall era. HuddleRule "credential" era => Proxy "committee_hot_credential" -> Proxy era -> Rule
committeeHotCredentialRule pname p = pname =.= huddleRule @"credential" p

committeeColdCredentialRule ::
  forall era. HuddleRule "credential" era => Proxy "committee_cold_credential" -> Proxy era -> Rule
committeeColdCredentialRule pname p = pname =.= huddleRule @"credential" p

guardrailsScriptHashRule ::
  forall era. HuddleRule "script_hash" era => Proxy "guardrails_script_hash" -> Proxy era -> Rule
guardrailsScriptHashRule pname p = pname =.= huddleRule @"script_hash" p

potentialLanguagesRule :: Proxy "potential_languages" -> Rule
potentialLanguagesRule pname = pname =.= (0 :: Integer) ... (255 :: Integer)

conwayCertificateRule ::
  forall era.
  ( HuddleGroup "account_registration_cert" era
  , HuddleGroup "account_unregistration_cert" era
  , HuddleGroup "delegation_to_stake_pool_cert" era
  , HuddleGroup "pool_registration_cert" era
  , HuddleGroup "pool_retirement_cert" era
  , HuddleGroup "account_registration_deposit_cert" era
  , HuddleGroup "account_unregistration_deposit_cert" era
  , HuddleGroup "delegation_to_drep_cert" era
  , HuddleGroup "delegation_to_stake_pool_and_drep_cert" era
  , HuddleGroup "account_registration_delegation_to_stake_pool_cert" era
  , HuddleGroup "account_registration_delegation_to_drep_cert" era
  , HuddleGroup "account_registration_delegation_to_stake_pool_and_drep_cert" era
  , HuddleGroup "committee_authorization_cert" era
  , HuddleGroup "committee_resignation_cert" era
  , HuddleGroup "drep_registration_cert" era
  , HuddleGroup "drep_unregistration_cert" era
  , HuddleGroup "drep_update_cert" era
  ) =>
  Proxy "certificate" ->
  Proxy era ->
  Rule
conwayCertificateRule pname p =
  pname
    =.= arr [a $ huddleGroup @"account_registration_cert" p]
    / arr [a $ huddleGroup @"account_unregistration_cert" p]
    / arr [a $ huddleGroup @"delegation_to_stake_pool_cert" p]
    / arr [a $ huddleGroup @"pool_registration_cert" p]
    / arr [a $ huddleGroup @"pool_retirement_cert" p]
    / arr [a $ huddleGroup @"account_registration_deposit_cert" p]
    / arr [a $ huddleGroup @"account_unregistration_deposit_cert" p]
    / arr [a $ huddleGroup @"delegation_to_drep_cert" p]
    / arr [a $ huddleGroup @"delegation_to_stake_pool_and_drep_cert" p]
    / arr [a $ huddleGroup @"account_registration_delegation_to_stake_pool_cert" p]
    / arr [a $ huddleGroup @"account_registration_delegation_to_drep_cert" p]
    / arr [a $ huddleGroup @"account_registration_delegation_to_stake_pool_and_drep_cert" p]
    / arr [a $ huddleGroup @"committee_authorization_cert" p]
    / arr [a $ huddleGroup @"committee_resignation_cert" p]
    / arr [a $ huddleGroup @"drep_registration_cert" p]
    / arr [a $ huddleGroup @"drep_unregistration_cert" p]
    / arr [a $ huddleGroup @"drep_update_cert" p]

certificatesRule ::
  forall era.
  (HuddleRule "certificate" era, HuddleRule1 "nonempty_oset" era) =>
  Proxy "certificates" -> Proxy era -> Rule
certificatesRule pname p =
  pname
    =.= huddleRule1 @"nonempty_oset" p (huddleRule @"certificate" p)

accountRegistrationDepositCertGroup ::
  forall era.
  (HuddleRule "stake_credential" era, HuddleRule "coin" era) =>
  Proxy "account_registration_deposit_cert" ->
  Proxy era ->
  GroupDef
accountRegistrationDepositCertGroup pname p =
  pname
    =.~ grp [7, a (huddleRule @"stake_credential" p), a (huddleRule @"coin" p)]

accountUnregistrationDepositCertGroup ::
  forall era.
  (HuddleRule "stake_credential" era, HuddleRule "coin" era) =>
  Proxy "account_unregistration_deposit_cert" ->
  Proxy era ->
  GroupDef
accountUnregistrationDepositCertGroup pname p =
  pname
    =.~ grp [8, a (huddleRule @"stake_credential" p), a (huddleRule @"coin" p)]

delegationToDrepCertGroup ::
  forall era.
  (HuddleRule "stake_credential" era, HuddleRule "drep" era) =>
  Proxy "delegation_to_drep_cert" ->
  Proxy era ->
  GroupDef
delegationToDrepCertGroup pname p =
  pname
    =.~ grp [9, a (huddleRule @"stake_credential" p), a (huddleRule @"drep" p)]

delegationToStakePoolAndDrepCertGroup ::
  forall era.
  ( HuddleRule "stake_credential" era
  , HuddleRule "pool_keyhash" era
  , HuddleRule "drep" era
  ) =>
  Proxy "delegation_to_stake_pool_and_drep_cert" ->
  Proxy era ->
  GroupDef
delegationToStakePoolAndDrepCertGroup pname p =
  pname
    =.~ grp
      [ 10
      , a (huddleRule @"stake_credential" p)
      , a (huddleRule @"pool_keyhash" p)
      , a (huddleRule @"drep" p)
      ]

accountRegistrationDelegationToStakePoolCertGroup ::
  forall era.
  ( HuddleRule "stake_credential" era
  , HuddleRule "pool_keyhash" era
  , HuddleRule "coin" era
  ) =>
  Proxy "account_registration_delegation_to_stake_pool_cert" ->
  Proxy era ->
  GroupDef
accountRegistrationDelegationToStakePoolCertGroup pname p =
  pname
    =.~ grp
      [ 11
      , a (huddleRule @"stake_credential" p)
      , a (huddleRule @"pool_keyhash" p)
      , a (huddleRule @"coin" p)
      ]

accountRegistrationDelegationToDrepCertGroup ::
  forall era.
  (HuddleRule "stake_credential" era, HuddleRule "drep" era, HuddleRule "coin" era) =>
  Proxy "account_registration_delegation_to_drep_cert" ->
  Proxy era ->
  GroupDef
accountRegistrationDelegationToDrepCertGroup pname p =
  pname
    =.~ grp
      [ 12
      , a (huddleRule @"stake_credential" p)
      , a (huddleRule @"drep" p)
      , a (huddleRule @"coin" p)
      ]

accountRegistrationDelegationToStakePoolAndDrepCertGroup ::
  forall era.
  ( HuddleRule "stake_credential" era
  , HuddleRule "pool_keyhash" era
  , HuddleRule "drep" era
  , HuddleRule "coin" era
  ) =>
  Proxy "account_registration_delegation_to_stake_pool_and_drep_cert" ->
  Proxy era ->
  GroupDef
accountRegistrationDelegationToStakePoolAndDrepCertGroup pname p =
  pname
    =.~ grp
      [ 13
      , a (huddleRule @"stake_credential" p)
      , a (huddleRule @"pool_keyhash" p)
      , a (huddleRule @"drep" p)
      , a (huddleRule @"coin" p)
      ]

committeeAuthorizationCertGroup ::
  forall era.
  ( HuddleRule "committee_cold_credential" era
  , HuddleRule "committee_hot_credential" era
  ) =>
  Proxy "committee_authorization_cert" ->
  Proxy era ->
  GroupDef
committeeAuthorizationCertGroup pname p =
  pname
    =.~ grp
      [ 14
      , a (huddleRule @"committee_cold_credential" p)
      , a (huddleRule @"committee_hot_credential" p)
      ]

committeeResignationCertGroup ::
  forall era.
  (HuddleRule "committee_cold_credential" era, HuddleRule "anchor" era) =>
  Proxy "committee_resignation_cert" ->
  Proxy era ->
  GroupDef
committeeResignationCertGroup pname p =
  pname
    =.~ grp [15, a (huddleRule @"committee_cold_credential" p), a (huddleRule @"anchor" p / VNil)]

drepRegistrationCertGroup ::
  forall era.
  (HuddleRule "drep_credential" era, HuddleRule "coin" era, HuddleRule "anchor" era) =>
  Proxy "drep_registration_cert" ->
  Proxy era ->
  GroupDef
drepRegistrationCertGroup pname p =
  pname
    =.~ grp
      [ 16
      , a (huddleRule @"drep_credential" p)
      , a (huddleRule @"coin" p)
      , a (huddleRule @"anchor" p / VNil)
      ]

drepUnregistrationCertGroup ::
  forall era.
  (HuddleRule "drep_credential" era, HuddleRule "coin" era) =>
  Proxy "drep_unregistration_cert" ->
  Proxy era ->
  GroupDef
drepUnregistrationCertGroup pname p =
  pname
    =.~ grp [17, a (huddleRule @"drep_credential" p), a (huddleRule @"coin" p)]

drepUpdateCertGroup ::
  forall era.
  (HuddleRule "drep_credential" era, HuddleRule "anchor" era) =>
  Proxy "drep_update_cert" ->
  Proxy era ->
  GroupDef
drepUpdateCertGroup pname p =
  pname
    =.~ grp [18, a (huddleRule @"drep_credential" p), a (huddleRule @"anchor" p / VNil)]

votingProcedureRule ::
  forall era.
  (HuddleRule "vote" era, HuddleRule "anchor" era) =>
  Proxy "voting_procedure" ->
  Proxy era ->
  Rule
votingProcedureRule pname p =
  pname
    =.= arr [a (huddleRule @"vote" p), a (huddleRule @"anchor" p / VNil)]

votingProceduresRule ::
  forall era.
  ( HuddleRule "voter" era
  , HuddleRule "gov_action_id" era
  , HuddleRule "voting_procedure" era
  ) =>
  Proxy "voting_procedures" ->
  Proxy era ->
  Rule
votingProceduresRule pname p =
  pname
    =.= mp
      [ 1
          <+ asKey (huddleRule @"voter" p)
          ==> mp [1 <+ asKey (huddleRule @"gov_action_id" p) ==> huddleRule @"voting_procedure" p]
      ]

constitutionRule ::
  forall era.
  (HuddleRule "anchor" era, HuddleRule "guardrails_script_hash" era) =>
  Proxy "constitution" ->
  Proxy era ->
  Rule
constitutionRule pname p =
  pname
    =.= arr
      [ a (huddleRule @"anchor" p)
      , a (huddleRule @"guardrails_script_hash" p / VNil)
      ]

parameterChangeActionGroup ::
  forall era.
  ( HuddleRule "gov_action_id" era
  , HuddleRule "protocol_param_update" era
  , HuddleRule "guardrails_script_hash" era
  ) =>
  Proxy "parameter_change_action" ->
  Proxy era ->
  GroupDef
parameterChangeActionGroup pname p =
  pname
    =.~ grp
      [ 0
      , a $ huddleRule @"gov_action_id" p / VNil
      , a $ huddleRule @"protocol_param_update" p
      , a $ huddleRule @"guardrails_script_hash" p / VNil
      ]

hardForkInitiationActionGroup ::
  forall era.
  (HuddleRule "gov_action_id" era, HuddleRule "protocol_version" era) =>
  Proxy "hard_fork_initiation_action" ->
  Proxy era ->
  GroupDef
hardForkInitiationActionGroup pname p =
  pname
    =.~ grp [1, a $ huddleRule @"gov_action_id" p / VNil, a $ huddleRule @"protocol_version" p]

treasuryWithdrawalsActionGroup ::
  forall era.
  ( HuddleRule "reward_account" era
  , HuddleRule "coin" era
  , HuddleRule "guardrails_script_hash" era
  ) =>
  Proxy "treasury_withdrawals_action" ->
  Proxy era ->
  GroupDef
treasuryWithdrawalsActionGroup pname p =
  pname
    =.~ grp
      [ 2
      , a $
          mp
            [ 0
                <+ asKey (huddleRule @"reward_account" p)
                ==> huddleRule @"coin" p
            ]
      , a $ huddleRule @"guardrails_script_hash" p / VNil
      ]

noConfidenceGroup ::
  forall era.
  HuddleRule "gov_action_id" era => Proxy "no_confidence" -> Proxy era -> GroupDef
noConfidenceGroup pname p =
  pname
    =.~ grp [3, a $ huddleRule @"gov_action_id" p / VNil]

updateCommitteeGroup ::
  forall era.
  ( HuddleRule "gov_action_id" era
  , HuddleRule "committee_cold_credential" era
  , HuddleRule "epoch" era
  , HuddleRule "unit_interval" era
  , HuddleRule1 "set" era
  ) =>
  Proxy "update_committee" ->
  Proxy era ->
  GroupDef
updateCommitteeGroup pname p =
  pname
    =.~ grp
      [ 4
      , a $ huddleRule @"gov_action_id" p / VNil
      , a $ huddleRule1 @"set" p (huddleRule @"committee_cold_credential" p)
      , a $
          mp
            [ 0
                <+ asKey (huddleRule @"committee_cold_credential" p)
                ==> huddleRule @"epoch" p
            ]
      , a $ huddleRule @"unit_interval" p
      ]

newConstitutionGroup ::
  forall era.
  (HuddleRule "gov_action_id" era, HuddleRule "constitution" era) =>
  Proxy "new_constitution" ->
  Proxy era ->
  GroupDef
newConstitutionGroup pname p =
  pname
    =.~ grp
      [ 5
      , a $ huddleRule @"gov_action_id" p / VNil
      , a $ huddleRule @"constitution" p
      ]

infoActionRule :: Proxy "info_action" -> Rule
infoActionRule pname = pname =.= int 6

govActionRule ::
  forall era.
  ( HuddleGroup "parameter_change_action" era
  , HuddleGroup "hard_fork_initiation_action" era
  , HuddleGroup "treasury_withdrawals_action" era
  , HuddleGroup "no_confidence" era
  , HuddleGroup "update_committee" era
  , HuddleGroup "new_constitution" era
  , HuddleRule "info_action" era
  ) =>
  Proxy "gov_action" ->
  Proxy era ->
  Rule
govActionRule pname p =
  pname
    =.= arr [a (huddleGroup @"parameter_change_action" p)]
    / arr [a (huddleGroup @"hard_fork_initiation_action" p)]
    / arr [a (huddleGroup @"treasury_withdrawals_action" p)]
    / arr [a (huddleGroup @"no_confidence" p)]
    / arr [a (huddleGroup @"update_committee" p)]
    / arr [a (huddleGroup @"new_constitution" p)]
    / arr [a (huddleRule @"info_action" p)]

proposalProcedureRule ::
  forall era.
  ( HuddleRule "coin" era
  , HuddleRule "reward_account" era
  , HuddleRule "gov_action" era
  , HuddleRule "anchor" era
  ) =>
  Proxy "proposal_procedure" ->
  Proxy era ->
  Rule
proposalProcedureRule pname p =
  pname
    =.= arr
      [ "deposit" ==> huddleRule @"coin" p
      , a (huddleRule @"reward_account" p)
      , a (huddleRule @"gov_action" p)
      , a (huddleRule @"anchor" p)
      ]

proposalProceduresRule ::
  forall era.
  ( HuddleRule "proposal_procedure" era
  , HuddleRule1 "nonempty_oset" era
  ) =>
  Proxy "proposal_procedures" ->
  Proxy era ->
  Rule
proposalProceduresRule pname p =
  pname
    =.= huddleRule1 @"nonempty_oset" p (huddleRule @"proposal_procedure" p)

poolVotingThresholdsRule ::
  forall era.
  HuddleRule "unit_interval" era => Proxy "pool_voting_thresholds" -> Proxy era -> Rule
poolVotingThresholdsRule pname p =
  pname
    =.= arr
      [ a (huddleRule @"unit_interval" p) //- "motion no confidence"
      , a (huddleRule @"unit_interval" p) //- "committee normal"
      , a (huddleRule @"unit_interval" p) //- "committee no confidence"
      , a (huddleRule @"unit_interval" p) //- "hard fork initiation"
      , a (huddleRule @"unit_interval" p) //- "security relevant parameter voting threshold"
      ]

drepVotingThresholdsRule ::
  forall era.
  HuddleRule "unit_interval" era => Proxy "drep_voting_thresholds" -> Proxy era -> Rule
drepVotingThresholdsRule pname p =
  pname
    =.= arr
      [ a (huddleRule @"unit_interval" p) //- "motion no confidence"
      , a (huddleRule @"unit_interval" p) //- "committee normal"
      , a (huddleRule @"unit_interval" p) //- "committee no confidence"
      , a (huddleRule @"unit_interval" p) //- "update constitution"
      , a (huddleRule @"unit_interval" p) //- "hard fork initiation"
      , a (huddleRule @"unit_interval" p) //- "PP network group"
      , a (huddleRule @"unit_interval" p) //- "PP economic group"
      , a (huddleRule @"unit_interval" p) //- "PP technical group"
      , a (huddleRule @"unit_interval" p) //- "PP governance group"
      , a (huddleRule @"unit_interval" p) //- "treasury withdrawal"
      ]

conwayMultiasset ::
  forall era a.
  (HuddleRule "policy_id" era, HuddleRule "asset_name" era, IsType0 a) =>
  Proxy "multiasset" ->
  Proxy era ->
  a ->
  GRuleCall
conwayMultiasset pname p =
  binding $ \x ->
    pname
      =.= mp
        [ 0
            <+ asKey (huddleRule @"policy_id" p)
            ==> mp [1 <+ asKey (huddleRule @"asset_name" p) ==> x]
        ]

conwayValueRule ::
  forall era.
  ( HuddleRule "positive_coin" era
  , HuddleRule1 "multiasset" era
  ) =>
  Proxy "value" ->
  Proxy era ->
  Rule
conwayValueRule pname p =
  pname
    =.= huddleRule @"coin" p
    / sarr
      [ a $ huddleRule @"coin" p
      , a $ huddleRule1 @"multiasset" p (huddleRule @"positive_coin" p)
      ]

conwayMintRule ::
  forall era.
  ( HuddleRule "policy_id" era
  , HuddleRule "asset_name" era
  , HuddleRule "nonzero_int64" era
  ) =>
  Proxy "mint" ->
  Proxy era ->
  Rule
conwayMintRule pname p =
  pname
    =.= mp
      [ 1
          <+ asKey (huddleRule @"policy_id" p)
          ==> mp [1 <+ asKey (huddleRule @"asset_name" p) ==> huddleRule @"nonzero_int64" p]
      ]

conwayWithdrawalsRule ::
  forall era. Era era => Proxy "withdrawals" -> Proxy era -> Rule
conwayWithdrawalsRule pname p =
  pname
    =.= mp
      [ 1
          <+ asKey (huddleRule @"reward_account" p)
          ==> huddleRule @"coin" p
      ]

conwayRedeemerTag :: Proxy "redeemer_tag" -> Rule
conwayRedeemerTag pname =
  comment
    [str|0: spend
        |1: mint
        |2: cert
        |3: reward
        |4: voting
        |5: proposing
        |]
    $ pname
      =.= (0 :: Integer)
      ... (5 :: Integer)

conwayRedeemer ::
  forall era.
  ( HuddleRule "redeemer_tag" era
  , HuddleRule "plutus_data" era
  , HuddleRule "ex_units" era
  ) =>
  Proxy "redeemer" ->
  Proxy era ->
  Rule
conwayRedeemer pname p =
  pname
    =.= arr
      [ "tag" ==> huddleRule @"redeemer_tag" p
      , "index" ==> VUInt `sized` (4 :: Word64)
      , "data" ==> huddleRule @"plutus_data" p
      , "ex_units" ==> huddleRule @"ex_units" p
      ]

mkMaybeTaggedSet ::
  forall name a. (KnownSymbol name, IsType0 a) => Proxy name -> Word64 -> a -> GRuleCall
mkMaybeTaggedSet pname n = binding $ \x -> pname =.= tag 258 (arr [n <+ a x]) / sarr [n <+ a x]

maybeTaggedSet :: IsType0 a => Proxy "set" -> a -> GRuleCall
maybeTaggedSet pname = mkMaybeTaggedSet pname 0

maybeTaggedNonemptySet :: IsType0 a => Proxy "nonempty_set" -> a -> GRuleCall
maybeTaggedNonemptySet pname = mkMaybeTaggedSet pname 1

maybeTaggedNonemptyOset :: IsType0 a => Proxy "nonempty_oset" -> a -> GRuleCall
maybeTaggedNonemptyOset pname = mkMaybeTaggedSet pname 1

instance HuddleRule "bounded_bytes" ConwayEra where
  huddleRuleNamed pname _ = boundedBytesRule pname

instance HuddleRule "distinct_bytes" ConwayEra where
  huddleRuleNamed pname _ = distinctBytesRule pname

instance HuddleRule "big_uint" ConwayEra where
  huddleRuleNamed = bigUintRule

instance HuddleRule "big_nint" ConwayEra where
  huddleRuleNamed = bigNintRule

instance HuddleRule "big_int" ConwayEra where
  huddleRuleNamed = bigIntRule

instance HuddleRule "network_id" ConwayEra where
  huddleRuleNamed pname _ = networkIdRule pname

instance HuddleRule "dns_name" ConwayEra where
  huddleRuleNamed pname _ = dnsNameRule pname

instance HuddleRule "url" ConwayEra where
  huddleRuleNamed pname _ = urlRule pname

instance HuddleRule "major_protocol_version" ConwayEra where
  huddleRuleNamed = majorProtocolVersionRule

instance HuddleRule "genesis_hash" ConwayEra where
  huddleRuleNamed = genesisHashRule

instance HuddleRule "genesis_delegate_hash" ConwayEra where
  huddleRuleNamed = genesisDelegateHashRule

instance HuddleRule "transaction_id" ConwayEra where
  huddleRuleNamed = transactionIdRule

instance HuddleRule "vkeywitness" ConwayEra where
  huddleRuleNamed = vkeywitnessRule

instance HuddleRule "bootstrap_witness" ConwayEra where
  huddleRuleNamed = bootstrapWitnessRule

instance HuddleRule "ex_units" ConwayEra where
  huddleRuleNamed pname _ = exUnitsRule pname

instance HuddleRule "positive_interval" ConwayEra where
  huddleRuleNamed = positiveIntervalRule

instance HuddleRule "vote" ConwayEra where
  huddleRuleNamed pname _ = voteRule pname

instance HuddleRule "asset_name" ConwayEra where
  huddleRuleNamed pname _ = assetNameRule pname

instance HuddleRule "drep_credential" ConwayEra where
  huddleRuleNamed = drepCredentialRule

instance HuddleRule "committee_cold_credential" ConwayEra where
  huddleRuleNamed = committeeColdCredentialRule

instance HuddleRule "committee_hot_credential" ConwayEra where
  huddleRuleNamed = committeeHotCredentialRule

instance HuddleRule "anchor" ConwayEra where
  huddleRuleNamed = anchorRule

instance HuddleRule "drep" ConwayEra where
  huddleRuleNamed = drepRule

instance HuddleRule "voter" ConwayEra where
  huddleRuleNamed = voterRule

instance (Era era, HuddleRule "transaction_id" era) => HuddleRule "gov_action_id" era where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ "transaction_id" ==> huddleRule @"transaction_id" p
        , "gov_action_index" ==> VUInt `sized` (2 :: Word64)
        ]

instance HuddleRule "operational_cert" ConwayEra where
  huddleRuleNamed = babbageOperationalCertRule

instance HuddleRule "protocol_version" ConwayEra where
  huddleRuleNamed = babbageProtocolVersionRule

instance (Era era, HuddleRule "distinct_bytes" era) => HuddleRule "plutus_v3_script" era where
  huddleRuleNamed pname p =
    comment
      [str|Conway introduces Plutus V3 with support for new governance features.
          |
          |Note: distinct VBytes ensures uniqueness in test generation.
          |The cddl tool we use for roundtrip testing doesn't generate
          |distinct collections, so we use sized variants to ensure uniqueness.
          |]
      $ pname =.= huddleRule @"distinct_bytes" p

instance Era era => HuddleRule "negative_int64" era where
  huddleRuleNamed pname p =
    pname
      =.= huddleRule @"min_int64" p
      ... (-1 :: Integer)

instance Era era => HuddleRule "positive_int64" era where
  huddleRuleNamed pname p =
    pname
      =.= (1 :: Integer)
      ... huddleRule @"max_int64" p

instance Era era => HuddleRule "nonzero_int64" era where
  huddleRuleNamed pname p =
    pname
      =.= huddleRule @"negative_int64" p
      / huddleRule @"positive_int64" p

instance HuddleRule "policy_id" ConwayEra where
  huddleRuleNamed pname p = pname =.= huddleRule @"script_hash" p

instance HuddleRule "guardrails_script_hash" ConwayEra where
  huddleRuleNamed = guardrailsScriptHashRule

instance HuddleGroup "script_pubkey" ConwayEra where
  huddleGroupNamed pname = scriptPubkeyGroup pname

instance HuddleGroup "script_all" ConwayEra where
  huddleGroupNamed pname = scriptAllGroup pname

instance HuddleGroup "script_any" ConwayEra where
  huddleGroupNamed pname = scriptAnyGroup pname

instance HuddleGroup "script_n_of_k" ConwayEra where
  huddleGroupNamed pname = scriptNOfKGroup pname

instance HuddleGroup "script_invalid_before" ConwayEra where
  huddleGroupNamed pname = scriptInvalidBeforeGroup pname

instance HuddleGroup "script_invalid_hereafter" ConwayEra where
  huddleGroupNamed pname = scriptInvalidHereafterGroup pname

instance HuddleRule "native_script" ConwayEra where
  huddleRuleNamed = nativeScriptRule

instance HuddleGroup "single_host_addr" ConwayEra where
  huddleGroupNamed pname = singleHostAddrGroup pname

instance HuddleGroup "single_host_name" ConwayEra where
  huddleGroupNamed pname = singleHostNameGroup pname

instance HuddleGroup "multi_host_name" ConwayEra where
  huddleGroupNamed pname = multiHostNameGroup pname

instance HuddleRule "relay" ConwayEra where
  huddleRuleNamed = relayRule

instance HuddleRule "pool_metadata" ConwayEra where
  huddleRuleNamed = poolMetadataRule

instance HuddleGroup "pool_params" ConwayEra where
  huddleGroupNamed pname = poolParamsGroup pname

instance HuddleGroup "account_registration_cert" ConwayEra where
  huddleGroupNamed pname = accountRegistrationCertGroup pname

instance HuddleGroup "account_unregistration_cert" ConwayEra where
  huddleGroupNamed pname = accountUnregistrationCertGroup pname

instance HuddleGroup "delegation_to_stake_pool_cert" ConwayEra where
  huddleGroupNamed pname = delegationToStakePoolCertGroup pname

instance HuddleGroup "pool_registration_cert" ConwayEra where
  huddleGroupNamed pname = poolRegistrationCertGroup pname

instance HuddleGroup "pool_retirement_cert" ConwayEra where
  huddleGroupNamed pname = poolRetirementCertGroup pname

instance HuddleGroup "account_registration_deposit_cert" ConwayEra where
  huddleGroupNamed pname = accountRegistrationDepositCertGroup pname

instance HuddleGroup "account_unregistration_deposit_cert" ConwayEra where
  huddleGroupNamed pname = accountUnregistrationDepositCertGroup pname

instance HuddleGroup "delegation_to_drep_cert" ConwayEra where
  huddleGroupNamed pname = delegationToDrepCertGroup pname

instance HuddleGroup "delegation_to_stake_pool_and_drep_cert" ConwayEra where
  huddleGroupNamed pname = delegationToStakePoolAndDrepCertGroup pname

instance HuddleGroup "account_registration_delegation_to_stake_pool_cert" ConwayEra where
  huddleGroupNamed pname = accountRegistrationDelegationToStakePoolCertGroup pname

instance HuddleGroup "account_registration_delegation_to_drep_cert" ConwayEra where
  huddleGroupNamed pname = accountRegistrationDelegationToDrepCertGroup pname

instance HuddleGroup "account_registration_delegation_to_stake_pool_and_drep_cert" ConwayEra where
  huddleGroupNamed pname = accountRegistrationDelegationToStakePoolAndDrepCertGroup pname

instance HuddleGroup "committee_authorization_cert" ConwayEra where
  huddleGroupNamed pname = committeeAuthorizationCertGroup pname

instance HuddleGroup "committee_resignation_cert" ConwayEra where
  huddleGroupNamed pname = committeeResignationCertGroup pname

instance HuddleGroup "drep_registration_cert" ConwayEra where
  huddleGroupNamed pname = drepRegistrationCertGroup pname

instance HuddleGroup "drep_unregistration_cert" ConwayEra where
  huddleGroupNamed pname = drepUnregistrationCertGroup pname

instance HuddleGroup "drep_update_cert" ConwayEra where
  huddleGroupNamed pname = drepUpdateCertGroup pname

instance HuddleRule "certificate" ConwayEra where
  huddleRuleNamed = conwayCertificateRule

instance HuddleRule "certificates" ConwayEra where
  huddleRuleNamed = certificatesRule

instance HuddleRule "voting_procedure" ConwayEra where
  huddleRuleNamed = votingProcedureRule

instance HuddleRule "voting_procedures" ConwayEra where
  huddleRuleNamed = votingProceduresRule

instance HuddleRule "constitution" ConwayEra where
  huddleRuleNamed = constitutionRule

instance HuddleGroup "parameter_change_action" ConwayEra where
  huddleGroupNamed pname = parameterChangeActionGroup pname

instance HuddleGroup "hard_fork_initiation_action" ConwayEra where
  huddleGroupNamed pname = hardForkInitiationActionGroup pname

instance HuddleGroup "treasury_withdrawals_action" ConwayEra where
  huddleGroupNamed pname = treasuryWithdrawalsActionGroup pname

instance HuddleGroup "no_confidence" ConwayEra where
  huddleGroupNamed pname = noConfidenceGroup pname

instance HuddleGroup "update_committee" ConwayEra where
  huddleGroupNamed pname = updateCommitteeGroup pname

instance HuddleGroup "new_constitution" ConwayEra where
  huddleGroupNamed pname = newConstitutionGroup pname

instance HuddleRule "info_action" ConwayEra where
  huddleRuleNamed pname _ = infoActionRule pname

instance HuddleRule "gov_action" ConwayEra where
  huddleRuleNamed = govActionRule

instance HuddleRule "proposal_procedure" ConwayEra where
  huddleRuleNamed = proposalProcedureRule

instance HuddleRule "proposal_procedures" ConwayEra where
  huddleRuleNamed = proposalProceduresRule

instance HuddleRule "transaction_input" ConwayEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ "transaction_id" ==> huddleRule @"transaction_id" p
        , "index" ==> VUInt `sized` (2 :: Word64)
        ]

instance HuddleRule "required_signers" ConwayEra where
  huddleRuleNamed pname p =
    pname
      =.= huddleRule1 @"nonempty_set" p (huddleRule @"addr_keyhash" p)

instance HuddleRule "value" ConwayEra where
  huddleRuleNamed = conwayValueRule

instance HuddleRule "mint" ConwayEra where
  huddleRuleNamed = conwayMintRule

instance HuddleRule "withdrawals" ConwayEra where
  huddleRuleNamed = conwayWithdrawalsRule

instance HuddleRule "data" ConwayEra where
  huddleRuleNamed pname p =
    pname =.= tag 24 (VBytes `cbor` huddleRule @"plutus_data" p)

instance HuddleRule "datum_option" ConwayEra where
  huddleRuleNamed pname p =
    pname
      =.= arr [0, a (huddleRule @"hash32" p)]
      / arr [1, a (huddleRule @"data" p)]

instance HuddleRule "script_ref" ConwayEra where
  huddleRuleNamed = scriptRefRule

instance HuddleRule "alonzo_transaction_output" ConwayEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ a (huddleRule @"address" p)
        , "amount" ==> huddleRule @"value" p
        , opt ("datum_hash" ==> huddleRule @"hash32" p)
        ]

instance HuddleRule "babbage_transaction_output" ConwayEra where
  huddleRuleNamed = babbageTransactionOutput

instance HuddleRule "transaction_output" ConwayEra where
  huddleRuleNamed pname p =
    comment
      [str|Both of the Alonzo and Babbage style TxOut formats are equally valid
          |and can be used interchangeably
          |]
      $ pname
        =.= huddleRule @"alonzo_transaction_output" p
        / huddleRule @"babbage_transaction_output" p

instance HuddleRule "script" ConwayEra where
  huddleRuleNamed pname p =
    comment
      [str|Conway supports four script types:
          |  0: Native scripts (timelock) - unchanged from Allegra
          |  1: Plutus V1 scripts
          |  2: Plutus V2 scripts
          |  3: Plutus V3 scripts
          |]
      $ pname
        =.= arr [0, a (huddleRule @"native_script" p)]
        / arr [1, a (huddleRule @"plutus_v1_script" p)]
        / arr [2, a (huddleRule @"plutus_v2_script" p)]
        / arr [3, a (huddleRule @"plutus_v3_script" p)]

instance HuddleRule "language" ConwayEra where
  huddleRuleNamed pname _ =
    comment
      [str|0: Plutus v1
          |1: Plutus v2
          |2: Plutus v3
          |]
      $ pname =.= (0 :: Integer) ... (2 :: Integer)

instance HuddleRule "potential_languages" ConwayEra where
  huddleRuleNamed pname _ = potentialLanguagesRule pname

instance HuddleRule "cost_models" ConwayEra where
  huddleRuleNamed pname p =
    comment
      [str|The format for cost_models is flexible enough to allow adding
          |Plutus built-ins and language versions in the future.
          |
          |Plutus v1: only 166 integers are used, but more are accepted (and ignored)
          |Plutus v2: only 175 integers are used, but more are accepted (and ignored)
          |Plutus v3: only 223 integers are used, but more are accepted (and ignored)
          |
          |Any 8-bit unsigned number can be used as a key.
          |]
      $ pname
        =.= mp
          [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , 0 <+ asKey ((3 :: Integer) ... (255 :: Integer)) ==> arr [0 <+ a (huddleRule @"int64" p)]
          ]

instance HuddleRule "redeemer_tag" ConwayEra where
  huddleRuleNamed pname _ = conwayRedeemerTag pname

instance HuddleRule "redeemer" ConwayEra where
  huddleRuleNamed = conwayRedeemer

instance HuddleRule "redeemers" ConwayEra where
  huddleRuleNamed pname p =
    comment
      [str|Flat Array support is included for backwards compatibility and
          |will be removed in the next era. It is recommended for tools to
          |adopt using a Map instead of Array going forward.
          |]
      $ pname
        =.= sarr [1 <+ a (huddleRule @"redeemer" p)]
        / smp
          [ 1
              <+ asKey
                ( arr
                    [ "tag" ==> huddleRule @"redeemer_tag" p
                    , "index" ==> VUInt `sized` (4 :: Word64)
                    ]
                )
              ==> arr
                [ "data" ==> huddleRule @"plutus_data" p
                , "ex_units" ==> huddleRule @"ex_units" p
                ]
          ]

instance HuddleRule "script_data_hash" ConwayEra where
  huddleRuleNamed pname p =
    comment
      [str|This is a hash of data which may affect evaluation of a script.
          |This data consists of:
          |  - The redeemers from the transaction_witness_set (the value of field 5).
          |  - The datums from the transaction_witness_set (the value of field 4).
          |  - The value in the cost_models map corresponding to the script's language
          |    (in field 18 of protocol_param_update.)
          |(In the future it may contain additional protocol parameters.)
          |
          |Since this data does not exist in contiguous form inside a transaction, it needs
          |to be independently constructed by each recipient.
          |
          |The bytestring which is hashed is the concatenation of three things:
          |  redeemers || datums || language views
          |The redeemers are exactly the data present in the transaction witness set.
          |Similarly for the datums, if present. If no datums are provided, the middle
          |field is omitted (i.e. it is the empty/null bytestring).
          |
          |language views CDDL:
          |{ * language => script_integrity_data }
          |
          |This must be encoded canonically, using the same scheme as in
          |RFC7049 section 3.9:
          | - Maps, strings, and bytestrings must use a definite-length encoding
          | - Integers must be as small as possible.
          | - The expressions for map length, string length, and bytestring length
          |   must be as short as possible.
          | - The keys in the map must be sorted as follows:
          |    -  If two keys have different lengths, the shorter one sorts earlier.
          |    -  If two keys have the same length, the one with the lower value
          |       in (byte-wise) lexical order sorts earlier.
          |
          |For PlutusV1 (language id 0), the language view is the following:
          |  - the value of cost_models map at key 0 (in other words, the script_integrity_data)
          |    is encoded as an indefinite length list and the result is encoded as a bytestring.
          |    (our apologies)
          |    For example, the script_integrity_data corresponding to the all zero costmodel for V1
          |    would be encoded as (in hex):
          |    58a89f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ff
          |  - the language ID tag is also encoded twice. first as a uint then as
          |    a bytestring. (our apologies)
          |    Concretely, this means that the language version for V1 is encoded as
          |    4100 in hex.
          |For PlutusV2 (language id 1), the language view is the following:
          |  - the value of cost_models map at key 1 is encoded as an definite length list.
          |    For example, the script_integrity_data corresponding to the all zero costmodel for V2
          |    would be encoded as (in hex):
          |    98af0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
          |  - the language ID tag is encoded as expected.
          |    Concretely, this means that the language version for V2 is encoded as
          |    01 in hex.
          |For PlutusV3 (language id 2), the language view is the following:
          |  - the value of cost_models map at key 2 is encoded as a definite length list.
          |
          |Note that each Plutus language represented inside a transaction must have
          |a cost model in the cost_models protocol parameter in order to execute,
          |regardless of what the script integrity data is.
          |
          |Finally, note that in the case that a transaction includes datums but does not
          |include the redeemers field, the script data format becomes (in hex):
          |[ A0 | datums | A0 ]
          |corresponding to a CBOR empty map and an empty map for language view.
          |This empty redeeemer case has changed from the previous eras, since default
          |representation for redeemers has been changed to a map. Also whenever redeemers are
          |supplied either as a map or as an array they must contain at least one element,
          |therefore there is no way to override this behavior by providing a custom
          |representation for empty redeemers.
          |]
      $ scriptDataHashRule pname p

instance HuddleRule "transaction_body" ConwayEra where
  huddleRuleNamed pname p =
    pname
      =.= mp
        [ idx 0 ==> huddleRule1 @"set" p (huddleRule @"transaction_input" p)
        , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
        , idx 2 ==> huddleRule @"coin" p //- "fee"
        , opt (idx 3 ==> huddleRule @"slot" p) //- "time to live"
        , opt (idx 4 ==> huddleRule @"certificates" p)
        , opt (idx 5 ==> huddleRule @"withdrawals" p)
        , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
        , opt (idx 8 ==> huddleRule @"slot" p) //- "validity interval start"
        , opt (idx 9 ==> huddleRule @"mint" p)
        , opt (idx 11 ==> huddleRule @"script_data_hash" p)
        , opt (idx 13 ==> huddleRule1 @"nonempty_set" p (huddleRule @"transaction_input" p)) //- "collateral"
        , opt (idx 14 ==> huddleRule @"required_signers" p)
        , opt (idx 15 ==> huddleRule @"network_id" p)
        , opt (idx 16 ==> huddleRule @"transaction_output" p) //- "collateral return"
        , opt (idx 17 ==> huddleRule @"coin" p) //- "total collateral"
        , opt (idx 18 ==> huddleRule1 @"nonempty_set" p (huddleRule @"transaction_input" p))
            //- "reference inputs"
        , opt (idx 19 ==> huddleRule @"voting_procedures" p)
        , opt (idx 20 ==> huddleRule @"proposal_procedures" p)
        , opt (idx 21 ==> huddleRule @"coin" p) //- "current treasury value"
        , opt (idx 22 ==> huddleRule @"positive_coin" p) //- "donation"
        ]

instance HuddleRule "transaction_witness_set" ConwayEra where
  huddleRuleNamed pname p =
    pname
      =.= mp
        [ opt $ idx 0 ==> huddleRule1 @"nonempty_set" p (huddleRule @"vkeywitness" p)
        , opt $ idx 1 ==> huddleRule1 @"nonempty_set" p (huddleRule @"native_script" p)
        , opt $ idx 2 ==> huddleRule1 @"nonempty_set" p (huddleRule @"bootstrap_witness" p)
        , opt $ idx 3 ==> huddleRule1 @"nonempty_set" p (huddleRule @"plutus_v1_script" p)
        , opt $ idx 4 ==> huddleRule1 @"nonempty_set" p (huddleRule @"plutus_data" p)
        , opt $ idx 5 ==> huddleRule @"redeemers" p
        , opt $ idx 6 ==> huddleRule1 @"nonempty_set" p (huddleRule @"plutus_v2_script" p)
        , opt $ idx 7 ==> huddleRule1 @"nonempty_set" p (huddleRule @"plutus_v3_script" p)
        ]

instance HuddleRule "transaction" ConwayEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a VBool
        , a (huddleRule @"auxiliary_data" p / VNil)
        ]

instance HuddleRule "ex_unit_prices" ConwayEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ "mem_price" ==> huddleRule @"nonnegative_interval" p
        , "step_price" ==> huddleRule @"nonnegative_interval" p
        ]

instance HuddleRule "pool_voting_thresholds" ConwayEra where
  huddleRuleNamed = poolVotingThresholdsRule

instance HuddleRule "drep_voting_thresholds" ConwayEra where
  huddleRuleNamed = drepVotingThresholdsRule

instance HuddleRule "protocol_param_update" ConwayEra where
  huddleRuleNamed pname p =
    pname
      =.= mp
        [ opt (idx 0 ==> huddleRule @"coin" p) //- "minfeeA"
        , opt (idx 1 ==> huddleRule @"coin" p) //- "minfeeB"
        , opt (idx 2 ==> VUInt `sized` (4 :: Word64)) //- "max block body size"
        , opt (idx 3 ==> VUInt `sized` (4 :: Word64)) //- "max transaction size"
        , opt (idx 4 ==> VUInt `sized` (2 :: Word64)) //- "max block header size"
        , opt (idx 5 ==> huddleRule @"coin" p) //- "key deposit"
        , opt (idx 6 ==> huddleRule @"coin" p) //- "pool deposit"
        , opt (idx 7 ==> huddleRule @"epoch_interval" p) //- "maximum epoch"
        , opt (idx 8 ==> VUInt `sized` (2 :: Word64)) //- "n_opt: desired number of stake pools"
        , opt (idx 9 ==> huddleRule @"nonnegative_interval" p) //- "pool pledge influence"
        , opt (idx 10 ==> huddleRule @"unit_interval" p) //- "expansion rate"
        , opt (idx 11 ==> huddleRule @"unit_interval" p) //- "treasury growth rate"
        , opt (idx 16 ==> huddleRule @"coin" p) //- "min pool cost"
        , opt (idx 17 ==> huddleRule @"coin" p) //- "ada per utxo byte"
        , opt (idx 18 ==> huddleRule @"cost_models" p) //- "cost models for script languages"
        , opt (idx 19 ==> huddleRule @"ex_unit_prices" p) //- "execution costs"
        , opt (idx 20 ==> huddleRule @"ex_units" p) //- "max tx ex units"
        , opt (idx 21 ==> huddleRule @"ex_units" p) //- "max block ex units"
        , opt (idx 22 ==> VUInt `sized` (4 :: Word64)) //- "max value size"
        , opt (idx 23 ==> VUInt `sized` (2 :: Word64)) //- "collateral percentage"
        , opt (idx 24 ==> VUInt `sized` (2 :: Word64)) //- "max collateral inputs"
        , opt (idx 25 ==> huddleRule @"pool_voting_thresholds" p) //- "pool voting thresholds"
        , opt (idx 26 ==> huddleRule @"drep_voting_thresholds" p) //- "drep voting thresholds"
        , opt (idx 27 ==> VUInt `sized` (2 :: Word64)) //- "min committee size"
        , opt (idx 28 ==> huddleRule @"epoch_interval" p) //- "committee term limit"
        , opt (idx 29 ==> huddleRule @"epoch_interval" p) //- "goveranance action validity period"
        , opt (idx 30 ==> huddleRule @"coin" p) //- "governance action deposit"
        , opt (idx 31 ==> huddleRule @"coin" p) //- "drep deposit"
        , opt (idx 32 ==> huddleRule @"epoch_interval" p) //- "drep inactivity period"
        , opt (idx 33 ==> huddleRule @"nonnegative_interval" p) //- "minfee refscriptcoinsperbyte"
        ]

instance HuddleRule "proposed_protocol_parameter_updates" ConwayEra where
  huddleRuleNamed = proposedProtocolParameterUpdatesRule

instance HuddleRule "update" ConwayEra where
  huddleRuleNamed = updateRule

instance HuddleRule "header_body" ConwayEra where
  huddleRuleNamed = babbageHeaderBodyRule

instance HuddleRule "header" ConwayEra where
  huddleRuleNamed = headerRule

instance HuddleRule "block" ConwayEra where
  huddleRuleNamed pname p =
    comment
      [str|Valid blocks must also satisfy the following two constraints:
          |  1) the length of transaction_bodies and transaction_witness_sets must be
          |     the same
          |  2) every transaction_index must be strictly smaller than the length of
          |     transaction_bodies
          |]
      $ pname
        =.= arr
          [ a $ huddleRule @"header" p
          , "transaction_bodies" ==> arr [0 <+ a (huddleRule @"transaction_body" p)]
          , "transaction_witness_sets" ==> arr [0 <+ a (huddleRule @"transaction_witness_set" p)]
          , "auxiliary_data_set"
              ==> mp
                [ 0
                    <+ asKey (huddleRule @"transaction_index" p)
                    ==> huddleRule @"auxiliary_data" p
                ]
          , "invalid_transactions" ==> arr [0 <+ a (huddleRule @"transaction_index" p)]
          ]

instance HuddleRule "auxiliary_scripts" ConwayEra where
  huddleRuleNamed = auxiliaryScriptsRule

instance HuddleRule "auxiliary_data_map" ConwayEra where
  huddleRuleNamed pname p =
    pname
      =.= tag
        259
        ( mp
            [ opt (idx 0 ==> huddleRule @"metadata" p)
            , opt (idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)])
            , opt (idx 2 ==> arr [0 <+ a (huddleRule @"plutus_v1_script" p)])
            , opt (idx 3 ==> arr [0 <+ a (huddleRule @"plutus_v2_script" p)])
            , opt (idx 4 ==> arr [0 <+ a (huddleRule @"plutus_v3_script" p)])
            ]
        )

instance HuddleRule "auxiliary_data_array" ConwayEra where
  huddleRuleNamed = auxiliaryDataArrayRule

instance HuddleRule "metadatum" ConwayEra where
  huddleRuleNamed = metadatumRule

instance HuddleRule "auxiliary_data" ConwayEra where
  huddleRuleNamed pname p =
    comment
      [str|auxiliary_data supports three serialization formats:
          |  1. metadata (raw) - Supported since Shelley
          |  2. auxiliary_data_array - Array format, introduced in Allegra
          |  3. auxiliary_data_map - Tagged map format, introduced in Alonzo
          |     Conway adds plutus_v3_script support at index 4
          |]
      $ pname
        =.= huddleRule @"metadata" p
        / huddleRule @"auxiliary_data_array" p
        / huddleRule @"auxiliary_data_map" p

instance HuddleRule1 "set" ConwayEra where
  huddleRule1Named pname _ = maybeTaggedSet pname

instance HuddleRule1 "nonempty_set" ConwayEra where
  huddleRule1Named pname _ = maybeTaggedNonemptySet pname

instance HuddleRule1 "nonempty_oset" ConwayEra where
  huddleRule1Named pname _ = maybeTaggedNonemptyOset pname

instance HuddleRule1 "multiasset" ConwayEra where
  huddleRule1Named = conwayMultiasset
