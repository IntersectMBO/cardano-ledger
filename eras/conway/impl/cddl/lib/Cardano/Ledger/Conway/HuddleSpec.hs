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
  conwayCDDL,
  conwayMultiasset,
  conwayValueRule,
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
  maybeTaggedSet,
  maybeTaggedNonemptySet,
  maybeTaggedNonemptyOset,
  policyHashRule,
  potentialLanguagesRule,
  conwayCertificateRule,
  certificatesRule,
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
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
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

anchorRule :: forall era. (HuddleRule "url" era, HuddleRule "hash32" era) => Proxy era -> Rule
anchorRule p =
  "anchor"
    =:= arr
      [ "anchor_url" ==> huddleRule @"url" p
      , "anchor_data_hash" ==> huddleRule @"hash32" p
      ]

drepRule ::
  forall era. (HuddleRule "addr_keyhash" era, HuddleRule "script_hash" era) => Proxy era -> Rule
drepRule p =
  "drep"
    =:= arr [0, a (huddleRule @"addr_keyhash" p)]
    / arr [1, a (huddleRule @"script_hash" p)]
    / arr [2]
    / arr [3]

voterRule ::
  forall era. (HuddleRule "addr_keyhash" era, HuddleRule "script_hash" era) => Proxy era -> Rule
voterRule p =
  "voter"
    =:= arr [0, a (huddleRule @"addr_keyhash" p)]
    / arr [1, a (huddleRule @"script_hash" p)]
    / arr [2, a (huddleRule @"addr_keyhash" p)]
    / arr [3, a (huddleRule @"script_hash" p)]
    / arr [4, a (huddleRule @"addr_keyhash" p)]

dnsNameRule :: Proxy era -> Rule
dnsNameRule _ = "dns_name" =:= VText `sized` (0 :: Word64, 128 :: Word64)

urlRule :: Proxy era -> Rule
urlRule _ = "url" =:= VText `sized` (0 :: Word64, 128 :: Word64)

voteRule :: Proxy era -> Rule
voteRule _ = "vote" =:= (0 :: Integer) ... (2 :: Integer)

drepCredentialRule :: forall era. HuddleRule "credential" era => Proxy era -> Rule
drepCredentialRule p = "drep_credential" =:= huddleRule @"credential" p

committeeHotCredentialRule :: forall era. HuddleRule "credential" era => Proxy era -> Rule
committeeHotCredentialRule p = "committee_hot_credential" =:= huddleRule @"credential" p

committeeColdCredentialRule :: forall era. HuddleRule "credential" era => Proxy era -> Rule
committeeColdCredentialRule p = "committee_cold_credential" =:= huddleRule @"credential" p

policyHashRule :: forall era. HuddleRule "script_hash" era => Proxy era -> Rule
policyHashRule p = "policy_hash" =:= huddleRule @"script_hash" p

potentialLanguagesRule :: Proxy era -> Rule
potentialLanguagesRule _ = "potential_languages" =:= (0 :: Integer) ... (255 :: Integer)

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
  Proxy era ->
  Rule
conwayCertificateRule p =
  "certificate"
    =:= arr [a $ huddleGroup @"account_registration_cert" p]
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

certificatesRule :: forall era. HuddleRule "certificate" era => Proxy era -> Rule
certificatesRule p =
  "certificates"
    =:= maybeTaggedNonemptyOset (huddleRule @"certificate" p)

accountRegistrationDepositCertGroup ::
  forall era.
  (HuddleRule "stake_credential" era, HuddleRule "coin" era) =>
  Proxy era ->
  GroupDef
accountRegistrationDepositCertGroup p =
  "account_registration_deposit_cert"
    =:~ grp [7, a (huddleRule @"stake_credential" p), a (huddleRule @"coin" p)]

accountUnregistrationDepositCertGroup ::
  forall era.
  (HuddleRule "stake_credential" era, HuddleRule "coin" era) =>
  Proxy era ->
  GroupDef
accountUnregistrationDepositCertGroup p =
  "account_unregistration_deposit_cert"
    =:~ grp [8, a (huddleRule @"stake_credential" p), a (huddleRule @"coin" p)]

delegationToDrepCertGroup ::
  forall era.
  (HuddleRule "stake_credential" era, HuddleRule "drep" era) =>
  Proxy era ->
  GroupDef
delegationToDrepCertGroup p =
  "delegation_to_drep_cert"
    =:~ grp [9, a (huddleRule @"stake_credential" p), a (huddleRule @"drep" p)]

delegationToStakePoolAndDrepCertGroup ::
  forall era.
  (HuddleRule "stake_credential" era, HuddleRule "pool_keyhash" era, HuddleRule "drep" era) =>
  Proxy era ->
  GroupDef
delegationToStakePoolAndDrepCertGroup p =
  "delegation_to_stake_pool_and_drep_cert"
    =:~ grp
      [ 10
      , a (huddleRule @"stake_credential" p)
      , a (huddleRule @"pool_keyhash" p)
      , a (huddleRule @"drep" p)
      ]

accountRegistrationDelegationToStakePoolCertGroup ::
  forall era.
  (HuddleRule "stake_credential" era, HuddleRule "pool_keyhash" era, HuddleRule "coin" era) =>
  Proxy era ->
  GroupDef
accountRegistrationDelegationToStakePoolCertGroup p =
  "account_registration_delegation_to_stake_pool_cert"
    =:~ grp
      [ 11
      , a (huddleRule @"stake_credential" p)
      , a (huddleRule @"pool_keyhash" p)
      , a (huddleRule @"coin" p)
      ]

accountRegistrationDelegationToDrepCertGroup ::
  forall era.
  (HuddleRule "stake_credential" era, HuddleRule "drep" era, HuddleRule "coin" era) =>
  Proxy era ->
  GroupDef
accountRegistrationDelegationToDrepCertGroup p =
  "account_registration_delegation_to_drep_cert"
    =:~ grp
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
  Proxy era ->
  GroupDef
accountRegistrationDelegationToStakePoolAndDrepCertGroup p =
  "account_registration_delegation_to_stake_pool_and_drep_cert"
    =:~ grp
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
  Proxy era ->
  GroupDef
committeeAuthorizationCertGroup p =
  "committee_authorization_cert"
    =:~ grp
      [ 14
      , a (huddleRule @"committee_cold_credential" p)
      , a (huddleRule @"committee_hot_credential" p)
      ]

committeeResignationCertGroup ::
  forall era.
  (HuddleRule "committee_cold_credential" era, HuddleRule "anchor" era) =>
  Proxy era ->
  GroupDef
committeeResignationCertGroup p =
  "committee_resignation_cert"
    =:~ grp [15, a (huddleRule @"committee_cold_credential" p), a (huddleRule @"anchor" p / VNil)]

drepRegistrationCertGroup ::
  forall era.
  (HuddleRule "drep_credential" era, HuddleRule "coin" era, HuddleRule "anchor" era) =>
  Proxy era ->
  GroupDef
drepRegistrationCertGroup p =
  "drep_registration_cert"
    =:~ grp
      [ 16
      , a (huddleRule @"drep_credential" p)
      , a (huddleRule @"coin" p)
      , a (huddleRule @"anchor" p / VNil)
      ]

drepUnregistrationCertGroup ::
  forall era.
  (HuddleRule "drep_credential" era, HuddleRule "coin" era) =>
  Proxy era ->
  GroupDef
drepUnregistrationCertGroup p =
  "drep_unregistration_cert"
    =:~ grp [17, a (huddleRule @"drep_credential" p), a (huddleRule @"coin" p)]

drepUpdateCertGroup ::
  forall era.
  (HuddleRule "drep_credential" era, HuddleRule "anchor" era) =>
  Proxy era ->
  GroupDef
drepUpdateCertGroup p =
  "drep_update_cert"
    =:~ grp [18, a (huddleRule @"drep_credential" p), a (huddleRule @"anchor" p / VNil)]

votingProcedureRule ::
  forall era.
  (HuddleRule "vote" era, HuddleRule "anchor" era) =>
  Proxy era ->
  Rule
votingProcedureRule p =
  "voting_procedure"
    =:= arr [a (huddleRule @"vote" p), a (huddleRule @"anchor" p / VNil)]

votingProceduresRule ::
  forall era.
  ( HuddleRule "voter" era
  , HuddleRule "gov_action_id" era
  , HuddleRule "voting_procedure" era
  ) =>
  Proxy era ->
  Rule
votingProceduresRule p =
  "voting_procedures"
    =:= mp
      [ 1
          <+ asKey (huddleRule @"voter" p)
          ==> mp [1 <+ asKey (huddleRule @"gov_action_id" p) ==> huddleRule @"voting_procedure" p]
      ]

constitutionRule ::
  forall era.
  (HuddleRule "anchor" era, HuddleRule "script_hash" era) =>
  Proxy era ->
  Rule
constitutionRule p =
  "constitution"
    =:= arr
      [ a (huddleRule @"anchor" p)
      , a (huddleRule @"script_hash" p / VNil)
      ]

parameterChangeActionGroup ::
  forall era.
  ( HuddleRule "gov_action_id" era
  , HuddleRule "protocol_param_update" era
  , HuddleRule "policy_hash" era
  ) =>
  Proxy era ->
  GroupDef
parameterChangeActionGroup p =
  "parameter_change_action"
    =:~ grp
      [ 0
      , a $ huddleRule @"gov_action_id" p / VNil
      , a $ huddleRule @"protocol_param_update" p
      , a $ huddleRule @"policy_hash" p / VNil
      ]

hardForkInitiationActionGroup ::
  forall era.
  (HuddleRule "gov_action_id" era, HuddleRule "protocol_version" era) =>
  Proxy era ->
  GroupDef
hardForkInitiationActionGroup p =
  "hard_fork_initiation_action"
    =:~ grp [1, a $ huddleRule @"gov_action_id" p / VNil, a $ huddleRule @"protocol_version" p]

treasuryWithdrawalsActionGroup ::
  forall era.
  (HuddleRule "reward_account" era, HuddleRule "coin" era, HuddleRule "policy_hash" era) =>
  Proxy era ->
  GroupDef
treasuryWithdrawalsActionGroup p =
  "treasury_withdrawals_action"
    =:~ grp
      [ 2
      , a $
          mp
            [ 0
                <+ asKey (huddleRule @"reward_account" p)
                ==> huddleRule @"coin" p
            ]
      , a $ huddleRule @"policy_hash" p / VNil
      ]

noConfidenceGroup :: forall era. HuddleRule "gov_action_id" era => Proxy era -> GroupDef
noConfidenceGroup p =
  "no_confidence"
    =:~ grp [3, a $ huddleRule @"gov_action_id" p / VNil]

updateCommitteeGroup ::
  forall era.
  ( HuddleRule "gov_action_id" era
  , HuddleRule "committee_cold_credential" era
  , HuddleRule "epoch" era
  , HuddleRule "unit_interval" era
  ) =>
  Proxy era ->
  GroupDef
updateCommitteeGroup p =
  "update_committee"
    =:~ grp
      [ 4
      , a $ huddleRule @"gov_action_id" p / VNil
      , a $ maybeTaggedSet (huddleRule @"committee_cold_credential" p)
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
  Proxy era ->
  GroupDef
newConstitutionGroup p =
  "new_constitution"
    =:~ grp
      [ 5
      , a $ huddleRule @"gov_action_id" p / VNil
      , a $ huddleRule @"constitution" p
      ]

infoActionRule :: Rule
infoActionRule = "info_action" =:= int 6

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
  Proxy era ->
  Rule
govActionRule p =
  "gov_action"
    =:= arr [a (huddleGroup @"parameter_change_action" p)]
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
  Proxy era ->
  Rule
proposalProcedureRule p =
  "proposal_procedure"
    =:= arr
      [ "deposit" ==> huddleRule @"coin" p
      , a (huddleRule @"reward_account" p)
      , a (huddleRule @"gov_action" p)
      , a (huddleRule @"anchor" p)
      ]

proposalProceduresRule ::
  forall era.
  HuddleRule "proposal_procedure" era =>
  Proxy era ->
  Rule
proposalProceduresRule p =
  "proposal_procedures"
    =:= maybeTaggedNonemptyOset (huddleRule @"proposal_procedure" p)

poolVotingThresholdsRule :: forall era. HuddleRule "unit_interval" era => Proxy era -> Rule
poolVotingThresholdsRule p =
  "pool_voting_thresholds"
    =:= arr
      [ a (huddleRule @"unit_interval" p) //- "motion no confidence"
      , a (huddleRule @"unit_interval" p) //- "committee normal"
      , a (huddleRule @"unit_interval" p) //- "committee no confidence"
      , a (huddleRule @"unit_interval" p) //- "hard fork initiation"
      , a (huddleRule @"unit_interval" p) //- "security relevant parameter voting threshold"
      ]

drepVotingThresholdsRule :: forall era. HuddleRule "unit_interval" era => Proxy era -> Rule
drepVotingThresholdsRule p =
  "drep_voting_thresholds"
    =:= arr
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
  Proxy era ->
  a ->
  GRuleCall
conwayMultiasset p =
  binding $ \x ->
    "multiasset"
      =:= mp
        [ 0
            <+ asKey (huddleRule @"policy_id" p)
            ==> mp [1 <+ asKey (huddleRule @"asset_name" p) ==> x]
        ]

conwayValueRule ::
  forall era.
  ( HuddleRule "policy_id" era
  , HuddleRule "asset_name" era
  , HuddleRule "positive_coin" era
  ) =>
  Proxy era ->
  Rule
conwayValueRule p =
  "value"
    =:= huddleRule @"coin" p
    / sarr [a $ huddleRule @"coin" p, a $ conwayMultiasset p (huddleRule @"positive_coin" p)]

conwayMintRule ::
  forall era.
  ( HuddleRule "policy_id" era
  , HuddleRule "asset_name" era
  , HuddleRule "nonzero_int64" era
  ) =>
  Proxy era ->
  Rule
conwayMintRule p =
  "mint"
    =:= mp
      [ 1
          <+ asKey (huddleRule @"policy_id" p)
          ==> mp [1 <+ asKey (huddleRule @"asset_name" p) ==> huddleRule @"nonzero_int64" p]
      ]

conwayWithdrawalsRule :: forall era. Era era => Proxy era -> Rule
conwayWithdrawalsRule p =
  "withdrawals"
    =:= mp
      [ 1
          <+ asKey (huddleRule @"reward_account" p)
          ==> huddleRule @"coin" p
      ]

conwayRedeemerTag :: Rule
conwayRedeemerTag =
  comment
    [str|0: spend
        |1: mint
        |2: cert
        |3: reward
        |4: voting
        |5: proposing
        |]
    $ "redeemer_tag"
      =:= (0 :: Integer)
      ... (5 :: Integer)

conwayRedeemer ::
  forall era.
  ( HuddleRule "redeemer_tag" era
  , HuddleRule "plutus_data" era
  , HuddleRule "ex_units" era
  ) =>
  Proxy era ->
  Rule
conwayRedeemer p =
  "redeemer"
    =:= arr
      [ "tag" ==> huddleRule @"redeemer_tag" p
      , "index" ==> (VUInt `sized` (4 :: Word64))
      , "data" ==> huddleRule @"plutus_data" p
      , "ex_units" ==> huddleRule @"ex_units" p
      ]

instance HuddleRule "bounded_bytes" ConwayEra where
  huddleRule _ = boundedBytesRule

instance HuddleRule "distinct_bytes" ConwayEra where
  huddleRule _ = distinctBytesRule

instance HuddleRule "big_uint" ConwayEra where
  huddleRule = bigUintRule

instance HuddleRule "big_nint" ConwayEra where
  huddleRule = bigNintRule

instance HuddleRule "big_int" ConwayEra where
  huddleRule = bigIntRule

instance HuddleRule "network_id" ConwayEra where
  huddleRule _ = networkIdRule

instance HuddleRule "dns_name" ConwayEra where
  huddleRule = dnsNameRule @ConwayEra

instance HuddleRule "url" ConwayEra where
  huddleRule = urlRule @ConwayEra

instance HuddleRule "major_protocol_version" ConwayEra where
  huddleRule = majorProtocolVersionRule @ConwayEra

instance HuddleRule "genesis_hash" ConwayEra where
  huddleRule = genesisHashRule @ConwayEra

instance HuddleRule "genesis_delegate_hash" ConwayEra where
  huddleRule = genesisDelegateHashRule @ConwayEra

instance HuddleRule "transaction_id" ConwayEra where
  huddleRule = transactionIdRule @ConwayEra

instance HuddleRule "vkeywitness" ConwayEra where
  huddleRule = vkeywitnessRule @ConwayEra

instance HuddleRule "bootstrap_witness" ConwayEra where
  huddleRule = bootstrapWitnessRule @ConwayEra

instance HuddleRule "ex_units" ConwayEra where
  huddleRule _ = exUnitsRule

instance HuddleRule "positive_interval" ConwayEra where
  huddleRule = positiveIntervalRule

instance HuddleRule "vote" ConwayEra where
  huddleRule = voteRule @ConwayEra

instance HuddleRule "asset_name" ConwayEra where
  huddleRule = assetNameRule @ConwayEra

instance HuddleRule "drep_credential" ConwayEra where
  huddleRule = drepCredentialRule @ConwayEra

instance HuddleRule "committee_cold_credential" ConwayEra where
  huddleRule = committeeColdCredentialRule @ConwayEra

instance HuddleRule "committee_hot_credential" ConwayEra where
  huddleRule = committeeHotCredentialRule @ConwayEra

instance HuddleRule "anchor" ConwayEra where
  huddleRule = anchorRule @ConwayEra

instance HuddleRule "drep" ConwayEra where
  huddleRule = drepRule @ConwayEra

instance HuddleRule "voter" ConwayEra where
  huddleRule = voterRule @ConwayEra

instance (Era era, HuddleRule "transaction_id" era) => HuddleRule "gov_action_id" era where
  huddleRule p =
    "gov_action_id"
      =:= arr
        [ "transaction_id" ==> huddleRule @"transaction_id" p
        , "gov_action_index" ==> (VUInt `sized` (2 :: Word64))
        ]

instance HuddleRule "operational_cert" ConwayEra where
  huddleRule = babbageOperationalCertRule @ConwayEra

instance HuddleRule "protocol_version" ConwayEra where
  huddleRule = babbageProtocolVersionRule @ConwayEra

instance (Era era, HuddleRule "distinct_bytes" era) => HuddleRule "plutus_v3_script" era where
  huddleRule p =
    comment
      [str|Conway introduces Plutus V3 with support for new governance features.
          |
          |Note: distinct VBytes ensures uniqueness in test generation.
          |The cddl tool we use for roundtrip testing doesn't generate
          |distinct collections, so we use sized variants to ensure uniqueness.
          |]
      $ "plutus_v3_script" =:= huddleRule @"distinct_bytes" p

instance Era era => HuddleRule "negative_int64" era where
  huddleRule p =
    "negative_int64"
      =:= huddleRule @"min_int64" p
      ... (-1 :: Integer)

instance Era era => HuddleRule "positive_int64" era where
  huddleRule p =
    "positive_int64"
      =:= (1 :: Integer)
      ... huddleRule @"max_int64" p

instance Era era => HuddleRule "nonzero_int64" era where
  huddleRule p =
    "nonzero_int64"
      =:= huddleRule @"negative_int64" p
      / huddleRule @"positive_int64" p

instance HuddleRule "policy_id" ConwayEra where
  huddleRule p = "policy_id" =:= huddleRule @"script_hash" p

instance HuddleRule "policy_hash" ConwayEra where
  huddleRule = policyHashRule @ConwayEra

instance HuddleGroup "script_pubkey" ConwayEra where
  huddleGroup = scriptPubkeyGroup @ConwayEra

instance HuddleGroup "script_all" ConwayEra where
  huddleGroup = scriptAllGroup @ConwayEra

instance HuddleGroup "script_any" ConwayEra where
  huddleGroup = scriptAnyGroup @ConwayEra

instance HuddleGroup "script_n_of_k" ConwayEra where
  huddleGroup = scriptNOfKGroup @ConwayEra

instance HuddleGroup "script_invalid_before" ConwayEra where
  huddleGroup = scriptInvalidBeforeGroup @ConwayEra

instance HuddleGroup "script_invalid_hereafter" ConwayEra where
  huddleGroup = scriptInvalidHereafterGroup @ConwayEra

instance HuddleRule "native_script" ConwayEra where
  huddleRule = nativeScriptRule @ConwayEra

instance HuddleGroup "single_host_addr" ConwayEra where
  huddleGroup = singleHostAddrGroup @ConwayEra

instance HuddleGroup "single_host_name" ConwayEra where
  huddleGroup = singleHostNameGroup @ConwayEra

instance HuddleGroup "multi_host_name" ConwayEra where
  huddleGroup = multiHostNameGroup @ConwayEra

instance HuddleRule "relay" ConwayEra where
  huddleRule = relayRule @ConwayEra

instance HuddleRule "pool_metadata" ConwayEra where
  huddleRule = poolMetadataRule @ConwayEra

instance HuddleGroup "pool_params" ConwayEra where
  huddleGroup = poolParamsGroup @ConwayEra

instance HuddleGroup "account_registration_cert" ConwayEra where
  huddleGroup = accountRegistrationCertGroup @ConwayEra

instance HuddleGroup "account_unregistration_cert" ConwayEra where
  huddleGroup = accountUnregistrationCertGroup @ConwayEra

instance HuddleGroup "delegation_to_stake_pool_cert" ConwayEra where
  huddleGroup = delegationToStakePoolCertGroup @ConwayEra

instance HuddleGroup "pool_registration_cert" ConwayEra where
  huddleGroup = poolRegistrationCertGroup @ConwayEra

instance HuddleGroup "pool_retirement_cert" ConwayEra where
  huddleGroup = poolRetirementCertGroup @ConwayEra

instance HuddleGroup "account_registration_deposit_cert" ConwayEra where
  huddleGroup = accountRegistrationDepositCertGroup @ConwayEra

instance HuddleGroup "account_unregistration_deposit_cert" ConwayEra where
  huddleGroup = accountUnregistrationDepositCertGroup @ConwayEra

instance HuddleGroup "delegation_to_drep_cert" ConwayEra where
  huddleGroup = delegationToDrepCertGroup @ConwayEra

instance HuddleGroup "delegation_to_stake_pool_and_drep_cert" ConwayEra where
  huddleGroup = delegationToStakePoolAndDrepCertGroup @ConwayEra

instance HuddleGroup "account_registration_delegation_to_stake_pool_cert" ConwayEra where
  huddleGroup = accountRegistrationDelegationToStakePoolCertGroup @ConwayEra

instance HuddleGroup "account_registration_delegation_to_drep_cert" ConwayEra where
  huddleGroup = accountRegistrationDelegationToDrepCertGroup @ConwayEra

instance HuddleGroup "account_registration_delegation_to_stake_pool_and_drep_cert" ConwayEra where
  huddleGroup = accountRegistrationDelegationToStakePoolAndDrepCertGroup @ConwayEra

instance HuddleGroup "committee_authorization_cert" ConwayEra where
  huddleGroup = committeeAuthorizationCertGroup @ConwayEra

instance HuddleGroup "committee_resignation_cert" ConwayEra where
  huddleGroup = committeeResignationCertGroup @ConwayEra

instance HuddleGroup "drep_registration_cert" ConwayEra where
  huddleGroup = drepRegistrationCertGroup @ConwayEra

instance HuddleGroup "drep_unregistration_cert" ConwayEra where
  huddleGroup = drepUnregistrationCertGroup @ConwayEra

instance HuddleGroup "drep_update_cert" ConwayEra where
  huddleGroup = drepUpdateCertGroup @ConwayEra

instance HuddleRule "certificate" ConwayEra where
  huddleRule = conwayCertificateRule @ConwayEra

instance HuddleRule "certificates" ConwayEra where
  huddleRule = certificatesRule @ConwayEra

instance HuddleRule "voting_procedure" ConwayEra where
  huddleRule = votingProcedureRule @ConwayEra

instance HuddleRule "voting_procedures" ConwayEra where
  huddleRule = votingProceduresRule @ConwayEra

instance HuddleRule "constitution" ConwayEra where
  huddleRule = constitutionRule @ConwayEra

instance HuddleGroup "parameter_change_action" ConwayEra where
  huddleGroup = parameterChangeActionGroup @ConwayEra

instance HuddleGroup "hard_fork_initiation_action" ConwayEra where
  huddleGroup = hardForkInitiationActionGroup @ConwayEra

instance HuddleGroup "treasury_withdrawals_action" ConwayEra where
  huddleGroup = treasuryWithdrawalsActionGroup @ConwayEra

instance HuddleGroup "no_confidence" ConwayEra where
  huddleGroup = noConfidenceGroup @ConwayEra

instance HuddleGroup "update_committee" ConwayEra where
  huddleGroup = updateCommitteeGroup @ConwayEra

instance HuddleGroup "new_constitution" ConwayEra where
  huddleGroup = newConstitutionGroup @ConwayEra

instance HuddleRule "info_action" ConwayEra where
  huddleRule _ = infoActionRule

instance HuddleRule "gov_action" ConwayEra where
  huddleRule = govActionRule @ConwayEra

instance HuddleRule "proposal_procedure" ConwayEra where
  huddleRule = proposalProcedureRule @ConwayEra

instance HuddleRule "proposal_procedures" ConwayEra where
  huddleRule = proposalProceduresRule @ConwayEra

instance HuddleRule "transaction_input" ConwayEra where
  huddleRule p =
    "transaction_input"
      =:= arr
        [ "transaction_id" ==> huddleRule @"transaction_id" p
        , "index" ==> (VUInt `sized` (2 :: Word64))
        ]

instance HuddleRule "required_signers" ConwayEra where
  huddleRule p =
    "required_signers"
      =:= maybeTaggedNonemptySet (huddleRule @"addr_keyhash" p)

instance HuddleRule "value" ConwayEra where
  huddleRule = conwayValueRule @ConwayEra

instance HuddleRule "mint" ConwayEra where
  huddleRule = conwayMintRule @ConwayEra

instance HuddleRule "withdrawals" ConwayEra where
  huddleRule = conwayWithdrawalsRule @ConwayEra

instance HuddleRule "data" ConwayEra where
  huddleRule p =
    "data" =:= tag 24 (VBytes `cbor` huddleRule @"plutus_data" p)

instance HuddleRule "datum_option" ConwayEra where
  huddleRule p =
    "datum_option"
      =:= arr [0, a (huddleRule @"hash32" p)]
      / arr [1, a (huddleRule @"data" p)]

instance HuddleRule "alonzo_transaction_output" ConwayEra where
  huddleRule p =
    "alonzo_transaction_output"
      =:= arr
        [ a (huddleRule @"address" p)
        , "amount" ==> huddleRule @"value" p
        , opt ("datum_hash" ==> huddleRule @"hash32" p)
        ]

instance HuddleRule "transaction_output" ConwayEra where
  huddleRule p =
    comment
      [str|Both of the Alonzo and Babbage style TxOut formats are equally valid
          |and can be used interchangeably
          |]
      $ "transaction_output"
        =:= huddleRule @"alonzo_transaction_output" p
        / babbageTransactionOutput p (huddleRule @"script" p)

instance HuddleRule "script" ConwayEra where
  huddleRule p =
    comment
      [str|Conway supports four script types:
          |  0: Native scripts (timelock) - unchanged from Allegra
          |  1: Plutus V1 scripts
          |  2: Plutus V2 scripts
          |  3: Plutus V3 scripts
          |]
      $ "script"
        =:= arr [0, a (huddleRule @"native_script" p)]
        / arr [1, a (huddleRule @"plutus_v1_script" p)]
        / arr [2, a (huddleRule @"plutus_v2_script" p)]
        / arr [3, a (huddleRule @"plutus_v3_script" p)]

instance HuddleRule "language" ConwayEra where
  huddleRule _ =
    comment
      [str|0: Plutus v1
          |1: Plutus v2
          |2: Plutus v3
          |]
      $ "language" =:= (0 :: Integer) ... (2 :: Integer)

instance HuddleRule "potential_languages" ConwayEra where
  huddleRule = potentialLanguagesRule

instance HuddleRule "cost_models" ConwayEra where
  huddleRule p =
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
      $ "cost_models"
        =:= mp
          [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , 0 <+ asKey ((3 :: Integer) ... (255 :: Integer)) ==> arr [0 <+ a (huddleRule @"int64" p)]
          ]

instance HuddleRule "redeemer_tag" ConwayEra where
  huddleRule _ = conwayRedeemerTag

instance HuddleRule "redeemer" ConwayEra where
  huddleRule = conwayRedeemer @ConwayEra

instance HuddleRule "redeemers" ConwayEra where
  huddleRule p =
    comment
      [str|Flat Array support is included for backwards compatibility and
          |will be removed in the next era. It is recommended for tools to
          |adopt using a Map instead of Array going forward.
          |]
      $ "redeemers"
        =:= sarr [1 <+ a (huddleRule @"redeemer" p)]
        / smp
          [ 1
              <+ asKey
                ( arr
                    [ "tag" ==> huddleRule @"redeemer_tag" p
                    , "index" ==> (VUInt `sized` (4 :: Word64))
                    ]
                )
              ==> arr
                [ "data" ==> huddleRule @"plutus_data" p
                , "ex_units" ==> huddleRule @"ex_units" p
                ]
          ]

instance HuddleRule "script_data_hash" ConwayEra where
  huddleRule p =
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
      $ scriptDataHashRule p

instance HuddleRule "transaction_body" ConwayEra where
  huddleRule p =
    "transaction_body"
      =:= mp
        [ idx 0 ==> maybeTaggedSet (huddleRule @"transaction_input" p)
        , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
        , idx 2 ==> huddleRule @"coin" p //- "fee"
        , opt (idx 3 ==> huddleRule @"slot" p) //- "time to live"
        , opt (idx 4 ==> huddleRule @"certificates" p)
        , opt (idx 5 ==> huddleRule @"withdrawals" p)
        , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
        , opt (idx 8 ==> huddleRule @"slot" p) //- "validity interval start"
        , opt (idx 9 ==> huddleRule @"mint" p)
        , opt (idx 11 ==> huddleRule @"script_data_hash" p)
        , opt (idx 13 ==> maybeTaggedNonemptySet (huddleRule @"transaction_input" p)) //- "collateral"
        , opt (idx 14 ==> huddleRule @"required_signers" p)
        , opt (idx 15 ==> huddleRule @"network_id" p)
        , opt (idx 16 ==> huddleRule @"transaction_output" p) //- "collateral return"
        , opt (idx 17 ==> huddleRule @"coin" p) //- "total collateral"
        , opt (idx 18 ==> maybeTaggedNonemptySet (huddleRule @"transaction_input" p)) //- "reference inputs"
        , opt (idx 19 ==> huddleRule @"voting_procedures" p)
        , opt (idx 20 ==> huddleRule @"proposal_procedures" p)
        , opt (idx 21 ==> huddleRule @"coin" p) //- "current treasury value"
        , opt (idx 22 ==> huddleRule @"positive_coin" p) //- "donation"
        ]

instance HuddleRule "transaction_witness_set" ConwayEra where
  huddleRule p =
    "transaction_witness_set"
      =:= mp
        [ opt $ idx 0 ==> maybeTaggedNonemptySet (huddleRule @"vkeywitness" p)
        , opt $ idx 1 ==> maybeTaggedNonemptySet (huddleRule @"native_script" p)
        , opt $ idx 2 ==> maybeTaggedNonemptySet (huddleRule @"bootstrap_witness" p)
        , opt $ idx 3 ==> maybeTaggedNonemptySet (huddleRule @"plutus_v1_script" p)
        , opt $ idx 4 ==> maybeTaggedNonemptySet (huddleRule @"plutus_data" p)
        , opt $ idx 5 ==> huddleRule @"redeemers" p
        , opt $ idx 6 ==> maybeTaggedNonemptySet (huddleRule @"plutus_v2_script" p)
        , opt $ idx 7 ==> maybeTaggedNonemptySet (huddleRule @"plutus_v3_script" p)
        ]

instance HuddleRule "transaction" ConwayEra where
  huddleRule p =
    "transaction"
      =:= arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a VBool
        , a (huddleRule @"auxiliary_data" p / VNil)
        ]

instance HuddleRule "ex_unit_prices" ConwayEra where
  huddleRule p =
    "ex_unit_prices"
      =:= arr
        [ "mem_price" ==> huddleRule @"nonnegative_interval" p
        , "step_price" ==> huddleRule @"nonnegative_interval" p
        ]

instance HuddleRule "pool_voting_thresholds" ConwayEra where
  huddleRule = poolVotingThresholdsRule @ConwayEra

instance HuddleRule "drep_voting_thresholds" ConwayEra where
  huddleRule = drepVotingThresholdsRule @ConwayEra

instance HuddleRule "protocol_param_update" ConwayEra where
  huddleRule p =
    "protocol_param_update"
      =:= mp
        [ opt (idx 0 ==> huddleRule @"coin" p) //- "minfeeA"
        , opt (idx 1 ==> huddleRule @"coin" p) //- "minfeeB"
        , opt (idx 2 ==> (VUInt `sized` (4 :: Word64))) //- "max block body size"
        , opt (idx 3 ==> (VUInt `sized` (4 :: Word64))) //- "max transaction size"
        , opt (idx 4 ==> (VUInt `sized` (2 :: Word64))) //- "max block header size"
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
        , opt (idx 22 ==> (VUInt `sized` (4 :: Word64))) //- "max value size"
        , opt (idx 23 ==> (VUInt `sized` (2 :: Word64))) //- "collateral percentage"
        , opt (idx 24 ==> (VUInt `sized` (2 :: Word64))) //- "max collateral inputs"
        , opt (idx 25 ==> huddleRule @"pool_voting_thresholds" p) //- "pool voting thresholds"
        , opt (idx 26 ==> huddleRule @"drep_voting_thresholds" p) //- "drep voting thresholds"
        , opt (idx 27 ==> (VUInt `sized` (2 :: Word64))) //- "min committee size"
        , opt (idx 28 ==> huddleRule @"epoch_interval" p) //- "committee term limit"
        , opt (idx 29 ==> huddleRule @"epoch_interval" p) //- "goveranance action validity period"
        , opt (idx 30 ==> huddleRule @"coin" p) //- "governance action deposit"
        , opt (idx 31 ==> huddleRule @"coin" p) //- "drep deposit"
        , opt (idx 32 ==> huddleRule @"epoch_interval" p) //- "drep inactivity period"
        , opt (idx 33 ==> huddleRule @"nonnegative_interval" p) //- "minfee refscriptcoinsperbyte"
        ]

instance HuddleRule "proposed_protocol_parameter_updates" ConwayEra where
  huddleRule = proposedProtocolParameterUpdatesRule @ConwayEra

instance HuddleRule "update" ConwayEra where
  huddleRule = updateRule @ConwayEra

instance HuddleRule "header_body" ConwayEra where
  huddleRule p =
    "header_body"
      =:= arr
        [ "block_number" ==> huddleRule @"block_number" p
        , "slot" ==> huddleRule @"slot" p
        , "prev_hash" ==> (huddleRule @"hash32" p / VNil)
        , "issuer_vkey" ==> huddleRule @"vkey" p
        , "vrf_vkey" ==> huddleRule @"vrf_vkey" p
        , "vrf_result" ==> huddleRule @"vrf_cert" p
        , "block_body_size" ==> (VUInt `sized` (4 :: Word64))
        , "block_body_hash" ==> huddleRule @"hash32" p //- "merkle triple root"
        , a $ huddleRule @"operational_cert" p
        , a $ huddleRule @"protocol_version" p
        ]

instance HuddleRule "header" ConwayEra where
  huddleRule = headerRule @ConwayEra

instance HuddleRule "block" ConwayEra where
  huddleRule p =
    comment
      [str|Valid blocks must also satisfy the following two constraints:
          |  1) the length of transaction_bodies and transaction_witness_sets must be
          |     the same
          |  2) every transaction_index must be strictly smaller than the length of
          |     transaction_bodies
          |]
      $ "block"
        =:= arr
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
  huddleRule = auxiliaryScriptsRule @ConwayEra

instance HuddleRule "auxiliary_data_map" ConwayEra where
  huddleRule p =
    "auxiliary_data_map"
      =:= tag
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
  huddleRule = auxiliaryDataArrayRule @ConwayEra

instance HuddleRule "auxiliary_data" ConwayEra where
  huddleRule p =
    comment
      [str|auxiliary_data supports three serialization formats:
          |  1. metadata (raw) - Supported since Shelley
          |  2. auxiliary_data_array - Array format, introduced in Allegra
          |  3. auxiliary_data_map - Tagged map format, introduced in Alonzo
          |     Conway adds plutus_v3_script support at index 4
          |]
      $ "auxiliary_data"
        =:= huddleRule @"metadata" p
        / huddleRule @"auxiliary_data_array" p
        / huddleRule @"auxiliary_data_map" p

mkMaybeTaggedSet :: IsType0 a => Name -> Word64 -> a -> GRuleCall
mkMaybeTaggedSet label n = binding $ \x -> label =:= tag 258 (arr [n <+ a x]) / sarr [n <+ a x]

maybeTaggedSet :: IsType0 a => a -> GRuleCall
maybeTaggedSet = mkMaybeTaggedSet "set" 0

maybeTaggedNonemptySet :: IsType0 a => a -> GRuleCall
maybeTaggedNonemptySet = mkMaybeTaggedSet "nonempty_set" 1

maybeTaggedNonemptyOset :: IsType0 a => a -> GRuleCall
maybeTaggedNonemptyOset = mkMaybeTaggedSet "nonempty_oset" 1
