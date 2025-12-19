{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.HuddleSpec (
  module Cardano.Ledger.Shelley.HuddleSpec,
  AllegraEra,
  allegraCDDL,
  blockRule,
  transactionRule,
  auxiliaryScriptsRule,
  auxiliaryDataArrayRule,
  auxiliaryDataRule,
  nativeScriptRule,
  scriptNOfKGroup,
  scriptInvalidBeforeGroup,
  scriptInvalidHereafterGroup,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Shelley.HuddleSpec
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol)
import Text.Heredoc
import Prelude hiding ((/))

allegraCDDL :: Huddle
allegraCDDL =
  collectFrom
    [ HIRule $ huddleRule @"block" (Proxy @AllegraEra)
    , HIRule $ huddleRule @"transaction" (Proxy @AllegraEra)
    ]

blockRule ::
  forall name era.
  ( KnownSymbol name
  , HuddleRule "header" era
  , HuddleRule "transaction_body" era
  , HuddleRule "transaction_witness_set" era
  , HuddleRule "auxiliary_data" era
  ) =>
  Proxy name ->
  Proxy era ->
  Rule
blockRule pname p =
  pname
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
      ]

transactionRule ::
  forall name era.
  ( KnownSymbol name
  , HuddleRule "transaction_body" era
  , HuddleRule "transaction_witness_set" era
  , HuddleRule "auxiliary_data" era
  ) =>
  Proxy name ->
  Proxy era ->
  Rule
transactionRule pname p =
  pname
    =.= arr
      [ a $ huddleRule @"transaction_body" p
      , a $ huddleRule @"transaction_witness_set" p
      , a $ huddleRule @"auxiliary_data" p / VNil
      ]

auxiliaryScriptsRule ::
  forall name era.
  (KnownSymbol name, HuddleRule "native_script" era) => Proxy name -> Proxy era -> Rule
auxiliaryScriptsRule pname p = pname =.= arr [0 <+ a (huddleRule @"native_script" p)]

auxiliaryDataArrayRule ::
  forall name era.
  (KnownSymbol name, HuddleRule "auxiliary_scripts" era) => Proxy name -> Proxy era -> Rule
auxiliaryDataArrayRule pname p =
  pname
    =.= arr
      [ "transaction_metadata" ==> huddleRule @"metadata" p
      , "auxiliary_scripts" ==> huddleRule @"auxiliary_scripts" p
      ]

auxiliaryDataRule ::
  forall name era.
  (KnownSymbol name, HuddleRule "auxiliary_data_array" era) => Proxy name -> Proxy era -> Rule
auxiliaryDataRule pname p =
  pname
    =.= huddleRule @"metadata" p
    / huddleRule @"auxiliary_data_array" p

nativeScriptRule ::
  forall name era.
  ( KnownSymbol name
  , HuddleGroup "script_pubkey" era
  , HuddleGroup "script_all" era
  , HuddleGroup "script_any" era
  , HuddleGroup "script_n_of_k" era
  , HuddleGroup "script_invalid_before" era
  , HuddleGroup "script_invalid_hereafter" era
  ) =>
  Proxy name ->
  Proxy era ->
  Rule
nativeScriptRule pname p =
  comment
    [str|Allegra introduces timelock support for native scripts.
        |
        |Timelock validity intervals are half-open intervals [a, b).
        |  script_invalid_before: specifies the left (included) endpoint a.
        |  script_invalid_hereafter: specifies the right (excluded) endpoint b.
        |
        |Note: Allegra switched to int64 for script_n_of_k thresholds.
        |]
    $ pname
      =.= arr [a $ huddleGroup @"script_pubkey" p]
      / arr [a $ huddleGroup @"script_all" p]
      / arr [a $ huddleGroup @"script_any" p]
      / arr [a $ huddleGroup @"script_n_of_k" p]
      / arr [a $ huddleGroup @"script_invalid_before" p]
      / arr [a $ huddleGroup @"script_invalid_hereafter" p]

scriptNOfKGroup ::
  forall name era.
  (KnownSymbol name, HuddleRule "native_script" era) =>
  Proxy name ->
  Proxy era ->
  GroupDef
scriptNOfKGroup pname p =
  pname
    =.~ grp
      [ 3
      , "n" ==> huddleRule @"int64" p
      , a $ arr [0 <+ a (huddleRule @"native_script" p)]
      ]

scriptInvalidBeforeGroup ::
  forall name era. (KnownSymbol name, Era era) => Proxy name -> Proxy era -> GroupDef
scriptInvalidBeforeGroup pname p =
  comment
    [str|Timelock validity intervals are half-open intervals [a, b).
        |This field specifies the left (included) endpoint a.
        |]
    $ pname
      =.~ grp [4, a (huddleRule @"slot" p)]

scriptInvalidHereafterGroup ::
  forall name era. (KnownSymbol name, Era era) => Proxy name -> Proxy era -> GroupDef
scriptInvalidHereafterGroup pname p =
  comment
    [str|Timelock validity intervals are half-open intervals [a, b).
        |This field specifies the right (excluded) endpoint b.
        |]
    $ pname
      =.~ grp [5, a (huddleRule @"slot" p)]

instance HuddleRule "major_protocol_version" AllegraEra where
  huddleRuleNamed = majorProtocolVersionRule

instance HuddleGroup "protocol_version" AllegraEra where
  huddleGroupNamed = shelleyProtocolVersionGroup

instance HuddleRule "protocol_param_update" AllegraEra where
  huddleRuleNamed = protocolParamUpdateRule

instance HuddleRule "proposed_protocol_parameter_updates" AllegraEra where
  huddleRuleNamed = proposedProtocolParameterUpdatesRule

instance HuddleRule "update" AllegraEra where
  huddleRuleNamed = updateRule

instance HuddleRule "genesis_hash" AllegraEra where
  huddleRuleNamed = genesisHashRule

instance HuddleGroup "operational_cert" AllegraEra where
  huddleGroupNamed = shelleyOperationalCertGroup

instance HuddleRule "header_body" AllegraEra where
  huddleRuleNamed = headerBodyRule

instance HuddleRule "header" AllegraEra where
  huddleRuleNamed = headerRule

instance Era era => HuddleRule "min_int64" era where
  huddleRuleNamed pname _ = pname =.= (-9223372036854775808 :: Integer)

instance Era era => HuddleRule "max_int64" era where
  huddleRuleNamed pname _ = pname =.= (9223372036854775807 :: Integer)

instance Era era => HuddleRule "int64" era where
  huddleRuleNamed pname p = pname =.= huddleRule @"min_int64" p ... huddleRule @"max_int64" p

instance HuddleGroup "script_all" AllegraEra where
  huddleGroupNamed = scriptAllGroup

instance HuddleGroup "script_any" AllegraEra where
  huddleGroupNamed = scriptAnyGroup

instance HuddleGroup "script_n_of_k" AllegraEra where
  huddleGroupNamed = scriptNOfKGroup

instance HuddleGroup "script_invalid_before" AllegraEra where
  huddleGroupNamed = scriptInvalidBeforeGroup

instance HuddleGroup "script_invalid_hereafter" AllegraEra where
  huddleGroupNamed = scriptInvalidHereafterGroup

instance HuddleRule "native_script" AllegraEra where
  huddleRuleNamed = nativeScriptRule

instance HuddleRule "vkeywitness" AllegraEra where
  huddleRuleNamed = vkeywitnessRule

instance HuddleRule "bootstrap_witness" AllegraEra where
  huddleRuleNamed = bootstrapWitnessRule

instance HuddleRule "transaction_witness_set" AllegraEra where
  huddleRuleNamed = transactionWitnessSetRule

instance HuddleGroup "script_pubkey" AllegraEra where
  huddleGroupNamed = scriptPubkeyGroup

instance HuddleRule "transaction_id" AllegraEra where
  huddleRuleNamed = transactionIdRule

instance HuddleRule "transaction_input" AllegraEra where
  huddleRuleNamed = transactionInputRule

instance HuddleRule "transaction_output" AllegraEra where
  huddleRuleNamed = transactionOutputRule

instance HuddleRule "dns_name" AllegraEra where
  huddleRuleNamed pname _ = dnsNameRule pname

instance HuddleRule "url" AllegraEra where
  huddleRuleNamed pname _ = urlRule pname

instance HuddleRule "pool_metadata" AllegraEra where
  huddleRuleNamed = poolMetadataRule

instance HuddleGroup "single_host_addr" AllegraEra where
  huddleGroupNamed = singleHostAddrGroup

instance HuddleGroup "single_host_name" AllegraEra where
  huddleGroupNamed = singleHostNameGroup

instance HuddleGroup "multi_host_name" AllegraEra where
  huddleGroupNamed = multiHostNameGroup

instance HuddleRule "relay" AllegraEra where
  huddleRuleNamed = relayRule

instance HuddleGroup "pool_params" AllegraEra where
  huddleGroupNamed = poolParamsGroup

instance HuddleGroup "pool_registration_cert" AllegraEra where
  huddleGroupNamed = poolRegistrationCertGroup

instance HuddleGroup "pool_retirement_cert" AllegraEra where
  huddleGroupNamed = poolRetirementCertGroup

instance HuddleRule "genesis_delegate_hash" AllegraEra where
  huddleRuleNamed = genesisDelegateHashRule

instance HuddleGroup "genesis_delegation_cert" AllegraEra where
  huddleGroupNamed = genesisDelegationCertGroup

instance HuddleRule "delta_coin" AllegraEra where
  huddleRuleNamed pname _ = deltaCoinRule pname

instance HuddleRule "move_instantaneous_reward" AllegraEra where
  huddleRuleNamed = moveInstantaneousRewardRule

instance HuddleGroup "move_instantaneous_rewards_cert" AllegraEra where
  huddleGroupNamed = moveInstantaneousRewardsCertGroup

instance HuddleGroup "account_registration_cert" AllegraEra where
  huddleGroupNamed = accountRegistrationCertGroup

instance HuddleGroup "account_unregistration_cert" AllegraEra where
  huddleGroupNamed = accountUnregistrationCertGroup

instance HuddleGroup "delegation_to_stake_pool_cert" AllegraEra where
  huddleGroupNamed = delegationToStakePoolCertGroup

instance HuddleRule "certificate" AllegraEra where
  huddleRuleNamed = certificateRule

instance HuddleRule "withdrawals" AllegraEra where
  huddleRuleNamed = shelleyWithdrawalsRule

instance HuddleRule "auxiliary_scripts" AllegraEra where
  huddleRuleNamed = auxiliaryScriptsRule

instance HuddleRule "auxiliary_data_array" AllegraEra where
  huddleRuleNamed = auxiliaryDataArrayRule

instance HuddleRule "auxiliary_data" AllegraEra where
  huddleRuleNamed = auxiliaryDataRule

instance HuddleRule "transaction_body" AllegraEra where
  huddleRuleNamed pname p =
    comment
      [str|Allegra transaction body adds the validity interval start at index 8
          |]
      $ pname
        =.= mp
          [ idx 0 ==> huddleRule1 @"set" p (huddleRule @"transaction_input" p)
          , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
          , idx 2 ==> huddleRule @"coin" p
          , opt (idx 3 ==> huddleRule @"slot" p)
          , opt (idx 4 ==> arr [0 <+ a (huddleRule @"certificate" p)])
          , opt (idx 5 ==> huddleRule @"withdrawals" p)
          , opt (idx 6 ==> huddleRule @"update" p)
          , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
          , opt (idx 8 ==> huddleRule @"slot" p)
          ]

instance HuddleRule "transaction" AllegraEra where
  huddleRuleNamed = transactionRule

instance HuddleRule "block" AllegraEra where
  huddleRuleNamed = blockRule

instance HuddleRule1 "set" AllegraEra where
  huddleRule1Named pname _ = untaggedSet pname
