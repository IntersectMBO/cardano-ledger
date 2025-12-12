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
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import Text.Heredoc
import Prelude hiding ((/))

allegraCDDL :: Huddle
allegraCDDL =
  collectFrom
    [ HIRule $ huddleRule @"block" (Proxy @AllegraEra)
    , HIRule $ huddleRule @"transaction" (Proxy @AllegraEra)
    ]

blockRule ::
  forall era.
  ( HuddleRule "header" era
  , HuddleRule "transaction_body" era
  , HuddleRule "transaction_witness_set" era
  , HuddleRule "auxiliary_data" era
  ) =>
  Proxy era ->
  Rule
blockRule p =
  "block"
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
      ]

transactionRule ::
  forall era.
  ( HuddleRule "transaction_body" era
  , HuddleRule "transaction_witness_set" era
  , HuddleRule "auxiliary_data" era
  ) =>
  Proxy era ->
  Rule
transactionRule p =
  "transaction"
    =:= arr
      [ a $ huddleRule @"transaction_body" p
      , a $ huddleRule @"transaction_witness_set" p
      , a (huddleRule @"auxiliary_data" p / VNil)
      ]

auxiliaryScriptsRule :: forall era. HuddleRule "native_script" era => Proxy era -> Rule
auxiliaryScriptsRule p = "auxiliary_scripts" =:= arr [0 <+ a (huddleRule @"native_script" p)]

auxiliaryDataArrayRule ::
  forall era. HuddleRule "auxiliary_scripts" era => Proxy era -> Rule
auxiliaryDataArrayRule p =
  "auxiliary_data_array"
    =:= arr
      [ "transaction_metadata" ==> huddleRule @"metadata" p
      , "auxiliary_scripts" ==> huddleRule @"auxiliary_scripts" p
      ]

auxiliaryDataRule ::
  forall era. HuddleRule "auxiliary_data_array" era => Proxy era -> Rule
auxiliaryDataRule p =
  "auxiliary_data"
    =:= huddleRule @"metadata" p
    / huddleRule @"auxiliary_data_array" p

nativeScriptRule ::
  forall era.
  ( HuddleGroup "script_pubkey" era
  , HuddleGroup "script_all" era
  , HuddleGroup "script_any" era
  , HuddleGroup "script_n_of_k" era
  , HuddleGroup "script_invalid_before" era
  , HuddleGroup "script_invalid_hereafter" era
  ) =>
  Proxy era ->
  Rule
nativeScriptRule p =
  comment
    [str|Allegra introduces timelock support for native scripts.
        |
        |Timelock validity intervals are half-open intervals [a, b).
        |  script_invalid_before: specifies the left (included) endpoint a.
        |  script_invalid_hereafter: specifies the right (excluded) endpoint b.
        |
        |Note: Allegra switched to int64 for script_n_of_k thresholds.
        |]
    $ "native_script"
      =:= arr [a $ huddleGroup @"script_pubkey" p]
      / arr [a $ huddleGroup @"script_all" p]
      / arr [a $ huddleGroup @"script_any" p]
      / arr [a $ huddleGroup @"script_n_of_k" p]
      / arr [a $ huddleGroup @"script_invalid_before" p]
      / arr [a $ huddleGroup @"script_invalid_hereafter" p]

scriptNOfKGroup ::
  forall era.
  HuddleRule "native_script" era =>
  Proxy era ->
  GroupDef
scriptNOfKGroup p =
  "script_n_of_k"
    =:~ grp
      [ 3
      , "n" ==> huddleRule @"int64" p
      , a $ arr [0 <+ a (huddleRule @"native_script" p)]
      ]

scriptInvalidBeforeGroup ::
  forall era. Era era => Proxy era -> GroupDef
scriptInvalidBeforeGroup p =
  comment
    [str|Timelock validity intervals are half-open intervals [a, b).
        |This field specifies the left (included) endpoint a.
        |]
    $ "script_invalid_before"
      =:~ grp [4, a (huddleRule @"slot" p)]

scriptInvalidHereafterGroup ::
  forall era. Era era => Proxy era -> GroupDef
scriptInvalidHereafterGroup p =
  comment
    [str|Timelock validity intervals are half-open intervals [a, b).
        |This field specifies the right (excluded) endpoint b.
        |]
    $ "script_invalid_hereafter"
      =:~ grp [5, a (huddleRule @"slot" p)]

instance HuddleRule "major_protocol_version" AllegraEra where
  huddleRule = majorProtocolVersionRule @AllegraEra

instance HuddleGroup "protocol_version" AllegraEra where
  huddleGroup = shelleyProtocolVersionGroup @AllegraEra

instance HuddleRule "protocol_param_update" AllegraEra where
  huddleRule = protocolParamUpdateRule @AllegraEra

instance HuddleRule "proposed_protocol_parameter_updates" AllegraEra where
  huddleRule = proposedProtocolParameterUpdatesRule @AllegraEra

instance HuddleRule "update" AllegraEra where
  huddleRule = updateRule @AllegraEra

instance HuddleRule "genesis_hash" AllegraEra where
  huddleRule = genesisHashRule @AllegraEra

instance HuddleGroup "operational_cert" AllegraEra where
  huddleGroup = shelleyOperationalCertGroup @AllegraEra

instance HuddleRule "header_body" AllegraEra where
  huddleRule = headerBodyRule @AllegraEra

instance HuddleRule "header" AllegraEra where
  huddleRule = headerRule @AllegraEra

instance Era era => HuddleRule "min_int64" era where
  huddleRule _ = "min_int64" =:= (-9223372036854775808 :: Integer)

instance Era era => HuddleRule "max_int64" era where
  huddleRule _ = "max_int64" =:= (9223372036854775807 :: Integer)

instance Era era => HuddleRule "int64" era where
  huddleRule p = "int64" =:= huddleRule @"min_int64" p ... huddleRule @"max_int64" p

instance HuddleGroup "script_all" AllegraEra where
  huddleGroup = scriptAllGroup @AllegraEra

instance HuddleGroup "script_any" AllegraEra where
  huddleGroup = scriptAnyGroup @AllegraEra

instance HuddleGroup "script_n_of_k" AllegraEra where
  huddleGroup = scriptNOfKGroup @AllegraEra

instance HuddleGroup "script_invalid_before" AllegraEra where
  huddleGroup = scriptInvalidBeforeGroup @AllegraEra

instance HuddleGroup "script_invalid_hereafter" AllegraEra where
  huddleGroup = scriptInvalidHereafterGroup @AllegraEra

instance HuddleRule "native_script" AllegraEra where
  huddleRule = nativeScriptRule @AllegraEra

instance HuddleRule "vkeywitness" AllegraEra where
  huddleRule = vkeywitnessRule @AllegraEra

instance HuddleRule "bootstrap_witness" AllegraEra where
  huddleRule = bootstrapWitnessRule @AllegraEra

instance HuddleRule "transaction_witness_set" AllegraEra where
  huddleRule = transactionWitnessSetRule @AllegraEra

instance HuddleGroup "script_pubkey" AllegraEra where
  huddleGroup = scriptPubkeyGroup @AllegraEra

instance HuddleRule "transaction_id" AllegraEra where
  huddleRule = transactionIdRule @AllegraEra

instance HuddleRule "transaction_input" AllegraEra where
  huddleRule = transactionInputRule @AllegraEra

instance HuddleRule "transaction_output" AllegraEra where
  huddleRule = transactionOutputRule @AllegraEra

instance HuddleRule "dns_name" AllegraEra where
  huddleRule _ = dnsNameRule

instance HuddleRule "url" AllegraEra where
  huddleRule _ = urlRule

instance HuddleRule "pool_metadata" AllegraEra where
  huddleRule = poolMetadataRule @AllegraEra

instance HuddleGroup "single_host_addr" AllegraEra where
  huddleGroup = singleHostAddrGroup @AllegraEra

instance HuddleGroup "single_host_name" AllegraEra where
  huddleGroup = singleHostNameGroup @AllegraEra

instance HuddleGroup "multi_host_name" AllegraEra where
  huddleGroup = multiHostNameGroup @AllegraEra

instance HuddleRule "relay" AllegraEra where
  huddleRule = relayRule @AllegraEra

instance HuddleGroup "pool_params" AllegraEra where
  huddleGroup = poolParamsGroup @AllegraEra

instance HuddleGroup "pool_registration_cert" AllegraEra where
  huddleGroup = poolRegistrationCertGroup @AllegraEra

instance HuddleGroup "pool_retirement_cert" AllegraEra where
  huddleGroup = poolRetirementCertGroup @AllegraEra

instance HuddleRule "genesis_delegate_hash" AllegraEra where
  huddleRule = genesisDelegateHashRule @AllegraEra

instance HuddleGroup "genesis_delegation_cert" AllegraEra where
  huddleGroup = genesisDelegationCertGroup @AllegraEra

instance HuddleRule "delta_coin" AllegraEra where
  huddleRule _ = deltaCoinRule

instance HuddleRule "move_instantaneous_reward" AllegraEra where
  huddleRule = moveInstantaneousRewardRule @AllegraEra

instance HuddleGroup "move_instantaneous_rewards_cert" AllegraEra where
  huddleGroup = moveInstantaneousRewardsCertGroup @AllegraEra

instance HuddleGroup "account_registration_cert" AllegraEra where
  huddleGroup = accountRegistrationCertGroup @AllegraEra

instance HuddleGroup "account_unregistration_cert" AllegraEra where
  huddleGroup = accountUnregistrationCertGroup @AllegraEra

instance HuddleGroup "delegation_to_stake_pool_cert" AllegraEra where
  huddleGroup = delegationToStakePoolCertGroup @AllegraEra

instance HuddleRule "certificate" AllegraEra where
  huddleRule = certificateRule @AllegraEra

instance HuddleRule "withdrawals" AllegraEra where
  huddleRule = shelleyWithdrawalsRule @AllegraEra

instance HuddleRule "auxiliary_scripts" AllegraEra where
  huddleRule = auxiliaryScriptsRule @AllegraEra

instance HuddleRule "auxiliary_data_array" AllegraEra where
  huddleRule = auxiliaryDataArrayRule @AllegraEra

instance HuddleRule "auxiliary_data" AllegraEra where
  huddleRule = auxiliaryDataRule @AllegraEra

instance HuddleRule "transaction_body" AllegraEra where
  huddleRule p =
    comment
      [str|Allegra transaction body adds the validity interval start at index 8
          |]
      $ "transaction_body"
        =:= mp
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
  huddleRule = transactionRule @AllegraEra

instance HuddleRule "block" AllegraEra where
  huddleRule = blockRule @AllegraEra

instance HuddleRule1 "set" AllegraEra where
  huddleRule1 _ = huddleRule1 @"set" (Proxy @ShelleyEra)

instance HuddleRule1 "nonempty_set" AllegraEra where
  huddleRule1 _ = huddleRule1 @"nonempty_set" (Proxy @ShelleyEra)

instance HuddleRule1 "nonempty_oset" AllegraEra where
  huddleRule1 _ = huddleRule1 @"nonempty_oset" (Proxy @ShelleyEra)
