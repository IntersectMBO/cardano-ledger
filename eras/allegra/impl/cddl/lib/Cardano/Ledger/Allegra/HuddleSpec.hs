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
      , a (huddleRule @"auxiliary_data" p / VNil)
      ]

auxiliaryScriptsRule ::
  forall name era. (KnownSymbol name, HuddleRule "native_script" era) => Proxy name -> Proxy era -> Rule
auxiliaryScriptsRule pname p = pname =.= arr [0 <+ a (huddleRule @"native_script" p)]

auxiliaryDataArrayRule ::
  forall name era. (KnownSymbol name, HuddleRule "auxiliary_scripts" era) => Proxy name -> Proxy era -> Rule
auxiliaryDataArrayRule pname p =
  pname
    =.= arr
      [ "transaction_metadata" ==> huddleRule @"metadata" p
      , "auxiliary_scripts" ==> huddleRule @"auxiliary_scripts" p
      ]

auxiliaryDataRule ::
  forall name era. (KnownSymbol name, HuddleRule "auxiliary_data_array" era) => Proxy name -> Proxy era -> Rule
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
  Named Group
scriptNOfKGroup pname p =
  pname
    =.~ grp
      [ 3
      , "n" ==> huddleRule @"int64" p
      , a $ arr [0 <+ a (huddleRule @"native_script" p)]
      ]

scriptInvalidBeforeGroup ::
  forall name era. (KnownSymbol name, Era era) => Proxy name -> Proxy era -> Named Group
scriptInvalidBeforeGroup pname p =
  comment
    [str|Timelock validity intervals are half-open intervals [a, b).
        |This field specifies the left (included) endpoint a.
        |]
    $ pname
      =.~ grp [4, a (huddleRule @"slot" p)]

scriptInvalidHereafterGroup ::
  forall name era. (KnownSymbol name, Era era) => Proxy name -> Proxy era -> Named Group
scriptInvalidHereafterGroup pname p =
  comment
    [str|Timelock validity intervals are half-open intervals [a, b).
        |This field specifies the right (excluded) endpoint b.
        |]
    $ pname
      =.~ grp [5, a (huddleRule @"slot" p)]

instance HuddleRule "major_protocol_version" AllegraEra where
  huddleRuleNamed pname p = majorProtocolVersionRule pname p

instance HuddleGroup "protocol_version" AllegraEra where
  huddleGroupNamed pname p = shelleyProtocolVersionGroup pname p

instance HuddleRule "protocol_param_update" AllegraEra where
  huddleRuleNamed pname p = protocolParamUpdateRule pname p

instance HuddleRule "proposed_protocol_parameter_updates" AllegraEra where
  huddleRuleNamed pname p = proposedProtocolParameterUpdatesRule pname p

instance HuddleRule "update" AllegraEra where
  huddleRuleNamed pname p = updateRule pname p

instance HuddleRule "genesis_hash" AllegraEra where
  huddleRuleNamed pname p = genesisHashRule pname p

instance HuddleGroup "operational_cert" AllegraEra where
  huddleGroupNamed pname p = shelleyOperationalCertGroup pname p

instance HuddleRule "header_body" AllegraEra where
  huddleRuleNamed pname p = headerBodyRule pname p

instance HuddleRule "header" AllegraEra where
  huddleRuleNamed pname p = headerRule pname p

instance Era era => HuddleRule "min_int64" era where
  huddleRuleNamed pname _ = pname =.= (-9223372036854775808 :: Integer)

instance Era era => HuddleRule "max_int64" era where
  huddleRuleNamed pname _ = pname =.= (9223372036854775807 :: Integer)

instance Era era => HuddleRule "int64" era where
  huddleRuleNamed pname p = pname =.= huddleRule @"min_int64" p ... huddleRule @"max_int64" p

instance HuddleGroup "script_all" AllegraEra where
  huddleGroupNamed pname p = scriptAllGroup pname p

instance HuddleGroup "script_any" AllegraEra where
  huddleGroupNamed pname p = scriptAnyGroup pname p

instance HuddleGroup "script_n_of_k" AllegraEra where
  huddleGroupNamed pname p = scriptNOfKGroup pname p

instance HuddleGroup "script_invalid_before" AllegraEra where
  huddleGroupNamed pname p = scriptInvalidBeforeGroup pname p

instance HuddleGroup "script_invalid_hereafter" AllegraEra where
  huddleGroupNamed pname p = scriptInvalidHereafterGroup pname p

instance HuddleRule "native_script" AllegraEra where
  huddleRuleNamed pname p = nativeScriptRule pname p

instance HuddleRule "vkeywitness" AllegraEra where
  huddleRuleNamed pname p = vkeywitnessRule pname p

instance HuddleRule "bootstrap_witness" AllegraEra where
  huddleRuleNamed pname p = bootstrapWitnessRule pname p

instance HuddleRule "transaction_witness_set" AllegraEra where
  huddleRuleNamed pname p = transactionWitnessSetRule pname p

instance HuddleGroup "script_pubkey" AllegraEra where
  huddleGroupNamed pname p = scriptPubkeyGroup pname p

instance HuddleRule "transaction_id" AllegraEra where
  huddleRuleNamed pname p = transactionIdRule pname p

instance HuddleRule "transaction_input" AllegraEra where
  huddleRuleNamed pname p = transactionInputRule pname p

instance HuddleRule "transaction_output" AllegraEra where
  huddleRuleNamed pname p = transactionOutputRule pname p

instance HuddleRule "dns_name" AllegraEra where
  huddleRuleNamed pname _ = dnsNameRule pname

instance HuddleRule "url" AllegraEra where
  huddleRuleNamed pname _ = urlRule pname

instance HuddleRule "pool_metadata" AllegraEra where
  huddleRuleNamed pname p = poolMetadataRule pname p

instance HuddleGroup "single_host_addr" AllegraEra where
  huddleGroupNamed pname p = singleHostAddrGroup pname p

instance HuddleGroup "single_host_name" AllegraEra where
  huddleGroupNamed pname p = singleHostNameGroup pname p

instance HuddleGroup "multi_host_name" AllegraEra where
  huddleGroupNamed pname p = multiHostNameGroup pname p

instance HuddleRule "relay" AllegraEra where
  huddleRuleNamed pname p = relayRule pname p

instance HuddleGroup "pool_params" AllegraEra where
  huddleGroupNamed pname p = poolParamsGroup pname p

instance HuddleGroup "pool_registration_cert" AllegraEra where
  huddleGroupNamed pname p = poolRegistrationCertGroup pname p

instance HuddleGroup "pool_retirement_cert" AllegraEra where
  huddleGroupNamed pname p = poolRetirementCertGroup pname p

instance HuddleRule "genesis_delegate_hash" AllegraEra where
  huddleRuleNamed pname p = genesisDelegateHashRule pname p

instance HuddleGroup "genesis_delegation_cert" AllegraEra where
  huddleGroupNamed pname p = genesisDelegationCertGroup pname p

instance HuddleRule "delta_coin" AllegraEra where
  huddleRuleNamed pname _ = deltaCoinRule pname

instance HuddleRule "move_instantaneous_reward" AllegraEra where
  huddleRuleNamed pname p = moveInstantaneousRewardRule pname p

instance HuddleGroup "move_instantaneous_rewards_cert" AllegraEra where
  huddleGroupNamed pname p = moveInstantaneousRewardsCertGroup pname p

instance HuddleGroup "account_registration_cert" AllegraEra where
  huddleGroupNamed pname p = accountRegistrationCertGroup pname p

instance HuddleGroup "account_unregistration_cert" AllegraEra where
  huddleGroupNamed pname p = accountUnregistrationCertGroup pname p

instance HuddleGroup "delegation_to_stake_pool_cert" AllegraEra where
  huddleGroupNamed pname p = delegationToStakePoolCertGroup pname p

instance HuddleRule "certificate" AllegraEra where
  huddleRuleNamed pname p = certificateRule pname p

instance HuddleRule "withdrawals" AllegraEra where
  huddleRuleNamed pname p = shelleyWithdrawalsRule pname p

instance HuddleRule "auxiliary_scripts" AllegraEra where
  huddleRuleNamed pname p = auxiliaryScriptsRule pname p

instance HuddleRule "auxiliary_data_array" AllegraEra where
  huddleRuleNamed pname p = auxiliaryDataArrayRule pname p

instance HuddleRule "auxiliary_data" AllegraEra where
  huddleRuleNamed pname p = auxiliaryDataRule pname p

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
  huddleRuleNamed pname p = transactionRule pname p

instance HuddleRule "block" AllegraEra where
  huddleRuleNamed pname p = blockRule pname p

instance HuddleRule1 "set" AllegraEra where
  huddleRule1Named _ _ = huddleRule1 @"set" (Proxy @ShelleyEra)

instance HuddleRule1 "nonempty_set" AllegraEra where
  huddleRule1Named _ _ = huddleRule1 @"nonempty_set" (Proxy @ShelleyEra)

instance HuddleRule1 "nonempty_oset" AllegraEra where
  huddleRule1Named _ _ = huddleRule1 @"nonempty_oset" (Proxy @ShelleyEra)
