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

module Cardano.Ledger.Dijkstra.HuddleSpec (
  module Cardano.Ledger.Conway.HuddleSpec,
  DijkstraEra,
  dijkstraCDDL,
  dijkstraMultiasset,
  dijkstraValueRule,
  guardsRule,
  subTransactionsRule,
  subTransactionRule,
  subTransactionBodyRule,
  requiredTopLevelGuardsRule,
  dijkstraScriptRule,
  dijkstraNativeScriptRule,
  scriptRequireGuardGroup,
  dijkstraRedeemerTagRule,
  auxiliaryDataMapRule,
) where

import Cardano.Ledger.Conway.HuddleSpec hiding ()
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import Data.Text ()
import Data.Word (Word64)
import Text.Heredoc
import Prelude hiding ((/))

dijkstraCDDL :: Huddle
dijkstraCDDL =
  collectFromInit
    [ HIRule $ huddleRule @"block" (Proxy @DijkstraEra)
    , HIRule $ huddleRule @"transaction" (Proxy @DijkstraEra)
    , HIRule $ huddleRule @"kes_signature" (Proxy @DijkstraEra)
    , HIRule $ huddleRule @"language" (Proxy @DijkstraEra)
    , HIRule $ huddleRule @"potential_languages" (Proxy @DijkstraEra)
    , HIRule $ huddleRule @"signkey_kes" (Proxy @DijkstraEra)
    , HIRule $ huddleRule @"certificate" (Proxy @DijkstraEra)
    ]

guardsRule ::
  forall era.
  ( HuddleRule "addr_keyhash" era
  , HuddleRule "credential" era
  , HuddleRule1 "nonempty_set" era
  , HuddleRule1 "nonempty_oset" era
  ) =>
  Proxy era ->
  Rule
guardsRule p =
  "guards"
    =:= huddleRule1 @"nonempty_set" p (huddleRule @"addr_keyhash" p)
    / huddleRule1 @"nonempty_oset" p (huddleRule @"credential" p)

subTransactionsRule ::
  forall era.
  ( HuddleRule "transaction_input" era
  , HuddleRule "transaction_output" era
  , HuddleRule "slot" era
  , HuddleRule "certificates" era
  , HuddleRule "withdrawals" era
  , HuddleRule "auxiliary_data_hash" era
  , HuddleRule "mint" era
  , HuddleRule "script_data_hash" era
  , HuddleRule "network_id" era
  , HuddleRule "voting_procedures" era
  , HuddleRule "proposal_procedures" era
  , HuddleRule "coin" era
  , HuddleRule "positive_coin" era
  , HuddleRule "credential" era
  , HuddleRule "plutus_data" era
  , HuddleRule "transaction_witness_set" era
  , HuddleRule "auxiliary_data" era
  , HuddleRule1 "set" era
  , HuddleRule1 "nonempty_set" era
  , HuddleRule1 "nonempty_oset" era
  ) =>
  Proxy era ->
  Rule
subTransactionsRule p =
  "sub_transactions" =:= huddleRule1 @"nonempty_oset" p (subTransactionRule p)

subTransactionRule ::
  forall era.
  ( HuddleRule "transaction_input" era
  , HuddleRule "transaction_output" era
  , HuddleRule "slot" era
  , HuddleRule "certificates" era
  , HuddleRule "withdrawals" era
  , HuddleRule "auxiliary_data_hash" era
  , HuddleRule "mint" era
  , HuddleRule "script_data_hash" era
  , HuddleRule "network_id" era
  , HuddleRule "voting_procedures" era
  , HuddleRule "proposal_procedures" era
  , HuddleRule "coin" era
  , HuddleRule "positive_coin" era
  , HuddleRule "credential" era
  , HuddleRule "plutus_data" era
  , HuddleRule "transaction_witness_set" era
  , HuddleRule "auxiliary_data" era
  , HuddleRule1 "set" era
  , HuddleRule1 "nonempty_set" era
  , HuddleRule1 "nonempty_oset" era
  ) =>
  Proxy era ->
  Rule
subTransactionRule p =
  "sub_transaction"
    =:= arr
      [ a (subTransactionBodyRule p)
      , a (huddleRule @"transaction_witness_set" p)
      , a (huddleRule @"auxiliary_data" p / VNil)
      ]

subTransactionBodyRule ::
  forall era.
  ( HuddleRule "transaction_input" era
  , HuddleRule "transaction_output" era
  , HuddleRule "slot" era
  , HuddleRule "certificates" era
  , HuddleRule "withdrawals" era
  , HuddleRule "auxiliary_data_hash" era
  , HuddleRule "mint" era
  , HuddleRule "script_data_hash" era
  , HuddleRule "network_id" era
  , HuddleRule "voting_procedures" era
  , HuddleRule "proposal_procedures" era
  , HuddleRule "coin" era
  , HuddleRule "positive_coin" era
  , HuddleRule "credential" era
  , HuddleRule "plutus_data" era
  , HuddleRule1 "set" era
  , HuddleRule1 "nonempty_set" era
  , HuddleRule1 "nonempty_oset" era
  ) =>
  Proxy era ->
  Rule
subTransactionBodyRule p =
  "sub_transaction_body"
    =:= mp
      [ idx 0 ==> huddleRule1 @"set" p (huddleRule @"transaction_input" p)
      , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
      , opt (idx 3 ==> huddleRule @"slot" p)
      , opt (idx 4 ==> huddleRule @"certificates" p)
      , opt (idx 5 ==> huddleRule @"withdrawals" p)
      , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
      , opt (idx 8 ==> huddleRule @"slot" p)
      , opt (idx 9 ==> huddleRule @"mint" p)
      , opt (idx 11 ==> huddleRule @"script_data_hash" p)
      , opt (idx 14 ==> guardsRule p)
      , opt (idx 15 ==> huddleRule @"network_id" p)
      , opt (idx 18 ==> huddleRule1 @"nonempty_set" p (huddleRule @"transaction_input" p))
      , opt (idx 19 ==> huddleRule @"voting_procedures" p)
      , opt (idx 20 ==> huddleRule @"proposal_procedures" p)
      , opt (idx 21 ==> huddleRule @"coin" p)
      , opt (idx 22 ==> huddleRule @"positive_coin" p)
      , opt (idx 24 ==> requiredTopLevelGuardsRule p)
      ]

requiredTopLevelGuardsRule ::
  forall era.
  ( HuddleRule "credential" era
  , HuddleRule "plutus_data" era
  ) =>
  Proxy era ->
  Rule
requiredTopLevelGuardsRule p =
  "required_top_level_guards"
    =:= mp
      [ 1
          <+ asKey (huddleRule @"credential" p)
          ==> (huddleRule @"plutus_data" p / VNil)
      ]

scriptRequireGuardGroup :: forall era. HuddleRule "credential" era => Proxy era -> GroupDef
scriptRequireGuardGroup p =
  comment
    [str|Dijkstra adds guard scripts for enhanced security.
        |A guard script requires a credential to authorize execution.
        |]
    $ "script_require_guard" =:~ grp [6, a (huddleRule @"credential" p)]

dijkstraNativeScriptRule ::
  forall era.
  ( HuddleGroup "script_pubkey" era
  , HuddleGroup "script_all" era
  , HuddleGroup "script_any" era
  , HuddleGroup "script_n_of_k" era
  , HuddleGroup "script_invalid_before" era
  , HuddleGroup "script_invalid_hereafter" era
  , HuddleRule "credential" era
  ) =>
  Proxy era ->
  Rule
dijkstraNativeScriptRule p =
  comment
    [str|Dijkstra native scripts extend Allegra's 6-variant format
        |with a 7th variant for guard scripts.
        |]
    $ "native_script"
      =:= arr [a (huddleGroup @"script_pubkey" p)]
      / arr [a (huddleGroup @"script_all" p)]
      / arr [a (huddleGroup @"script_any" p)]
      / arr [a (huddleGroup @"script_n_of_k" p)]
      / arr [a (huddleGroup @"script_invalid_before" p)]
      / arr [a (huddleGroup @"script_invalid_hereafter" p)]
      / arr [a (scriptRequireGuardGroup p)]

dijkstraScriptRule ::
  forall era.
  ( HuddleRule "native_script" era
  , HuddleRule "plutus_v1_script" era
  , HuddleRule "plutus_v2_script" era
  , HuddleRule "plutus_v3_script" era
  , HuddleRule "plutus_v4_script" era
  ) =>
  Proxy era ->
  Rule
dijkstraScriptRule p =
  comment
    [str|Dijkstra supports five script types:
        |  0: Native scripts with guard support (7 variants)
        |  1: Plutus V1 scripts
        |  2: Plutus V2 scripts
        |  3: Plutus V3 scripts
        |  4: Plutus V4 scripts (NEW)
        |]
    $ "script"
      =:= arr [0, a (huddleRule @"native_script" p)]
      / arr [1, a (huddleRule @"plutus_v1_script" p)]
      / arr [2, a (huddleRule @"plutus_v2_script" p)]
      / arr [3, a (huddleRule @"plutus_v3_script" p)]
      / arr [4, a (huddleRule @"plutus_v4_script" p)]

dijkstraRedeemerTagRule :: Proxy era -> Rule
dijkstraRedeemerTagRule _ =
  "redeemer_tag"
    =:= (int 0 //- "spend")
    / (int 1 //- "mint")
    / (int 2 //- "cert")
    / (int 3 //- "reward")
    / (int 4 //- "voting")
    / (int 5 //- "proposing")
    / (int 6 //- "guarding")

auxiliaryDataMapRule ::
  forall era.
  ( HuddleRule "metadata" era
  , HuddleRule "native_script" era
  , HuddleRule "plutus_v1_script" era
  , HuddleRule "plutus_v2_script" era
  , HuddleRule "plutus_v3_script" era
  , HuddleRule "plutus_v4_script" era
  ) =>
  Proxy era ->
  Rule
auxiliaryDataMapRule p =
  "auxiliary_data_map"
    =:= tag
      259
      ( mp
          [ opt (idx 0 ==> huddleRule @"metadata" p)
          , opt (idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)])
          , opt (idx 2 ==> arr [0 <+ a (huddleRule @"plutus_v1_script" p)])
          , opt (idx 3 ==> arr [0 <+ a (huddleRule @"plutus_v2_script" p)])
          , opt (idx 4 ==> arr [0 <+ a (huddleRule @"plutus_v3_script" p)])
          , opt (idx 5 ==> arr [0 <+ a (huddleRule @"plutus_v4_script" p)])
          ]
      )

dijkstraValueRule ::
  forall era.
  ( HuddleRule "policy_id" era
  , HuddleRule "asset_name" era
  , HuddleRule "positive_coin" era
  ) =>
  Proxy era ->
  Rule
dijkstraValueRule p =
  "value"
    =:= huddleRule @"coin" p
    / sarr [a $ huddleRule @"coin" p, a $ dijkstraMultiasset p (huddleRule @"positive_coin" p)]

dijkstraMultiasset ::
  forall era a.
  (HuddleRule "policy_id" era, HuddleRule "asset_name" era, IsType0 a) =>
  Proxy era ->
  a ->
  GRuleCall
dijkstraMultiasset p =
  binding $ \x ->
    "multiasset"
      =:= mp
        [ 1
            <+ asKey (huddleRule @"policy_id" p)
            ==> mp [1 <+ asKey (huddleRule @"asset_name" p) ==> x]
        ]

instance HuddleRule "bounded_bytes" DijkstraEra where
  huddleRule _ = boundedBytesRule

instance HuddleRule "distinct_bytes" DijkstraEra where
  huddleRule _ = distinctBytesRule

instance HuddleRule "big_uint" DijkstraEra where
  huddleRule = bigUintRule

instance HuddleRule "big_nint" DijkstraEra where
  huddleRule = bigNintRule

instance HuddleRule "big_int" DijkstraEra where
  huddleRule = bigIntRule

instance HuddleRule "network_id" DijkstraEra where
  huddleRule _ = networkIdRule

instance HuddleRule "dns_name" DijkstraEra where
  huddleRule = dnsNameRule @DijkstraEra

instance HuddleRule "url" DijkstraEra where
  huddleRule = urlRule @DijkstraEra

instance HuddleRule "major_protocol_version" DijkstraEra where
  huddleRule = majorProtocolVersionRule @DijkstraEra

instance HuddleRule "genesis_hash" DijkstraEra where
  huddleRule = genesisHashRule @DijkstraEra

instance HuddleRule "genesis_delegate_hash" DijkstraEra where
  huddleRule = genesisDelegateHashRule @DijkstraEra

instance HuddleRule "transaction_id" DijkstraEra where
  huddleRule = transactionIdRule @DijkstraEra

instance HuddleRule "vkeywitness" DijkstraEra where
  huddleRule = vkeywitnessRule @DijkstraEra

instance HuddleRule "bootstrap_witness" DijkstraEra where
  huddleRule = bootstrapWitnessRule @DijkstraEra

instance HuddleRule "ex_units" DijkstraEra where
  huddleRule _ = exUnitsRule

instance HuddleRule "positive_interval" DijkstraEra where
  huddleRule = positiveIntervalRule

instance HuddleRule "vote" DijkstraEra where
  huddleRule = voteRule @DijkstraEra

instance HuddleRule "asset_name" DijkstraEra where
  huddleRule = assetNameRule @DijkstraEra

instance HuddleRule "drep_credential" DijkstraEra where
  huddleRule = drepCredentialRule @DijkstraEra

instance HuddleRule "committee_cold_credential" DijkstraEra where
  huddleRule = committeeColdCredentialRule @DijkstraEra

instance HuddleRule "committee_hot_credential" DijkstraEra where
  huddleRule = committeeHotCredentialRule @DijkstraEra

instance HuddleRule "anchor" DijkstraEra where
  huddleRule = anchorRule @DijkstraEra

instance HuddleRule "drep" DijkstraEra where
  huddleRule = drepRule @DijkstraEra

instance HuddleRule "voter" DijkstraEra where
  huddleRule = voterRule @DijkstraEra

instance HuddleRule "operational_cert" DijkstraEra where
  huddleRule = babbageOperationalCertRule @DijkstraEra

instance HuddleRule "protocol_version" DijkstraEra where
  huddleRule = babbageProtocolVersionRule @DijkstraEra

instance HuddleRule "policy_id" DijkstraEra where
  huddleRule p = "policy_id" =:= huddleRule @"script_hash" p

instance HuddleRule "policy_hash" DijkstraEra where
  huddleRule = policyHashRule @DijkstraEra

instance HuddleGroup "script_pubkey" DijkstraEra where
  huddleGroup = scriptPubkeyGroup @DijkstraEra

instance HuddleGroup "script_all" DijkstraEra where
  huddleGroup = scriptAllGroup @DijkstraEra

instance HuddleGroup "script_any" DijkstraEra where
  huddleGroup = scriptAnyGroup @DijkstraEra

instance HuddleGroup "script_n_of_k" DijkstraEra where
  huddleGroup = scriptNOfKGroup @DijkstraEra

instance HuddleGroup "script_invalid_before" DijkstraEra where
  huddleGroup = scriptInvalidBeforeGroup @DijkstraEra

instance HuddleGroup "script_invalid_hereafter" DijkstraEra where
  huddleGroup = scriptInvalidHereafterGroup @DijkstraEra

instance HuddleGroup "single_host_addr" DijkstraEra where
  huddleGroup = singleHostAddrGroup @DijkstraEra

instance HuddleGroup "single_host_name" DijkstraEra where
  huddleGroup = singleHostNameGroup @DijkstraEra

instance HuddleGroup "multi_host_name" DijkstraEra where
  huddleGroup = multiHostNameGroup @DijkstraEra

instance HuddleRule "relay" DijkstraEra where
  huddleRule = relayRule @DijkstraEra

instance HuddleRule "pool_metadata" DijkstraEra where
  huddleRule = poolMetadataRule @DijkstraEra

instance HuddleGroup "pool_params" DijkstraEra where
  huddleGroup = poolParamsGroup @DijkstraEra

instance HuddleGroup "account_registration_cert" DijkstraEra where
  huddleGroup = accountRegistrationCertGroup @DijkstraEra

instance HuddleGroup "account_unregistration_cert" DijkstraEra where
  huddleGroup = accountUnregistrationCertGroup @DijkstraEra

instance HuddleGroup "delegation_to_stake_pool_cert" DijkstraEra where
  huddleGroup = delegationToStakePoolCertGroup @DijkstraEra

instance HuddleGroup "pool_registration_cert" DijkstraEra where
  huddleGroup = poolRegistrationCertGroup @DijkstraEra

instance HuddleGroup "pool_retirement_cert" DijkstraEra where
  huddleGroup = poolRetirementCertGroup @DijkstraEra

instance HuddleGroup "account_registration_deposit_cert" DijkstraEra where
  huddleGroup = accountRegistrationDepositCertGroup @DijkstraEra

instance HuddleGroup "account_unregistration_deposit_cert" DijkstraEra where
  huddleGroup = accountUnregistrationDepositCertGroup @DijkstraEra

instance HuddleGroup "delegation_to_drep_cert" DijkstraEra where
  huddleGroup = delegationToDrepCertGroup @DijkstraEra

instance HuddleGroup "delegation_to_stake_pool_and_drep_cert" DijkstraEra where
  huddleGroup = delegationToStakePoolAndDrepCertGroup @DijkstraEra

instance HuddleGroup "account_registration_delegation_to_stake_pool_cert" DijkstraEra where
  huddleGroup = accountRegistrationDelegationToStakePoolCertGroup @DijkstraEra

instance HuddleGroup "account_registration_delegation_to_drep_cert" DijkstraEra where
  huddleGroup = accountRegistrationDelegationToDrepCertGroup @DijkstraEra

instance HuddleGroup "account_registration_delegation_to_stake_pool_and_drep_cert" DijkstraEra where
  huddleGroup = accountRegistrationDelegationToStakePoolAndDrepCertGroup @DijkstraEra

instance HuddleGroup "committee_authorization_cert" DijkstraEra where
  huddleGroup = committeeAuthorizationCertGroup @DijkstraEra

instance HuddleGroup "committee_resignation_cert" DijkstraEra where
  huddleGroup = committeeResignationCertGroup @DijkstraEra

instance HuddleGroup "drep_registration_cert" DijkstraEra where
  huddleGroup = drepRegistrationCertGroup @DijkstraEra

instance HuddleGroup "drep_unregistration_cert" DijkstraEra where
  huddleGroup = drepUnregistrationCertGroup @DijkstraEra

instance HuddleGroup "drep_update_cert" DijkstraEra where
  huddleGroup = drepUpdateCertGroup @DijkstraEra

instance HuddleRule "certificate" DijkstraEra where
  huddleRule = conwayCertificateRule @DijkstraEra

instance HuddleRule "certificates" DijkstraEra where
  huddleRule = certificatesRule @DijkstraEra

instance HuddleRule "voting_procedure" DijkstraEra where
  huddleRule = votingProcedureRule @DijkstraEra

instance HuddleRule "voting_procedures" DijkstraEra where
  huddleRule = votingProceduresRule @DijkstraEra

instance HuddleRule "constitution" DijkstraEra where
  huddleRule = constitutionRule @DijkstraEra

instance HuddleGroup "parameter_change_action" DijkstraEra where
  huddleGroup = parameterChangeActionGroup @DijkstraEra

instance HuddleGroup "hard_fork_initiation_action" DijkstraEra where
  huddleGroup = hardForkInitiationActionGroup @DijkstraEra

instance HuddleGroup "treasury_withdrawals_action" DijkstraEra where
  huddleGroup = treasuryWithdrawalsActionGroup @DijkstraEra

instance HuddleGroup "no_confidence" DijkstraEra where
  huddleGroup = noConfidenceGroup @DijkstraEra

instance HuddleGroup "update_committee" DijkstraEra where
  huddleGroup = updateCommitteeGroup @DijkstraEra

instance HuddleGroup "new_constitution" DijkstraEra where
  huddleGroup = newConstitutionGroup @DijkstraEra

instance HuddleRule "info_action" DijkstraEra where
  huddleRule _ = infoActionRule

instance HuddleRule "gov_action" DijkstraEra where
  huddleRule = govActionRule @DijkstraEra

instance HuddleRule "proposal_procedure" DijkstraEra where
  huddleRule = proposalProcedureRule @DijkstraEra

instance HuddleRule "proposal_procedures" DijkstraEra where
  huddleRule = proposalProceduresRule @DijkstraEra

instance HuddleRule "transaction_input" DijkstraEra where
  huddleRule p =
    "transaction_input"
      =:= arr
        [ "transaction_id" ==> huddleRule @"transaction_id" p
        , "index" ==> (VUInt `sized` (2 :: Word64))
        ]

instance HuddleRule "required_signers" DijkstraEra where
  huddleRule p =
    "required_signers"
      =:= huddleRule1 @"nonempty_set" p (huddleRule @"addr_keyhash" p)

instance HuddleRule "value" DijkstraEra where
  huddleRule = dijkstraValueRule @DijkstraEra

instance HuddleRule "mint" DijkstraEra where
  huddleRule = conwayMintRule @DijkstraEra

instance HuddleRule "withdrawals" DijkstraEra where
  huddleRule = conwayWithdrawalsRule @DijkstraEra

instance HuddleRule "data" DijkstraEra where
  huddleRule = dataRule @DijkstraEra

instance HuddleRule "datum_option" DijkstraEra where
  huddleRule = datumOptionRule @DijkstraEra

instance HuddleRule "alonzo_transaction_output" DijkstraEra where
  huddleRule = alonzoTransactionOutputRule @DijkstraEra

instance HuddleRule "transaction_output" DijkstraEra where
  huddleRule p =
    comment
      [str|Both of the Alonzo and Babbage style TxOut formats are equally valid
          |and can be used interchangeably
          |]
      $ "transaction_output"
        =:= huddleRule @"alonzo_transaction_output" p
        / babbageTransactionOutput p (huddleRule @"script" p)

instance HuddleRule "potential_languages" DijkstraEra where
  huddleRule = potentialLanguagesRule

instance HuddleRule "redeemers" DijkstraEra where
  huddleRule p =
    comment
      [str|Dijkstra uses map format only for redeemers.
          |The flat array format has been removed.
          |]
      $ "redeemers"
        =:= mp
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

instance HuddleRule "script_data_hash" DijkstraEra where
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

instance HuddleRule "transaction" DijkstraEra where
  huddleRule p =
    "transaction"
      =:= arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a VBool
        , a (huddleRule @"auxiliary_data" p / VNil)
        ]

instance HuddleRule "ex_unit_prices" DijkstraEra where
  huddleRule p =
    "ex_unit_prices"
      =:= arr
        [ "mem_price" ==> huddleRule @"nonnegative_interval" p
        , "step_price" ==> huddleRule @"nonnegative_interval" p
        ]

instance HuddleRule "pool_voting_thresholds" DijkstraEra where
  huddleRule = poolVotingThresholdsRule @DijkstraEra

instance HuddleRule "drep_voting_thresholds" DijkstraEra where
  huddleRule = drepVotingThresholdsRule @DijkstraEra

instance HuddleRule "protocol_param_update" DijkstraEra where
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
        , opt (idx 33 ==> huddleRule @"nonnegative_interval" p) //- "minfee refScript coins per byte"
        , opt (idx 34 ==> (VUInt `sized` (4 :: Word64))) //- "max refScript size per block"
        , opt (idx 35 ==> (VUInt `sized` (4 :: Word64))) //- "max refScript size per tx"
        , opt (idx 36 ==> huddleRule @"positive_word32" p) //- "refScript cost stride"
        , opt (idx 37 ==> huddleRule @"positive_interval" p) //- "refScript cost multiplier"
        ]

instance HuddleRule "proposed_protocol_parameter_updates" DijkstraEra where
  huddleRule = proposedProtocolParameterUpdatesRule @DijkstraEra

instance HuddleRule "update" DijkstraEra where
  huddleRule = updateRule @DijkstraEra

instance HuddleRule "header_body" DijkstraEra where
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

instance HuddleRule "header" DijkstraEra where
  huddleRule = headerRule @DijkstraEra

instance HuddleRule "block" DijkstraEra where
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

instance HuddleRule "auxiliary_scripts" DijkstraEra where
  huddleRule = auxiliaryScriptsRule @DijkstraEra

instance HuddleRule "auxiliary_data_array" DijkstraEra where
  huddleRule = auxiliaryDataArrayRule @DijkstraEra

instance HuddleRule "transaction_body" DijkstraEra where
  huddleRule p =
    "transaction_body"
      =:= mp
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
        , opt (idx 14 ==> guardsRule p) //- "guards (replaces required_signers)"
        , opt (idx 15 ==> huddleRule @"network_id" p)
        , opt (idx 16 ==> huddleRule @"transaction_output" p) //- "collateral return"
        , opt (idx 17 ==> huddleRule @"coin" p) //- "total collateral"
        , opt (idx 18 ==> huddleRule1 @"nonempty_set" p (huddleRule @"transaction_input" p))
            //- "reference inputs"
        , opt (idx 19 ==> huddleRule @"voting_procedures" p)
        , opt (idx 20 ==> huddleRule @"proposal_procedures" p)
        , opt (idx 21 ==> huddleRule @"coin" p) //- "current treasury value"
        , opt (idx 22 ==> huddleRule @"positive_coin" p) //- "donation"
        , opt (idx 23 ==> subTransactionsRule p) //- "sub-transactions (NEW)"
        ]

instance HuddleRule "transaction_witness_set" DijkstraEra where
  huddleRule p =
    "transaction_witness_set"
      =:= mp
        [ opt $ idx 0 ==> huddleRule1 @"nonempty_set" p (huddleRule @"vkeywitness" p)
        , opt $ idx 1 ==> huddleRule1 @"nonempty_set" p (huddleRule @"native_script" p)
        , opt $ idx 2 ==> huddleRule1 @"nonempty_set" p (huddleRule @"bootstrap_witness" p)
        , opt $ idx 3 ==> huddleRule1 @"nonempty_set" p (huddleRule @"plutus_v1_script" p)
        , opt $ idx 4 ==> huddleRule1 @"nonempty_set" p (huddleRule @"plutus_data" p)
        , opt $ idx 5 ==> huddleRule @"redeemers" p
        , opt $ idx 6 ==> huddleRule1 @"nonempty_set" p (huddleRule @"plutus_v2_script" p)
        , opt $ idx 7 ==> huddleRule1 @"nonempty_set" p (huddleRule @"plutus_v3_script" p)
        -- TODO: Add plutus_v4_script at index 8 once AlonzoTxWitsRaw encoder/decoder supports it
        ]

instance HuddleRule "native_script" DijkstraEra where
  huddleRule = dijkstraNativeScriptRule @DijkstraEra

instance HuddleRule "script" DijkstraEra where
  huddleRule = dijkstraScriptRule @DijkstraEra

instance HuddleRule "redeemer_tag" DijkstraEra where
  huddleRule _ = dijkstraRedeemerTagRule (Proxy @DijkstraEra)

instance (Era era, HuddleRule "distinct_bytes" era) => HuddleRule "plutus_v4_script" era where
  huddleRule p =
    comment
      [str|Dijkstra introduces Plutus V4.
          |
          |Note: distinct VBytes ensures uniqueness in test generation.
          |]
      $ "plutus_v4_script" =:= huddleRule @"distinct_bytes" p

instance HuddleRule "auxiliary_data" DijkstraEra where
  huddleRule p =
    comment
      [str|auxiliary_data supports three serialization formats:
          |  1. metadata (raw) - Supported since Shelley
          |  2. auxiliary_data_array - Array format, introduced in Allegra
          |  3. auxiliary_data_map - Tagged map format, introduced in Alonzo
          |     Dijkstra adds plutus_v4_script support at index 5
          |]
      $ "auxiliary_data"
        =:= huddleRule @"metadata" p
        / huddleRule @"auxiliary_data_array" p
        / auxiliaryDataMapRule p

instance HuddleRule "auxiliary_data_map" DijkstraEra where
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
            , opt (idx 5 ==> arr [0 <+ a (huddleRule @"plutus_v4_script" p)])
            ]
        )

instance HuddleRule "language" DijkstraEra where
  huddleRule _ =
    comment
      [str|0: Plutus v1
          |1: Plutus v2
          |2: Plutus v3
          |3: Plutus v4 (NEW)
          |]
      $ "language" =:= (0 :: Integer) ... (3 :: Integer)

instance HuddleRule "cost_models" DijkstraEra where
  huddleRule p =
    comment
      [str|The format for cost_models is flexible enough to allow adding
          |Plutus built-ins and language versions in the future.
          |
          |Plutus v1: only 166 integers are used, but more are accepted (and ignored)
          |Plutus v2: only 175 integers are used, but more are accepted (and ignored)
          |Plutus v3: only 223 integers are used, but more are accepted (and ignored)
          |Plutus v4: TBD integers are used (NEW)
          |
          |Any 8-bit unsigned number can be used as a key.
          |]
      $ "cost_models"
        =:= mp
          [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 3 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , 0 <+ asKey ((4 :: Integer) ... (255 :: Integer)) ==> arr [0 <+ a (huddleRule @"int64" p)]
          ]

mkMaybeTaggedSet :: IsType0 a => Name -> Word64 -> a -> GRuleCall
mkMaybeTaggedSet label n = binding $ \x -> label =:= tag 258 (arr [n <+ a x]) / sarr [n <+ a x]

maybeTaggedSet :: IsType0 a => a -> GRuleCall
maybeTaggedSet = mkMaybeTaggedSet "set" 0

maybeTaggedNonemptySet :: IsType0 a => a -> GRuleCall
maybeTaggedNonemptySet = mkMaybeTaggedSet "nonempty_set" 1

maybeTaggedNonemptyOset :: IsType0 a => a -> GRuleCall
maybeTaggedNonemptyOset = mkMaybeTaggedSet "nonempty_oset" 1

instance HuddleRule1 "set" DijkstraEra where
  huddleRule1 _ = maybeTaggedSet

instance HuddleRule1 "nonempty_set" DijkstraEra where
  huddleRule1 _ = maybeTaggedNonemptySet

instance HuddleRule1 "nonempty_oset" DijkstraEra where
  huddleRule1 _ = maybeTaggedNonemptyOset
