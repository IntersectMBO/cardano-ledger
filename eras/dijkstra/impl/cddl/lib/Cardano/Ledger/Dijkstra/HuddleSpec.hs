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

-- | Dijkstra constrains the minor protocol version to Word32 (uint .size 4).
dijkstraProtocolVersionRule ::
  forall era.
  HuddleRule "major_protocol_version" era => Proxy "protocol_version" -> Proxy era -> Rule
dijkstraProtocolVersionRule pname p =
  pname =.= arr [a $ huddleRule @"major_protocol_version" p, a $ VUInt `sized` (4 :: Word64)]

guardsRule ::
  forall era.
  ( HuddleRule "addr_keyhash" era
  , HuddleRule "credential" era
  , HuddleRule1 "nonempty_set" era
  , HuddleRule1 "nonempty_oset" era
  ) =>
  Proxy "guards" ->
  Proxy era ->
  Rule
guardsRule pname p =
  pname
    =.= huddleRule1 @"nonempty_set" p (huddleRule @"addr_keyhash" p)
    / huddleRule1 @"nonempty_oset" p (huddleRule @"credential" p)

subTransactionsRule ::
  forall era.
  ( HuddleRule "sub_transaction" era
  , HuddleRule1 "nonempty_oset" era
  ) =>
  Proxy "sub_transactions" ->
  Proxy era ->
  Rule
subTransactionsRule pname p =
  pname =.= huddleRule1 @"nonempty_oset" p (huddleRule @"sub_transaction" p)

subTransactionRule ::
  forall era.
  ( HuddleRule "sub_transaction_body" era
  , HuddleRule "transaction_witness_set" era
  , HuddleRule "auxiliary_data" era
  ) =>
  Proxy "sub_transaction" ->
  Proxy era ->
  Rule
subTransactionRule pname p =
  pname
    =.= arr
      [ a (huddleRule @"sub_transaction_body" p)
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
  , HuddleRule "guards" era
  , HuddleRule "required_top_level_guards" era
  , HuddleRule1 "set" era
  , HuddleRule1 "nonempty_set" era
  ) =>
  Proxy "sub_transaction_body" ->
  Proxy era ->
  Rule
subTransactionBodyRule pname p =
  pname
    =.= mp
      [ idx 0 ==> huddleRule1 @"set" p (huddleRule @"transaction_input" p)
      , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
      , opt (idx 3 ==> huddleRule @"slot" p)
      , opt (idx 4 ==> huddleRule @"certificates" p)
      , opt (idx 5 ==> huddleRule @"withdrawals" p)
      , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
      , opt (idx 8 ==> huddleRule @"slot" p)
      , opt (idx 9 ==> huddleRule @"mint" p)
      , opt (idx 11 ==> huddleRule @"script_data_hash" p)
      , opt (idx 14 ==> huddleRule @"guards" p)
      , opt (idx 15 ==> huddleRule @"network_id" p)
      , opt (idx 18 ==> huddleRule1 @"nonempty_set" p (huddleRule @"transaction_input" p))
      , opt (idx 19 ==> huddleRule @"voting_procedures" p)
      , opt (idx 20 ==> huddleRule @"proposal_procedures" p)
      , opt (idx 21 ==> huddleRule @"coin" p)
      , opt (idx 22 ==> huddleRule @"positive_coin" p)
      , opt (idx 24 ==> huddleRule @"required_top_level_guards" p)
      ]

requiredTopLevelGuardsRule ::
  forall era.
  ( HuddleRule "credential" era
  , HuddleRule "plutus_data" era
  ) =>
  Proxy "required_top_level_guards" ->
  Proxy era ->
  Rule
requiredTopLevelGuardsRule pname p =
  pname
    =.= mp
      [ 1
          <+ asKey (huddleRule @"credential" p)
          ==> (huddleRule @"plutus_data" p / VNil)
      ]

scriptRequireGuardGroup ::
  forall era.
  HuddleRule "credential" era => Proxy "script_require_guard" -> Proxy era -> GroupDef
scriptRequireGuardGroup pname p =
  comment
    [str|Dijkstra adds guard scripts for enhanced security.
        |A guard script requires a credential to authorize execution.
        |]
    $ pname =.~ grp [6, a (huddleRule @"credential" p)]

dijkstraNativeScriptRule ::
  forall era.
  ( HuddleGroup "script_pubkey" era
  , HuddleGroup "script_all" era
  , HuddleGroup "script_any" era
  , HuddleGroup "script_n_of_k" era
  , HuddleGroup "script_invalid_before" era
  , HuddleGroup "script_invalid_hereafter" era
  , HuddleGroup "script_require_guard" era
  ) =>
  Proxy "native_script" ->
  Proxy era ->
  Rule
dijkstraNativeScriptRule pname p =
  comment
    [str|Dijkstra native scripts extend Allegra's 6-variant format
        |with a 7th variant for guard scripts.
        |]
    $ pname
      =.= arr [a (huddleGroup @"script_pubkey" p)]
      / arr [a (huddleGroup @"script_all" p)]
      / arr [a (huddleGroup @"script_any" p)]
      / arr [a (huddleGroup @"script_n_of_k" p)]
      / arr [a (huddleGroup @"script_invalid_before" p)]
      / arr [a (huddleGroup @"script_invalid_hereafter" p)]
      / arr [a (huddleGroup @"script_require_guard" p)]

dijkstraScriptRule ::
  forall era.
  ( HuddleRule "native_script" era
  , HuddleRule "plutus_v1_script" era
  , HuddleRule "plutus_v2_script" era
  , HuddleRule "plutus_v3_script" era
  , HuddleRule "plutus_v4_script" era
  ) =>
  Proxy "script" ->
  Proxy era ->
  Rule
dijkstraScriptRule pname p =
  comment
    [str|Dijkstra supports five script types:
        |  0: Native scripts with guard support (7 variants)
        |  1: Plutus V1 scripts
        |  2: Plutus V2 scripts
        |  3: Plutus V3 scripts
        |  4: Plutus V4 scripts (NEW)
        |]
    $ pname
      =.= arr [0, a (huddleRule @"native_script" p)]
      / arr [1, a (huddleRule @"plutus_v1_script" p)]
      / arr [2, a (huddleRule @"plutus_v2_script" p)]
      / arr [3, a (huddleRule @"plutus_v3_script" p)]
      / arr [4, a (huddleRule @"plutus_v4_script" p)]

dijkstraRedeemerTagRule :: Proxy "redeemer_tag" -> Rule
dijkstraRedeemerTagRule pname =
  pname
    =.= (int 0 //- "spend")
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
  Proxy "auxiliary_data_map" ->
  Proxy era ->
  Rule
auxiliaryDataMapRule pname p =
  pname
    =.= tag
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
  ( HuddleRule "positive_coin" era
  , HuddleRule1 "multiasset" era
  ) =>
  Proxy "value" ->
  Proxy era ->
  Rule
dijkstraValueRule pname p =
  pname
    =.= huddleRule @"coin" p
    / sarr
      [ a $ huddleRule @"coin" p
      , a $ huddleRule1 @"multiasset" p (huddleRule @"positive_coin" p)
      ]

dijkstraMultiasset ::
  forall era a.
  (HuddleRule "policy_id" era, HuddleRule "asset_name" era, IsType0 a) =>
  Proxy "multiasset" ->
  Proxy era ->
  a ->
  GRuleCall
dijkstraMultiasset pname p =
  binding $ \x ->
    pname
      =.= mp
        [ 1
            <+ asKey (huddleRule @"policy_id" p)
            ==> mp [1 <+ asKey (huddleRule @"asset_name" p) ==> x]
        ]

instance HuddleRule "bounded_bytes" DijkstraEra where
  huddleRuleNamed pname _ = boundedBytesRule pname

instance HuddleRule "distinct_bytes" DijkstraEra where
  huddleRuleNamed pname _ = distinctBytesRule pname

instance HuddleRule "big_uint" DijkstraEra where
  huddleRuleNamed = bigUintRule

instance HuddleRule "big_nint" DijkstraEra where
  huddleRuleNamed = bigNintRule

instance HuddleRule "big_int" DijkstraEra where
  huddleRuleNamed = bigIntRule

instance HuddleRule "network_id" DijkstraEra where
  huddleRuleNamed pname _ = networkIdRule pname

instance HuddleRule "dns_name" DijkstraEra where
  huddleRuleNamed pname _ = dnsNameRule pname

instance HuddleRule "url" DijkstraEra where
  huddleRuleNamed pname _ = urlRule pname

instance HuddleRule "major_protocol_version" DijkstraEra where
  huddleRuleNamed = majorProtocolVersionRule

instance HuddleRule "genesis_hash" DijkstraEra where
  huddleRuleNamed = genesisHashRule

instance HuddleRule "genesis_delegate_hash" DijkstraEra where
  huddleRuleNamed = genesisDelegateHashRule

instance HuddleRule "transaction_id" DijkstraEra where
  huddleRuleNamed = transactionIdRule

instance HuddleRule "vkeywitness" DijkstraEra where
  huddleRuleNamed = vkeywitnessRule

instance HuddleRule "bootstrap_witness" DijkstraEra where
  huddleRuleNamed = bootstrapWitnessRule

instance HuddleRule "ex_units" DijkstraEra where
  huddleRuleNamed pname _ = exUnitsRule pname

instance HuddleRule "positive_interval" DijkstraEra where
  huddleRuleNamed = positiveIntervalRule

instance HuddleRule "vote" DijkstraEra where
  huddleRuleNamed pname _ = voteRule pname

instance HuddleRule "asset_name" DijkstraEra where
  huddleRuleNamed pname _ = assetNameRule pname

instance HuddleRule "drep_credential" DijkstraEra where
  huddleRuleNamed = drepCredentialRule

instance HuddleRule "committee_cold_credential" DijkstraEra where
  huddleRuleNamed = committeeColdCredentialRule

instance HuddleRule "committee_hot_credential" DijkstraEra where
  huddleRuleNamed = committeeHotCredentialRule

instance HuddleRule "anchor" DijkstraEra where
  huddleRuleNamed = anchorRule

instance HuddleRule "drep" DijkstraEra where
  huddleRuleNamed = drepRule

instance HuddleRule "voter" DijkstraEra where
  huddleRuleNamed = voterRule

instance HuddleRule "operational_cert" DijkstraEra where
  huddleRuleNamed = babbageOperationalCertRule

instance HuddleRule "protocol_version" DijkstraEra where
  huddleRuleNamed = dijkstraProtocolVersionRule

instance HuddleRule "policy_id" DijkstraEra where
  huddleRuleNamed pname p = pname =.= huddleRule @"script_hash" p

instance HuddleRule "guardrails_script_hash" DijkstraEra where
  huddleRuleNamed = guardrailsScriptHashRule

instance HuddleGroup "script_pubkey" DijkstraEra where
  huddleGroupNamed = scriptPubkeyGroup

instance HuddleGroup "script_all" DijkstraEra where
  huddleGroupNamed = scriptAllGroup

instance HuddleGroup "script_any" DijkstraEra where
  huddleGroupNamed = scriptAnyGroup

instance HuddleGroup "script_n_of_k" DijkstraEra where
  huddleGroupNamed = scriptNOfKGroup

instance HuddleGroup "script_invalid_before" DijkstraEra where
  huddleGroupNamed = scriptInvalidBeforeGroup

instance HuddleGroup "script_invalid_hereafter" DijkstraEra where
  huddleGroupNamed = scriptInvalidHereafterGroup

instance HuddleGroup "script_require_guard" DijkstraEra where
  huddleGroupNamed = scriptRequireGuardGroup

instance HuddleGroup "single_host_addr" DijkstraEra where
  huddleGroupNamed = singleHostAddrGroup

instance HuddleGroup "single_host_name" DijkstraEra where
  huddleGroupNamed = singleHostNameGroup

instance HuddleGroup "multi_host_name" DijkstraEra where
  huddleGroupNamed = multiHostNameGroup

instance HuddleRule "relay" DijkstraEra where
  huddleRuleNamed = relayRule

instance HuddleRule "pool_metadata" DijkstraEra where
  huddleRuleNamed = poolMetadataRule

instance HuddleGroup "pool_params" DijkstraEra where
  huddleGroupNamed = poolParamsGroup

instance HuddleGroup "account_registration_cert" DijkstraEra where
  huddleGroupNamed = accountRegistrationCertGroup

instance HuddleGroup "account_unregistration_cert" DijkstraEra where
  huddleGroupNamed = accountUnregistrationCertGroup

instance HuddleGroup "delegation_to_stake_pool_cert" DijkstraEra where
  huddleGroupNamed = delegationToStakePoolCertGroup

instance HuddleGroup "pool_registration_cert" DijkstraEra where
  huddleGroupNamed = poolRegistrationCertGroup

instance HuddleGroup "pool_retirement_cert" DijkstraEra where
  huddleGroupNamed = poolRetirementCertGroup

instance HuddleGroup "account_registration_deposit_cert" DijkstraEra where
  huddleGroupNamed = accountRegistrationDepositCertGroup

instance HuddleGroup "account_unregistration_deposit_cert" DijkstraEra where
  huddleGroupNamed = accountUnregistrationDepositCertGroup

instance HuddleGroup "delegation_to_drep_cert" DijkstraEra where
  huddleGroupNamed = delegationToDrepCertGroup

instance HuddleGroup "delegation_to_stake_pool_and_drep_cert" DijkstraEra where
  huddleGroupNamed = delegationToStakePoolAndDrepCertGroup

instance HuddleGroup "account_registration_delegation_to_stake_pool_cert" DijkstraEra where
  huddleGroupNamed = accountRegistrationDelegationToStakePoolCertGroup

instance HuddleGroup "account_registration_delegation_to_drep_cert" DijkstraEra where
  huddleGroupNamed = accountRegistrationDelegationToDrepCertGroup

instance HuddleGroup "account_registration_delegation_to_stake_pool_and_drep_cert" DijkstraEra where
  huddleGroupNamed = accountRegistrationDelegationToStakePoolAndDrepCertGroup

instance HuddleGroup "committee_authorization_cert" DijkstraEra where
  huddleGroupNamed = committeeAuthorizationCertGroup

instance HuddleGroup "committee_resignation_cert" DijkstraEra where
  huddleGroupNamed = committeeResignationCertGroup

instance HuddleGroup "drep_registration_cert" DijkstraEra where
  huddleGroupNamed = drepRegistrationCertGroup

instance HuddleGroup "drep_unregistration_cert" DijkstraEra where
  huddleGroupNamed = drepUnregistrationCertGroup

instance HuddleGroup "drep_update_cert" DijkstraEra where
  huddleGroupNamed = drepUpdateCertGroup

instance HuddleRule "certificate" DijkstraEra where
  huddleRuleNamed = conwayCertificateRule

instance HuddleRule "certificates" DijkstraEra where
  huddleRuleNamed = certificatesRule

instance HuddleRule "voting_procedure" DijkstraEra where
  huddleRuleNamed = votingProcedureRule

instance HuddleRule "voting_procedures" DijkstraEra where
  huddleRuleNamed = votingProceduresRule

instance HuddleRule "constitution" DijkstraEra where
  huddleRuleNamed = constitutionRule

instance HuddleGroup "parameter_change_action" DijkstraEra where
  huddleGroupNamed = parameterChangeActionGroup

instance HuddleGroup "hard_fork_initiation_action" DijkstraEra where
  huddleGroupNamed = hardForkInitiationActionGroup

instance HuddleGroup "treasury_withdrawals_action" DijkstraEra where
  huddleGroupNamed = treasuryWithdrawalsActionGroup

instance HuddleGroup "no_confidence" DijkstraEra where
  huddleGroupNamed = noConfidenceGroup

instance HuddleGroup "update_committee" DijkstraEra where
  huddleGroupNamed = updateCommitteeGroup

instance HuddleGroup "new_constitution" DijkstraEra where
  huddleGroupNamed = newConstitutionGroup

instance HuddleRule "info_action" DijkstraEra where
  huddleRuleNamed pname _ = infoActionRule pname

instance HuddleRule "gov_action" DijkstraEra where
  huddleRuleNamed = govActionRule

instance HuddleRule "proposal_procedure" DijkstraEra where
  huddleRuleNamed = proposalProcedureRule

instance HuddleRule "proposal_procedures" DijkstraEra where
  huddleRuleNamed = proposalProceduresRule

instance HuddleRule "transaction_input" DijkstraEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ "transaction_id" ==> huddleRule @"transaction_id" p
        , "index" ==> VUInt `sized` (2 :: Word64)
        ]

instance HuddleRule "required_signers" DijkstraEra where
  huddleRuleNamed pname p =
    pname
      =.= huddleRule1 @"nonempty_set" p (huddleRule @"addr_keyhash" p)

instance HuddleRule "value" DijkstraEra where
  huddleRuleNamed = dijkstraValueRule

instance HuddleRule "mint" DijkstraEra where
  huddleRuleNamed = conwayMintRule

instance HuddleRule "withdrawals" DijkstraEra where
  huddleRuleNamed = conwayWithdrawalsRule

instance HuddleRule "data" DijkstraEra where
  huddleRuleNamed = dataRule

instance HuddleRule "datum_option" DijkstraEra where
  huddleRuleNamed = datumOptionRule

instance HuddleRule "script_ref" DijkstraEra where
  huddleRuleNamed = scriptRefRule

instance HuddleRule "alonzo_transaction_output" DijkstraEra where
  huddleRuleNamed = alonzoTransactionOutputRule

instance HuddleRule "babbage_transaction_output" DijkstraEra where
  huddleRuleNamed = babbageTransactionOutput

instance HuddleRule "transaction_output" DijkstraEra where
  huddleRuleNamed pname p =
    comment
      [str|Both of the Alonzo and Babbage style TxOut formats are equally valid
          |and can be used interchangeably
          |]
      $ pname
        =.= huddleRule @"alonzo_transaction_output" p
        / huddleRule @"babbage_transaction_output" p

instance HuddleRule "sub_transaction_body" DijkstraEra where
  huddleRuleNamed = subTransactionBodyRule

instance HuddleRule "sub_transaction" DijkstraEra where
  huddleRuleNamed = subTransactionRule

instance HuddleRule "sub_transactions" DijkstraEra where
  huddleRuleNamed = subTransactionsRule

instance HuddleRule "guards" DijkstraEra where
  huddleRuleNamed = guardsRule

instance HuddleRule "required_top_level_guards" DijkstraEra where
  huddleRuleNamed = requiredTopLevelGuardsRule

instance HuddleRule "potential_languages" DijkstraEra where
  huddleRuleNamed pname _ = potentialLanguagesRule pname

instance HuddleRule "redeemers" DijkstraEra where
  huddleRuleNamed pname p =
    comment
      [str|Dijkstra uses map format only for redeemers.
          |The flat array format has been removed.
          |]
      $ pname
        =.= mp
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

instance HuddleRule "script_data_hash" DijkstraEra where
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

instance HuddleRule "transaction" DijkstraEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a $ (bool True)
        , a (huddleRule @"auxiliary_data" p / VNil)
        ]
      / arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a (huddleRule @"auxiliary_data" p / VNil)
        ]

instance HuddleRule "ex_unit_prices" DijkstraEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ "mem_price" ==> huddleRule @"nonnegative_interval" p
        , "step_price" ==> huddleRule @"nonnegative_interval" p
        ]

instance HuddleRule "pool_voting_thresholds" DijkstraEra where
  huddleRuleNamed = poolVotingThresholdsRule

instance HuddleRule "drep_voting_thresholds" DijkstraEra where
  huddleRuleNamed = drepVotingThresholdsRule

instance HuddleRule "protocol_param_update" DijkstraEra where
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
        , opt (idx 33 ==> huddleRule @"nonnegative_interval" p) //- "minfee refScript coins per byte"
        , opt (idx 34 ==> VUInt `sized` (4 :: Word64)) //- "max refScript size per block"
        , opt (idx 35 ==> VUInt `sized` (4 :: Word64)) //- "max refScript size per tx"
        , opt (idx 36 ==> huddleRule @"positive_word32" p) //- "refScript cost stride"
        , opt (idx 37 ==> huddleRule @"positive_interval" p) //- "refScript cost multiplier"
        ]

instance HuddleRule "proposed_protocol_parameter_updates" DijkstraEra where
  huddleRuleNamed = proposedProtocolParameterUpdatesRule

instance HuddleRule "update" DijkstraEra where
  huddleRuleNamed = updateRule

instance HuddleRule "header_body" DijkstraEra where
  huddleRuleNamed = babbageHeaderBodyRule

instance HuddleRule "header" DijkstraEra where
  huddleRuleNamed = headerRule

instance HuddleRule "block" DijkstraEra where
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

instance HuddleRule "auxiliary_scripts" DijkstraEra where
  huddleRuleNamed = auxiliaryScriptsRule

instance HuddleRule "auxiliary_data_array" DijkstraEra where
  huddleRuleNamed = auxiliaryDataArrayRule

instance HuddleRule "transaction_body" DijkstraEra where
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
        , opt (idx 14 ==> huddleRule @"guards" p) //- "guards (replaces required_signers)"
        , opt (idx 15 ==> huddleRule @"network_id" p)
        , opt (idx 16 ==> huddleRule @"transaction_output" p) //- "collateral return"
        , opt (idx 17 ==> huddleRule @"coin" p) //- "total collateral"
        , opt (idx 18 ==> huddleRule1 @"nonempty_set" p (huddleRule @"transaction_input" p))
            //- "reference inputs"
        , opt (idx 19 ==> huddleRule @"voting_procedures" p)
        , opt (idx 20 ==> huddleRule @"proposal_procedures" p)
        , opt (idx 21 ==> huddleRule @"coin" p) //- "current treasury value"
        , opt (idx 22 ==> huddleRule @"positive_coin" p) //- "donation"
        , opt (idx 23 ==> huddleRule @"sub_transactions" p) //- "sub-transactions (NEW)"
        ]

instance HuddleRule "transaction_witness_set" DijkstraEra where
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
        -- TODO: Add plutus_v4_script at index 8 once AlonzoTxWitsRaw encoder/decoder supports it
        ]

instance HuddleRule "native_script" DijkstraEra where
  huddleRuleNamed = dijkstraNativeScriptRule

instance HuddleRule "script" DijkstraEra where
  huddleRuleNamed = dijkstraScriptRule

instance HuddleRule "redeemer_tag" DijkstraEra where
  huddleRuleNamed pname _ = dijkstraRedeemerTagRule pname

instance (Era era, HuddleRule "distinct_bytes" era) => HuddleRule "plutus_v4_script" era where
  huddleRuleNamed pname p =
    comment
      [str|Dijkstra introduces Plutus V4.
          |
          |Note: distinct VBytes ensures uniqueness in test generation.
          |]
      $ pname =.= huddleRule @"distinct_bytes" p

instance HuddleRule "auxiliary_data" DijkstraEra where
  huddleRuleNamed pname p =
    comment
      [str|auxiliary_data supports three serialization formats:
          |  1. metadata (raw) - Supported since Shelley
          |  2. auxiliary_data_array - Array format, introduced in Allegra
          |  3. auxiliary_data_map - Tagged map format, introduced in Alonzo
          |     Dijkstra adds plutus_v4_script support at index 5
          |]
      $ pname
        =.= huddleRule @"metadata" p
        / huddleRule @"auxiliary_data_array" p
        / huddleRule @"auxiliary_data_map" p

instance HuddleRule "auxiliary_data_map" DijkstraEra where
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
            , opt (idx 5 ==> arr [0 <+ a (huddleRule @"plutus_v4_script" p)])
            ]
        )

instance HuddleRule "language" DijkstraEra where
  huddleRuleNamed pname _ =
    comment
      [str|0: Plutus v1
          |1: Plutus v2
          |2: Plutus v3
          |3: Plutus v4 (NEW)
          |]
      $ pname =.= (0 :: Integer) ... (3 :: Integer)

instance HuddleRule "cost_models" DijkstraEra where
  huddleRuleNamed pname p =
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
      $ pname
        =.= mp
          [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 3 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , 0 <+ asKey ((4 :: Integer) ... (255 :: Integer)) ==> arr [0 <+ a (huddleRule @"int64" p)]
          ]

instance HuddleRule1 "set" DijkstraEra where
  huddleRule1Named pname _ = maybeTaggedSet pname

instance HuddleRule1 "nonempty_set" DijkstraEra where
  huddleRule1Named pname _ = maybeTaggedNonemptySet pname

instance HuddleRule1 "nonempty_oset" DijkstraEra where
  huddleRule1Named pname _ = maybeTaggedNonemptyOset pname

instance HuddleRule1 "multiasset" DijkstraEra where
  huddleRule1Named = dijkstraMultiasset
