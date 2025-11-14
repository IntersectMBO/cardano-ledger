{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.CDDL (
  allegraCDDL,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.CDDL
import Cardano.Ledger.Core.CDDL ()
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.CDDL (
  mkHeader,
  mkHeaderBody,
  mkProposedProtocolParameterUpdates,
  mkProtocolParamUpdate,
  mkTransactionWitnessSet,
  mkUpdate,
  untagged_set,
 )
import Codec.CBOR.Cuddle.Huddle
import Text.Heredoc
import Prelude hiding ((/))

allegraCDDL :: Huddle
allegraCDDL =
  collectFrom
    [ huddleItem @"block" @AllegraEra
    , huddleItem @"transaction" @AllegraEra
    ]

instance HasCDDL "major_protocol_version" AllegraEra where
  huddleItem = HIRule $ "major_protocol_version" =:= (0 :: Integer) ... (4 :: Integer)

instance HasCDDL "protocol_version" AllegraEra where
  huddleItem =
    HIGroup $ "protocol_version" =:~ grp [a (huddleRule @"major_protocol_version" @AllegraEra), a VUInt]

instance HasCDDL "protocol_param_update" AllegraEra where
  huddleItem = mkProtocolParamUpdate @AllegraEra

instance HasCDDL "proposed_protocol_parameter_updates" AllegraEra where
  huddleItem = mkProposedProtocolParameterUpdates @AllegraEra

instance HasCDDL "update" AllegraEra where
  huddleItem = mkUpdate @AllegraEra

instance HasCDDL "header_body" AllegraEra where
  huddleItem = mkHeaderBody @AllegraEra @ShelleyEra

instance HasCDDL "header" AllegraEra where
  huddleItem = mkHeader @AllegraEra

instance HasCDDL "min_int64" AllegraEra where
  huddleItem = HIRule $ "min_int64" =:= (-9223372036854775808 :: Integer)

instance HasCDDL "max_int64" AllegraEra where
  huddleItem = HIRule $ "max_int64" =:= (9223372036854775807 :: Integer)

instance HasCDDL "int64" AllegraEra where
  huddleItem = HIRule $ "int64" =:= huddleRule @"min_int64" @AllegraEra ... huddleRule @"max_int64" @AllegraEra

instance HasCDDL "script_all" AllegraEra where
  huddleItem = HIGroup $ "script_all" =:~ grp [1, a $ arr [0 <+ a (huddleRule @"native_script" @AllegraEra)]]

instance HasCDDL "script_any" AllegraEra where
  huddleItem = HIGroup $ "script_any" =:~ grp [2, a $ arr [0 <+ a (huddleRule @"native_script" @AllegraEra)]]

instance HasCDDL "script_n_of_k" AllegraEra where
  huddleItem =
    HIGroup $
      "script_n_of_k"
        =:~ grp
          [ 3
          , "n" ==> huddleRule @"int64" @AllegraEra
          , a $ arr [0 <+ a (huddleRule @"native_script" @AllegraEra)]
          ]

instance HasCDDL "script_invalid_before" AllegraEra where
  huddleItem =
    HIGroup
      $ comment
        [str|Timelock validity intervals are half-open intervals [a, b).
            |This field specifies the left (included) endpoint a.
            |]
      $ "script_invalid_before"
        =:~ grp [4, a (huddleRule @"slot" @AllegraEra)]

instance HasCDDL "script_invalid_hereafter" AllegraEra where
  huddleItem =
    HIGroup
      $ comment
        [str|Timelock validity intervals are half-open intervals [a, b).
            |This field specifies the right (excluded) endpoint b.
            |]
      $ "script_invalid_hereafter"
        =:~ grp [5, a (huddleRule @"slot" @AllegraEra)]

instance HasCDDL "native_script" AllegraEra where
  huddleItem =
    HIRule
      $ comment
        [str|Allegra introduces timelock support for native scripts.
            |
            |Timelock validity intervals are half-open intervals [a, b).
            |  script_invalid_before: specifies the left (included) endpoint a.
            |  script_invalid_hereafter: specifies the right (excluded) endpoint b.
            |
            |Note: Allegra switched to int64 for script_n_of_k thresholds.
            |]
      $ "native_script"
        =:= arr [a $ huddleGroup @"script_pubkey" @ShelleyEra]
        / arr [a $ huddleGroup @"script_all" @AllegraEra]
        / arr [a $ huddleGroup @"script_any" @AllegraEra]
        / arr [a $ huddleGroup @"script_n_of_k" @AllegraEra]
        / arr [a $ huddleGroup @"script_invalid_before" @AllegraEra]
        / arr [a $ huddleGroup @"script_invalid_hereafter" @AllegraEra]

instance HasCDDL "transaction_witness_set" AllegraEra where
  huddleItem = mkTransactionWitnessSet @AllegraEra

instance HasCDDL "auxiliary_scripts" AllegraEra where
  huddleItem = HIRule $ "auxiliary_scripts" =:= arr [0 <+ a (huddleRule @"native_script" @AllegraEra)]

instance HasCDDL "auxiliary_data_array" AllegraEra where
  huddleItem =
    HIRule $
      "auxiliary_data_array"
        =:= arr
          [ "transaction_metadata" ==> huddleRule @"metadata" @ShelleyEra
          , "auxiliary_scripts" ==> huddleRule @"auxiliary_scripts" @AllegraEra
          ]

instance HasCDDL "auxiliary_data" AllegraEra where
  huddleItem =
    HIRule $
      "auxiliary_data"
        =:= huddleRule @"metadata" @ShelleyEra
        / huddleRule @"auxiliary_data_array" @AllegraEra

instance HasCDDL "transaction_body" AllegraEra where
  huddleItem =
    HIRule
      $ comment
        [str|Allegra transaction body adds the validity interval start at index 8
            |]
      $ "transaction_body"
        =:= mp
          [ idx 0 ==> untagged_set (huddleRule @"transaction_input" @ShelleyEra)
          , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" @ShelleyEra)]
          , idx 2 ==> huddleRule @"coin" @AllegraEra
          , opt (idx 3 ==> huddleRule @"slot" @AllegraEra)
          , opt (idx 4 ==> arr [0 <+ a (huddleRule @"certificate" @ShelleyEra)])
          , opt (idx 5 ==> huddleRule @"withdrawals" @ShelleyEra)
          , opt (idx 6 ==> huddleRule @"update" @AllegraEra)
          , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" @AllegraEra)
          , opt (idx 8 ==> huddleRule @"slot" @AllegraEra)
          ]

instance HasCDDL "transaction" AllegraEra where
  huddleItem =
    HIRule $
      "transaction"
        =:= arr
          [ a $ huddleRule @"transaction_body" @AllegraEra
          , a $ huddleRule @"transaction_witness_set" @AllegraEra
          , a (huddleRule @"auxiliary_data" @AllegraEra / VNil)
          ]

instance HasCDDL "block" AllegraEra where
  huddleItem =
    HIRule $
      "block"
        =:= arr
          [ a $ huddleRule @"header" @AllegraEra
          , "transaction_bodies" ==> arr [0 <+ a (huddleRule @"transaction_body" @AllegraEra)]
          , "transaction_witness_sets" ==> arr [0 <+ a (huddleRule @"transaction_witness_set" @AllegraEra)]
          , "auxiliary_data_set"
              ==> mp
                [ 0
                    <+ asKey (huddleRule @"transaction_index" @ShelleyEra)
                    ==> huddleRule @"auxiliary_data" @AllegraEra
                ]
          ]
