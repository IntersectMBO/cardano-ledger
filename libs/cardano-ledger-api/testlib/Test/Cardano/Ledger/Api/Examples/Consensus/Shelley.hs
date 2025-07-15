{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Shelley (
  -- * re-exports from Shelley and TPraos Examples
  LedgerExamples (..),
  defaultLedgerExamples,
  exampleLedgerBlock,
  exampleBlockHeader,
  exampleHashHeader,
  exampleTx,
  examplePoolDistr,
  exampleNonMyopicRewards,
  exampleNewEpochState,
  exampleLedgerChainDepState,
  exampleCoin,
  exampleTxBodyShelley,
  exampleAuxDataMap,
  exampleTxIns,
  exampleCerts,
  exampleWithdrawals,
  exampleProposedPPUpdates,
  examplePayKey,
  exampleStakeKey,
  exampleKeys,
  exampleAuxiliaryDataShelley,
  examplePoolParams,
  exampleVrfVerKeyHash,
  ledgerExamples,
  testShelleyGenesis,
  keyToCredential,
  mkKeyHash,
  mkWitnessesPreAlonzo,
  mkScriptHash,
  seedFromByte,
  seedFromWords,
  emptyFromByronTranslationContext,

  -- * Shelley examples
  ledgerExamplesShelley,
) where

import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.API (ChainDepState)
import Cardano.Protocol.TPraos.BHeader (BHeader, HashHeader)
import Test.Cardano.Ledger.Shelley.Examples
import Test.Cardano.Protocol.TPraos.Examples (
  exampleBlockHeader,
  exampleHashHeader,
  exampleKeys,
  exampleLedgerChainDepState,
 )

ledgerExamplesShelley ::
  LedgerExamples (BHeader StandardCrypto) HashHeader ChainDepState ShelleyEra
ledgerExamplesShelley =
  ledgerExamples
    (exampleBlockHeader [exampleTxShelley])
    exampleHashHeader
    (exampleLedgerChainDepState 1)
