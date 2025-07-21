{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Api.Examples (
  ProtocolLedgerExamples (..),
  protocolLedgerExamples,
  -- re-export
  LedgerExamples (..),
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State (EraGov, InstantStake)
import Control.State.Transition
import Data.Functor.Identity (Identity)
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
 )

data ProtocolLedgerExamples bh chd era = ProtocolLedgerExamples
  { pleHashHeader :: HashHeader
  , pleBlockHeader :: bh
  , pleChainDepState :: chd
  , pleLedgerExamples :: LedgerExamples era
  , pleBlock :: Block bh era
  }

deriving instance
  ( EraTx era
  , Eq h
  , Eq chd
  , Eq (PParamsHKD Identity era)
  , Eq (PParamsHKD StrictMaybe era)
  , EraGov era
  , Eq (BlockBody era)
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Eq (StashedAVVMAddresses era)
  , Eq (TranslationContext era)
  , Eq (CertState era)
  , Eq (InstantStake era)
  ) =>
  Eq (ProtocolLedgerExamples h chd era)

protocolLedgerExamples ::
  EraBlockBody era =>
  HashHeader -> ([Tx era] -> bh) -> chd -> LedgerExamples era -> ProtocolLedgerExamples bh chd era
protocolLedgerExamples hashHeader toBlockHeader chainDepState ledgerExamples =
  ProtocolLedgerExamples hashHeader blockHeader chainDepState ledgerExamples block
  where
    blockHeader = toBlockHeader txs
    block = Block blockHeader $ blockBody
    txs = [leTx ledgerExamples]
    blockBody = mkBasicBlockBody & txSeqBlockBodyL .~ StrictSeq.fromList txs
