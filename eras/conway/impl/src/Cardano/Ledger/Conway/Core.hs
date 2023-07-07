{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Conway.Core (
  module X,
  ConwayEraTxBody (..),
)
where

import Cardano.Ledger.Babbage.Core as X
import Cardano.Ledger.Conway.Governance (ProposalProcedure, VotingProcedure)
import Data.Sequence.Strict (StrictSeq)
import Lens.Micro (Lens')

class BabbageEraTxBody era => ConwayEraTxBody era where

  -- | Lens for getting and setting `VotingProcedure`s.
  votingProceduresTxBodyL ::
    Lens' (TxBody era) (StrictSeq (VotingProcedure era))

  -- | Lens for getting and setting `ProposalProcedure`s.
  proposalProceduresTxBodyL ::
    Lens' (TxBody era) (StrictSeq (ProposalProcedure era))
