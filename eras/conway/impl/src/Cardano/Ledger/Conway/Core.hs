{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Core (
  module X,
  ConwayEraTxBody (..),
)
where

import Cardano.Ledger.Babbage.Core as X
import Cardano.Ledger.Conway.Delegation.Certificates (ConwayDCert)
import Cardano.Ledger.Conway.Rules.Tally (GovernanceProcedure)
import Data.Sequence.Strict (StrictSeq)
import Lens.Micro (Lens')

class BabbageEraTxBody era => ConwayEraTxBody era where
  govProcsTxBodyL :: Lens' (TxBody era) (StrictSeq (GovernanceProcedure era))
  conwayCertsTxBodyL :: Lens' (TxBody era) (StrictSeq (ConwayDCert (EraCrypto era)))
