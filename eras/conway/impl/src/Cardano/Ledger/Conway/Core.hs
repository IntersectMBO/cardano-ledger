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

module Cardano.Ledger.Conway.Core
  ( ConwayEraTxBody (..),
  )
where

import Cardano.Ledger.Babbage.Core (BabbageEraTxBody, Era (..), EraTxBody (..))
import Cardano.Ledger.Conway.Delegation.Certificates (ConwayDCert)
import Cardano.Ledger.Conway.Governance (GovernanceActionInfo, Vote)
import Data.Sequence.Strict (StrictSeq)
import Lens.Micro (Lens')

class BabbageEraTxBody era => ConwayEraTxBody era where
  govActionsTxBodyL :: Lens' (TxBody era) (StrictSeq (GovernanceActionInfo era))
  votesTxBodyL :: Lens' (TxBody era) (StrictSeq (Vote era))
  conwayCertsTxBodyL :: Lens' (TxBody era) (StrictSeq (ConwayDCert (EraCrypto era)))
