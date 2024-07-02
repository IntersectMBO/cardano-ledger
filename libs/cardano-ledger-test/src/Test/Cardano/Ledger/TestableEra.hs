{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Defines the requirements on an era to be testable
module Test.Cardano.Ledger.TestableEra where

import Cardano.Ledger.Shelley.API
import Cardano.Protocol.TPraos.API

class
  ( ApplyBlock "LEDGERS" era
  , ApplyTx era
  , GetLedgerView era
  ) =>
  TestableEra era