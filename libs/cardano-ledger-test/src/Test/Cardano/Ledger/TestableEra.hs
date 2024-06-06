{-# LANGUAGE UndecidableSuperClasses #-}

-- | Defines the requirements on an era to be testable
module Test.Cardano.Ledger.TestableEra where

import qualified Cardano.Ledger.Conway.API.Validation as Conway
import Cardano.Ledger.Shelley.API
import Cardano.Protocol.TPraos.API

class
  ( ApplyBlock era
  , ApplyTx era
  , GetLedgerView era
  ) =>
  TestableEra era

class
  ( Conway.ApplyBlock era
  , ApplyTx era
  , GetLedgerView era
  ) =>
  TestableConwayEra era
