-- | This module provides a library interface for working with types that will allow a user to
-- interact with Cardano ledger.
--
-- It is intended to be the complete API covering everything but without exposing constructors that
-- reveal any lower level types.
--
-- In the interest of simplicity it glosses over some details of the system.
-- Most tools should be able to work just using this interface, however you can go deeper and
-- experiment with internal modules if necessary, such as "Cardano.Ledger.Core",
-- "Cardano.Ledger.Shelley", "Cardano.Ledger.Babbage", etc.
module Cardano.Ledger.Api (
  -- | Definition of Cardano Ledger era types.
  module Cardano.Ledger.Api.Era,
  -- | Building and inspecting transactions.
  module Cardano.Ledger.Api.Tx,
  -- | Protocol parameters.
  module Cardano.Ledger.Api.PParams,
  -- | Scripts
  module Cardano.Ledger.Api.Scripts,
  -- | Governance
  module Cardano.Ledger.Api.Governance,
)
where

import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Governance
import Cardano.Ledger.Api.PParams
import Cardano.Ledger.Api.Scripts
import Cardano.Ledger.Api.Tx
