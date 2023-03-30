module Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators (
  genCoherentBlock,
) where

import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Protocol.TPraos.Arbitrary (genCoherentBlock)
