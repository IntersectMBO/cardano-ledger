module Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators (
  genCoherentBlock,
  maxTxWits,
) where

import Test.Cardano.Ledger.Shelley.Arbitrary (maxTxWits)
import Test.Cardano.Protocol.TPraos.Arbitrary (genCoherentBlock)
