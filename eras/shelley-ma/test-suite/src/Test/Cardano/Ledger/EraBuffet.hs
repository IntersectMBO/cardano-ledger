-- | This module exports pieces one may use to make a new Era.
--   The pieces come in 3 flavors:
--   1) Crypto
--   2) Era's
--   3) Type families
--   Some of these pieces are pre-specialized to the Shelley,
--   Allegra, and Mary Eras. Some are trivial datatypes that one
--   may use as type instances for the type families when nothing
--   more specific is needed. One may make your own Era or just use
--   some of the pieces, which have been collected here for convenience.
--   To make a new Era, import this file, then write something like this:
--   data MyEra
--   instance Era MyEra where
--   type Crypto MyEra = TestCrypto
--   type instance Value MyEra = ConcreteValue.Value MyEra
--   type instance Script MyEra = TestScript
--   type instance TxBody MyEra = Mary.TxBody MyEra
module Test.Cardano.Ledger.EraBuffet
  ( TestCrypto, -- These are two crypto versions
    StandardCrypto,
    ShelleyEra, -- These are the crypto parameterized Eras re-exported for convenience.
    MaryEra, -- one needs to apply these to a crypto be be concrete
    AllegraEra,
    Value, -- These are the type families re-exported for convenience.
    Script,
    TxBody,
    AuxiliaryData,
    Era (..), -- The Era class re-exported
  )
where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Core (AuxiliaryData, Script, TxBody, Value)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (StandardCrypto, TestCrypto)

{------------------------------------------------------------------------------
 Example concrete Eras:

  type ShelleyTest = ShelleyEra TestCrypto

  type ShelleyStandard = ShelleyEra StandardCrypto

  type MaryTest = MaryEra TestCrypto

  type MaryStandard = MaryEra StandardCrypto

  type AllegraTest = AllegraEra TestCrypto

  type AllegraStandard = AllegraEra StandardCrypto
------------------------------------------------------------------------------}
