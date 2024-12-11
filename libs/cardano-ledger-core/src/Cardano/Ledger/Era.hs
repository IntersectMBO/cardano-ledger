module Cardano.Ledger.Era (getAllTxInputs) where

import Cardano.Ledger.Address (Addr, BootstrapAddress, CompactAddr)
import Cardano.Ledger.Core
import Cardano.Ledger.TxIn (TxIn)
import Data.Set (Set)
import Lens.Micro

-- | The validity of any individual block depends only on a subset
-- of the UTxO stored in the ledger state. The consensus layer makes
-- use of this fact, and uses the function below to to retrieve the
-- needed UTxO from disk and present only those to the ledger.
-- It is therefore neccessary that this function account for all the
-- different types of inputs inside a transaction.
getAllTxInputs :: EraTxBody era => TxBody era -> Set (TxIn (EraCrypto era))
getAllTxInputs txBody = txBody ^. allInputsTxBodyF
