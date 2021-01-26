{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This is a stub example of a Shelley era, designed for testing,
-- prototyping, and demo purposes.
module Cardano.Ledger.ExampleShelley where

import Cardano.Binary (toCBOR)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Shelley.Constraints (UsesTxBody, UsesTxOut (..), UsesValue)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Keys (hashWithSerialiser)
import Shelley.Spec.Ledger.Metadata (Metadata (Metadata), validMetadatum)
import Shelley.Spec.Ledger.Scripts (MultiSig)
import Shelley.Spec.Ledger.Tx
  ( TxBody,
    ValidateScript (hashScript, validateScript),
    hashMultiSigScript,
    validateNativeMultiSigScript,
  )
import Shelley.Spec.Ledger.API
  ( ApplyBlock,
    ApplyTx,
    GetLedgerView,
    PraosCrypto,
    ShelleyBasedEra,
    TxOut,
  )

data ExampleShelleyEra c

instance CryptoClass.Crypto c => Era (ExampleShelleyEra c) where
  type Crypto (ExampleShelleyEra c) = c

instance CryptoClass.Crypto c => UsesValue (ExampleShelleyEra c)

instance CryptoClass.Crypto c => UsesTxOut (ExampleShelleyEra c) where
  makeTxOut _ a v = TxOut a v

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Value (ExampleShelleyEra _c) = Coin

type instance Core.TxBody (ExampleShelleyEra c) = TxBody (ExampleShelleyEra c)

type instance Core.TxOut (ExampleShelleyEra c) = TxOut (ExampleShelleyEra c)

type instance Core.Script (ExampleShelleyEra c) = MultiSig c

type instance Core.AuxiliaryData (ExampleShelleyEra c) = Metadata

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

instance
  (CryptoClass.Crypto c) =>
  ValidateScript (ExampleShelleyEra c)
  where
  validateScript = validateNativeMultiSigScript
  hashScript = hashMultiSigScript

instance CryptoClass.Crypto c => ValidateAuxiliaryData (ExampleShelleyEra c) where
  hashAuxiliaryData = AuxiliaryDataHash . Hash.castHash . hashWithSerialiser @(HASH c) toCBOR
  validateAuxiliaryData (Metadata m) = all validMetadatum m

instance PraosCrypto c => ApplyTx (ExampleShelleyEra c)
instance PraosCrypto c => ApplyBlock (ExampleShelleyEra c)
instance PraosCrypto c => GetLedgerView (ExampleShelleyEra c)
instance PraosCrypto c => ShelleyBasedEra (ExampleShelleyEra c)
