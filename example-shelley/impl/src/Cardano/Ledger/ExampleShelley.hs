{-# LANGUAGE DataKinds #-}
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
import Cardano.Ledger.Shelley.Constraints (UsesTxOut (..), UsesValue)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Keys (hashWithSerialiser)
import Shelley.Spec.Ledger.Metadata (Metadata (Metadata), validMetadatum)
import Shelley.Spec.Ledger.Scripts (MultiSig)
import Shelley.Spec.Ledger.Tx
  ( ValidateScript (hashScript, validateScript),
    hashMultiSigScript,
    validateNativeMultiSigScript,
  )
import Shelley.Spec.Ledger.API
  ( ApplyBlock,
    ApplyTx,
    GetLedgerView,
    PraosCrypto,
    ShelleyBasedEra,
    TxOut (..)
  )

import Shelley.Spec.Ledger.TxBody (TxBody (..))

import Shelley.Spec.Ledger.STS.Utxo (UTXO)
import Shelley.Spec.Ledger.STS.Utxow (UTXOW)
import qualified Shelley.Spec.Ledger.API as Shelley
import qualified Shelley.Spec.Ledger.STS.Bbody as Shelley
import qualified Shelley.Spec.Ledger.STS.Epoch as Shelley
import qualified Shelley.Spec.Ledger.STS.Mir as Shelley
import qualified Shelley.Spec.Ledger.STS.Newpp as Shelley
import qualified Shelley.Spec.Ledger.STS.Ocert as Shelley
import qualified Shelley.Spec.Ledger.STS.Overlay as Shelley
import qualified Shelley.Spec.Ledger.STS.Rupd as Shelley
import qualified Shelley.Spec.Ledger.STS.Snap as Shelley
import qualified Shelley.Spec.Ledger.STS.Tick as Shelley
import qualified Shelley.Spec.Ledger.STS.Upec as Shelley

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

-- These rules are all inherited from Shelley

type instance Core.EraRule "BBODY" (ExampleShelleyEra c) = Shelley.BBODY (ExampleShelleyEra c)
type instance Core.EraRule "DELEG" (ExampleShelleyEra c) = Shelley.DELEG (ExampleShelleyEra c)
type instance Core.EraRule "DELEGS" (ExampleShelleyEra c) = Shelley.DELEGS (ExampleShelleyEra c)
type instance Core.EraRule "DELPL" (ExampleShelleyEra c) = Shelley.DELPL (ExampleShelleyEra c)
type instance Core.EraRule "EPOCH" (ExampleShelleyEra c) = Shelley.EPOCH (ExampleShelleyEra c)
type instance Core.EraRule "LEDGER" (ExampleShelleyEra c) = Shelley.LEDGER (ExampleShelleyEra c)
type instance Core.EraRule "LEDGERS" (ExampleShelleyEra c) = Shelley.LEDGERS (ExampleShelleyEra c)
type instance Core.EraRule "MIR" (ExampleShelleyEra c) = Shelley.MIR (ExampleShelleyEra c)
type instance Core.EraRule "NEWEPOCH" (ExampleShelleyEra c) = Shelley.NEWEPOCH (ExampleShelleyEra c)
type instance Core.EraRule "NEWPP" (ExampleShelleyEra c) = Shelley.NEWPP (ExampleShelleyEra c)
type instance Core.EraRule "OCERT" (ExampleShelleyEra c) = Shelley.OCERT (ExampleShelleyEra c)
type instance Core.EraRule "OVERLAY" (ExampleShelleyEra c) = Shelley.OVERLAY (ExampleShelleyEra c)
type instance Core.EraRule "POOL" (ExampleShelleyEra c) = Shelley.POOL (ExampleShelleyEra c)
type instance Core.EraRule "POOLREAP" (ExampleShelleyEra c) = Shelley.POOLREAP (ExampleShelleyEra c)
type instance Core.EraRule "PPUP" (ExampleShelleyEra c) = Shelley.PPUP (ExampleShelleyEra c)
type instance Core.EraRule "RUPD" (ExampleShelleyEra c) = Shelley.RUPD (ExampleShelleyEra c)
type instance Core.EraRule "SNAP" (ExampleShelleyEra c) = Shelley.SNAP (ExampleShelleyEra c)
type instance Core.EraRule "TICK" (ExampleShelleyEra c) = Shelley.TICK (ExampleShelleyEra c)
type instance Core.EraRule "TICKF" (ExampleShelleyEra c) = Shelley.TICKF (ExampleShelleyEra c)
type instance Core.EraRule "TICKN" (ExampleShelleyEra _c) = Shelley.TICKN
type instance Core.EraRule "UPEC" (ExampleShelleyEra c) = Shelley.UPEC (ExampleShelleyEra c)

-- These rules are defined anew in the ExampleShelley era, copied from ShelleyMA
type instance Core.EraRule "UTXO" (ExampleShelleyEra c) = UTXO (ExampleShelleyEra c)
type instance Core.EraRule "UTXOW" (ExampleShelleyEra c) = UTXOW (ExampleShelleyEra c)
