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
import Shelley.Spec.Ledger.STS.Ledger (LEDGER)
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS)
import Shelley.Spec.Ledger.STS.Ppup (PPUP)
import Shelley.Spec.Ledger.STS.Bbody (BBODY)
import Shelley.Spec.Ledger.STS.Deleg (DELEG)
import Shelley.Spec.Ledger.STS.Delegs (DELEGS)
import Shelley.Spec.Ledger.STS.Delpl (DELPL)
import Shelley.Spec.Ledger.STS.Epoch (EPOCH)
import Shelley.Spec.Ledger.STS.NewEpoch (NEWEPOCH)
import Shelley.Spec.Ledger.STS.Mir (MIR)
import Shelley.Spec.Ledger.STS.Newpp (NEWPP)
import Shelley.Spec.Ledger.STS.Ocert (OCERT)
import Shelley.Spec.Ledger.STS.Overlay (OVERLAY)
import Shelley.Spec.Ledger.STS.Pool (POOL)
import Shelley.Spec.Ledger.STS.PoolReap (POOLREAP)
import Shelley.Spec.Ledger.STS.Rupd (RUPD)
import Shelley.Spec.Ledger.STS.Snap (SNAP)
import Shelley.Spec.Ledger.STS.Tick (TICK, TICKF)
import Shelley.Spec.Ledger.STS.Tickn (TICKN)
import Shelley.Spec.Ledger.STS.Upec (UPEC)

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
type instance Core.EraRule "BBODY" (ExampleShelleyEra c) = BBODY (ExampleShelleyEra c)
type instance Core.EraRule "DELEG" (ExampleShelleyEra c) = DELEG (ExampleShelleyEra c)
type instance Core.EraRule "DELEGS" (ExampleShelleyEra c) = DELEGS (ExampleShelleyEra c)
type instance Core.EraRule "DELPL" (ExampleShelleyEra c) = DELPL (ExampleShelleyEra c)
type instance Core.EraRule "EPOCH" (ExampleShelleyEra c) = EPOCH (ExampleShelleyEra c)
type instance Core.EraRule "LEDGER" (ExampleShelleyEra c) = LEDGER (ExampleShelleyEra c)
type instance Core.EraRule "LEDGERS" (ExampleShelleyEra c) = LEDGERS (ExampleShelleyEra c)
type instance Core.EraRule "MIR" (ExampleShelleyEra c) = MIR (ExampleShelleyEra c)
type instance Core.EraRule "NEWEPOCH" (ExampleShelleyEra c) = NEWEPOCH (ExampleShelleyEra c)
type instance Core.EraRule "NEWPP" (ExampleShelleyEra c) = NEWPP (ExampleShelleyEra c)
type instance Core.EraRule "OCERT" (ExampleShelleyEra c) = OCERT (ExampleShelleyEra c)
type instance Core.EraRule "OVERLAY" (ExampleShelleyEra c) = OVERLAY (ExampleShelleyEra c)
type instance Core.EraRule "POOL" (ExampleShelleyEra c) = POOL (ExampleShelleyEra c)
type instance Core.EraRule "POOLREAP" (ExampleShelleyEra c) = POOLREAP (ExampleShelleyEra c)
type instance Core.EraRule "PPUP" (ExampleShelleyEra c) = PPUP (ExampleShelleyEra c)
type instance Core.EraRule "RUPD" (ExampleShelleyEra c) = RUPD (ExampleShelleyEra c)
type instance Core.EraRule "SNAP" (ExampleShelleyEra c) = SNAP (ExampleShelleyEra c)
type instance Core.EraRule "TICK" (ExampleShelleyEra c) = TICK (ExampleShelleyEra c)
type instance Core.EraRule "TICKF" (ExampleShelleyEra c) = TICKF (ExampleShelleyEra c)
type instance Core.EraRule "TICKN" (ExampleShelleyEra _c) = TICKN
type instance Core.EraRule "UPEC" (ExampleShelleyEra c) = UPEC (ExampleShelleyEra c)
type instance Core.EraRule "UTXO" (ExampleShelleyEra c) = UTXO (ExampleShelleyEra c)
type instance Core.EraRule "UTXOW" (ExampleShelleyEra c) = UTXOW (ExampleShelleyEra c)
