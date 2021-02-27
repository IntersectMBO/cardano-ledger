{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition of the shelley era, along with instances ot the @Core@ types
-- defined in @module Cardano.Ledger.Core@, and instances of the @API@ classes
-- exposed in @module Shelley.Spec.Ledger.API@.
module Cardano.Ledger.Shelley where

import Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.CoreUtxow (CoreUtxow (..), ValidateScript (..))
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.SafeHash (EraIndependentAuxiliaryData, makeHashWithExplicitProxys)
import Cardano.Ledger.Shelley.Constraints
  ( UsesPParams (..),
    UsesTxBody,
    UsesTxOut (..),
    UsesValue,
  )
import Data.Proxy
import Data.Set as Set (empty)
import GHC.Records (HasField (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.CompactAddr (decompactAddr)
import Shelley.Spec.Ledger.Metadata (Metadata (Metadata), validMetadatum)
import Shelley.Spec.Ledger.PParams (PParams, PParamsUpdate, updatePParams)
import Shelley.Spec.Ledger.Scripts (MultiSig)
import Shelley.Spec.Ledger.Tx
  ( Tx (Tx'),
    TxBody,
    TxOut (..),
    WitnessSet,
    WitnessSetHKD (..),
    hashMultiSigScript,
    validateNativeMultiSigScript,
  )

data ShelleyEra c

instance CryptoClass.Crypto c => Era (ShelleyEra c) where
  type Crypto (ShelleyEra c) = c

instance CryptoClass.Crypto c => UsesValue (ShelleyEra c)

instance CryptoClass.Crypto c => UsesTxOut (ShelleyEra c) where
  makeTxOut _ a v = TxOut a v

instance CryptoClass.Crypto c => UsesPParams (ShelleyEra c) where
  type PParamsDelta (ShelleyEra c) = PParamsUpdate (ShelleyEra c)
  mergePPUpdates _ = updatePParams

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Value (ShelleyEra _c) = Coin

type instance Core.TxBody (ShelleyEra c) = TxBody (ShelleyEra c)

type instance Core.TxOut (ShelleyEra c) = TxOut (ShelleyEra c)

type instance Core.Script (ShelleyEra c) = MultiSig c

type instance Core.AuxiliaryData (ShelleyEra c) = Metadata

type instance Core.PParams (ShelleyEra c) = PParams (ShelleyEra c)

type instance Core.Tx (ShelleyEra c) = Tx (ShelleyEra c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

instance
  (CryptoClass.Crypto c, UsesTxBody (ShelleyEra c)) =>
  ValidateScript (ShelleyEra c)
  where
  validateScript = validateNativeMultiSigScript
  hashScript = hashMultiSigScript

instance CryptoClass.Crypto c => ValidateAuxiliaryData (ShelleyEra c) where
  validateAuxiliaryData (Metadata m) = all validMetadatum m
  hashAuxiliaryData metadata = AuxiliaryDataHash (makeHashWithExplicitProxys (Proxy @c) index metadata)
    where
      index = Proxy @EraIndependentAuxiliaryData

instance
  CryptoClass.Crypto c =>
  CoreUtxow (ShelleyEra c) Tx TxBody WitnessSet TxOut
  where
  bodyTx (Tx' body _wit _meta _) = body
  witTx (Tx' _body wit _meta _) = wit
  metaTx (Tx' _body _wit meta _) = meta
  addrWit x = addrWits' x
  bootWit x = bootWits' x
  scriptWit x = scriptWits' x
  updateBody x = getField @"update" x
  wdrlsBody x = getField @"wdrls" x
  certsBody x = getField @"certs" x
  inputsBody x = getField @"inputs" x
  mintBody _ = Set.empty
  adHashBody x = getField @"adHash" x
  addressOut (TxOutCompact ca _) = decompactAddr ca
