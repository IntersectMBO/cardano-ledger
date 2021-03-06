{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition of the shelley era, along with instances ot the @Core@ types
-- defined in @module Cardano.Ledger.Core@, and instances of the @API@ classes
-- exposed in @module Shelley.Spec.Ledger.API@.
module Cardano.Ledger.Shelley
  ( ShelleyEra,
    Era,
    proxy,
    TxOut,
    Value,
    TxBody,
    Script,
    AuxiliaryData,
    PParams,
    Tx,
  )
where

import Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import qualified Cardano.Ledger.Era as E (Era (Crypto))
import Cardano.Ledger.SafeHash (EraIndependentAuxiliaryData, makeHashWithExplicitProxys)
import Cardano.Ledger.Shelley.Constraints
  ( UsesPParams (..),
    UsesTxBody,
    UsesTxOut (..),
    UsesValue,
  )
import Data.Proxy
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Metadata (Metadata (Metadata), validMetadatum)
import Shelley.Spec.Ledger.PParams (PParamsUpdate, updatePParams)
import qualified Shelley.Spec.Ledger.PParams as SPP (PParams)
import Shelley.Spec.Ledger.Scripts (MultiSig)
import Shelley.Spec.Ledger.Tx
  ( ValidateScript (hashScript, validateScript),
    hashMultiSigScript,
    validateNativeMultiSigScript,
  )
import qualified Shelley.Spec.Ledger.Tx as STx (Tx, TxBody, TxOut (..))

data ShelleyEra c

instance CryptoClass.Crypto c => E.Era (ShelleyEra c) where
  type Crypto (ShelleyEra c) = c

instance CryptoClass.Crypto c => UsesValue (ShelleyEra c)

instance CryptoClass.Crypto c => UsesTxOut (ShelleyEra c) where
  makeTxOut _ a v = STx.TxOut a v

instance CryptoClass.Crypto c => UsesPParams (ShelleyEra c) where
  type PParamsDelta (ShelleyEra c) = PParamsUpdate (ShelleyEra c)
  mergePPUpdates _ = updatePParams

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Value (ShelleyEra _c) = Coin

type instance Core.TxBody (ShelleyEra c) = STx.TxBody (ShelleyEra c)

type instance Core.TxOut (ShelleyEra c) = STx.TxOut (ShelleyEra c)

type instance Core.Script (ShelleyEra c) = MultiSig c

type instance Core.AuxiliaryData (ShelleyEra c) = Metadata (ShelleyEra c)

type instance Core.PParams (ShelleyEra c) = SPP.PParams (ShelleyEra c)

type instance Core.Tx (ShelleyEra c) = STx.Tx (ShelleyEra c)

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

-- Self describing synonyms

type Value era = Coin

proxy :: Proxy (ShelleyEra c)
proxy = Proxy

type Script era = MultiSig (E.Crypto era)

type AuxiliaryData era = Metadata era

type Era c = ShelleyEra c

type Tx era = STx.Tx era

type TxOut era = STx.TxOut era

type TxBody era = STx.TxBody era

type PParams era = SPP.PParams era
