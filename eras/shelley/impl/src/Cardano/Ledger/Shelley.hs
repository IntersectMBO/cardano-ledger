{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition of the shelley era, along with instances ot the @Core@ types
-- defined in @module Cardano.Ledger.Core@, and instances of the @API@ classes
-- exposed in @module Cardano.Ledger.Shelley.API@.
module Cardano.Ledger.Shelley
  ( ShelleyEra,
    Self,
    TxOut,
    TxBody,
    Value,
    Script,
    AuxiliaryData,
    PParams,
    Core.PParamsDelta,
    Tx,
    Witnesses,
    nativeMultiSigTag,
  )
where

import Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (SupportsSegWit (..), ValidateScript (..))
import qualified Cardano.Ledger.Era as E (Era (Crypto), TranslationContext)
import Cardano.Ledger.Hashes (EraIndependentAuxiliaryData)
import Cardano.Ledger.SafeHash (makeHashWithExplicitProxys)
import Cardano.Ledger.Shelley.BlockChain (bbHash)
import qualified Cardano.Ledger.Shelley.BlockChain as Shelley
  ( TxSeq (..),
    txSeqTxns,
  )
import Cardano.Ledger.Shelley.Constraints
  ( UsesPParams (..),
    UsesTxBody,
    UsesTxOut (..),
    UsesValue,
  )
import Cardano.Ledger.Shelley.Metadata (Metadata (Metadata), validMetadatum)
import Cardano.Ledger.Shelley.PParams (PParams, PParamsUpdate, updatePParams)
import Cardano.Ledger.Shelley.Scripts (MultiSig)
import Cardano.Ledger.Shelley.Tx
  ( WitnessSet,
    validateNativeMultiSigScript,
  )
import qualified Cardano.Ledger.Shelley.Tx as STx (Tx, TxBody, TxOut (..))
import qualified Data.ByteString as BS
import Data.Proxy

data ShelleyEra c

instance CryptoClass.Crypto c => E.Era (ShelleyEra c) where
  type Crypto (ShelleyEra c) = c

instance CryptoClass.Crypto c => UsesValue (ShelleyEra c)

instance CryptoClass.Crypto c => UsesTxOut (ShelleyEra c) where
  makeTxOut _ a v = STx.TxOut a v

instance CryptoClass.Crypto c => UsesPParams (ShelleyEra c) where
  mergePPUpdates _ = updatePParams

type instance E.TranslationContext (ShelleyEra c) = ()

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Tx (ShelleyEra c) = STx.Tx (ShelleyEra c)

type instance Core.Value (ShelleyEra _c) = Coin

type instance Core.TxBody (ShelleyEra c) = STx.TxBody (ShelleyEra c)

type instance Core.TxOut (ShelleyEra c) = STx.TxOut (ShelleyEra c)

type instance Core.Script (ShelleyEra c) = MultiSig c

type instance Core.AuxiliaryData (ShelleyEra c) = Metadata (ShelleyEra c)

type instance Core.PParams (ShelleyEra c) = PParams (ShelleyEra c)

type instance Core.Witnesses (ShelleyEra c) = WitnessSet (ShelleyEra c)

type instance Core.PParamsDelta (ShelleyEra c) = PParamsUpdate (ShelleyEra c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

-- | Magic number "memorialized" in the ValidateScript class under the method:
--   scriptPrefixTag:: Core.Script era -> Bs.ByteString, for the Shelley Era.
nativeMultiSigTag :: BS.ByteString
nativeMultiSigTag = "\00"

instance
  (CryptoClass.Crypto c, UsesTxBody (ShelleyEra c)) =>
  ValidateScript (ShelleyEra c)
  where
  scriptPrefixTag _script = nativeMultiSigTag

  -- In the ShelleyEra there is only one kind of Script and its tag is "\x00"
  validateScript = validateNativeMultiSigScript

instance CryptoClass.Crypto c => SupportsSegWit (ShelleyEra c) where
  type TxSeq (ShelleyEra c) = Shelley.TxSeq (ShelleyEra c)
  fromTxSeq = Shelley.txSeqTxns
  toTxSeq = Shelley.TxSeq
  hashTxSeq = bbHash
  numSegComponents = 3

instance CryptoClass.Crypto c => ValidateAuxiliaryData (ShelleyEra c) c where
  validateAuxiliaryData (Metadata m) = all validMetadatum m
  hashAuxiliaryData metadata = AuxiliaryDataHash (makeHashWithExplicitProxys (Proxy @c) index metadata)
    where
      index = Proxy @EraIndependentAuxiliaryData

-- Self describing synonyms

type Value era = Coin

type Script era = MultiSig (E.Crypto era)

type AuxiliaryData era = Metadata era

type Self c = ShelleyEra c

type Tx era = STx.Tx era

type TxOut era = STx.TxOut era

type TxBody era = STx.TxBody era

type Witnesses era = WitnessSet (E.Crypto era)
