{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.ShelleyMA where

import Cardano.Binary (toCBOR)
import Cardano.Crypto.Hash (hashWithSerialiser)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary.Value (Value)
import Cardano.Ledger.Shelley.Constraints (TxBodyConstraints)
import Cardano.Ledger.ShelleyMA.Metadata (Metadata, pattern Metadata)
import Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (..),
    ValidityInterval,
    hashTimelockScript,
    validateTimelock,
  )
import Cardano.Ledger.ShelleyMA.TxBody (TxBody)
import Control.DeepSeq (deepseq)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Records (HasField)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Metadata
  ( MetadataHash (..),
    ValidateMetadata (..),
    validMetadatum,
  )
import Shelley.Spec.Ledger.Tx
  ( ValidateScript (..),
  )

-- | The Shelley Mary/Allegra eras
--
-- Both eras are implemented within the same codebase, matching the formal
-- specification. They differ only in the @value@ type. Due to some annoying
-- issues with 'Coin' and 'Value' being of different kinds, we don't parametrise
-- over the value but instead over a closed kind 'MaryOrAllegra'. But this
-- should be transparent to the user.
data ShelleyMAEra (ma :: MaryOrAllegra) c

data MaryOrAllegra = Mary | Allegra

instance
  forall c (ma :: MaryOrAllegra).
  (Typeable ma, CryptoClass.Crypto c) =>
  Era (ShelleyMAEra ma c)
  where
  type Crypto (ShelleyMAEra ma c) = c

type family MAValue (x :: MaryOrAllegra) era :: Type where
  MAValue 'Allegra _ = Coin
  MAValue 'Mary era = Value era

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Value (ShelleyMAEra m c) = MAValue m (ShelleyMAEra m c)

type instance
  Core.TxBody (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    TxBody (ShelleyMAEra ma c)

type instance
  Core.Script (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    Timelock (ShelleyMAEra ma c)

type instance
  Core.Metadata (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    Metadata (ShelleyMAEra (ma :: MaryOrAllegra) c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

instance
  ( CryptoClass.Crypto c,
    Typeable ma,
    TxBodyConstraints (ShelleyMAEra ma c),
    Core.AnnotatedData (Core.Metadata (ShelleyMAEra ma c)),
    (HasField "vldt" (Core.TxBody (ShelleyMAEra ma c)) ValidityInterval)
  ) =>
  ValidateScript (ShelleyMAEra ma c)
  where
  validateScript s tx = validateTimelock s tx
  hashScript s = hashTimelockScript s

instance
  ( CryptoClass.Crypto c,
    Typeable ma,
    Core.AnnotatedData (Core.Script (ShelleyMAEra ma c))
  ) =>
  ValidateMetadata (ShelleyMAEra (ma :: MaryOrAllegra) c)
  where
  hashMetadata = MetadataHash . hashWithSerialiser toCBOR

  validateMetadata (Metadata blob sp) = deepseq sp $ all validMetadatum blob
