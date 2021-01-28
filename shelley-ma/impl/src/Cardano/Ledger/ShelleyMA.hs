{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.ShelleyMA where

import Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary.Value (Value)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.Constraints (UsesTxBody, UsesTxOut (..), UsesValue)
import Cardano.Ledger.ShelleyMA.AuxiliaryData (AuxiliaryData, pattern AuxiliaryData)
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
import Shelley.Spec.Ledger.Metadata (validMetadatum)
import Shelley.Spec.Ledger.Tx
  ( TxOut (..),
    ValidateScript (..),
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

type family MAValue (x :: MaryOrAllegra) c :: Type where
  MAValue 'Allegra _ = Coin
  MAValue 'Mary c = Value c

instance CryptoClass.Crypto c => UsesValue (ShelleyMAEra 'Mary c)

instance CryptoClass.Crypto c => UsesValue (ShelleyMAEra 'Allegra c)

instance CryptoClass.Crypto c => UsesTxOut (ShelleyMAEra 'Mary c) where
  makeTxOut _ a v = TxOut a v

instance CryptoClass.Crypto c => UsesTxOut (ShelleyMAEra 'Allegra c) where
  makeTxOut _ a v = TxOut a v

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Value (ShelleyMAEra m c) = MAValue m c

type instance
  Core.TxOut (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    TxOut (ShelleyMAEra ma c)

type instance
  Core.TxBody (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    TxBody (ShelleyMAEra ma c)

type instance
  Core.Script (ShelleyMAEra (_ma :: MaryOrAllegra) c) =
    Timelock c

type instance
  Core.AuxiliaryData (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    AuxiliaryData (ShelleyMAEra (ma :: MaryOrAllegra) c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

instance
  ( CryptoClass.Crypto c,
    UsesTxBody (ShelleyMAEra ma c),
    Core.AnnotatedData (Core.AuxiliaryData (ShelleyMAEra ma c)),
    (HasField "vldt" (Core.TxBody (ShelleyMAEra ma c)) ValidityInterval)
  ) =>
  ValidateScript (ShelleyMAEra ma c)
  where
  validateScript s tx = validateTimelock s tx
  hashScript s = hashTimelockScript s

instance
  ( CryptoClass.Crypto c,
    Core.AnnotatedData (Core.Script (ShelleyMAEra ma c))
  ) =>
  ValidateAuxiliaryData (ShelleyMAEra (ma :: MaryOrAllegra) c)
  where
  validateAuxiliaryData (AuxiliaryData md as) = deepseq as $ all validMetadatum md
  hashAuxiliaryData aux = AuxiliaryDataHash (hashAnnotated aux)
