{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.ShelleyMA.Era
  ( ShelleyMAEra,
    MAClass (..),
    MaryOrAllegra (..),
    ShelleyMAUTXO,
    ShelleyMAUTXOW,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (CompactForm, Compactible)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), policies, policyID)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.PParams (ShelleyPParams, ShelleyPParamsUpdate, updatePParams)
import Cardano.Ledger.Shelley.Rules.Bbody (ShelleyBBODY)
import Cardano.Ledger.Shelley.Rules.Epoch (ShelleyEPOCH)
import Cardano.Ledger.Shelley.Rules.Mir (ShelleyMIR)
import Cardano.Ledger.Shelley.Rules.Newpp (ShelleyNEWPP)
import Cardano.Ledger.Shelley.Rules.Rupd (ShelleyRUPD)
import Cardano.Ledger.Shelley.Rules.Snap (ShelleySNAP)
import Cardano.Ledger.Shelley.Rules.Tick (ShelleyTICKF)
import Cardano.Ledger.Shelley.Rules.Upec (ShelleyUPEC)
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative, EncodeMint, Val, zero)
import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Data.Set as Set (Set, empty, map)
import Data.Typeable (Typeable)
import GHC.TypeLits
import NoThunks.Class (NoThunks)

-- | The Shelley Mary/Allegra eras
--   The uninhabited type that indexes both the Mary and Allegra Eras.
data ShelleyMAEra (ma :: MaryOrAllegra) c

-- | Both eras are implemented within the same codebase, matching the formal
-- specification. They differ only in the @value@ type. Due to some annoying
-- issues with 'Coin' and 'Value' being of different kinds, we don't parametrise
-- over the value but instead over a closed kind 'MaryOrAllegra'. But this
-- should be transparent to the user.
data MaryOrAllegra = Mary | Allegra

-- | The MAClass provides a method and a type, which implement the differences
--   between the Mary and Allegra instances
class
  ( Typeable ma,
    CC.Crypto crypto,
    DecodeNonNegative (MAValue ma crypto),
    Compactible (MAValue ma crypto),
    Eq (CompactForm (MAValue ma crypto)),
    NFData (MAValue ma crypto),
    Show (MAValue ma crypto),
    Val (MAValue ma crypto),
    Eq (MAValue ma crypto),
    FromCBOR (MAValue ma crypto),
    ToCBOR (MAValue ma crypto),
    EncodeMint (MAValue ma crypto),
    DecodeMint (MAValue ma crypto),
    NoThunks (MAValue ma crypto)
  ) =>
  MAClass (ma :: MaryOrAllegra) crypto
  where
  type MAValue (ma :: MaryOrAllegra) crypto :: Type
  getScriptHash :: proxy ma -> MultiAsset crypto -> Set.Set (ScriptHash crypto)
  promoteMultiAsset :: proxy ma -> MultiAsset crypto -> Value (ShelleyMAEra ma crypto)

instance CC.Crypto c => MAClass 'Mary c where
  type MAValue 'Mary c = MaryValue c
  getScriptHash _ x = Set.map policyID (policies x)
  promoteMultiAsset _ ma = MaryValue 0 ma

instance CC.Crypto c => MAClass 'Allegra c where
  type MAValue 'Allegra c = Coin
  getScriptHash _ _ = Set.empty
  promoteMultiAsset _ _ = zero

-- | The actual Mary and Allegra instances, rolled into one, the MAClass superclass
--   provides the era-specific code for where they differ.
instance MAClass ma crypto => Era (ShelleyMAEra ma crypto) where
  type Crypto (ShelleyMAEra ma crypto) = crypto
  type ProtVerLow (ShelleyMAEra ma crypto) = MAProtVer ma

type family MAProtVer (ma :: MaryOrAllegra) :: Nat where
  MAProtVer 'Mary = 3
  MAProtVer 'Allegra = 4

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Value (ShelleyMAEra ma c) = MAValue ma c

instance MAClass ma crypto => EraPParams (ShelleyMAEra ma crypto) where
  type PParams (ShelleyMAEra ma crypto) = ShelleyPParams (ShelleyMAEra ma crypto)
  type PParamsUpdate (ShelleyMAEra ma crypto) = ShelleyPParamsUpdate (ShelleyMAEra ma crypto)

  applyPPUpdates = updatePParams

-- These rules are all inherited from Shelley

type instance EraRule "BBODY" (ShelleyMAEra ma c) = ShelleyBBODY (ShelleyMAEra ma c)

type instance EraRule "DELEG" (ShelleyMAEra ma c) = API.ShelleyDELEG (ShelleyMAEra ma c)

type instance EraRule "DELEGS" (ShelleyMAEra ma c) = API.ShelleyDELEGS (ShelleyMAEra ma c)

type instance EraRule "DELPL" (ShelleyMAEra ma c) = API.ShelleyDELPL (ShelleyMAEra ma c)

type instance EraRule "EPOCH" (ShelleyMAEra ma c) = ShelleyEPOCH (ShelleyMAEra ma c)

type instance EraRule "LEDGER" (ShelleyMAEra ma c) = API.ShelleyLEDGER (ShelleyMAEra ma c)

type instance EraRule "LEDGERS" (ShelleyMAEra ma c) = API.ShelleyLEDGERS (ShelleyMAEra ma c)

type instance EraRule "MIR" (ShelleyMAEra ma c) = ShelleyMIR (ShelleyMAEra ma c)

type instance EraRule "NEWEPOCH" (ShelleyMAEra ma c) = API.ShelleyNEWEPOCH (ShelleyMAEra ma c)

type instance EraRule "NEWPP" (ShelleyMAEra ma c) = ShelleyNEWPP (ShelleyMAEra ma c)

type instance EraRule "POOL" (ShelleyMAEra ma c) = API.ShelleyPOOL (ShelleyMAEra ma c)

type instance EraRule "POOLREAP" (ShelleyMAEra ma c) = API.ShelleyPOOLREAP (ShelleyMAEra ma c)

type instance EraRule "PPUP" (ShelleyMAEra ma c) = API.ShelleyPPUP (ShelleyMAEra ma c)

type instance EraRule "RUPD" (ShelleyMAEra ma c) = ShelleyRUPD (ShelleyMAEra ma c)

type instance EraRule "SNAP" (ShelleyMAEra ma c) = ShelleySNAP (ShelleyMAEra ma c)

type instance EraRule "TICK" (ShelleyMAEra ma c) = API.ShelleyTICK (ShelleyMAEra ma c)

type instance EraRule "TICKF" (ShelleyMAEra ma c) = ShelleyTICKF (ShelleyMAEra ma c)

type instance EraRule "UPEC" (ShelleyMAEra ma c) = ShelleyUPEC (ShelleyMAEra ma c)

-- These rules are defined anew in the ShelleyMA era(s)

data ShelleyMAUTXO era

type instance EraRule "UTXO" (ShelleyMAEra ma c) = ShelleyMAUTXO (ShelleyMAEra ma c)

data ShelleyMAUTXOW era

type instance EraRule "UTXOW" (ShelleyMAEra ma c) = ShelleyMAUTXOW (ShelleyMAEra ma c)
