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
    MaryEra,
    AllegraEra,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (CompactForm, Compactible)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), policies, policyID)
import Cardano.Ledger.ProtVer ()
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Rules
  ( ShelleyBBODY,
    ShelleyEPOCH,
    ShelleyMIR,
    ShelleyNEWPP,
    ShelleyRUPD,
    ShelleySNAP,
    ShelleyTICKF,
    ShelleyUPEC,
  )
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative, EncodeMint, Val, zero)
import Control.DeepSeq (NFData (..))
import Data.Default (def)
import Data.Kind (Type)
import Data.Set as Set (Set, empty, map)
import Data.Typeable (Typeable)
import GHC.TypeLits
import Lens.Micro (lens)
import NoThunks.Class (NoThunks)

type MaryEra = ShelleyMAEra 'Mary

type AllegraEra = ShelleyMAEra 'Allegra

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
    CC.Crypto c,
    DecodeNonNegative (MAValue ma c),
    Compactible (MAValue ma c),
    Eq (CompactForm (MAValue ma c)),
    NFData (MAValue ma c),
    Show (MAValue ma c),
    Val (MAValue ma c),
    Eq (MAValue ma c),
    FromCBOR (MAValue ma c),
    ToCBOR (MAValue ma c),
    EncodeMint (MAValue ma c),
    DecodeMint (MAValue ma c),
    NoThunks (MAValue ma c)
  ) =>
  MAClass (ma :: MaryOrAllegra) c
  where
  type MAValue (ma :: MaryOrAllegra) c :: Type
  getScriptHash :: proxy ma -> MultiAsset c -> Set.Set (ScriptHash c)
  promoteMultiAsset :: proxy ma -> MultiAsset c -> Value (ShelleyMAEra ma c)

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
instance MAClass ma c => Era (ShelleyMAEra ma c) where
  type EraCrypto (ShelleyMAEra ma c) = c
  type ProtVerLow (ShelleyMAEra ma c) = MAProtVer ma

type family MAProtVer (ma :: MaryOrAllegra) :: Nat where
  MAProtVer 'Allegra = 3
  MAProtVer 'Mary = 4

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Value (ShelleyMAEra ma c) = MAValue ma c

instance (MAClass ma c, ProtVerAtMost (ShelleyMAEra ma c) 6) => EraPParams (ShelleyMAEra ma c) where
  type PParamsHKD f (ShelleyMAEra ma c) = ShelleyPParamsHKD f (ShelleyMAEra ma c)

  emptyPParams = def
  emptyPParamsUpdate = def

  hkdMinFeeAL = lens _minfeeA $ \pp x -> pp {_minfeeA = x}
  hkdMinFeeBL = lens _minfeeB $ \pp x -> pp {_minfeeB = x}
  hkdMaxBBSizeL = lens _maxBBSize $ \pp x -> pp {_maxBBSize = x}
  hkdMaxTxSizeL = lens _maxTxSize $ \pp x -> pp {_maxTxSize = x}
  hkdMaxBHSizeL = lens _maxBHSize $ \pp x -> pp {_maxBHSize = x}
  hkdKeyDepositL = lens _keyDeposit $ \pp x -> pp {_keyDeposit = x}
  hkdPoolDepositL = lens _poolDeposit $ \pp x -> pp {_poolDeposit = x}
  hkdEMaxL = lens _eMax $ \pp x -> pp {_eMax = x}
  hkdNOptL = lens _nOpt $ \pp x -> pp {_nOpt = x}
  hkdA0L = lens _a0 $ \pp x -> pp {_a0 = x}
  hkdRhoL = lens _rho $ \pp x -> pp {_rho = x}
  hkdTauL = lens _tau $ \pp x -> pp {_tau = x}
  hkdDL = lens _d $ \pp x -> pp {_d = x}
  hkdExtraEntropyL = lens _extraEntropy $ \pp x -> pp {_extraEntropy = x}
  hkdProtocolVersionL = lens _protocolVersion $ \pp x -> pp {_protocolVersion = x}
  hkdMinUTxOValueL = lens _minUTxOValue $ \pp x -> pp {_minUTxOValue = x}
  hkdMinPoolCostL = lens _minPoolCost $ \pp x -> pp {_minPoolCost = x}

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
