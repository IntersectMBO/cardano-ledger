{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Translation where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era hiding (Crypto)
import Cardano.Ledger.Mary (MaryEra)
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.API

--------------------------------------------------------------------------------
-- Translation from Allegra to Mary
--
-- The instances below are needed by the consensus layer. Do not remove any of
-- them without coordinating with consensus.
--
-- Please add auxiliary instances and other declarations at the bottom of this
-- module, not in the list below so that it remains clear which instances the
-- consensus layer needs.
--
-- WARNING: when a translation instance currently uses the default
-- 'TranslationError', i.e., 'Void', it means the consensus layer relies on it
-- being total. Do not change it!
--------------------------------------------------------------------------------

type instance PreviousEra (MaryEra c) = AllegraEra c

-- | Currently no context is needed to translate from Allegra to Mary.
--
-- Note: if context is needed, please coordinate with consensus, who will have
-- to provide the context in the right place.
type instance TranslationContext (MaryEra c) = ()

instance Crypto c => TranslateEra (MaryEra c) NewEpochState where
  translateEra _ = error "TODO Allegra to Mary translation"

instance Crypto c => TranslateEra (MaryEra c) Tx where
  type TranslationError (MaryEra c) Tx = ()
  translateEra _ = error "TODO Allegra to Mary translation"

-- TODO when a genesis has been introduced for Mary, this instance can be
-- removed.
instance Crypto c => TranslateEra (MaryEra c) ShelleyGenesis where
  translateEra ctxt genesis =
    return
      ShelleyGenesis
        { sgSystemStart = sgSystemStart genesis,
          sgNetworkMagic = sgNetworkMagic genesis,
          sgNetworkId = sgNetworkId genesis,
          sgActiveSlotsCoeff = sgActiveSlotsCoeff genesis,
          sgSecurityParam = sgSecurityParam genesis,
          sgEpochLength = sgEpochLength genesis,
          sgSlotsPerKESPeriod = sgSlotsPerKESPeriod genesis,
          sgMaxKESEvolutions = sgMaxKESEvolutions genesis,
          sgSlotLength = sgSlotLength genesis,
          sgUpdateQuorum = sgUpdateQuorum genesis,
          sgMaxLovelaceSupply = sgMaxLovelaceSupply genesis,
          sgProtocolParams = translateEra' ctxt (sgProtocolParams genesis),
          sgGenDelegs = sgGenDelegs genesis,
          sgInitialFunds = Map.mapKeys (translateEra' ctxt) $ sgInitialFunds genesis,
          sgStaking = translateEra' ctxt $ sgStaking genesis
        }

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance Crypto c => TranslateEra (MaryEra c) (PParams' Identity) where
  translateEra _ = error "TODO Allegra to Mary translation"

instance Crypto c => TranslateEra (MaryEra c) RewardAcnt where
  translateEra _ = error "TODO Allegra to Mary translation"

instance Crypto c => TranslateEra (MaryEra c) PoolParams where
  translateEra ctxt poolParams =
    return $
      PoolParams
        { _poolPubKey = _poolPubKey poolParams,
          _poolVrf = _poolVrf poolParams,
          _poolPledge = _poolPledge poolParams,
          _poolCost = _poolCost poolParams,
          _poolMargin = _poolMargin poolParams,
          _poolRAcnt = translateEra' ctxt (_poolRAcnt poolParams),
          _poolOwners = _poolOwners poolParams,
          _poolRelays = _poolRelays poolParams,
          _poolMD = _poolMD poolParams
        }

instance Crypto c => TranslateEra (MaryEra c) ShelleyGenesisStaking where
  translateEra ctxt sgs =
    return $
      ShelleyGenesisStaking
        { sgsPools = Map.map (translateEra' ctxt) (sgsPools sgs),
          sgsStake = sgsStake sgs
        }

instance Crypto c => TranslateEra (MaryEra c) Addr where
  translateEra _ = error "TODO Allegra to Mary translation"
