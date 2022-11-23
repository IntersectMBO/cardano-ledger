{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Translation where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Shelley.API hiding (Metadata, TxBody)
import Cardano.Ledger.ShelleyMA.AuxiliaryData
  ( AllegraTxAuxData (..),
  )
import Cardano.Ledger.ShelleyMA.Era (MaryEra)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock, translateTimelock)
import qualified Cardano.Ledger.Val as Val
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

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
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = nesBprev nes,
          nesBcur = nesBcur nes,
          nesEs = translateEra' ctxt $ nesEs nes,
          nesRu = nesRu nes,
          nesPd = nesPd nes,
          stashedAVVMAddresses = ()
        }

instance Crypto c => TranslateEra (MaryEra c) ShelleyTx where
  type TranslationError (MaryEra c) ShelleyTx = DecoderError
  translateEra _ctx = translateEraThroughCBOR "ShelleyTx"

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
          sgInitialFunds = sgInitialFunds genesis,
          sgStaking = sgStaking genesis
        }

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance (Crypto c, Functor f) => TranslateEra (MaryEra c) (ShelleyPParamsHKD f)

instance Crypto c => TranslateEra (MaryEra c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translateEra' ctxt $ esPrevPp es,
          esPp = translateEra' ctxt $ esPp es,
          esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (MaryEra c) LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { lsUTxOState = translateEra' ctxt $ lsUTxOState ls,
          lsDPState = lsDPState ls
        }

instance Crypto c => TranslateEra (MaryEra c) ProposedPPUpdates where
  translateEra ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ Map.map (translateEra' ctxt) ppup

instance Crypto c => TranslateEra (MaryEra c) PPUPState where
  translateEra ctxt ps =
    return
      PPUPState
        { proposals = translateEra' ctxt $ proposals ps,
          futureProposals = translateEra' ctxt $ futureProposals ps
        }

instance Crypto c => TranslateEra (MaryEra c) UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { utxosUtxo = translateEra' ctxt $ utxosUtxo us,
          utxosDeposited = utxosDeposited us,
          utxosFees = utxosFees us,
          utxosPpups = translateEra' ctxt $ utxosPpups us,
          utxosStakeDistr = utxosStakeDistr us
        }

instance Crypto c => TranslateEra (MaryEra c) ShelleyTxOut where
  translateEra () (TxOutCompact addr cfval) =
    pure $ TxOutCompact (coerce addr) (translateCompactValue cfval)

instance Crypto c => TranslateEra (MaryEra c) UTxO where
  translateEra ctxt utxo =
    return $ UTxO (translateEra' ctxt `Map.map` unUTxO utxo)

instance Crypto c => TranslateEra (MaryEra c) ShelleyTxWits where
  type TranslationError (MaryEra c) ShelleyTxWits = DecoderError
  translateEra _ctx = translateEraThroughCBOR "ShelleyTxWits"

instance Crypto c => TranslateEra (MaryEra c) Update where
  translateEra _ (Update pp en) = pure $ Update (coerce pp) en

instance Crypto c => TranslateEra (MaryEra c) Timelock where
  translateEra _ = pure . translateTimelock

instance Crypto c => TranslateEra (MaryEra c) AllegraTxAuxData where
  translateEra ctx (AllegraTxAuxData md as) =
    pure $ AllegraTxAuxData md $ translateEra' ctx <$> as

translateValue :: Crypto c => Coin -> MaryValue c
translateValue = Val.inject

translateCompactValue :: Crypto c => CompactForm Coin -> CompactForm (MaryValue c)
translateCompactValue =
  fromMaybe (error msg) . toCompact . translateValue . fromCompact
  where
    msg = "impossible error: compact coin is out of range"
