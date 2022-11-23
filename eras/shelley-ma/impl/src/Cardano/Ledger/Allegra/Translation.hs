{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Translation where

import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API
import qualified Cardano.Ledger.Shelley.LedgerState as LS
  ( returnRedeemAddrsToReserves,
  )
import Cardano.Ledger.ShelleyMA ()
import Cardano.Ledger.ShelleyMA.Era (AllegraEra)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Translation from Shelley to Allegra
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

-- | Return the subset of UTxO corresponding to Byron-era AVVM addresses, which
-- are to be removed on the Shelley/Allegra boundary. This set will be passed
-- _back_ to the translation functions as the UTxO, allowing these addresses to
-- be removed. This is needed because we cannot do a full scan on the UTxO at
-- this point, since it has been persisted to disk.
shelleyToAllegraAVVMsToDelete :: NewEpochState (ShelleyEra c) -> UTxO (ShelleyEra c)
shelleyToAllegraAVVMsToDelete = stashedAVVMAddresses

type instance PreviousEra (AllegraEra c) = ShelleyEra c

-- | Currently no context is needed to translate from Shelley to Allegra.

-- Note: if context is needed, please coordinate with consensus, who will have
-- to provide the context in the right place.
type instance TranslationContext (AllegraEra c) = ()

instance Crypto c => TranslateEra (AllegraEra c) NewEpochState where
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = nesBprev nes,
          nesBcur = nesBcur nes,
          nesEs = translateEra' ctxt $ LS.returnRedeemAddrsToReserves . nesEs $ nes,
          nesRu = nesRu nes,
          nesPd = nesPd nes,
          -- At this point, the consensus layer has passed in our stashed AVVM
          -- addresses as our UTxO, and we have deleted them above (with
          -- 'returnRedeemAddrsToReserves'), so we may safely discard this map.
          stashedAVVMAddresses = ()
        }

instance forall c. Crypto c => TranslateEra (AllegraEra c) ShelleyTx where
  type TranslationError (AllegraEra c) ShelleyTx = DecoderError
  translateEra _ctx = translateEraThroughCBOR "ShelleyTx"

instance Crypto c => TranslateEra (AllegraEra c) ShelleyGenesis where
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

instance Crypto c => TranslateEra (AllegraEra c) (ShelleyPParamsHKD f) where
  translateEra _ pp =
    return $
      ShelleyPParams
        { _minfeeA = _minfeeA pp,
          _minfeeB = _minfeeB pp,
          _maxBBSize = _maxBBSize pp,
          _maxTxSize = _maxTxSize pp,
          _maxBHSize = _maxBHSize pp,
          _keyDeposit = _keyDeposit pp,
          _poolDeposit = _poolDeposit pp,
          _eMax = _eMax pp,
          _nOpt = _nOpt pp,
          _a0 = _a0 pp,
          _rho = _rho pp,
          _tau = _tau pp,
          _d = _d pp,
          _extraEntropy = _extraEntropy pp,
          _protocolVersion = _protocolVersion pp,
          _minUTxOValue = _minUTxOValue pp,
          _minPoolCost = _minPoolCost pp
        }

instance Crypto c => TranslateEra (AllegraEra c) ProposedPPUpdates where
  translateEra ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ Map.map (translateEra' ctxt) ppup

instance Crypto c => TranslateEra (AllegraEra c) PPUPState where
  translateEra ctxt ps =
    return
      PPUPState
        { proposals = translateEra' ctxt $ proposals ps,
          futureProposals = translateEra' ctxt $ futureProposals ps
        }

instance Crypto c => TranslateEra (AllegraEra c) ShelleyTxOut where
  translateEra () (TxOutCompact addr cfval) =
    pure $ TxOutCompact (coerce addr) cfval

instance Crypto c => TranslateEra (AllegraEra c) UTxO where
  translateEra ctxt utxo =
    return $ UTxO (translateEra' ctxt `Map.map` unUTxO utxo)

instance Crypto c => TranslateEra (AllegraEra c) UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { utxosUtxo = translateEra' ctxt $ utxosUtxo us,
          utxosDeposited = utxosDeposited us,
          utxosFees = utxosFees us,
          utxosPpups = translateEra' ctxt $ utxosPpups us,
          utxosStakeDistr = utxosStakeDistr us
        }

instance Crypto c => TranslateEra (AllegraEra c) LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { lsUTxOState = translateEra' ctxt $ lsUTxOState ls,
          lsDPState = lsDPState ls
        }

instance Crypto c => TranslateEra (AllegraEra c) EpochState where
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

instance Crypto c => TranslateEra (AllegraEra c) ShelleyTxWits where
  type TranslationError (AllegraEra c) ShelleyTxWits = DecoderError
  translateEra _ctx = translateEraThroughCBOR "ShelleyTxWits"

instance Crypto c => TranslateEra (AllegraEra c) Update where
  translateEra _ (Update pp en) = pure $ Update (coerce pp) en
