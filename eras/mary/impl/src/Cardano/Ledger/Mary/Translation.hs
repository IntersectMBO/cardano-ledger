{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Translation where

import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Scripts (Timelock, translateTimelock)
import Cardano.Ledger.Mary.State
import Cardano.Ledger.Mary.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..), Update (..))
import Cardano.Ledger.Shelley.Tx (ShelleyTx)
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut)
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map

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

instance TranslateEra MaryEra NewEpochState where
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes
        , nesBprev = nesBprev nes
        , nesBcur = nesBcur nes
        , nesEs = translateEra' ctxt $ nesEs nes
        , nesRu = nesRu nes
        , nesPd = nesPd nes
        , stashedAVVMAddresses = ()
        }

instance TranslateEra MaryEra ShelleyTx where
  type TranslationError MaryEra ShelleyTx = DecoderError
  translateEra _ctx = translateEraThroughCBOR "ShelleyTx"

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance TranslateEra MaryEra PParams

instance TranslateEra MaryEra PParamsUpdate

instance TranslateEra MaryEra FuturePParams where
  translateEra ctxt = \case
    NoPParamsUpdate -> pure NoPParamsUpdate
    DefinitePParamsUpdate pp -> DefinitePParamsUpdate <$> translateEra ctxt pp
    PotentialPParamsUpdate mpp -> PotentialPParamsUpdate <$> mapM (translateEra ctxt) mpp

instance TranslateEra MaryEra EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esChainAccountState = esChainAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esNonMyopic = esNonMyopic es
        }

instance TranslateEra MaryEra DState where
  translateEra _ DState {..} = pure DState {..}

instance TranslateEra MaryEra CommitteeState where
  translateEra _ CommitteeState {..} = pure CommitteeState {..}

instance TranslateEra MaryEra PState where
  translateEra _ PState {..} = pure PState {..}

instance TranslateEra MaryEra ShelleyCertState where
  translateEra ctxt ls =
    pure
      ShelleyCertState
        { shelleyCertDState = translateEra' ctxt $ shelleyCertDState ls
        , shelleyCertPState = translateEra' ctxt $ shelleyCertPState ls
        }

instance TranslateEra MaryEra LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { lsUTxOState = translateEra' ctxt $ lsUTxOState ls
        , lsCertState = translateEra' ctxt $ lsCertState ls
        }

instance TranslateEra MaryEra ProposedPPUpdates where
  translateEra ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ Map.map (translateEra' ctxt) ppup

instance TranslateEra MaryEra ShelleyGovState where
  translateEra ctxt ps =
    return
      ShelleyGovState
        { sgsCurProposals = translateEra' ctxt $ sgsCurProposals ps
        , sgsFutureProposals = translateEra' ctxt $ sgsFutureProposals ps
        , sgsCurPParams = translateEra' ctxt $ sgsCurPParams ps
        , sgsPrevPParams = translateEra' ctxt $ sgsPrevPParams ps
        , sgsFuturePParams = translateEra' ctxt $ sgsFuturePParams ps
        }

instance TranslateEra MaryEra UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { utxosUtxo = translateEra' ctxt $ utxosUtxo us
        , utxosDeposited = utxosDeposited us
        , utxosFees = utxosFees us
        , utxosGovState = translateEra' ctxt $ utxosGovState us
        , utxosInstantStake = translateEra' ctxt $ utxosInstantStake us
        , utxosDonation = utxosDonation us
        }

instance TranslateEra MaryEra ShelleyInstantStake where
  translateEra _ = pure . coerce

instance TranslateEra MaryEra ShelleyTxOut where
  translateEra NoGenesis = pure . upgradeTxOut

instance TranslateEra MaryEra UTxO where
  translateEra ctxt utxo =
    return $ UTxO (translateEra' ctxt `Map.map` unUTxO utxo)

instance TranslateEra MaryEra ShelleyTxWits where
  type TranslationError MaryEra ShelleyTxWits = DecoderError
  translateEra _ctx = translateEraThroughCBOR "ShelleyTxWits"

instance TranslateEra MaryEra Update where
  translateEra _ (Update pp en) = pure $ Update (coerce pp) en

instance TranslateEra MaryEra Timelock where
  translateEra _ = pure . translateTimelock

instance TranslateEra MaryEra AllegraTxAuxData where
  translateEra ctx (AllegraTxAuxData md as) =
    pure $ AllegraTxAuxData md $ translateEra' ctx <$> as
