{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Translation (shelleyToAllegraAVVMsToDelete) where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Tx ()
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.CertState (CommitteeState (..))
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  UTxOState (..),
  VState (..),
  returnRedeemAddrsToReserves,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..), Update (..))
import Cardano.Ledger.Shelley.Tx (ShelleyTx)
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut)
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits)
import Cardano.Ledger.State (UTxO (..))
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
shelleyToAllegraAVVMsToDelete :: NewEpochState ShelleyEra -> UTxO ShelleyEra
shelleyToAllegraAVVMsToDelete = stashedAVVMAddresses

instance TranslateEra AllegraEra NewEpochState where
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes
        , nesBprev = nesBprev nes
        , nesBcur = nesBcur nes
        , nesEs = translateEra' ctxt $ returnRedeemAddrsToReserves $ nesEs nes
        , nesRu = nesRu nes
        , nesPd = nesPd nes
        , -- At this point, the consensus layer has passed in our stashed AVVM
          -- addresses as our UTxO, and we have deleted them above (with
          -- 'returnRedeemAddrsToReserves'), so we may safely discard this map.
          stashedAVVMAddresses = ()
        }

instance TranslateEra AllegraEra ShelleyTx where
  type TranslationError AllegraEra ShelleyTx = DecoderError
  translateEra _ctx = translateEraThroughCBOR "ShelleyTx"

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance TranslateEra AllegraEra PParams

instance TranslateEra AllegraEra PParamsUpdate

instance TranslateEra AllegraEra FuturePParams where
  translateEra ctxt = \case
    NoPParamsUpdate -> pure NoPParamsUpdate
    DefinitePParamsUpdate pp -> DefinitePParamsUpdate <$> translateEra ctxt pp
    PotentialPParamsUpdate mpp -> PotentialPParamsUpdate <$> mapM (translateEra ctxt) mpp

instance TranslateEra AllegraEra ProposedPPUpdates where
  translateEra ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ Map.map (translateEra' ctxt) ppup

instance TranslateEra AllegraEra ShelleyGovState where
  translateEra ctxt ps =
    return
      ShelleyGovState
        { sgsCurProposals = translateEra' ctxt $ sgsCurProposals ps
        , sgsFutureProposals = translateEra' ctxt $ sgsFutureProposals ps
        , sgsCurPParams = translateEra' ctxt $ sgsCurPParams ps
        , sgsPrevPParams = translateEra' ctxt $ sgsPrevPParams ps
        , sgsFuturePParams = translateEra' ctxt $ sgsFuturePParams ps
        }

instance TranslateEra AllegraEra ShelleyTxOut where
  translateEra NoGenesis = pure . upgradeTxOut

instance TranslateEra AllegraEra UTxO where
  translateEra ctxt utxo =
    return $ UTxO (translateEra' ctxt `Map.map` unUTxO utxo)

instance TranslateEra AllegraEra UTxOState where
  translateEra ctxt us =
    return
      UTxOState
        { utxosUtxo = translateEra' ctxt $ utxosUtxo us
        , utxosDeposited = utxosDeposited us
        , utxosFees = utxosFees us
        , utxosGovState = translateEra' ctxt $ utxosGovState us
        , utxosStakeDistr = utxosStakeDistr us
        , utxosDonation = utxosDonation us
        }

instance TranslateEra AllegraEra DState where
  translateEra _ DState {..} = pure DState {..}

instance TranslateEra AllegraEra CommitteeState where
  translateEra _ CommitteeState {..} = pure CommitteeState {..}

instance TranslateEra AllegraEra VState where
  translateEra ctx VState {..} = do
    committeeState <- translateEra ctx vsCommitteeState
    pure VState {vsCommitteeState = committeeState, ..}

instance TranslateEra AllegraEra PState where
  translateEra _ PState {..} = pure PState {..}

instance TranslateEra AllegraEra CertState where
  translateEra ctxt ls =
    pure
      CertState
        { certDState = translateEra' ctxt $ certDState ls
        , certPState = translateEra' ctxt $ certPState ls
        , certVState = translateEra' ctxt $ certVState ls
        }

instance TranslateEra AllegraEra LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { lsUTxOState = translateEra' ctxt $ lsUTxOState ls
        , lsCertState = translateEra' ctxt $ lsCertState ls
        }

instance TranslateEra AllegraEra EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esNonMyopic = esNonMyopic es
        }

instance TranslateEra AllegraEra ShelleyTxWits where
  type TranslationError AllegraEra ShelleyTxWits = DecoderError
  translateEra _ctx = translateEraThroughCBOR "ShelleyTxWits"

instance TranslateEra AllegraEra Update where
  translateEra _ (Update pp en) = pure $ Update (coerce pp) en
