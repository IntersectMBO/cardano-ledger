{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Translation where

import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Babbage.Core hiding (Tx)
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams ()
import Cardano.Ledger.Babbage.Tx (AlonzoTx (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..), Datum (..))
import Cardano.Ledger.Binary (DecoderError)
import qualified Cardano.Ledger.Core as Core (Tx)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley.API (
  CertState (..),
  DState (..),
  EpochState (..),
  NewEpochState (..),
  PState (..),
  StrictMaybe (..),
  VState (..),
 )
import qualified Cardano.Ledger.Shelley.API as API
import qualified Data.Map.Strict as Map
import Lens.Micro

--------------------------------------------------------------------------------
-- Translation from Alonzo to Babbage
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

type instance TranslationContext (BabbageEra c) = ()

instance
  Crypto c =>
  TranslateEra (BabbageEra c) NewEpochState
  where
  translateEra ctxt nes =
    pure $
      NewEpochState
        { nesEL = nesEL nes
        , nesBprev = nesBprev nes
        , nesBcur = nesBcur nes
        , nesEs = translateEra' ctxt $ nesEs nes
        , nesRu = nesRu nes
        , nesPd = nesPd nes
        , stashedAVVMAddresses = ()
        }

newtype Tx era = Tx {unTx :: Core.Tx era}

instance Crypto c => TranslateEra (BabbageEra c) Tx where
  type TranslationError (BabbageEra c) Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    txBody <- translateEraThroughCBOR "TxBody" $ tx ^. bodyTxL
    txWits <- translateEraThroughCBOR "TxWitness" $ tx ^. witsTxL
    auxData <- case tx ^. auxDataTxL of
      SNothing -> pure SNothing
      SJust auxData -> SJust <$> translateEraThroughCBOR "AuxData" auxData
    let validating = tx ^. Alonzo.isValidTxL
    pure $ Tx $ AlonzoTx txBody txWits validating auxData

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance Crypto c => TranslateEra (BabbageEra c) PParams where
  translateEra _ = pure . upgradePParams ()

instance Crypto c => TranslateEra (BabbageEra c) EpochState where
  translateEra ctxt es =
    pure
      EpochState
        { esAccountState = esAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esPrevPp = upgradePParams () $ esPrevPp es
        , esPp = upgradePParams () $ esPp es
        , esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (BabbageEra c) DState where
  translateEra _ DState {..} = pure DState {..}

instance Crypto c => TranslateEra (BabbageEra c) VState where
  translateEra _ VState {..} = pure VState {..}

instance Crypto c => TranslateEra (BabbageEra c) PState where
  translateEra _ PState {..} = pure PState {..}

instance Crypto c => TranslateEra (BabbageEra c) CertState where
  translateEra ctxt ls =
    pure
      CertState
        { certDState = translateEra' ctxt $ certDState ls
        , certPState = translateEra' ctxt $ certPState ls
        , certVState = translateEra' ctxt $ certVState ls
        }

instance Crypto c => TranslateEra (BabbageEra c) API.LedgerState where
  translateEra ctxt ls =
    pure
      API.LedgerState
        { API.lsUTxOState = translateEra' ctxt $ API.lsUTxOState ls
        , API.lsCertState = translateEra' ctxt $ API.lsCertState ls
        }

instance Crypto c => TranslateEra (BabbageEra c) API.UTxOState where
  translateEra ctxt us =
    pure
      API.UTxOState
        { API.utxosUtxo = translateEra' ctxt $ API.utxosUtxo us
        , API.utxosDeposited = API.utxosDeposited us
        , API.utxosFees = API.utxosFees us
        , API.utxosGovernance = translateEra' ctxt $ API.utxosGovernance us
        , API.utxosStakeDistr = API.utxosStakeDistr us
        }

instance Crypto c => TranslateEra (BabbageEra c) API.UTxO where
  translateEra _ctxt utxo =
    pure $ API.UTxO $ translateTxOut `Map.map` API.unUTxO utxo

instance Crypto c => TranslateEra (BabbageEra c) API.ShelleyPPUPState where
  translateEra ctxt ps =
    pure
      API.ShelleyPPUPState
        { API.proposals = translateEra' ctxt $ API.proposals ps
        , API.futureProposals = translateEra' ctxt $ API.futureProposals ps
        }

instance Crypto c => TranslateEra (BabbageEra c) API.ProposedPPUpdates where
  translateEra _ctxt (API.ProposedPPUpdates ppup) =
    pure $ API.ProposedPPUpdates $ fmap (upgradePParamsUpdate ()) ppup

translateTxOut ::
  Crypto c =>
  TxOut (AlonzoEra c) ->
  TxOut (BabbageEra c)
translateTxOut (AlonzoTxOut addr value dh) = BabbageTxOut addr value d SNothing
  where
    d = case dh of
      SNothing -> NoDatum
      SJust d' -> DatumHash d'
