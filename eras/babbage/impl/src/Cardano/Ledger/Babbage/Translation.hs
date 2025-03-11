{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Translation where

import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Babbage.Core hiding (Tx)
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams ()
import Cardano.Ledger.Babbage.State
import Cardano.Ledger.Babbage.Tx (AlonzoTx (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (DecoderError)
import qualified Cardano.Ledger.Core as Core (Tx)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Data.Coerce (coerce)
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

instance TranslateEra BabbageEra NewEpochState where
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

instance TranslateEra BabbageEra Tx where
  type TranslationError BabbageEra Tx = DecoderError
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

instance TranslateEra BabbageEra PParams where
  translateEra _ = pure . upgradePParams ()

instance TranslateEra BabbageEra FuturePParams where
  translateEra ctxt = \case
    NoPParamsUpdate -> pure NoPParamsUpdate
    DefinitePParamsUpdate pp -> DefinitePParamsUpdate <$> translateEra ctxt pp
    PotentialPParamsUpdate mpp -> PotentialPParamsUpdate <$> mapM (translateEra ctxt) mpp

instance TranslateEra BabbageEra EpochState where
  translateEra ctxt es =
    pure
      EpochState
        { esAccountState = esAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esNonMyopic = esNonMyopic es
        }

instance TranslateEra BabbageEra DState where
  translateEra _ DState {..} = pure DState {..}

instance TranslateEra BabbageEra CommitteeState where
  translateEra _ CommitteeState {..} = pure CommitteeState {..}

instance TranslateEra BabbageEra PState where
  translateEra _ PState {..} = pure PState {..}

instance TranslateEra BabbageEra ShelleyCertState where
  translateEra ctxt ls =
    pure
      ShelleyCertState
        { shelleyCertDState = translateEra' ctxt $ shelleyCertDState ls
        , shelleyCertPState = translateEra' ctxt $ shelleyCertPState ls
        }

instance TranslateEra BabbageEra LedgerState where
  translateEra ctxt ls =
    pure
      LedgerState
        { lsUTxOState = translateEra' ctxt $ lsUTxOState ls
        , lsCertState = translateEra' ctxt $ lsCertState ls
        }

instance TranslateEra BabbageEra UTxOState where
  translateEra ctxt us =
    pure
      UTxOState
        { utxosUtxo = translateEra' ctxt $ utxosUtxo us
        , utxosDeposited = utxosDeposited us
        , utxosFees = utxosFees us
        , utxosGovState = translateEra' ctxt $ utxosGovState us
        , utxosInstantStake = translateEra' ctxt $ utxosInstantStake us
        , utxosDonation = utxosDonation us
        }

instance TranslateEra BabbageEra ShelleyInstantStake where
  translateEra _ = pure . coerce

instance TranslateEra BabbageEra UTxO where
  translateEra _ctxt utxo =
    pure $ UTxO $ translateTxOut `Map.map` unUTxO utxo

instance TranslateEra BabbageEra ShelleyGovState where
  translateEra ctxt ps =
    pure
      ShelleyGovState
        { sgsCurProposals = translateEra' ctxt $ sgsCurProposals ps
        , sgsFutureProposals = translateEra' ctxt $ sgsFutureProposals ps
        , sgsCurPParams = translateEra' ctxt $ sgsCurPParams ps
        , sgsPrevPParams = translateEra' ctxt $ sgsPrevPParams ps
        , sgsFuturePParams = translateEra' ctxt $ sgsFuturePParams ps
        }

instance TranslateEra BabbageEra ProposedPPUpdates where
  translateEra _ctxt (ProposedPPUpdates ppup) =
    pure $ ProposedPPUpdates $ fmap (upgradePParamsUpdate ()) ppup

translateTxOut ::
  TxOut AlonzoEra ->
  TxOut BabbageEra
translateTxOut = upgradeTxOut
{-# DEPRECATED translateTxOut "Use `upgradeTxOut` instead" #-}
