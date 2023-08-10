{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Translation where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.CertState (PState (..), VState (..))
import Cardano.Ledger.Core (upgradePParams, upgradePParamsUpdate, upgradeTxOut)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era (
  TranslateEra (..),
  TranslationContext,
  translateEra',
 )
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley.API (
  CertState (..),
  DState (..),
  EpochState (..),
  NewEpochState (..),
  ShelleyGovState (..),
  StrictMaybe (..),
 )
import qualified Cardano.Ledger.Shelley.API as API
import qualified Cardano.Ledger.Shelley.Tx as LTX
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Translation from Mary to Alonzo
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

type instance TranslationContext (AlonzoEra c) = AlonzoGenesis

instance Crypto c => TranslateEra (AlonzoEra c) NewEpochState where
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

instance Crypto c => TranslateEra (AlonzoEra c) Core.PParams where
  translateEra (AlonzoGenesisWrapper upgradeArgs) = pure . upgradePParams upgradeArgs

newtype Tx era = Tx {unTx :: Core.Tx era}

instance Crypto c => TranslateEra (AlonzoEra c) Tx where
  type TranslationError (AlonzoEra c) Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    txBody <- Core.translateEraThroughCBOR "TxBody" $ LTX.body tx
    txWits <- Core.translateEraThroughCBOR "TxWits" $ LTX.wits tx
    -- transactions from Mary era always pass script ("phase 2") validation
    auxData <- case LTX.auxiliaryData tx of
      SNothing -> pure SNothing
      SJust auxData -> SJust <$> Core.translateEraThroughCBOR "AuxData" auxData
    let validating = IsValid True
    pure $ Tx $ AlonzoTx txBody txWits validating auxData

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance Crypto c => TranslateEra (AlonzoEra c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (AlonzoEra c) DState where
  translateEra _ DState {..} = pure DState {..}

instance Crypto c => TranslateEra (AlonzoEra c) VState where
  translateEra _ VState {..} = pure VState {..}

instance Crypto c => TranslateEra (AlonzoEra c) PState where
  translateEra _ PState {..} = pure PState {..}

instance Crypto c => TranslateEra (AlonzoEra c) CertState where
  translateEra ctxt ls =
    pure
      CertState
        { certDState = translateEra' ctxt $ certDState ls
        , certPState = translateEra' ctxt $ certPState ls
        , certVState = translateEra' ctxt $ certVState ls
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.LedgerState where
  translateEra ctxt ls =
    return
      API.LedgerState
        { API.lsUTxOState = translateEra' ctxt $ API.lsUTxOState ls
        , API.lsCertState = translateEra' ctxt $ API.lsCertState ls
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.UTxOState where
  translateEra ctxt us =
    return
      API.UTxOState
        { API.utxosUtxo = translateEra' ctxt $ API.utxosUtxo us
        , API.utxosDeposited = API.utxosDeposited us
        , API.utxosFees = API.utxosFees us
        , API.utxosGovState = translateEra' ctxt $ API.utxosGovState us
        , API.utxosStakeDistr = API.utxosStakeDistr us
        , API.utxosDonation = API.utxosDonation us
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.UTxO where
  translateEra _ctxt utxo =
    return $ API.UTxO $ translateTxOut `Map.map` API.unUTxO utxo

instance Crypto c => TranslateEra (AlonzoEra c) API.ShelleyGovState where
  translateEra ctxt@(AlonzoGenesisWrapper upgradeArgs) ps =
    return
      API.ShelleyGovState
        { API.proposals = translateEra' ctxt $ API.proposals ps
        , API.futureProposals = translateEra' ctxt $ API.futureProposals ps
        , API.sgovPrevPp = upgradePParams upgradeArgs $ sgovPrevPp ps
        , API.sgovPp = upgradePParams upgradeArgs $ sgovPp ps
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.ProposedPPUpdates where
  translateEra _ctxt (API.ProposedPPUpdates ppup) =
    return $ API.ProposedPPUpdates $ fmap (upgradePParamsUpdate def) ppup

translateTxOut ::
  Crypto c =>
  Core.TxOut (MaryEra c) ->
  Core.TxOut (AlonzoEra c)
translateTxOut = upgradeTxOut
{-# DEPRECATED translateTxOut "Use `upgradeTxOut` instead" #-}
