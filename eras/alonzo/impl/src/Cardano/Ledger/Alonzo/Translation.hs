{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Translation where

import Cardano.Ledger.Alonzo.Core hiding (Tx)
import qualified Cardano.Ledger.Alonzo.Core as Core
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.CertState (CommitteeState (..), PState (..), VState (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))

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

instance Crypto c => TranslateEra (AlonzoEra c) PParams where
  translateEra (AlonzoGenesisWrapper upgradeArgs) = pure . upgradePParams upgradeArgs

newtype Tx era = Tx {unTx :: Core.Tx era}

instance Crypto c => TranslateEra (AlonzoEra c) Tx where
  type TranslationError (AlonzoEra c) Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    txBody <- translateEraThroughCBOR "TxBody" $ tx ^. bodyTxL
    txWits <- translateEraThroughCBOR "TxWits" $ tx ^. witsTxL
    txAuxData <- mapM (translateEraThroughCBOR "TxAuxData") (tx ^. auxDataTxL)
    -- transactions from Mary era always pass script ("phase 2") validation
    let validating = IsValid True
    pure $ Tx $ AlonzoTx txBody txWits validating txAuxData -- mempty

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

instance Crypto c => TranslateEra (AlonzoEra c) CommitteeState where
  translateEra _ CommitteeState {..} = pure CommitteeState {..}

instance Crypto c => TranslateEra (AlonzoEra c) VState where
  translateEra ctx VState {..} = do
    committeeState <- translateEra ctx vsCommitteeState
    pure VState {vsCommitteeState = committeeState, ..}

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

instance Crypto c => TranslateEra (AlonzoEra c) LedgerState where
  translateEra ctxt ls =
    return
      LedgerState
        { lsUTxOState = translateEra' ctxt $ lsUTxOState ls
        , lsCertState = translateEra' ctxt $ lsCertState ls
        }

instance Crypto c => TranslateEra (AlonzoEra c) UTxOState where
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

instance Crypto c => TranslateEra (AlonzoEra c) UTxO where
  translateEra _ctxt utxo =
    return $ UTxO $ translateTxOut `Map.map` unUTxO utxo

instance Crypto c => TranslateEra (AlonzoEra c) ShelleyGovState where
  translateEra ctxt ps =
    return
      ShelleyGovState
        { sgsCurProposals = translateEra' ctxt $ sgsCurProposals ps
        , sgsFutureProposals = translateEra' ctxt $ sgsFutureProposals ps
        , sgsCurPParams = translateEra' ctxt $ sgsCurPParams ps
        , sgsPrevPParams = translateEra' ctxt $ sgsPrevPParams ps
        }

instance Crypto c => TranslateEra (AlonzoEra c) ProposedPPUpdates where
  translateEra _ctxt (ProposedPPUpdates ppup) =
    return $ ProposedPPUpdates $ fmap (upgradePParamsUpdate def) ppup

translateTxOut ::
  Crypto c =>
  TxOut (MaryEra c) ->
  TxOut (AlonzoEra c)
translateTxOut = upgradeTxOut
{-# DEPRECATED translateTxOut "Use `upgradeTxOut` instead" #-}
