{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Translation (
  Tx (..),
  addrPtrNormalize,
  translateDatum,
  translateTxOut,
) where

import Cardano.Ledger.Address (addrPtrNormalize)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.CertState (CommitteeState (..))
import Cardano.Ledger.Conway.Core hiding (Tx)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (
  cgsCommitteeL,
  cgsConstitutionL,
  cgsCurPParamsL,
  cgsPrevPParamsL,
  mkEnactState,
  rsEnactStateL,
  setCompleteDRepPulsingState,
 )
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Conway.Tx ()
import qualified Cardano.Ledger.Core as Core (Tx)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Plutus.Data (translateDatum)
import Cardano.Ledger.Shelley.API (
  CertState (..),
  DState (..),
  EpochState (..),
  NewEpochState (..),
  PState (..),
  StrictMaybe (..),
  UTxOState (..),
  VState (..),
 )
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.LedgerState (
  epochStateGovStateL,
 )
import Data.Default.Class (Default (def))
import qualified Data.Map.Strict as Map
import Lens.Micro

--------------------------------------------------------------------------------
-- Translation from Babbage to Conway
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

type instance TranslationContext (ConwayEra c) = ConwayGenesis c

instance Crypto c => TranslateEra (ConwayEra c) NewEpochState where
  translateEra ctxt nes = do
    let es = translateEra' ctxt $ nesEs nes
        -- We need to ensure that we have the same initial EnactState in the pulser as
        -- well as in the current EnactState, otherwise in the very first EPOCH rule call
        -- the pulser will reset it.
        ratifyState =
          def
            & rsEnactStateL .~ mkEnactState (es ^. epochStateGovStateL)
    pure $
      NewEpochState
        { nesEL = nesEL nes
        , nesBprev = nesBprev nes
        , nesBcur = nesBcur nes
        , nesEs = setCompleteDRepPulsingState def ratifyState es
        , nesRu = nesRu nes
        , nesPd = nesPd nes
        , stashedAVVMAddresses = ()
        }

newtype Tx era = Tx {unTx :: Core.Tx era}

instance Crypto c => TranslateEra (ConwayEra c) Tx where
  type TranslationError (ConwayEra c) Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    txBody <- translateEraThroughCBOR "TxBody" $ tx ^. bodyTxL
    txWits <- translateEraThroughCBOR "TxWitness" $ tx ^. witsTxL
    auxData <- mapM (translateEraThroughCBOR "AuxData") (tx ^. auxDataTxL)
    let isValidTx = tx ^. isValidTxL
        newTx =
          mkBasicTx txBody
            & witsTxL .~ txWits
            & isValidTxL .~ isValidTx
            & auxDataTxL .~ auxData
    pure $ Tx newTx

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance Crypto c => TranslateEra (ConwayEra c) PParams where
  translateEra ConwayGenesis {cgUpgradePParams} = pure . upgradePParams cgUpgradePParams

instance Crypto c => TranslateEra (ConwayEra c) EpochState where
  translateEra ctxt es =
    pure $
      EpochState
        { esAccountState = esAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (ConwayEra c) DState where
  translateEra _ DState {..} = pure DState {..}

instance Crypto c => TranslateEra (ConwayEra c) CommitteeState where
  translateEra _ CommitteeState {..} = pure CommitteeState {..}

instance Crypto c => TranslateEra (ConwayEra c) VState where
  translateEra ctx VState {..} = do
    committeeState <- translateEra ctx vsCommitteeState
    pure VState {vsCommitteeState = committeeState, ..}

instance Crypto c => TranslateEra (ConwayEra c) PState where
  translateEra _ PState {..} = pure PState {..}

instance Crypto c => TranslateEra (ConwayEra c) CertState where
  translateEra ctxt ls =
    pure
      CertState
        { certDState = translateEra' ctxt $ certDState ls
        , certPState = translateEra' ctxt $ certPState ls
        , certVState = translateEra' ctxt $ certVState ls
        }

instance Crypto c => TranslateEra (ConwayEra c) API.LedgerState where
  translateEra conwayGenesis ls =
    pure
      API.LedgerState
        { API.lsUTxOState = translateEra' conwayGenesis $ API.lsUTxOState ls
        , API.lsCertState = translateEra' conwayGenesis $ API.lsCertState ls
        }

translateGovState ::
  Crypto c =>
  TranslationContext (ConwayEra c) ->
  GovState (BabbageEra c) ->
  GovState (ConwayEra c)
translateGovState ctxt@ConwayGenesis {..} sgov =
  let curPParams = translateEra' ctxt (sgov ^. curPParamsGovStateL)
      prevPParams = translateEra' ctxt (sgov ^. prevPParamsGovStateL)
   in emptyGovState
        & cgsCurPParamsL .~ curPParams
        & cgsPrevPParamsL .~ prevPParams
        & cgsCommitteeL .~ SJust cgCommittee
        & cgsConstitutionL .~ cgConstitution

instance Crypto c => TranslateEra (ConwayEra c) UTxOState where
  translateEra ctxt us =
    pure
      UTxOState
        { API.utxosUtxo = translateEra' ctxt $ API.utxosUtxo us
        , API.utxosDeposited = API.utxosDeposited us
        , API.utxosFees = API.utxosFees us
        , API.utxosGovState =
            translateGovState ctxt $
              API.utxosGovState us
        , API.utxosStakeDistr = API.utxosStakeDistr us
        , API.utxosDonation = API.utxosDonation us
        }

instance Crypto c => TranslateEra (ConwayEra c) API.UTxO where
  translateEra _ctxt utxo =
    pure $ API.UTxO $ upgradeTxOut `Map.map` API.unUTxO utxo

-- | Filter out `TxOut`s with zero Coins and normalize Pointers,
-- while converting `TxOut`s to Conway era.
translateTxOut ::
  Crypto c =>
  TxOut (BabbageEra c) ->
  TxOut (ConwayEra c)
translateTxOut = upgradeTxOut
{-# DEPRECATED translateTxOut "In favor of `upgradeTxOut`" #-}
