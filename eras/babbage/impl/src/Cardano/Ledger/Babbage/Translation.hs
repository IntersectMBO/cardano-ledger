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

module Cardano.Ledger.Babbage.Translation where

import Cardano.Binary
  ( DecoderError,
  )
import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxOut (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Tx (ValidatedTx (..))
import Cardano.Ledger.Babbage.TxBody (Datum (..), TxOut (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era
  ( PreviousEra,
    TranslateEra (..),
    TranslationContext,
    translateEra',
  )
import Cardano.Ledger.Serialization (translateViaCBORAnn)
import Cardano.Ledger.Shelley.API
  ( EpochState (..),
    NewEpochState (..),
    ShelleyGenesis,
    StrictMaybe (..),
  )
import qualified Cardano.Ledger.Shelley.API as API

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

type instance PreviousEra (BabbageEra c) = AlonzoEra c

type instance TranslationContext (BabbageEra c) = AlonzoGenesis

instance
  (Crypto c) =>
  TranslateEra (BabbageEra c) NewEpochState
  where
  translateEra ctxt nes =
    pure $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = nesBprev nes,
          nesBcur = nesBcur nes,
          nesEs = translateEra' ctxt $ nesEs nes,
          nesRu = nesRu nes,
          nesPd = nesPd nes
        }

instance Crypto c => TranslateEra (BabbageEra c) ShelleyGenesis where
  translateEra ctxt genesis =
    pure
      API.ShelleyGenesis
        { API.sgSystemStart = API.sgSystemStart genesis,
          API.sgNetworkMagic = API.sgNetworkMagic genesis,
          API.sgNetworkId = API.sgNetworkId genesis,
          API.sgActiveSlotsCoeff = API.sgActiveSlotsCoeff genesis,
          API.sgSecurityParam = API.sgSecurityParam genesis,
          API.sgEpochLength = API.sgEpochLength genesis,
          API.sgSlotsPerKESPeriod = API.sgSlotsPerKESPeriod genesis,
          API.sgMaxKESEvolutions = API.sgMaxKESEvolutions genesis,
          API.sgSlotLength = API.sgSlotLength genesis,
          API.sgUpdateQuorum = API.sgUpdateQuorum genesis,
          API.sgMaxLovelaceSupply = API.sgMaxLovelaceSupply genesis,
          API.sgProtocolParams = translateEra' ctxt (API.sgProtocolParams genesis),
          API.sgGenDelegs = API.sgGenDelegs genesis,
          API.sgInitialFunds = API.sgInitialFunds genesis,
          API.sgStaking = API.sgStaking genesis
        }

newtype Tx era = Tx {unTx :: Core.Tx era}

instance
  ( Crypto c,
    Core.Tx (BabbageEra c) ~ ValidatedTx (BabbageEra c)
  ) =>
  TranslateEra (BabbageEra c) Tx
  where
  type TranslationError (BabbageEra c) Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    bdy <- translateViaCBORAnn "txbody" $ Alonzo.body tx
    txwits <- translateViaCBORAnn "txwitness" $ Alonzo.wits tx
    aux <- case Alonzo.auxiliaryData tx of
      SNothing -> pure SNothing
      SJust axd -> SJust <$> translateViaCBORAnn "auxiliarydata" axd
    let validating = Alonzo.isValid tx
    pure $ Tx $ ValidatedTx bdy txwits validating aux

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance (Crypto c, Functor f) => TranslateEra (BabbageEra c) (API.PParams' f)

instance Crypto c => TranslateEra (BabbageEra c) EpochState where
  translateEra ctxt es =
    pure
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translatePParams $ esPrevPp es,
          esPp = translatePParams $ esPp es,
          esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (BabbageEra c) API.LedgerState where
  translateEra ctxt ls =
    pure
      API.LedgerState
        { API._utxoState = translateEra' ctxt $ API._utxoState ls,
          API._delegationState = API._delegationState ls
        }

instance Crypto c => TranslateEra (BabbageEra c) API.UTxOState where
  translateEra ctxt us =
    pure
      API.UTxOState
        { API._utxo = translateEra' ctxt $ API._utxo us,
          API._deposited = API._deposited us,
          API._fees = API._fees us,
          API._ppups = translateEra' ctxt $ API._ppups us,
          API._stakeDistro = API._stakeDistro us
        }

instance Crypto c => TranslateEra (BabbageEra c) API.UTxO where
  translateEra _ctxt utxo =
    pure $ API.UTxO $ translateTxOut <$> API.unUTxO utxo

instance Crypto c => TranslateEra (BabbageEra c) API.PPUPState where
  translateEra ctxt ps =
    pure
      API.PPUPState
        { API.proposals = translateEra' ctxt $ API.proposals ps,
          API.futureProposals = translateEra' ctxt $ API.futureProposals ps
        }

instance Crypto c => TranslateEra (BabbageEra c) API.ProposedPPUpdates where
  translateEra _ctxt (API.ProposedPPUpdates ppup) =
    pure $ API.ProposedPPUpdates $ fmap translatePParams ppup

translateTxOut ::
  Crypto c =>
  Core.TxOut (AlonzoEra c) ->
  Core.TxOut (BabbageEra c)
translateTxOut (Alonzo.TxOut addr value dh) = TxOut addr value d
  where
    d = case dh of
      SNothing -> NoDatum
      SJust d' -> DatumHash d'

translatePParams ::
  Alonzo.PParams' f (AlonzoEra c) -> PParams' f (BabbageEra c)
translatePParams (Alonzo.PParams {..}) = PParams {..}
