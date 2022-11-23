{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Translation where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..), extendPPWithGenesis)
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams, AlonzoPParamsUpdate, extendPP)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Binary (DecoderError)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era
  ( PreviousEra,
    TranslateEra (..),
    TranslationContext,
    translateEra',
  )
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley.API
  ( EpochState (..),
    NewEpochState (..),
    ShelleyGenesis,
    StrictMaybe (..),
  )
import qualified Cardano.Ledger.Shelley.API as API
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import qualified Cardano.Ledger.Shelley.Tx as LTX
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
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

type instance PreviousEra (AlonzoEra c) = MaryEra c

type instance TranslationContext (AlonzoEra c) = AlonzoGenesis

instance Crypto c => TranslateEra (AlonzoEra c) NewEpochState where
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

instance Crypto c => TranslateEra (AlonzoEra c) ShelleyGenesis where
  translateEra ctxt genesis =
    return
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
    Core.Tx (AlonzoEra c) ~ AlonzoTx (AlonzoEra c)
  ) =>
  TranslateEra (AlonzoEra c) Tx
  where
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

instance (Crypto c, Functor f) => TranslateEra (AlonzoEra c) (API.ShelleyPParamsHKD f)

instance Crypto c => TranslateEra (AlonzoEra c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translatePParams ctxt $ esPrevPp es,
          esPp = translatePParams ctxt $ esPp es,
          esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.LedgerState where
  translateEra ctxt ls =
    return
      API.LedgerState
        { API.lsUTxOState = translateEra' ctxt $ API.lsUTxOState ls,
          API.lsDPState = API.lsDPState ls
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.UTxOState where
  translateEra ctxt us =
    return
      API.UTxOState
        { API.utxosUtxo = translateEra' ctxt $ API.utxosUtxo us,
          API.utxosDeposited = API.utxosDeposited us,
          API.utxosFees = API.utxosFees us,
          API.utxosPpups = translateEra' ctxt $ API.utxosPpups us,
          API.utxosStakeDistr = API.utxosStakeDistr us
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.UTxO where
  translateEra _ctxt utxo =
    return $ API.UTxO $ translateTxOut `Map.map` API.unUTxO utxo

instance Crypto c => TranslateEra (AlonzoEra c) API.PPUPState where
  translateEra ctxt ps =
    return
      API.PPUPState
        { API.proposals = translateEra' ctxt $ API.proposals ps,
          API.futureProposals = translateEra' ctxt $ API.futureProposals ps
        }

instance Crypto c => TranslateEra (AlonzoEra c) API.ProposedPPUpdates where
  translateEra _ctxt (API.ProposedPPUpdates ppup) =
    return $ API.ProposedPPUpdates $ fmap translatePParamsUpdate ppup

translateTxOut ::
  Crypto c =>
  Core.TxOut (MaryEra c) ->
  Core.TxOut (AlonzoEra c)
translateTxOut (Shelley.TxOutCompact addr value) = TxOutCompact addr value

-- extendPP with type: extendPP :: Shelley.PParams' f era1 -> ... -> PParams' f era2
-- Is general enough to work for both
-- (PParams era)       = (PParams' Identity era)    and
-- (PParamsUpdate era) = (PParams' StrictMaybe era)

translatePParams ::
  AlonzoGenesis -> Shelley.ShelleyPParams (MaryEra c) -> AlonzoPParams (AlonzoEra c)
translatePParams = flip extendPPWithGenesis

translatePParamsUpdate ::
  Shelley.ShelleyPParamsUpdate (MaryEra c) -> AlonzoPParamsUpdate (AlonzoEra c)
translatePParamsUpdate pp =
  extendPP pp SNothing SNothing SNothing SNothing SNothing SNothing SNothing SNothing
