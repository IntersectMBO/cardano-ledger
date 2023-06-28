{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Translation where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra.Scripts (translateTimelock)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..), Datum (..))
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Conway.Core hiding (Tx)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Conway.Tx ()
import qualified Cardano.Ledger.Core as Core (Tx)
import Cardano.Ledger.Credential (StakeReference (..), normalizePtr)
import Cardano.Ledger.Crypto (Crypto)
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
import Cardano.Ledger.Val (Val (coin, zero))
import Data.Coerce
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

instance Crypto c => TranslateEra (ConwayEra c) Tx where
  type TranslationError (ConwayEra c) Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    txBody <- translateEraThroughCBOR "TxBody" $ tx ^. bodyTxL
    txWits <- translateEraThroughCBOR "TxWitness" $ tx ^. witsTxL
    auxData <- case tx ^. auxDataTxL of
      SNothing -> pure SNothing
      SJust auxData -> SJust <$> translateEraThroughCBOR "AuxData" auxData
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
  translateEra ctxt@ConwayGenesis {cgUpgradePParams} es =
    pure
      EpochState
        { esAccountState = esAccountState es
        , esSnapshots = esSnapshots es
        , esLState = translateEra' ctxt $ esLState es
        , esPrevPp = upgradePParams cgUpgradePParams $ esPrevPp es
        , esPp = upgradePParams cgUpgradePParams $ esPp es
        , esNonMyopic = esNonMyopic es
        }

instance Crypto c => TranslateEra (ConwayEra c) DState where
  translateEra _ DState {..} = pure DState {..}

instance Crypto c => TranslateEra (ConwayEra c) VState where
  translateEra _ VState {..} = pure VState {..}

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

instance Crypto c => TranslateEra (ConwayEra c) UTxOState where
  translateEra ctxt us =
    pure
      UTxOState
        { API.utxosUtxo = translateEra' ctxt $ API.utxosUtxo us
        , API.utxosDeposited = API.utxosDeposited us
        , API.utxosFees = API.utxosFees us
        , API.utxosGovernance = emptyGovernanceState
        , API.utxosStakeDistr = API.utxosStakeDistr us
        }

instance Crypto c => TranslateEra (ConwayEra c) API.UTxO where
  translateEra _ctxt utxo =
    pure $ API.UTxO $ translateTxOut `Map.mapMaybe` API.unUTxO utxo

-- | Filter out `TxOut`s with zero Coins and normalize Pointers,
-- while converting `TxOut`s to Conway era.
translateTxOut ::
  Crypto c =>
  TxOut (BabbageEra c) ->
  Maybe (TxOut (ConwayEra c))
translateTxOut (BabbageTxOut addr value d s)
  | coin value == zero = Nothing
  | otherwise =
      Just $ BabbageTxOut (addrPtrNormalize addr) value (translateDatum d) (translateScript <$> s)

-- | This function is implemented solely for the purpose of translating garbage pointers
-- into knowingly invalid ones. Any pointer that contains a SlotNo, TxIx or CertIx that
-- is too large to fit into Word32, Word16 and Word16 respectively, will have all of its
-- values set to 0 using `normalizePtr`.
--
-- There are two reasons why we can safely do that at the Babbage/Conway era boundary:
--
-- * Invalid pointers are no longer allowed in transactions starting with Babbage era
--
-- * There are only a handful of `Ptr`s on mainnet that are invalid.
--
-- Once the transition is complete and we are officially in Conway era, this translation
-- logic can be removed in favor of a fixed deserializer that does the same thing for all
-- eras prior to Babbage.
addrPtrNormalize :: Addr c -> Addr c
addrPtrNormalize = \case
  Addr n cred (StakeRefPtr ptr) -> Addr n cred (StakeRefPtr (normalizePtr ptr))
  addr -> addr

translateDatum :: Datum (BabbageEra c) -> Datum (ConwayEra c)
translateDatum = \case
  NoDatum -> NoDatum
  DatumHash dh -> DatumHash dh
  Datum bd -> Datum (coerce bd)

translateScript :: Crypto c => Script (BabbageEra c) -> Script (ConwayEra c)
translateScript = \case
  TimelockScript ts -> TimelockScript $ translateTimelock ts
  PlutusScript plutus -> PlutusScript plutus
