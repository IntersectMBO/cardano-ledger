{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway (
  Conway,
  ConwayEra,
  readNewEpochState,
  tTx,
  mkGlobals,
  mkEnv,
  repro,
)
where

import Cardano.Ledger.Alonzo (reapplyAlonzoTx)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.TxInfo (EraPlutusContext, ExtendedUTxO (..))
import Cardano.Ledger.Babbage.Tx (babbageTxScripts, getDatumBabbage)
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Binary (sizedValue)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Rules ()
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Conway.TxInfo (conwayTxInfo)
import Cardano.Ledger.Conway.TxOut ()
import Cardano.Ledger.Conway.UTxO ()
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import qualified Data.Set as Set
import Lens.Micro

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API.Mempool
import Cardano.Ledger.Shelley.Genesis
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL

repro :: IO (LedgerState Conway)
repro = do
  nes <- readNewEpochState
  tx <- tTx
  globals <- mkGlobals (esPp (nesEs nes))
  let env = mkEnv nes
      st = esLState (nesEs nes)
  either (error . show) (pure . fst) $ applyTx globals env st tx

readEraType :: (Era era, NFData (t era), DecCBOR (t era), MonadIO m) => FilePath -> m (t era)
readEraType filePath = liftIO $ do
  bls <- BSL.readFile filePath
  either (error . show) (evaluate . force) $ decodeFull (eraProtVerLow @era) bls


readNewEpochState :: IO (NewEpochState Conway)
readNewEpochState = do
  ls <- BSL.readFile "/home/lehins/Downloads/ledger-state.42.cbor"
  either (error . show) pure $ decodeFull (natVersion @9) ls :: IO (NewEpochState Conway)

tTx :: IO (Tx Conway)
tTx = do
  tx <- either error (pure . BSL.fromStrict) $ B16.decode "84a300818258207c7e5fc419c2fd1a33d8f4761dd6e4cb6c43554c55378c7fccdf65f22c1c946b000181a200581d60ed0998e484ef9c9088c43ee7169cf0811cea2e52c07a3bf7b5b0cb63011b00000045d962375b021a000280a5a10081825820190e437455e5909c2af62f5e61e316324dccca282a4a6fa8c47b851e1d4137b05840f687759143c365eb459597dca30c46fdda72a37cada18da040813624a59ae6da1eed3db6292470ea40c1a55ccc228be39791d6ae8a1cb06ddbc371ee52a37803f5f6"
  either (error . show) pure $ decodeFullAnnotator (natVersion @9) "Tx" decCBOR tx :: IO (Tx Conway)

getShelleyGenesis :: IO (ShelleyGenesis StandardCrypto)
getShelleyGenesis = do
  either error id <$> Aeson.eitherDecodeFileStrict' "/home/lehins/Downloads/genesis.json"

mkGlobals :: PParams Conway -> IO Globals
mkGlobals pp = do
  genesis <- getShelleyGenesis
  pure $ mkShelleyGlobals genesis (epochInfoE genesis) majorPParamsVer
  where
    majorPParamsVer = pvMajor $ pp ^. ppProtocolVersionL
    epochInfoE genesis =
      fixedEpochInfo
        (sgEpochLength genesis)
        (mkSlotLength . fromNominalDiffTimeMicro . sgSlotLength $ genesis)

mkEnv :: NewEpochState Conway -> LedgerEnv Conway
mkEnv nes =
  LedgerEnv
    { ledgerSlotNo = SlotNo 123456
    , ledgerIx = mkTxIxPartial 0
    , ledgerPp = pp
    , ledgerAccount = as
    }
  where
    pp = esPp (nesEs nes)
    as = esAccountState (nesEs nes)

type Conway = ConwayEra StandardCrypto

-- =====================================================

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  , EraPlutusContext 'PlutusV2 (ConwayEra c)
  , EraPlutusContext 'PlutusV3 (ConwayEra c)
  ) =>
  API.ApplyTx (ConwayEra c)
  where
  reapplyTx = reapplyAlonzoTx

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  , EraPlutusContext 'PlutusV2 (ConwayEra c)
  , EraPlutusContext 'PlutusV3 (ConwayEra c)
  ) =>
  API.ApplyBlock (ConwayEra c)

instance Crypto c => API.CanStartFromGenesis (ConwayEra c) where
  type AdditionalGenesisConfig (ConwayEra c) = ConwayGenesis c
  fromShelleyPParams =
    error "Unimplemented: Current interface is too limited and needs replacement for Conway to work"

instance
  ( Crypto c
  , EraPlutusContext 'PlutusV2 (ConwayEra c)
  , EraPlutusContext 'PlutusV3 (ConwayEra c)
  ) =>
  ExtendedUTxO (ConwayEra c)
  where
  txInfo = conwayTxInfo
  txscripts = babbageTxScripts
  getAllowedSupplimentalDataHashes txBody (UTxO utxo) =
    Set.fromList [dh | txOut <- outs, SJust dh <- [txOut ^. dataHashTxOutL]]
    where
      newOuts = map sizedValue $ toList $ txBody ^. allSizedOutputsTxBodyF
      referencedOuts = Map.elems $ Map.restrictKeys utxo (txBody ^. referenceInputsTxBodyL)
      outs = newOuts <> referencedOuts
  getDatum = getDatumBabbage

-- cabal exec -- ghci -package cardano-ledger-core -package cardano-ledger-conway -package cardano-ledger-shelley -package cardano-ledger-babbage
