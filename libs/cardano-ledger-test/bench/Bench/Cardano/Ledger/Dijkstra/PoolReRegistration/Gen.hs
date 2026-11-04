{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Cardano.Ledger.Dijkstra.PoolReRegistration.Gen (
  writePoolReRegistrationResources,
  readPoolReRegistrationResources,
) where

import Cardano.Crypto.DSIGN (
  BLS12381MinSigDSIGN,
  createPossessionProofDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
 )
import Cardano.Crypto.DSIGN.BLS12381.Internal (minSigPoPDST)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (decodeFull', serialize')
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Tx (toCBORForBlockInclusion)
import Cardano.Ledger.Shelley.API (NewEpochState)
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.State (BlsKey (..), StakePoolParams (..))
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Default (def)
import qualified Data.Sequence.Strict as SSeq
import Data.Word (Word8)
import Lens.Micro
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.ImpSpec (ImpSpec (impInitIO), assertFailure, evalImpM)
import Test.QuickCheck.Random (newQCGen)

data PoolReRegistrationResources = PoolReRegistrationResources
  { prrSlotNo :: !SlotNo
  , prrNewEpochState :: !(NewEpochState DijkstraEra)
  , prrBlsTx :: !(Tx TopTx DijkstraEra)
  , prrNoBlsTx :: !(Tx TopTx DijkstraEra)
  }

ledgerStateFile, blsTxFile, noBlsTxFile :: FilePath
ledgerStateFile = "bench/resources/2_ledgerstate.cbor"
blsTxFile = "bench/resources/2_tx_bls.cbor"
noBlsTxFile = "bench/resources/2_tx_no_bls.cbor"

poolReRegistrationScenario :: ImpTestM DijkstraEra PoolReRegistrationResources
poolReRegistrationScenario = do
  poolId <- freshKeyHash
  vrfKeyHash <- freshKeyHashVRF
  accountAddress <- registerAccountAddress
  minPoolCost <- getsPParams ppMinPoolCostL
  let registration pledge blsKey =
        RegPoolTxCert
          StakePoolParams
            { sppId = poolId
            , sppVrf = vrfKeyHash
            , sppPledge = pledge
            , sppCost = minPoolCost
            , sppMargin = def
            , sppAccountAddress = accountAddress
            , sppOwners = mempty
            , sppRelays = mempty
            , sppMetadata = SNothing
            , sppBlsKey = blsKey
            }
  submitTx_ $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL .~ SSeq.singleton (registration mempty (SJust (mkBlsKey 0)))
  -- The certificates of a transaction are a set, so they all have to differ.
  -- in the BLS transasction the keys are all different, and in the control tx
  -- we have different pledge.
  prrBlsTx <- fillTx $ \n -> registration mempty . SJust . mkBlsKey <$> take n [1 ..]
  prrNoBlsTx <- fillTx $ \n -> (`registration` SNothing) . Coin <$> take n [1 ..]
  prrSlotNo <- getCurSlotNo
  prrNewEpochState <- getsNES id
  pure PoolReRegistrationResources {..}
  where
    mkBlsKey :: Word8 -> BlsKey
    mkBlsKey i =
      BlsKey (deriveVerKeyDSIGN signKey) (createPossessionProofDSIGN minSigPoPDST signKey)
      where
        signKey = genKeyDSIGN @BLS12381MinSigDSIGN . mkSeedFromBytes $ BS.pack (i : replicate 31 0xb1)

    -- The fixed-up transaction with the most of the given certificates that still fits in the
    -- maximum transaction size.
    fillTx ::
      (Int -> [TxCert DijkstraEra]) ->
      ImpTestM DijkstraEra (Tx TopTx DijkstraEra)
    fillTx certificates = do
      maxTxSize <- getsPParams ppMaxTxSizeL
      let build n = do
            tx <-
              fixupTx $
                mkBasicTx mkBasicTxBody & bodyTxL . certsTxBodyL .~ SSeq.fromList (certificates n)
            when (length (tx ^. bodyTxL . certsTxBodyL) /= n) $
              assertFailure $
                "Expected " <> show n <> " distinct certificates"
            pure tx
          grow n tx = do
            bigger <- build (n + 1)
            if bigger ^. sizeTxF <= maxTxSize then grow (n + 1) bigger else pure tx
      build 1 >>= grow 1

encodeTxInBlock :: Tx TopTx DijkstraEra -> BS.ByteString
encodeTxInBlock = serialize' (eraProtVerLow @DijkstraEra) . toCBORForBlockInclusion

writePoolReRegistrationResources :: IO ()
writePoolReRegistrationResources = do
  qcGen <- newQCGen
  impInit <- impInitIO @(LedgerSpec DijkstraEra) qcGen
  PoolReRegistrationResources {..} <- evalImpM (Just qcGen) Nothing impInit poolReRegistrationScenario
  BS.writeFile ledgerStateFile $ serialize' (eraProtVerLow @DijkstraEra) (prrSlotNo, prrNewEpochState)
  BS.writeFile blsTxFile $ encodeTxInBlock prrBlsTx
  BS.writeFile noBlsTxFile $ encodeTxInBlock prrNoBlsTx

readPoolReRegistrationResources ::
  IO ((SlotNo, NewEpochState DijkstraEra), BS.ByteString, BS.ByteString)
readPoolReRegistrationResources = do
  stateBytes <- BS.readFile ledgerStateFile
  state <-
    either (fail . ("Decoding " <>) . (ledgerStateFile <>) . (": " <>) . show) pure $
      decodeFull' (eraProtVerLow @DijkstraEra) stateBytes
  (,,) state <$> BS.readFile blsTxFile <*> BS.readFile noBlsTxFile
