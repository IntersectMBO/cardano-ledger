{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Cardano.Ledger.Dijkstra.PoolReRegistration (
  Fixture (..),
  TxFixture (..),
  KeyFixture (..),
  loadFixture,
  TxSteps (..),
  txSteps,
  verKeyDecompression,
  verKeyDeserialisation,
  verKeySubgroupCheck,
  possessionProofDecompression,
  possessionProofDeserialisation,
  possessionProofSubgroupCheck,
  poolReRegistrationBenchmarks,
) where

import Bench.Cardano.Ledger.Dijkstra.PoolReRegistration.Gen (readPoolReRegistrationResources)
import Cardano.Binary.FixedSizeCodec (rawDecodeFixedSized, rawEncodeFixedSized)
import Cardano.Crypto.DSIGN (
  BLS12381MinSigDSIGN,
  DSIGNAggregatable (..),
  DSIGNAlgorithm (..),
 )
import Cardano.Crypto.DSIGN.BLS12381.Internal (
  PossessionProofDSIGN (PossessionProofBLS12381),
  VerKeyDSIGN (VerKeyBLS12381),
  minSigPoPDST,
 )
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (
  AffinePtr,
  BLS (c_blst_deserialize, c_blst_uncompress),
  Curve1,
  Curve2,
  blsInGroup,
  blsSerialize,
  withNewAffine_,
 )
import Cardano.Ledger.BaseTypes (Globals (epochInfo, systemStart), StrictMaybe (..))
import Cardano.Ledger.Binary (
  DecCBOR,
  DecoderError,
  EncCBOR,
  decodeFull',
  decodeFullAnnotator,
  serialize',
 )
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Tx (Tx (MkDijkstraTx), decodeDijkstraTopTxInBlock)
import Cardano.Ledger.MempoolTx.Internal (MempoolTx (UnsafeMkMempoolTx))
import Cardano.Ledger.Shelley.API (
  ApplyTx (mkStAnnTx),
  MempoolEnv,
  MempoolState,
  applyTxWithFullValidation,
  mkMempoolEnv,
  mkMempoolState,
  reapplyValidatedTx,
  ruleApplyTxValidation,
  unsafeMakeValidatedTx,
 )
import Cardano.Ledger.Shelley.Rules (ledgerPpL)
import Cardano.Ledger.State (BlsKey (..), StakePoolParams (..), utxoG)
import Control.DeepSeq (NFData (..))
import Control.State.Transition (ValidationPolicy (ValidateNone))
import Criterion
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BSU
import Data.Foldable (toList)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import Lens.Micro
import Test.Cardano.Ledger.Dijkstra.ImpTest (LedgerSpec, impGlobalsL)
import Test.ImpSpec (ImpInit (impInitState), ImpSpec (impInitIO))
import Test.QuickCheck.Random (newQCGen)

--------------------------------------------------------------------------------
-- Fixture
--------------------------------------------------------------------------------

data Fixture = Fixture
  { fGlobals :: !Globals
  , fMempoolEnv :: !(MempoolEnv DijkstraEra)
  , fMempoolState :: !(MempoolState DijkstraEra)
  , fBlsTx :: !TxFixture
  -- ^ transaction full of BLS key registration certificates
  , fNoBlsTx :: !TxFixture
  -- ^ transaction full of certificates that are not BLS keys
  , fKey :: !KeyFixture
  }

instance NFData Fixture where
  rnf (Fixture _ _ mempoolState blsTx noBlsTx key) = rnf (mempoolState, blsTx, noBlsTx, key)

data TxFixture = TxFixture
  { tfBytes :: !BSL.ByteString
  , tfTx :: !(Tx TopTx DijkstraEra)
  }
  deriving (Generic)

instance NFData TxFixture

data KeyFixture = KeyFixture
  { kfVerKeyBytes :: !BS.ByteString
  , kfPossessionProofBytes :: !BS.ByteString
  , kfBlsKey :: !BlsKey
  , kfUncompressedVerKeyBytes :: !BS.ByteString
  , kfUncompressedPossessionProofBytes :: !BS.ByteString
  , kfCertificateWithBlsKeyBytes :: !BS.ByteString
  , kfCertificateWithoutBlsKeyBytes :: !BS.ByteString
  }
  deriving (Generic)

instance NFData KeyFixture

loadFixture :: IO Fixture
loadFixture = do
  impInit <- impInitIO @(LedgerSpec DijkstraEra) =<< newQCGen
  ((slotNo, nes), blsTxBytes, noBlsTxBytes) <- readPoolReRegistrationResources
  blsTx <- txFixture (BSL.fromStrict blsTxBytes)
  noBlsTx <- txFixture (BSL.fromStrict noBlsTxBytes)
  pure
    Fixture
      { fGlobals = impInitState impInit ^. impGlobalsL
      , fMempoolEnv = mkMempoolEnv nes slotNo
      , fMempoolState = mkMempoolState nes
      , fBlsTx = blsTx
      , fNoBlsTx = noBlsTx
      , fKey = keyFixture (firstCertificate blsTx) (firstCertificate noBlsTx)
      }
  where
    txFixture bytes = either (fail . show) (pure . TxFixture bytes) (decodeTxInBlock bytes)
    firstCertificate tx = case toList (tfTx tx ^. bodyTxL . certsTxBodyL) of
      certificate : _ -> certificate
      [] -> error "Transaction without certificates"

keyFixture :: TxCert DijkstraEra -> TxCert DijkstraEra -> KeyFixture
keyFixture certificateWithBlsKey certificateWithoutBlsKey =
  KeyFixture
    { kfVerKeyBytes = rawEncodeFixedSized (blsPubKey blsKey)
    , kfPossessionProofBytes = rawEncodeFixedSized (blsPossessionProof blsKey)
    , kfUncompressedVerKeyBytes = blsSerialize verKeyPoint
    , kfUncompressedPossessionProofBytes = blsSerialize possessionProofPoint
    , kfBlsKey = blsKey
    , kfCertificateWithBlsKeyBytes = encode certificateWithBlsKey
    , kfCertificateWithoutBlsKeyBytes = encode certificateWithoutBlsKey
    }
  where
    blsKey = case certificateWithBlsKey of
      RegPoolTxCert StakePoolParams {sppBlsKey = SJust key} -> key
      _ -> error "The first certificate of the BLS transaction is not a registration with a BLS key"
    VerKeyBLS12381 verKeyPoint = blsPubKey blsKey
    PossessionProofBLS12381 possessionProofPoint = blsPossessionProof blsKey
    encode :: EncCBOR a => a -> BS.ByteString
    encode = serialize' (eraProtVerLow @DijkstraEra)

decodeTxInBlock :: BSL.ByteString -> Either DecoderError (Tx TopTx DijkstraEra)
decodeTxInBlock =
  decodeFullAnnotator
    (eraProtVerLow @DijkstraEra)
    "Tx in block"
    (fmap MkDijkstraTx <$> decodeDijkstraTopTxInBlock)

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

data TxSteps a = TxSteps
  { tsDecode :: a
  , tsApplyWithFullValidation :: a
  , tsReapplyValidated :: a
  , tsApplyWithoutValidation :: a
  }
  deriving (Functor, Foldable, Traversable)

txSteps :: Fixture -> TxFixture -> TxSteps (String, Benchmarkable)
txSteps fixture tx =
  TxSteps
    { tsDecode =
        ("decode", nf (either (error . show) id . decodeTxInBlock) (tfBytes tx))
    , tsApplyWithFullValidation =
        ( "apply with full validation"
        , nf
            ( either (error . show) fst
                . applyTxWithFullValidation globals mempoolEnv mempoolState
                . UnsafeMkMempoolTx
            )
            (tfTx tx)
        )
    , tsReapplyValidated =
        ( "reapply validated"
        , nf
            ( either (error . show) id
                . reapplyValidatedTx globals mempoolEnv mempoolState
                . unsafeMakeValidatedTx globals mempoolEnv mempoolState
            )
            (tfTx tx)
        )
    , tsApplyWithoutValidation =
        ( "apply without validation"
        , nf
            ( either (error . show) fst
                . ruleApplyTxValidation @"LEDGER" ValidateNone globals mempoolEnv mempoolState
                . mkStAnnTx
                  (epochInfo globals)
                  (systemStart globals)
                  (mempoolEnv ^. ledgerPpL)
                  (mempoolState ^. utxoG)
                  mempty
            )
            (tfTx tx)
        )
    }
  where
    globals = fGlobals fixture
    mempoolEnv = fMempoolEnv fixture
    mempoolState = fMempoolState fixture

--------------------------------------------------------------------------------
-- Decoding a BLS key in steps (decompress, deserialise, subgroup check)
--------------------------------------------------------------------------------

verKeyDecompression :: KeyFixture -> Benchmarkable
verKeyDecompression keys = whnfIO (parsePoint (c_blst_uncompress @Curve2) (kfVerKeyBytes keys))

verKeyDeserialisation :: KeyFixture -> Benchmarkable
verKeyDeserialisation keys =
  whnfIO (parsePoint (c_blst_deserialize @Curve2) (kfUncompressedVerKeyBytes keys))

verKeySubgroupCheck :: KeyFixture -> Benchmarkable
verKeySubgroupCheck keys = whnf blsInGroup point
  where
    VerKeyBLS12381 point = blsPubKey (kfBlsKey keys)

possessionProofDecompression :: KeyFixture -> Benchmarkable
possessionProofDecompression keys =
  whnfIO (parsePoint (c_blst_uncompress @Curve1) (kfPossessionProofBytes keys))

possessionProofDeserialisation :: KeyFixture -> Benchmarkable
possessionProofDeserialisation keys =
  whnfIO (parsePoint (c_blst_deserialize @Curve1) (kfUncompressedPossessionProofBytes keys))

possessionProofSubgroupCheck :: KeyFixture -> Benchmarkable
possessionProofSubgroupCheck keys = whnf blsInGroup point
  where
    PossessionProofBLS12381 point = blsPossessionProof (kfBlsKey keys)

parsePoint :: BLS curve => (AffinePtr curve -> CString -> IO CInt) -> BS.ByteString -> IO CInt
parsePoint parse bytes =
  BSU.unsafeUseAsCString bytes $ \bytesPtr ->
    withNewAffine_ $ \affinePtr -> parse affinePtr bytesPtr

--------------------------------------------------------------------------------
-- Criterion suite
--------------------------------------------------------------------------------

poolReRegistrationBenchmarks :: Benchmark
poolReRegistrationBenchmarks =
  env loadFixture $ \fixture ->
    let keys = fKey fixture
        txBenchmarks tx = [bench name step | (name, step) <- toList (txSteps fixture tx)]
     in bgroup
          "DijkstraPoolReRegistration"
          [ bgroup
              "crypto"
              [ bench "decode BLS verification key (G2, 96 bytes)" $
                  nf (rawDecodeFixedSized @(VerKeyDSIGN BLS12381MinSigDSIGN) @Maybe) (kfVerKeyBytes keys)
              , bench "G2 decompression only" $ verKeyDecompression keys
              , bench "G2 subgroup check only" $ verKeySubgroupCheck keys
              , bench "decode BLS proof of possession (G1, 48 bytes)" $
                  nf
                    (rawDecodeFixedSized @(PossessionProofDSIGN BLS12381MinSigDSIGN) @Maybe)
                    (kfPossessionProofBytes keys)
              , bench "G1 decompression only" $ possessionProofDecompression keys
              , bench "G1 subgroup check only" $ possessionProofSubgroupCheck keys
              , bench "G2 deserialisation (uncompressed, 192 bytes) only" $ verKeyDeserialisation keys
              , bench "G1 deserialisation (uncompressed, 96 bytes) only" $
                  possessionProofDeserialisation keys
              , bench "verify proof of possession" $
                  nf
                    (verifyPossessionProofDSIGN minSigPoPDST (blsPubKey (kfBlsKey keys)))
                    (blsPossessionProof (kfBlsKey keys))
              ]
          , bgroup
              "ledger codec"
              [ bench "decode pool registration with BLS key" $
                  nf (decodeOrDie @(TxCert DijkstraEra)) (kfCertificateWithBlsKeyBytes keys)
              , bench "decode pool registration without BLS key" $
                  nf (decodeOrDie @(TxCert DijkstraEra)) (kfCertificateWithoutBlsKeyBytes keys)
              ]
          , bgroup "BLS tx" (txBenchmarks (fBlsTx fixture))
          , bgroup "baseline tx" (txBenchmarks (fNoBlsTx fixture))
          , bgroup
              "ledger state"
              [bench "force the ledger state (included in every apply)" $ nf id (fMempoolState fixture)]
          ]
  where
    decodeOrDie :: DecCBOR a => BS.ByteString -> a
    decodeOrDie = either (error . show) id . decodeFull' (eraProtVerLow @DijkstraEra)
