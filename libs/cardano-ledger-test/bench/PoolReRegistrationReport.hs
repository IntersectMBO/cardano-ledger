{-# LANGUAGE LambdaCase #-}

-- > cabal bench cardano-ledger-test:bench:pool-reregistration-report
module Main (main) where

import Bench.Cardano.Ledger.Dijkstra.PoolReRegistration
import Cardano.Ledger.Dijkstra.Core (bodyTxL, certsTxBodyL, sizeTxF)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Criterion (Benchmarkable, benchmarkWith')
import Criterion.Main.Options (defaultConfig)
import Criterion.Types (
  Config (timeLimit, verbosity),
  Report (reportAnalysis),
  SampleAnalysis (anMean),
  Verbosity (Quiet),
 )
import qualified Data.ByteString as BS
import Data.List (intercalate, transpose)
import Lens.Micro ((^.))
import Statistics.Types (estPoint)
import System.IO (hFlush, hPutStr, hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import Text.Printf (printf)

ebTxs :: Int
ebTxs = 774

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  fixture <- evaluate . force =<< loadFixture
  let key = fKey fixture
      measureTxSteps group tx =
        traverse (\(name, step) -> measure (group <> ": " <> name) step) (txSteps fixture tx)
      measureKey label verKey possessionProof =
        (+)
          <$> measure ("G2 " <> label) (verKey key)
          <*> measure ("G1 " <> label) (possessionProof key)
  bls <- measureTxSteps "BLS tx" (fBlsTx fixture)
  baseline <- measureTxSteps "baseline tx" (fNoBlsTx fixture)
  decompression <- measureKey "decompression" verKeyDecompression possessionProofDecompression
  deserialisation <-
    measureKey "deserialisation" verKeyDeserialisation possessionProofDeserialisation
  subgroupChecks <- measureKey "subgroup check" verKeySubgroupCheck possessionProofSubgroupCheck
  let blsKeys = txCertificates (fBlsTx fixture)
      keyBytes = BS.length (kfVerKeyBytes key) + BS.length (kfPossessionProofBytes key)
      uncompressedKeyBytes =
        BS.length (kfUncompressedVerKeyBytes key) + BS.length (kfUncompressedPossessionProofBytes key)
      -- As many certificates with uncompressed keys as fit in the bytes of the BLS tx's certificates.
      certificateBytes = BS.length (kfCertificateWithBlsKeyBytes key)
      uncompressedBlsKeys =
        blsKeys * certificateBytes `div` (certificateBytes + uncompressedKeyBytes - keyBytes)
      -- The BLS tx with its keys deserialised instead of decompressed, scaled to the certificates
      -- that fit.
      uncompressed =
        (* (fromIntegral uncompressedBlsKeys / fromIntegral blsKeys))
          <$> bls {tsDecode = tsDecode bls - fromIntegral blsKeys * (decompression - deserialisation)}
      comparison label x y = [label, showTime x, showTime y, showRatio (x / y)]
      txRow label step = comparison label (step bls) (step baseline)
      keysRow label perKey = [label, showTime (fromIntegral blsKeys * perKey), "—", "—"]
      ebRows x y =
        [ comparison label (perEb x) (perEb y)
        | (label, steps) <-
            [ ("Vote, cached txs (decode + reapply)", [tsDecode, tsReapplyValidated])
            , ("Vote, unseen txs (decode + full validation)", [tsDecode, tsApplyWithFullValidation])
            , ("Certifying RB adoption (decode + no validation)", [tsDecode, tsApplyWithoutValidation])
            , ("Serving, per peer (decode)", [tsDecode])
            ]
        , let perEb times = fromIntegral ebTxs * sum (map ($ times) steps)
        ]
      ebHeader = "Per EB of " <> show ebTxs <> " txs"
  putStr . unlines . intercalate [""] $
    [
      [ "BLS keys in Dijkstra pool re-registrations (cardano-ledger#6242)."
      , "BLS tx: "
          <> show blsKeys
          <> " pool re-registrations, each with a new BLS key ("
          <> show (txSize (fBlsTx fixture))
          <> " bytes). Baseline tx: "
          <> show (txCertificates (fNoBlsTx fixture))
          <> " without one ("
          <> show (txSize (fNoBlsTx fixture))
          <> " bytes)."
      ]
    , renderTable
        [ ["Per tx", "BLS tx", "Baseline tx", "BLS / baseline"]
        , txRow "Decode" tsDecode
        , keysRow ("↳ decompressing " <> show blsKeys <> " BLS keys") decompression
        , keysRow ("↳ subgroup-checking " <> show blsKeys <> " BLS keys") subgroupChecks
        , txRow "Apply with full validation" tsApplyWithFullValidation
        , txRow "Reapply validated" tsReapplyValidated
        , txRow "Apply without validation" tsApplyWithoutValidation
        ]
    , renderTable $ [ebHeader, "BLS EB", "Baseline EB", "BLS / baseline"] : ebRows bls baseline
    ,
      [ printf
          "Uncompressed BLS keys (%d bytes instead of %d): a max-size tx fits %d re-registrations instead of %d. Extrapolated from the BLS tx, scaled to %d re-registrations."
          uncompressedKeyBytes
          keyBytes
          uncompressedBlsKeys
          blsKeys
          uncompressedBlsKeys
      ]
    , renderTable
        [ ["Per BLS key", "Uncompressed", "Compressed", "Uncompressed / compressed"]
        , comparison "Decode" (deserialisation + subgroupChecks) (decompression + subgroupChecks)
        , comparison "↳ deserialising / decompressing" deserialisation decompression
        , comparison "↳ subgroup checks" subgroupChecks subgroupChecks
        ]
    , renderTable $
        [ebHeader, "Uncompressed keys", "Compressed keys", "Uncompressed / compressed"]
          : ebRows uncompressed bls
    ]

txCertificates :: TxFixture -> Int
txCertificates tx = length (tfTx tx ^. bodyTxL . certsTxBodyL)

txSize :: TxFixture -> Int
txSize tx = fromIntegral (tfTx tx ^. sizeTxF)

-- | Criterion's mean time of one run.
measure :: String -> Benchmarkable -> IO Double
measure label benchmarkable = do
  hPutStr stderr $ "Measuring " <> label <> " ... "
  hFlush stderr
  time <-
    estPoint . anMean . reportAnalysis
      <$> benchmarkWith' defaultConfig {timeLimit = 5, verbosity = Quiet} benchmarkable
  hPutStrLn stderr (showTime time)
  pure time

-- | A Markdown table padded to line up in a terminal, with the first column aligned left and the
-- others right.
renderTable :: [[String]] -> [String]
renderTable = \case
  [] -> []
  rows@(header : body) ->
    let widths = maximum . map length <$> transpose rows
        cells align = zipWith3 align [0 :: Int ..] widths
        renderRow row = "|" <> intercalate "|" (cells pad row) <> "|"
        pad column width cell
          | column == 0 = " " <> cell <> padding <> " "
          | otherwise = " " <> padding <> cell <> " "
          where
            padding = replicate (width - length cell) ' '
        dashes column width _
          | column == 0 = replicate (width + 2) '-'
          | otherwise = replicate (width + 1) '-' <> ":"
     in renderRow header : ("|" <> intercalate "|" (cells dashes header) <> "|") : map renderRow body

-- | A duration in seconds, with three significant digits and the most fitting unit.
showTime :: Double -> String
showTime seconds
  | seconds < 1e-6 = showNumber (seconds * 1e9) <> " ns"
  | seconds < 1e-3 = showNumber (seconds * 1e6) <> " µs"
  | seconds < 1 = showNumber (seconds * 1e3) <> " ms"
  | otherwise = showNumber seconds <> " s"

-- | Three significant digits, without an exponent.
showNumber :: Double -> String
showNumber x
  | x <= 0 = printf "%.2f" x
  | otherwise = printf "%.*f" decimals x
  where
    decimals = max 0 (2 - floor (logBase 10 x :: Double)) :: Int

showRatio :: Double -> String
showRatio r = showNumber r <> "×"
