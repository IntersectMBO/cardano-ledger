{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Cardano.Ledger.Receiving (receivingBenchmarks) where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Alonzo.Plutus.Context (LedgerLevelTxInfo (..), LedgerTxInfo (..))
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage
import Cardano.Ledger.BaseTypes (Globals (..), Network (..), ProtVer (..))
import Cardano.Ledger.Binary (decodeFull)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.TxBody (
  receivingKeyHashes,
  receivingScriptHashes,
  receivingScriptTargets,
 )
import Cardano.Ledger.Dijkstra.TxInfo (transTxRedeemersV4)
import Cardano.Ledger.Dijkstra.UTxO (getDijkstraScriptsNeeded, getDijkstraWitsVKeyNeeded)
import Cardano.Ledger.Plutus (Data, ExUnits (..), Language (..))
import Criterion.Main
import Data.Bits (shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isRight)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as SSeq
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Core.Utils (testGlobals)

-- Domain construction is measured at both the narrow queries and their real
-- witness/purpose callers. The larger workloads diagnose scaling and need not
-- fit in a valid transaction; this is not a phase-2 execution benchmark.
receivingBenchmarks :: Benchmark
receivingBenchmarks =
  bgroup
    "Receiving"
    [ bgroup
        (show count)
        [ workload "ordinary/unique" Addr True count
        , workload "protected/duplicates" AddrProtected False count
        , workload "protected/unique" AddrProtected True count
        , redeemerTranslationWorkload count
        ]
    | count <- [16, 256, 4096]
    ]

workload ::
  String -> (Network -> Credential Payment -> StakeReference -> Addr) -> Bool -> Int -> Benchmark
workload name address unique count =
  env (pure (body, map fst (receivingScriptTargets body))) $ \ ~(txBody, outputIndices) ->
    bgroup
      name
      [ bench "script-domain" $ nf receivingScriptHashes txBody
      , bench "output-targets" $ nf receivingScriptTargets txBody
      , bench "key-domain" $ nf receivingKeyHashes txBody
      , bench "scripts-needed" $ nf (getDijkstraScriptsNeeded mempty) txBody
      , bench "key-witnesses-needed" $ nf (getDijkstraWitsVKeyNeeded mempty) txBody
      , bench "both-witness-callers" $
          nf (\b -> (getDijkstraScriptsNeeded mempty b, getDijkstraWitsVKeyNeeded mempty b)) txBody
      , bench "all-pointer-lookups" $
          nf (\b -> map (redeemerPointer b . ReceivingPurpose . AsItem) outputIndices) txBody
      ]
  where
    body =
      mkBasicTxBody @DijkstraEra @TopTx
        & outputsTxBodyL
          .~ SSeq.fromList
            [ mkCoinTxOut @DijkstraEra (address Testnet (credential i) StakeRefNull) (Coin 1)
            | i <- [0 .. count - 1]
            ]
    credential i =
      let ScriptHash hash = scriptHash (if unique then fromIntegral i else 0)
       in if even i then ScriptHashObj (ScriptHash hash) else KeyHashObj (KeyHash (Hash.castHash hash))

-- These are the actual V4 context redeemer translators, with identical output
-- fully demanded through Show because the Plutus map has no NFData instance.
-- Rendering is a common cost in both measurements. Native script outputs are
-- at raw indices divisible by four, Plutus outputs at indices two modulo four,
-- and key outputs occupy all odd indices. Nothing compresses these positions.
redeemerTranslationWorkload :: Int -> Benchmark
redeemerTranslationWorkload count =
  env setup $ \ ~fixture ->
    bgroup
      "v4-redeemers/native-gaps"
      [ bench "old" $ nf (show . Babbage.transTxRedeemers proxy . ledgerInfo) fixture
      , bench "shared" $ nf (show . transTxRedeemersV4 . ledgerInfo) fixture
      ]
  where
    proxy = Proxy @PlutusV4
    body =
      mkBasicTxBody @DijkstraEra @TopTx
        & outputsTxBodyL
          .~ SSeq.fromList
            [ mkCoinTxOut @DijkstraEra
                (AddrProtected Testnet (credential i) StakeRefNull)
                (Coin 1)
            | i <- [0 .. count - 1]
            ]
    credential i =
      let ScriptHash hash = scriptHash (fromIntegral i)
       in if even i then ScriptHashObj (ScriptHash hash) else KeyHashObj (KeyHash (Hash.castHash hash))
    plutusTargets =
      [ (ReceivingPurpose (AsIx purposeIndex), hash)
      | (purposeIndex, hash) <- receivingScriptTargets body
      , purposeIndex `mod` 4 == 2
      ]
    datum :: Data DijkstraEra
    datum = either (error . show) id $ decodeFull (eraProtVerLow @DijkstraEra) (BSL.singleton 0)
    tx =
      mkBasicTx body
        & witsTxL . rdmrsTxWitsL . unRedeemersL
          .~ Map.fromList [(pointer, (datum, ExUnits 0 0)) | (pointer, _) <- plutusTargets]
    annotations = Map.fromList plutusTargets
    ledgerInfo ::
      (Tx TopTx DijkstraEra, Map.Map (PlutusPurpose AsIx DijkstraEra) ScriptHash) ->
      LedgerTxInfo TopTx DijkstraEra
    ledgerInfo (fixtureTx, hashes) =
      LedgerTxInfo
        { ltiProtVer = ProtVer (eraProtVerLow @DijkstraEra) 0
        , ltiEpochInfo = epochInfo testGlobals
        , ltiSystemStart = systemStart testGlobals
        , ltiUTxO = mempty
        , ltiTx = fixtureTx
        , ltiScriptsUsed = []
        , ltiScriptHashesUsed = hashes
        , ltiLevelTxInfo = LedgerTopTxInfo mempty
        }
    setup =
      let fixture = (tx, annotations)
          info = ledgerInfo fixture
          old = Babbage.transTxRedeemers proxy info
          shared = transTxRedeemersV4 info
       in if not (null plutusTargets) && isRight old && isRight shared && old == shared
            then pure fixture
            else error "V4 redeemer benchmark requires equal successful nonempty translations"

-- Exact-length deterministic hash bytes keep fixture construction outside the
-- timed region and supply genuinely distinct hashes for the unique workload.
scriptHash :: Word64 -> ScriptHash
scriptHash n =
  ScriptHash $
    fromJust $
      Hash.hashFromBytes $
        BS.pack [fromIntegral (n `shiftR` (8 * k)) | k <- [0 .. 7]] <> BS.replicate 20 0
