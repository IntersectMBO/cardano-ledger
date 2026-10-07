{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Cardano.Ledger.Receiving (receivingBenchmarks) where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.TxBody (receivingKeyHashes, receivingScriptHashes)
import Cardano.Ledger.Dijkstra.UTxO (getDijkstraScriptsNeeded, getDijkstraWitsVKeyNeeded)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (Payment))
import Criterion.Main
import Data.Bits (shiftR)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro

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
        ]
    | count <- [16, 256, 4096]
    ]

workload ::
  String -> (Network -> Credential Payment -> StakeReference -> Addr) -> Bool -> Int -> Benchmark
workload name address unique count =
  env (pure (body, Set.toAscList (receivingScriptHashes body))) $ \ ~(txBody, scripts) ->
    bgroup
      name
      [ bench "script-domain" $ nf receivingScriptHashes txBody
      , bench "key-domain" $ nf receivingKeyHashes txBody
      , bench "scripts-needed" $ nf (getDijkstraScriptsNeeded mempty) txBody
      , bench "key-witnesses-needed" $ nf (getDijkstraWitsVKeyNeeded mempty) txBody
      , bench "both-witness-callers" $
          nf (\b -> (getDijkstraScriptsNeeded mempty b, getDijkstraWitsVKeyNeeded mempty b)) txBody
      , bench "all-pointer-lookups" $
          nf (\b -> map (redeemerPointer b . ReceivingPurpose . AsItem) scripts) txBody
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

-- Exact-length deterministic hash bytes keep fixture construction outside the
-- timed region and supply genuinely distinct hashes for the unique workload.
scriptHash :: Word64 -> ScriptHash
scriptHash n =
  ScriptHash $
    fromJust $
      Hash.hashFromBytes $
        BS.pack [fromIntegral (n `shiftR` (8 * k)) | k <- [0 .. 7]] <> BS.replicate 20 0
