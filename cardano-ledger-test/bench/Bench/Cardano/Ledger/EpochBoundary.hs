{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Benchmarks for things which happen on an epoch boundary.
module Bench.Cardano.Ledger.EpochBoundary where

import Cardano.Crypto.DSIGN.Mock
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Compactible (Compactible (toCompact))
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.SafeHash
  ( SafeToHash (makeHashWithExplicitProxys),
    castSafeHash,
  )
import Cardano.Ledger.ShelleyMA ()
import qualified Cardano.Ledger.Val as Val
import Criterion
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Word (Word64)
import Shelley.Spec.Ledger.Address (Addr (Addr))
import Shelley.Spec.Ledger.BaseTypes (Network (Testnet))
import Shelley.Spec.Ledger.CompactAddr (compactAddr)
import Shelley.Spec.Ledger.Credential
  ( Credential (KeyHashObj),
    PaymentCredential,
    Ptr (..),
    StakeCredential,
    StakeReference (StakeRefBase, StakeRefPtr),
  )
import Shelley.Spec.Ledger.EpochBoundary (aggregateUtxoCoinByCredential)
import Shelley.Spec.Ledger.Keys (VKey (..), hashKey)
import Shelley.Spec.Ledger.Slot (SlotNo (SlotNo))
import Shelley.Spec.Ledger.TxBody (TxId (..), TxIn (TxInCompact), TxOut (..))
import Shelley.Spec.Ledger.UTxO (UTxO (UTxO))
import Test.Cardano.Ledger.EraBuffet (TestCrypto)

type TestEra = MaryEra TestCrypto

payCred :: PaymentCredential TestCrypto
payCred = KeyHashObj (hashKey . VKey $ VerKeyMockDSIGN 0)

-- | Infinite list of transaction inputs
txIns :: [TxIn TestCrypto]
txIns = [0 ..] <&> TxInCompact txId
  where
    txId =
      TxId . castSafeHash $
        makeHashWithExplicitProxys
          (Proxy @TestCrypto)
          (Proxy @EraIndependentTxBody)
          ("Galadriel" :: ByteString)

-- | Generate TxOuts for each stake credential.
txOutsFromCreds :: [StakeCredential TestCrypto] -> [TxOut TestEra]
txOutsFromCreds creds =
  [ TxOutCompact
      (compactAddr $ Addr Testnet payCred (StakeRefBase cred))
      (fromJust . toCompact . Val.inject $ Coin 100)
    | cred <- creds
  ]

txOutsFromPtrs :: [Ptr] -> [TxOut TestEra]
txOutsFromPtrs ptrs =
  [ TxOutCompact
      (compactAddr $ Addr Testnet payCred (StakeRefPtr ptr))
      (fromJust . toCompact . Val.inject $ Coin 200)
    | ptr <- ptrs
  ]

-- | Generate n stake credentials
stakeCreds :: Word64 -> [StakeCredential TestCrypto]
stakeCreds n =
  [0 .. n]
    <&> (\i -> KeyHashObj (hashKey . VKey $ VerKeyMockDSIGN i))

-- | Generate pointers to a list of stake credentials
stakePtrs :: [StakeCredential c] -> Map Ptr (StakeCredential c)
stakePtrs creds =
  Map.fromList
    [ (Ptr (SlotNo i) 0 0, cred)
      | (i, cred) <- zip [0 ..] creds
    ]

-- | Create a UTxO set containing 'noBase' base stake credentials and any
-- pointer credentials passed in 'ptrMap'. For each credential, there will be
-- 'dupFactor' entries in the UTxO.
utxo :: Word64 -> Map Ptr c -> Int -> UTxO TestEra
utxo noBase ptrMap dupFactor =
  UTxO $
    Map.fromList
      [ (txIn, txOut)
        | let txOutB = txOutsFromCreds $ stakeCreds noBase,
          let txOutP = txOutsFromPtrs $ Map.keys ptrMap,
          let allTxs = txOutP ++ txOutB,
          txIn <- take (dupFactor * length allTxs) txIns,
          txOut <- allTxs
      ]

data AggTestSetup = AggTestSetup
  { atsPtrMap :: !(Map Ptr (StakeCredential TestCrypto)),
    atsUTxO :: !(UTxO TestEra)
  }

-- | Construct the relevant UTxO and pointer map to test
-- 'aggregateUtxoCoinByCredential'.
--
-- There are three parameters to this:
-- - The number of distinct 'StakeRefBase' outputs present in the UTxO.
-- - The number of distinct 'StakeRefPtr' outputs present in the UTxO, and the
--   corresponding size of the pointer map.
-- - The "duplication factor". This represents the number of 'TxIn's pointing to
--   outputs with a given stake address. Thus with a duplication factor of 1,
--   there will be only one "coin" per credential, and little aggregation. With
--   a factor of 10, there will be 10 coins that must be aggregated.
--
--   Note that this corresponds to two situations in real usage: multiple
--   transactions paying to the same address, and multiple addresses sharing the
--   same stake credential. These scenarios are identical for benchmarking
--   purposes, and hence we roll them into one.
--
-- There is one potential missing parameter; the number of unstaked addresses
-- present. These contribute to the size of the UTxO but not to any aggregation
-- cost.
sizedAggTestSetup ::
  -- | Number of distinct base stake credentials
  Word64 ->
  -- | Number of distinct pointer stake credentials
  Word64 ->
  -- | Duplication factor
  Int ->
  AggTestSetup
sizedAggTestSetup noBase noPtr dupFactor = AggTestSetup pm ut
  where
    pm = stakePtrs $ stakeCreds noPtr
    ut = utxo noBase pm dupFactor

aggregateUtxoBench :: Benchmark
aggregateUtxoBench =
  bgroup
    "aggregateUtxoCoinByCredential"
    [ bench "100/100" $ whnf go (sizedAggTestSetup 100 100 1),
      bench "10/10 * 100" $ whnf go (sizedAggTestSetup 10 10 100),
      bench "100/100 * 10" $ whnf go (sizedAggTestSetup 100 100 10),
      bench "1000/1000" $ whnf go (sizedAggTestSetup 1000 1000 1),
      bench "1000/1000 * 10" $ whnf go (sizedAggTestSetup 1000 1000 10),
      bench "10000/1000" $ whnf go (sizedAggTestSetup 10000 1000 1),
      bench "1000/10000" $ whnf go (sizedAggTestSetup 1000 10000 1),
      bench "10000/10000" $ whnf go (sizedAggTestSetup 10000 10000 1)
    ]
  where
    go AggTestSetup {atsPtrMap, atsUTxO} =
      aggregateUtxoCoinByCredential atsPtrMap atsUTxO Map.empty
