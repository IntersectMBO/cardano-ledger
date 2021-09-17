{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Benchmarks for things which happen on an epoch boundary.
module Bench.Cardano.Ledger.EpochBoundary where

import Cardano.Crypto.DSIGN.Mock
import Cardano.Ledger.Address (Addr (Addr))
import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Compactible (Compactible (toCompact))
import Cardano.Ledger.Credential
  ( Credential (KeyHashObj),
    PaymentCredential,
    Ptr (..),
    StakeCredential,
    StakeReference (StakeRefBase, StakeRefNull, StakeRefPtr),
  )
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (VKey (..), hashKey)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.SafeHash
  ( SafeToHash (makeHashWithExplicitProxys),
    castSafeHash,
  )
import Cardano.Ledger.Shelley.CompactAddr (compactAddr)
import Cardano.Ledger.Shelley.EpochBoundary (aggregateUtxoCoinByCredential)
import Cardano.Ledger.Shelley.TxBody (TxId (..), TxIn (TxInCompact), TxOut (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (UTxO))
import Cardano.Ledger.ShelleyMA ()
import Cardano.Ledger.Slot (SlotNo (SlotNo))
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData (..))
import Criterion
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.List (genericReplicate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Word (Word64)
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

-- | Unstaked address
txOutUnstaked :: TxOut TestEra
txOutUnstaked =
  TxOutCompact @TestEra
    (compactAddr $ Addr Testnet payCred StakeRefNull)
    (fromJust . toCompact . Val.inject $ Coin 1000)

-- | Generate TxOuts for each stake credential.
txOutsFromCreds :: [StakeCredential TestCrypto] -> [TxOut TestEra]
txOutsFromCreds creds =
  [ TxOutCompact
      (compactAddr $ Addr Testnet payCred (StakeRefBase cred))
      coinVal
    | cred <- creds
  ]
  where
    coinVal = fromJust . toCompact . Val.inject $ Coin 100

txOutsFromPtrs :: [Ptr] -> [TxOut TestEra]
txOutsFromPtrs ptrs =
  [ TxOutCompact
      (compactAddr $ Addr Testnet payCred (StakeRefPtr ptr))
      coinVal
    | ptr <- ptrs
  ]
  where
    coinVal = fromJust . toCompact . Val.inject $ Coin 200

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

-- | Create a UTxO set containing 'noUnstaked' unstaked TxOuts, 'noBase'
-- base stake credentials and any pointer credentials passed in 'ptrMap'. For
-- each credential, there will be 'dupFactor' entries in the UTxO.
utxo ::
  -- | Number of unstaked outputs
  Word64 ->
  -- | Number of distinct base stake credentials
  Word64 ->
  Map Ptr c ->
  Int ->
  UTxO TestEra
utxo noUnstaked noBase ptrMap dupFactor =
  UTxO . Map.fromAscList $
    txIns
      `zip` ( genericReplicate noUnstaked txOutUnstaked
                <> cycleTimes dupFactor stakedTxs
            )
  where
    txOutB = txOutsFromCreds $ stakeCreds noBase
    txOutP = txOutsFromPtrs $ Map.keys ptrMap
    stakedTxs = txOutP ++ txOutB

    cycleTimes _ [] = []
    cycleTimes 1 xs = xs
    cycleTimes n xs = xs ++ cycleTimes (n - 1) xs

data AggTestSetup = AggTestSetup
  { atsPtrMap :: !(Map Ptr (StakeCredential TestCrypto)),
    atsUTxO :: !(UTxO TestEra)
  }

instance NFData AggTestSetup where
  rnf (AggTestSetup p u) = seq p (seq u ())

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
sizedAggTestSetup ::
  -- | Number of unstaked addresses
  Word64 ->
  -- | Number of distinct base stake credentials
  Word64 ->
  -- | Number of distinct pointer stake credentials
  Word64 ->
  -- | Duplication factor
  Int ->
  AggTestSetup
sizedAggTestSetup noUnstaked noBase noPtr dupFactor = AggTestSetup pm ut
  where
    pm = stakePtrs $ stakeCreds noPtr
    ut = utxo noUnstaked noBase pm dupFactor

aggregateUtxoBench :: Benchmark
aggregateUtxoBench =
  bgroup
    "aggregateUtxoCoinByCredential"
    [ bgroup
        "duplication"
        [ env (pure $ sizedAggTestSetup 0 1000 0 1) $ bench "1000/0" . whnf go,
          env (pure $ sizedAggTestSetup 0 10 0 100) $ bench "10/0 * 100" . whnf go,
          env (pure $ sizedAggTestSetup 0 100 0 10) $ bench "100/0 * 10" . whnf go
        ],
      bgroup
        "ptr"
        [ env (pure $ sizedAggTestSetup 0 1000 0 1) $ bench "1000/0" . whnf go,
          env (pure $ sizedAggTestSetup 0 500 500 1) $ bench "500/500" . whnf go
        ],
      bgroup
        "utxo"
        [ env (pure $ sizedAggTestSetup 0 1000 0 1) $ bench "0 1000/0" . whnf go,
          env (pure $ sizedAggTestSetup 1000 1000 0 1) $ bench "1000 1000/0" . whnf go,
          env (pure $ sizedAggTestSetup 10000 1000 0 1) $ bench "10000 1000/0" . whnf go,
          env (pure $ sizedAggTestSetup 100000 1000 0 1) $ bench "100000 1000/0" . whnf go,
          env (pure $ sizedAggTestSetup 1000000 1000 0 1) $ bench "1000000 1000/0" . whnf go
        ],
      bgroup
        "size"
        [ env (pure $ sizedAggTestSetup 0 100 0 1) $ bench "100/0" . whnf go,
          env (pure $ sizedAggTestSetup 0 1000 0 1) $ bench "1000/0" . whnf go,
          env (pure $ sizedAggTestSetup 0 10000 0 1) $ bench "10000/0" . whnf go,
          env (pure $ sizedAggTestSetup 0 100000 0 1) $ bench "100000/0" . whnf go,
          env (pure $ sizedAggTestSetup 0 1000000 0 1) $ bench "1000000/0" . whnf go
        ],
      bgroup
        "mainnet"
        [ env (pure $ sizedAggTestSetup 4000000 100000 0 5) $ bench "current" . whnf go,
          env (pure $ sizedAggTestSetup 4000000 500000 0 1) $ bench "current no dup" . whnf go,
          env (pure $ sizedAggTestSetup 8000000 200000 0 5) $ bench "2x" . whnf go
        ]
    ]
  where
    go AggTestSetup {atsPtrMap, atsUTxO} =
      aggregateUtxoCoinByCredential atsPtrMap atsUTxO Map.empty
