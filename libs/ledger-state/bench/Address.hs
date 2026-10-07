{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Crypto.Hash
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Control.DeepSeq (NFData, deepseq)
import Criterion.Main
import Data.Foldable (foldMap')
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Unit.Strict
import Test.Cardano.Ledger.Core.Address (decompactAddrOldLazy)

main :: IO ()
main = do
  let mkPayment :: Int -> Credential Payment
      mkPayment = KeyHashObj . payAddr28
      stakeRefBase :: Int -> StakeReference
      stakeRefBase = StakeRefBase . KeyHashObj . stakeAddr28
      mkAddr :: (Int -> StakeReference) -> Int -> Addr
      mkAddr mkStake n = Addr Mainnet (mkPayment n) (mkStake n)
      mkPtr n =
        let ni = toInteger n
         in Ptr (SlotNo32 (fromIntegral n)) (mkTxIxPartial ni) (mkCertIxPartial (ni + 1))
      count :: Int
      count = 10000
      seqUnit :: a -> StrictUnit
      seqUnit x = x `seq` mempty
      forcePaymentCred :: Addr -> StrictUnit
      forcePaymentCred addr = case shelleyAddressView addr of
        Just (_, _, p, _) -> p `seq` mempty
        _ -> mempty
      forceStakingCred :: Addr -> StrictUnit
      forceStakingCred addr = case shelleyAddressView addr of
        Just (_, _, _, s) -> s `deepseq` mempty
        _ -> mempty
      addrs :: (Int -> StakeReference) -> [Addr]
      addrs mkStake = mkAddr mkStake <$> [1 .. count]
      partialDeserializeAddr :: ByteString -> Addr
      partialDeserializeAddr =
        either (error . show) id . decodeFullDecoder' version "Addr" fromCborAddr
      version = maxBound :: Version
  defaultMain
    [ bgroup
        "protection"
        [ protectionBench "ordinary/base" (addrs stakeRefBase)
        , protectionBench "protected/base" (map (either error id . protectAddress) (addrs stakeRefBase))
        , protectionBench "ordinary/enterprise" (addrs (const StakeRefNull))
        , protectionBench
            "protected/enterprise"
            (map (either error id . protectAddress) (addrs (const StakeRefNull)))
        ]
    , bgroup
        "encode"
        [ bgroup "StakeRefNull" $
            [ env (pure (addrs (const StakeRefNull))) $
                bench "old" . whnf (foldMap' (seqUnit . compactAddr))
            ]
        , bgroup "StakeRefBase" $
            [ env (pure (addrs stakeRefBase)) $
                bench "old" . whnf (foldMap' (seqUnit . compactAddr))
            ]
        , bgroup "StakeRefPtr" $
            [ env (pure (addrs (StakeRefPtr . mkPtr))) $
                bench "old" . whnf (foldMap' (seqUnit . compactAddr))
            ]
        ]
    , bgroup
        "decode"
        [ bgroup
            "fromCompact"
            [ bgroup
                "NormalForm"
                [ benchDecode
                    "StakeRefNull"
                    deepseqUnit
                    (compactAddr <$> addrs (const StakeRefNull))
                    decompactAddrOldLazy
                    decompactAddr
                , benchDecode
                    "StakeRefBase"
                    deepseqUnit
                    (compactAddr <$> addrs stakeRefBase)
                    decompactAddrOldLazy
                    decompactAddr
                , benchDecode
                    "StakeRefPtr"
                    deepseqUnit
                    (compactAddr <$> addrs (StakeRefPtr . mkPtr))
                    decompactAddrOldLazy
                    decompactAddr
                ]
            , bgroup
                "PaymentCredential"
                [ benchDecode
                    "StakeRefNull"
                    forcePaymentCred
                    (compactAddr <$> addrs (const StakeRefNull))
                    decompactAddrOldLazy
                    decompactAddr
                , benchDecode
                    "StakeRefBase"
                    forcePaymentCred
                    (compactAddr <$> addrs stakeRefBase)
                    decompactAddrOldLazy
                    decompactAddr
                , benchDecode
                    "StakeRefPtr"
                    forcePaymentCred
                    (compactAddr <$> addrs (StakeRefPtr . mkPtr))
                    decompactAddrOldLazy
                    decompactAddr
                ]
            , bgroup
                "StakingCredential"
                [ benchDecode
                    "StakeRefNull"
                    forceStakingCred
                    (compactAddr <$> addrs (const StakeRefNull))
                    decompactAddrOldLazy
                    decompactAddr
                , benchDecode
                    "StakeRefBase"
                    forceStakingCred
                    (compactAddr <$> addrs stakeRefBase)
                    decompactAddrOldLazy
                    decompactAddr
                , benchDecode
                    "StakeRefPtr"
                    forceStakingCred
                    (compactAddr <$> addrs (StakeRefPtr . mkPtr))
                    decompactAddrOldLazy
                    decompactAddr
                ]
            ]
        , bgroup
            "decCBOR-Addr"
            [ benchDecode
                "StakeRefNull"
                forcePaymentCred
                (serialize' version <$> addrs (const StakeRefNull))
                (unsafeDeserialize' version)
                partialDeserializeAddr
            , benchDecode
                "StakeRefBase"
                forcePaymentCred
                (serialize' version <$> addrs stakeRefBase)
                (unsafeDeserialize' version)
                partialDeserializeAddr
            , benchDecode
                "StakeRefPtr"
                forcePaymentCred
                (serialize' version <$> addrs (StakeRefPtr . mkPtr))
                (unsafeDeserialize' version)
                partialDeserializeAddr
            ]
        ]
    ]

-- Current-format workloads are identical apart from protection. Historical
-- old/new decoder comparisons above remain ordinary-only and retain their names.
protectionBench :: String -> [Addr] -> Benchmark
protectionBench name addresses = env (pure addresses) $ \as ->
  bgroup
    name
    [ bench "encode-raw" $ nf (map serialiseAddr) as
    , bench "compact" $ nf (map compactAddr) as
    , env (pure (map serialiseAddr as)) $ \bytes ->
        bench "decode-raw" $ nf (map (either error id . decodeAddrEither)) bytes
    , env (pure (map compactAddr as)) $ \compact ->
        bgroup
          "decompact"
          [ bench "full" $ nf (map decompactAddr) compact
          , bench "network" $ nf (map (getNetwork . decompactAddr)) compact
          , bench "payment" $
              nf (map (fmap (\(_, _, pc, _) -> pc) . shelleyAddressView . decompactAddr)) compact
          , bench "stake" $ nf (map (fmap (\(_, _, _, sr) -> sr) . shelleyAddressView . decompactAddr)) compact
          ]
    ]

benchDecode ::
  NFData a =>
  String ->
  (b -> StrictUnit) ->
  [a] ->
  (a -> b) ->
  (a -> b) ->
  Benchmark
benchDecode benchName forceResult as oldDecode newDecode =
  env (pure as) $ \cas ->
    bgroup benchName $
      [ bench "old" $ whnf (foldMap' (forceResult . oldDecode)) cas
      , bench "new" $ whnf (foldMap' (forceResult . newDecode)) cas
      ]

deepseqUnit :: NFData a => a -> StrictUnit
deepseqUnit x = x `deepseq` mempty

textDigits :: Int -> T.Text
textDigits n = let i = n `mod` 10 in T.pack (take 6 (cycle (show i)))

payAddr28 :: Int -> KeyHash Payment
payAddr28 n =
  KeyHash $
    fromMaybe "Unexpected PayAddr28" $
      hashFromTextAsHex $
        textDigits n <> "0405060708090a0b0c0d0e0f12131415161718191a1b1c1d1e"

stakeAddr28 :: Int -> KeyHash Staking
stakeAddr28 n =
  KeyHash $
    fromMaybe "Unexpected StakeAddr28" $
      hashFromTextAsHex $
        textDigits n <> "2122232425262728292a2b2c2d2e2f32333435363738393a3b"
