{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Crypto.Hash
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.CompactAddress
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys
import Control.DeepSeq (NFData, deepseq)
import Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (foldMap')
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Unit.Strict

main :: IO ()
main = do
  let mkPayment :: Int -> Credential 'Payment StandardCrypto
      mkPayment = KeyHashObj . payAddr28
      stakeRefBase :: Int -> StakeReference StandardCrypto
      stakeRefBase = StakeRefBase . KeyHashObj . stakeAddr28
      mkAddr :: (Int -> StakeReference StandardCrypto) -> Int -> Addr StandardCrypto
      mkAddr mkStake n = Addr Mainnet (mkPayment n) (mkStake n)
      mkPtr n =
        let ni = toInteger n
         in Ptr (SlotNo (fromIntegral n)) (mkTxIxPartial ni) (mkCertIxPartial (ni + 1))
      count :: Int
      count = 10000
      seqUnit :: a -> StrictUnit
      seqUnit x = x `seq` mempty
      forcePaymentCred :: Addr StandardCrypto -> StrictUnit
      forcePaymentCred = \case
        Addr _ p _ -> p `seq` mempty
        _ -> mempty
      forceStakingCred :: Addr StandardCrypto -> StrictUnit
      forceStakingCred = \case
        Addr _ _ s -> s `deepseq` mempty
        _ -> mempty
      addrs :: (Int -> StakeReference StandardCrypto) -> [Addr StandardCrypto]
      addrs mkStake = mkAddr mkStake <$> [1 .. count]
      partialDeserializeAddr :: ByteString -> Addr StandardCrypto
      partialDeserializeAddr =
        either (error . show) id . decodeFullDecoder version "Addr" fromCborAddr . BSL.fromStrict
      partialDeserializeCompactAddr :: ByteString -> CompactAddr StandardCrypto
      partialDeserializeCompactAddr =
        either (error . show) id
          . decodeFullDecoder version "CompactAddr" fromCborCompactAddrOld
          . BSL.fromStrict
      version = maxBound :: Version
  defaultMain
    [ bgroup
        "encode"
        [ bgroup "StakeRefNull" $
            [ env (pure (addrs (const StakeRefNull))) $
                bench "old" . whnf (foldMap' (seqUnit . compactAddr))
            ],
          bgroup "StakeRefBase" $
            [ env (pure (addrs stakeRefBase)) $
                bench "old" . whnf (foldMap' (seqUnit . compactAddr))
            ],
          bgroup "StakeRefPtr" $
            [ env (pure (addrs (StakeRefPtr . mkPtr))) $
                bench "old" . whnf (foldMap' (seqUnit . compactAddr))
            ]
        ],
      bgroup
        "decode"
        [ bgroup
            "fromCompact"
            [ bgroup
                "NormalForm"
                [ benchDecode
                    "StakeRefNull"
                    deepseqUnit
                    (compactAddr <$> addrs (const StakeRefNull))
                    decompactAddrLazy
                    decompactAddr,
                  benchDecode
                    "StakeRefBase"
                    deepseqUnit
                    (compactAddr <$> addrs stakeRefBase)
                    decompactAddrLazy
                    decompactAddr,
                  benchDecode
                    "StakeRefPtr"
                    deepseqUnit
                    (compactAddr <$> addrs (StakeRefPtr . mkPtr))
                    decompactAddrLazy
                    decompactAddr
                ],
              bgroup
                "PaymentCredential"
                [ benchDecode
                    "StakeRefNull"
                    forcePaymentCred
                    (compactAddr <$> addrs (const StakeRefNull))
                    decompactAddrLazy
                    decompactAddr,
                  benchDecode
                    "StakeRefBase"
                    forcePaymentCred
                    (compactAddr <$> addrs stakeRefBase)
                    decompactAddrLazy
                    decompactAddr,
                  benchDecode
                    "StakeRefPtr"
                    forcePaymentCred
                    (compactAddr <$> addrs (StakeRefPtr . mkPtr))
                    decompactAddrLazy
                    decompactAddr
                ],
              bgroup
                "StakingCredential"
                [ benchDecode
                    "StakeRefNull"
                    forceStakingCred
                    (compactAddr <$> addrs (const StakeRefNull))
                    decompactAddrLazy
                    decompactAddr,
                  benchDecode
                    "StakeRefBase"
                    forceStakingCred
                    (compactAddr <$> addrs stakeRefBase)
                    decompactAddrLazy
                    decompactAddr,
                  benchDecode
                    "StakeRefPtr"
                    forceStakingCred
                    (compactAddr <$> addrs (StakeRefPtr . mkPtr))
                    decompactAddrLazy
                    decompactAddr
                ]
            ],
          bgroup
            "fromCBOR-Addr"
            [ benchDecode
                "StakeRefNull"
                forcePaymentCred
                (serialize' version <$> addrs (const StakeRefNull))
                (unsafeDeserialize' version)
                partialDeserializeAddr,
              benchDecode
                "StakeRefBase"
                forcePaymentCred
                (serialize' version <$> addrs stakeRefBase)
                (unsafeDeserialize' version)
                partialDeserializeAddr,
              benchDecode
                "StakeRefPtr"
                forcePaymentCred
                (serialize' version <$> addrs (StakeRefPtr . mkPtr))
                (unsafeDeserialize' version)
                partialDeserializeAddr
            ],
          bgroup
            "fromCBOR-CompactAddr"
            [ benchDecode
                "StakeRefNull"
                seqUnit
                (serialize' version <$> addrs (const StakeRefNull))
                partialDeserializeCompactAddr
                (unsafeDeserialize' version),
              benchDecode
                "StakeRefBase"
                seqUnit
                (serialize' version <$> addrs stakeRefBase)
                partialDeserializeCompactAddr
                (unsafeDeserialize' version),
              benchDecode
                "StakeRefPtr"
                seqUnit
                (serialize' version <$> addrs (StakeRefPtr . mkPtr))
                partialDeserializeCompactAddr
                (unsafeDeserialize' version)
            ]
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
      [ bench "old" $ whnf (foldMap' (forceResult . oldDecode)) cas,
        bench "new" $ whnf (foldMap' (forceResult . newDecode)) cas
      ]

deepseqUnit :: NFData a => a -> StrictUnit
deepseqUnit x = x `deepseq` mempty

textDigits :: Int -> T.Text
textDigits n = let i = n `mod` 10 in T.pack (take 6 (cycle (show i)))

payAddr28 :: Int -> KeyHash 'Payment StandardCrypto
payAddr28 n =
  KeyHash $
    fromMaybe "Unexpected PayAddr28" $
      hashFromTextAsHex $
        textDigits n <> "0405060708090a0b0c0d0e0f12131415161718191a1b1c1d1e"

stakeAddr28 :: Int -> KeyHash 'Staking StandardCrypto
stakeAddr28 n =
  KeyHash $
    fromMaybe "Unexpected StakeAddr28" $
      hashFromTextAsHex $
        textDigits n <> "2122232425262728292a2b2c2d2e2f32333435363738393a3b"
