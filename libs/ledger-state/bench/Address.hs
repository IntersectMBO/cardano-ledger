{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- import Cardano.Binary

import Cardano.Crypto.Hash
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CompactAddress as CompactAddress
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys
import Cardano.Slotting.Slot
import Control.DeepSeq (NFData, deepseq)
import Criterion.Main
import Data.Foldable (foldMap')
import Data.Maybe (fromJust)
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
         in Ptr (SlotNo (fromIntegral n)) (mkTxIxPartial ni) (mkCertIxPartial ni)
      count :: Int
      count = 10000
      seqUnit :: a -> StrictUnit
      seqUnit x = x `seq` mempty
      deepseqUnit :: NFData a => a -> StrictUnit
      deepseqUnit x = x `deepseq` mempty
      addrs :: (Int -> StakeReference StandardCrypto) -> [Addr StandardCrypto]
      addrs mkStake = mkAddr mkStake <$> [1 .. count]
      {-# NOINLINE addrs #-}
      compactAddrs :: (Int -> StakeReference StandardCrypto) -> [CompactAddr StandardCrypto]
      compactAddrs = map CompactAddress.compactAddr . addrs
      {-# NOINLINE compactAddrs #-}
  defaultMain
    [ bgroup
        "toCompact"
        [ bgroup "StakeRefNull" $
            [ env (pure (addrs (const StakeRefNull))) $
                bench "old" . whnf (foldMap' (seqUnit . CompactAddress.compactAddr))
            ],
          bgroup "StakeRefBase" $
            [ env (pure (addrs stakeRefBase)) $
                bench "old" . whnf (foldMap' (seqUnit . CompactAddress.compactAddr))
            ],
          bgroup "StakeRefPtr" $
            [ env (pure (addrs (StakeRefPtr . mkPtr))) $
                bench "old" . whnf (foldMap' (seqUnit . CompactAddress.compactAddr))
            ]
        ],
      bgroup
        "fromCompact"
        [ bgroup "StakeRefNull" $
            [ env (pure (compactAddrs (const StakeRefNull))) $
                bench "old"
                  . whnf
                    (foldMap' (deepseqUnit @(Addr StandardCrypto) . CompactAddress.decompactAddr))
            ],
          bgroup "StakeRefBase" $
            [ env (pure (compactAddrs stakeRefBase)) $
                bench "old"
                  . whnf
                    (foldMap' (deepseqUnit @(Addr StandardCrypto) . CompactAddress.decompactAddr))
            ],
          bgroup "StakeRefPtr" $
            [ env (pure (compactAddrs (StakeRefPtr . mkPtr))) $
                bench "old"
                  . whnf
                    (foldMap' (deepseqUnit @(Addr StandardCrypto) . CompactAddress.decompactAddr))
            ]
        ]
    ]

textDigits :: Int -> T.Text
textDigits n = let i = n `mod` 10 in T.pack (take 6 (cycle (show i)))

payAddr28 :: Int -> KeyHash 'Payment StandardCrypto
payAddr28 n =
  KeyHash $
    fromJust $
      hashFromTextAsHex $
        textDigits n <> "0405060708090a0b0c0d0e0f12131415161718191a1b1c1d1e"

stakeAddr28 :: Int -> KeyHash 'Staking StandardCrypto
stakeAddr28 n =
  KeyHash $
    fromJust $
      hashFromTextAsHex $
        textDigits n <> "2122232425262728292a2b2c2d2e2f32333435363738393a3b"
