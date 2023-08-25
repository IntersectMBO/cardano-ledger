{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Ledger.Address (Addr, decodeAddrEither, serialiseAddr)
import Cardano.Ledger.Crypto (StandardCrypto)

import Control.Monad (replicateM)

import Criterion (Benchmark, bench, env, nf)
import Criterion.Main (defaultMain)

import Data.ByteString.Char8 (ByteString)
import Data.Either (lefts)

import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck (arbitrary, generate)

main :: IO ()
main = do
  defaultMain $
    map (\c -> env (generateAddrAsBytestring c) decodeAddrBench) (map (* 500) [1 .. 2])
  where
    decodeAddrBench :: [ByteString] -> Benchmark
    decodeAddrBench xs =
      bench ("decodeAddr (" ++ show (length xs) ++ ")") (nf tryDecodeAddr xs)

-- -------------------------------------------------------------------------------------------------

generateAddrAsBytestring :: Int -> IO [ByteString]
generateAddrAsBytestring count =
  replicateM count (serialiseAddr <$> genAddr)
  where
    genAddr :: IO (Addr StandardCrypto)
    genAddr = generate arbitrary

tryDecodeAddr :: [ByteString] -> ()
tryDecodeAddr xs =
  case lefts $ map decode xs of
    [] -> ()
    ys -> error $ "tryDecodeAddr: " ++ show ys
  where
    decode :: ByteString -> Either String (Addr StandardCrypto)
    decode = decodeAddrEither
