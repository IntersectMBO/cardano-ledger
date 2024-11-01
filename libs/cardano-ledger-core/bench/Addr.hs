import Cardano.Ledger.Address (Addr, decodeAddrEither, serialiseAddr)
import Control.Monad (replicateM)
import Criterion (Benchmark, bench, env, nf)
import Criterion.Main (defaultMain)
import Data.ByteString.Char8 (ByteString)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (QCGen, mkQCGen)

main :: IO ()
main = do
  defaultMain $
    map (\c -> env (generateAddrAsBytestring qcGen c) decodeAddrBench) [500, 1000]
  where
    qcGen = mkQCGen 2023
    decodeAddrBench :: [ByteString] -> Benchmark
    decodeAddrBench xs =
      bench ("decodeAddr (" ++ show (length xs) ++ ")") (nf tryDecodeAddr xs)

-- -------------------------------------------------------------------------------------------------

generateAddrAsBytestring :: QCGen -> Int -> IO [ByteString]
generateAddrAsBytestring qcGen count =
  pure $ unGen (replicateM count (serialiseAddr <$> genAddr)) qcGen 30
  where
    genAddr :: Gen Addr
    genAddr = arbitrary

tryDecodeAddr :: [ByteString] -> [Addr]
tryDecodeAddr xs =
  case mapM decode xs of
    Right addrs -> addrs
    Left err -> error $ "tryDecodeAddr: " ++ show err
  where
    decode :: ByteString -> Either String Addr
    decode = decodeAddrEither
