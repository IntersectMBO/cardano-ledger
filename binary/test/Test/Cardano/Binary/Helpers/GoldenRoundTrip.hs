{-# LANGUAGE RankNTypes #-}

-- | Golden and round-trip testing of 'Bi' instances

module Test.Cardano.Binary.Helpers.GoldenRoundTrip
  ( goldenTestBi
  , roundTripsBiShow
  , roundTripsBiBuildable
  , compareHexDump
  , deprecatedGoldenDecode
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Codec.CBOR.Decoding as D
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import Formatting.Buildable (Buildable(..))
import Hedgehog
  (MonadTest, Property, eval, property, success, tripping, withTests, (===))
import Hedgehog.Internal.Property (failWith)
import Hedgehog.Internal.Show
  (LineDiff, lineDiff, mkValue, renderLineDiff, showPretty)

import Cardano.Binary.Class (Bi(..), decodeFull, decodeFullDecoder, serialize)
import qualified Prelude
import Text.Show.Pretty (Value(..))


type HexDump = LByteString

type HexDumpDiff = [LineDiff]

renderHexDumpDiff :: HexDumpDiff -> [Char]
renderHexDumpDiff = Prelude.unlines . fmap renderLineDiff

-- | Diff two 'HexDump's by comparing lines pairwise
hexDumpDiff :: HexDump -> HexDump -> Maybe HexDumpDiff
hexDumpDiff x y = do
  xs <- sequence (mkValue <$> BS.lines x)
  ys <- sequence (mkValue <$> BS.lines y)
  pure $ concatMap (uncurry lineDiff) $ zipWithPadding
    (String "")
    (String "")
    xs
    ys

zipWithPadding :: a -> b -> [a] -> [b] -> [(a, b)]
zipWithPadding a b (x : xs) (y : ys) = (x, y) : zipWithPadding a b xs ys
zipWithPadding a _ []       ys       = zip (repeat a) ys
zipWithPadding _ b xs       []       = zip xs (repeat b)

-- | A custom version of '(===)' for 'HexDump's to get prettier diffs
compareHexDump :: (MonadTest m, HasCallStack) => HexDump -> HexDump -> m ()
compareHexDump x y = do
  ok <- withFrozenCallStack $ eval (x == y)
  if ok then success else withFrozenCallStack $ failHexDumpDiff x y

-- | Fail with a nice line diff of the two HexDumps
failHexDumpDiff :: (MonadTest m, HasCallStack) => HexDump -> HexDump -> m ()
failHexDumpDiff x y = case hexDumpDiff x y of
  Nothing -> withFrozenCallStack $ failWith Nothing $ Prelude.unlines
    ["━━━ Not Equal ━━━", showPretty x, showPretty y]
  Just dif -> withFrozenCallStack $ failWith Nothing $ renderHexDumpDiff dif

goldenTestBi :: (Bi a, Eq a, Show a, HasCallStack) => a -> FilePath -> Property
goldenTestBi x path = withFrozenCallStack $ do
  let bs' = encodeWithIndex . serialize $ x
  withTests 1 . property $ do
    bs <- liftIO $ BS.readFile path
    let target = decodeBase16 bs
    compareHexDump bs bs'
    fmap decodeFull target === Just (Right x)


-- | Round trip test a value (any instance of both the 'Bi' and 'Show' classes)
--   by serializing it to a ByteString and back again and that also has a 'Show'
--   instance. If the 'a' type has both 'Show' and 'Buildable' instances, it's
--   best to use this version.
roundTripsBiShow :: (Bi a, Eq a, MonadTest m, Show a) => a -> m ()
roundTripsBiShow x = tripping x serialize decodeFull

-- | Round trip (via ByteString) any instance of the 'Bi' class
--   that also has a 'Buildable' instance.
roundTripsBiBuildable :: (Bi a, Eq a, MonadTest m, Buildable a) => a -> m ()
roundTripsBiBuildable a = trippingBuildable a serialize decodeFull

deprecatedGoldenDecode
  :: HasCallStack => Text -> (forall s . D.Decoder s ()) -> FilePath -> Property
deprecatedGoldenDecode lbl decoder path =
  withFrozenCallStack $ withTests 1 . property $ do
    bs <- decodeBase16 <$> liftIO (BS.readFile path)
    fmap (decodeFullDecoder lbl decoder) bs === Just (Right ())
