{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Helpers for round-trip testing datatypes

module Test.Cardano.Prelude.Tripping
       ( runTests
       , discoverRoundTrip
       , roundTripsAesonShow
       , roundTripsAesonBuildable
       , trippingBuildable
       ) where

import           Cardano.Prelude

import           Data.Aeson (FromJSON, ToJSON, decode, encode)
import           Data.Text.Internal.Builder (toLazyText)
import           Formatting.Buildable (Buildable (..))
import           Hedgehog (Group, MonadTest, discoverPrefix, success, tripping)
import           Hedgehog.Internal.Property (Diff (..), failWith)
import           Hedgehog.Internal.Show (valueDiff)
import           Hedgehog.Internal.TH (TExpQ)
import qualified Prelude
import           System.IO (hSetEncoding, stderr, stdout, utf8)
import           Text.Show.Pretty (Value (..), parseValue)


discoverRoundTrip :: TExpQ Group
discoverRoundTrip = discoverPrefix "roundTrip"

roundTripsAesonShow
  :: (Eq a, MonadTest m, ToJSON a, FromJSON a, Show a) => a -> m ()
roundTripsAesonShow a = tripping a encode decode

-- | Round trip any `a` with both `ToJSON` and `FromJSON` instances
roundTripsAesonBuildable
  :: (Eq a, MonadTest m, ToJSON a, FromJSON a, Buildable a) => a -> m ()
roundTripsAesonBuildable a = trippingBuildable a encode decode

runTests :: [IO Bool] -> IO ()
runTests tests' = do
  -- ensure UTF-8. As that's what hedgehog needs.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  result <- and <$> sequence tests'
  unless result exitFailure

-- | Round trip using given encode and decode functions for types with a
--   'Buildable' instance
trippingBuildable
  :: forall f a b m
   . ( HasCallStack
     , Buildable (f a)
     , Eq (f a)
     , Show b
     , Applicative f
     , MonadTest m
     )
  => a
  -> (a -> b)
  -> (b -> f a)
  -> m ()
trippingBuildable x enc dec =
  let
    mx :: f a
    mx = pure x
    i  = enc x
    my = dec i
  in if mx == my
    then success
    else case valueDiff <$> buildValue mx <*> buildValue my of
      Nothing -> withFrozenCallStack $ failWith Nothing $ Prelude.unlines
        [ "━━━ Original ━━━"
        , show $ buildValue mx
        , "━━━ Intermediate ━━━"
        , show i
        , "━━━ Roundtrip ━━━"
        , show $ buildValue my
        ]

      Just dif ->
        withFrozenCallStack
          $ failWith
              (Just $ Diff "━━━ " "- Original" "/" "+ Roundtrip" " ━━━" dif)
          $ Prelude.unlines ["━━━ Intermediate ━━━", show i]

instance (Buildable e, Buildable a) => Buildable (Either e a) where
    build (Left e)  = build e
    build (Right a) = build a

buildValue :: Buildable a => a -> Maybe Value
buildValue = parseValue . toS . toLazyText . build
