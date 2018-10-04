{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module contains custom @Format@s for use with the `formatting` library
--
--   @Format@s allow a type-safe version of string formatting. For example we
--   might have:
--
--   > myFormat :: Format r (Text -> Int -> r)
--   > myFormat = "Person's name is " % text % ", age is " % hex
--
--   The type parameter @r@ is the polymorphic return type. `formatting`
--   provides a number of functions to run the format. For example, we could use
--   @sformat :: Format Text a -> a@ to run @myFormat@, which would give:
--
--   > sformat myFormat :: Text -> Int -> Text
--
--   This is now a simple function, which will return a formatted strict @Text@.
--
--   `formatting` also provides a @Buildable a@ type-class for values that can
--   be turned into a @Text@ @Builder@. It provides @build :: a -> Builder@.
--   There is then a formatter @build :: Buildable a => Format r (a -> r)@.
--   So, to get a @Text@ from any @Buildable@ value we can simply call
--   @sformat build value@, and we can compose this @build@ in larger @Format@s.

module Cardano.Prelude.Formatting
       ( base16Builder
       , base16F
       , pairF
       , pairBuilder
       , listJson
       , listJsonIndent
       , mapJson
       ) where

import           Prelude

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (Builder, fromLazyText, fromString)
import           Formatting (Format, bprint, later, (%))
import qualified Formatting as F (build)
import           Formatting.Buildable (Buildable (build))
import           GHC.Exts (IsList (toList), Item)


--------------------------------------------------------------------------------
-- Base16
--------------------------------------------------------------------------------

-- | A @Builder@ for a @ByteString@ that performs base 16 encoding
base16Builder :: ByteString -> Builder
base16Builder = fromString . BS.unpack . B16.encode

-- | A @Format@ for a @ByteString@ that performs base 16 encoding
base16F :: Format r (ByteString -> r)
base16F = later base16Builder


--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

-- | A @Builder@ for a pair of @Buildable@ values @(a, b)@
pairBuilder :: (Buildable a, Buildable b) => (a, b) -> Builder
pairBuilder (a, b) = bprint ("(" % F.build % ", " % F.build % ")") a b

-- | A @Format@ for a pair of @Buildable@ values @(a, b)@
pairF :: (Buildable a, Buildable b) => Format r ((a, b) -> r)
pairF = later pairBuilder

-- | A @Builder@ for @Foldable@ containers of @Buildable@ values that surrounds
--   values using @prefix@ and @suffix@, and splits them using @delimiter@
foldableBuilder
  :: (Foldable t, Buildable a)
  => Builder
  -> Builder
  -> Builder
  -> t a
  -> Builder
foldableBuilder prefix delimiter suffix as = mconcat
  [prefix, mconcat builders, suffix]
 where
  builders = foldr appendBuilder [] as
  appendBuilder a [] = [build a]
  appendBuilder a bs = build a : delimiter : bs

-- | A @Builder@ for @Foldable@ containers that outputs a JSON-style list
--
--   > "[111, ololo, blablabla]"
listBuilderJson :: (Foldable t, Buildable a) => t a -> Builder
listBuilderJson = foldableBuilder "[" ", " "]"

-- | A @Format@ for @Foldable@ containers that outputs a JSON-style list
listJson :: (Foldable t, Buildable a) => Format r (t a -> r)
listJson = later listBuilderJson

-- | A @Builder@ similar to @listBuilderJson@ that prints each value on a new
--   line with @indent@ spaces of indentation
listBuilderJsonIndent :: (Foldable t, Buildable a) => Word -> t a -> Builder
listBuilderJsonIndent indent as
  | null as   = "[]"
  | otherwise = foldableBuilder ("[\n" <> spaces) delimiter "\n]" as
 where
  spaces    = fromLazyText $ LT.replicate (fromIntegral indent) " "
  delimiter = ",\n" <> spaces

-- | A @Format@ similar to @listJson@ that prints each value on a new line with
--   @indent@ spaces of indentation
listJsonIndent :: (Foldable t, Buildable a) => Word -> Format r (t a -> r)
listJsonIndent = later . listBuilderJsonIndent

-- | A @Builder@ for @IsList@ containers of @Buildable@ key-value pairs that
--   outputs a JSON-style colon-separated map
mapBuilderJson
  :: (IsList t, Item t ~ (k, v), Buildable k, Buildable v) => t -> Builder
mapBuilderJson =
  foldableBuilder "{" ", " "}"
    . map (uncurry $ bprint (F.build % ": " % F.build))
    . toList

-- | A @Format@ for @IsList@ containers of @Buildable@ key-value pairs that
--   outputs a JSON-style colon-separated map
mapJson
  :: (IsList t, Item t ~ (k, v), Buildable k, Buildable v) => Format r (t -> r)
mapJson = later mapBuilderJson
