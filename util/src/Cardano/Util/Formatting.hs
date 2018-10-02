{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Cardano.Util.Formatting
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
import           Formatting.Buildable (Buildable (..))
import           GHC.Exts (IsList (..), Item)


--------------------------------------------------------------------------------
-- Base16
--------------------------------------------------------------------------------

base16Builder :: ByteString -> Builder
base16Builder = fromString . BS.unpack . B16.encode

base16F :: Format r (ByteString -> r)
base16F = later base16Builder

--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

pairF :: (Buildable a, Buildable b) => Format r ((a, b) -> r)
pairF = later pairBuilder

listJson :: (Foldable t, Buildable a) => Format r (t a -> r)
listJson = later listBuilderJSON

listJsonIndent :: (Foldable t, Buildable a) => Word -> Format r (t a -> r)
listJsonIndent = later . listBuilderJSONIndent

mapJson
    :: (IsList t, Item t ~ (k, v), Buildable k, Buildable v)
    => Format r (t -> r)
mapJson = later mapBuilderJson

-- | Prints pair (a, b) like "(a, b)"
pairBuilder :: (Buildable a, Buildable b) => (a, b) -> Builder
pairBuilder (a, b) = bprint ("(" % F.build % ", " % F.build % ")") a b

-- | Generic list builder. Prints prefix, then values separated by delimiter and finally suffix
listBuilder
    :: ( Buildable prefix
       , Buildable delimiter
       , Buildable suffix
       , Foldable t
       , Buildable a
       )
    => prefix
    -> delimiter
    -> suffix
    -> t a
    -> Builder
listBuilder prefix delimiter suffix as = mconcat
    [build prefix, mconcat builders, build suffix]
  where
    builders = foldr appendBuilder [] as
    appendBuilder a [] = [build a]
    appendBuilder a bs = build a : build delimiter : bs

-- | This function helps to deduce type arising from string literal
_listBuilder
    :: (Foldable t, Buildable a)
    => Builder
    -> Builder
    -> Builder
    -> t a
    -> Builder
_listBuilder = listBuilder

-- | Prints values in JSON-style (e. g. `[111, ololo, blablabla]`)
listBuilderJSON :: (Foldable t, Buildable a) => t a -> Builder
listBuilderJSON = _listBuilder "[" ", " "]"

-- | Like listBuilderJSON, but prints each value on a new line with indentation
listBuilderJSONIndent :: (Foldable t, Buildable a) => Word -> t a -> Builder
listBuilderJSONIndent indent as
    | null as = "[]"
    | otherwise = _listBuilder
        ("[\n" <> spaces)
        delimiter
        "\n]"
        as
  where
    spaces    = fromLazyText $ LT.replicate (fromIntegral indent) " "
    delimiter = ",\n" <> spaces

mapBuilderJson
    :: (IsList t, Item t ~ (k, v), Buildable k, Buildable v) => t -> Builder
mapBuilderJson =
    _listBuilder "{" ", " "}"
        . map (uncurry $ bprint (F.build % ": " % F.build))
        . toList
