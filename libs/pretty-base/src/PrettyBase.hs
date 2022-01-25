{-# LANGUAGE OverloadedStrings #-}

module PrettyBase where

import Codec.Binary.Bech32
import qualified Data.ByteString as Long (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString, toStrict)
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.Compact.VMap as VMap
import Data.Fixed (Fixed, HasResolution)
import qualified Data.Map.Strict as Map (Map, toList)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set, toList)
import Data.Text (Text)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Prettyprinter
import Prettyprinter.Internal (Doc (Empty))
import Prettyprinter.Util (putDocW)

-- =====================================================================================================
-- HELPER FUNCTIONS
-- =====================================================================================================

-- ======================
-- Named pretty printers for some simple types

ppString :: String -> Doc a
ppString = pretty

ppDouble :: Double -> Doc a
ppDouble = viaShow

ppInteger :: Integer -> Doc a
ppInteger = viaShow

ppRational :: Rational -> Doc a
ppRational = viaShow

ppFloat :: Float -> Doc a
ppFloat = viaShow

ppNatural :: Natural -> Doc a
ppNatural = viaShow

ppWord64 :: Word64 -> Doc a
ppWord64 = viaShow

ppWord32 :: Word32 -> Doc a
ppWord32 = viaShow

ppWord8 :: Word8 -> Doc a
ppWord8 = viaShow

ppWord16 :: Word16 -> Doc a
ppWord16 = viaShow

ppPair :: (t1 -> PDoc) -> (t2 -> PDoc) -> (t1, t2) -> PDoc
ppPair pp1 pp2 (x, y) = ppSexp' mempty [pp1 x, pp2 y]

-- ppSignedDSIGN :: SignedDSIGN a b -> Doc ann
ppSignedDSIGN :: Show a => a -> PDoc
ppSignedDSIGN x = reAnnotate (Width 5 :) (viaShow x)

ppBool :: Bool -> Doc a
ppBool = viaShow

ppInt :: Int -> Doc a
ppInt = viaShow

ppStrictMaybe :: (x -> Doc ann) -> StrictMaybe x -> Doc ann
ppStrictMaybe _ SNothing = text "?-"
ppStrictMaybe p (SJust x) = text "?" <> p x

ppFixedPoint :: HasResolution i => Fixed i -> Doc a
ppFixedPoint = viaShow

-- =========================
-- operations for pretty printing

isEmpty :: Doc ann -> Bool
isEmpty Empty = True
isEmpty _ = False

putDoc :: Doc ann -> IO ()
putDoc = putDocW 80

newtype PrettyAnn = Width Int

type Ann = [PrettyAnn]

type PDoc = Doc Ann

text :: Text -> Doc ann
text = pretty

-- ======================
-- Byte Strings in Bech32 format

long_bech32 :: Long.ByteString -> Text
long_bech32 x =
  case humanReadablePartFromText "*" of
    Right human ->
      case encode human (dataPartFromBytes x) of
        Right ans -> ans
        Left _ -> "bech32Error"
    Left _ -> "bech32Error"

lazy_bech32 :: Lazy.ByteString -> Text
lazy_bech32 x =
  case humanReadablePartFromText "*" of
    Right human ->
      case encode human (dataPartFromBytes (Lazy.toStrict x)) of
        Right ans -> ans
        Left _ -> "bech32Error"
    Left _ -> "bech32Error"

ppLong :: Long.ByteString -> PDoc
ppLong x = text (long_bech32 x)

ppLazy :: Lazy.ByteString -> PDoc
ppLazy x = text (lazy_bech32 x)

-- ================================
-- Combinators for common patterns of layout

-- | x == y
equate :: Doc a -> Doc a -> Doc a
equate x y = group (flatAlt (hang 2 (sep [x <+> text "=", y])) (hsep [x, text "=", y]))

-- | x -> y
arrow :: (Doc a, Doc a) -> Doc a
arrow (x, y) = group (flatAlt (hang 2 (sep [x <+> text "->", y])) (hsep [x, text "->", y]))

-- | ppSexp x [w,y,z] --> (x w y z)
ppSexp :: Text -> [PDoc] -> PDoc
ppSexp con = ppSexp' (text con)

ppSexp' :: PDoc -> [PDoc] -> PDoc
ppSexp' con fields =
  group $
    flatAlt
      (hang 2 (encloseSep lparen rparen space docs))
      (encloseSep lparen rparen space docs)
  where
    docs = if isEmpty con then fields else con : fields

-- | ppRecord name [("a",x),("b",y),("c",z)] --> name { a = x, b = y, c = z }
ppRecord :: Text -> [(Text, PDoc)] -> PDoc
ppRecord con = ppRecord' (text con)

ppRecord' :: PDoc -> [(Text, PDoc)] -> PDoc
ppRecord' con fields =
  group $
    flatAlt
      (hang 1 (vcat [con, puncLeft lbrace (map (\(x, y) -> equate (text x) y) fields) comma rbrace]))
      (con <> encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) (map (\(x, y) -> equate (text x) y) fields))

-- | Vertical layout with commas aligned on the left hand side
puncLeft :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann -> Doc ann
puncLeft open [] _ close = hsep [open, close]
puncLeft open [x] _ close = hsep [open, x, close]
puncLeft open (x : xs) coma close = align (sep ((open <+> x) : help xs))
  where
    help [] = mempty
    help [y] = [hsep [coma, y, close]]
    help (y : ys) = (coma <+> y) : help ys

ppSet :: (x -> Doc ann) -> Set x -> Doc ann
ppSet p xs = encloseSep lbrace rbrace comma (map p (toList xs))

ppList :: (x -> Doc ann) -> [x] -> Doc ann
ppList p xs =
  group $
    flatAlt
      (puncLeft lbracket (map p xs) comma rbracket)
      (encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (map p xs))

ppStrictSeq :: (a -> Doc ann) -> StrictSeq a -> Doc ann
ppStrictSeq p xs = ppList p (foldr (:) [] xs)

ppMaybe :: (x -> Doc ann) -> Maybe x -> Doc ann
ppMaybe _ Nothing = text "?-"
ppMaybe p (Just x) = text "?" <> p x

ppAssocList :: PDoc -> (k -> PDoc) -> (v -> PDoc) -> [(k, v)] -> PDoc
ppAssocList name kf vf xs =
  let docs = fmap (\(k, v) -> arrow (kf k, vf v)) xs
      vertical =
        if isEmpty name
          then hang 1 (puncLeft lbrace docs comma rbrace)
          else hang 1 (vcat [name, puncLeft lbrace docs comma rbrace])
   in group $
        flatAlt
          vertical
          (name <> encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) docs)

ppSplitMap :: (k -> PDoc) -> (v -> PDoc) -> SplitMap.SplitMap k v -> PDoc
ppSplitMap kf vf = ppAssocList (text "SplitMap") kf vf . SplitMap.toList

ppMap' :: PDoc -> (k -> PDoc) -> (v -> PDoc) -> Map.Map k v -> PDoc
ppMap' name kf vf = ppAssocList name kf vf . Map.toList

ppMap :: (k -> PDoc) -> (v -> PDoc) -> Map.Map k v -> PDoc
ppMap = ppMap' (text "Map")

ppVMap ::
  (VMap.Vector kv k, VMap.Vector vv v) =>
  (k -> PDoc) ->
  (v -> PDoc) ->
  VMap.VMap kv vv k v ->
  PDoc
ppVMap pk pv = ppAssocList (text "VMap") pk pv . VMap.toList

-- | Used to test pretty printing things with different widths
--   for example: testwidth 120 ls ppLedgerState
--   prints LedgerState, ls, with a max width of 120 columns
--   one can use this to observe the how "pretty" a printer is at different widths
atWidth :: Int -> a -> (a -> PDoc) -> IO ()
atWidth n a pp = do
  let doc = pp a
  putDocW n doc
  putStrLn ""
