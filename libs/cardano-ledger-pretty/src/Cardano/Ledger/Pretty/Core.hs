{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Pretty.Core (
  PrettyAnn (..),
  Ann,
  PDoc,
  PrettyA (..),
  GPrettyA (..),
  -- Utils
  text,
  putDoc,
  puncLeft,
  isEmpty,
  atWidth,
  ptrace,
  occaisionally,
  arrow,
  -- Common prettyprinters
  ppSexp,
  ppRecord,
  ppMap,
  ppMap',
  ppAssocList,
) where

import qualified Data.Hashable as Hashable
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Debug.Trace (trace)
import GHC.Generics (Generic (..))
import Prettyprinter
import Prettyprinter.Internal.Type (Doc (..))
import Prettyprinter.Util (putDocW)

newtype PrettyAnn = Width Int

type Ann = [PrettyAnn]

type PDoc = Doc Ann

class GPrettyA t where
  genericPrettyA :: t -> PDoc

class PrettyA t where
  prettyA :: t -> PDoc
  default prettyA :: forall r. (Generic t, GPrettyA (Rep t r)) => t -> PDoc
  prettyA = genericPrettyA @(Rep t r) . from

isEmpty :: Doc ann -> Bool
isEmpty Empty = True
isEmpty _ = False

putDoc :: Doc ann -> IO ()
putDoc = putDocW 80

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

text :: Text -> Doc ann
text = pretty

-- | Vertical layout with commas aligned on the left hand side
puncLeft :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann -> Doc ann
puncLeft open [] _ close = hsep [open, close]
puncLeft open [x] _ close = hsep [open, x, close]
puncLeft open (x : xs) coma close = align (sep ((open <+> x) : help xs))
  where
    help [] = mempty
    help [y] = [hsep [coma, y, close]]
    help (y : ys) = (coma <+> y) : help ys

-- ================================
-- Combinators for common patterns of layout

-- | x == y
equate :: Doc a -> Doc a -> Doc a
equate x y = group (flatAlt (hang 2 (sep [x <+> text "=", y])) (hsep [x, text "=", y]))

-- | Used to test pretty printing things with different widths
--   for example: testwidth 120 ls ppLedgerState
--   prints LedgerState, ls, with a max width of 120 columns
--   one can use this to observe the how "pretty" a printer is at different widths
atWidth :: Int -> a -> (a -> PDoc) -> IO ()
atWidth n a pp = do
  let doc = pp a
  putDocW n doc
  putStrLn ""

ptrace :: PrettyA t => String -> t -> a -> a
ptrace x y z = trace ("\n" ++ show (prettyA y) ++ "\n" ++ show x) z

-- | turn on trace appromimately 1 in 'n' times it is called.
occaisionally :: Hashable.Hashable a => a -> Int -> String -> String
occaisionally x n s = if mod (Hashable.hash x) n == 0 then trace s s else s

-- | x -> y
arrow :: (Doc a, Doc a) -> Doc a
arrow (x, y) = group (flatAlt (hang 2 (sep [x <+> text "->", y])) (hsep [x, text "->", y]))

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

ppMap' :: PDoc -> (k -> PDoc) -> (v -> PDoc) -> Map.Map k v -> PDoc
ppMap' name kf vf = ppAssocList name kf vf . Map.toList

ppMap :: (k -> PDoc) -> (v -> PDoc) -> Map.Map k v -> PDoc
ppMap = ppMap' (text "Map")
