{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Utility functions for writing pretty-printers
module Constrained.PrettyUtils (
  -- * Precedence
  WithPrec (..),
  parensIf,
  prettyPrec,

  -- * Lists
  ppList,
  ppListC,

  -- * General helpers
  prettyType,
  vsep',
  (/>),
  showType,
  short,
) where

import Constrained.List
import Data.String (fromString)
import Data.Typeable
import Prettyprinter

-- | Wrapper for pretty-printing with precendence. To get precedence
-- pretty-printing implement an instance of @`Pretty` (t`WithPrec` YourType)@ so
-- that you can use `prettyPrec`.
data WithPrec a = WithPrec Int a

-- | Pretty-print with precedence
prettyPrec :: Pretty (WithPrec a) => Int -> a -> Doc ann
prettyPrec p = pretty . WithPrec p

-- | Wrap a term in @( .. )@ if the first argument is `True`. Useful
-- in combination with t`WithPrec`
parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

-- | Map a pretty-printer for elements over a `List`
ppList :: forall f as ann. (forall a. f a -> Doc ann) -> List f as -> [Doc ann]
ppList _ Nil = []
ppList pp (a :> as) = pp a : ppList pp as

-- | Like `ppList` for a constrained pretty-printer
ppListC ::
  forall c f as ann. All c as => (forall a. c a => f a -> Doc ann) -> List f as -> [Doc ann]
ppListC _ Nil = []
ppListC pp (a :> as) = pp a : ppListC @c pp as

-- | Pretty-print a type
prettyType :: forall t x. Typeable t => Doc x
prettyType = fromString $ show (typeRep (Proxy @t))

-- | Separate documents by a hardline and align them
vsep' :: [Doc ann] -> Doc ann
vsep' = align . mconcat . punctuate hardline

-- | Lay the header (first argument) out before the body
-- and if it overflows the line indent the body by 2
(/>) :: Doc ann -> Doc ann -> Doc ann
h /> cont = hang 2 $ sep [h, align cont]

infixl 5 />

-- | Show a `Typeable` thing's type
showType :: forall t. Typeable t => String
showType = show (typeRep (Proxy @t))

-- | Pretty-print a short list in full and truncate longer lists
short :: forall a x. (Show a, Typeable a) => [a] -> Doc x
short [] = "[]"
short [x] =
  let raw = show x
      refined = if length raw <= 20 then raw else take 20 raw ++ " ... "
   in "[" <+> fromString refined <+> "]"
short xs =
  let raw = show xs
   in if length raw <= 50
        then fromString raw
        else "([" <+> viaShow (length xs) <+> "elements ...] @" <> prettyType @a <> ")"
