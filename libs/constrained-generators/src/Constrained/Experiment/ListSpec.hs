{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Random Natural, Arbitrary Natural, Uniform Natural
{-# OPTIONS_GHC -Wno-orphans #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.ListSpecExperiment where

import Constrained.BaseExperiment

import Constrained.Core (unionWithMaybe)
import Constrained.GenT (GenT, MonadGenError (..), pureGen, sizeT)
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.Foldable (fold)
import Data.Kind
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Typeable (typeOf)
import Data.Word
import GHC.Int
import GHC.Natural
import GHC.Real
import GHC.TypeLits (Symbol)
import System.Random.Stateful (Random (..), Uniform (..))
import Test.QuickCheck (Arbitrary (arbitrary, shrink), choose, frequency)

-- =====================================================================
-- STUB requires Foldy instance

data ListSpec fn a = ListSpec
  { listSpecHint :: Maybe Integer
  , listSpecMust :: [a]
  , listSpecSize :: Specification fn Integer
  , listSpecElem :: Specification fn a
  , listSpecFold :: FoldSpec fn a
  }

instance
  ( Arbitrary a
  , Arbitrary (FoldSpec fn a)
  , Arbitrary (TypeSpec fn a)
  , HasSpec fn a
  ) =>
  Arbitrary (ListSpec fn a)
  where
  arbitrary = ListSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (ListSpec a b c d e) = [ListSpec a' b' c' d' e' | (a', b', c', d', e') <- shrink (a, b, c, d, e)]

instance HasSpec fn a => Show (FoldSpec fn a) where
  showsPrec d = shows . prettyPrec d

instance HasSpec fn a => Pretty (WithPrec (FoldSpec fn a)) where
  pretty (WithPrec _ NoFold) = "NoFold"
  pretty (WithPrec d (FoldSpec fn s)) =
    parensIf (d > 10) $
      "FoldSpec"
        /> vsep'
          [ "fn   =" <+> parens (hsep ["injectFn", "$", viaShow fn])
          , "spec =" <+> pretty s
          ]

instance HasSpec fn a => Pretty (FoldSpec fn a) where
  pretty = prettyPrec 0

instance HasSpec fn a => Show (ListSpec fn a) where
  showsPrec d = shows . prettyPrec d

instance
  HasSpec fn a =>
  Pretty (WithPrec (ListSpec fn a))
  where
  pretty (WithPrec d s) =
    parensIf (d > 10) $
      "ListSpec"
        /> vsep'
          [ "hint =" <+> viaShow (listSpecHint s)
          , "must =" <+> viaShow (listSpecMust s)
          , "size =" <+> pretty (listSpecSize s)
          , "elem =" <+> pretty (listSpecElem s)
          , "fold =" <+> pretty (listSpecFold s)
          ]

instance HasSpec fn a => Pretty (ListSpec fn a) where
  pretty = prettyPrec 0

guardListSpec :: HasSpec fn a => [String] -> ListSpec fn a -> Specification fn [a]
guardListSpec msg l@(ListSpec _hint must size elemS _fold)
  | ErrorSpec es <- size = ErrorSpec $ (NE.fromList ("Error in size of ListSpec" : msg)) <> es
  | Just u <- knownUpperBound size
  , u < 0 =
      ErrorSpec $ NE.fromList (["Negative size in guardListSpec", show size] ++ msg)
  | not (all (`conformsToSpec` elemS) must) =
      ErrorSpec $
        ( NE.fromList
            (["Some items in the must list do not conform to 'element' spec.", "   " ++ show elemS] ++ msg)
        )
  | otherwise = typeSpec l

instance HasSpec fn a => HasSpec fn [a] where
  type TypeSpec fn [a] = ListSpec fn a
  type Prerequisites fn [a] = HasSpec fn a
  emptySpec = ListSpec Nothing [] mempty mempty NoFold
  combineSpec l1@(ListSpec msz must size elemS foldS) l2@(ListSpec msz' must' size' elemS' foldS') =
    let must'' = nub $ must <> must'
        elemS'' = elemS <> elemS'
        size'' = size <> size'
        foldeither = combineFoldSpec foldS foldS'
        msg = ["Error in combineSpec for ListSpec", "1) " ++ show l1, "2) " ++ show l2]
     in case foldeither of
          Left foldmsg -> ErrorSpec (NE.fromList (msg ++ foldmsg))
          Right fold'' -> guardListSpec msg $ ListSpec (unionWithMaybe min msz msz') must'' size'' elemS'' fold''

  genFromTypeSpec (ListSpec _ must _ elemS _)
    | any (not . (`conformsToSpec` elemS)) must =
        genError1 "genTypeSpecSpec @ListSpec: some elements of mustSet do not conform to elemS"
  genFromTypeSpec (ListSpec msz must TrueSpec elemS NoFold) = do
    lst <- case msz of
      Nothing -> listOfT $ genFromSpecT elemS
      Just szHint -> do
        sz <- genFromSizeSpec (leqSpec @fn szHint)
        listOfUntilLenT (genFromSpecT elemS) (fromIntegral sz) (const True)
    pureGen $ shuffle (must ++ lst)
  genFromTypeSpec (ListSpec msz must szSpec elemS NoFold) = do
    sz0 <- genFromSizeSpec (szSpec <> geqSpec @fn (sizeOf must) <> maybe TrueSpec (leqSpec . max 0) msz)
    let sz = fromIntegral (sz0 - sizeOf must)
    lst <-
      listOfUntilLenT
        (genFromSpecT elemS)
        sz
        ((`conformsToSpec` szSpec) . (+ sizeOf must) . fromIntegral)
    pureGen $ shuffle (must ++ lst)
  genFromTypeSpec (ListSpec msz must szSpec elemS (FoldSpec f foldS)) = do
    let szSpec' = szSpec <> maybe TrueSpec (leqSpec . max 0) msz
    genFromFold must szSpec' elemS f foldS

  shrinkWithTypeSpec (ListSpec _ _ _ es _) as =
    shrinkList (shrinkWithSpec es) as

  cardinalTypeSpec _ = TrueSpec

  guardTypeSpec = guardListSpec

  conformsTo xs (ListSpec _ must size elemS foldS) =
    sizeOf xs `conformsToSpec` size
      && all (`elem` xs) must
      && all (`conformsToSpec` elemS) xs
      && xs `conformsToFoldSpec` foldS

  toPreds x (ListSpec msz must size elemS foldS) =
    (forAll x $ \x' -> satisfies x' elemS)
      <> (forAll (lit must) $ \x' -> assert (elem_ x' x))
      <> toPredsFoldSpec x foldS
      <> satisfies (sizeOf_ x) size
      <> maybe TruePred (flip genHint x) msz

instance HasSpec fn a => HasGenHint fn [a] where
  type Hint [a] = Integer
  giveHint szHint = typeSpec $ ListSpec (Just szHint) [] mempty mempty NoFold

instance Forallable [a] a where
  fromForAllSpec es = typeSpec (ListSpec Nothing [] mempty es NoFold)
  forAllToList = id
