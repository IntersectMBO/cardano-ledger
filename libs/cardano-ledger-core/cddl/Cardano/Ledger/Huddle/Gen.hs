{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Huddle.Gen (
  -- * MonadGen
  module GenT,

  -- * CBORGen
  module CBORGen,

  -- * Term generators
  Term (..),
  WrappedTerm (..),
  genRule,
  genArrayTerm,
  genBytesTerm,
  genStringTerm,
  genMapTerm,

  -- * Lifted generators
  arbitrary,
  scale,
  shuffle,

  -- * Antigen
  module AntiGen,
) where

import Cardano.Ledger.Binary (Term (..))
import Cardano.Ledger.Huddle (HuddleRule ())
import Codec.CBOR.Cuddle.CBOR.Gen (generateFromName)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CBORGenerator as CBORGen
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHC.TypeLits (symbolVal)
import Test.AntiGen as AntiGen
import qualified Test.QuickCheck as QC
import Test.QuickCheck.GenT as GenT

-- | A function for generating a term from a rule. The @HuddleRule@ constraint
-- ensures that the rule is actually defined in that era.
genRule :: forall rule era. HuddleRule rule era => CBORGen Term
genRule = generateFromName (Name . T.pack . symbolVal $ Proxy @rule)

-- Lifted Gen functions

arbitrary :: forall a m. (MonadGen m, QC.Arbitrary a) => m a
arbitrary = liftGen QC.arbitrary

scale :: MonadGen m => (Int -> Int) -> m a -> m a
scale f m = sized $ \sz -> resize (f sz) m

shuffle :: MonadGen m => [a] -> m [a]
shuffle = liftGen . QC.shuffle

-- Term generators

genArrayTerm :: MonadGen m => [Term] -> m Term
genArrayTerm es = GenT.elements [TList es, TListI es]

genBytesTerm :: MonadGen m => ByteString -> m Term
genBytesTerm bs = GenT.elements [TBytes bs, TBytesI $ LBS.fromStrict bs]

genStringTerm :: MonadGen m => T.Text -> m Term
genStringTerm t = GenT.elements [TString t, TStringI $ LT.fromStrict t]

genMapTerm :: MonadGen m => [(Term, Term)] -> m Term
genMapTerm m = GenT.elements [TMap m, TMapI m]
