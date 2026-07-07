{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Huddle.Gen (
  -- * MonadGen
  module GenT,

  -- * Custom core
  module CustomCore,

  -- * Term generators
  module CustomGen,
  Term (..),
  RuleTerm (..),
  Name (..),
  genRule,
  generateFromName,
  generateFromGRef,
  genArrayTerm,
  genBytesTerm,
  genStringTerm,
  genMapTerm,
  genVectorOfUnique,
  unwrapSingleOrError,

  -- * Term validators
  module CustomValidator,
  validateFromName,
  validateFromGRef,
  validateInt,
  validateUInt,
  validateNInt,
  validateArrayTerm,
  validateBytesTerm,
  validateStringTerm,
  validateMapTerm,
  validateNonEmpty,
  validateUnique,
  validateUniqueOn,
  unwrapSingle,

  -- * Lifted generators
  arbitrary,
  scale,
  shuffle,

  -- * Antigen
  module AntiGen,
  antiVectorOfUnique,
  antiVectorOfUniqueBy,
  antiVectorOfUniqueOn,
) where

import Cardano.Ledger.Binary (Term (..))
import Cardano.Ledger.Huddle (HuddleRule ())
import Codec.CBOR.Cuddle.CBOR.Gen (generateFromGRef, generateFromName)
import Codec.CBOR.Cuddle.CBOR.Validator (validateFromGRef, validateFromName)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (nintMin, uintMax)
import Codec.CBOR.Cuddle.CDDL.Custom.Core as CustomCore
import Codec.CBOR.Cuddle.CDDL.Custom.Generator as CustomGen
import Codec.CBOR.Cuddle.CDDL.Custom.Validator as CustomValidator
import Control.Monad (unless, when)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
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

-- Term validators

unwrapSingleOrError :: RuleTerm -> Term
unwrapSingleOrError (SingleTerm x) = x
unwrapSingleOrError _ = error "Expected a single term"
