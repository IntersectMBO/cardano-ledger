{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missed-specialisations #-}

module Cardano.Chain.Genesis.Delegation
  ( GenesisDelegation (..),
    GenesisDelegationError,
    mkGenesisDelegation,
  )
where

import Cardano.Binary
import Cardano.Chain.Common (KeyHash, hashKey)
import Cardano.Chain.Delegation.Certificate
  ( ACertificate (delegateVK, issuerVK),
    Certificate,
  )
import Cardano.Prelude
import Data.List (nub)
import qualified Data.Map.Strict as M
import Formatting (bprint, build, formatToString)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors (..), ToJSON (..))

-- | This type contains genesis state of heavyweight delegation. It wraps a map
--   where keys are issuers and values are delegation certificates. There are
--   some invariants:
--
--   1. In each pair delegate must differ from issuer, i. e. no revocations.
--   2. PSKs must be consistent with keys in the map, i. e. issuer's ID must be
--      equal to the key in the map.
--   3. Delegates can't be issuers, i. e. transitive delegation is not
--      supported. It's not needed in genesis, it can always be reduced.
newtype GenesisDelegation = UnsafeGenesisDelegation
  { unGenesisDelegation :: Map KeyHash Certificate
  }
  deriving (Show, Eq, NoThunks)

instance Monad m => ToJSON m GenesisDelegation where
  toJSON = toJSON . unGenesisDelegation

instance MonadError SchemaError m => FromJSON m GenesisDelegation where
  fromJSON val = do
    certs <- fromJSON val
    case recreateGenesisDelegation certs of
      Left err ->
        expected
          "GenesisDelegation"
          (Just $ "Error: " <> formatToString build err)
      Right delegation -> pure delegation

instance ToCBOR GenesisDelegation where
  toCBOR (UnsafeGenesisDelegation gd) =
    encodeListLen 1
      <> toCBOR @(Map KeyHash Certificate) gd

instance FromCBOR GenesisDelegation where
  fromCBOR = do
    enforceSize "GenesisDelegation" 1
    UnsafeGenesisDelegation <$> fromCBOR @(Map KeyHash Certificate)

data GenesisDelegationError
  = GenesisDelegationDuplicateIssuer
  | GenesisDelegationInvalidKey KeyHash KeyHash
  | GenesisDelegationMultiLayerDelegation KeyHash
  deriving (Eq, Show)

instance B.Buildable GenesisDelegationError where
  build = \case
    GenesisDelegationDuplicateIssuer ->
      bprint
        "Encountered duplicate issuer VerificationKey while constructing GenesisDelegation."
    GenesisDelegationInvalidKey k k' ->
      bprint
        ( "Invalid key in GenesisDelegation map.\nExpected: "
            . build
            . "\nGot: "
            . build
        )
        k
        k'
    GenesisDelegationMultiLayerDelegation k ->
      bprint
        ( "Encountered multi-layer delegation.\n"
            . build
            . " is a delegate and an issuer."
        )
        k

-- | Safe constructor of 'GenesisDelegation' from a list of 'Certificate's
mkGenesisDelegation ::
  MonadError GenesisDelegationError m => [Certificate] -> m GenesisDelegation
mkGenesisDelegation certs = do
  ((length . nub $ issuerVK <$> certs) == length certs)
    `orThrowError` GenesisDelegationDuplicateIssuer
  let res = M.fromList [(hashKey $ issuerVK cert, cert) | cert <- certs]
  recreateGenesisDelegation res

-- | Safe constructor of 'GenesisDelegation' from existing map
recreateGenesisDelegation ::
  MonadError GenesisDelegationError m =>
  Map KeyHash Certificate ->
  m GenesisDelegation
recreateGenesisDelegation certMap = do
  forM_ (M.toList certMap) $ \(k, cert) -> do
    let k' = hashKey $ issuerVK cert
    (k == k') `orThrowError` GenesisDelegationInvalidKey k k'

    let delegateId = hashKey $ delegateVK cert
    (delegateId `M.notMember` certMap)
      `orThrowError` GenesisDelegationMultiLayerDelegation delegateId

  pure $ UnsafeGenesisDelegation certMap
