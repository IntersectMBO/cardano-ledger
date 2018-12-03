{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Chain.Genesis.Delegation
  ( GenesisDelegation(..)
  , GenesisDelegationError
  , mkGenesisDelegation
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError)
import qualified Data.Aeson as Aeson
import Data.List (nub)
import qualified Data.Map.Strict as M
import Formatting (build, formatToString, bprint)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical (FromJSON(..), ReportSchemaErrors(..), ToJSON(..))

import Cardano.Chain.Common (StakeholderId, mkStakeholderId)
import Cardano.Chain.Delegation.Certificate (Certificate)
import Cardano.Crypto (isSelfSignedPsk, pskDelegatePk, pskIssuerPk)


-- | This type contains genesis state of heavyweight delegation. It wraps a map
--   where keys are issuers (i. e. stakeholders who delegated) and values are
--   proxy signing keys. There are some invariants:
--
--   1. In each pair delegate must differ from issuer, i. e. no revocations.
--   2. PSKs must be consistent with keys in the map, i. e. issuer's ID must be
--      equal to the key in the map.
--   3. Delegates can't be issuers, i. e. transitive delegation is not
--      supported. It's not needed in genesis, it can always be reduced.
--
newtype GenesisDelegation = UnsafeGenesisDelegation
  { unGenesisDelegation :: Map StakeholderId Certificate
  } deriving (Show, Eq)

instance Monad m => ToJSON m GenesisDelegation where
  toJSON = toJSON . unGenesisDelegation

instance MonadError SchemaError m => FromJSON m GenesisDelegation where
  fromJSON val = do
    psks <- fromJSON val
    case recreateGenesisDelegation psks of
      Left err -> expected "GenesisDelegation" (Just $ "Error: " <> formatToString build err)
      Right delegation -> pure delegation

instance Aeson.ToJSON GenesisDelegation where
  toJSON = Aeson.toJSON . unGenesisDelegation

instance Aeson.FromJSON GenesisDelegation where
  parseJSON = Aeson.parseJSON >=> \v -> do
    elems' <- mapM Aeson.parseJSON v
    toAesonError $ recreateGenesisDelegation elems'

data GenesisDelegationError
  = GenesisDelegationDuplicateIssuer
  | GenesisDelegationInvalidKey StakeholderId StakeholderId
  | GenesisDelegationMultiLayerDelegation StakeholderId
  | GenesisDelegationSelfSignedPsk Certificate
  deriving (Eq, Show)

instance B.Buildable GenesisDelegationError where
  build = \case
    GenesisDelegationDuplicateIssuer ->
      bprint
        "Encountered duplicate issuer PublicKey while constructing GenesisDelegation."
    GenesisDelegationInvalidKey k k' -> bprint
      ( "Invalid key in GenesisDelegation map.\nExpected: "
      . build
      . "\nGot: "
      . build
      )
      k
      k'
    GenesisDelegationMultiLayerDelegation k -> bprint
      ( "Encountered multi-layer delegation.\n"
      . build
      . " is a delegate and an issuer."
      )
      k
    GenesisDelegationSelfSignedPsk psk -> bprint
      ("Encountered self-signed ProxySecretKey while constructing GenesisDelegation.\n"
      . build
      )
      psk

-- | Safe constructor of 'GenesisDelegation' from a list of PSKs.
mkGenesisDelegation
  :: MonadError GenesisDelegationError m => [Certificate] -> m GenesisDelegation
mkGenesisDelegation psks = do
  ((length . nub $ pskIssuerPk <$> psks) == length psks)
    `orThrowError` GenesisDelegationDuplicateIssuer
  let
    res = M.fromList [ (mkStakeholderId $ pskIssuerPk psk, psk) | psk <- psks ]
  recreateGenesisDelegation res

-- | Safe constructor of 'GenesisDelegation' from existing map.
recreateGenesisDelegation
  :: MonadError GenesisDelegationError m
  => Map StakeholderId Certificate
  -> m GenesisDelegation
recreateGenesisDelegation pskMap = do
  forM_ (M.toList pskMap) $ \(k, psk) -> do

    let k' = mkStakeholderId $ pskIssuerPk psk
    (k == k') `orThrowError` GenesisDelegationInvalidKey k k'

    not (isSelfSignedPsk psk) `orThrowError` GenesisDelegationSelfSignedPsk psk

    let delegateId = mkStakeholderId $ pskDelegatePk psk
    (delegateId `M.notMember` pskMap)
      `orThrowError` GenesisDelegationMultiLayerDelegation delegateId

  pure $ UnsafeGenesisDelegation pskMap
