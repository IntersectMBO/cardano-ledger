{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Ssc.VssCertificatesMap
       ( VssCertificatesMap (..)

       -- ** Creating maps
       , checkVssCertificatesMap
       , mkVssCertificatesMap
       , mkVssCertificatesMapLossy
       , mkVssCertificatesMapSingleton

       -- ** Working with maps
       , validateVssCertificatesMap
       , memberVss
       , lookupVss
       , insertVss
       , deleteVss
       ) where

import           Cardano.Prelude hiding (id)

import           Control.Lens (makeWrapped)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Formatting (build, sformat, (%))
import           Text.JSON.Canonical (FromJSON (..), ToJSON (..), expected)

import           Cardano.Binary.Class (Bi (..), Decoder, DecoderError (..),
                     Encoding)
import           Cardano.Chain.Common (StakeholderId)
import           Cardano.Chain.Ssc.VssCertificate (VssCertificate (..),
                     checkVssCertificate, getCertId, toCertPair)
import           Cardano.Crypto (ProtocolMagic)


-- | VssCertificatesMap contains all valid certificates collected
-- during some period of time.
--
-- Invariants:
--   * stakeholder ids correspond to 'vcSigningKey's of associated certs
--   * no two certs have the same 'vcVssKey'
newtype VssCertificatesMap = UnsafeVssCertificatesMap
    { getVssCertificatesMap :: Map StakeholderId VssCertificate }
    deriving (Eq, Show, Generic, NFData, Container)

makeWrapped ''VssCertificatesMap

-- | A left-biased instance
instance Semigroup VssCertificatesMap where
    (UnsafeVssCertificatesMap a) <> (UnsafeVssCertificatesMap b) =
        UnsafeVssCertificatesMap $
        a <> Map.filter (not . (`Set.member` lVssKeys) . vcVssKey) b
      where
        lVssKeys = Set.fromList (map vcVssKey (toList a))

instance Monoid VssCertificatesMap where
    mempty = UnsafeVssCertificatesMap mempty
    mappend = (<>)

instance Bi VssCertificatesMap where
    encode = encodeVssCertificates
    decode = decodeVssCertificates

instance Monad m => ToJSON m VssCertificatesMap where
    toJSON = toJSON . getVssCertificatesMap

instance MonadError SchemaError m => FromJSON m VssCertificatesMap where
  fromJSON val = do
    m <- UnsafeVssCertificatesMap <$> fromJSON val
    case validateVssCertificatesMap m of
      Left err ->
        expected "VssCertificatesMap" (Just $ "Error: " <> toString err)
      Right m' -> pure m'

instance Aeson.ToJSON VssCertificatesMap where
    toJSON = Aeson.toJSON . getVssCertificatesMap

instance Aeson.FromJSON VssCertificatesMap where
    parseJSON = Aeson.parseJSON >=>
        toAesonError . validateVssCertificatesMap . UnsafeVssCertificatesMap

-- | Construct a 'VssCertificatesMap' from a list of certs by making a
-- hashmap on certificate identifiers.
mkVssCertificatesMap :: [VssCertificate] -> VssCertificatesMap
mkVssCertificatesMap = UnsafeVssCertificatesMap . Map.fromList . map toCertPair

-- | Guard against certificates with duplicate signing keys or with duplicate
-- 'vcVssKey's. Also checks every VssCertificate in the map (see
-- 'checkVssCertificate').
checkVssCertificatesMap
  :: (MonadError Text m) => ProtocolMagic -> VssCertificatesMap -> m ()
checkVssCertificatesMap pm vssCertsMap = forM_ certs (checkVssCertificate pm)
    -- unless (allDistinct (map vcSigningKey certs))
    --     (throwError "VssCertificatesMap: two certs have the same signing key")
    -- unless (allDistinct (map vcVssKey certs))
    --     (throwError "VssCertificatesMap: two certs have the same VSS key")
  where certs = Map.elems (getVssCertificatesMap vssCertsMap)

-- | A convenient constructor of 'VssCertificatesMap' that throws away
-- certificates with duplicate signing keys or with duplicate 'vcVssKey's.
mkVssCertificatesMapLossy :: [VssCertificate] -> VssCertificatesMap
mkVssCertificatesMapLossy =
  UnsafeVssCertificatesMap . Map.fromList . map toCertPair -- . nubOrdOn vcVssKey

-- | A map with a single certificate is always valid so this function is safe to
--   use in case you have one certificate and want to create a map from it
mkVssCertificatesMapSingleton :: VssCertificate -> VssCertificatesMap
mkVssCertificatesMapSingleton =
  UnsafeVssCertificatesMap . uncurry Map.singleton . toCertPair

-- | Return given 'VssCertificatesMap' if it's valid or an error if it's not
validateVssCertificatesMap
  :: MonadError Text m => VssCertificatesMap -> m VssCertificatesMap
validateVssCertificatesMap (UnsafeVssCertificatesMap certs) = do
  forM_ (Map.toList certs) $ \(k, v) ->
    when (getCertId v /= k) $ throwError $ sformat
      ( "wrong issuerPk set as key for delegation map: "
      % "issuer id = "
      % build
      % ", cert id = "
      % build
      )
      k
      (getCertId v)
  -- unless (allDistinct (map vcVssKey (toList certs))) $
  --     throwError "two certs have the same VSS key"
  pure (UnsafeVssCertificatesMap certs)

memberVss :: StakeholderId -> VssCertificatesMap -> Bool
memberVss id (UnsafeVssCertificatesMap m) = Map.member id m

lookupVss :: StakeholderId -> VssCertificatesMap -> Maybe VssCertificate
lookupVss id (UnsafeVssCertificatesMap m) = Map.lookup id m

-- | Insert a certificate into the map.
--
-- In order to preserve invariants, this function removes certificates with
-- our certificate's signing key / VSS key, if they exist. It also returns a
-- list of deleted certificates' keys.
insertVss
  :: VssCertificate
  -> VssCertificatesMap
  -> (VssCertificatesMap, [StakeholderId])
insertVss c (UnsafeVssCertificatesMap m) =
  ( UnsafeVssCertificatesMap $ Map.insert (getCertId c) c $ Map.filter
    (not . willBeDeleted)
    m
  , deleted
  )
 where
  willBeDeleted c2 =
    vcVssKey c2 == vcVssKey c || vcSigningKey c2 == vcSigningKey c
  deleted = Map.keys $ Map.filter willBeDeleted m

deleteVss :: StakeholderId -> VssCertificatesMap -> VssCertificatesMap
deleteVss id (UnsafeVssCertificatesMap m) =
  UnsafeVssCertificatesMap (Map.delete id m)

{-
'VssCertificatesMap' is simply sets of values, indexed
by stakeholder id *for performance only*; the invariant is that the key
(stakeholder id) corresponds to the key stored in the value. This means that
the keys are redundant and putting them into encoded data is bad for two
reasons:

  * it takes more space
  * we have to do an extra invariant check after decoding

Instead, we serialize those maps as sets, and we make sure to check that
there are no values with duplicate stakeholder ids.
-}

encodeVssCertificates :: VssCertificatesMap -> Encoding
encodeVssCertificates = encode . Set.fromList . toList

decodeVssCertificates :: Decoder s VssCertificatesMap
decodeVssCertificates = do
  certs <- decode @(Set VssCertificate)
  let vssMap = mkVssCertificatesMap (toList certs)
  -- If the set is bigger than the map, then there must be some entires in
  -- the set which have the same signing key. That means it's a
  -- non-canonical encoding. The set itself could very well be canonical,
  -- though, since its values include more than just the signing keys.
  when
    (length certs > length vssMap)
    (cborError $ DecoderErrorCustom "VssCertificatesMap" "Duplicate vss key")
  pure vssMap
