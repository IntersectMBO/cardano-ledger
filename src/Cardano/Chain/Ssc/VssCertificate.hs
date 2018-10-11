{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Chain.Ssc.VssCertificate
       ( VssCertificate (..)

       , _vcVssKey
       , _vcExpiryEpoch
       , _vcSignature
       , _vcSigningKey

       , mkVssCertificate
       , checkVssCertificate
       , checkCertSign
       , getCertId
       , toCertPair

       , dropVssCertificate
       ) where

import           Cardano.Prelude

import           Control.Lens (makeLensesFor)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson.Options as S (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Formatting (bprint, build, int, (%))
import qualified Formatting.Buildable as B (Buildable (..))
import           Text.JSON.Canonical (FromJSON (..), Int54, JSValue (..),
                     ToJSON (..), fromJSField, mkObject)

import           Cardano.Binary.Class (AsBinary, Bi (..), Dropper, dropBytes,
                     dropWord64, encodeListLen, enforceSize)
import           Cardano.Chain.Common (StakeholderId, mkStakeholderId)
import           Cardano.Chain.Slotting (EpochIndex)
import           Cardano.Crypto (ProtocolMagic, PublicKey, SecretKey,
                     SignTag (SignVssCert), Signature, VssPublicKey, checkSig,
                     sign, toPublic)


-- | VssCertificate allows VssPublicKey to participate in MPC. Each
-- stakeholder should create a Vss keypair, sign VSS public key with signing
-- key and send it into blockchain.
--
-- A public key of node is included in certificate in order to enable
-- validation of it using only node's P2PKH address. Expiry epoch is last
-- epoch when certificate is valid, expiry epoch is included in certificate
-- and signature.
--
-- Other nodes accept this certificate if it is valid and if node has enough
-- stake.
--
-- Invariant: 'checkSig vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature'.
data VssCertificate = UnsafeVssCertificate
    { vcVssKey      :: !(AsBinary VssPublicKey)
    , vcExpiryEpoch :: !EpochIndex
    -- ^ Epoch up to which certificate is valid.
    , vcSignature   :: !(Signature (AsBinary VssPublicKey, EpochIndex))
    , vcSigningKey  :: !PublicKey
    } deriving (Show, Eq, Generic)
      deriving anyclass NFData

makeLensesFor
  [ ("vcVssKey"     , "_vcVssKey")
  , ("vcExpiryEpoch", "_vcExpiryEpoch")
  , ("vcSignature"  , "_vcSignature")
  , ("vcSigningKey" , "_vcSigningKey")
  ]
  ''VssCertificate

instance Ord VssCertificate where
    compare a b = toTuple a `compare` toTuple b
      where
        toTuple vc =
            (vcExpiryEpoch vc, vcVssKey vc, vcSigningKey vc, vcSignature vc)

instance B.Buildable VssCertificate where
    build vc = bprint
        ("vssCert:" % build % ":" % int)
        (vcSigningKey vc)
        (vcExpiryEpoch vc)

instance B.Buildable (StakeholderId, VssCertificate) where
    build (a, b) = bprint ("(id: "%build%" , cert: "%build%")") a b

dropVssCertificate :: Dropper s
dropVssCertificate = do
  enforceSize "VssCertificate" 4
  -- AsBinary VssPublicKey
  dropBytes
  -- EpochIndex
  dropWord64
  -- Signature (AsBinary VssPublicKey, EpochIndex)
  dropBytes
  -- PublicKey
  dropBytes

instance Bi VssCertificate where
    encode vssCert = encodeListLen 4
        <> encode (vcVssKey vssCert)
        <> encode (vcExpiryEpoch vssCert)
        <> encode (vcSignature vssCert)
        <> encode (vcSigningKey vssCert)

    decode = do
        enforceSize "VssCertificate" 4
        UnsafeVssCertificate <$> decode <*> decode <*> decode <*> decode

instance Monad m => ToJSON m VssCertificate where
    toJSON vc = mkObject
        [ ("vssKey"     , toJSON (vcVssKey vc))
        , ("expiryEpoch", pure (JSNum . fromIntegral $ vcExpiryEpoch vc))
        , ("signature"  , toJSON (vcSignature vc))
        , ("signingKey" , toJSON (vcSigningKey vc))
        ]

instance MonadError SchemaError m => FromJSON m VssCertificate where
    fromJSON obj = do
        vssKey <- fromJSField obj "vssKey"
        expiryEpoch <- fromIntegral @Int54 <$> fromJSField obj "expiryEpoch"
        signature <- fromJSField obj "signature"
        signingKey <- fromJSField obj "signingKey"
        return $ UnsafeVssCertificate
            { vcVssKey      = vssKey
            , vcExpiryEpoch = expiryEpoch
            , vcSignature   = signature
            , vcSigningKey  = signingKey
            }

deriveJSON S.defaultOptions ''VssCertificate

-- | Make VssCertificate valid up to given epoch using 'SecretKey' to sign
-- data.
mkVssCertificate
    :: ProtocolMagic
    -> SecretKey
    -> AsBinary VssPublicKey
    -> EpochIndex
    -> VssCertificate
mkVssCertificate pm sk vk expiry = UnsafeVssCertificate
    vk
    expiry
    signature
    (toPublic sk)
    where signature = sign pm SignVssCert sk (vk, expiry)

-- | Check a 'VssCertificate' for validity.
checkVssCertificate
    :: (MonadError Text m) => ProtocolMagic -> VssCertificate -> m ()
checkVssCertificate pm it = unless (checkCertSign pm it)
    $ throwError "checkVssCertificate: invalid sign"

-- CHECK: @checkCertSign
-- | Check that the VSS certificate is signed properly
-- #checkPubKeyAddress
-- #checkSig
checkCertSign :: ProtocolMagic -> VssCertificate -> Bool
checkCertSign pm vc = checkSig
    pm
    SignVssCert
    (vcSigningKey vc)
    (vcVssKey vc, vcExpiryEpoch vc)
    (vcSignature vc)

getCertId :: VssCertificate -> StakeholderId
getCertId = mkStakeholderId . vcSigningKey

toCertPair :: VssCertificate -> (StakeholderId, VssCertificate)
toCertPair vc = (getCertId vc, vc)
