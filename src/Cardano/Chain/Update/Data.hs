{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Update.Data
  ( UpdateData(..)
  )
where

import Cardano.Prelude

import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class (Bi(..), Raw, encodeListLen, enforceSize)
import Cardano.Crypto (Hash)


-- | Data which describes update. It is specific for each system.
data UpdateData = UpdateData
  { udAppDiffHash  :: !(Hash Raw)
  -- ^ Hash of binary diff between two applications. This diff can be passed to
  --   updater to create new application.
  , udPkgHash      :: !(Hash Raw)
  -- ^ Hash of package to install new application. This package can be used to
  --   install new application from scratch instead of updating existing
  --   application.
  , udUpdaterHash  :: !(Hash Raw)
  -- ^ Hash if update application which can be used to install this update
  --   (relevant only when updater is used, not package).
  , udMetadataHash :: !(Hash Raw)
  -- ^ Hash of metadata relevant to this update. It is raw hash, because
  --   metadata can include image or something (maybe). Anyway, we can always
  --   use `unsafeHash`.
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance B.Buildable UpdateData where
  build ud = bprint
    ( "{ appDiff: "
    . build
    . ", pkg: "
    . build
    . ", updater: "
    . build
    . ", metadata: "
    . build
    . " }"
    )
    (udAppDiffHash ud)
    (udPkgHash ud)
    (udUpdaterHash ud)
    (udMetadataHash ud)

instance Bi UpdateData where
  encode ud =
    encodeListLen 4
      <> encode (udAppDiffHash ud)
      <> encode (udPkgHash ud)
      <> encode (udUpdaterHash ud)
      <> encode (udMetadataHash ud)

  decode = do
    enforceSize "UpdateData" 4
    UpdateData <$> decode <*> decode <*> decode <*> decode
