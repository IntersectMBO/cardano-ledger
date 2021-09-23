{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Epoch.File
  ( tests,
  )
where

import Cardano.Chain.Epoch.File (ParseError, mainnetEpochSlots, parseEpochFilesWithBoundary)
import Cardano.Prelude
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Hedgehog (Group, Property, discover, (===))
import qualified Hedgehog as H
import Streaming (Of ((:>)))
import qualified Streaming as S
import System.Environment (lookupEnv)
import Test.Cardano.Mirror (mainnetEpochFiles)

tests :: Group
tests = $$discover

prop_deserializeEpochs :: Property
prop_deserializeEpochs = H.withTests 1 $
  H.property $ do
    menv <- liftIO $ lookupEnv "CARDANO_MAINNET_MIRROR"
    H.assert $ isJust menv

    files <- take 10 <$> liftIO mainnetEpochFiles
    H.assert $ not (null files)
    -- TODO: the property cannot take any parameters (if we want discoverPrefix
    -- to work). Now the question is whether it is OK to use an hardcoded value
    -- for the number of slots per epoch, and if so in which module should we
    -- store this constant?
    let stream = parseEpochFilesWithBoundary mainnetEpochSlots files
    result <- (liftIO . runResourceT . runExceptT . S.run) (S.maps discard stream)
    result === Right ()
  where
    discard :: Of a m -> ExceptT ParseError ResIO m
    discard (_ :> rest) = pure rest
