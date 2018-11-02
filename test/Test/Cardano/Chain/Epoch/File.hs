{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Epoch.File
       ( tests
       ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Resource
    (ResIO, runResourceT)
import           Hedgehog
    (Property, (===))
import qualified Hedgehog as H
import           Streaming
    (Of ((:>)))
import qualified Streaming as S
import           System.Directory
    (getDirectoryContents)
import           System.FilePath
    (isExtensionOf, (</>))

import           Cardano.Chain.Epoch.File
    (ParseError, parseEpochFiles)


tests :: IO Bool
tests = H.checkSequential $$(H.discoverPrefix "prop")

propDeserializeEpochs :: Property
propDeserializeEpochs = H.withTests 1 $ H.property $ do
  files <- liftIO getEpochFiles
  H.assert $ not (null files)
  let stream = parseEpochFiles files
  result <- (liftIO . runResourceT . runExceptT . S.run) (S.maps discard stream)
  result === Right ()
 where
  epochDir = "cardano-mainnet-mirror/epochs"

  getEpochFiles :: IO [FilePath]
  getEpochFiles =
    take 10
      .   fmap (epochDir </>)
      .   filter ("epoch" `isExtensionOf`)
      <$> getDirectoryContents epochDir

  discard :: Of a m -> ExceptT ParseError ResIO m
  discard (_ :> rest) = pure rest
