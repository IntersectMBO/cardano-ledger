
module Test.Cardano.Chain.Epoch.File
       ( tests
       ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Resource (ResIO, runResourceT)
import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Streaming (Of ((:>)))
import qualified Streaming as S

import           Cardano.Chain.Epoch.File (ParseError, parseEpochFiles)


tests :: IO Bool
tests = H.check testDeserializeEpochs

testDeserializeEpochs :: Property
testDeserializeEpochs =
  let files = [ "test/resources/epochs/00000.epoch"
              , "test/resources/epochs/00001.epoch"
              ]
      stream = parseEpochFiles files
      discard :: Of a m -> ExceptT ParseError ResIO m
      discard (_ :> rest) = pure rest
  in H.withTests 1 $ H.property $ do
     result <- (liftIO . runResourceT . runExceptT . S.run) (S.maps discard stream)
     result === Right ()
