
module Test.Cardano.Chain.Epoch.File
       ( tests
       ) where

import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Either (isRight)
import           Hedgehog (Property)
import qualified Hedgehog as H
import           Streaming (Of ((:>)))
import qualified Streaming as S

import           Cardano.Chain.Epoch.File (parseEpochFiles)
import           Cardano.Prelude

tests :: IO Bool
tests = H.check testDeserializeEpochs

testDeserializeEpochs :: Property
testDeserializeEpochs =
  let files = [ "test/resources/epochs/00000.epoch"
              , "test/resources/epochs/00001.epoch"
              ]
      stream = parseEpochFiles files
      discard (_ :> rest) = pure rest
  in H.withTests 1 $ H.property $ do
     result <- (liftIO . runResourceT . runExceptT . S.run) (S.maps discard stream)
     H.assert (isRight result)
