{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Txp.Validation
       ( tests
       ) where

import Cardano.Prelude

import Control.Monad.Trans.Resource
  (ResIO, runResourceT)
import Data.IORef
  (IORef, newIORef, readIORef, writeIORef)
import Data.String
  (fromString)
import Formatting
  (build, sformat)
import Streaming
  (Of (..), Stream, hoist)
import qualified Streaming.Prelude as S

import Hedgehog
  ( Group (..)
  , Property
  , PropertyName
  , checkSequential
  , evalEither
  , property
  , withTests
  )

import Cardano.Chain.Block
  (Blund, blockSlot, blockTxPayload)
import Cardano.Chain.Epoch.File
  (ParseError, parseEpochFile)
import Cardano.Chain.Genesis
  (readGenesisData)
import Cardano.Chain.Slotting
  (SlotId)
import Cardano.Chain.Txp
  (UTxO, UTxOValidationError, genesisUtxo, txpTxs, updateUTxO)

import Test.Cardano.Chain.Epoch.File
  (getEpochFiles)


tests :: IO Bool
tests = do
  genesisData <- either (panic . sformat build) identity
    <$> runExceptT (readGenesisData "test/mainnet-genesis.json")
  let utxo = genesisUtxo genesisData
  utxoRef <- newIORef utxo
  files   <- take 15 <$> getEpochFiles
  let
    properties :: [(PropertyName, Property)]
    properties = zip (fromString <$> files) (epochValid utxoRef <$> files)
  checkSequential $ Group "Test.Cardano.Chain.Txp.Validation" properties

data Error
  = ErrorParseError ParseError
  | ErrorUTxOValidationError SlotId UTxOValidationError
  deriving (Eq, Show)

epochValid :: IORef UTxO -> FilePath -> Property
epochValid utxoRef fp = withTests 1 . property $ do
  utxo <- liftIO $ readIORef utxoRef
  let stream = parseEpochFile fp
  result <- (liftIO . runResourceT . runExceptT) (foldUTxO utxo stream)
  newUtxo <- evalEither result
  liftIO $ writeIORef utxoRef newUtxo

foldUTxO
  :: UTxO
  -> Stream (Of Blund) (ExceptT ParseError ResIO) ()
  -> ExceptT Error ResIO UTxO
foldUTxO utxo blunds = S.foldM_
  foldUTxOBlund
  (pure utxo)
  pure
  (hoist (withExceptT ErrorParseError) blunds)

foldUTxOBlund :: UTxO -> Blund -> ExceptT Error ResIO UTxO
foldUTxOBlund utxo (block, _) =
  withExceptT (ErrorUTxOValidationError $ blockSlot block)
    $ foldM updateUTxO utxo (txpTxs $ blockTxPayload block)
