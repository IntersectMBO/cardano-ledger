{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Chain.Epoch.Validation
  ( EpochError (..),
    validateEpochFile,
    validateEpochFiles,
  )
where

import Cardano.Chain.Block
  ( ABlockOrBoundary (..),
    ChainValidationError,
    ChainValidationState (..),
    blockSlot,
    updateChainBlockOrBoundary,
  )
import Cardano.Chain.Epoch.File
  ( ParseError,
    mainnetEpochSlots,
    parseEpochFileWithBoundary,
    parseEpochFilesWithBoundary,
  )
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Slotting
  ( EpochAndSlotCount,
    fromSlotNumber,
  )
import Cardano.Chain.ValidationMode (ValidationMode)
import Cardano.Prelude hiding (trace)
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Control.Tracer
import Streaming (Of (..), Stream, hoist)
import qualified Streaming.Prelude as S

data EpochError
  = EpochParseError ParseError
  | EpochChainValidationError (Maybe EpochAndSlotCount) ChainValidationError
  | Initial
  deriving (Eq, Show)

-- | Check that a single epoch's `Block`s are valid by folding over them
-- TODO(KS): We should use contra-tracer here!
-- tracing is orthogonal to throwing errors; it does not change the program flow.
validateEpochFile ::
  forall m.
  (MonadIO m) =>
  Tracer m EpochError ->
  ValidationMode ->
  Genesis.Config ->
  ChainValidationState ->
  FilePath ->
  m ChainValidationState
validateEpochFile tr vMode config cvs fp = do
  res <-
    liftIO $
      runResourceT $
        (`runReaderT` vMode) $
          runExceptT $
            foldChainValidationState config cvs stream

  case res of
    Left e -> traceWith tr e >> pure cvs
    Right cvs' -> pure cvs'
  where
    stream = parseEpochFileWithBoundary mainnetEpochSlots fp

-- | Check that a list of epochs 'Block's are valid.
validateEpochFiles ::
  ValidationMode ->
  Genesis.Config ->
  ChainValidationState ->
  [FilePath] ->
  IO (Either EpochError ChainValidationState)
validateEpochFiles vMode config cvs fps =
  runResourceT $
    (`runReaderT` vMode) $
      runExceptT
        (foldChainValidationState config cvs stream)
  where
    stream = parseEpochFilesWithBoundary mainnetEpochSlots fps

-- | Fold chain validation over a 'Stream' of 'Block's
foldChainValidationState ::
  Genesis.Config ->
  ChainValidationState ->
  Stream (Of (ABlockOrBoundary ByteString)) (ExceptT ParseError ResIO) () ->
  ExceptT EpochError (ReaderT ValidationMode ResIO) ChainValidationState
foldChainValidationState config chainValState blocks =
  S.foldM_
    ( \cvs block ->
        withExceptT (EpochChainValidationError (blockOrBoundarySlot block)) $
          updateChainBlockOrBoundary config cvs block
    )
    (pure chainValState)
    pure
    (pure (hoist (withExceptT EpochParseError) blocks))
  where
    blockOrBoundarySlot :: ABlockOrBoundary a -> Maybe EpochAndSlotCount
    blockOrBoundarySlot = \case
      ABOBBoundary _ -> Nothing
      ABOBBlock block -> Just . fromSlotNumber mainnetEpochSlots $ blockSlot block
