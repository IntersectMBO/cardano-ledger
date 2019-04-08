{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Epoch.Validation
  ( EpochError(..)
  , validateEpochFile
  , validateEpochFiles
  , validateEpochFileForFolding
  )
where

import Cardano.Prelude

import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Streaming (Of(..), Stream, hoist)
import qualified Streaming.Prelude as S

import Cardano.Chain.Block
  ( ABlockOrBoundary(..)
  , ChainValidationError
  , ChainValidationState(..)
  , blockSlot
  , updateChainBlockOrBoundary
  )
import Cardano.Chain.Epoch.File
  (ParseError, mainnetEpochSlots, parseEpochFileWithBoundary, parseEpochFilesWithBoundary)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Slotting (SlotId, unflattenSlotId)


data EpochError
  = EpochParseError ParseError
  | EpochChainValidationError (Maybe SlotId) ChainValidationError
  | Initial
  deriving (Eq, Show)


-- | Check that a single epoch's `Block`s are valid by folding over them
validateEpochFile
  :: Genesis.Config
  -> ChainValidationState
  -> FilePath
  -> IO (Either EpochError ChainValidationState)
validateEpochFile config cvs fp =
    runResourceT . runExceptT $ foldChainValidationState config cvs stream
  where stream = parseEpochFileWithBoundary mainnetEpochSlots fp

-- | TODO: Annotate me and include logging
validateEpochFileForFolding
  :: Genesis.Config
  -> ChainValidationState
  -> FilePath
  -> ExceptT EpochError ResIO ChainValidationState
validateEpochFileForFolding config cvs fp = foldChainValidationState config cvs stream
  where stream = parseEpochFileWithBoundary mainnetEpochSlots fp

-- | Check that a list of epochs 'Block's are valid.
validateEpochFiles
  :: Genesis.Config
  -> ChainValidationState
  -> [FilePath]
  -> IO (Either EpochError ChainValidationState)
validateEpochFiles config cvs fps =
    runResourceT . runExceptT $ foldChainValidationState config cvs stream
  where stream = parseEpochFilesWithBoundary mainnetEpochSlots fps

-- | Fold chain validation over a 'Stream' of 'Block's
foldChainValidationState
  :: Genesis.Config
  -> ChainValidationState
  -> Stream (Of (ABlockOrBoundary ByteString)) (ExceptT ParseError ResIO) ()
  -> ExceptT EpochError ResIO ChainValidationState
foldChainValidationState config chainValState blocks = S.foldM_
  (\cvs block ->
    withExceptT (EpochChainValidationError (blockOrBoundarySlot block))
      $ updateChainBlockOrBoundary config cvs block
  )
  (pure chainValState)
  pure $ hoist (withExceptT EpochParseError) blocks
 where
  blockOrBoundarySlot :: ABlockOrBoundary a -> Maybe SlotId
  blockOrBoundarySlot = \case
    ABOBBoundary _     -> Nothing
    ABOBBlock    block -> Just . unflattenSlotId mainnetEpochSlots $ blockSlot block
