{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Cardano.Chain.Block.Validation
  ( tests,
  )
where

import Cardano.Chain.Block
  ( ABlockOrBoundary (..),
    BlockValidationMode (BlockValidation),
    ChainValidationError,
    ChainValidationState (..),
    blockSlot,
    initialChainValidationState,
    updateBlock,
    updateChainBoundary,
  )
import Cardano.Chain.Epoch.File (ParseError, parseEpochFilesWithBoundary)
import Cardano.Chain.Genesis as Genesis (Config (..), configEpochSlots)
import Cardano.Chain.Slotting (SlotNumber)
import Cardano.Chain.ValidationMode (fromBlockValidationMode)
import Cardano.Prelude
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Hedgehog
  ( Group (..),
    annotate,
    assert,
    discover,
    evalEither,
    property,
    withTests,
  )
import Streaming (Of (..), Stream, hoist)
import qualified Streaming.Prelude as S
import System.Environment (lookupEnv)
import Test.Cardano.Chain.Config (readMainetCfg)
import Test.Cardano.Mirror (mainnetEpochFiles)
import Test.Options
  ( ShouldAssertNF (..),
    TSGroup,
    TSProperty,
    TestScenario (..),
    concatTSGroups,
  )

-- | These tests perform chain validation over mainnet epoch files
tests :: ShouldAssertNF -> TSGroup
tests shouldAssertNF =
  concatTSGroups
    [ const $$discover,
      \scenario ->
        Group
          "Test.Cardano.Chain.Block.Validation"
          [ ( "ts_prop_mainnetEpochsValid",
              ts_prop_mainnetEpochsValid shouldAssertNF scenario
            )
          ]
    ]

ts_prop_mainnetEpochsValid :: ShouldAssertNF -> TSProperty
ts_prop_mainnetEpochsValid shouldAssertNF scenario = withTests 1 . property $ do
  menv <- liftIO $ lookupEnv "CARDANO_MAINNET_MIRROR"
  assert $ isJust menv

  -- Get the 'Genesis.Config' from the mainnet genesis JSON
  config <- readMainetCfg

  -- Construct the initial 'ChainValidationState'
  let cvs = either (panic . show) identity $ initialChainValidationState config

  let takeFiles :: [FilePath] -> [FilePath]
      takeFiles = case scenario of
        ContinuousIntegration -> identity
        Development -> take 15
        QualityAssurance -> identity

  -- Get a list of epoch files to perform validation on
  files <- takeFiles <$> liftIO mainnetEpochFiles

  let stream = parseEpochFilesWithBoundary (configEpochSlots config) files

  annotate
    ( "Did you build with `ghc -fhpc` or `stack --coverage`?\n"
        <> "If so, please be aware that hpc will introduce thunks around "
        <> "expressions for its program coverage measurement purposes and "
        <> "this assertion can fail as a result.\n"
        <> "Otherwise, for some reason, the `ChainValidationState` is not in "
        <> "normal form."
    )

  result <-
    (liftIO . runResourceT . runExceptT)
      (foldChainValidationState shouldAssertNF config cvs stream)

  void $ evalEither result

data Error
  = ErrorParseError ParseError
  | ErrorChainValidationError (Maybe SlotNumber) ChainValidationError
  deriving (Eq, Show)

-- | Fold chain validation over a 'Stream' of 'Block's
foldChainValidationState ::
  ShouldAssertNF ->
  Genesis.Config ->
  ChainValidationState ->
  Stream (Of (ABlockOrBoundary ByteString)) (ExceptT ParseError ResIO) () ->
  ExceptT Error ResIO ChainValidationState
foldChainValidationState shouldAssertNF config cvs blocks =
  S.foldM_ validate (pure cvs) pure (hoist (withExceptT ErrorParseError) blocks)
  where
    validate ::
      MonadIO m =>
      ChainValidationState ->
      ABlockOrBoundary ByteString ->
      ExceptT Error m ChainValidationState
    validate c b =
      withExceptT (ErrorChainValidationError (blockOrBoundarySlot b))
        . (`runReaderT` fromBlockValidationMode BlockValidation)
        $ case b of
          ABOBBoundary bvd -> do
            case shouldAssertNF of
              AssertNF -> do
                isNF <- liftIO $ isNormalForm $! c
                unless
                  isNF
                  ( panic $
                      "ChainValidationState not in normal form at slot: "
                        <> show (cvsLastSlot c)
                  )
              NoAssertNF -> pure ()
            updateChainBoundary c bvd
          ABOBBlock block -> updateBlock config c block

    blockOrBoundarySlot :: ABlockOrBoundary a -> Maybe SlotNumber
    blockOrBoundarySlot = \case
      ABOBBoundary _ -> Nothing
      ABOBBlock block -> Just $ blockSlot block
