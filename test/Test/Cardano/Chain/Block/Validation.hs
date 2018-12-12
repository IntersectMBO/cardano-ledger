{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Test.Cardano.Chain.Block.Validation
  ( tests
  )
where

import Cardano.Prelude

import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.String (fromString)
import Streaming (Of(..), Stream, hoist)
import qualified Streaming.Prelude as S

import Hedgehog
  ( Group(..)
  , Property
  , PropertyName
  , PropertyT
  , assert
  , checkParallel
  , checkSequential
  , discover
  , evalEither
  , forAll
  , property
  , withTests
  , (===)
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Block
  ( ABlockOrBoundary(..)
  , ChainValidationError
  , ChainValidationState
  , SigningHistory(..)
  , blockSlot
  , initialChainValidationState
  , updateChain
  , updateChainBoundary
  , updateSigningHistory
  )
import Cardano.Chain.Common (BlockCount(..), mkStakeholderId)
import Cardano.Chain.Epoch.File (ParseError, parseEpochFileWithBoundary)
import Cardano.Chain.Genesis as Genesis (Config(..), mkConfigFromFile)
import Cardano.Chain.Slotting (SlotId)
import Cardano.Crypto (PublicKey)

import Test.Cardano.Chain.Epoch.File (getEpochFiles)
import Test.Cardano.Crypto.Gen (genPublicKey)
import Test.Options (TestScenario(..))


-- | These tests perform chain validation over mainnet epoch files
--
--   We have chosen to split each epoch file into its own 'Property', because
--   this leads to a clearer log of progress during testing. This requires an
--   'IORef' to synchronise the 'ChainValidationState' between epochs, as
--   'Property's do not return values.
tests :: TestScenario -> IO Bool
tests scenario = do
  -- Get the 'Genesis.Config' from the mainnet genesis JSON
  config <-
    either
        (panic "TODO: Add buildable instance for Genesis.ConfigurationError")
        identity
      <$> runExceptT (mkConfigFromFile "test/mainnet-genesis.json" Nothing)

  -- Create an 'IORef' containing the initial 'ChainValidationState'
  cvsRef <-
    newIORef $ either (panic . show) identity $ initialChainValidationState
      config

  let
    takeFiles :: [FilePath] -> [FilePath]
    takeFiles = case scenario of
      ContinuousIntegration -> take 10
      Development           -> take 15
      QualityAssurance      -> identity

  -- Get a list of epoch files to perform validation on
  files <- takeFiles <$> getEpochFiles

  -- Validate the blocks of each epoch file in a single 'Property' and check
  -- them all sequentially
  let
    properties :: [(PropertyName, Property)]
    properties =
      zip (fromString <$> files) (epochValid config cvsRef <$> files)
  (&&)
    <$> checkSequential (Group "Test.Cardano.Chain.Block.Validation" properties)
    <*> checkParallel $$(discover)


data Error
  = ErrorParseError ParseError
  | ErrorChainValidationError (Maybe SlotId) ChainValidationError
  deriving (Eq, Show)


-- | Check that a single epoch's 'Block's are valid by folding over them
epochValid
  :: Genesis.Config -> IORef ChainValidationState -> FilePath -> Property
epochValid config cvsRef fp = withTests 1 . property $ do
  cvs <- liftIO $ readIORef cvsRef
  let stream = parseEpochFileWithBoundary fp
  result <- (liftIO . runResourceT . runExceptT)
    (foldChainValidationState config cvs $ S.map fst stream)
  newCvs <- evalEither result
  liftIO $ writeIORef cvsRef newCvs


-- | Fold chain validation over a 'Stream' of 'Blund's
foldChainValidationState
  :: Genesis.Config
  -> ChainValidationState
  -> Stream (Of (ABlockOrBoundary ByteString)) (ExceptT ParseError ResIO) ()
  -> ExceptT Error ResIO ChainValidationState
foldChainValidationState config cvs blocks = S.foldM_
  (\c b ->
    withExceptT (ErrorChainValidationError (blockOrBoundarySlot b)) $ case b of
      ABOBBoundary bvd   -> updateChainBoundary config c bvd
      ABOBBlock    block -> updateChain config c block
  )
  (pure cvs)
  pure
  (hoist (withExceptT ErrorParseError) blocks)
 where
  blockOrBoundarySlot :: ABlockOrBoundary a -> Maybe SlotId
  blockOrBoundarySlot = \case
    ABOBBoundary _     -> Nothing
    ABOBBlock    block -> Just $ blockSlot block


--------------------------------------------------------------------------------
-- SigningHistory
--------------------------------------------------------------------------------

-- | Check that updating a 'SigningHistory' maintains the invariants that:
--
--   - The map and sequence agree on the number of blocks signed by each
--     stakeholder
--   - The sequence never exceeds @k@ values
prop_signingHistoryUpdatesPreserveInvariants :: Property
prop_signingHistoryUpdatesPreserveInvariants =
  withTests 100
    . property
    $ do

        -- Generate a list of fake genesis stakeholders
        publicKeys <- forAll $ replicateM 7 genPublicKey
        let stakeholders = fmap mkStakeholderId publicKeys

        -- Generate a length for the 'SigningHistory'
        -- We don't use 'genBlockCount' as that would produce too large values
        k <- forAll $ BlockCount <$> Gen.word64 (Range.constant 1 100)

        let
          initialSigningHistory = SigningHistory
            { shK                 = k
            , shSigningQueue      = Seq.Empty
            , shStakeholderCounts = M.fromList $ fmap (, 0) stakeholders
            }

        -- Generate a list of signers with which to update the 'SigningHistory'
        signers <- forAll $ Gen.list
          (Range.constant 0 (2 * fromIntegral k))
          (Gen.element publicKeys)

        let
          updateAndCheckSigningHistory
            :: SigningHistory -> PublicKey -> PropertyT IO SigningHistory
          updateAndCheckSigningHistory sh s = do
            -- Update the  'SigningHistory'
            let sh' = updateSigningHistory s sh

            -- For each stakeholder the value in the map is the same as that in
            -- the sequence
            stakeholders `forM_` \s' -> do
              let
                stakeholderCount :: Int
                stakeholderCount = fromIntegral $ fromMaybe 0 $ M.lookup
                  s'
                  (shStakeholderCounts sh')
                stakeholderCount' =
                  length $ Seq.filter (== s') (shSigningQueue sh')
              stakeholderCount === stakeholderCount'

            -- The length of the overall sequence is less than or equal to 'k'
            assert $ length (shSigningQueue sh') <= fromIntegral (shK sh')

            pure sh'

        -- Check that at each stage the 'Map' and 'Seq' are in agreement and the
        -- sequence never exceeds 'k'
        foldM_ updateAndCheckSigningHistory initialSigningHistory signers
