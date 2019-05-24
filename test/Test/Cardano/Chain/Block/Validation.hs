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
import Test.Cardano.Prelude

import Control.Monad.Trans.Resource (ResIO, runResourceT)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Streaming (Of(..), Stream, hoist)
import qualified Streaming.Prelude as S

import Hedgehog
  ( Property
  , PropertyT
  , assert
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
  , updateBlock
  , updateChainBoundary
  , updateSigningHistory
  )
import Cardano.Chain.Common (BlockCount(..), hashKey)
import Cardano.Chain.Epoch.File (ParseError, parseEpochFilesWithBoundary)
import Cardano.Chain.Genesis as Genesis (Config(..), configEpochSlots)
import Cardano.Chain.Slotting (FlatSlotId)
import Cardano.Crypto (VerificationKey)

import Test.Cardano.Chain.Config (readMainetCfg)
import Test.Cardano.Crypto.Gen (genVerificationKey)
import Test.Cardano.Mirror (mainnetEpochFiles)
import Test.Options (TestScenario(..), TSGroup, TSProperty, concatTSGroups)


-- | These tests perform chain validation over mainnet epoch files
tests :: TSGroup
tests = concatTSGroups [const $$discover, $$discoverPropArg]

ts_prop_mainnetEpochsValid :: TSProperty
ts_prop_mainnetEpochsValid scenario = withTests 1 . property $ do
  -- Get the 'Genesis.Config' from the mainnet genesis JSON
  config <- readMainetCfg

  -- Construct the initial 'ChainValidationState'
  let cvs = either (panic . show) identity $ initialChainValidationState config

  let
    takeFiles :: [FilePath] -> [FilePath]
    takeFiles = case scenario of
      ContinuousIntegration -> identity
      Development           -> take 15
      QualityAssurance      -> identity

  -- Get a list of epoch files to perform validation on
  files <- takeFiles <$> liftIO mainnetEpochFiles

  let stream = parseEpochFilesWithBoundary (configEpochSlots config) files

  result <- (liftIO . runResourceT . runExceptT)
    (foldChainValidationState config cvs stream)

  void $ evalEither result


data Error
  = ErrorParseError ParseError
  | ErrorChainValidationError (Maybe FlatSlotId) ChainValidationError
  deriving (Eq, Show)



-- | Fold chain validation over a 'Stream' of 'Block's
foldChainValidationState
  :: Genesis.Config
  -> ChainValidationState
  -> Stream (Of (ABlockOrBoundary ByteString)) (ExceptT ParseError ResIO) ()
  -> ExceptT Error ResIO ChainValidationState
foldChainValidationState config cvs blocks =
  S.foldM_ validate (pure cvs) pure (hoist (withExceptT ErrorParseError) blocks)
 where
  validate
     :: Monad m
     => ChainValidationState
     -> ABlockOrBoundary ByteString
     -> ExceptT Error m ChainValidationState
  validate c b =
    withExceptT (ErrorChainValidationError (blockOrBoundarySlot b)) $
      case b of
        ABOBBoundary bvd   -> updateChainBoundary c bvd
        ABOBBlock    block -> updateBlock config c block

  blockOrBoundarySlot :: ABlockOrBoundary a -> Maybe FlatSlotId
  blockOrBoundarySlot = \case
    ABOBBoundary _     -> Nothing
    ABOBBlock    block -> Just $ blockSlot block


--------------------------------------------------------------------------------
-- SigningHistory
--------------------------------------------------------------------------------

-- | Check that updating a 'SigningHistory' maintains the invariants that:
--
--   - The map and sequence agree on the number of blocks signed by each
--     keyHash
--   - The sequence never exceeds @k@ values
prop_signingHistoryUpdatesPreserveInvariants :: Property
prop_signingHistoryUpdatesPreserveInvariants =
  withTests 100
    . property
    $ do

        -- Generate a list of fake genesis keyHashes
        verificationKeys <- forAll $ replicateM 7 genVerificationKey
        let keyHashes = fmap hashKey verificationKeys

        -- Generate a length for the 'SigningHistory'
        -- We don't use 'genBlockCount' as that would produce too large values
        k <- forAll $ BlockCount <$> Gen.word64 (Range.constant 1 100)

        let
          initialSigningHistory = SigningHistory
            { shK = k
            , shSigningQueue = Seq.Empty
            , shKeyHashCounts = M.fromList $ fmap (, BlockCount 0) keyHashes
            }

        -- Generate a list of signers with which to update the 'SigningHistory'
        signers <- forAll $ Gen.list
          (Range.constant 0 (fromIntegral $ 2 * unBlockCount k))
          (Gen.element verificationKeys)

        let
          updateAndCheckSigningHistory
            :: SigningHistory -> VerificationKey -> PropertyT IO SigningHistory
          updateAndCheckSigningHistory sh s = do
            -- Update the  'SigningHistory'
            let sh' = updateSigningHistory s sh

            -- For each keyHash the value in the map is the same as that in
            -- the sequence
            keyHashes `forM_` \s' -> do
              let
                keyHashCount :: Int
                keyHashCount = maybe 0 (fromIntegral . unBlockCount) $ M.lookup
                  s'
                  (shKeyHashCounts sh')
                keyHashCount' =
                  length $ Seq.filter (== s') (shSigningQueue sh')
              keyHashCount === keyHashCount'

            -- The length of the overall sequence is less than or equal to 'k'
            assert $ length (shSigningQueue sh') <= fromIntegral (unBlockCount $ shK sh')

            pure sh'

        -- Check that at each stage the 'Map' and 'Seq' are in agreement and the
        -- sequence never exceeds 'k'
        foldM_ updateAndCheckSigningHistory initialSigningHistory signers
