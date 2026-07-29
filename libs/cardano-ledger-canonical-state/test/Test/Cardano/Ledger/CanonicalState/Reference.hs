{-# LANGUAGE OverloadedStrings #-}

-- | Module for loading and parsing reference CDDL specifications from CIP-0165
module Test.Cardano.Ledger.CanonicalState.Reference (
  parseReferenceCDDL,
  loadReferenceCDDL,
  allReferenceCDDLs,
  loadAllReferenceCDDLs,
  LoadError (..),
) where

import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (
  MonoReferenced,
  NameResolutionFailure,
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.IndexMappable (mapCDDLDropExt)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)

data LoadError
  = FailedToParseCDDL (ParseErrorBundle Text Void)
  | FailedToCompileCDDL NameResolutionFailure
  | FailedToLoadFile FilePath

instance Show LoadError where
  show (FailedToParseCDDL err) = "Failed to parse CDDL: " ++ errorBundlePretty err
  show (FailedToCompileCDDL err) = "Failed to compile CDDL: " ++ show err
  show (FailedToLoadFile path) = "Reference CDDL file not found: " ++ path

parseReferenceCDDL :: Text -> Text -> Either LoadError (CTreeRoot MonoReferenced)
parseReferenceCDDL namespace cddlText = do
  parsedCDDL <- case runParser pCDDL (T.unpack namespace) cddlText of
    Left err -> Left $ FailedToParseCDDL err
    Right c -> Right c

  case buildMonoCTree =<< buildResolvedCTree (buildRefCTree $ asMap $ mapCDDLDropExt parsedCDDL) of
    Left err -> Left $ FailedToCompileCDDL err
    Right c -> Right c

loadReferenceCDDL :: FilePath -> Text -> IO (Either LoadError (CTreeRoot MonoReferenced))
loadReferenceCDDL path namespace = do
  fileExists <- doesFileExist path
  if not fileExists
    then pure $ Left $ FailedToLoadFile path
    else do
      content <- TIO.readFile path
      pure $ parseReferenceCDDL namespace content

allReferenceCDDLs :: [(Text, FilePath)]
allReferenceCDDLs =
  -- This should match the CDDL files listed in CIP-0165
  [ ("utxo/v0", "utxo_v0.cddl")
  , ("blocks/v0", "blocks_v0.cddl")
  , ("entities/accounts/v0", "entities_accounts_v0.cddl")
  , ("entities/committee/v0", "entities_committee_v0.cddl")
  , ("entities/dreps/v0", "entities_dreps_v0.cddl")
  , ("entities/stake_pools/v0", "entities_stake_pools_v0.cddl")
  , ("entities/stake_pools/vrf_key_hashes/v0", "entities_stake_pools_vrf_key_hashes_v0.cddl")
  , ("gov/committee/v0", "gov_committee_v0.cddl")
  , ("gov/constitution/v0", "gov_constitution_v0.cddl")
  , ("gov/pparams/v0", "gov_pparams_v0.cddl")
  , ("gov/proposals/v0", "gov_proposals_v0.cddl")
  , ("gov/proposals/roots/v0", "gov_proposals_roots_v0.cddl")
  ]

loadAllReferenceCDDLs :: IO (Maybe [(Text, Either LoadError (CTreeRoot MonoReferenced))])
loadAllReferenceCDDLs = do
  mCddlDir <- lookupEnv "REFERENCE_CDDL_DIR"
  case mCddlDir of
    Nothing -> do
      pure Nothing
    Just cddlDir -> do
      results <- mapM (loadOne cddlDir) allReferenceCDDLs

      pure $ Just results
  where
    loadOne :: FilePath -> (Text, FilePath) -> IO (Text, Either LoadError (CTreeRoot MonoReferenced))
    loadOne cddlDir (ns, fileName) = do
      let path = cddlDir </> fileName
      result <- loadReferenceCDDL path ns
      pure (ns, result)
