{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The 'main' function in this file writes a file
-- @libs\/cardano-ledger-core\/testlib\/Test\/Cardano\/Ledger\/Plutus\/Examples.hs@. When
-- this file is compiled it exports a bunch of Plutus Scripts.  Compiling that file does
-- not have any dependency on the plutus-plugin.  Instead this package
-- 'plutus-preprocessor' has that dependency, but one does not have to compile this
-- package to build the system.  If the plutus package changes, we will have to regenerate
-- the Examples.hs file.
-- To regenerate Examples.hs, on a machine that can depend upon plutus=plugin,
-- run 'cabal run plutus-preprocessor'
module Main where

import Cardano.Ledger.Plutus.Language (Language (..))
import Data.ByteString.Short as SBS (ShortByteString, fromShort)
import Data.Foldable (forM_)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Language.Haskell.TH
import qualified PlutusV1Scripts as V1
import qualified PlutusV1Scripts as V2
import qualified PlutusV3Scripts as V3
import System.IO
import Test.Cardano.Ledger.Binary.TreeDiff (showHexBytesGrouped)

-- =============================================
-- how to display a preprocessed script

display :: Handle -> [String] -> Map.Map String (Language -> (Q [Dec], ShortByteString)) -> IO ()
display h scriptNames scripts = do
  let indent = ("  " ++)
  hPutStrLn h "class PlutusLanguage l => PlutusTestScript (l :: Language) where"
  forM_ scriptNames $ \scriptName -> do
    hPutStrLn h $ indent scriptName ++ " :: SLanguage l -> Plutus l"
  hPutStrLn h ""
  forM_ [minBound .. maxBound] $ \lang -> do
    hPutStrLn h $ "instance PlutusTestScript '" ++ show lang ++ " where"
    forM_ scriptNames $ \scriptName -> do
      let (scriptQ, scriptBytes) = (scripts Map.! scriptName) lang
      compiledScript <- runQ scriptQ
      hPutStr h $
        unlines $
          [ indent "-- Preproceesed " ++ show lang ++ " Script:"
          , indent "-- @@@"
          ]
            ++ map (indent . ("-- " ++)) (lines (pprint compiledScript))
            ++ [ indent "-- @@@"
               , indent scriptName ++ " _slang ="
               , indent . indent $ "decodeHexPlutus . mconcat $"
               , indent . indent . indent $
                  let sep = (("\n" ++) . indent . indent . indent $ ", ")
                      hexChunks = map show $ showHexBytesGrouped 128 (SBS.fromShort scriptBytes)
                   in "[ " ++ intercalate sep hexChunks
               , indent . indent . indent $ "]"
               , ""
               ]

-- ========================================================================
-- Generate the PlutusScripts.hs which does not depend on plutus-plugin.
-- write out the file header (module and imports), then 'display' the result
-- for each plutus script.

displayScripts :: Handle -> IO ()
displayScripts outHandle = do
  let
    scriptNames = fst <$> scripts
    scripts =
      [
        ( "alwaysSucceedsNoDatum"
        , \case
            PlutusV1 -> V1.alwaysSucceedsNoDatumBytes
            PlutusV2 -> V2.alwaysSucceedsNoDatumBytes
            PlutusV3 -> V3.alwaysSucceedsNoDatumBytes
        )
      ,
        ( "alwaysSucceedsWithDatum"
        , \case
            PlutusV1 -> V1.alwaysSucceedsWithDatumBytes
            PlutusV2 -> V2.alwaysSucceedsWithDatumBytes
            PlutusV3 -> V3.alwaysSucceedsWithDatumBytes
        )
      ,
        ( "alwaysFailsNoDatum"
        , \case
            PlutusV1 -> V1.alwaysFailsNoDatumBytes
            PlutusV2 -> V2.alwaysFailsNoDatumBytes
            PlutusV3 -> V3.alwaysFailsNoDatumBytes
        )
      ,
        ( "alwaysFailsWithDatum"
        , \case
            PlutusV1 -> V1.alwaysFailsWithDatumBytes
            PlutusV2 -> V2.alwaysFailsWithDatumBytes
            PlutusV3 -> V3.alwaysFailsWithDatumBytes
        )
      ,
        ( "redeemerSameAsDatum"
        , \case
            PlutusV1 -> V1.redeemerSameAsDatumBytes
            PlutusV2 -> V2.redeemerSameAsDatumBytes
            PlutusV3 -> V3.redeemerSameAsDatumBytes
        )
      ,
        ( "evenDatum"
        , \case
            PlutusV1 -> V1.evenDatumBytes
            PlutusV2 -> V2.evenDatumBytes
            PlutusV3 -> V3.evenDatumBytes
        )
      ,
        ( "evenRedeemerNoDatum"
        , \case
            PlutusV1 -> V1.evenRedeemerNoDatumBytes
            PlutusV2 -> V2.evenRedeemerNoDatumBytes
            PlutusV3 -> V3.evenRedeemerNoDatumBytes
        )
      ,
        ( "evenRedeemerWithDatum"
        , \case
            PlutusV1 -> V1.evenRedeemerWithDatumBytes
            PlutusV2 -> V2.evenRedeemerWithDatumBytes
            PlutusV3 -> V3.evenRedeemerWithDatumBytes
        )
      ]
  display outHandle scriptNames (Map.fromList scripts)

main :: IO ()
main = do
  withFile "libs/cardano-ledger-core/testlib/Test/Cardano/Ledger/Plutus/Examples.hs" WriteMode $ \outHandle -> do
    hPutStrLn outHandle $
      unlines
        [ "{-# LANGUAGE DataKinds #-}"
        , "{-# LANGUAGE GADTs #-}"
        , "{-# LANGUAGE KindSignatures #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "-- | This file is generated by \"plutus-preprocessor:plutus-preprocessor\""
        , "module Test.Cardano.Ledger.Plutus.Examples where"
        , ""
        , "import Cardano.Ledger.Plutus.Language ("
        , "  Language (..),"
        , "  Plutus (..),"
        , "  PlutusBinary (..),"
        , "  PlutusLanguage,"
        , " )"
        , "import Data.ByteString (ByteString)"
        , "import qualified Data.ByteString.Base16 as Base16 (decode)"
        , "import qualified Data.ByteString.Short as SBS (toShort)"
        , "import GHC.Stack"
        , ""
        , "decodeHexPlutus :: HasCallStack => ByteString -> Plutus l"
        , "decodeHexPlutus = either error (Plutus . PlutusBinary . SBS.toShort) . Base16.decode"
        ]
    displayScripts outHandle
