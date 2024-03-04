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
import Data.ByteString.Short (ShortByteString, unpack)
import Data.Foldable (forM_)
import Language.Haskell.TH
import qualified PlutusV1Scripts as PV1S
import qualified PlutusV3Scripts as PV3S
import ScriptSource
import System.IO

-- =============================================
-- how to display a preprocessed script

display :: Handle -> (Language -> ShortByteString) -> Q [Dec] -> String -> IO ()
display h scriptBytesFun code name = do
  xxx <- runQ code
  hPutStrLn h ("\n\n{- Preproceesed Plutus Script\n" ++ pprint xxx ++ "\n-}")
  hPutStr h $
    concat
      [ "\n"
      , name ++ " :: SLanguage l -> Plutus l\n"
      , name ++ " = Plutus . PlutusBinary . pack . \n  (\\case\n"
      ]
  forM_ [PlutusV1 .. PlutusV3] $ \lang -> do
    hPutStrLn h $ "    S" <> show lang <> " -> " <> show (unpack (scriptBytesFun lang))
  hPutStrLn h "  )"

manylines :: Show t => Handle -> Int -> [t] -> IO ()
manylines h n ts' = write (split ts')
  where
    split [] = []
    split ts = take n ts : split (drop n ts)
    write [] = pure ()
    write [ts] = hPutStrLn h (show ts ++ "]")
    write (ts : tss) = do
      hPutStr h (show ts ++ ",\n   ")
      write tss

-- ========================================================================
-- Generate the PlutusScripts.hs which does not depend on plutus-plugin.
-- write out the file header (module and imports), then 'display' the result
-- for each plutus script.

displayScripts :: Handle -> IO ()
displayScripts outh = do
  let
    scripts =
      [
        ( \case
            PlutusV1 -> PV1S.alwaysSucceeds2argsBytes
            PlutusV2 -> PV1S.alwaysSucceeds2argsBytes
            PlutusV3 -> PV3S.alwaysSucceeds2argsBytes
        , alwaysSucceedsDecl2args
        , "alwaysSucceeds2"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.alwaysSucceeds3argsBytes
            PlutusV2 -> PV1S.alwaysSucceeds3argsBytes
            PlutusV3 -> PV3S.alwaysSucceeds3argsBytes
        , alwaysSucceedsDecl3args
        , "alwaysSucceeds3"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.alwaysFails2argsBytes
            PlutusV2 -> PV1S.alwaysFails2argsBytes
            PlutusV3 -> PV3S.alwaysFails2argsBytes
        , alwaysFailsDecl2args
        , "alwaysFails2"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.alwaysFails3argsBytes
            PlutusV2 -> PV1S.alwaysFails3argsBytes
            PlutusV3 -> PV3S.alwaysFails3argsBytes
        , alwaysFailsDecl3args
        , "alwaysFails3"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.guess2args
            PlutusV2 -> PV1S.guess2args
            PlutusV3 -> PV3S.guess2args
        , guessDecl2args
        , "guessTheNumber2"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.guessTheNumberBytes
            PlutusV2 -> PV1S.guessTheNumberBytes
            PlutusV3 -> PV3S.guessTheNumberBytes
        , guessDecl
        , "guessTheNumber3"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.evendataBytes
            PlutusV2 -> PV1S.evendataBytes
            PlutusV3 -> PV3S.evendataBytes
        , evendataDecl
        , "evendata3"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.odddataBytes
            PlutusV2 -> PV1S.odddataBytes
            PlutusV3 -> PV3S.odddataBytes
        , odddataDecl
        , "odddata3"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.evenRedeemerBytes
            PlutusV2 -> PV1S.evenRedeemerBytes
            PlutusV3 -> PV3S.evenRedeemerBytes
        , evenRedeemerDecl
        , "evenRedeemer3"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.oddRedeemerBytes
            PlutusV2 -> PV1S.oddRedeemerBytes
            PlutusV3 -> PV3S.oddRedeemerBytes
        , oddRedeemerDecl
        , "oddRedeemer3"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.sumsTo10Bytes
            PlutusV2 -> PV1S.sumsTo10Bytes
            PlutusV3 -> PV3S.sumsTo10Bytes
        , sumsTo10Decl
        , "sumsTo103"
        )
      , -- 2 arg plutus scripts

        ( \case
            PlutusV1 -> PV1S.oddRedeemerBytes2Arg
            PlutusV2 -> PV1S.oddRedeemerBytes2Arg
            PlutusV3 -> PV3S.oddRedeemerBytes2Arg
        , oddRedeemerDecl2Args
        , "oddRedeemer2"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.evenRedeemerBytes2Args
            PlutusV2 -> PV1S.evenRedeemerBytes2Args
            PlutusV3 -> PV3S.evenRedeemerBytes2Args
        , evenRedeemerDecl2Args
        , "evenRedeemer2"
        )
      ,
        ( \case
            PlutusV1 -> PV1S.redeemerIs10Bytes2Args
            PlutusV2 -> PV1S.redeemerIs10Bytes2Args
            PlutusV3 -> PV3S.redeemerIs10Bytes2Args
        , redeemerIs10Decl2Args
        , "redeemerIs102"
        )
      ]
  forM_ scripts $ \(scriptBytesFun, scriptArgs, name) -> do
    display outh scriptBytesFun scriptArgs name

main :: IO ()
main = do
  outh <- openFile "libs/cardano-ledger-core/testlib/Test/Cardano/Ledger/Plutus/Examples.hs" WriteMode
  hPutStrLn outh $
    unlines
      [ "{-# LANGUAGE DataKinds #-}"
      , "{-# LANGUAGE LambdaCase #-}"
      , "{-# LANGUAGE GADTs #-}"
      , "-- | This file is generated by \"plutus-preprocessor:plutus-preprocessor\""
      , "module Test.Cardano.Ledger.Plutus.Examples where"
      , ""
      , "import Cardano.Ledger.Plutus.Language (SLanguage (..), Plutus (..), PlutusBinary (..))"
      , "import Data.ByteString.Short (pack)"
      ]
  displayScripts outh
  hClose outh
