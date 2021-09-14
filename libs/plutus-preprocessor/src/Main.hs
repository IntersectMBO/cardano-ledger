{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The 'main' function in this file writes a file
--     'cardano-ledger-specs/alonzo/test/lib/Test/Cardano/Ledger/Alonzo/PlutusScripts.hs'
--     When this file is compiled it exports a bunch of Alonzo era scripts that are plutus scripts.
--     Compiling that file does not have any dependency on the plutus-plugin.
--     Instead this package 'plutus-preproccssor' has that dependency, but one does not have
--     to compile this package to build the system.
--     If the plutus package changes, we will have to regenerate the PlutusScripts.hs file.
--     To regenerate PlutusScripts.hs, on a machine that can depend upon plutus=plugin, then
--     cd into the plutus-preprocessor directory and type 'cabal run'
module Main where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (ShortByteString, pack, toShort, unpack)
import Flat (flat)
import Language.Haskell.TH
import Language.Haskell.TH.Ppr
import qualified Plutus.V1.Ledger.Api as P
import PlutusScripts
  ( evenRedeemerDecl,
    evenRedeemerDecl2Args,
    evendataDecl,
    guessDecl,
    guessDecl2args,
    oddRedeemerDecl,
    oddRedeemerDecl2Args,
    odddataDecl,
    redeemerIs10Decl2Args,
    sumsTo10Decl,
  )
import qualified PlutusTx as P (Data (..), compile)
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Prelude as P
import System.IO

-- =============================================
-- how to display a preprocessed script

display :: Handle -> Language -> ShortByteString -> Q [Dec] -> String -> IO ()
display h lang bytes code name = do
  xxx <- runQ code
  hPutStrLn h $ ("\n\n{- Preproceesed Plutus Script\n" ++ pprint xxx ++ "\n-}")
  hPutStr h $
    concat
      [ "\n",
        name,
        " :: Script era\n",
        name,
        " = (PlutusScript ",
        show lang,
        ". pack . concat)\n  ["
      ]
  manylines h 15 (unpack bytes)

manylines :: Show t => Handle -> Int -> [t] -> IO ()
manylines h n ts = write (split ts)
  where
    split [] = []
    split ts = take n ts : split (drop n ts)
    write [ts] = hPutStrLn h (show ts ++ "]")
    write (ts : tss) = do
      hPutStr h (show ts ++ ",\n   ")
      write tss

-- ==========================================================================
-- Turn the Template Haskell Decls into real haskell functions using Template
-- Haskell top-level splices, of TH.Decl imported from PlutusScripts. We use
-- this 2 step process (1. define elsewhere as a TH.Decl, 2. splice here) so that
-- can export the actual text of the plutus code as a comment in the generated file.

$guessDecl
$guessDecl2args
$evendataDecl
$evenRedeemerDecl
$odddataDecl
$oddRedeemerDecl
$sumsTo10Decl
$evenRedeemerDecl2Args
$oddRedeemerDecl2Args
$redeemerIs10Decl2Args

-- ================================================================
-- Compile the real functions as Plutus scripts, and get their
-- bytestring equaivalents. Here is where we depend on plutus-plugin.

guessTheNumberBytes :: ShortByteString
guessTheNumberBytes =
  toShort . toStrict . serialise . P.fromCompiledCode $
    $$(P.compile [||guessTheNumber'3||])

guess2args :: ShortByteString
guess2args =
  toShort . toStrict . serialise . P.fromCompiledCode $
    $$(P.compile [||guessTheNumber'2||])

evendataBytes :: ShortByteString
evendataBytes =
  toShort . toStrict . serialise . P.fromCompiledCode $
    $$(P.compile [||evendata'||])

evenRedeemerBytes :: ShortByteString
evenRedeemerBytes =
  toShort . toStrict . serialise . P.fromCompiledCode $
    $$(P.compile [||evenRedeemer'||])

odddataBytes :: ShortByteString
odddataBytes =
  toShort . toStrict . serialise . P.fromCompiledCode $
    $$(P.compile [||odddata'||])

oddRedeemerBytes :: ShortByteString
oddRedeemerBytes =
  toShort . toStrict . serialise . P.fromCompiledCode $
    $$(P.compile [||oddRedeemer'||])

sumsTo10Bytes :: ShortByteString
sumsTo10Bytes =
  toShort . toStrict . serialise . P.fromCompiledCode $
    $$(P.compile [||sumsTo10'||])

oddRedeemerBytes2Arg :: ShortByteString
oddRedeemerBytes2Arg =
  toShort . toStrict . serialise . P.fromCompiledCode $
    $$(P.compile [||oddRedeemer2'||])

evenRedeemerBytes2Args :: ShortByteString
evenRedeemerBytes2Args =
  toShort . toStrict . serialise . P.fromCompiledCode $
    $$(P.compile [||evenRedeemer2'||])

redeemerIs10Bytes2Args :: ShortByteString
redeemerIs10Bytes2Args =
  toShort . toStrict . serialise . P.fromCompiledCode $
    $$(P.compile [||redeemerIs102'||])

-- ========================================================================
-- Generate the PlutusScripts.hs which does not depend on plutus-plugin.
-- write out the file header (module and imports), then 'display' the result
-- for each plutus script.

main :: IO ()
main = do
  outh <- openFile "../alonzo/test/lib/Test/Cardano/Ledger/Alonzo/PlutusScripts.hs" WriteMode
  mapM_
    (hPutStrLn outh)
    [ "-- | This file is generated by plutus-preprocessor/src/Main.hs",
      "module Test.Cardano.Ledger.Alonzo.PlutusScripts where\n",
      "import Cardano.Ledger.Alonzo.Language (Language (..))",
      "import Cardano.Ledger.Alonzo.Scripts (CostModel (..), Script (..))",
      "import Data.ByteString.Short (pack)",
      "import Plutus.V1.Ledger.Api (defaultCostModelParams)\n",
      "defaultCostModel :: Maybe CostModel",
      "defaultCostModel = CostModel <$> defaultCostModelParams"
    ]
  display outh PlutusV1 guess2args guessDecl2args "guessTheNumber2"
  display outh PlutusV1 guessTheNumberBytes guessDecl "guessTheNumber3"
  display outh PlutusV1 evendataBytes evendataDecl "evendata3"
  display outh PlutusV1 odddataBytes odddataDecl "odddata3"
  display outh PlutusV1 evenRedeemerBytes evenRedeemerDecl "evenRedeemer3"
  display outh PlutusV1 oddRedeemerBytes oddRedeemerDecl "oddRedeemer3"
  display outh PlutusV1 sumsTo10Bytes sumsTo10Decl "sumsTo103"
  -- 2 arg plutPlutusV1 us scripts
  display outh PlutusV1 oddRedeemerBytes2Arg oddRedeemerDecl2Args "oddRedeemer2"
  display outh PlutusV1 evenRedeemerBytes2Args evenRedeemerDecl2Args "evenRedeemer2"
  display outh PlutusV1 redeemerIs10Bytes2Args redeemerIs10Decl2Args "redeemerIs102"
  display outh PlutusV2 guess2args guessDecl2args "guessTheNumber2V2"
  display outh PlutusV2 guessTheNumberBytes guessDecl "guessTheNumber3V2"
  hClose outh
