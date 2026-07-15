{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Cardano.Ledger.CanonicalState.Namespace.CDDL (knownNamespaces)
import Cardano.SCLS.NamespaceSymbol (
  KnownSpec (namespaceSpec),
  SomeNamespaceSymbol (SomeNamespaceSymbol),
  toString,
 )
import Codec.CBOR.Cuddle.CDDL (CDDL)
import qualified Codec.CBOR.Cuddle.Huddle as Cuddle
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (mapIndex))
import Codec.CBOR.Cuddle.Pretty (PrettyStage)
import Control.Monad (forM_)
import qualified Data.Text as T
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (hPutDoc)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((<.>), (</>))
import System.IO

main :: IO ()
main =
  getArgs >>= \case
    [dir] -> do
      createDirectoryIfMissing True dir
      forM_ knownNamespaces $ \ns@(SomeNamespaceSymbol p) -> do
        writeSpec (namespaceSpec p) (dir </> T.unpack (T.replace "/" "_" (T.pack (toString ns))) <.> "cddl")
    _ -> error "Usage: gen-cddl directory"

writeSpec :: Cuddle.Huddle -> FilePath -> IO ()
writeSpec hddl path =
  let cddl :: CDDL PrettyStage = mapIndex $ Cuddle.toCDDLNoRoot hddl
      preface = "; This file was auto-generated from huddle. Please do not modify it directly!\n"
   in withFile path WriteMode $ \h -> do
        hPutStrLn h preface
        hPutDoc h (pretty cddl)
        -- Write an empty line at the end of the file
        hPutStrLn h ""
