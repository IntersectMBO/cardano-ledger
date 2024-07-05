{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Codec.CBOR.Cuddle.CDDL (sortCDDL)
import Codec.CBOR.Cuddle.Huddle (Huddle, toCDDL)
import Codec.CBOR.Cuddle.Pretty ()
import Data.Foldable (for_)
import Prettyprinter (Pretty (pretty))
import Prettyprinter.Render.Text (hPutDoc)
import System.IO (IOMode (..), withFile)
import Test.Cardano.Ledger.Conway.CDDL qualified as Conway

-- | Map from spec to the CDDL file location
cuddleSpecs :: [(Huddle, String)]
cuddleSpecs =
  [ (Conway.conway, "./eras/conway/impl/cddl-files/conway-merged.cddl")
  ]

writeSpec :: Huddle -> String -> IO ()
writeSpec hddl path =
  let cddl = toCDDL hddl
   in withFile path WriteMode $ \h ->
        hPutDoc h (pretty $ sortCDDL cddl)

-- Generate cddl files for all relevant specifications
main :: IO ()
main = for_ cuddleSpecs (uncurry writeSpec)
