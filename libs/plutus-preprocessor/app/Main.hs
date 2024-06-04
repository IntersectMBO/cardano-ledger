-- | The 'main' function in this file writes a file
-- @libs\/cardano-ledger-core\/testlib\/Test\/Cardano\/Ledger\/Plutus\/Examples.hs@. When
-- this file is compiled it exports a bunch of Plutus Scripts.  Compiling that file does
-- not have any dependency on the plutus-plugin.  Instead this package
-- 'plutus-preprocessor' has that dependency, but one does not have to compile this
-- package to build the system.  If the plutus package changes, we will have to regenerate
-- the Examples.hs file.
-- To regenerate Examples.hs, on a machine that can depend upon plutus=plugin,
-- run 'cabal run plutus-preprocessor:plutus-preprocessor' using ghc = 9.6.x
module Main (main) where

import Cardano.Ledger.Plutus.Preprocessor (display)
import System.IO (IOMode (WriteMode), withFile)

main :: IO ()
main =
  withFile "libs/cardano-ledger-core/testlib/Test/Cardano/Ledger/Plutus/Examples.hs" WriteMode display
