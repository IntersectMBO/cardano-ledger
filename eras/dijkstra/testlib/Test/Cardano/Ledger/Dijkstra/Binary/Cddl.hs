module Test.Cardano.Ledger.Dijkstra.Binary.Cddl (
  readDijkstraCddlFileNames,
  readDijkstraCddlFiles,
) where

import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_dijkstra

readDijkstraCddlFileNames :: IO [FilePath]
readDijkstraCddlFileNames = do
  base <- getDataFileName "cddl-files/dijkstra.cddl"
  pure [base]

readDijkstraCddlFiles :: IO [BSL.ByteString]
readDijkstraCddlFiles = mapM BSL.readFile =<< readDijkstraCddlFileNames
