{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Constrained.Preds.Repl where

import Data.Char (toLower)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT, setComplete)
import System.Console.Haskeline.Completion (completeWord, simpleCompletion)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Size (seps)
import Test.Cardano.Ledger.Constrained.TypeRep

-- =====================================================

repl :: Proof era -> Env era -> IO ()
repl proof env = runInputT defaultSettings (loop proof env)

goRepl :: Proof era -> Env era -> String -> IO ()
goRepl p env@(Env emap) str = runInputT (setComplete comp defaultSettings) (helpRepl p env str)
  where
    keys = Set.toList (Map.keysSet emap)
    comp = completeWord Nothing [' '] (\s -> pure (map simpleCompletion (filter (List.isPrefixOf s) keys)))

loop :: Proof era -> Env era -> InputT IO ()
loop proof env@(Env emap) = do
  minput <- getInputLine "prompt> "
  case minput of
    Nothing -> return ()
    Just ":q" -> return ()
    Just "help" -> helpRepl proof env []
    Just "?" -> helpRepl proof env []
    Just (':' : 'p' : more) -> helpRepl proof env more
    Just str ->
      case Map.lookup (filter (/= ' ') str) emap of
        Nothing -> outputStrLn "Not found" >> loop proof env
        Just (Payload rep t _) -> do
          outputStrLn (str ++ " :: " ++ show rep)
          outputStrLn (format rep t)
          loop proof env

helpRepl :: forall era. Proof era -> Env era -> [Char] -> InputT IO ()
helpRepl proof env@(Env emap) more = do
  let keys = Set.toList (Map.keysSet emap)
      matches = filter (\k -> List.isInfixOf (map toLower more) (map toLower k)) keys
  if more == ""
    then
      outputStrLn
        ( unlines
            [ "\nEnter one of these Strings at the 'prompt>' to see its value."
            , "Type ':q' to exit."
            , "Type ':pXXX' to see variables whose name contains the substring 'XXX' (case insensitive)."
            , "Type 'help' or '?' to see these instructions.\n"
            ]
        )
    else pure ()
  outputStrLn (seps matches)
  outputStrLn ""
  loop proof env
