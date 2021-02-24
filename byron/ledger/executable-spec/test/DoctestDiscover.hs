{-# LANGUAGE CPP #-}

#if !MIN_VERSION_GLASGOW_HASKELL(8,10,0,0) || MIN_VERSION_GLASGOW_HASKELL(8,11,0,0)
{-# OPTIONS_GHC -F -pgmF doctest-discover #-}
#else
module Main where

import qualified System.IO as IO

main :: IO ()
main = IO.putStrLn "WARNING: doctest will not run on GHC 8.10.x.\nSee https://gitlab.haskell.org/ghc/ghc/-/issues/19421"
#endif
