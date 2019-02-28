module Main where

import Control.Monad (void)

import qualified Control.State.Transition.Examples.RegistryModel as RegistryModel

main :: IO ()
main = void $ RegistryModel.tests
