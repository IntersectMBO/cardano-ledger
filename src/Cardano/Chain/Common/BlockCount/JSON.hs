{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Common.BlockCount.JSON (unToGet) where

import Cardano.Prelude

unToGet :: [Char] -> [Char]
unToGet "unBlockCount" = "getBlockCount"
unToGet x = x
