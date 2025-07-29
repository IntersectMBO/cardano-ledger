{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.TxInfoSpec (spec) where

import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Genesis ()

spec :: Spec
spec = do
  describe "TxInfo" $ do
    pure () -- TODO
