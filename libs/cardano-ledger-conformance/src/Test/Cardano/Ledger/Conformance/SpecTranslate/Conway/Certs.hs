{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Certs () where

import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Conway.TreeDiff

instance
  ToExpr (PredicateFailure (EraRule "CERT" era)) =>
  SpecTranslate ctx (ConwayCertsPredFailure era)
  where
  type SpecRep (ConwayCertsPredFailure era) = OpaqueErrorString
  toSpecRep = pure . OpaqueErrorString . show . toExpr
