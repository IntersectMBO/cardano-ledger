{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Imp (spec) where

import Cardano.Ledger.Mary (MaryEra)
import qualified Test.Cardano.Ledger.Allegra.Imp as AllegraImp
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp.UtxoSpec as Utxo
import Test.Cardano.Ledger.Mary.ImpTest

spec :: forall era. (MaryEraImp era, EraSpecificSpec era) => Spec
spec = do
  AllegraImp.spec @era
  describe "MaryImpSpec" $
    withEachEraVersion @era $
      Utxo.spec

instance EraSpecificSpec MaryEra
