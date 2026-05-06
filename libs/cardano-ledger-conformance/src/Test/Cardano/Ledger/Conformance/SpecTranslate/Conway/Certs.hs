{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Certs () where

import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Data.Functor.Identity (Identity)
import Data.Map (keysSet)
import Data.Map.Strict (Map)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()

instance
  ( SpecTranslate (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecContext (PParamsHKD Identity era) ~ ()
  ) =>
  SpecTranslate (CertsEnv era)
  where
  type SpecRep (CertsEnv era) = Agda.CertEnv
  type SpecContext (CertsEnv era) = (VotingProcedures era, Map AccountAddress Coin)
  toSpecRep CertsEnv {..} = do
    (votes, withdrawals) <- askSpecTransM
    let ccColdCreds = foldMap (keysSet . committeeMembers) certsCurrentCommittee
    withSpecTransM (const ()) $
      Agda.MkCertEnv
        <$> toSpecRep certsCurrentEpoch
        <*> toSpecRep certsPParams
        <*> toSpecRep votes
        <*> toSpecRepMap withdrawals
        <*> toSpecRep ccColdCreds
