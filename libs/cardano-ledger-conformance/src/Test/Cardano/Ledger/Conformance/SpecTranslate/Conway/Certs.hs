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
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Data.Map (keysSet)
import Data.Map.Strict (Map)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()

instance SpecTranslate ConwayEra (Conway.CertsEnv ConwayEra) where
  type SpecRep ConwayEra (Conway.CertsEnv ConwayEra) = Agda.CertEnv
  type
    SpecContext ConwayEra (Conway.CertsEnv ConwayEra) =
      (VotingProcedures ConwayEra, Map AccountAddress Coin)
  toSpecRep Conway.CertsEnv {..} = do
    (votes, withdrawals) <- askSpecTransM
    let ccColdCreds = foldMap (keysSet . committeeMembers) certsCurrentCommittee
    withCtxSpecTransM () $
      Agda.MkCertEnv
        <$> toSpecRep certsCurrentEpoch
        <*> toSpecRep certsPParams
        <*> toSpecRep votes
        <*> toSpecRepMap withdrawals
        <*> toSpecRep ccColdCreds
