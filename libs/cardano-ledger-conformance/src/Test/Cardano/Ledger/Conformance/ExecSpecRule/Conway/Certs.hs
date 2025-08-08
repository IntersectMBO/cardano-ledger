{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Certs () where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.State (CanSetAccounts (..), EraAccounts (..), EraCertState (..))
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((%~), (.~))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTranslate (..),
  runFromAgdaFunction,
  runSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (ConwayCertExecContext (..))

instance ExecSpecRule "CERTS" ConwayEra where
  type ExecContext "CERTS" ConwayEra = ConwayCertExecContext ConwayEra

  runAgdaRule = runFromAgdaFunction Agda.certsStep

  translateOutput ctx@ConwayCertExecContext {..} _ = runSpecTransM ctx . toSpecRep . fixRewards
    where
      -- This is necessary because the implementation zeroes out the rewards
      -- in the CERTS rule, but the spec does it in a different rule
      fixRewards =
        certDStateL . accountsL . accountsMapL
          %~ \m -> foldr' zeroRewards m . Set.map raCredential $ Map.keysSet ccecWithdrawals
      zeroRewards = Map.adjust (balanceAccountStateL .~ mempty)
