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

import Cardano.Ledger.Address (accountAddressCredentialL)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.State (CanSetAccounts (..), EraAccounts (..), EraCertState (..))
import Control.State.Transition.Extended (TRC (..))
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((%~), (.~))
import Lens.Micro.Extras (view)
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (ConwayCertExecContext (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  SpecTRC (..),
  runFromAgdaFunction,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  askSpecTransM,
  withCtxSpecTransM,
 )

instance ExecSpecRule "CERTS" ConwayEra where
  type ExecContext "CERTS" ConwayEra = ConwayCertExecContext ConwayEra

  translateInputs (TRC (env, st, sig)) = do
    ConwayCertExecContext {..} <- askSpecTransM
    agdaEnv <- withCtxSpecTransM (ccecVotes, ccecWithdrawals) $ toSpecRep env
    agdaSt <- withCtxSpecTransM () $ toSpecRep st
    agdaSig <- withCtxSpecTransM () $ toSpecRep sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig

  runAgdaRule = runFromAgdaFunction Agda.certsStep

  translateOutput _ st = do
    ConwayCertExecContext {..} <- askSpecTransM
    let
      -- This is necessary because the implementation zeroes out the rewards
      -- in the CERTS rule, but the spec does it in a different rule
      zeroRewards = Map.adjust (balanceAccountStateL .~ mempty)
      fixRewards =
        certDStateL . accountsL . accountsMapL
          %~ \m ->
            foldr' zeroRewards m . Set.map (view accountAddressCredentialL) $
              Map.keysSet ccecWithdrawals
    withCtxSpecTransM () $ toSpecRep $ fixRewards st
