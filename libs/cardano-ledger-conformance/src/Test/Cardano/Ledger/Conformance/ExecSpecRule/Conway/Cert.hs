{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (
  ConwayCertExecContext (..),
) where

import Cardano.Ledger.Address (AccountAddress)
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (Era)
import Cardano.Ledger.Conway.Governance (VotingProcedures)
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Common (Arbitrary (..), ToExpr)
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), runFromAgdaFunction)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base ()

data ConwayCertExecContext era
  = ConwayCertExecContext
  { ccecWithdrawals :: !(Map AccountAddress Coin)
  , ccecVotes :: !(VotingProcedures era)
  }
  deriving (Generic, Eq, Show)

instance Era era => NFData (ConwayCertExecContext era)

instance Inject (ConwayCertExecContext era) (Map AccountAddress Coin) where
  inject = ccecWithdrawals

instance Inject (ConwayCertExecContext era) (VotingProcedures era) where
  inject = ccecVotes

instance Era era => Arbitrary (ConwayCertExecContext era) where
  arbitrary =
    ConwayCertExecContext
      <$> arbitrary
      <*> arbitrary

instance Era era => EncCBOR (ConwayCertExecContext era) where
  encCBOR x@(ConwayCertExecContext _ _) =
    let ConwayCertExecContext {..} = x
     in encode $
          Rec ConwayCertExecContext
            !> To ccecWithdrawals
            !> To ccecVotes

instance Era era => DecCBOR (ConwayCertExecContext era) where
  decCBOR =
    decode $
      RecD ConwayCertExecContext
        <! From
        <! From

instance Era era => ToExpr (ConwayCertExecContext era)

instance ExecSpecRule "CERT" ConwayEra where
  type ExecContext "CERT" ConwayEra = ConwayCertExecContext ConwayEra

  runAgdaRule = runFromAgdaFunction Agda.certStep
