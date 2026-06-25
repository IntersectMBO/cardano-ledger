{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (
  ConwayCertExecContext (..),
) where

import Cardano.Ledger.Address (AccountAddress)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (Era)
import Cardano.Ledger.Conway.Governance (VotingProcedures)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (TRC (..))
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Common (Arbitrary (..), ToExpr)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base ()
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

data ConwayCertExecContext era
  = ConwayCertExecContext
  { ccecWithdrawals :: !(Map AccountAddress Coin)
  , ccecVotes :: !(VotingProcedures era)
  }
  deriving (Generic, Eq, Show)

instance Era era => NFData (ConwayCertExecContext era)

instance Era era => Arbitrary (ConwayCertExecContext era) where
  arbitrary =
    ConwayCertExecContext
      <$> arbitrary
      <*> arbitrary

instance Era era => EncCBOR (ConwayCertExecContext era) where
  encCBOR x@(ConwayCertExecContext _ _) =
    let ConwayCertExecContext {..} = x
     in encodeListLen 2
          <> encCBOR ccecWithdrawals
          <> encCBOR ccecVotes

instance Era era => DecCBOR (ConwayCertExecContext era) where
  decCBOR =
    decodeRecordNamed "ConwayCertExecContext" (const 2) $
      ConwayCertExecContext <$> decCBOR <*> decCBOR

instance Era era => ToExpr (ConwayCertExecContext era)

instance ExecSpecRule "CERT" ConwayEra where
  type ExecContext "CERT" ConwayEra = ConwayCertExecContext ConwayEra

  translateInputs (TRC (env, st, sig)) = do
    ConwayCertExecContext {..} <- askSpecTransM
    agdaEnv <- withCtxSpecTransM (ccecVotes, ccecWithdrawals) $ toSpecRep env
    agdaSt <- withCtxSpecTransM () $ toSpecRep st
    agdaSig <- withCtxSpecTransM () $ toSpecRep sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig

  runAgdaRule = runFromAgdaFunction Agda.certStep
