{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (
  ConwayCertExecContext (..),
  nameEpoch,
  nameEnact,
  nameGovAction,
) where

import Cardano.Ledger.BaseTypes (EpochNo (..), Inject (..), Network, StrictMaybe (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (Era (..), EraPParams (..))
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  EnactState (..),
  GovAction (..),
  GovActionState (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  VotingProcedures,
  gasAction,
 )
import Cardano.Ledger.Conway.Rules (
  EnactSignal (..),
  committeeAcceptedRatio,
  dRepAcceptedRatio,
  spoAcceptedRatio,
 )
import Cardano.Ledger.Conway.Tx (AlonzoTx)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Constrained
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Lib as Agda
import Test.Cardano.Ledger.Common (Arbitrary (..))
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  OpaqueErrorString (..),
  computationResultToEither,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (ConwayExecEnactEnv)
import Test.Cardano.Ledger.Constrained.Conway (
  EpochExecEnv,
  IsConwayUniv,
  epochEnvSpec,
  epochSignalSpec,
  epochStateSpec,
  newEpochStateSpec,
  utxoEnvSpec,
  utxoStateSpec,
  utxoTxSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.Instances ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Imp.Common hiding (arbitrary, forAll, prop)

instance
  forall fn.
  IsConwayUniv fn =>
  ExecSpecRule fn "UTXO" Conway
  where
  environmentSpec _ = utxoEnvSpec

  stateSpec _ = utxoStateSpec

  signalSpec _ env st =
    utxoTxSpec env st
      <> constrained disableInlineDatums
    where
      disableInlineDatums :: Term fn (AlonzoTx Conway) -> Pred fn
      disableInlineDatums tx = match @fn tx $ \txBody _ _ _ ->
        match txBody $
          \_ctbSpendInputs
           _ctbCollateralInputs
           _ctbReferenceInputs
           ctbOutputs
           _ctbCollateralReturn
           _ctbTotalCollateral
           _ctbCerts
           _ctbWithdrawals
           _ctbTxfee
           _ctbVldt
           _ctbReqSignerHashes
           _ctbMint
           _ctbScriptIntegrityHash
           _ctbAdHash
           _ctbTxNetworkId
           _ctbVotingProcedures
           _ctbProposalProcedures
           _ctbCurrentTreasuryValue
           _ctbTreasuryDonation ->
              match ctbOutputs $
                \outs -> forAll' outs $
                  \txOut _ -> match txOut $
                    \_ _ dat _ ->
                      (caseOn dat)
                        (branch $ \_ -> True)
                        (branch $ \_ -> True)
                        (branch $ \_ -> False)

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.utxoStep env st sig

data ConwayCertExecContext era = ConwayCertExecContext
  { ccecWithdrawals :: !(Map (Network, Credential 'Staking (EraCrypto era)) Coin)
  , ccecVotes :: !(VotingProcedures era)
  }
  deriving (Generic, Eq, Show)

instance Era era => Arbitrary (ConwayCertExecContext era) where
  arbitrary =
    ConwayCertExecContext
      <$> arbitrary
      <*> arbitrary

instance
  c ~ EraCrypto era =>
  Inject
    (ConwayCertExecContext era)
    (Map (Network, Credential 'Staking c) Coin)
  where
  inject = ccecWithdrawals

instance Inject (ConwayCertExecContext era) (VotingProcedures era) where
  inject = ccecVotes

instance Era era => ToExpr (ConwayCertExecContext era)

data ConwayRatifyExecContext era = ConwayRatifyExecContext
  { crecTreasury :: Coin
  , crecGovActionMap :: [GovActionState era]
  }
  deriving (Generic, Eq, Show)

instance ToExpr (PParamsHKD StrictMaybe era) => ToExpr (ConwayRatifyExecContext era)

instance
  ( Era era
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  Arbitrary (ConwayRatifyExecContext era)
  where
  arbitrary =
    ConwayRatifyExecContext
      <$> arbitrary
      <*> arbitrary

instance Inject (ConwayRatifyExecContext era) Coin where
  inject = crecTreasury

instance
  Inject
    (ConwayRatifyExecContext era)
    [GovActionState era]
  where
  inject = crecGovActionMap

instance HasSimpleRep (ConwayRatifyExecContext era)
instance
  ( IsConwayUniv fn
  , EraPParams era
  , HasSpec fn (GovActionState era)
  ) =>
  HasSpec fn (ConwayRatifyExecContext era)

ratifyEnvSpec :: Specification fn (RatifyEnv era)
ratifyEnvSpec = TrueSpec

ratifyStateSpec ::
  IsConwayUniv fn =>
  ConwayRatifyExecContext Conway ->
  Specification fn (RatifyState Conway)
ratifyStateSpec _ =
  constrained $ \st ->
    match st $ \_ enacted expired _ ->
      mconcat
        [ assert $ enacted ==. lit mempty
        , assert $ expired ==. lit mempty
        ]

ratifySignalSpec ::
  IsConwayUniv fn =>
  ConwayRatifyExecContext Conway ->
  Specification fn (RatifySignal Conway)
ratifySignalSpec ConwayRatifyExecContext {crecGovActionMap} =
  constrained $ \sig ->
    match sig $ \gasS ->
      match gasS $ \gasL ->
        forAll gasL $ \gas ->
          gas `elem_` lit crecGovActionMap

instance IsConwayUniv fn => ExecSpecRule fn "RATIFY" Conway where
  type ExecContext fn "RATIFY" Conway = ConwayRatifyExecContext Conway

  genExecContext = arbitrary

  environmentSpec _ctx = ratifyEnvSpec

  stateSpec ctx _env = ratifyStateSpec ctx

  signalSpec ctx _env _st = ratifySignalSpec ctx

  runAgdaRule env st sig =
    first (\case {})
      . computationResultToEither
      $ Agda.ratifyStep env st sig

  extraInfo _ctx env@RatifyEnv {..} st (RatifySignal actions) =
    unlines . toList $ actionAcceptedRatio <$> actions
    where
      members = foldMap' (committeeMembers @Conway) $ ensCommittee (rsEnactState st)
      actionAcceptedRatio gas@GovActionState {..} =
        unlines
          [ "Acceptance ratios:"
          , "GovActionId: \t" <> showExpr gasId
          , "DRep: \t" <> show (dRepAcceptedRatio env gasDRepVotes (gasAction gas))
          , "CC: \t" <> show (committeeAcceptedRatio members gasCommitteeVotes reCommitteeState reCurrentEpoch)
          , "SPO: \t" <> show (spoAcceptedRatio env gas)
          ]

instance IsConwayUniv fn => ExecSpecRule fn "ENACT" Conway where
  type ExecEnvironment fn "ENACT" Conway = ConwayExecEnactEnv Conway
  type ExecState fn "ENACT" Conway = EnactState Conway
  type ExecSignal fn "ENACT" Conway = EnactSignal Conway

  environmentSpec _ = TrueSpec
  stateSpec _ _ = TrueSpec
  signalSpec _ _ _ = TrueSpec
  runAgdaRule env st sig =
    first (error "ENACT failed")
      . computationResultToEither
      $ Agda.enactStep env st sig

  classOf = Just . nameEnact

instance Inject (EpochExecEnv era) () where
  inject _ = ()

nameEnact :: EnactSignal era -> String
nameEnact (EnactSignal _ x) = nameGovAction x

nameGovAction :: GovAction era -> String
nameGovAction ParameterChange {} = "ParameterChange"
nameGovAction HardForkInitiation {} = "HardForkInitiation"
nameGovAction TreasuryWithdrawals {} = "TreasuryWithdrawals"
nameGovAction NoConfidence {} = "NoConfidence"
nameGovAction UpdateCommittee {} = "UpdateCommittee"
nameGovAction NewConstitution {} = "NewConstitution"
nameGovAction InfoAction {} = "InfoAction"

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "EPOCH" Conway
  where
  type ExecContext fn "EPOCH" Conway = [GovActionState Conway]
  type ExecEnvironment fn "EPOCH" Conway = EpochExecEnv Conway

  environmentSpec _ = epochEnvSpec

  stateSpec _ _ = epochStateSpec

  signalSpec _ _ _ = epochSignalSpec

  runAgdaRule env st sig =
    first (\case {})
      . computationResultToEither
      $ Agda.epochStep env st sig

  classOf = Just . nameEpoch

nameEpoch :: EpochNo -> String
nameEpoch x = show x

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "NEWEPOCH" Conway
  where
  type ExecContext fn "NEWEPOCH" Conway = [GovActionState Conway]
  type ExecEnvironment fn "NEWEPOCH" Conway = EpochExecEnv Conway

  environmentSpec _ = epochEnvSpec

  stateSpec _ _ = newEpochStateSpec

  signalSpec _ _ _ = epochSignalSpec

  runAgdaRule env st sig =
    first (\case {})
      . computationResultToEither
      $ Agda.newEpochStep env st sig
