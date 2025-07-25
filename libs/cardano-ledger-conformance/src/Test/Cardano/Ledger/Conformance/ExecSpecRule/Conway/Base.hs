{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (
  ConwayCertExecContext (..),
  ConwayRatifyExecContext (..),
  crecTreasuryL,
  crecGovActionMapL,
  externalFunctions,
  whoDelegatesSpec,
) where

import Cardano.Crypto.DSIGN (SignedDSIGN (..), verifySignedDSIGN)
import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  Inject (..),
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (DecCBOR (decCBOR), EncCBOR (encCBOR))
import Cardano.Ledger.Binary.Coders (Decode (From, RecD), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  EnactState (..),
  GovActionState (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  VotingProcedures,
  ensProtVerL,
  gasAction,
  rsEnactStateL,
 )
import Cardano.Ledger.Conway.Rules (
  committeeAccepted,
  committeeAcceptedRatio,
  dRepAccepted,
  dRepAcceptedRatio,
  spoAccepted,
  spoAcceptedRatio,
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (DSIGN, VKey (..))
import Control.State.Transition.Extended (TRC (..))
import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Set (Set)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import qualified Prettyprinter as PP
import Test.Cardano.Ledger.Binary.TreeDiff (tableDoc)
import Test.Cardano.Ledger.Common (Arbitrary (..))
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTranslate (..),
  integerToHash,
  runSpecTransM,
  unComputationResult,
  unComputationResult_,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (SpecTRC (..))
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway (vkeyFromInteger)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (
  signatureFromInteger,
 )
import Test.Cardano.Ledger.Constrained.Conway.Epoch
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs (whoDelegatesSpec)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof (Reflect)
import Test.Cardano.Ledger.Imp.Common hiding (arbitrary, forAll, prop, var, witness)
import Test.Cardano.Ledger.Conway.ImpTest ()

-- ======================================================================

data ConwayCertExecContext era
  = -- | The VState and the DState (which contains the AccountState) are subtly related.
    --   VState {vsDReps :: !(Map (Credential DRepRole) DRepState) ..}
    --   ConwayAccounts :: {caStates :: Map(Credential Staking) (ConwayAccountState era)}
    --   we maintain this subtle relationship by storing the ccecDelagatees field in this context.
    --   See Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs(whoDelegatesSpec) and its uses
    --   in that module.  whoDelegatesSpec :: WitUniv era -> Specification (Map (Credential 'DRepRole) (Set (Credential 'Staking)))
    ConwayCertExecContext
    { ccecWithdrawals :: !(Map RewardAccount Coin)
    , ccecVotes :: !(VotingProcedures era)
    , ccecDelegatees :: !(Map (Credential 'DRepRole) (Set (Credential 'Staking)))
    }
  deriving (Generic, Eq, Show)

instance Era era => Arbitrary (ConwayCertExecContext era) where
  arbitrary =
    ConwayCertExecContext
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Era era => EncCBOR (ConwayCertExecContext era) where
  encCBOR x@(ConwayCertExecContext _ _ _) =
    let ConwayCertExecContext {..} = x
     in encode $
          Rec ConwayCertExecContext
            !> To ccecWithdrawals
            !> To ccecVotes
            !> To ccecDelegatees

instance Reflect era => DecCBOR (ConwayCertExecContext era) where
  decCBOR =
    decode $
      RecD ConwayCertExecContext
        <! From
        <! From
        <! From

instance Inject (ConwayCertExecContext era) (Map RewardAccount Coin) where
  inject = ccecWithdrawals

instance Inject (ConwayCertExecContext era) (VotingProcedures era) where
  inject = ccecVotes

instance Era era => ToExpr (ConwayCertExecContext era)

instance Era era => NFData (ConwayCertExecContext era)

data ConwayRatifyExecContext era = ConwayRatifyExecContext
  { crecTreasury :: Coin
  , crecGovActionMap :: [GovActionState era]
  }
  deriving (Generic, Eq, Show)

crecTreasuryL :: Lens' (ConwayRatifyExecContext era) Coin
crecTreasuryL = lens crecTreasury (\x y -> x {crecTreasury = y})

crecGovActionMapL :: Lens' (ConwayRatifyExecContext era) [GovActionState era]
crecGovActionMapL = lens crecGovActionMap (\x y -> x {crecGovActionMap = y})

instance EraPParams era => EncCBOR (ConwayRatifyExecContext era) where
  encCBOR x@(ConwayRatifyExecContext _ _) =
    let ConwayRatifyExecContext {..} = x
     in encode $
          Rec ConwayRatifyExecContext
            !> To crecTreasury
            !> To crecGovActionMap

instance EraPParams era => DecCBOR (ConwayRatifyExecContext era) where
  decCBOR =
    decode $
      RecD ConwayRatifyExecContext
        <! From
        <! From

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

  shrink = genericShrink

instance Inject (ConwayRatifyExecContext era) Coin where
  inject = crecTreasury

instance
  Inject
    (ConwayRatifyExecContext era)
    [GovActionState era]
  where
  inject = crecGovActionMap

instance EraPParams era => NFData (ConwayRatifyExecContext era)

instance ExecSpecRule "RATIFY" ConwayEra where
  type ExecContext "RATIFY" ConwayEra = ConwayRatifyExecContext ConwayEra

  runAgdaRule (SpecTRC env st sig) = unComputationResult_ $ Agda.ratifyStep env st sig

  extraInfo _ ctx (TRC (env@RatifyEnv {..}, st, sig@(RatifySignal actions))) _ =
    PP.vsep $ specExtraInfo : (actionAcceptedRatio <$> toList actions)
    where
      members = foldMap' (committeeMembers @ConwayEra) $ ensCommittee (rsEnactState st)
      showAccepted True = PP.brackets "✓"
      showAccepted False = PP.brackets "×"
      showRatio r = PP.viaShow (numerator r) <> "/" <> PP.viaShow (denominator r)
      specExtraInfo =
        PP.vsep
          [ "Spec extra info:"
          , either PP.viaShow PP.pretty . runSpecTransM ctx $
              Agda.ratifyDebug
                <$> toSpecRep env
                <*> toSpecRep st
                <*> toSpecRep sig
          ]
      pv = st ^. rsEnactStateL . ensProtVerL
      actionAcceptedRatio gas@GovActionState {..} =
        tableDoc
          (Just "GovActionState")
          [
            ( "GovActionId:"
            , PP.line <> PP.indent 2 (ansiExpr gasId)
            )
          ,
            ( "SPO:"
            , showAccepted (spoAccepted env st gas)
                PP.<+> showRatio (spoAcceptedRatio env gas pv)
            )
          ,
            ( "DRep:"
            , showAccepted (dRepAccepted env st gas)
                PP.<+> showRatio (dRepAcceptedRatio env gasDRepVotes (gasAction gas))
            )
          ,
            ( "CC:"
            , showAccepted (committeeAccepted env st gas)
                PP.<+> showRatio (committeeAcceptedRatio members gasCommitteeVotes reCommitteeState reCurrentEpoch)
            )
          ]

newtype ConwayEnactExecContext era = ConwayEnactExecContext
  { ceecMaxTerm :: EpochInterval
  }
  deriving (Generic)

instance Arbitrary (ConwayEnactExecContext era) where
  arbitrary = ConwayEnactExecContext <$> arbitrary

instance NFData (ConwayEnactExecContext era)

instance ToExpr (ConwayEnactExecContext era)

instance Era era => EncCBOR (ConwayEnactExecContext era) where
  encCBOR (ConwayEnactExecContext x) = encCBOR x

instance ExecSpecRule "ENACT" ConwayEra where
  type ExecContext "ENACT" ConwayEra = ConwayEnactExecContext ConwayEra
  type SpecEnvironment "ENACT" ConwayEra = Agda.EnactEnv

  translateInputs = undefined

  runAgdaRule (SpecTRC env st sig) = unComputationResult $ Agda.enactStep env st sig

instance Inject (EpochExecEnv era) () where
  inject _ = ()

instance ExecSpecRule "EPOCH" ConwayEra where
  type ExecContext "EPOCH" ConwayEra = [GovActionState ConwayEra]

  runAgdaRule (SpecTRC env st sig) = unComputationResult_ $ Agda.epochStep env st sig

instance ExecSpecRule "NEWEPOCH" ConwayEra where
  type ExecContext "NEWEPOCH" ConwayEra = [GovActionState ConwayEra]

  runAgdaRule (SpecTRC env st sig) = unComputationResult_ $ Agda.newEpochStep env st sig

externalFunctions :: Agda.ExternalFunctions
externalFunctions = Agda.MkExternalFunctions {..}
  where
    extIsSigned vk ser sig =
      isRight $
        verifySignedDSIGN
          @DSIGN
          @(Hash HASH ByteString)
          ()
          vkey
          hash
          signature
      where
        vkey =
          unVKey
            . fromMaybe (error "Failed to convert an Agda VKey to a Haskell VKey")
            $ vkeyFromInteger vk
        hash =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show ser)
            $ integerToHash ser
        signature =
          SignedDSIGN
            . fromMaybe
              (error "Failed to decode the signature")
            $ signatureFromInteger sig
