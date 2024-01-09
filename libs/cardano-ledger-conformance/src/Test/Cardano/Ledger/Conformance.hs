{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conformance (
  SpecTranslate (..),
  spec,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), SignedDSIGN (..))
import Cardano.Crypto.Hash (Hash, hashToBytes)
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..), serialiseAddr)
import Cardano.Ledger.Alonzo (AlonzoTxAuxData)
import Cardano.Ledger.Alonzo.PParams (OrdExUnits (OrdExUnits))
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  CostModels,
  ExUnits (..),
  Prices,
  Tag (..),
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoTxWits (..),
  RdmrPtr (..),
  Redeemers (..),
  TxDats (..),
 )
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (
  Anchor,
  EpochInterval (..),
  EpochNo (..),
  ProtVer (..),
  SlotNo (..),
  StrictMaybe (..),
  TxIx (..),
  getVersion,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (GovAction (..), ProposalProcedure (..), VotingProcedures (..))
import Cardano.Ledger.Conway.PParams (ConwayPParams (..), THKD (..))
import Cardano.Ledger.Conway.TxBody (proposalProceduresTxBodyL, treasuryDonationTxBodyL, votingProceduresTxBodyL)
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  ConwayGovCert (..),
  ConwayTxCert (..),
  getStakePoolDelegatee,
  getVoteDelegatee,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), VKey (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import Cardano.Ledger.Plutus.Data (Data, Datum (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeHash, extractHash)
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  UTxOState (..),
  curPParamsEpochStateL,
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesEsL,
  totalObligation,
  utxosDepositedL,
  utxosGovStateL,
  utxosUtxoL,
 )
import Cardano.Ledger.Shelley.Rules (Identity, UtxoEnv (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Control.Monad.RWS.Class (asks, gets, modify)
import Control.State.Transition.Extended (STS (..))
import Data.Bitraversable (bimapM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Coerce (Coercible, coerce)
import Data.Data (Typeable)
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..))
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Semigroup (Sum (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Lens.Micro (to, (.~), (^.))
import Lens.Micro.Extras (view)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance.Orphans ()
import Test.Cardano.Ledger.Constrained.Preds.Tx (genTxAndNewEpoch)
import Test.Cardano.Ledger.Constrained.Preds.Universes (UnivSize (..))
import Test.Cardano.Ledger.Conway.ImpTest (
  ImpTestEnv (..),
  ImpTestM,
  ImpTestState,
  evalImpTestM,
  fixupFees,
  getsNES,
  impAnn,
  impLastTickL,
  logCurPParams,
  logEntry,
  modifyNES,
  trySubmitTx,
  withImpState,
  withNoFixup,
 )
import Test.Cardano.Ledger.Generic.Proof (Reflect (..))
import Test.Cardano.Ledger.Imp.Common

type SpecTranslationError = Text

class SpecTranslate a where
  type SpecRep a
  type TestRep a
  type TestRep a = SpecRep a

  toSpecRep :: a -> Either SpecTranslationError (SpecRep a)

  specToTestRep :: SpecRep a -> TestRep a
  default specToTestRep :: Coercible (SpecRep a) (TestRep a) => SpecRep a -> TestRep a
  specToTestRep = coerce

  toTestRep :: a -> Either SpecTranslationError (TestRep a)
  toTestRep x = specToTestRep @a <$> toSpecRep x

instance SpecTranslate TxIx where
  type SpecRep TxIx = Integer

  toSpecRep (TxIx x) = pure $ toInteger x

instance SpecTranslate (TxIn era) where
  type SpecRep (TxIn era) = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRep (txId, txIx)

instance SpecTranslate (Addr era) where
  type SpecRep (Addr era) = Agda.Addr

  toSpecRep = pure . byteStringToInteger . serialiseAddr

instance SpecTranslate (SafeHash c EraIndependentData) where
  type SpecRep (SafeHash c EraIndependentData) = Agda.DataHash

  toSpecRep _ = pure ()

instance SpecTranslate (SafeHash c EraIndependentScriptIntegrity) where
  type SpecRep (SafeHash c EraIndependentScriptIntegrity) = Agda.Hash

  toSpecRep = toSpecRep . extractHash

instance SpecTranslate (SafeHash c EraIndependentTxBody) where
  type SpecRep (SafeHash c EraIndependentTxBody) = Agda.Hash

  toSpecRep = toSpecRep . extractHash

instance EraTxOut era => SpecTranslate (BabbageTxOut era) where
  type SpecRep (BabbageTxOut era) = Agda.TxOut

  toSpecRep (BabbageTxOut addr val datum _) =
    let mkTxOut x = do
          addr' <- toSpecRep addr
          coin' <- toSpecRep $ coin val
          pure (addr', (coin', x))
     in case datum of
          NoDatum -> mkTxOut Nothing
          DatumHash h -> mkTxOut . Just =<< toSpecRep h
          Datum _ -> Left "Inline datums not supported by spec"

instance SpecTranslate Integer where
  type SpecRep Integer = Integer

  toSpecRep = pure

deriving instance SpecTranslate Coin

instance
  ( SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  ) =>
  SpecTranslate (UTxO era)
  where
  type SpecRep (UTxO era) = SpecRep (Map (TxIn (EraCrypto era)) (TxOut era))
  toSpecRep (UTxO m) = toSpecRep m
  specToTestRep = L.sortOn fst

instance
  ( SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  ) =>
  SpecTranslate (UTxOState era)
  where
  type SpecRep (UTxOState era) = Agda.UTxOState

  toSpecRep x =
    Agda.MkUTxOState
      <$> toSpecRep (utxosUtxo x)
      <*> toSpecRep (utxosFees x)

  specToTestRep x@(Agda.MkUTxOState _ _) =
    let Agda.MkUTxOState {..} = x
     in Agda.MkUTxOState (specToTestRep @(UTxO era) utxo) (specToTestRep @Coin fees)

deriving instance SpecTranslate SlotNo

deriving instance SpecTranslate EpochNo

deriving instance SpecTranslate EpochInterval

instance SpecTranslate ProtVer where
  type SpecRep ProtVer = (Integer, Integer)

  toSpecRep (ProtVer ver minor) = pure (getVersion ver, toInteger minor)

data VotingThresholds
  = VotingThresholds
      DRepVotingThresholds
      PoolVotingThresholds

instance SpecTranslate VotingThresholds where
  type SpecRep VotingThresholds = ()

  toSpecRep _ = pure ()

instance SpecTranslate CostModels where
  type SpecRep CostModels = Agda.AgdaEmpty

  toSpecRep _ =
    pure $
      error "TODO change this to unit once it is fixed in the spec"

instance SpecTranslate Prices where
  type SpecRep Prices = ()

  toSpecRep _ = pure ()

instance SpecTranslate ExUnits where
  type SpecRep ExUnits = Agda.ExUnits

  toSpecRep (ExUnits a b) = pure (toInteger a, toInteger b)

deriving instance SpecTranslate OrdExUnits

deriving instance SpecTranslate CoinPerByte

instance SpecTranslate (HKD f a) => SpecTranslate (THKD r f a) where
  type SpecRep (THKD r f a) = SpecRep (HKD f a)

  toSpecRep = toSpecRep . unTHKD

instance SpecTranslate (ConwayPParams Identity era) where
  type SpecRep (ConwayPParams Identity era) = Agda.PParams

  toSpecRep x =
    Agda.MkPParams
      <$> toSpecRep (cppMinFeeA x)
      <*> toSpecRep (cppMinFeeB x)
      <*> Right (toInteger . unTHKD $ cppMaxBBSize x)
      <*> Right (toInteger . unTHKD $ cppMaxTxSize x)
      <*> Right (toInteger . unTHKD $ cppMaxBHSize x)
      <*> Right (toInteger . unTHKD $ cppMaxValSize x)
      <*> Right 0
      <*> toSpecRep (cppPoolDeposit x)
      <*> toSpecRep (cppEMax x)
      <*> toSpecRep (cppProtocolVersion x)
      <*> toSpecRep
        ( VotingThresholds
            (unTHKD $ cppDRepVotingThresholds x)
            (unTHKD $ cppPoolVotingThresholds x)
        )
      <*> toSpecRep (cppGovActionLifetime x)
      <*> toSpecRep (cppGovActionDeposit x)
      <*> toSpecRep (cppDRepDeposit x)
      <*> toSpecRep (cppDRepActivity x)
      <*> Right (toInteger . unTHKD $ cppCommitteeMinSize x)
      <*> Right (toInteger . unEpochInterval . unTHKD $ cppCommitteeMaxTermLength x)
      -- This is the minimumAVS field which we ended up not needing. It will be
      -- removed from the spec.
      <*> Right (error "minimumAVS is not in Conway, do not use this field")
      <*> toSpecRep (cppCostModels x)
      <*> toSpecRep (cppPrices x)
      <*> toSpecRep (cppMaxTxExUnits x)
      <*> toSpecRep (cppMaxBlockExUnits x)
      <*> toSpecRep (cppCoinsPerUTxOByte x)
      <*> Right (toInteger . unTHKD $ cppMaxCollateralInputs x)

instance
  SpecTranslate (PParamsHKD Identity era) =>
  SpecTranslate (PParams era)
  where
  type SpecRep (PParams era) = SpecRep (PParamsHKD Identity era)

  toSpecRep (PParams x) = toSpecRep x

instance
  ( SpecTranslate (PParams era)
  , SpecRep (PParams era) ~ Agda.PParams
  ) =>
  SpecTranslate (UtxoEnv era)
  where
  type SpecRep (UtxoEnv era) = Agda.UTxOEnv

  toSpecRep x =
    Agda.MkUTxOEnv
      <$> toSpecRep (ueSlot x)
      <*> toSpecRep (uePParams x)

instance (SpecTranslate a, Ord (SpecRep a)) => SpecTranslate (Set a) where
  type SpecRep (Set a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . Set.toList
  specToTestRep = L.sort
  toTestRep = toSpecRep

instance SpecTranslate a => SpecTranslate (StrictSeq a) where
  type SpecRep (StrictSeq a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate a => SpecTranslate (Sized a) where
  type SpecRep (Sized a) = SpecRep a

  toSpecRep (Sized x _) = toSpecRep x

instance SpecTranslate ValidityInterval where
  type SpecRep ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRep (lo, hi)

instance SpecTranslate (Hash a b) where
  type SpecRep (Hash a b) = Agda.Hash

  toSpecRep = pure . byteStringToInteger . hashToBytes

deriving instance SpecTranslate (KeyHash r c)

instance Crypto c => SpecTranslate (VKey k c) where
  type SpecRep (VKey k c) = Integer

  toSpecRep (VKey x) = pure . byteStringToInteger $ rawSerialiseVerKeyDSIGN x

instance DSIGNAlgorithm v => SpecTranslate (SignedDSIGN v a) where
  type SpecRep (SignedDSIGN v a) = Integer

  toSpecRep (SignedDSIGN x) =
    pure . byteStringToInteger $ rawSerialiseSigDSIGN x

instance (Crypto c, Typeable k) => SpecTranslate (WitVKey k c) where
  type SpecRep (WitVKey k c) = (Integer, Integer)

  toSpecRep (WitVKey vk sk) = toSpecRep (vk, sk)

instance Era era => SpecTranslate (TxDats era) where
  type SpecRep (TxDats era) = [(Agda.DataHash, Agda.Datum)]

  toSpecRep (TxDats x) = toSpecRep x

instance
  ( SpecTranslate k
  , SpecTranslate v
  , Ord (SpecRep k)
  ) =>
  SpecTranslate (Map k v)
  where
  type SpecRep (Map k v) = [(SpecRep k, SpecRep v)]

  toSpecRep = traverse (bimapM toSpecRep toSpecRep) . Map.toList
  specToTestRep = L.sortOn fst
  toTestRep = toSpecRep

instance SpecTranslate Tag where
  type SpecRep Tag = Agda.Tag

  toSpecRep Spend = pure Agda.Spend
  toSpecRep Mint = pure Agda.Mint
  toSpecRep Cert = pure Agda.Cert
  toSpecRep Rewrd = pure Agda.Rewrd

instance SpecTranslate Word64 where
  type SpecRep Word64 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate Word32 where
  type SpecRep Word32 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate RdmrPtr where
  type SpecRep RdmrPtr = Agda.RdmrPtr

  toSpecRep (RdmrPtr t x) = toSpecRep (t, x)

instance (SpecTranslate a, SpecTranslate b) => SpecTranslate (a, b) where
  type SpecRep (a, b) = (SpecRep a, SpecRep b)

  toSpecRep (x, y) = (,) <$> toSpecRep x <*> toSpecRep y

instance SpecTranslate (Data era) where
  type SpecRep (Data era) = ()

  toSpecRep _ = pure ()

instance Era era => SpecTranslate (Redeemers era) where
  type SpecRep (Redeemers era) = [(Agda.RdmrPtr, (Agda.Redeemer, Agda.ExUnits))]

  toSpecRep (Redeemers x) = toSpecRep x

instance
  AlonzoEraScript era =>
  SpecTranslate (AlonzoTxWits era)
  where
  type SpecRep (AlonzoTxWits era) = Agda.TxWitnesses

  toSpecRep x =
    Agda.MkTxWitnesses
      <$> toSpecRep (txwitsVKey x)
      <*> pure []
      <*> toSpecRep (txdats x)
      <*> toSpecRep (txrdmrs x)

instance SpecTranslate a => SpecTranslate (StrictMaybe a) where
  type SpecRep (StrictMaybe a) = Maybe (SpecRep a)

  toSpecRep = toSpecRep . strictMaybeToMaybe

instance SpecTranslate a => SpecTranslate (Maybe a) where
  type SpecRep (Maybe a) = Maybe (SpecRep a)

  toSpecRep = traverse toSpecRep

instance SpecTranslate (AlonzoTxAuxData era) where
  type SpecRep (AlonzoTxAuxData era) = Agda.AuxiliaryData

  toSpecRep _ = pure ()

deriving instance SpecTranslate (ScriptHash c)

instance SpecTranslate (Credential k c) where
  type SpecRep (Credential k c) = Agda.Credential

  toSpecRep (KeyHashObj h) = Agda.KeyHashObj <$> toSpecRep h
  toSpecRep (ScriptHashObj h) = Agda.ScriptObj <$> toSpecRep h

instance SpecTranslate (RewardAcnt c) where
  type SpecRep (RewardAcnt c) = SpecRep (Credential 'Staking c)

  toSpecRep = toSpecRep . getRwdCred

instance SpecTranslate (PoolParams era) where
  type SpecRep (PoolParams era) = Agda.PoolParams

  toSpecRep = toSpecRep . ppRewardAcnt

instance SpecTranslate (DRep c) where
  type SpecRep (DRep c) = Agda.VDeleg

  toSpecRep (DRepCredential c) = Agda.CredVoter Agda.DRep <$> toSpecRep c
  toSpecRep DRepAlwaysAbstain = pure Agda.AbstainRep
  toSpecRep DRepAlwaysNoConfidence = pure Agda.NoConfidenceRep

instance SpecTranslate (Anchor c) where
  type SpecRep (Anchor c) = Agda.Anchor
  toSpecRep _ = pure ()

instance SpecTranslate (ConwayTxCert era) where
  type SpecRep (ConwayTxCert era) = Agda.TxCert

  toSpecRep (ConwayTxCertDeleg (ConwayRegCert _ _)) = Left "RegCert not supported"
  toSpecRep (ConwayTxCertDeleg (ConwayUnRegCert _ _)) = Left "UnRegCert not supported"
  toSpecRep (ConwayTxCertDeleg (ConwayDelegCert c d)) =
    Agda.Delegate
      <$> toSpecRep c
      <*> toSpecRep (getVoteDelegatee d)
      <*> toSpecRep (KeyHashObj <$> getStakePoolDelegatee d)
      <*> pure 0
  toSpecRep (ConwayTxCertDeleg (ConwayRegDelegCert s d c)) =
    Agda.Delegate
      <$> toSpecRep s
      <*> toSpecRep (getVoteDelegatee d)
      <*> toSpecRep (KeyHashObj <$> getStakePoolDelegatee d)
      <*> toSpecRep c
  toSpecRep (ConwayTxCertPool (RegPool p@PoolParams {ppId})) =
    Agda.RegPool
      <$> toSpecRep (KeyHashObj ppId)
      <*> toSpecRep p
  toSpecRep (ConwayTxCertPool (RetirePool kh e)) =
    Agda.RetirePool
      <$> toSpecRep (KeyHashObj kh)
      <*> toSpecRep e
  toSpecRep (ConwayTxCertGov (ConwayRegDRep c d _)) =
    Agda.RegDRep
      <$> toSpecRep c
      <*> toSpecRep d
      <*> pure () -- TODO Are the anchors supposed to be optional?
  toSpecRep (ConwayTxCertGov (ConwayUnRegDRep c _)) =
    Agda.DeRegDRep
      <$> toSpecRep c
  toSpecRep (ConwayTxCertGov (ConwayUpdateDRep c _)) =
    Agda.RegDRep
      <$> toSpecRep c
      <*> pure 0
      <*> pure () -- TODO Are the anchors supposed to be optional?
  toSpecRep (ConwayTxCertGov (ConwayAuthCommitteeHotKey c h)) =
    Agda.CCRegHot
      <$> toSpecRep c
      <*> toSpecRep (SJust h)
  toSpecRep (ConwayTxCertGov (ConwayResignCommitteeColdKey c _)) =
    Agda.CCRegHot
      <$> toSpecRep c
      <*> toSpecRep (SNothing @(Credential _ _))

deriving instance SpecTranslate (TxId era)

toAgdaTxBody ::
  ( SpecRep (TxOut era) ~ Agda.TxOut
  , SpecRep (TxCert era) ~ Agda.TxCert
  , EraTx era
  , AlonzoEraTxBody era
  , SpecTranslate (TxOut era)
  , SpecTranslate (TxCert era)
  ) =>
  Tx era ->
  Either SpecTranslationError Agda.TxBody
toAgdaTxBody tx =
  Agda.MkTxBody
    <$> toSpecRep (tx ^. bodyTxL . inputsTxBodyL)
    <*> (zip [0 ..] <$> toSpecRep (tx ^. bodyTxL . outputsTxBodyL))
    <*> toSpecRep (tx ^. bodyTxL . feeTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . vldtTxBodyL)
    <*> pure (tx ^. sizeTxF)
    <*> toSpecRep (txid $ tx ^. bodyTxL)
    <*> toSpecRep (tx ^. bodyTxL . collateralInputsTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . reqSignerHashesTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . scriptIntegrityHashTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . certsTxBodyL)

instance
  ( SpecTranslate (TxWits era)
  , SpecTranslate (TxAuxData era)
  , SpecTranslate (TxOut era)
  , SpecTranslate (TxCert era)
  , SpecRep (TxWits era) ~ Agda.TxWitnesses
  , SpecRep (TxAuxData era) ~ Agda.AuxiliaryData
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecRep (TxCert era) ~ Agda.TxCert
  , Tx era ~ AlonzoTx era
  , EraTx era
  , AlonzoEraTxBody era
  ) =>
  SpecTranslate (AlonzoTx era)
  where
  type SpecRep (AlonzoTx era) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> toAgdaTxBody @era tx
      <*> toSpecRep (wits tx)
      <*> toSpecRep (auxiliaryData tx)

trySubmitTxConform_ :: Tx Conway -> ImpTestM Conway ()
trySubmitTxConform_ = void . trySubmitTxConform

trySubmitTxConform ::
  Tx Conway ->
  ImpTestM
    Conway
    ( Either
        [PredicateFailure (EraRule "LEDGER" Conway)]
        (TxId (EraCrypto Conway))
    )
trySubmitTxConform txPreFixup = do
  nes <- getsNES id
  doFixup <- asks iteDoTxFixup
  tx <-
    if doFixup
      then fixupFees txPreFixup
      else pure txPreFixup
  let
  agdaUtxoState <- expectRight . toSpecRep $ nes ^. nesEsL . esLStateL . lsUTxOStateL
  agdaTx <- expectRight $ toSpecRep tx
  lastTick <- gets $ view impLastTickL
  pParams <- getsNES $ nesEsL . curPParamsEpochStateL
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  let
    utxoEnv =
      UtxoEnv
        { ueSlot = lastTick
        , uePParams = pParams
        , ueCertState = certState
        }
  agdaUtxoEnv <- expectRight $ toSpecRep utxoEnv
  let
    newAgdaUtxoState = Agda.utxoStep agdaUtxoEnv agdaUtxoState agdaTx
  -- We already applied the fixup above, so no point doing it again
  submitRes <- withNoFixup $ trySubmitTx tx
  newUtxoState <- getsNES $ nesEsL . esLStateL . lsUTxOStateL
  let
    finalAgdaState :: Maybe Agda.UTxOState
    finalAgdaState =
      specToTestRep @(UTxOState Conway) <$> newAgdaUtxoState
    initialState = Just . toExpr $ specToTestRep @(UTxOState Conway) agdaUtxoState
  finalImplState :: Maybe Agda.UTxOState <- do
    impAnn "Failed to submit transaction on the implementation side" $
      expectRightDeep_ submitRes
    pure . either (const Nothing) Just $ toTestRep newUtxoState
  unless (finalAgdaState == finalImplState) $ do
    logEntry $
      "Changes in spec Utxo state:\n"
        ++ diffExprCompact (toExpr initialState) (toExpr finalAgdaState)
    logEntry $
      "Changes in impl Utxo state:\n"
        ++ diffExprCompact (toExpr initialState) (toExpr finalImplState)
    expectationFailure "The final states of spec and implementation do not match"
  pure submitRes

conformsWhen :: Bool -> (NewEpochState Conway -> Tx Conway -> Bool) -> ImpTestState Conway -> Property
conformsWhen makeTestsSmall condition impState =
  let
    smallerExamples x =
      x
        { usNumPreUtxo = 3
        , usNumColUtxo = 3
        , usMinInputs = 1
        , usMaxInputs = 3
        , usMinCollaterals = 1
        , usMaxCollaterals = 3
        }
    usNecessary =
      def
        { -- Not supported by the spec:
          usRegKeyFreq = 0
        , usUnRegKeyFreq = 0
        , usDatumFreq = 0
        , usGenerateWithdrawals = False
        , -- Ensures there's no double pool registration:
          usMinCerts = 0
        , usMaxCerts = 4
        , usAllowReRegisterPool = False
        , -- Not supported by the implementation:
          usSpendScriptFreq = 0
        , usCredScriptFreq = 0
        }
    us
      | makeTestsSmall = smallerExamples usNecessary
      | otherwise = usNecessary
    gen = genTxAndNewEpoch us $ reify @Conway
   in
    forAllBlind gen $ \(nes, tx, _env) ->
      condition nes tx
        ==> evalImpTestM impState
        $ do
          modify (impLastTickL @Conway .~ 100)
          modifyNES $ const nes
          logCurPParams
          -- logNESSummary
          logTxSummary tx
          withNoFixup $ trySubmitTxConform_ tx

txIsValid :: AlonzoEraTx era => Tx era -> Bool
txIsValid tx = (tx ^. isValidTxL) == IsValid True

spec :: HasCallStack => Spec
spec = describe "Conway conformance tests" . withImpState $ do
  it "Simple tx" $ do
    do
      certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
      govState <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
      totalObligation certState govState `shouldBe` zero
    do
      deposited <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL
      deposited `shouldBe` zero
    res <- trySubmitTxConform $ mkBasicTx mkBasicTxBody
    expectRightDeep_ res
  describe "Constrained generator" $ do
    it "UTXO conforms with small examples when IsValid True" $ conformsWhen True (const txIsValid)
    it "UTXO conforms with large examples when IsValid True" $ conformsWhen False (const txIsValid)

logTxSummary :: Tx Conway -> ImpTestM Conway ()
logTxSummary tx = do
  UTxO utxo <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
  let
    txb = tx ^. bodyTxL
    ins = toList $ txb ^. inputsTxBodyL
    collIns = toList $ txb ^. collateralInputsTxBodyL
    outs = toList $ txb ^. outputsTxBodyL
    collRet = txb ^. collateralReturnTxBodyL
    showTxId = ("  " ++) . showExpr . byteStringToInteger . hashToBytes . extractHash
    txInToOut txIn = fromJust (Map.lookup txIn utxo)
    showScript (BabbageTxOut _ _ _ script) =
      case script of
        SJust (TimelockScript _) -> "(Timelock)"
        SJust (PlutusScript _) -> "(Plutus)"
        SNothing -> ""
    showTxIn txIn@(TxIn (TxId txId) (TxIx i)) = showTxId txId ++ " #" ++ show i ++ showScript (txInToOut txIn)
    bodyHash = hashAnnotated txb
    showTxOut :: BabbageTxOut Conway -> Int -> [Char]
    showTxOut txOut i = "  " ++ show valCoin ++ " (" ++ show (getSum $ foldMap (Sum . sum) maMap) ++ ") #" ++ show @Int i ++ " " ++ showScript txOut
      where
        MaryValue valCoin (MultiAsset maMap) = txOut ^. valueTxOutL
    showProposal (ProposalProcedure {pProcGovAction}) =
      "  " ++ case pProcGovAction of
        ParameterChange {} -> "ParameterChange"
        HardForkInitiation {} -> "HardForkInitiation"
        TreasuryWithdrawals {} -> "TreasuryWithdrawals"
        NoConfidence {} -> "NoConfidence"
        UpdateCommittee {} -> "UpdateCommittee"
        NewConstitution {} -> "NewConstitution"
        InfoAction {} -> "InfoAction"
    showProposals = showProposal <$> toList (txb ^. proposalProceduresTxBodyL)
    showVotes = show <$> Map.toList (txb ^. votingProceduresTxBodyL . to unVotingProcedures)
    showCerts = ("  " ++) . show <$> toList (txb ^. certsTxBodyL)
  logEntry . unlines $
    concat
      [ pure ""
      , pure "bodyHash:"
      , pure $ showTxId bodyHash
      , pure "inputs:"
      , showTxIn <$> ins
      , pure "collateral inputs:"
      , showTxIn <$> collIns
      , pure "outputs:"
      , zipWith showTxOut outs [0 ..]
      , pure "collateral return:"
      , case collRet of
          SJust cr -> pure . showTxOut cr $ length outs
          SNothing -> pure "no collateral return"
      , pure "certs:"
      , showCerts
      , pure "proposals:"
      , showProposals
      , pure "votes:"
      , showVotes
      , pure $ "donation: " ++ show (txb ^. treasuryDonationTxBodyL)
      , pure $ show (tx ^. isValidTxL)
      ]

byteStringToInteger :: ByteString -> Integer
byteStringToInteger = BS.foldr' (\x y -> y * 256 + toInteger x) 0
