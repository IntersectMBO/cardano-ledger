{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.TxBody (
  BabelEraTxBody (..),
  BabelTxBody (
    BabelTxBody,
    bbtbSpendInputs,
    bbtbCollateralInputs,
    bbtbReferenceInputs,
    bbtbOutputs,
    bbtbCollateralReturn,
    bbtbTotalCollateral,
    bbtbCerts,
    bbtbWithdrawals,
    bbtbTxfee,
    bbtbVldt,
    bbtbReqSignerHashes,
    bbtbMint,
    bbtbScriptIntegrityHash,
    bbtbAdHash,
    bbtbTxNetworkId,
    bbtbVotingProcedures,
    bbtbProposalProcedures,
    bbtbCurrentTreasuryValue,
    bbtbTreasuryDonation,
    bbtbFulfills,
    bbtbRequests,
    bbtbRequiredTxs
  ),
  BabelTxBodyRaw,
) where

import Cardano.Ledger.Alonzo.TxAuxData (AuxiliaryDataHash (..))
import Cardano.Ledger.Alonzo.TxBody (Indexable (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody (
  allSizedOutputsBabbageTxBodyF,
  babbageAllInputsTxBodyF,
  babbageSpendableInputsTxBodyF,
 )
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.Scripts (BabelEraScript, BabelPlutusPurpose (..))
import Cardano.Ledger.Babel.TxCert (
  BabelEraTxCert,
  -- BabelTxCert (..),
  BabelTxCertUpgradeError,
 )
import Cardano.Ledger.Babel.TxOut ()
import Cardano.Ledger.BaseTypes (Network, fromSMaybe)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  EncCBOR (..),
  Sized (..),
  ToCBOR (..),
  mkSized,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Density (..),
  Encode (..),
  Field (..),
  Wrapped (..),
  decode,
  encode,
  encodeKeyedStrictMaybe,
  field,
  fieldGuarded,
  ofield,
  (!>),
 )
import Cardano.Ledger.Coin (Coin (..), decodePositiveCoin)
import Cardano.Ledger.Conway.Core (ConwayEraScript (mkVotingPurpose), ConwayEraTxBody (..))
import Cardano.Ledger.Conway.Governance (ProposalProcedure, VotingProcedures (..))
import Cardano.Ledger.Conway.PParams (ConwayEraPParams)
import Cardano.Ledger.Conway.Scripts (
  ConwayEraScript (mkProposingPurpose, toProposingPurpose, toVotingPurpose),
 )
import Cardano.Ledger.Conway.TxBody (
  ConwayTxBody (..),
  conwayTotalDepositsTxBody,
 )
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (
  MaryValue (..),
  MultiAsset (..),
  PolicyID,
  policies,
 )
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes (..),
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.TxIn (Fulfill, TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.OSet.Strict as OSet
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (Lens', to, (^.))
import NoThunks.Class (NoThunks)

instance Memoized BabelTxBody where
  type RawType BabelTxBody = BabelTxBodyRaw

data BabelTxBodyRaw era = BabelTxBodyRaw
  { bbtbrSpendInputs :: !(Set (TxIn (EraCrypto era)))
  , bbtbrCollateralInputs :: !(Set (TxIn (EraCrypto era)))
  , bbtbrReferenceInputs :: !(Set (TxIn (EraCrypto era)))
  , bbtbrOutputs :: !(StrictSeq (Sized (TxOut era)))
  , bbtbrCollateralReturn :: !(StrictMaybe (Sized (TxOut era)))
  , bbtbrTotalCollateral :: !(StrictMaybe Coin)
  , bbtbrCerts :: !(OSet.OSet (ConwayTxCert era))
  , bbtbrWithdrawals :: !(Withdrawals (EraCrypto era))
  , bbtbrTxfee :: !Coin
  , bbtbrVldt :: !ValidityInterval
  , bbtbrReqSignerHashes :: !(Set (KeyHash 'Witness (EraCrypto era)))
  , bbtbrMint :: !(MultiAsset (EraCrypto era))
  , bbtbrScriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
  , bbtbrAuxDataHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
  , bbtbrTxNetworkId :: !(StrictMaybe Network)
  , bbtbrVotingProcedures :: !(VotingProcedures era)
  , bbtbrProposalProcedures :: !(OSet.OSet (ProposalProcedure era))
  , bbtbrCurrentTreasuryValue :: !(StrictMaybe Coin)
  , bbtbrTreasuryDonation :: !Coin
  , -- Tx body fields for intents (babel-fees)

    {- CIP-0118#0-tx-body

      Here, we demonstrate the three (or two...I'll explain shortly) new fields required
      for Babel fees.

      The type of `Fulfill` is equivalent to that of an input, and the type of
      a `Request` is equivalent to that of an ourput.

      A `RequiredTx` is a transaction on which *this* transaction depends.

      This has certain implications when it comes to cyclic dependencies. Because the
      `TxBody` must be hashed for witnessing, if two transactions are dependent on
      one-another with `RequiredTxs`, it becomes impossible to hash either of them.
      This is why we might not want to put `RequiredTxs` in the `TxBody`: if we want
      to allow these cyclic dependencies.

      If we do, we'll need to move `RequiredTxs` up to the `Tx` level, like, for example:

      data AlonzoTx era = AlonzoTx
        { body :: !(TxBody era)
        , wits :: !(TxWits era)
        , isValid :: !IsValid
        , auxiliaryData :: !(StrictMaybe (TxAuxData era))
        , requiredTxs :: !(RequiredTxs era)
        }
        deriving (Generic)

      This'll allow us to calculate a composite hash...TODO explain how they can do this

      Jump to CIP-0118#ZONE-rule to continue... -}
    bbtbrFulfills :: !(Set (Fulfill (EraCrypto era)))
  , bbtbrRequests :: !(StrictSeq (Sized (TxOut era)))
  , bbtbrRequiredTxs :: !(Set (TxIn (EraCrypto era)))
  }
  deriving (Generic, Typeable)

deriving instance (EraPParams era, Eq (TxOut era)) => Eq (BabelTxBodyRaw era)

instance
  (EraPParams era, NoThunks (TxOut era)) =>
  NoThunks (BabelTxBodyRaw era)

instance
  (EraPParams era, NFData (TxOut era)) =>
  NFData (BabelTxBodyRaw era)

deriving instance
  (EraPParams era, Show (TxOut era)) =>
  Show (BabelTxBodyRaw era)

instance
  ( EraPParams era
  , DecCBOR (TxOut era)
  , ShelleyEraTxCert era
  , TxCert era ~ ConwayTxCert era
  ) =>
  DecCBOR (BabelTxBodyRaw era)
  where
  decCBOR =
    decode $
      SparseKeyed
        "TxBodyRaw"
        basicBabelTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field (BabelTxBodyRaw era)
      bodyFields 0 = field (\x tx -> tx {bbtbrSpendInputs = x}) From
      bodyFields 1 = field (\x tx -> tx {bbtbrOutputs = x}) From
      bodyFields 2 = field (\x tx -> tx {bbtbrTxfee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {bbtbrVldt = (bbtbrVldt tx) {invalidHereafter = x}})
          From
      bodyFields 4 =
        fieldGuarded
          (emptyFailure "Certificates" "non-empty")
          OSet.null
          (\x tx -> tx {bbtbrCerts = x})
          From
      bodyFields 5 =
        fieldGuarded
          (emptyFailure "Withdrawals" "non-empty")
          (null . unWithdrawals)
          (\x tx -> tx {bbtbrWithdrawals = x})
          From
      bodyFields 7 = ofield (\x tx -> tx {bbtbrAuxDataHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {bbtbrVldt = (bbtbrVldt tx) {invalidBefore = x}})
          From
      bodyFields 9 =
        fieldGuarded
          (emptyFailure "Mint" "non-empty")
          (== mempty)
          (\x tx -> tx {bbtbrMint = x})
          From
      bodyFields 11 = ofield (\x tx -> tx {bbtbrScriptIntegrityHash = x}) From
      bodyFields 13 =
        fieldGuarded
          (emptyFailure "Collateral Inputs" "non-empty")
          null
          (\x tx -> tx {bbtbrCollateralInputs = x})
          From
      bodyFields 14 =
        fieldGuarded
          (emptyFailure "Required Signer Hashes" "non-empty")
          null
          (\x tx -> tx {bbtbrReqSignerHashes = x})
          From
      bodyFields 15 = ofield (\x tx -> tx {bbtbrTxNetworkId = x}) From
      bodyFields 16 = ofield (\x tx -> tx {bbtbrCollateralReturn = x}) From
      bodyFields 17 = ofield (\x tx -> tx {bbtbrTotalCollateral = x}) From
      bodyFields 18 =
        fieldGuarded
          (emptyFailure "Reference Inputs" "non-empty")
          null
          (\x tx -> tx {bbtbrReferenceInputs = x})
          From
      bodyFields 19 =
        fieldGuarded
          (emptyFailure "VotingProcedures" "non-empty")
          (null . unVotingProcedures)
          (\x tx -> tx {bbtbrVotingProcedures = x})
          From
      bodyFields 20 =
        fieldGuarded
          (emptyFailure "ProposalProcedures" "non-empty")
          OSet.null
          (\x tx -> tx {bbtbrProposalProcedures = x})
          From
      bodyFields 21 = ofield (\x tx -> tx {bbtbrCurrentTreasuryValue = x}) From
      bodyFields 22 =
        ofield
          (\x tx -> tx {bbtbrTreasuryDonation = fromSMaybe zero x})
          (D (decodePositiveCoin $ emptyFailure "Treasury Donation" "non-zero"))
      bodyFields 23 = field (\x tx -> tx {bbtbrFulfills = x}) From
      bodyFields 24 = field (\x tx -> tx {bbtbrRequests = x}) From
      bodyFields 25 = field (\x tx -> tx {bbtbrRequiredTxs = x}) From
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields :: [(Word, String)]
      requiredFields =
        [ (0, "inputs")
        , (1, "outputs")
        , (2, "fee")
        ]
      emptyFailure fieldName requirement =
        "TxBody: '" <> fieldName <> "' must be " <> requirement <> " when supplied"

newtype BabelTxBody era = TxBodyConstr (MemoBytes BabelTxBodyRaw era)
  deriving (Generic, SafeToHash, ToCBOR)

deriving instance
  (EraPParams era, NoThunks (TxOut era)) =>
  NoThunks (BabelTxBody era)

deriving instance
  (EraPParams era, Eq (TxOut era)) =>
  Eq (BabelTxBody era)

deriving newtype instance
  (EraPParams era, NFData (TxOut era)) =>
  NFData (BabelTxBody era)

deriving instance
  (EraPParams era, Show (TxOut era)) =>
  Show (BabelTxBody era)

type instance MemoHashIndex BabelTxBodyRaw = EraIndependentTxBody

instance c ~ EraCrypto era => HashAnnotated (BabelTxBody era) EraIndependentTxBody c where
  hashAnnotated = getMemoSafeHash

instance
  ( DecCBOR (TxOut era)
  , EraPParams era
  , ShelleyEraTxCert era
  , TxCert era ~ ConwayTxCert era
  ) =>
  DecCBOR (Annotator (BabelTxBodyRaw era))
  where
  decCBOR = pure <$> decCBOR

deriving via
  (Mem BabelTxBodyRaw era)
  instance
    ( DecCBOR (TxOut era)
    , EraPParams era
    , ShelleyEraTxCert era
    , TxCert era ~ ConwayTxCert era
    ) =>
    DecCBOR (Annotator (BabelTxBody era))

mkBabelTxBody :: BabelEraTxBody era => BabelTxBody era
mkBabelTxBody = mkMemoized basicBabelTxBodyRaw

basicBabelTxBodyRaw :: BabelTxBodyRaw era
basicBabelTxBodyRaw =
  BabelTxBodyRaw
    mempty
    mempty
    mempty
    mempty
    SNothing
    SNothing
    OSet.empty
    (Withdrawals mempty)
    mempty
    (ValidityInterval SNothing SNothing)
    mempty
    mempty
    SNothing
    SNothing
    SNothing
    (VotingProcedures mempty)
    OSet.empty
    SNothing
    mempty
    mempty
    mempty
    mempty

data BabelTxBodyUpgradeError c
  = CTBUETxCert BabelTxCertUpgradeError
  | -- | The TxBody contains an update proposal from a pre-Babel era. Since
    --   this can only have come from the genesis delegates, we just discard it.
    CTBUEContainsUpdate
  | -- | In eras prior to Babel duplicate certificates where allowed
    CTBUEContainsDuplicateCerts (Set (TxCert (BabelEra c)))
  deriving (Eq, Show)

instance Crypto c => EraTxBody (BabelEra c) where
  {-# SPECIALIZE instance EraTxBody (BabelEra StandardCrypto) #-}

  type TxBody (BabelEra c) = BabelTxBody (BabelEra c)
  type TxBodyUpgradeError (BabelEra c) = BabelTxBodyUpgradeError c

  mkBasicTxBody = mkBabelTxBody

  inputsTxBodyL = lensMemoRawType bbtbrSpendInputs (\txb x -> txb {bbtbrSpendInputs = x})
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType
      (fmap sizedValue . bbtbrOutputs)
      (\txb x -> txb {bbtbrOutputs = mkSized (eraProtVerLow @(BabelEra c)) <$> x})
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = lensMemoRawType bbtbrTxfee (\txb x -> txb {bbtbrTxfee = x})
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = lensMemoRawType bbtbrAuxDataHash (\txb x -> txb {bbtbrAuxDataHash = x})
  {-# INLINE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = babbageSpendableInputsTxBodyF
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = babbageAllInputsTxBodyF
  {-# INLINE allInputsTxBodyF #-}

  withdrawalsTxBodyL = lensMemoRawType bbtbrWithdrawals (\txb x -> txb {bbtbrWithdrawals = x})
  {-# INLINE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType (OSet.toStrictSeq . bbtbrCerts) (\txb x -> txb {bbtbrCerts = OSet.fromStrictSeq x})
  {-# INLINE certsTxBodyL #-}

  getTotalDepositsTxBody = conwayTotalDepositsTxBody

  getTotalRefundsTxBody pp lookupStakingDeposit lookupDRepDeposit txBody =
    getTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit (txBody ^. certsTxBodyL)

  upgradeTxBody ctb = do
    certs <- traverse (left CTBUETxCert . upgradeTxCert) (OSet.toStrictSeq $ ctbCerts ctb)
    let (duplicates, certsOSet) = OSet.fromStrictSeqDuplicates certs
    unless (null duplicates) $ Left $ CTBUEContainsDuplicateCerts duplicates
    pure $
      BabelTxBody
        { bbtbSpendInputs = ctbSpendInputs ctb
        , bbtbOutputs =
            mkSized (eraProtVerLow @(BabelEra c))
              . upgradeTxOut
              . sizedValue
              <$> ctbOutputs ctb
        , bbtbCerts = certsOSet
        , bbtbWithdrawals = ctbWithdrawals ctb
        , bbtbTxfee = ctbTxfee ctb
        , bbtbVldt = ctbVldt ctb
        , bbtbAdHash = ctbAdHash ctb
        , bbtbMint = ctbMint ctb
        , bbtbCollateralInputs = ctbCollateralInputs ctb
        , bbtbReqSignerHashes = ctbReqSignerHashes ctb
        , bbtbScriptIntegrityHash = ctbScriptIntegrityHash ctb
        , bbtbTxNetworkId = ctbTxNetworkId ctb
        , bbtbReferenceInputs = ctbReferenceInputs ctb
        , bbtbCollateralReturn =
            mkSized (eraProtVerLow @(BabelEra c))
              . upgradeTxOut
              . sizedValue
              <$> ctbCollateralReturn ctb
        , bbtbTotalCollateral = ctbTotalCollateral ctb
        , bbtbCurrentTreasuryValue = SNothing
        , bbtbProposalProcedures = OSet.empty
        , bbtbVotingProcedures = VotingProcedures mempty
        , bbtbTreasuryDonation = Coin 0
        , bbtbFulfills = mempty
        , bbtbRequests = mempty
        , bbtbRequiredTxs = mempty
        }

instance Crypto c => AllegraEraTxBody (BabelEra c) where
  {-# SPECIALIZE instance AllegraEraTxBody (BabelEra StandardCrypto) #-}

  vldtTxBodyL = lensMemoRawType bbtbrVldt (\txb x -> txb {bbtbrVldt = x})
  {-# INLINE vldtTxBodyL #-}

instance Crypto c => MaryEraTxBody (BabelEra c) where
  {-# SPECIALIZE instance MaryEraTxBody (BabelEra StandardCrypto) #-}

  mintTxBodyL = lensMemoRawType bbtbrMint (\txb x -> txb {bbtbrMint = x})
  {-# INLINE mintTxBodyL #-}

  mintValueTxBodyF = mintTxBodyL . to (MaryValue mempty)

  mintedTxBodyF =
    to (\(TxBodyConstr (Memo txBodyRaw _)) -> policies (bbtbrMint txBodyRaw))
  {-# INLINE mintedTxBodyF #-}

instance Crypto c => AlonzoEraTxBody (BabelEra c) where
  {-# SPECIALIZE instance AlonzoEraTxBody (BabelEra StandardCrypto) #-}

  collateralInputsTxBodyL =
    lensMemoRawType bbtbrCollateralInputs (\txb x -> txb {bbtbrCollateralInputs = x})
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL =
    lensMemoRawType bbtbrReqSignerHashes (\txb x -> txb {bbtbrReqSignerHashes = x})
  {-# INLINE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL =
    lensMemoRawType bbtbrScriptIntegrityHash (\txb x -> txb {bbtbrScriptIntegrityHash = x})
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = lensMemoRawType bbtbrTxNetworkId (\txb x -> txb {bbtbrTxNetworkId = x})
  {-# INLINE networkIdTxBodyL #-}

  redeemerPointer = babelRedeemerPointer

  redeemerPointerInverse = babelRedeemerPointerInverse

instance Crypto c => BabbageEraTxBody (BabelEra c) where
  {-# SPECIALIZE instance BabbageEraTxBody (BabelEra StandardCrypto) #-}

  sizedOutputsTxBodyL = lensMemoRawType bbtbrOutputs (\txb x -> txb {bbtbrOutputs = x})
  {-# INLINE sizedOutputsTxBodyL #-}

  referenceInputsTxBodyL =
    lensMemoRawType bbtbrReferenceInputs (\txb x -> txb {bbtbrReferenceInputs = x})
  {-# INLINE referenceInputsTxBodyL #-}

  totalCollateralTxBodyL =
    lensMemoRawType bbtbrTotalCollateral (\txb x -> txb {bbtbrTotalCollateral = x})
  {-# INLINE totalCollateralTxBodyL #-}

  collateralReturnTxBodyL =
    lensMemoRawType
      (fmap sizedValue . bbtbrCollateralReturn)
      (\txb x -> txb {bbtbrCollateralReturn = mkSized (eraProtVerLow @(BabelEra c)) <$> x})
  {-# INLINE collateralReturnTxBodyL #-}

  sizedCollateralReturnTxBodyL =
    lensMemoRawType bbtbrCollateralReturn (\txb x -> txb {bbtbrCollateralReturn = x})
  {-# INLINE sizedCollateralReturnTxBodyL #-}

  allSizedOutputsTxBodyF = allSizedOutputsBabbageTxBodyF
  {-# INLINE allSizedOutputsTxBodyF #-}

instance Crypto c => ConwayEraScript (BabelEra c) where
  mkVotingPurpose = BabelVoting

  toVotingPurpose (BabelVoting i) = Just i
  toVotingPurpose _ = Nothing

  mkProposingPurpose = BabelProposing

  toProposingPurpose (BabelProposing i) = Just i
  toProposingPurpose _ = Nothing

instance Crypto c => ConwayEraTxBody (BabelEra c) where
  {-# SPECIALIZE instance ConwayEraTxBody (BabelEra StandardCrypto) #-}

  currentTreasuryValueTxBodyL =
    lensMemoRawType bbtbrCurrentTreasuryValue (\txb x -> txb {bbtbrCurrentTreasuryValue = x})
  {-# INLINE currentTreasuryValueTxBodyL #-}

  votingProceduresTxBodyL =
    lensMemoRawType bbtbrVotingProcedures (\txb x -> txb {bbtbrVotingProcedures = x})
  {-# INLINE votingProceduresTxBodyL #-}

  proposalProceduresTxBodyL =
    lensMemoRawType bbtbrProposalProcedures (\txb x -> txb {bbtbrProposalProcedures = x})
  {-# INLINE proposalProceduresTxBodyL #-}

  treasuryDonationTxBodyL =
    lensMemoRawType bbtbrTreasuryDonation (\txb x -> txb {bbtbrTreasuryDonation = x})
  {-# INLINE treasuryDonationTxBodyL #-}

instance (Crypto c, ConwayEraTxBody (BabelEra c)) => BabelEraTxBody (BabelEra c) where
  fulfillsTxBodyL = lensMemoRawType bbtbrFulfills (\txb x -> txb {bbtbrFulfills = x})
  {-# INLINE fulfillsTxBodyL #-}
  requestsTxBodyL = lensMemoRawType bbtbrRequests (\txb x -> txb {bbtbrRequests = x})
  {-# INLINE requestsTxBodyL #-}
  requiredTxsTxBodyL = lensMemoRawType bbtbrRequiredTxs (\txb x -> txb {bbtbrRequiredTxs = x})
  {-# INLINE requiredTxsTxBodyL #-}

instance
  (EraPParams era, Eq (TxOut era), Eq (TxCert era)) =>
  EqRaw (BabelTxBody era)

pattern BabelTxBody ::
  BabelEraTxBody era =>
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (Sized (TxOut era)) ->
  StrictMaybe (Sized (TxOut era)) ->
  StrictMaybe Coin ->
  OSet.OSet (ConwayTxCert era) ->
  Withdrawals (EraCrypto era) ->
  Coin ->
  ValidityInterval ->
  Set (KeyHash 'Witness (EraCrypto era)) ->
  MultiAsset (EraCrypto era) ->
  StrictMaybe (ScriptIntegrityHash (EraCrypto era)) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  StrictMaybe Network ->
  VotingProcedures era ->
  OSet.OSet (ProposalProcedure era) ->
  StrictMaybe Coin ->
  Coin ->
  Set (Fulfill (EraCrypto era)) ->
  StrictSeq (Sized (TxOut era)) ->
  Set (TxIn (EraCrypto era)) ->
  BabelTxBody era
pattern BabelTxBody
  { bbtbSpendInputs
  , bbtbCollateralInputs
  , bbtbReferenceInputs
  , bbtbOutputs
  , bbtbCollateralReturn
  , bbtbTotalCollateral
  , bbtbCerts
  , bbtbWithdrawals
  , bbtbTxfee
  , bbtbVldt
  , bbtbReqSignerHashes
  , bbtbMint
  , bbtbScriptIntegrityHash
  , bbtbAdHash
  , bbtbTxNetworkId
  , bbtbVotingProcedures
  , bbtbProposalProcedures
  , bbtbCurrentTreasuryValue
  , bbtbTreasuryDonation
  , bbtbFulfills
  , bbtbRequests
  , bbtbRequiredTxs
  } <-
  ( getMemoRawType ->
      BabelTxBodyRaw
        { bbtbrSpendInputs = bbtbSpendInputs
        , bbtbrCollateralInputs = bbtbCollateralInputs
        , bbtbrReferenceInputs = bbtbReferenceInputs
        , bbtbrOutputs = bbtbOutputs
        , bbtbrCollateralReturn = bbtbCollateralReturn
        , bbtbrTotalCollateral = bbtbTotalCollateral
        , bbtbrCerts = bbtbCerts
        , bbtbrWithdrawals = bbtbWithdrawals
        , bbtbrTxfee = bbtbTxfee
        , bbtbrVldt = bbtbVldt
        , bbtbrReqSignerHashes = bbtbReqSignerHashes
        , bbtbrMint = bbtbMint
        , bbtbrScriptIntegrityHash = bbtbScriptIntegrityHash
        , bbtbrAuxDataHash = bbtbAdHash
        , bbtbrTxNetworkId = bbtbTxNetworkId
        , bbtbrVotingProcedures = bbtbVotingProcedures
        , bbtbrProposalProcedures = bbtbProposalProcedures
        , bbtbrCurrentTreasuryValue = bbtbCurrentTreasuryValue
        , bbtbrTreasuryDonation = bbtbTreasuryDonation
        , bbtbrFulfills = bbtbFulfills
        , bbtbrRequests = bbtbRequests
        , bbtbrRequiredTxs = bbtbRequiredTxs
        }
    )
  where
    BabelTxBody
      inputsX
      collateralX
      referenceInputsX
      outputsX
      collateralReturnX
      totalCollateralX
      certsX
      withdrawalsX
      txfeeX
      vldtX
      reqSignerHashesX
      mintX
      scriptIntegrityHashX
      adHashX
      txnetworkidX
      votingProcedures
      proposalProcedures
      currentTreasuryValue
      treasuryDonation
      fulfills
      requests
      requiredTxs =
        mkMemoized $
          BabelTxBodyRaw
            inputsX
            collateralX
            referenceInputsX
            outputsX
            collateralReturnX
            totalCollateralX
            certsX
            withdrawalsX
            txfeeX
            vldtX
            reqSignerHashesX
            mintX
            scriptIntegrityHashX
            adHashX
            txnetworkidX
            votingProcedures
            proposalProcedures
            currentTreasuryValue
            treasuryDonation
            fulfills
            requests
            requiredTxs

{-# COMPLETE BabelTxBody #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeTxBodyRaw ::
  BabelEraTxBody era =>
  BabelTxBodyRaw era ->
  Encode ('Closed 'Sparse) (BabelTxBodyRaw era)
encodeTxBodyRaw BabelTxBodyRaw {..} =
  let ValidityInterval bot top = bbtbrVldt
   in Keyed
        ( \i ci ri o cr tc f t c w b ->
            BabelTxBodyRaw i ci ri o cr tc c w f (ValidityInterval b t)
        )
        !> Key 0 (To bbtbrSpendInputs)
        !> Omit null (Key 13 (To bbtbrCollateralInputs))
        !> Omit null (Key 18 (To bbtbrReferenceInputs))
        !> Key 1 (To bbtbrOutputs)
        !> encodeKeyedStrictMaybe 16 bbtbrCollateralReturn
        !> encodeKeyedStrictMaybe 17 bbtbrTotalCollateral
        !> Key 2 (To bbtbrTxfee)
        !> encodeKeyedStrictMaybe 3 top
        !> Omit OSet.null (Key 4 (To bbtbrCerts))
        !> Omit (null . unWithdrawals) (Key 5 (To bbtbrWithdrawals))
        !> encodeKeyedStrictMaybe 8 bot
        !> Omit null (Key 14 (To bbtbrReqSignerHashes))
        !> Omit (== mempty) (Key 9 (To bbtbrMint))
        !> encodeKeyedStrictMaybe 11 bbtbrScriptIntegrityHash
        !> encodeKeyedStrictMaybe 7 bbtbrAuxDataHash
        !> encodeKeyedStrictMaybe 15 bbtbrTxNetworkId
        !> Omit (null . unVotingProcedures) (Key 19 (To bbtbrVotingProcedures))
        !> Omit OSet.null (Key 20 (To bbtbrProposalProcedures))
        !> encodeKeyedStrictMaybe 21 bbtbrCurrentTreasuryValue
        !> Omit (== mempty) (Key 22 $ To bbtbrTreasuryDonation)
        !> Omit (== mempty) (Key 23 (To bbtbrFulfills))
        !> Omit (== mempty) (Key 24 (To bbtbrRequests))
        !> Omit (== mempty) (Key 25 (To bbtbrRequiredTxs))

instance BabelEraTxBody era => EncCBOR (BabelTxBodyRaw era) where
  encCBOR = encode . encodeTxBodyRaw

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (BabelTxBody era)

class
  (ConwayEraTxBody era, BabelEraTxCert era, ConwayEraPParams era, BabelEraScript era) =>
  BabelEraTxBody era
  where
  fulfillsTxBodyL :: Lens' (TxBody era) (Set (Fulfill (EraCrypto era)))

  requestsTxBodyL :: Lens' (TxBody era) (StrictSeq (Sized (TxOut era)))

  requiredTxsTxBodyL :: Lens' (TxBody era) (Set (TxIn (EraCrypto era)))

babelRedeemerPointer ::
  forall era.
  BabelEraTxBody era =>
  TxBody era ->
  BabelPlutusPurpose AsItem era ->
  StrictMaybe (BabelPlutusPurpose AsIx era)
babelRedeemerPointer txBody = \case
  BabelMinting policyID ->
    BabelMinting <$> indexOf policyID (txBody ^. mintedTxBodyF :: Set (PolicyID (EraCrypto era)))
  BabelSpending txIn ->
    BabelSpending <$> indexOf txIn (txBody ^. inputsTxBodyL)
  BabelRewarding rewardAccount ->
    BabelRewarding <$> indexOf rewardAccount (unWithdrawals (txBody ^. withdrawalsTxBodyL))
  BabelCertifying txCert ->
    BabelCertifying <$> indexOf txCert (txBody ^. certsTxBodyL)
  BabelVoting votingProcedure ->
    BabelVoting <$> indexOf votingProcedure (txBody ^. votingProceduresTxBodyL)
  BabelProposing proposalProcedure ->
    BabelProposing <$> indexOf proposalProcedure (txBody ^. proposalProceduresTxBodyL)

babelRedeemerPointerInverse ::
  BabelEraTxBody era =>
  TxBody era ->
  BabelPlutusPurpose AsIx era ->
  StrictMaybe (BabelPlutusPurpose AsIxItem era)
babelRedeemerPointerInverse txBody = \case
  BabelMinting idx ->
    BabelMinting <$> fromIndex idx (txBody ^. mintedTxBodyF)
  BabelSpending idx ->
    BabelSpending <$> fromIndex idx (txBody ^. inputsTxBodyL)
  BabelRewarding idx ->
    BabelRewarding <$> fromIndex idx (unWithdrawals (txBody ^. withdrawalsTxBodyL))
  BabelCertifying idx ->
    BabelCertifying <$> fromIndex idx (txBody ^. certsTxBodyL)
  BabelVoting idx ->
    BabelVoting <$> fromIndex idx (txBody ^. votingProceduresTxBodyL)
  BabelProposing idx ->
    BabelProposing <$> fromIndex idx (txBody ^. proposalProceduresTxBodyL)
