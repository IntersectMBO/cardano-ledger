{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxBody (
  DijkstraEraTxBody (..),
  TxBody (
    MkDijkstraTxBody,
    DijkstraTxBody,
    DijkstraSubTxBody,
    dtbSpendInputs,
    dtbCollateralInputs,
    dtbReferenceInputs,
    dtbOutputs,
    dtbCollateralReturn,
    dtbTotalCollateral,
    dtbCerts,
    dtbWithdrawals,
    dtbTxfee,
    dtbVldt,
    dtbMint,
    dtbScriptIntegrityHash,
    dtbAdHash,
    dtbTxNetworkId,
    dtbVotingProcedures,
    dtbProposalProcedures,
    dtbCurrentTreasuryValue,
    dtbTreasuryDonation,
    dtbGuards,
    dstbSpendInputs,
    dstbReferenceInputs,
    dstbOutputs,
    dstbCerts,
    dstbWithdrawals,
    dstbVldt,
    dstbMint,
    dstbScriptIntegrityHash,
    dstbAdHash,
    dstbTxNetworkId,
    dstbVotingProcedures,
    dstbProposalProcedures,
    dstbCurrentTreasuryValue,
    dstbTreasuryDonation,
    dstbGuards
  ),
  upgradeProposals,
  upgradeGovAction,
  DijkstraTxBodyRaw (..),
) where

import Cardano.Ledger.Alonzo.TxBody (Indexable (..))
import Cardano.Ledger.Babbage.TxBody (
  BabbageTxOut,
  allSizedOutputsBabbageTxBodyF,
  babbageAllInputsTxBodyF,
  babbageSpendableInputsTxBodyF,
 )
import Cardano.Ledger.BaseTypes (Network, StrictMaybe (..), fromSMaybe)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  Sized (..),
  ToCBOR,
  TokenType (..),
  liftST,
  mkSized,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Density (..),
  Encode (..),
  Field,
  Wrapped (..),
  decode,
  encode,
  encodeKeyedStrictMaybe,
  field,
  fieldGuarded,
  invalidField,
  ofield,
  (!>),
 )
import Cardano.Ledger.Coin (Coin, decodePositiveCoin)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  ProposalProcedure (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Conway.TxBody (
  conwayProposalsDeposits,
 )
import Cardano.Ledger.Core (EraPParams (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose (..))
import Cardano.Ledger.Dijkstra.TxOut ()
import Cardano.Ledger.Keys (HasKeyRole (..))
import Cardano.Ledger.Mary.Value (MultiAsset)
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  memoRawTypeL,
  mkMemoizedEra,
 )
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData (..), deepseq)
import Data.Coerce (coerce)
import Data.OSet.Strict (OSet, decodeOSet)
import qualified Data.OSet.Strict as OSet
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set, foldr')
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, to, (&), (.~), (^.))
import NoThunks.Class (InspectHeap (..), NoThunks)

data DijkstraTxBodyRaw l era where
  DijkstraTxBodyRaw ::
    { dtbrSpendInputs :: !(Set TxIn)
    , dtbrCollateralInputs :: !(Set TxIn)
    , dtbrReferenceInputs :: !(Set TxIn)
    , dtbrOutputs :: !(StrictSeq (Sized (TxOut DijkstraEra)))
    , dtbrCollateralReturn :: !(StrictMaybe (Sized (TxOut DijkstraEra)))
    , dtbrTotalCollateral :: !(StrictMaybe Coin)
    , dtbrCerts :: !(OSet.OSet (TxCert DijkstraEra))
    , dtbrWithdrawals :: !Withdrawals
    , dtbrFee :: !Coin
    , dtbrVldt :: !ValidityInterval
    , dtbrGuards :: !(OSet (Credential Guard))
    , dtbrMint :: !MultiAsset
    , dtbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
    , dtbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
    , dtbrNetworkId :: !(StrictMaybe Network)
    , dtbrVotingProcedures :: !(VotingProcedures DijkstraEra)
    , dtbrProposalProcedures :: !(OSet.OSet (ProposalProcedure DijkstraEra))
    , dtbrCurrentTreasuryValue :: !(StrictMaybe Coin)
    , dtbrTreasuryDonation :: !Coin
    } ->
    DijkstraTxBodyRaw TopTx era
  DijkstraSubTxBodyRaw ::
    { dstbrSpendInputs :: !(Set TxIn)
    , dstbrReferenceInputs :: !(Set TxIn)
    , dstbrOutputs :: !(StrictSeq (Sized (TxOut DijkstraEra)))
    , dstbrCerts :: !(OSet.OSet (TxCert DijkstraEra))
    , dstbrWithdrawals :: !Withdrawals
    , dstbrVldt :: !ValidityInterval
    , dstbrGuards :: !(OSet (Credential Guard))
    , dstbrMint :: !MultiAsset
    , dstbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
    , dstbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
    , dstbrNetworkId :: !(StrictMaybe Network)
    , dstbrVotingProcedures :: !(VotingProcedures DijkstraEra)
    , dstbrProposalProcedures :: !(OSet.OSet (ProposalProcedure DijkstraEra))
    , dstbrCurrentTreasuryValue :: !(StrictMaybe Coin)
    , dstbrTreasuryDonation :: !Coin
    } ->
    DijkstraTxBodyRaw SubTx era

deriving instance Eq (DijkstraTxBodyRaw l DijkstraEra)

instance EqRaw (TxBody l DijkstraEra)

deriving via
  InspectHeap (DijkstraTxBodyRaw l DijkstraEra)
  instance
    Typeable l => NoThunks (DijkstraTxBodyRaw l DijkstraEra)

instance NFData (DijkstraTxBodyRaw l DijkstraEra) where
  rnf DijkstraTxBodyRaw {..} =
    dtbrSpendInputs `deepseq`
      dtbrCollateralInputs `deepseq`
        dtbrReferenceInputs `deepseq`
          dtbrOutputs `deepseq`
            dtbrCollateralReturn `deepseq`
              dtbrTotalCollateral `deepseq`
                dtbrCerts `deepseq`
                  dtbrWithdrawals `deepseq`
                    dtbrFee `deepseq`
                      dtbrVldt `deepseq`
                        dtbrGuards `deepseq`
                          dtbrMint `deepseq`
                            dtbrScriptIntegrityHash `deepseq`
                              dtbrAuxDataHash `deepseq`
                                dtbrNetworkId `deepseq`
                                  dtbrVotingProcedures `deepseq`
                                    dtbrProposalProcedures `deepseq`
                                      dtbrCurrentTreasuryValue `deepseq`
                                        rnf dtbrTreasuryDonation
  rnf DijkstraSubTxBodyRaw {..} =
    dstbrSpendInputs `deepseq`
      dstbrReferenceInputs `deepseq`
        dstbrOutputs `deepseq`
          dstbrCerts `deepseq`
            dstbrWithdrawals `deepseq`
              dstbrVldt `deepseq`
                dstbrGuards `deepseq`
                  dstbrMint `deepseq`
                    dstbrScriptIntegrityHash `deepseq`
                      dstbrAuxDataHash `deepseq`
                        dstbrNetworkId `deepseq`
                          dstbrVotingProcedures `deepseq`
                            dstbrProposalProcedures `deepseq`
                              dstbrCurrentTreasuryValue `deepseq`
                                rnf dstbrTreasuryDonation

deriving instance Show (DijkstraTxBodyRaw l DijkstraEra)

basicDijkstraTxBodyRaw :: DijkstraTxBodyRaw TopTx DijkstraEra
basicDijkstraTxBodyRaw =
  DijkstraTxBodyRaw
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

instance Typeable l => DecCBOR (DijkstraTxBodyRaw l DijkstraEra) where
  decCBOR = withSTxBothLevels @l $ \case
    STopTx ->
      decode $
        SparseKeyed
          "TxBodyRaw"
          basicDijkstraTxBodyRaw
          bodyFields
          requiredFields
    SSubTx ->
      decode $
        SparseKeyed
          "SubTxBodyRaw"
          basicDijkstraSubTxBodyRaw
          bodyFields
          requiredFields
    where
      bodyFields :: Word -> Field (DijkstraTxBodyRaw l DijkstraEra)
      bodyFields 0 = field (\x tx -> tx & inputsDijkstraTxBodyRawL .~ x) From
      bodyFields 1 = field (\x tx -> tx & outputsDijkstraTxBodyRawL .~ x) From
      bodyFields n@2 =
        withSTxBothLevels @l $ \case
          STopTx -> field (\x tx -> tx & feeDijkstraTxBodyRawL .~ x) From
          SSubTx -> invalidField n
      bodyFields sTxLevel n@3 =
        case sTxLevel of
          STopTx ->
            ofield
              (\x tx -> tx & vldtDijkstraTxBodyRawL .~ (dtbrVldt tx) {invalidHereafter = x})
              From
          SSubTx -> invalidField n
      bodyFields 4 =
        fieldGuarded
          (emptyFailure "Certificates" "non-empty")
          OSet.null
          (\x tx -> tx & certsDijkstraTxBodyRawL .~ x)
          From
      bodyFields 5 =
        fieldGuarded
          (emptyFailure "Withdrawals" "non-empty")
          (null . unWithdrawals)
          (\x tx -> tx & withdrawalsDijkstraTxBodyRawL .~ x)
          From
      bodyFields 7 = ofield (\x tx -> tx & auxDataHashDijkstraTxBodyRawL .~ x) From
      bodyFields 8 =
        ofield
          (\x tx -> tx & vldtDijkstraTxBodyRawL .~ (dtbrVldt tx) {invalidBefore = x})
      bodyFields 9 =
        fieldGuarded
          (emptyFailure "Mint" "non-empty")
          (== mempty)
          (\x tx -> tx & mintDijkstraTxBodyRawL .~ x)
          From
      bodyFields 11 = ofield (\x tx -> tx & scriptIntegrityHashDijkstraTxBodyRawL .~ x) From
      bodyFields n@13 =
        withSTxBothLevels @l $ \case
          STopTx ->
            fieldGuarded
              (emptyFailure "Collateral Inputs" "non-empty")
              null
              (\x tx -> tx & collateralInputsDijkstraTxBodyRawL .~ x)
              From
          SSubTx -> invalidField n
      bodyFields 14 =
        ofield
          (\x tx -> tx & guardsDijkstraTxBodyRawL .~ fromSMaybe mempty x)
          (D decodeGuards)
      bodyFields 15 = ofield (\x tx -> tx & networkIdDijkstraTxBodyRawL .~ x) From
      bodyFields n@16 =
        withSTxBothLevels @l $ \case
          STopTx -> ofield (\x tx -> tx & collateralReturnDijkstraTxBodyRawL .~ x) From
          SSubTx -> invalidField n
      bodyFields n@17 =
        withSTxBothLevels @l $ \case
          STopTx -> ofield (\x tx -> tx & totalCollateralDijkstraTxBodyRawL .~ x) From
          SSubTx -> invalidField n
      bodyFields 18 =
        fieldGuarded
          (emptyFailure "Reference Inputs" "non-empty")
          null
          (\x tx -> tx & referenceInputsDijkstraTxBodyRawL .~ x)
          From
      bodyFields 19 =
        fieldGuarded
          (emptyFailure "VotingProcedures" "non-empty")
          (null . unVotingProcedures)
          (\x -> votingProceduresDijkstraTxBodyRawL .~ x)
          From
      bodyFields 20 =
        fieldGuarded
          (emptyFailure "ProposalProcedures" "non-empty")
          OSet.null
          (\x tx -> tx & proposalProceduresDijkstraTxBodyRawL .~ x)
          From
      bodyFields 21 = ofield (\x tx -> tx & currentTreasuryValueDijkstraTxBodyRawL .~ x) From
      bodyFields 22 =
        ofield
          (\x tx -> tx & treasuryDonationDijkstraTxBodyRawL .~ fromSMaybe zero x)
          (D (decodePositiveCoin $ emptyFailure "Treasury Donation" "non-zero"))
      bodyFields n = invalidField n
      requiredFields :: [(Word, String)]
      requiredFields =
        [ (0, "inputs")
        , (1, "outputs")
        , (2, "fee")
        ]
      emptyFailure fieldName requirement =
        "TxBody: '" <> fieldName <> "' must be " <> requirement <> " when supplied"

encodeTxBodyRaw ::
  DijkstraTxBodyRaw l DijkstraEra ->
  Encode ('Closed 'Sparse) (DijkstraTxBodyRaw l DijkstraEra)
encodeTxBodyRaw DijkstraTxBodyRaw {..} =
  let ValidityInterval bot top = dtbrVldt
   in Keyed
        ( \i ci ri o cr tc f t c w b ->
            DijkstraTxBodyRaw i ci ri o cr tc c w f (ValidityInterval b t)
        )
        !> Key 0 (To dtbrSpendInputs)
        !> Omit null (Key 13 (To dtbrCollateralInputs))
        !> Omit null (Key 18 (To dtbrReferenceInputs))
        !> Key 1 (To dtbrOutputs)
        !> encodeKeyedStrictMaybe 16 dtbrCollateralReturn
        !> encodeKeyedStrictMaybe 17 dtbrTotalCollateral
        !> Key 2 (To dtbrFee)
        !> encodeKeyedStrictMaybe 3 top
        !> Omit OSet.null (Key 4 (To dtbrCerts))
        !> Omit (null . unWithdrawals) (Key 5 (To dtbrWithdrawals))
        !> encodeKeyedStrictMaybe 8 bot
        !> Omit null (Key 14 (To dtbrGuards))
        !> Omit (== mempty) (Key 9 (To dtbrMint))
        !> encodeKeyedStrictMaybe 11 dtbrScriptIntegrityHash
        !> encodeKeyedStrictMaybe 7 dtbrAuxDataHash
        !> encodeKeyedStrictMaybe 15 dtbrNetworkId
        !> Omit (null . unVotingProcedures) (Key 19 (To dtbrVotingProcedures))
        !> Omit OSet.null (Key 20 (To dtbrProposalProcedures))
        !> encodeKeyedStrictMaybe 21 dtbrCurrentTreasuryValue
        !> Omit (== mempty) (Key 22 $ To dtbrTreasuryDonation)
encodeTxBodyRaw DijkstraSubTxBodyRaw {..} =
  let ValidityInterval bot top = dstbrVldt
   in Keyed
        ( \i ri o t c w b ->
            DijkstraSubTxBodyRaw i ri o c w (ValidityInterval b t)
        )
        !> Key 0 (To dstbrSpendInputs)
        !> Omit null (Key 18 (To dstbrReferenceInputs))
        !> Key 1 (To dstbrOutputs)
        !> encodeKeyedStrictMaybe 3 top
        !> Omit OSet.null (Key 4 (To dstbrCerts))
        !> Omit (null . unWithdrawals) (Key 5 (To dstbrWithdrawals))
        !> encodeKeyedStrictMaybe 8 bot
        !> Omit null (Key 14 (To dstbrGuards))
        !> Omit (== mempty) (Key 9 (To dstbrMint))
        !> encodeKeyedStrictMaybe 11 dstbrScriptIntegrityHash
        !> encodeKeyedStrictMaybe 7 dstbrAuxDataHash
        !> encodeKeyedStrictMaybe 15 dstbrNetworkId
        !> Omit (null . unVotingProcedures) (Key 19 (To dstbrVotingProcedures))
        !> Omit OSet.null (Key 20 (To dstbrProposalProcedures))
        !> encodeKeyedStrictMaybe 21 dstbrCurrentTreasuryValue
        !> Omit (== mempty) (Key 22 $ To dstbrTreasuryDonation)

instance EncCBOR (DijkstraTxBodyRaw l DijkstraEra) where
  encCBOR = encode . encodeTxBodyRaw

deriving instance Typeable l => NoThunks (TxBody l DijkstraEra)

deriving instance Eq (TxBody l DijkstraEra)

deriving newtype instance NFData (TxBody l DijkstraEra)

deriving instance Show (TxBody l DijkstraEra)

pattern DijkstraTxBody ::
  Set TxIn ->
  Set TxIn ->
  Set TxIn ->
  StrictSeq (Sized (TxOut DijkstraEra)) ->
  StrictMaybe (Sized (TxOut DijkstraEra)) ->
  StrictMaybe Coin ->
  OSet.OSet (TxCert DijkstraEra) ->
  Withdrawals ->
  Coin ->
  ValidityInterval ->
  OSet (Credential Guard) ->
  MultiAsset ->
  StrictMaybe ScriptIntegrityHash ->
  StrictMaybe TxAuxDataHash ->
  StrictMaybe Network ->
  VotingProcedures DijkstraEra ->
  OSet.OSet (ProposalProcedure DijkstraEra) ->
  StrictMaybe Coin ->
  Coin ->
  TxBody TopTx DijkstraEra
pattern DijkstraTxBody
  { dtbSpendInputs
  , dtbCollateralInputs
  , dtbReferenceInputs
  , dtbOutputs
  , dtbCollateralReturn
  , dtbTotalCollateral
  , dtbCerts
  , dtbWithdrawals
  , dtbTxfee
  , dtbVldt
  , dtbGuards
  , dtbMint
  , dtbScriptIntegrityHash
  , dtbAdHash
  , dtbTxNetworkId
  , dtbVotingProcedures
  , dtbProposalProcedures
  , dtbCurrentTreasuryValue
  , dtbTreasuryDonation
  } <-
  ( getMemoRawType ->
      DijkstraTxBodyRaw
        { dtbrSpendInputs = dtbSpendInputs
        , dtbrCollateralInputs = dtbCollateralInputs
        , dtbrReferenceInputs = dtbReferenceInputs
        , dtbrOutputs = dtbOutputs
        , dtbrCollateralReturn = dtbCollateralReturn
        , dtbrTotalCollateral = dtbTotalCollateral
        , dtbrCerts = dtbCerts
        , dtbrWithdrawals = dtbWithdrawals
        , dtbrFee = dtbTxfee
        , dtbrVldt = dtbVldt
        , dtbrGuards = dtbGuards
        , dtbrMint = dtbMint
        , dtbrScriptIntegrityHash = dtbScriptIntegrityHash
        , dtbrAuxDataHash = dtbAdHash
        , dtbrNetworkId = dtbTxNetworkId
        , dtbrVotingProcedures = dtbVotingProcedures
        , dtbrProposalProcedures = dtbProposalProcedures
        , dtbrCurrentTreasuryValue = dtbCurrentTreasuryValue
        , dtbrTreasuryDonation = dtbTreasuryDonation
        }
    )
  where
    DijkstraTxBody
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
      guards
      mintX
      scriptIntegrityHashX
      adHashX
      txnetworkidX
      votingProcedures
      proposalProcedures
      currentTreasuryValue
      treasuryDonation =
        mkMemoizedEra @DijkstraEra $
          DijkstraTxBodyRaw
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
            guards
            mintX
            scriptIntegrityHashX
            adHashX
            txnetworkidX
            votingProcedures
            proposalProcedures
            currentTreasuryValue
            treasuryDonation

pattern DijkstraSubTxBody ::
  Set TxIn ->
  Set TxIn ->
  StrictSeq (Sized (TxOut DijkstraEra)) ->
  OSet.OSet (TxCert DijkstraEra) ->
  Withdrawals ->
  ValidityInterval ->
  OSet (Credential Guard) ->
  MultiAsset ->
  StrictMaybe ScriptIntegrityHash ->
  StrictMaybe TxAuxDataHash ->
  StrictMaybe Network ->
  VotingProcedures DijkstraEra ->
  OSet.OSet (ProposalProcedure DijkstraEra) ->
  StrictMaybe Coin ->
  Coin ->
  TxBody SubTx DijkstraEra
pattern DijkstraSubTxBody
  { dstbSpendInputs
  , dstbReferenceInputs
  , dstbOutputs
  , dstbCerts
  , dstbWithdrawals
  , dstbVldt
  , dstbGuards
  , dstbMint
  , dstbScriptIntegrityHash
  , dstbAdHash
  , dstbTxNetworkId
  , dstbVotingProcedures
  , dstbProposalProcedures
  , dstbCurrentTreasuryValue
  , dstbTreasuryDonation
  } <-
  ( getMemoRawType ->
      DijkstraSubTxBodyRaw
        { dstbrSpendInputs = dstbSpendInputs
        , dstbrReferenceInputs = dstbReferenceInputs
        , dstbrOutputs = dstbOutputs
        , dstbrCerts = dstbCerts
        , dstbrWithdrawals = dstbWithdrawals
        , dstbrVldt = dstbVldt
        , dstbrGuards = dstbGuards
        , dstbrMint = dstbMint
        , dstbrScriptIntegrityHash = dstbScriptIntegrityHash
        , dstbrAuxDataHash = dstbAdHash
        , dstbrNetworkId = dstbTxNetworkId
        , dstbrVotingProcedures = dstbVotingProcedures
        , dstbrProposalProcedures = dstbProposalProcedures
        , dstbrCurrentTreasuryValue = dstbCurrentTreasuryValue
        , dstbrTreasuryDonation = dstbTreasuryDonation
        }
    )
  where
    DijkstraSubTxBody
      inputsX
      referenceInputsX
      outputsX
      certsX
      withdrawalsX
      vldtX
      guards
      mintX
      scriptIntegrityHashX
      adHashX
      txnetworkidX
      votingProcedures
      proposalProcedures
      currentTreasuryValue
      treasuryDonation =
        mkMemoizedEra @DijkstraEra $
          DijkstraSubTxBodyRaw
            inputsX
            referenceInputsX
            outputsX
            certsX
            withdrawalsX
            vldtX
            guards
            mintX
            scriptIntegrityHashX
            adHashX
            txnetworkidX
            votingProcedures
            proposalProcedures
            currentTreasuryValue
            treasuryDonation

{-# COMPLETE DijkstraTxBody, DijkstraSubTxBody #-}

instance Memoized (TxBody l DijkstraEra) where
  type RawType (TxBody l DijkstraEra) = DijkstraTxBodyRaw l DijkstraEra

deriving newtype instance EncCBOR (TxBody l DijkstraEra)

type instance MemoHashIndex (DijkstraTxBodyRaw l DijkstraEra) = EraIndependentTxBody

instance HashAnnotated (TxBody l DijkstraEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

instance Typeable l => DecCBOR (Annotator (DijkstraTxBodyRaw l DijkstraEra)) where
  decCBOR = pure <$> decCBOR

deriving via
  Mem (DijkstraTxBodyRaw l DijkstraEra)
  instance
    Typeable l => DecCBOR (Annotator (TxBody l DijkstraEra))

instance HasEraTxLevel DijkstraTxBodyRaw DijkstraEra where
  toSTxLevel DijkstraTxBodyRaw {} = STopTx
  toSTxLevel DijkstraSubTxBodyRaw {} = SSubTx

instance HasEraTxLevel TxBody DijkstraEra where
  toSTxLevel = toSTxLevel . getMemoRawType

inputsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (Set TxIn)
inputsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrSpendInputs
        DijkstraSubTxBodyRaw {..} -> dstbrSpendInputs
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrSpendInputs = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrSpendInputs = y}
    )

outputsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l DijkstraEra) (StrictSeq (TxOut DijkstraEra))
outputsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> sizedValue <$> dtbrOutputs
        DijkstraSubTxBodyRaw {..} -> sizedValue <$> dstbrOutputs
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrOutputs = mkSized (eraProtVerLow @DijkstraEra) <$> y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrOutputs = mkSized (eraProtVerLow @DijkstraEra) <$> y}
    )

feeDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw TopTx era) Coin
feeDijkstraTxBodyRawL = lens dtbrFee (\txb x -> txb {dtbrFee = x})

auxDataHashDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (StrictMaybe TxAuxDataHash)
auxDataHashDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrAuxDataHash
        DijkstraSubTxBodyRaw {..} -> dstbrAuxDataHash
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrAuxDataHash = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrAuxDataHash = y}
    )

certsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (OSet (TxCert DijkstraEra))
certsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrCerts
        DijkstraSubTxBodyRaw {..} -> dstbrCerts
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrCerts = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrCerts = y}
    )

withdrawalsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) Withdrawals
withdrawalsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrWithdrawals
        DijkstraSubTxBodyRaw {..} -> dstbrWithdrawals
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrWithdrawals = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrWithdrawals = y}
    )

instance EraTxBody DijkstraEra where
  newtype TxBody l DijkstraEra = MkDijkstraTxBody (MemoBytes (DijkstraTxBodyRaw l DijkstraEra))
    deriving (Generic, SafeToHash, ToCBOR)

  mkBasicTxBody =
    asSTxBothLevels
      (mkMemoizedEra @DijkstraEra basicDijkstraTxBodyRaw)
      (mkMemoizedEra @DijkstraEra basicDijkstraSubTxBodyRaw)

  inputsTxBodyL = memoRawTypeL @DijkstraEra . inputsDijkstraTxBodyRawL
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL = memoRawTypeL @DijkstraEra . outputsDijkstraTxBodyRawL
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = memoRawTypeL @DijkstraEra . feeDijkstraTxBodyRawL
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = memoRawTypeL @DijkstraEra . auxDataHashDijkstraTxBodyRawL
  {-# INLINE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = to $ \txBody ->
    withBothTxLevels txBody (^. babbageSpendableInputsTxBodyF) (^. inputsTxBodyL)
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = babbageAllInputsTxBodyF
  {-# INLINE allInputsTxBodyF #-}

  withdrawalsTxBodyL = memoRawTypeL @DijkstraEra . withdrawalsDijkstraTxBodyRawL
  {-# INLINE withdrawalsTxBodyL #-}

  certsTxBodyL =
    memoRawTypeL @DijkstraEra
      . certsDijkstraTxBodyRawL
      . lens OSet.toStrictSeq (\_ x -> OSet.fromStrictSeq x)
  {-# INLINE certsTxBodyL #-}

  getTotalDepositsTxBody = dijkstraTotalDepositsTxBody

  getTotalRefundsTxBody pp lookupStakingDeposit lookupDRepDeposit txBody =
    getTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit (txBody ^. certsTxBodyL)

basicDijkstraSubTxBodyRaw :: DijkstraTxBodyRaw SubTx DijkstraEra
basicDijkstraSubTxBodyRaw =
  DijkstraSubTxBodyRaw
    mempty
    mempty
    mempty
    mempty
    (Withdrawals mempty)
    (ValidityInterval SNothing SNothing)
    mempty
    mempty
    SNothing
    SNothing
    SNothing
    (VotingProcedures mempty)
    mempty
    mempty
    mempty

upgradeGovAction ::
  forall era.
  (AlonzoEraPParams era, EraPParams (PreviousEra era)) =>
  GovAction (PreviousEra era) ->
  GovAction era
upgradeGovAction (ParameterChange x y z) =
  ParameterChange (coerce x) (upgradePParamsUpdate (emptyUpgradePParamsUpdate @era) y) z
upgradeGovAction (HardForkInitiation x y) = HardForkInitiation (coerce x) y
upgradeGovAction (TreasuryWithdrawals x y) = TreasuryWithdrawals x y
upgradeGovAction (NoConfidence x) = NoConfidence x
upgradeGovAction (UpdateCommittee x y z w) = UpdateCommittee x y z w
upgradeGovAction (NewConstitution x y) = NewConstitution x (coerce y)
upgradeGovAction InfoAction = InfoAction

upgradeProposals :: ProposalProcedure ConwayEra -> ProposalProcedure DijkstraEra
upgradeProposals ProposalProcedure {..} =
  ProposalProcedure
    { pProcDeposit = pProcDeposit
    , pProcReturnAddr = pProcReturnAddr
    , pProcGovAction = upgradeGovAction pProcGovAction
    , pProcAnchor = pProcAnchor
    }

dijkstraTotalDepositsTxBody ::
  PParams DijkstraEra -> (KeyHash StakePool -> Bool) -> TxBody l DijkstraEra -> Coin
dijkstraTotalDepositsTxBody pp isPoolRegisted txBody =
  getTotalDepositsTxCerts pp isPoolRegisted (txBody ^. certsTxBodyL)
    <+> conwayProposalsDeposits pp txBody

-- | This newtype wrapper lets us index into the guards with a ScriptHash. It
-- will return the index of the credential when using `indexOf` and the `fromIndex`
-- method returns a `Nothing` if the credential at the index being looked up is
-- actually a keyhash.
newtype GuardsScriptHashView = GuardsScriptHashView (OSet (Credential Guard))

instance Indexable ScriptHash GuardsScriptHashView where
  indexOf (AsItem sh) (GuardsScriptHashView s) =
    coerce $ indexOf @(Credential Guard) (AsItem $ ScriptHashObj sh) s
  fromIndex (AsIx i) (GuardsScriptHashView s) = toScriptHash =<< fromIndex @(Credential Guard) (AsIx i) s
    where
      toScriptHash (AsIxItem idx (ScriptHashObj sh)) = SJust $ AsIxItem idx sh
      toScriptHash _ = SNothing

dijkstraRedeemerPointer ::
  forall era l.
  DijkstraEraTxBody era =>
  TxBody l era ->
  DijkstraPlutusPurpose AsItem era ->
  StrictMaybe (DijkstraPlutusPurpose AsIx era)
dijkstraRedeemerPointer txBody = \case
  DijkstraMinting policyID ->
    DijkstraMinting <$> indexOf policyID (txBody ^. mintedTxBodyF)
  DijkstraSpending txIn ->
    DijkstraSpending <$> indexOf txIn (txBody ^. inputsTxBodyL)
  DijkstraRewarding rewardAccount ->
    DijkstraRewarding <$> indexOf rewardAccount (unWithdrawals (txBody ^. withdrawalsTxBodyL))
  DijkstraCertifying txCert ->
    DijkstraCertifying <$> indexOf txCert (txBody ^. certsTxBodyL)
  DijkstraVoting votingProcedure ->
    DijkstraVoting <$> indexOf votingProcedure (txBody ^. votingProceduresTxBodyL)
  DijkstraProposing proposalProcedure ->
    DijkstraProposing <$> indexOf proposalProcedure (txBody ^. proposalProceduresTxBodyL)
  DijkstraGuarding scriptHash ->
    DijkstraGuarding
      <$> indexOf scriptHash (GuardsScriptHashView $ txBody ^. guardsTxBodyL)

dijkstraRedeemerPointerInverse ::
  DijkstraEraTxBody era =>
  TxBody l era ->
  DijkstraPlutusPurpose AsIx era ->
  StrictMaybe (DijkstraPlutusPurpose AsIxItem era)
dijkstraRedeemerPointerInverse txBody = \case
  DijkstraMinting idx ->
    DijkstraMinting <$> fromIndex idx (txBody ^. mintedTxBodyF)
  DijkstraSpending idx ->
    DijkstraSpending <$> fromIndex idx (txBody ^. inputsTxBodyL)
  DijkstraRewarding idx ->
    DijkstraRewarding <$> fromIndex idx (unWithdrawals (txBody ^. withdrawalsTxBodyL))
  DijkstraCertifying idx ->
    DijkstraCertifying <$> fromIndex idx (txBody ^. certsTxBodyL)
  DijkstraVoting idx ->
    DijkstraVoting <$> fromIndex idx (txBody ^. votingProceduresTxBodyL)
  DijkstraProposing idx ->
    DijkstraProposing <$> fromIndex idx (txBody ^. proposalProceduresTxBodyL)
  DijkstraGuarding idx ->
    DijkstraGuarding <$> fromIndex idx (GuardsScriptHashView $ txBody ^. guardsTxBodyL)

vldtDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) ValidityInterval
vldtDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrVldt
        DijkstraSubTxBodyRaw {..} -> dstbrVldt
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrVldt = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrVldt = y}
    )

instance AllegraEraTxBody DijkstraEra where
  vldtTxBodyL = memoRawTypeL @DijkstraEra . vldtDijkstraTxBodyRawL
  {-# INLINE vldtTxBodyL #-}

mintDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) MultiAsset
mintDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrMint
        DijkstraSubTxBodyRaw {..} -> dstbrMint
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrMint = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrMint = y}
    )

instance MaryEraTxBody DijkstraEra where
  mintTxBodyL = memoRawTypeL @DijkstraEra . mintDijkstraTxBodyRawL
  {-# INLINE mintTxBodyL #-}

collateralInputsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw TopTx era) (Set TxIn)
collateralInputsDijkstraTxBodyRawL =
  lens dtbrCollateralInputs $ \txb x -> txb {dtbrCollateralInputs = x}

scriptIntegrityHashDijkstraTxBodyRawL ::
  Lens' (DijkstraTxBodyRaw l era) (StrictMaybe ScriptIntegrityHash)
scriptIntegrityHashDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrScriptIntegrityHash
        DijkstraSubTxBodyRaw {..} -> dstbrScriptIntegrityHash
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrScriptIntegrityHash = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrScriptIntegrityHash = y}
    )

networkIdDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (StrictMaybe Network)
networkIdDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrNetworkId
        DijkstraSubTxBodyRaw {..} -> dstbrNetworkId
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrNetworkId = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrNetworkId = y}
    )

instance AlonzoEraTxBody DijkstraEra where
  collateralInputsTxBodyL = memoRawTypeL @DijkstraEra . collateralInputsDijkstraTxBodyRawL
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL = notSupportedInThisEraL
  {-# INLINE reqSignerHashesTxBodyL #-}

  reqSignerHashesTxBodyG = guardsTxBodyL . to (foldr' insertKeyHash mempty . OSet.toSet)
    where
      insertKeyHash (KeyHashObj kh) = Set.insert $ coerceKeyRole kh
      insertKeyHash (ScriptHashObj _) = id
  {-# INLINE reqSignerHashesTxBodyG #-}

  scriptIntegrityHashTxBodyL = memoRawTypeL @DijkstraEra . scriptIntegrityHashDijkstraTxBodyRawL
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = memoRawTypeL @DijkstraEra . networkIdDijkstraTxBodyRawL
  {-# INLINE networkIdTxBodyL #-}

  redeemerPointer = dijkstraRedeemerPointer

  redeemerPointerInverse = dijkstraRedeemerPointerInverse

collateralReturnDijkstraTxBodyRawL ::
  Lens' (DijkstraTxBodyRaw TopTx era) (StrictMaybe (BabbageTxOut DijkstraEra))
collateralReturnDijkstraTxBodyRawL =
  lens (fmap sizedValue . dtbrCollateralReturn) $
    \txb x -> txb {dtbrCollateralReturn = mkSized (eraProtVerLow @DijkstraEra) <$> x}

totalCollateralDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw TopTx era) (StrictMaybe Coin)
totalCollateralDijkstraTxBodyRawL =
  lens dtbrTotalCollateral $
    \txb x -> txb {dtbrTotalCollateral = x}

referenceInputsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (Set TxIn)
referenceInputsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrReferenceInputs
        DijkstraSubTxBodyRaw {..} -> dstbrReferenceInputs
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrReferenceInputs = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrReferenceInputs = y}
    )

instance BabbageEraTxBody DijkstraEra where
  sizedOutputsTxBodyL =
    lensMemoRawType @DijkstraEra
      ( \case
          DijkstraTxBodyRaw {..} -> dtbrOutputs
          DijkstraSubTxBodyRaw {..} -> dstbrOutputs
      )
      ( \case
          x@DijkstraTxBodyRaw {} -> \y -> x {dtbrOutputs = y}
          x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrOutputs = y}
      )
  {-# INLINE sizedOutputsTxBodyL #-}

  referenceInputsTxBodyL = memoRawTypeL @DijkstraEra . referenceInputsDijkstraTxBodyRawL
  {-# INLINE referenceInputsTxBodyL #-}

  totalCollateralTxBodyL = memoRawTypeL @DijkstraEra . totalCollateralDijkstraTxBodyRawL
  {-# INLINE totalCollateralTxBodyL #-}

  collateralReturnTxBodyL = memoRawTypeL @DijkstraEra . collateralReturnDijkstraTxBodyRawL
  {-# INLINE collateralReturnTxBodyL #-}

  sizedCollateralReturnTxBodyL =
    lensMemoRawType @DijkstraEra dtbrCollateralReturn $
      \txb x -> txb {dtbrCollateralReturn = x}
  {-# INLINE sizedCollateralReturnTxBodyL #-}

  allSizedOutputsTxBodyF = to $ \txBody ->
    withBothTxLevels txBody (^. allSizedOutputsBabbageTxBodyF) (^. sizedOutputsTxBodyL)
  {-# INLINE allSizedOutputsTxBodyF #-}

votingProceduresDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (VotingProcedures DijkstraEra)
votingProceduresDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrVotingProcedures
        DijkstraSubTxBodyRaw {..} -> dstbrVotingProcedures
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrVotingProcedures = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrVotingProcedures = y}
    )

proposalProceduresDijkstraTxBodyRawL ::
  Lens' (DijkstraTxBodyRaw l era) (OSet (ProposalProcedure DijkstraEra))
proposalProceduresDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrProposalProcedures
        DijkstraSubTxBodyRaw {..} -> dstbrProposalProcedures
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrProposalProcedures = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrProposalProcedures = y}
    )

treasuryDonationDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) Coin
treasuryDonationDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrTreasuryDonation
        DijkstraSubTxBodyRaw {..} -> dstbrTreasuryDonation
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrTreasuryDonation = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrTreasuryDonation = y}
    )

currentTreasuryValueDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (StrictMaybe Coin)
currentTreasuryValueDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrCurrentTreasuryValue
        DijkstraSubTxBodyRaw {..} -> dstbrCurrentTreasuryValue
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrCurrentTreasuryValue = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrCurrentTreasuryValue = y}
    )

instance ConwayEraTxBody DijkstraEra where
  votingProceduresTxBodyL = memoRawTypeL @DijkstraEra . votingProceduresDijkstraTxBodyRawL
  {-# INLINE votingProceduresTxBodyL #-}
  proposalProceduresTxBodyL = memoRawTypeL @DijkstraEra . proposalProceduresDijkstraTxBodyRawL
  {-# INLINE proposalProceduresTxBodyL #-}
  currentTreasuryValueTxBodyL = memoRawTypeL @DijkstraEra . currentTreasuryValueDijkstraTxBodyRawL
  {-# INLINE currentTreasuryValueTxBodyL #-}
  treasuryDonationTxBodyL = memoRawTypeL @DijkstraEra . treasuryDonationDijkstraTxBodyRawL
  {-# INLINE treasuryDonationTxBodyL #-}

class ConwayEraTxBody era => DijkstraEraTxBody era where
  guardsTxBodyL :: Lens' (TxBody l era) (OSet (Credential Guard))

guardsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (OSet (Credential Guard))
guardsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {..} -> dtbrGuards
        DijkstraSubTxBodyRaw {..} -> dstbrGuards
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrGuards = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrGuards = y}
    )

instance DijkstraEraTxBody DijkstraEra where
  {-# INLINE guardsTxBodyL #-}
  guardsTxBodyL = memoRawTypeL @DijkstraEra . guardsDijkstraTxBodyRawL

-- | Decoder for decoding guards in a backwards-compatible manner. It peeks at
-- the first element and if it's a credential, it decodes the rest of the
-- elements as credentials. If the first element is a plain keyhash, it will
-- decode rest of the elements as keyhashes.
decodeGuards :: Decoder s (OSet (Credential Guard))
decodeGuards = do
  elementsAreCredentials <- liftST $ newSTRef Nothing
  let
    decodeElement = do
      liftST (readSTRef elementsAreCredentials) >>= \case
        Nothing -> do
          tokenType <- peekTokenType
          liftST . writeSTRef elementsAreCredentials . Just $ case tokenType of
            TypeListLen -> True
            TypeListLen64 -> True
            TypeListLenIndef -> True
            _ -> False
          decodeElement
        Just True -> decCBOR
        Just False -> KeyHashObj <$> decCBOR
  decodeOSet decodeElement
