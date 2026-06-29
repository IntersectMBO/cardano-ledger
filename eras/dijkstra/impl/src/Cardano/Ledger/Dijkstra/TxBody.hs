{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
    dtbSubTransactions,
    dtbDirectDeposits,
    dtbAccountBalanceIntervals,
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
    dstbRequiredTopLevelGuards,
    dstbGuards,
    dstbDirectDeposits,
    dstbAccountBalanceIntervals
  ),
  upgradeProposals,
  upgradeGovAction,
  decodeGuards,
  DijkstraTxBodyRaw (..),
  basicDijkstraTxBodyRaw,
  inputsDijkstraTxBodyRawL,
  outputsDijkstraTxBodyRawL,
  feeDijkstraTxBodyRawL,
  vldtDijkstraTxBodyRawL,
  certsDijkstraTxBodyRawL,
  withdrawalsDijkstraTxBodyRawL,
  auxDataHashDijkstraTxBodyRawL,
  mintDijkstraTxBodyRawL,
  scriptIntegrityHashDijkstraTxBodyRawL,
  collateralInputsDijkstraTxBodyRawL,
  guardsDijkstraTxBodyRawL,
  networkIdDijkstraTxBodyRawL,
  collateralReturnDijkstraTxBodyRawL,
  totalCollateralDijkstraTxBodyRawL,
  referenceInputsDijkstraTxBodyRawL,
  votingProceduresDijkstraTxBodyRawL,
  proposalProceduresDijkstraTxBodyRawL,
  currentTreasuryValueDijkstraTxBodyRawL,
  treasuryDonationDijkstraTxBodyRawL,
  subTransactionsDijkstraTxBodyRawL,
  requiredTopLevelGuardsDijkstraTxBodyRawL,
  directDepositsDijkstraTxBodyRawL,
  accountBalanceIntervalsDijkstraTxBodyRawL,
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.Allegra.Scripts (invalidBeforeL, invalidHereAfterL)
import Cardano.Ledger.Alonzo.TxBody (Indexable (..))
import Cardano.Ledger.Babbage.TxBody (
  allSizedOutputsBabbageTxBodyF,
  babbageAllInputsTxBodyF,
  babbageSpendableInputsTxBodyF,
 )
import Cardano.Ledger.BaseTypes (Network, StrictMaybe (..), fromSMaybe)
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
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
import Cardano.Ledger.Dijkstra.Scripts (AccountBalanceIntervals (..), DijkstraPlutusPurpose (..))
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
import Cardano.Ledger.Plutus.Data (Data)
import Cardano.Ledger.TxIn (TxId, TxIn)
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData (..), deepseq)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.OMap.Strict (OMap)
import qualified Data.OMap.Strict as OMap
import Data.OSet.Strict (OSet, decodeOSet)
import qualified Data.OSet.Strict as OSet
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set, foldr')
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, to, (.~), (^.))
import NoThunks.Class (InspectHeap (..), NoThunks)

data DijkstraTxBodyRaw l era where
  DijkstraTxBodyRaw ::
    { dtbrSpendInputs :: !(Set TxIn)
    , dtbrCollateralInputs :: !(Set TxIn)
    , dtbrReferenceInputs :: !(Set TxIn)
    , dtbrOutputs :: !(StrictSeq (Sized (TxOut era)))
    , dtbrCollateralReturn :: !(StrictMaybe (Sized (TxOut era)))
    , dtbrTotalCollateral :: !(StrictMaybe Coin)
    , dtbrCerts :: !(OSet.OSet (TxCert era))
    , dtbrWithdrawals :: !Withdrawals
    , dtbrFee :: !Coin
    , dtbrVldt :: !ValidityInterval
    , dtbrGuards :: !(OSet (Credential Guard))
    , dtbrMint :: !MultiAsset
    , dtbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
    , dtbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
    , dtbrNetworkId :: !(StrictMaybe Network)
    , dtbrVotingProcedures :: !(VotingProcedures era)
    , dtbrProposalProcedures :: !(OSet.OSet (ProposalProcedure era))
    , dtbrCurrentTreasuryValue :: !(StrictMaybe Coin)
    , dtbrTreasuryDonation :: !Coin
    , dtbrSubTransactions :: !(OMap TxId (Tx SubTx era))
    , dtbrDirectDeposits :: !DirectDeposits
    , dtbrAccountBalanceIntervals :: !(AccountBalanceIntervals era)
    } ->
    DijkstraTxBodyRaw TopTx era
  DijkstraSubTxBodyRaw ::
    { dstbrSpendInputs :: !(Set TxIn)
    , dstbrReferenceInputs :: !(Set TxIn)
    , dstbrOutputs :: !(StrictSeq (Sized (TxOut era)))
    , dstbrCerts :: !(OSet.OSet (TxCert era))
    , dstbrWithdrawals :: !Withdrawals
    , dstbrVldt :: !ValidityInterval
    , dstbrGuards :: !(OSet (Credential Guard))
    , dstbrMint :: !MultiAsset
    , dstbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
    , dstbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
    , dstbrNetworkId :: !(StrictMaybe Network)
    , dstbrVotingProcedures :: !(VotingProcedures era)
    , dstbrProposalProcedures :: !(OSet.OSet (ProposalProcedure era))
    , dstbrCurrentTreasuryValue :: !(StrictMaybe Coin)
    , dstbrTreasuryDonation :: !Coin
    , dstbrRequiredTopLevelGuards :: !(Map (Credential Guard) (StrictMaybe (Data era)))
    , dstbrDirectDeposits :: !DirectDeposits
    , dstbrAccountBalanceIntervals :: !(AccountBalanceIntervals era)
    } ->
    DijkstraTxBodyRaw SubTx era

deriving instance (EraTxBody era, Eq (Tx SubTx era)) => Eq (DijkstraTxBodyRaw l era)

instance
  ( Eq (Tx SubTx DijkstraEra)
  , NFData (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  EqRaw (TxBody l DijkstraEra)

deriving via
  InspectHeap (DijkstraTxBodyRaw l era)
  instance
    (Typeable l, EraTxBody era) => NoThunks (DijkstraTxBodyRaw l era)

instance (EraTxBody era, NFData (Tx SubTx era)) => NFData (DijkstraTxBodyRaw l era) where
  rnf txBodyRaw@(DijkstraTxBodyRaw _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) =
    let DijkstraTxBodyRaw {..} = txBodyRaw
     in dtbrSpendInputs `deepseq`
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
                                            dtbrTreasuryDonation `deepseq`
                                              dtbrSubTransactions `deepseq`
                                                dtbrDirectDeposits `deepseq`
                                                  rnf dtbrAccountBalanceIntervals
  rnf txBodyRaw@(DijkstraSubTxBodyRaw _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) =
    let DijkstraSubTxBodyRaw {..} = txBodyRaw
     in dstbrSpendInputs `deepseq`
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
                                    dstbrTreasuryDonation `deepseq`
                                      dstbrRequiredTopLevelGuards `deepseq`
                                        dstbrDirectDeposits `deepseq`
                                          rnf dstbrAccountBalanceIntervals

deriving instance (EraTxBody era, Show (Tx SubTx era)) => Show (DijkstraTxBodyRaw l era)

basicDijkstraTxBodyRaw :: EraTxBody era => STxBothLevels l era -> DijkstraTxBodyRaw l era
basicDijkstraTxBodyRaw STopTx =
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
    OMap.Empty
    (DirectDeposits mempty)
    (AccountBalanceIntervals mempty)
basicDijkstraTxBodyRaw SSubTx =
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
    mempty
    (DirectDeposits mempty)
    (AccountBalanceIntervals mempty)

instance
  ( Typeable l
  , EraTxBody era
  , DecCBOR (Annotator (Tx SubTx era))
  , OMap.HasOKey TxId (Tx SubTx era)
  ) =>
  DecCBOR (Annotator (DijkstraTxBodyRaw l era))
  where
  decCBOR = withSTxBothLevels @l $ \sTxLevel ->
    decode $
      SparseKeyed
        "TxBodyRaw"
        (pure $ basicDijkstraTxBodyRaw sTxLevel)
        (bodyFields sTxLevel)
        (requiredFields sTxLevel)
    where
      bodyFields :: STxBothLevels l era -> Word -> Field (Annotator (DijkstraTxBodyRaw l era))
      bodyFields sTxLevel = \case
        0 -> fieldA (inputsDijkstraTxBodyRawL .~) From
        1 -> fieldA (outputsDijkstraTxBodyRawL .~) From
        2 | STopTx <- sTxLevel -> fieldA (feeDijkstraTxBodyRawL .~) From
        3 -> ofieldA (vldtDijkstraTxBodyRawL . invalidHereAfterL .~) From
        4 ->
          fieldAGuarded
            (emptyFailure "Certificates" "non-empty")
            OSet.null
            (certsDijkstraTxBodyRawL .~)
            From
        5 ->
          fieldAGuarded
            (emptyFailure "Withdrawals" "non-empty")
            (null . unWithdrawals)
            (withdrawalsDijkstraTxBodyRawL .~)
            From
        7 -> ofieldA (auxDataHashDijkstraTxBodyRawL .~) From
        8 -> ofieldA (vldtDijkstraTxBodyRawL . invalidBeforeL .~) From
        9 ->
          fieldAGuarded
            (emptyFailure "Mint" "non-empty")
            (== mempty)
            (mintDijkstraTxBodyRawL .~)
            From
        11 -> ofieldA (scriptIntegrityHashDijkstraTxBodyRawL .~) From
        13
          | STopTx <- sTxLevel ->
              fieldAGuarded
                (emptyFailure "Collateral Inputs" "non-empty")
                null
                (collateralInputsDijkstraTxBodyRawL .~)
                From
        14 ->
          ofieldA
            (\x -> guardsDijkstraTxBodyRawL .~ fromSMaybe mempty x)
            (D decodeGuards)
        15 -> ofieldA (networkIdDijkstraTxBodyRawL .~) From
        16
          | STopTx <- sTxLevel ->
              ofieldA (collateralReturnDijkstraTxBodyRawL .~) From
        17
          | STopTx <- sTxLevel ->
              ofieldA (totalCollateralDijkstraTxBodyRawL .~) From
        18 ->
          fieldAGuarded
            (emptyFailure "Reference Inputs" "non-empty")
            null
            (referenceInputsDijkstraTxBodyRawL .~)
            From
        19 ->
          fieldAGuarded
            (emptyFailure "VotingProcedures" "non-empty")
            (null . unVotingProcedures)
            (votingProceduresDijkstraTxBodyRawL .~)
            From
        20 ->
          fieldAGuarded
            (emptyFailure "ProposalProcedures" "non-empty")
            OSet.null
            (proposalProceduresDijkstraTxBodyRawL .~)
            From
        21 -> ofieldA (currentTreasuryValueDijkstraTxBodyRawL .~) From
        22 ->
          ofieldA
            (\x -> treasuryDonationDijkstraTxBodyRawL .~ fromSMaybe zero x)
            (D (decodePositiveCoin $ emptyFailure "Treasury Donation" "non-zero"))
        23
          | STopTx <- sTxLevel ->
              fieldAA (subTransactionsDijkstraTxBodyRawL .~) (D decodeSubTransactions)
        24
          | SSubTx <- sTxLevel ->
              fieldAGuarded
                (emptyFailure "RequiredTopLevelGuards" "non-empty")
                Map.null
                (requiredTopLevelGuardsDijkstraTxBodyRawL .~)
                (D (decodeMap decCBOR (decodeNullStrictMaybe decCBOR)))
        25 ->
          fieldAGuarded
            (emptyFailure "DirectDeposits" "non-empty")
            (null . unDirectDeposits)
            (directDepositsDijkstraTxBodyRawL .~)
            From
        26 ->
          fieldAGuarded
            (emptyFailure "AccountBalanceIntervals" "non-empty")
            (null . unAccountBalanceIntervals)
            (accountBalanceIntervalsDijkstraTxBodyRawL .~)
            From
        n -> invalidField n
      decodeSubTransactions :: Decoder s (Annotator (OMap TxId (Tx SubTx era)))
      decodeSubTransactions =
        decodeNonEmptySetLikeEnforceNoDuplicatesAnn
          (flip (OMap.|>))
          (\o -> (OMap.size o, o))
      requiredFields :: STxBothLevels l era -> [(Word, String)]
      requiredFields = \case
        STopTx -> [(0, "inputs"), (1, "outputs"), (2, "fee")]
        SSubTx -> [(0, "inputs"), (1, "outputs")]

      emptyFailure fieldName requirement =
        "TxBody: '" <> fieldName <> "' must be " <> requirement <> " when supplied"

encodeTxBodyRaw ::
  (EraTxBody era, EncCBOR (Tx SubTx era)) =>
  DijkstraTxBodyRaw l era -> Encode (Closed Sparse) (DijkstraTxBodyRaw l era)
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
        !> Omit null (Key 23 $ To dtbrSubTransactions)
        !> Omit (null . unDirectDeposits) (Key 25 (To dtbrDirectDeposits))
        !> Omit (null . unAccountBalanceIntervals) (Key 26 (To dtbrAccountBalanceIntervals))
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
        !> Omit
          (== mempty)
          (Key 24 $ E (encodeMap encCBOR (encodeNullStrictMaybe encCBOR)) dstbrRequiredTopLevelGuards)
        !> Omit (null . unDirectDeposits) (Key 25 (To dstbrDirectDeposits))
        !> Omit (null . unAccountBalanceIntervals) (Key 26 (To dstbrAccountBalanceIntervals))

instance
  ( EraTxBody era
  , EncCBOR (Tx SubTx era)
  ) =>
  EncCBOR (DijkstraTxBodyRaw l era)
  where
  encCBOR = encode . encodeTxBodyRaw

deriving instance
  ( Typeable l
  , Eq (Tx SubTx DijkstraEra)
  , NFData (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  NoThunks (TxBody l DijkstraEra)

deriving instance
  ( Eq (Tx SubTx DijkstraEra)
  , NFData (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  Eq (TxBody l DijkstraEra)

deriving newtype instance
  ( NFData (Tx SubTx DijkstraEra)
  , Eq (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  NFData (TxBody l DijkstraEra)

deriving instance
  ( Show (Tx SubTx DijkstraEra)
  , Eq (Tx SubTx DijkstraEra)
  , NFData (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  Show (TxBody l DijkstraEra)

pattern DijkstraTxBody ::
  ( EncCBOR (Tx SubTx DijkstraEra)
  , Eq (Tx SubTx DijkstraEra)
  , NFData (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
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
  OMap TxId (Tx SubTx DijkstraEra) ->
  DirectDeposits ->
  AccountBalanceIntervals DijkstraEra ->
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
  , dtbSubTransactions
  , dtbDirectDeposits
  , dtbAccountBalanceIntervals
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
        , dtbrSubTransactions = dtbSubTransactions
        , dtbrDirectDeposits = dtbDirectDeposits
        , dtbrAccountBalanceIntervals = dtbAccountBalanceIntervals
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
      treasuryDonation
      subTransactions
      directDeposits
      accountBalanceIntervals =
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
            subTransactions
            directDeposits
            accountBalanceIntervals

pattern DijkstraSubTxBody ::
  ( EncCBOR (Tx SubTx DijkstraEra)
  , Eq (Tx SubTx DijkstraEra)
  , NFData (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
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
  Map (Credential Guard) (StrictMaybe (Data DijkstraEra)) ->
  DirectDeposits ->
  AccountBalanceIntervals DijkstraEra ->
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
  , dstbRequiredTopLevelGuards
  , dstbDirectDeposits
  , dstbAccountBalanceIntervals
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
        , dstbrRequiredTopLevelGuards = dstbRequiredTopLevelGuards
        , dstbrDirectDeposits = dstbDirectDeposits
        , dstbrAccountBalanceIntervals = dstbAccountBalanceIntervals
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
      treasuryDonation
      requiredTopLevelGuards
      directDeposits
      accountBalanceIntervals =
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
            requiredTopLevelGuards
            directDeposits
            accountBalanceIntervals

{-# COMPLETE DijkstraTxBody, DijkstraSubTxBody #-}

instance Memoized (TxBody l DijkstraEra) where
  type RawType (TxBody l DijkstraEra) = DijkstraTxBodyRaw l DijkstraEra

deriving newtype instance EncCBOR (TxBody l DijkstraEra)

type instance MemoHashIndex (DijkstraTxBodyRaw l DijkstraEra) = EraIndependentTxBody

instance HashAnnotated (TxBody l DijkstraEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

deriving via
  Mem (DijkstraTxBodyRaw l DijkstraEra)
  instance
    ( Typeable l
    , Eq (Tx SubTx DijkstraEra)
    , NFData (Tx SubTx DijkstraEra)
    , Show (Tx SubTx DijkstraEra)
    , EncCBOR (Tx SubTx DijkstraEra)
    , DecCBOR (Annotator (Tx SubTx DijkstraEra))
    , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
    ) =>
    DecCBOR (Annotator (TxBody l DijkstraEra))

instance HasEraTxLevel DijkstraTxBodyRaw DijkstraEra where
  toSTxLevel DijkstraTxBodyRaw {} = STopTx
  toSTxLevel DijkstraSubTxBodyRaw {} = SSubTx

instance HasEraTxLevel TxBody DijkstraEra where
  toSTxLevel = toSTxLevel . getMemoRawType

inputsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (Set TxIn)
inputsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrSpendInputs} -> dtbrSpendInputs
        DijkstraSubTxBodyRaw {dstbrSpendInputs} -> dstbrSpendInputs
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrSpendInputs = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrSpendInputs = y}
    )

outputsDijkstraTxBodyRawL ::
  forall era l.
  EraTxOut era =>
  Lens' (DijkstraTxBodyRaw l era) (StrictSeq (TxOut era))
outputsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrOutputs} -> sizedValue <$> dtbrOutputs
        DijkstraSubTxBodyRaw {dstbrOutputs} -> sizedValue <$> dstbrOutputs
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrOutputs = mkSized (eraProtVerLow @era) <$> y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrOutputs = mkSized (eraProtVerLow @era) <$> y}
    )

feeDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw TopTx era) Coin
feeDijkstraTxBodyRawL = lens dtbrFee (\txb x -> txb {dtbrFee = x})

auxDataHashDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (StrictMaybe TxAuxDataHash)
auxDataHashDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrAuxDataHash} -> dtbrAuxDataHash
        DijkstraSubTxBodyRaw {dstbrAuxDataHash} -> dstbrAuxDataHash
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrAuxDataHash = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrAuxDataHash = y}
    )

certsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (OSet (TxCert era))
certsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrCerts} -> dtbrCerts
        DijkstraSubTxBodyRaw {dstbrCerts} -> dstbrCerts
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrCerts = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrCerts = y}
    )

withdrawalsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) Withdrawals
withdrawalsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrWithdrawals} -> dtbrWithdrawals
        DijkstraSubTxBodyRaw {dstbrWithdrawals} -> dstbrWithdrawals
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrWithdrawals = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrWithdrawals = y}
    )

directDepositsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) DirectDeposits
directDepositsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrDirectDeposits} -> dtbrDirectDeposits
        DijkstraSubTxBodyRaw {dstbrDirectDeposits} -> dstbrDirectDeposits
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrDirectDeposits = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrDirectDeposits = y}
    )

accountBalanceIntervalsDijkstraTxBodyRawL ::
  Lens' (DijkstraTxBodyRaw l era) (AccountBalanceIntervals era)
accountBalanceIntervalsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrAccountBalanceIntervals} -> dtbrAccountBalanceIntervals
        DijkstraSubTxBodyRaw {dstbrAccountBalanceIntervals} -> dstbrAccountBalanceIntervals
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrAccountBalanceIntervals = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrAccountBalanceIntervals = y}
    )

instance
  ( Eq (Tx SubTx DijkstraEra)
  , NFData (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  EraTxBody DijkstraEra
  where
  newtype TxBody l DijkstraEra = MkDijkstraTxBody (MemoBytes (DijkstraTxBodyRaw l DijkstraEra))
    deriving (Generic, SafeToHash, ToCBOR)

  mkBasicTxBody = mkMemoizedEra @DijkstraEra $ withSTxBothLevels basicDijkstraTxBodyRaw

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
  ConwayEraTxBody era => PParams era -> (KeyHash StakePool -> Bool) -> TxBody l era -> Coin
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
  DijkstraRewarding accountAddress ->
    DijkstraRewarding <$> indexOf accountAddress (unWithdrawals (txBody ^. withdrawalsTxBodyL))
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
        DijkstraTxBodyRaw {dtbrVldt} -> dtbrVldt
        DijkstraSubTxBodyRaw {dstbrVldt} -> dstbrVldt
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrVldt = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrVldt = y}
    )

instance
  ( Eq (Tx SubTx DijkstraEra)
  , NFData (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  AllegraEraTxBody DijkstraEra
  where
  vldtTxBodyL = memoRawTypeL @DijkstraEra . vldtDijkstraTxBodyRawL
  {-# INLINE vldtTxBodyL #-}

mintDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) MultiAsset
mintDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrMint} -> dtbrMint
        DijkstraSubTxBodyRaw {dstbrMint} -> dstbrMint
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrMint = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrMint = y}
    )

instance
  ( Eq (Tx SubTx DijkstraEra)
  , NFData (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  MaryEraTxBody DijkstraEra
  where
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
        DijkstraTxBodyRaw {dtbrScriptIntegrityHash} -> dtbrScriptIntegrityHash
        DijkstraSubTxBodyRaw {dstbrScriptIntegrityHash} -> dstbrScriptIntegrityHash
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrScriptIntegrityHash = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrScriptIntegrityHash = y}
    )

networkIdDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (StrictMaybe Network)
networkIdDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrNetworkId} -> dtbrNetworkId
        DijkstraSubTxBodyRaw {dstbrNetworkId} -> dstbrNetworkId
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrNetworkId = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrNetworkId = y}
    )

instance
  ( Eq (Tx SubTx DijkstraEra)
  , NFData (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  AlonzoEraTxBody DijkstraEra
  where
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
  forall era.
  EraTxBody era =>
  Lens' (DijkstraTxBodyRaw TopTx era) (StrictMaybe (TxOut era))
collateralReturnDijkstraTxBodyRawL =
  lens (fmap sizedValue . dtbrCollateralReturn) $
    \txb x -> txb {dtbrCollateralReturn = mkSized (eraProtVerLow @era) <$> x}

totalCollateralDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw TopTx era) (StrictMaybe Coin)
totalCollateralDijkstraTxBodyRawL =
  lens dtbrTotalCollateral $
    \txb x -> txb {dtbrTotalCollateral = x}

referenceInputsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (Set TxIn)
referenceInputsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrReferenceInputs} -> dtbrReferenceInputs
        DijkstraSubTxBodyRaw {dstbrReferenceInputs} -> dstbrReferenceInputs
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrReferenceInputs = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrReferenceInputs = y}
    )

instance
  ( NFData (Tx SubTx DijkstraEra)
  , Eq (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  BabbageEraTxBody DijkstraEra
  where
  sizedOutputsTxBodyL =
    lensMemoRawType @DijkstraEra
      ( \case
          DijkstraTxBodyRaw {dtbrOutputs} -> dtbrOutputs
          DijkstraSubTxBodyRaw {dstbrOutputs} -> dstbrOutputs
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

votingProceduresDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (VotingProcedures era)
votingProceduresDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrVotingProcedures} -> dtbrVotingProcedures
        DijkstraSubTxBodyRaw {dstbrVotingProcedures} -> dstbrVotingProcedures
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrVotingProcedures = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrVotingProcedures = y}
    )

proposalProceduresDijkstraTxBodyRawL ::
  Lens' (DijkstraTxBodyRaw l era) (OSet (ProposalProcedure era))
proposalProceduresDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrProposalProcedures} -> dtbrProposalProcedures
        DijkstraSubTxBodyRaw {dstbrProposalProcedures} -> dstbrProposalProcedures
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrProposalProcedures = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrProposalProcedures = y}
    )

treasuryDonationDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) Coin
treasuryDonationDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrTreasuryDonation} -> dtbrTreasuryDonation
        DijkstraSubTxBodyRaw {dstbrTreasuryDonation} -> dstbrTreasuryDonation
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrTreasuryDonation = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrTreasuryDonation = y}
    )

currentTreasuryValueDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (StrictMaybe Coin)
currentTreasuryValueDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrCurrentTreasuryValue} -> dtbrCurrentTreasuryValue
        DijkstraSubTxBodyRaw {dstbrCurrentTreasuryValue} -> dstbrCurrentTreasuryValue
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrCurrentTreasuryValue = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrCurrentTreasuryValue = y}
    )

instance
  ( NFData (Tx SubTx DijkstraEra)
  , Eq (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  ConwayEraTxBody DijkstraEra
  where
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

  subTransactionsTxBodyL :: Lens' (TxBody TopTx era) (OMap TxId (Tx SubTx era))

  requiredTopLevelGuardsL ::
    Lens' (TxBody SubTx era) (Map (Credential Guard) (StrictMaybe (Data era)))

  directDepositsTxBodyL :: Lens' (TxBody l era) DirectDeposits

  accountBalanceIntervalsTxBodyL :: Lens' (TxBody l era) (AccountBalanceIntervals era)

guardsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw l era) (OSet (Credential Guard))
guardsDijkstraTxBodyRawL =
  lens
    ( \case
        DijkstraTxBodyRaw {dtbrGuards} -> dtbrGuards
        DijkstraSubTxBodyRaw {dstbrGuards} -> dstbrGuards
    )
    ( \case
        x@DijkstraTxBodyRaw {} -> \y -> x {dtbrGuards = y}
        x@DijkstraSubTxBodyRaw {} -> \y -> x {dstbrGuards = y}
    )

subTransactionsDijkstraTxBodyRawL :: Lens' (DijkstraTxBodyRaw TopTx era) (OMap TxId (Tx SubTx era))
subTransactionsDijkstraTxBodyRawL = lens dtbrSubTransactions (\x y -> x {dtbrSubTransactions = y})

requiredTopLevelGuardsDijkstraTxBodyRawL ::
  Lens' (DijkstraTxBodyRaw SubTx era) (Map (Credential Guard) (StrictMaybe (Data era)))
requiredTopLevelGuardsDijkstraTxBodyRawL =
  lens dstbrRequiredTopLevelGuards (\x y -> x {dstbrRequiredTopLevelGuards = y})

instance
  ( NFData (Tx SubTx DijkstraEra)
  , Eq (Tx SubTx DijkstraEra)
  , Show (Tx SubTx DijkstraEra)
  , EncCBOR (Tx SubTx DijkstraEra)
  , DecCBOR (Annotator (Tx SubTx DijkstraEra))
  , OMap.HasOKey TxId (Tx SubTx DijkstraEra)
  ) =>
  DijkstraEraTxBody DijkstraEra
  where
  {-# INLINE guardsTxBodyL #-}
  guardsTxBodyL = memoRawTypeL @DijkstraEra . guardsDijkstraTxBodyRawL

  {-# INLINE subTransactionsTxBodyL #-}
  subTransactionsTxBodyL = memoRawTypeL @DijkstraEra . subTransactionsDijkstraTxBodyRawL

  requiredTopLevelGuardsL = memoRawTypeL @DijkstraEra . requiredTopLevelGuardsDijkstraTxBodyRawL

  directDepositsTxBodyL = memoRawTypeL @DijkstraEra . directDepositsDijkstraTxBodyRawL
  {-# INLINE directDepositsTxBodyL #-}

  accountBalanceIntervalsTxBodyL = memoRawTypeL @DijkstraEra . accountBalanceIntervalsDijkstraTxBodyRawL
  {-# INLINE accountBalanceIntervalsTxBodyL #-}

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
