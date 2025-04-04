{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Provides Babbage TxBody internals
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
module Cardano.Ledger.Babbage.TxBody.Internal (
  BabbageTxOut (
    BabbageTxOut,
    TxOutCompact,
    TxOutCompactDH,
    TxOutCompactDatum,
    TxOutCompactRefScript
  ),
  allSizedOutputsBabbageTxBodyF,
  babbageMinUTxOValue,
  BabbageTxBody (
    ..,
    BabbageTxBody,
    btbInputs,
    btbCollateral,
    btbReferenceInputs,
    btbOutputs,
    btbCollateralReturn,
    btbTotalCollateral,
    btbCerts,
    btbWithdrawals,
    btbTxFee,
    btbValidityInterval,
    btbUpdate,
    btbReqSignerHashes,
    btbMint,
    btbScriptIntegrityHash,
    btbAuxDataHash,
    btbTxNetworkId
  ),
  BabbageTxBodyRaw (..),
  BabbageTxBodyUpgradeError (..),
  babbageAllInputsTxBodyF,
  babbageSpendableInputsTxBodyF,
  BabbageEraTxBody (..),
  spendInputs',
  collateralInputs',
  referenceInputs',
  outputs',
  collateralReturn',
  totalCollateral',
  certs',
  withdrawals',
  txfee',
  vldt',
  update',
  reqSignerHashes',
  mint',
  scriptIntegrityHash',
  adHash',
  txnetworkid',
  getEitherAddrBabbageTxOut,
  EraIndependentScriptIntegrity,
  ScriptIntegrityHash,
  txOutData,
  txOutDataHash,
  txOutScript,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams (appExtraEntropy), appD)
import Cardano.Ledger.Alonzo.TxBody (alonzoRedeemerPointer, alonzoRedeemerPointerInverse)
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams (upgradeBabbagePParams)
import Cardano.Ledger.Babbage.Scripts ()
import Cardano.Ledger.Babbage.TxCert ()
import Cardano.Ledger.Babbage.TxOut hiding (TxOut)
import Cardano.Ledger.BaseTypes (
  Network (..),
  StrictMaybe (..),
  isSJust,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  Sized (..),
  ToCBOR (..),
  mkSized,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value (MaryValue (MaryValue), MultiAsset, PolicyID (..), policies)
import Cardano.Ledger.MemoBytes (
  EqRaw,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  eqRaw,
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
  zipMemoRawType,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (ProposedPPUpdates), Update (..))
import Cardano.Ledger.Shelley.TxBody (getShelleyGenesisKeyHashCountTxBody)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Foldable as F (foldl')
import Data.Sequence.Strict (StrictSeq, (|>))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Void (absurd)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

class (AlonzoEraTxBody era, BabbageEraTxOut era) => BabbageEraTxBody era where
  sizedOutputsTxBodyL :: Lens' (TxBody era) (StrictSeq (Sized (TxOut era)))

  referenceInputsTxBodyL :: Lens' (TxBody era) (Set TxIn)

  totalCollateralTxBodyL :: Lens' (TxBody era) (StrictMaybe Coin)

  collateralReturnTxBodyL :: Lens' (TxBody era) (StrictMaybe (TxOut era))

  sizedCollateralReturnTxBodyL :: Lens' (TxBody era) (StrictMaybe (Sized (TxOut era)))

  allSizedOutputsTxBodyF :: SimpleGetter (TxBody era) (StrictSeq (Sized (TxOut era)))

-- ======================================

data BabbageTxBodyRaw era = BabbageTxBodyRaw
  { btbrSpendInputs :: !(Set TxIn)
  , btbrCollateralInputs :: !(Set TxIn)
  , btbrReferenceInputs :: !(Set TxIn)
  , btbrOutputs :: !(StrictSeq (Sized (TxOut era)))
  , btbrCollateralReturn :: !(StrictMaybe (Sized (TxOut era)))
  , btbrTotalCollateral :: !(StrictMaybe Coin)
  , btbrCerts :: !(StrictSeq (TxCert era))
  , btbrWithdrawals :: !Withdrawals
  , btbrTxFee :: !Coin
  , btbrValidityInterval :: !ValidityInterval
  , btbrUpdate :: !(StrictMaybe (Update era))
  , btbrReqSignerHashes :: !(Set (KeyHash 'Witness))
  , btbrMint :: !MultiAsset
  , -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.MaryValue, not a Value.
    -- Operations on the TxBody in the BabbageEra depend upon this.
    -- We now store only the MultiAsset part of a Mary.Value.
    btbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
  , btbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
  , btbrTxNetworkId :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

-- We override this instance because the 'Sized' types also reference their
-- serialisation and as such cannot be compared directly. An alternative would
-- be to derive `EqRaw` for `Sized`.
instance
  (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
  EqRaw (BabbageTxBodyRaw era)
  where
  eqRaw a b =
    btbrSpendInputs a == btbrSpendInputs b
      && btbrCollateralInputs a == btbrCollateralInputs b
      && btbrReferenceInputs a == btbrReferenceInputs b
      && btbrOutputs a `eqSeqUnsized` btbrOutputs b
      && btbrCollateralReturn a `eqMbUnsized` btbrCollateralReturn b
      && btbrTotalCollateral a == btbrTotalCollateral b
      && btbrCerts a == btbrCerts b
      && btbrWithdrawals a == btbrWithdrawals b
      && btbrTxFee a == btbrTxFee b
      && btbrValidityInterval a == btbrValidityInterval b
      && btbrUpdate a == btbrUpdate b
      && btbrReqSignerHashes a == btbrReqSignerHashes b
      && btbrMint a == btbrMint b
      && btbrScriptIntegrityHash a == btbrScriptIntegrityHash b
      && btbrAuxDataHash a == btbrAuxDataHash b
      && btbrTxNetworkId a == btbrTxNetworkId b
    where
      eqMbUnsized x y = case (x, y) of
        (SJust a', SJust b') -> a' `eqUnsized` b'
        (SNothing, SNothing) -> True
        _ -> False
      eqSeqUnsized x y =
        length x == length y
          && F.foldl' (\acc (x', y') -> acc && x' `eqUnsized` y') True (StrictSeq.zip x y)
      eqUnsized x y = sizedValue x == sizedValue y

type instance MemoHashIndex (BabbageTxBodyRaw era) = EraIndependentTxBody

deriving instance
  (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
  Eq (BabbageTxBodyRaw era)

instance
  (Era era, NoThunks (TxOut era), NoThunks (TxCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (BabbageTxBodyRaw era)

instance
  (Era era, NFData (TxOut era), NFData (TxCert era), NFData (PParamsUpdate era)) =>
  NFData (BabbageTxBodyRaw era)

deriving instance
  (Era era, Show (TxOut era), Show (TxCert era), Show (PParamsUpdate era)) =>
  Show (BabbageTxBodyRaw era)

newtype BabbageTxBody era = TxBodyConstr (MemoBytes (BabbageTxBodyRaw era))
  deriving newtype (Generic, SafeToHash, ToCBOR)

deriving newtype instance
  ( Era era
  , DecCBOR (TxOut era)
  , DecCBOR (TxCert era)
  , DecCBOR (PParamsUpdate era)
  ) =>
  DecCBOR (BabbageTxBody era)

instance Memoized (BabbageTxBody era) where
  type RawType (BabbageTxBody era) = BabbageTxBodyRaw era

deriving newtype instance
  (Era era, NFData (TxOut era), NFData (TxCert era), NFData (PParamsUpdate era)) =>
  NFData (BabbageTxBody era)

inputsBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set TxIn)
inputsBabbageTxBodyL =
  lensMemoRawType @era btbrSpendInputs $ \txBodyRaw inputs -> txBodyRaw {btbrSpendInputs = inputs}
{-# INLINEABLE inputsBabbageTxBodyL #-}

outputsBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (TxOut era))
outputsBabbageTxBodyL =
  lensMemoRawType @era (fmap sizedValue . btbrOutputs) $
    \txBodyRaw outputs -> txBodyRaw {btbrOutputs = mkSized (eraProtVerLow @era) <$> outputs}
{-# INLINEABLE outputsBabbageTxBodyL #-}

feeBabbageTxBodyL :: forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) Coin
feeBabbageTxBodyL =
  lensMemoRawType @era btbrTxFee $
    \txBodyRaw fee -> txBodyRaw {btbrTxFee = fee}
{-# INLINEABLE feeBabbageTxBodyL #-}

auxDataHashBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe TxAuxDataHash)
auxDataHashBabbageTxBodyL =
  lensMemoRawType @era btbrAuxDataHash $
    \txBodyRaw auxDataHash -> txBodyRaw {btbrAuxDataHash = auxDataHash}
{-# INLINEABLE auxDataHashBabbageTxBodyL #-}

babbageSpendableInputsTxBodyF ::
  BabbageEraTxBody era => SimpleGetter (TxBody era) (Set TxIn)
babbageSpendableInputsTxBodyF =
  to $ \txBody ->
    (txBody ^. inputsTxBodyL)
      `Set.union` (txBody ^. collateralInputsTxBodyL)
{-# INLINEABLE babbageSpendableInputsTxBodyF #-}

babbageAllInputsTxBodyF ::
  BabbageEraTxBody era => SimpleGetter (TxBody era) (Set TxIn)
babbageAllInputsTxBodyF =
  to $ \txBody ->
    (txBody ^. inputsTxBodyL)
      `Set.union` (txBody ^. collateralInputsTxBodyL)
      `Set.union` (txBody ^. referenceInputsTxBodyL)
{-# INLINEABLE babbageAllInputsTxBodyF #-}

mintedBabbageTxBodyF :: SimpleGetter (BabbageTxBody era) (Set PolicyID)
mintedBabbageTxBodyF = to (policies . btbrMint . getMemoRawType)
{-# INLINEABLE mintedBabbageTxBodyF #-}

withdrawalsBabbbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) Withdrawals
withdrawalsBabbbageTxBodyL =
  lensMemoRawType @era btbrWithdrawals $
    \txBodyRaw withdrawals -> txBodyRaw {btbrWithdrawals = withdrawals}
{-# INLINEABLE withdrawalsBabbbageTxBodyL #-}

updateBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (Update era))
updateBabbageTxBodyL =
  lensMemoRawType @era btbrUpdate $
    \txBodyRaw update -> txBodyRaw {btbrUpdate = update}
{-# INLINEABLE updateBabbageTxBodyL #-}

certsBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (TxCert era))
certsBabbageTxBodyL =
  lensMemoRawType @era btbrCerts $
    \txBodyRaw certs -> txBodyRaw {btbrCerts = certs}
{-# INLINEABLE certsBabbageTxBodyL #-}

vldtBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) ValidityInterval
vldtBabbageTxBodyL =
  lensMemoRawType @era btbrValidityInterval $
    \txBodyRaw vldt -> txBodyRaw {btbrValidityInterval = vldt}
{-# INLINEABLE vldtBabbageTxBodyL #-}

mintBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) MultiAsset
mintBabbageTxBodyL =
  lensMemoRawType @era btbrMint $
    \txBodyRaw mint -> txBodyRaw {btbrMint = mint}
{-# INLINEABLE mintBabbageTxBodyL #-}

mintValueBabbageTxBodyF ::
  (BabbageEraTxBody era, Value era ~ MaryValue) =>
  SimpleGetter (BabbageTxBody era) (Value era)
mintValueBabbageTxBodyF = mintBabbageTxBodyL . to (MaryValue mempty)
{-# INLINEABLE mintValueBabbageTxBodyF #-}

collateralInputsBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set TxIn)
collateralInputsBabbageTxBodyL =
  lensMemoRawType @era btbrCollateralInputs $
    \txBodyRaw collateral -> txBodyRaw {btbrCollateralInputs = collateral}
{-# INLINEABLE collateralInputsBabbageTxBodyL #-}

reqSignerHashesBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (KeyHash 'Witness))
reqSignerHashesBabbageTxBodyL =
  lensMemoRawType @era btbrReqSignerHashes $
    \txBodyRaw reqSignerHashes -> txBodyRaw {btbrReqSignerHashes = reqSignerHashes}
{-# INLINEABLE reqSignerHashesBabbageTxBodyL #-}

scriptIntegrityHashBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe ScriptIntegrityHash)
scriptIntegrityHashBabbageTxBodyL =
  lensMemoRawType @era btbrScriptIntegrityHash $
    \txBodyRaw scriptIntegrityHash -> txBodyRaw {btbrScriptIntegrityHash = scriptIntegrityHash}
{-# INLINEABLE scriptIntegrityHashBabbageTxBodyL #-}

networkIdBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe Network)
networkIdBabbageTxBodyL =
  lensMemoRawType @era btbrTxNetworkId $
    \txBodyRaw networkId -> txBodyRaw {btbrTxNetworkId = networkId}
{-# INLINEABLE networkIdBabbageTxBodyL #-}

sizedOutputsBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (Sized (TxOut era)))
sizedOutputsBabbageTxBodyL =
  lensMemoRawType @era btbrOutputs $
    \txBodyRaw outputs -> txBodyRaw {btbrOutputs = outputs}
{-# INLINEABLE sizedOutputsBabbageTxBodyL #-}

referenceInputsBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set TxIn)
referenceInputsBabbageTxBodyL =
  lensMemoRawType @era btbrReferenceInputs $
    \txBodyRaw reference -> txBodyRaw {btbrReferenceInputs = reference}
{-# INLINEABLE referenceInputsBabbageTxBodyL #-}

totalCollateralBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe Coin)
totalCollateralBabbageTxBodyL =
  lensMemoRawType @era btbrTotalCollateral $
    \txBodyRaw totalCollateral -> txBodyRaw {btbrTotalCollateral = totalCollateral}
{-# INLINEABLE totalCollateralBabbageTxBodyL #-}

collateralReturnBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (TxOut era))
collateralReturnBabbageTxBodyL =
  lensMemoRawType @era (fmap sizedValue . btbrCollateralReturn) $
    \txBodyRaw collateralReturn ->
      txBodyRaw {btbrCollateralReturn = mkSized (eraProtVerLow @era) <$> collateralReturn}
{-# INLINEABLE collateralReturnBabbageTxBodyL #-}

sizedCollateralReturnBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (Sized (TxOut era)))
sizedCollateralReturnBabbageTxBodyL =
  lensMemoRawType @era btbrCollateralReturn $
    \txBodyRaw collateralReturn -> txBodyRaw {btbrCollateralReturn = collateralReturn}
{-# INLINEABLE sizedCollateralReturnBabbageTxBodyL #-}

allSizedOutputsBabbageTxBodyF ::
  BabbageEraTxBody era =>
  SimpleGetter (TxBody era) (StrictSeq (Sized (TxOut era)))
allSizedOutputsBabbageTxBodyF =
  to $ \txBody ->
    let txOuts = txBody ^. sizedOutputsTxBodyL
     in case txBody ^. sizedCollateralReturnTxBodyL of
          SNothing -> txOuts
          SJust collTxOut -> txOuts |> collTxOut
{-# INLINEABLE allSizedOutputsBabbageTxBodyF #-}

data BabbageTxBodyUpgradeError
  = -- | The update attempts to update the decentralistion parameter, which is
    -- dropped in Babbage.
    BTBUEUpdatesD
  | -- | The update attempts to update the extra entropy, which is dropped in
    --   Babbage.
    BTBUEUpdatesExtraEntropy
  deriving (Eq, Show)

instance EraTxBody BabbageEra where
  type TxBody BabbageEra = BabbageTxBody BabbageEra
  type TxBodyUpgradeError BabbageEra = BabbageTxBodyUpgradeError

  mkBasicTxBody = mkMemoizedEra @BabbageEra basicBabbageTxBodyRaw

  inputsTxBodyL = inputsBabbageTxBodyL
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL = outputsBabbageTxBodyL
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = feeBabbageTxBodyL
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = auxDataHashBabbageTxBodyL
  {-# INLINE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = babbageSpendableInputsTxBodyF
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = babbageAllInputsTxBodyF
  {-# INLINE allInputsTxBodyF #-}

  withdrawalsTxBodyL = withdrawalsBabbbageTxBodyL
  {-# INLINE withdrawalsTxBodyL #-}

  certsTxBodyL = certsBabbageTxBodyL
  {-# INLINE certsTxBodyL #-}

  getGenesisKeyHashCountTxBody = getShelleyGenesisKeyHashCountTxBody

  upgradeTxBody txBody = do
    certs <-
      traverse
        (left absurd . upgradeTxCert)
        (txBody ^. certsTxBodyL)
    updates <- traverse upgradeUpdate (txBody ^. updateTxBodyL)
    pure $
      BabbageTxBody
        { btbInputs = txBody ^. inputsTxBodyL
        , btbOutputs =
            mkSized (eraProtVerLow @BabbageEra) . upgradeTxOut <$> (txBody ^. outputsTxBodyL)
        , btbCerts = certs
        , btbWithdrawals = txBody ^. withdrawalsTxBodyL
        , btbTxFee = txBody ^. feeTxBodyL
        , btbValidityInterval = txBody ^. vldtTxBodyL
        , btbUpdate = updates
        , btbAuxDataHash = txBody ^. auxDataHashTxBodyL
        , btbMint = txBody ^. mintTxBodyL
        , btbCollateral = txBody ^. collateralInputsTxBodyL
        , btbReqSignerHashes = txBody ^. reqSignerHashesTxBodyL
        , btbScriptIntegrityHash = txBody ^. scriptIntegrityHashTxBodyL
        , btbTxNetworkId = txBody ^. networkIdTxBodyL
        , btbReferenceInputs = mempty
        , btbCollateralReturn = SNothing
        , btbTotalCollateral = SNothing
        }
    where
      upgradeUpdate ::
        Update AlonzoEra ->
        Either BabbageTxBodyUpgradeError (Update BabbageEra)
      upgradeUpdate (Update pp epoch) =
        Update <$> upgradeProposedPPUpdates pp <*> pure epoch

      -- Note that here we use 'upgradeBabbagePParams False' in order to
      -- preserve 'CoinsPerUTxOWord', in spite of the value now being
      -- semantically incorrect. Anything else will result in an invalid
      -- transaction.
      upgradeProposedPPUpdates ::
        ProposedPPUpdates AlonzoEra ->
        Either BabbageTxBodyUpgradeError (ProposedPPUpdates BabbageEra)
      upgradeProposedPPUpdates (ProposedPPUpdates m) =
        ProposedPPUpdates
          <$> traverse
            ( \(PParamsUpdate pphkd) -> do
                when (isSJust $ appD pphkd) $
                  Left BTBUEUpdatesD
                when (isSJust $ appExtraEntropy pphkd) $
                  Left BTBUEUpdatesExtraEntropy
                pure . PParamsUpdate $ upgradeBabbagePParams False pphkd
            )
            m

instance ShelleyEraTxBody BabbageEra where
  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINE ttlTxBodyL #-}

  updateTxBodyL = updateBabbageTxBodyL
  {-# INLINE updateTxBodyL #-}

instance AllegraEraTxBody BabbageEra where
  vldtTxBodyL = vldtBabbageTxBodyL
  {-# INLINE vldtTxBodyL #-}

instance MaryEraTxBody BabbageEra where
  mintTxBodyL = mintBabbageTxBodyL
  {-# INLINE mintTxBodyL #-}

  mintValueTxBodyF = mintValueBabbageTxBodyF
  {-# INLINE mintValueTxBodyF #-}

  mintedTxBodyF = mintedBabbageTxBodyF
  {-# INLINE mintedTxBodyF #-}

instance AlonzoEraTxBody BabbageEra where
  collateralInputsTxBodyL = collateralInputsBabbageTxBodyL
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL = reqSignerHashesBabbageTxBodyL
  {-# INLINE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL = scriptIntegrityHashBabbageTxBodyL
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = networkIdBabbageTxBodyL
  {-# INLINE networkIdTxBodyL #-}

  redeemerPointer = alonzoRedeemerPointer

  redeemerPointerInverse = alonzoRedeemerPointerInverse

instance BabbageEraTxBody BabbageEra where
  sizedOutputsTxBodyL = sizedOutputsBabbageTxBodyL
  {-# INLINE sizedOutputsTxBodyL #-}

  referenceInputsTxBodyL = referenceInputsBabbageTxBodyL
  {-# INLINE referenceInputsTxBodyL #-}

  totalCollateralTxBodyL = totalCollateralBabbageTxBodyL
  {-# INLINE totalCollateralTxBodyL #-}

  collateralReturnTxBodyL = collateralReturnBabbageTxBodyL
  {-# INLINE collateralReturnTxBodyL #-}

  sizedCollateralReturnTxBodyL = sizedCollateralReturnBabbageTxBodyL
  {-# INLINE sizedCollateralReturnTxBodyL #-}

  allSizedOutputsTxBodyF = allSizedOutputsBabbageTxBodyF
  {-# INLINE allSizedOutputsTxBodyF #-}

instance
  (Era era, Eq (PParamsUpdate era), Eq (TxOut era), Eq (TxCert era)) =>
  EqRaw (BabbageTxBody era)
  where
  eqRaw = zipMemoRawType eqRaw

deriving newtype instance
  (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
  Eq (BabbageTxBody era)

deriving instance
  (Era era, NoThunks (TxOut era), NoThunks (TxCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (BabbageTxBody era)

deriving instance
  (Era era, Show (TxOut era), Show (TxCert era), Show (PParamsUpdate era)) =>
  Show (BabbageTxBody era)

pattern BabbageTxBody ::
  forall era.
  BabbageEraTxBody era =>
  Set TxIn ->
  Set TxIn ->
  Set TxIn ->
  StrictSeq (Sized (TxOut era)) ->
  StrictMaybe (Sized (TxOut era)) ->
  StrictMaybe Coin ->
  StrictSeq (TxCert era) ->
  Withdrawals ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  Set (KeyHash 'Witness) ->
  MultiAsset ->
  StrictMaybe ScriptIntegrityHash ->
  StrictMaybe TxAuxDataHash ->
  StrictMaybe Network ->
  BabbageTxBody era
pattern BabbageTxBody
  { btbInputs
  , btbCollateral
  , btbReferenceInputs
  , btbOutputs
  , btbCollateralReturn
  , btbTotalCollateral
  , btbCerts
  , btbWithdrawals
  , btbTxFee
  , btbValidityInterval
  , btbUpdate
  , btbReqSignerHashes
  , btbMint
  , btbScriptIntegrityHash
  , btbAuxDataHash
  , btbTxNetworkId
  } <-
  ( getMemoRawType ->
      BabbageTxBodyRaw
        { btbrSpendInputs = btbInputs
        , btbrCollateralInputs = btbCollateral
        , btbrReferenceInputs = btbReferenceInputs
        , btbrOutputs = btbOutputs
        , btbrCollateralReturn = btbCollateralReturn
        , btbrTotalCollateral = btbTotalCollateral
        , btbrCerts = btbCerts
        , btbrWithdrawals = btbWithdrawals
        , btbrTxFee = btbTxFee
        , btbrValidityInterval = btbValidityInterval
        , btbrUpdate = btbUpdate
        , btbrReqSignerHashes = btbReqSignerHashes
        , btbrMint = btbMint
        , btbrScriptIntegrityHash = btbScriptIntegrityHash
        , btbrAuxDataHash = btbAuxDataHash
        , btbrTxNetworkId = btbTxNetworkId
        }
    )
  where
    BabbageTxBody
      inputs
      collateral
      referenceInputs
      outputs
      collateralReturn
      totalCollateral
      certs
      withdrawals
      txFee
      validityInterval
      update
      reqSignerHashes
      mint
      scriptIntegrityHash
      auxDataHash
      txNetworkId =
        mkMemoizedEra @era $
          BabbageTxBodyRaw
            { btbrSpendInputs = inputs
            , btbrCollateralInputs = collateral
            , btbrReferenceInputs = referenceInputs
            , btbrOutputs = outputs
            , btbrCollateralReturn = collateralReturn
            , btbrTotalCollateral = totalCollateral
            , btbrCerts = certs
            , btbrWithdrawals = withdrawals
            , btbrTxFee = txFee
            , btbrValidityInterval = validityInterval
            , btbrUpdate = update
            , btbrReqSignerHashes = reqSignerHashes
            , btbrMint = mint
            , btbrScriptIntegrityHash = scriptIntegrityHash
            , btbrAuxDataHash = auxDataHash
            , btbrTxNetworkId = txNetworkId
            }

{-# COMPLETE BabbageTxBody #-}

instance HashAnnotated (BabbageTxBody era) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (BabbageBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

spendInputs' :: BabbageTxBody era -> Set TxIn
collateralInputs' :: BabbageTxBody era -> Set TxIn
referenceInputs' :: BabbageTxBody era -> Set TxIn
outputs' :: BabbageTxBody era -> StrictSeq (TxOut era)
collateralReturn' :: BabbageTxBody era -> StrictMaybe (TxOut era)
totalCollateral' :: BabbageTxBody era -> StrictMaybe Coin
certs' :: BabbageTxBody era -> StrictSeq (TxCert era)
txfee' :: BabbageTxBody era -> Coin
withdrawals' :: BabbageTxBody era -> Withdrawals
vldt' :: BabbageTxBody era -> ValidityInterval
update' :: BabbageTxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: BabbageTxBody era -> Set (KeyHash 'Witness)
adHash' :: BabbageTxBody era -> StrictMaybe TxAuxDataHash
mint' :: BabbageTxBody era -> MultiAsset
scriptIntegrityHash' :: BabbageTxBody era -> StrictMaybe ScriptIntegrityHash
spendInputs' = btbrSpendInputs . getMemoRawType

txnetworkid' :: BabbageTxBody era -> StrictMaybe Network

collateralInputs' = btbrCollateralInputs . getMemoRawType

referenceInputs' = btbrReferenceInputs . getMemoRawType

outputs' = fmap sizedValue . btbrOutputs . getMemoRawType

collateralReturn' = fmap sizedValue . btbrCollateralReturn . getMemoRawType

totalCollateral' = btbrTotalCollateral . getMemoRawType

certs' = btbrCerts . getMemoRawType

withdrawals' = btbrWithdrawals . getMemoRawType

txfee' = btbrTxFee . getMemoRawType

vldt' = btbrValidityInterval . getMemoRawType

update' = btbrUpdate . getMemoRawType

reqSignerHashes' = btbrReqSignerHashes . getMemoRawType

adHash' = btbrAuxDataHash . getMemoRawType

mint' = btbrMint . getMemoRawType

scriptIntegrityHash' = btbrScriptIntegrityHash . getMemoRawType

txnetworkid' = btbrTxNetworkId . getMemoRawType

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (BabbageTxBody era)

instance
  (Era era, EncCBOR (TxOut era), EncCBOR (TxCert era), EncCBOR (PParamsUpdate era)) =>
  EncCBOR (BabbageTxBodyRaw era)
  where
  encCBOR
    BabbageTxBodyRaw
      { btbrSpendInputs
      , btbrCollateralInputs
      , btbrReferenceInputs
      , btbrOutputs
      , btbrCollateralReturn
      , btbrTotalCollateral
      , btbrCerts
      , btbrWithdrawals
      , btbrTxFee
      , btbrValidityInterval = ValidityInterval bot top
      , btbrUpdate
      , btbrReqSignerHashes
      , btbrMint
      , btbrScriptIntegrityHash
      , btbrAuxDataHash
      , btbrTxNetworkId
      } =
      encode $
        Keyed
          ( \i ifee ri o cr tc f t c w u b rsh mi sh ah ni ->
              BabbageTxBodyRaw i ifee ri o cr tc c w f (ValidityInterval b t) u rsh mi sh ah ni
          )
          !> Key 0 (To btbrSpendInputs)
          !> Omit null (Key 13 (To btbrCollateralInputs))
          !> Omit null (Key 18 (To btbrReferenceInputs))
          !> Key 1 (To btbrOutputs)
          !> encodeKeyedStrictMaybe 16 btbrCollateralReturn
          !> encodeKeyedStrictMaybe 17 btbrTotalCollateral
          !> Key 2 (To btbrTxFee)
          !> encodeKeyedStrictMaybe 3 top
          !> Omit null (Key 4 (To btbrCerts))
          !> Omit (null . unWithdrawals) (Key 5 (To btbrWithdrawals))
          !> encodeKeyedStrictMaybe 6 btbrUpdate
          !> encodeKeyedStrictMaybe 8 bot
          !> Omit null (Key 14 (To btbrReqSignerHashes))
          !> Omit (== mempty) (Key 9 (To btbrMint))
          !> encodeKeyedStrictMaybe 11 btbrScriptIntegrityHash
          !> encodeKeyedStrictMaybe 7 btbrAuxDataHash
          !> encodeKeyedStrictMaybe 15 btbrTxNetworkId

instance
  (Era era, DecCBOR (TxOut era), DecCBOR (TxCert era), DecCBOR (PParamsUpdate era)) =>
  DecCBOR (BabbageTxBodyRaw era)
  where
  decCBOR =
    decode $
      SparseKeyed
        "BabbageTxBodyRaw"
        basicBabbageTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field (BabbageTxBodyRaw era)
      bodyFields 0 = field (\x tx -> tx {btbrSpendInputs = x}) From
      bodyFields 13 = field (\x tx -> tx {btbrCollateralInputs = x}) From
      bodyFields 18 = field (\x tx -> tx {btbrReferenceInputs = x}) From
      bodyFields 1 = field (\x tx -> tx {btbrOutputs = x}) From
      bodyFields 16 = ofield (\x tx -> tx {btbrCollateralReturn = x}) From
      bodyFields 17 = ofield (\x tx -> tx {btbrTotalCollateral = x}) From
      bodyFields 2 = field (\x tx -> tx {btbrTxFee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {btbrValidityInterval = (btbrValidityInterval tx) {invalidHereafter = x}})
          From
      bodyFields 4 = field (\x tx -> tx {btbrCerts = x}) From
      bodyFields 5 = field (\x tx -> tx {btbrWithdrawals = x}) From
      bodyFields 6 = ofield (\x tx -> tx {btbrUpdate = x}) From
      bodyFields 7 = ofield (\x tx -> tx {btbrAuxDataHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {btbrValidityInterval = (btbrValidityInterval tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {btbrMint = x}) From
      bodyFields 11 = ofield (\x tx -> tx {btbrScriptIntegrityHash = x}) From
      bodyFields 14 = field (\x tx -> tx {btbrReqSignerHashes = x}) From
      bodyFields 15 = ofield (\x tx -> tx {btbrTxNetworkId = x}) From
      bodyFields n = invalidField n
      {-# INLINE bodyFields #-}
      requiredFields :: [(Word, String)]
      requiredFields =
        [ (0, "inputs")
        , (1, "outputs")
        , (2, "fee")
        ]
  {-# INLINE decCBOR #-}

basicBabbageTxBodyRaw :: BabbageTxBodyRaw era
basicBabbageTxBodyRaw =
  BabbageTxBodyRaw
    mempty
    mempty
    mempty
    StrictSeq.empty
    SNothing
    SNothing
    StrictSeq.empty
    (Withdrawals mempty)
    mempty
    (ValidityInterval SNothing SNothing)
    SNothing
    mempty
    mempty
    SNothing
    SNothing
    SNothing
