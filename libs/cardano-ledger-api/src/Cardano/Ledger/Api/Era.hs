{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Api.Era (
  -- * Eras
  Era (
    PreviousEra,
    ProtVerLow,
    ProtVerHigh
  ),
  EraApi (..),
  eraName,

  -- ** Byron
  ByronEra,

  -- ** Shelley
  Shelley,
  ShelleyEra,

  -- ** Allegra
  Allegra,
  AllegraEra,

  -- ** Mary
  Mary,
  MaryEra,

  -- ** Alonzo
  Alonzo,
  AlonzoEra,

  -- ** Babbage
  Babbage,
  BabbageEra,

  -- ** Conway
  Conway,
  ConwayEra,

  -- ** Latest Known
  LatestKnownEra,

  -- * Protocol version

  -- ** Value level
  eraProtVerHigh,
  eraProtVerLow,

  -- ** Type level constraints
  AtLeastEra,
  AtMostEra,
  ExactEra,
  ProtVerAtLeast,
  ProtVerAtMost,
  ProtVerInBounds,
  atLeastEra,
  atMostEra,
) where

import Cardano.Ledger.Allegra (Allegra, AllegraEra)
import Cardano.Ledger.Allegra.Scripts (translateTimelock)
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Allegra.TxBody (AllegraEraTxBody (..), ValidityInterval (..))
import qualified Cardano.Ledger.Allegra.TxBody as Allegra (TxBody (..))
import Cardano.Ledger.Alonzo (Alonzo, AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams (appExtraEntropy), appD)
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript, upgradePlutusPurposeAsIx)
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..), AlonzoTxAuxDataRaw (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..), TxBody (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..), unRedeemers)
import Cardano.Ledger.Babbage (Babbage, BabbageEra)
import Cardano.Ledger.Babbage.PParams (upgradeBabbagePParams)
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.BaseTypes (StrictMaybe (..), isSJust)
import Cardano.Ledger.Binary (mkSized, sizedValue)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.TxBody (TxBody (..))
import Cardano.Ledger.Conway.TxCert (ConwayTxCertUpgradeError)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (Mary, MaryEra, TxBody (..))
import Cardano.Ledger.Mary.TxBody (MaryEraTxBody (..))
import Cardano.Ledger.MemoBytes (mkMemoizedEra)
import Cardano.Ledger.Plutus.Data (upgradeData)
import Cardano.Ledger.Shelley (Shelley, ShelleyEra)
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxAuxData (ShelleyTxAuxData (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Cardano.Ledger.Slot (SlotNo)
import Control.Arrow (left)
import Control.Monad (unless, when)
import Data.Bifunctor (Bifunctor (first))
import Data.Default (def)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import Data.Set (Set)
import Data.Void (Void, absurd)
import Lens.Micro

-- | Sometimes it is useful to specify that a type corresponds to a latest era that is
-- currently implemented
type LatestKnownEra = ConwayEra

class EraApi era where
  type TxUpgradeError era :: Type
  type TxUpgradeError era = Void

  type TxBodyUpgradeError era :: Type
  type TxBodyUpgradeError era = Void

  upgradeTx ::
    EraTx (PreviousEra era) =>
    Tx (PreviousEra era) ->
    Either (TxUpgradeError era) (Tx era)

  -- | Upgrade the transaction body from the previous era.
  --
  -- This can fail where elements of the transaction body are deprecated.
  -- Compare this to `translateEraThroughCBOR`:
  -- - `upgradeTxBody` will use the Haskell representation, but will not
  --   preserve the serialised form. However, it will be suitable for iterated
  --   translation through eras.
  -- - `translateEraThroughCBOR` will preserve the binary representation, but is
  --   not guaranteed to work through multiple eras - that is, the serialised
  --   representation from era n is guaranteed valid in era n + 1, but not
  --   necessarily in era n + 2.
  upgradeTxBody ::
    EraTxBody (PreviousEra era) =>
    TxBody (PreviousEra era) ->
    Either (TxBodyUpgradeError era) (TxBody era)

  -- | Every era, except Shelley, must be able to upgrade a `TxAuxData` from a previous
  -- era.
  --
  -- /Warning/ - Important to note that any memoized binary representation will not be
  -- preserved. If you need to retain underlying bytes you can use `translateEraThroughCBOR`
  upgradeTxAuxData :: EraTxAuxData (PreviousEra era) => TxAuxData (PreviousEra era) -> TxAuxData era

  upgradeTxWits :: EraTxWits (PreviousEra era) => TxWits (PreviousEra era) -> TxWits era

instance EraApi ShelleyEra where
  upgradeTx =
    error
      "Calling this function will cause a compilation error, since there is no Tx instance for Byron"

  upgradeTxBody =
    error $
      "Calling this function will cause a compilation error, "
        ++ "since there is no `EraTxBody` instance for `ByronEra`"

  -- Calling this partial function will result in compilation error, since ByronEra has
  -- no instance for EraTxOut type class.
  upgradeTxAuxData = error "It is not possible to translate Byron TxOut with 'upgradeTxOut'"

  upgradeTxWits =
    error
      "Calling this function will cause a compilation error, since there is no TxWits instance for ByronEra"

instance EraApi AllegraEra where
  upgradeTx (ShelleyTx txb txwits txAux) =
    ShelleyTx
      <$> upgradeTxBody txb
      <*> pure (upgradeTxWits txwits)
      <*> pure (fmap upgradeTxAuxData txAux)

  upgradeTxBody txBody = do
    certs <- traverse upgradeTxCert (txBody ^. certsTxBodyL)
    pure $
      Allegra.AllegraTxBody
        { Allegra.atbInputs = txBody ^. inputsTxBodyL
        , Allegra.atbOutputs = upgradeTxOut <$> (txBody ^. outputsTxBodyL)
        , Allegra.atbCerts = certs
        , Allegra.atbWithdrawals = txBody ^. withdrawalsTxBodyL
        , Allegra.atbTxFee = txBody ^. feeTxBodyL
        , Allegra.atbValidityInterval = ttlToValidityInterval (txBody ^. ttlTxBodyL)
        , Allegra.atbUpdate = upgradeUpdate () <$> (txBody ^. updateTxBodyL)
        , Allegra.atbAuxDataHash = txBody ^. auxDataHashTxBodyL
        }
    where
      ttlToValidityInterval :: SlotNo -> ValidityInterval
      ttlToValidityInterval ttl = ValidityInterval SNothing (SJust ttl)

  upgradeTxAuxData (ShelleyTxAuxData md) = AllegraTxAuxData md mempty

  upgradeTxWits stw =
    ShelleyTxWits
      (addrWits stw)
      (upgradeScript <$> scriptWits stw)
      (bootWits stw)

instance EraApi MaryEra where
  upgradeTx (ShelleyTx txb txwits txAux) =
    ShelleyTx
      <$> upgradeTxBody txb
      <*> pure (upgradeTxWits txwits)
      <*> pure (fmap upgradeTxAuxData txAux)

  upgradeTxBody atb = do
    certs <- traverse upgradeTxCert (Allegra.atbCerts atb)
    pure $
      MaryTxBody
        { mtbInputs = Allegra.atbInputs atb
        , mtbOutputs = upgradeTxOut <$> Allegra.atbOutputs atb
        , mtbCerts = certs
        , mtbWithdrawals = Allegra.atbWithdrawals atb
        , mtbTxFee = Allegra.atbTxFee atb
        , mtbValidityInterval = Allegra.atbValidityInterval atb
        , mtbUpdate = upgradeUpdate () <$> Allegra.atbUpdate atb
        , mtbAuxDataHash = Allegra.atbAuxDataHash atb
        , mtbMint = mempty
        }

  upgradeTxAuxData (AllegraTxAuxData md scripts) = AllegraTxAuxData md $ upgradeScript <$> scripts

  upgradeTxWits stw =
    ShelleyTxWits
      (addrWits stw)
      (upgradeScript <$> scriptWits stw)
      (bootWits stw)

newtype AlonzoTxUpgradeError = ATUEBodyUpgradeError AlonzoTxBodyUpgradeError
  deriving (Show)

data AlonzoTxBodyUpgradeError
  = -- | The TxBody contains a protocol parameter update that attempts to update
    -- the min UTxO. Since this doesn't exist in Alonzo, we fail if an attempt is
    -- made to update it.
    ATBUEMinUTxOUpdated
  deriving (Show)

instance EraApi AlonzoEra where
  type TxUpgradeError AlonzoEra = AlonzoTxUpgradeError
  type TxBodyUpgradeError AlonzoEra = AlonzoTxBodyUpgradeError

  upgradeTx (ShelleyTx body wits aux) =
    AlonzoTx
      <$> left ATUEBodyUpgradeError (upgradeTxBody body)
      <*> pure (upgradeTxWits wits)
      <*> pure (IsValid True)
      <*> pure (fmap upgradeTxAuxData aux)

  upgradeTxBody
    MaryTxBody
      { mtbInputs
      , mtbOutputs
      , mtbCerts
      , mtbWithdrawals
      , mtbTxFee
      , mtbValidityInterval
      , mtbUpdate
      , mtbAuxDataHash
      , mtbMint
      } = do
      certs <-
        traverse
          (left absurd . upgradeTxCert)
          mtbCerts

      updates <- traverse upgradeUpdateEither mtbUpdate
      pure $
        AlonzoTxBody
          { atbInputs = mtbInputs
          , atbOutputs = upgradeTxOut <$> mtbOutputs
          , atbCerts = certs
          , atbWithdrawals = mtbWithdrawals
          , atbTxFee = mtbTxFee
          , atbValidityInterval = mtbValidityInterval
          , atbUpdate = updates
          , atbAuxDataHash = mtbAuxDataHash
          , atbMint = mtbMint
          , atbCollateral = mempty
          , atbReqSignerHashes = mempty
          , atbScriptIntegrityHash = SNothing
          , atbTxNetworkId = SNothing
          }
      where
        upgradeUpdateEither ::
          Update MaryEra ->
          Either AlonzoTxBodyUpgradeError (Update AlonzoEra)
        upgradeUpdateEither (Update pp epoch) =
          Update <$> upgradeProposedPPUpdates pp <*> pure epoch

        upgradeProposedPPUpdates ::
          ProposedPPUpdates MaryEra ->
          Either AlonzoTxBodyUpgradeError (ProposedPPUpdates AlonzoEra)
        upgradeProposedPPUpdates (ProposedPPUpdates m) =
          ProposedPPUpdates
            <$> traverse
              ( \ppu -> do
                  when (isSJust $ ppu ^. ppuMinUTxOValueL) $
                    Left ATBUEMinUTxOUpdated
                  pure $ upgradePParamsUpdate def ppu
              )
              m

  upgradeTxAuxData (AllegraTxAuxData md scripts) =
    mkMemoizedEra @AlonzoEra $
      AlonzoTxAuxDataRaw
        { atadrMetadata = md
        , atadrTimelock = translateTimelock <$> scripts
        , atadrPlutus = mempty
        }

  upgradeTxWits (ShelleyTxWits {addrWits, scriptWits, bootWits}) =
    AlonzoTxWits addrWits bootWits (upgradeScript <$> scriptWits) mempty (Redeemers mempty)

-- | Upgrade redeemers from one era to another. The underlying data structure
-- will remain identical, but the memoised serialisation may change to reflect
-- the versioned serialisation of the new era.
upgradeRedeemers ::
  forall era.
  (AlonzoEraScript (PreviousEra era), AlonzoEraScript era) =>
  Redeemers (PreviousEra era) ->
  Redeemers era
upgradeRedeemers =
  Redeemers
    . Map.mapKeys upgradePlutusPurposeAsIx
    . Map.map (first upgradeData)
    . unRedeemers

-- | Upgrade 'TxDats' from one era to another. The underlying data structure
-- will remain identical, but the memoised serialisation may change to reflect
-- the versioned serialisation of the new era.
upgradeTxDats ::
  (Era era1, Era era2) =>
  TxDats era1 ->
  TxDats era2
upgradeTxDats (TxDats datMap) = TxDats $ fmap upgradeData datMap

translateAlonzoTxAuxData ::
  (AlonzoEraScript era1, AlonzoEraScript era2) =>
  AlonzoTxAuxData era1 ->
  AlonzoTxAuxData era2
translateAlonzoTxAuxData AlonzoTxAuxData {atadMetadata, atadTimelock, atadPlutus} =
  AlonzoTxAuxData
    { atadMetadata = atadMetadata
    , atadTimelock = translateTimelock <$> atadTimelock
    , atadPlutus = atadPlutus
    }

newtype BabbageTxUpgradeError
  = BTUEBodyUpgradeError BabbageTxBodyUpgradeError
  deriving (Eq, Show)

data BabbageTxBodyUpgradeError
  = -- | The update attempts to update the decentralistion parameter, which is
    -- dropped in Babbage.
    BTBUEUpdatesD
  | -- | The update attempts to update the extra entropy, which is dropped in
    --   Babbage.
    BTBUEUpdatesExtraEntropy
  deriving (Eq, Show)

instance EraApi BabbageEra where
  type TxUpgradeError BabbageEra = BabbageTxUpgradeError
  type TxBodyUpgradeError BabbageEra = BabbageTxBodyUpgradeError

  upgradeTx (AlonzoTx b w valid aux) =
    AlonzoTx
      <$> left BTUEBodyUpgradeError (upgradeTxBody b)
      <*> pure (upgradeTxWits w)
      <*> pure valid
      <*> pure (fmap upgradeTxAuxData aux)

  upgradeTxBody txBody = do
    certs <-
      traverse
        (left absurd . upgradeTxCert)
        (txBody ^. certsTxBodyL)
    updates <- traverse upgradeUpdateEither (txBody ^. updateTxBodyL)
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
      upgradeUpdateEither ::
        Update AlonzoEra ->
        Either BabbageTxBodyUpgradeError (Update BabbageEra)
      upgradeUpdateEither (Update pp epoch) =
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

  upgradeTxAuxData = translateAlonzoTxAuxData

  upgradeTxWits atw =
    AlonzoTxWits
      { txwitsVKey = txwitsVKey atw
      , txwitsBoot = txwitsBoot atw
      , txscripts = upgradeScript <$> txscripts atw
      , txdats = upgradeTxDats (txdats atw)
      , txrdmrs = upgradeRedeemers (txrdmrs atw)
      }

data ConwayTxBodyUpgradeError
  = CTBUETxCert ConwayTxCertUpgradeError
  | -- | The TxBody contains an update proposal from a pre-Conway era. Since
    --   this can only have come from the genesis delegates, we just discard it.
    CTBUEContainsUpdate
  | -- | In eras prior to Conway duplicate certificates where allowed
    CTBUEContainsDuplicateCerts (Set (TxCert ConwayEra))
  deriving (Eq, Show)

instance EraApi ConwayEra where
  type TxUpgradeError ConwayEra = TxBodyUpgradeError ConwayEra
  type TxBodyUpgradeError ConwayEra = ConwayTxBodyUpgradeError

  upgradeTx (AlonzoTx b w valid aux) =
    AlonzoTx
      <$> upgradeTxBody b
      <*> pure (upgradeTxWits w)
      <*> pure valid
      <*> pure (fmap upgradeTxAuxData aux)

  upgradeTxBody btb = do
    when (isSJust (btbUpdate btb)) $ Left CTBUEContainsUpdate
    certs <- traverse (left CTBUETxCert . upgradeTxCert) (btbCerts btb)
    let (duplicates, certsOSet) = OSet.fromStrictSeqDuplicates certs
    unless (null duplicates) $ Left $ CTBUEContainsDuplicateCerts duplicates
    pure $
      ConwayTxBody
        { ctbSpendInputs = btbInputs btb
        , ctbOutputs =
            mkSized (eraProtVerLow @ConwayEra)
              . upgradeTxOut
              . sizedValue
              <$> btbOutputs btb
        , ctbCerts = certsOSet
        , ctbWithdrawals = btbWithdrawals btb
        , ctbTxfee = btbTxFee btb
        , ctbVldt = btbValidityInterval btb
        , ctbAdHash = btbAuxDataHash btb
        , ctbMint = btbMint btb
        , ctbCollateralInputs = btbCollateral btb
        , ctbReqSignerHashes = btbReqSignerHashes btb
        , ctbScriptIntegrityHash = btbScriptIntegrityHash btb
        , ctbTxNetworkId = btbTxNetworkId btb
        , ctbReferenceInputs = btbReferenceInputs btb
        , ctbCollateralReturn =
            mkSized (eraProtVerLow @ConwayEra)
              . upgradeTxOut
              . sizedValue
              <$> btbCollateralReturn btb
        , ctbTotalCollateral = btbTotalCollateral btb
        , ctbCurrentTreasuryValue = SNothing
        , ctbProposalProcedures = OSet.empty
        , ctbVotingProcedures = VotingProcedures mempty
        , ctbTreasuryDonation = Coin 0
        }

  upgradeTxAuxData = translateAlonzoTxAuxData

  upgradeTxWits atw =
    AlonzoTxWits
      { txwitsVKey = txwitsVKey atw
      , txwitsBoot = txwitsBoot atw
      , txscripts = upgradeScript <$> txscripts atw
      , txdats = upgradeTxDats (txdats atw)
      , txrdmrs = upgradeRedeemers (txrdmrs atw)
      }
