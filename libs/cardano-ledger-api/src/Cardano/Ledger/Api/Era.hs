{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Api.Era (
  -- * Eras
  Era (
    EraName,
    PreviousEra,
    ProtVerLow,
    ProtVerHigh
  ),
  EraApi (..),
  eraName,
  EraHasName (EraFromName),

  -- ** Byron
  ByronEra,

  -- ** Shelley
  ShelleyEra,
  ttlToValidityInterval,

  -- ** Allegra
  AllegraEra,

  -- ** Mary
  MaryEra,

  -- ** Alonzo
  AlonzoEra,

  -- ** Babbage
  BabbageEra,

  -- ** Conway
  ConwayEra,

  -- ** Dijkstra
  DijkstraEra,

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

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.Scripts (translateTimelock, upgradeMultiSig)
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Allegra.TxBody (
  AllegraEraTxBody (..),
  AllegraTxBodyRaw (..),
  ValidityInterval (..),
 )
import qualified Cardano.Ledger.Allegra.TxBody as Allegra (TxBody (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams (appExtraEntropy), appD)
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript, upgradePlutusPurposeAsIx)
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..), AlonzoTxAuxDataRaw (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..), AlonzoTxBodyRaw (..), TxBody (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..), unRedeemers)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams (upgradeBabbagePParams)
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babbage.TxBody (BabbageTxBodyRaw (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..), isSJust)
import Cardano.Ledger.Binary (mkSized, unsafeMapSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra, Tx (..))
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBodyRaw (..), TxBody (..))
import Cardano.Ledger.Conway.TxCert (ConwayTxCertUpgradeError)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Dijkstra.Tx (DijkstraTx (..), Tx (..))
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..), upgradeProposals)
import Cardano.Ledger.Dijkstra.TxCert (DijkstraTxCertUpgradeError)
import Cardano.Ledger.Internal.Era (EraHasName (..))
import Cardano.Ledger.Keys (HasKeyRole (..))
import Cardano.Ledger.Mary (MaryEra, TxBody (..))
import Cardano.Ledger.Mary.TxBody (MaryEraTxBody (..))
import Cardano.Ledger.MemoBytes (getMemoRawType, mkMemoizedEra)
import Cardano.Ledger.Plutus.Data (upgradeData)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxAuxData (ShelleyTxAuxData (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), ShelleyTxBodyRaw (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Cardano.Ledger.Slot (SlotNo)
import Control.Arrow (left)
import Control.Monad (unless, when)
import Data.Bifunctor (Bifunctor (first))
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import Lens.Micro

-- | Sometimes it is useful to specify that a type corresponds to a latest era that is
-- currently implemented
type LatestKnownEra = DijkstraEra

class
  ( EraTx era
  , EraTxOut era
  , EraTxBody era
  , EraTxAuxData era
  , EraTxWits era
  , EraScript era
  , EraPParams era
  , EraBlockBody era
  , EraTxCert era
  ) =>
  EraApi era
  where
  -- | Upgrade transactions from the previous era.
  -- This will preseve the values in corresponding Haskell structures where possible,
  -- however it will not preserve the binary representation for memoized types.
  -- If binary representation and validity of signatures, scriptIntegrityHash and
  --  auxDataHash must be retained, then the corresponding
  -- `binaryUpgradeTx`, `binaryUpgradeTxBody`, `binaryUpgradeTxAuxData` and `binaryUpgradeTxWits` functions
  -- should be used instead.
  -- Compare the two types of upgrade:
  -- - `upgrade[Tx, TxBody, TxAuxData, TxWits]` will use the Haskell representation, but will not
  --   preserve the serialised form. However, they will be suitable for iterated
  --   translation through eras.
  -- - `binaryUpgrade[Tx, TxBody, TxAuxData, TxWits]` will preserve the binary representation, but are
  --   not guaranteed to work through multiple eras - that is, the serialised
  --   representation from era n is guaranteed valid in era n + 1, but not
  --   necessarily in era n + 2.
  --
  -- See below an example of how the upgrade function can change the underlying serialization:
  --
  -- >>> import Cardano.Ledger.Api.Era (BabbageEra, ConwayEra)
  -- >>> import Test.QuickCheck
  -- >>> import Test.Cardano.Ledger.Babbage.Arbitrary ()
  -- >>> import Cardano.Ledger.Plutus.Data
  -- >>> import Cardano.Ledger.Alonzo.TxWits (unTxDats)
  -- >>> import Cardano.Ledger.MemoBytes (getMemoRawBytes)
  -- >>> import Cardano.Ledger.Binary (serialize)
  --
  -- Let's generate an arbitrary Babbage TxWits:
  -- >>> witsBabbage <- generate $ arbitrary @(TxWits BabbageEra)
  --
  -- Now we upgrade it to Conway using the upgrade functionality in this module:
  -- >>> witsUpgraded = upgradeTxWits witsBabbage
  --
  -- We can check that the values in the data structures have been preserved.
  -- For simplicity, let's check the txDats:
  -- let dats = Map.map unData . unTxDats . txdats
  -- in dats witsBabbage == dats witsUpgraded
  -- True
  --
  -- However, the upgraded value will have a different serialized binary representation than the original,
  -- because the serialization of TxWits changed from Babbage to Conway
  -- >>> getMemoRawBytes witsBabbage == getMemoRawBytes witsUpgraded
  -- False
  -- >>> serialize (eraProtVerHigh @BabbageEra) witsBabbage == serialize (eraProtVerHigh @BabbageEra) witsUpgraded
  -- False
  type TxUpgradeError era :: Type

  type TxUpgradeError era = Void

  type TxBodyUpgradeError era :: Type
  type TxBodyUpgradeError era = Void

  -- | Upgrade a transaction from the previous era.
  -- /Warning/ - This may not preserve the underlying binary representation.
  -- Use `binaryUpgradeTx` instead, if you need to preserve the serialised form.
  upgradeTx ::
    EraTx (PreviousEra era) =>
    Tx l (PreviousEra era) ->
    Either (TxUpgradeError era) (Tx l era)

  -- | Upgrade a transaction body from the previous era.
  -- /Warning/ - This may not preserve the underlying binary representation.
  -- Use `binaryUpgradeTxBody` instead, if you need to preserve the serialised form.
  upgradeTxBody ::
    EraTxBody (PreviousEra era) =>
    TxBody l (PreviousEra era) ->
    Either (TxBodyUpgradeError era) (TxBody l era)

  -- | Upgrade txAuxData from the previous era.
  -- /Warning/ - This may not preserve the underlying binary representation.
  -- Use `binaryUpgradeTxAuxData` instead, if you need to preserve the serialised form.
  upgradeTxAuxData :: EraTxAuxData (PreviousEra era) => TxAuxData (PreviousEra era) -> TxAuxData era

  -- | Upgrade txWits from the previous era.
  -- /Warning/ - This may not preserve the underlying binary representation.
  -- Use `binaryUpgradeTxWits` instead, if you need to preserve the serialised form.
  upgradeTxWits :: EraTxWits (PreviousEra era) => TxWits (PreviousEra era) -> TxWits era

  -- | Upgrade a native script from the previous era.
  upgradeNativeScript :: NativeScript (PreviousEra era) -> NativeScript era

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

  upgradeNativeScript =
    error
      "Calling this function will cause a compilation error, since there is no `NativeScript` in the ByronEra"

instance EraApi AllegraEra where
  upgradeTx (MkShelleyTx (ShelleyTx txb txwits txAux)) =
    fmap MkAllegraTx $
      ShelleyTx
        <$> upgradeTxBody txb
        <*> pure (upgradeTxWits txwits)
        <*> pure (fmap upgradeTxAuxData txAux)

  upgradeTxBody txBody =
    case getMemoRawType txBody of
      ShelleyTxBodyRaw {} -> do
        certs <- traverse upgradeTxCert (txBody ^. certsTxBodyL)
        pure . asSTxTopLevel $
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

  upgradeTxAuxData (ShelleyTxAuxData md) = AllegraTxAuxData md mempty

  upgradeTxWits stw =
    ShelleyTxWits
      (addrWits stw)
      (upgradeScript <$> scriptWits stw)
      (bootWits stw)

  upgradeNativeScript = upgradeMultiSig

ttlToValidityInterval :: SlotNo -> ValidityInterval
ttlToValidityInterval ttl = ValidityInterval SNothing (SJust ttl)

instance EraApi MaryEra where
  upgradeTx (MkAllegraTx (ShelleyTx txb txwits txAux)) =
    fmap MkMaryTx $
      ShelleyTx
        <$> upgradeTxBody txb
        <*> pure (upgradeTxWits txwits)
        <*> pure (fmap upgradeTxAuxData txAux)

  upgradeTxBody atb =
    case getMemoRawType atb of
      AllegraTxBodyRaw {} -> do
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

  upgradeNativeScript = translateTimelock

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

  upgradeTx (MkMaryTx (ShelleyTx body wits aux)) =
    fmap MkAlonzoTx $
      AlonzoTx
        <$> left ATUEBodyUpgradeError (upgradeTxBody body)
        <*> pure (upgradeTxWits wits)
        <*> pure (IsValid True)
        <*> pure (fmap upgradeTxAuxData aux)

  upgradeTxBody
    txb =
      case getMemoRawType txb of
        AllegraTxBodyRaw
          { atbrInputs
          , atbrOutputs
          , atbrCerts
          , atbrWithdrawals
          , atbrFee
          , atbrValidityInterval
          , atbrUpdate
          , atbrAuxDataHash
          , atbrMint
          } -> do
            certs <-
              traverse
                (left absurd . upgradeTxCert)
                atbrCerts

            updates <- traverse upgradeUpdateEither atbrUpdate
            pure $
              AlonzoTxBody
                { atbInputs = atbrInputs
                , atbOutputs = upgradeTxOut <$> atbrOutputs
                , atbCerts = certs
                , atbWithdrawals = atbrWithdrawals
                , atbTxFee = atbrFee
                , atbValidityInterval = atbrValidityInterval
                , atbUpdate = updates
                , atbAuxDataHash = atbrAuxDataHash
                , atbMint = atbrMint
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
    mkMemoizedEra @AllegraEra $
      AlonzoTxAuxDataRaw
        { atadrMetadata = md
        , atadrNativeScripts = upgradeNativeScript <$> scripts
        , atadrPlutus = mempty
        }

  upgradeTxWits (ShelleyTxWits {addrWits, scriptWits, bootWits}) =
    AlonzoTxWits addrWits bootWits (upgradeScript <$> scriptWits) mempty (Redeemers mempty)

  upgradeNativeScript = translateTimelock

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
  (AlonzoEraScript (PreviousEra era), AlonzoEraScript era, EraApi era) =>
  AlonzoTxAuxData (PreviousEra era) ->
  AlonzoTxAuxData era
translateAlonzoTxAuxData AlonzoTxAuxData {atadMetadata, atadNativeScripts, atadPlutus} =
  AlonzoTxAuxData
    { atadMetadata = atadMetadata
    , atadNativeScripts = upgradeNativeScript <$> atadNativeScripts
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

  upgradeTx (MkAlonzoTx (AlonzoTx b w valid aux)) =
    fmap MkBabbageTx $
      AlonzoTx
        <$> left BTUEBodyUpgradeError (upgradeTxBody b)
        <*> pure (upgradeTxWits w)
        <*> pure valid
        <*> pure (fmap upgradeTxAuxData aux)

  upgradeTxBody txBody =
    case getMemoRawType txBody of
      AlonzoTxBodyRaw {} -> do
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

  upgradeNativeScript = translateTimelock

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

  upgradeTx (MkBabbageTx (AlonzoTx b w valid aux)) =
    fmap MkConwayTx $
      AlonzoTx
        <$> upgradeTxBody b
        <*> pure (upgradeTxWits w)
        <*> pure valid
        <*> pure (fmap upgradeTxAuxData aux)

  upgradeTxBody btb =
    case getMemoRawType btb of
      BabbageTxBodyRaw {} -> do
        when (isSJust (btbUpdate btb)) $ Left CTBUEContainsUpdate
        certs <- traverse (left CTBUETxCert . upgradeTxCert) (btbCerts btb)
        let (duplicates, certsOSet) = OSet.fromStrictSeqDuplicates certs
        unless (null duplicates) $ Left $ CTBUEContainsDuplicateCerts duplicates
        pure $
          ConwayTxBody
            { ctbSpendInputs = btbInputs btb
            , ctbOutputs = unsafeMapSized upgradeTxOut <$> btbOutputs btb
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
            , ctbCollateralReturn = unsafeMapSized upgradeTxOut <$> btbCollateralReturn btb
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

  upgradeNativeScript = translateTimelock

newtype DijkstraTxBodyUpgradeError = DTBUETxCert DijkstraTxCertUpgradeError
  deriving (Eq, Show)

instance EraApi DijkstraEra where
  type TxUpgradeError DijkstraEra = TxBodyUpgradeError DijkstraEra
  type TxBodyUpgradeError DijkstraEra = DijkstraTxBodyUpgradeError
  upgradeTx (MkConwayTx (AlonzoTx b w valid aux)) =
    fmap MkDijkstraTx $
      DijkstraTx
        <$> upgradeTxBody b
        <*> pure (upgradeTxWits w)
        <*> pure valid
        <*> pure (fmap upgradeTxAuxData aux)

  upgradeTxBody txBody =
    case getMemoRawType txBody of
      ConwayTxBodyRaw {..} -> do
        certs <- traverse (left DTBUETxCert . upgradeTxCert) $ OSet.toStrictSeq ctbrCerts
        pure $
          DijkstraTxBody
            { dtbSpendInputs = ctbrSpendInputs
            , dtbOutputs = unsafeMapSized upgradeTxOut <$> ctbrOutputs
            , dtbCerts = OSet.fromStrictSeq certs
            , dtbWithdrawals = ctbrWithdrawals
            , dtbTxfee = ctbrFee
            , dtbVldt = ctbrVldt
            , dtbAdHash = ctbrAuxDataHash
            , dtbMint = ctbrMint
            , dtbCollateralInputs = ctbrCollateralInputs
            , dtbGuards = OSet.fromSet $ Set.map (KeyHashObj . coerceKeyRole) ctbrReqSignerHashes
            , dtbScriptIntegrityHash = ctbrScriptIntegrityHash
            , dtbTxNetworkId = ctbrNetworkId
            , dtbReferenceInputs = ctbrReferenceInputs
            , dtbCollateralReturn = unsafeMapSized upgradeTxOut <$> ctbrCollateralReturn
            , dtbTotalCollateral = ctbrTotalCollateral
            , dtbCurrentTreasuryValue = ctbrCurrentTreasuryValue
            , dtbProposalProcedures = OSet.mapL upgradeProposals ctbrProposalProcedures
            , dtbVotingProcedures = coerce ctbrVotingProcedures
            , dtbTreasuryDonation = ctbrTreasuryDonation
            , dtbSubTransactions = mempty
            }

  upgradeTxWits atw =
    AlonzoTxWits
      { txwitsVKey = txwitsVKey atw
      , txwitsBoot = txwitsBoot atw
      , txscripts = upgradeScript <$> txscripts atw
      , txdats = upgradeTxDats (txdats atw)
      , txrdmrs = upgradeRedeemers (txrdmrs atw)
      }

  upgradeTxAuxData = translateAlonzoTxAuxData

  upgradeNativeScript = upgradeTimelock
