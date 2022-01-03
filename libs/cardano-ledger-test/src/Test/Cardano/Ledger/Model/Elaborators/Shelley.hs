{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Model.Elaborators.Shelley where

import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.BaseTypes (Globals (..), activeSlotVal, epochInfo)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto (DSIGN, KES)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Keys (GenDelegPair, KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.Genesis (initialState)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTxError (..))
import Cardano.Ledger.Shelley.Genesis
  ( ShelleyGenesis (..),
    emptyGenesisStaking,
  )
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import Cardano.Ledger.Shelley.Rules.EraMapping ()
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (..))
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoPredicateFailure (..))
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure (..))
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import Cardano.Protocol.TPraos.API (PraosCrypto)
import Cardano.Slotting.EpochInfo.API (epochInfoSize)
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Control.Monad.Trans.State as State hiding (state)
import Data.Functor.Identity (Identity (..))
import Data.Group (Group (..))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Void (absurd)
import qualified GHC.Records as GHC
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelValue (..),
  )
import Test.Cardano.Ledger.Model.Elaborators
  ( ApplyBlockTransitionError (..),
    ElaborateEraModel (..),
    EraFeatureSet,
    ExpectedValueTypeC (..),
    TxBodyArguments (..),
    TxWitnessArguments (..),
    lookupModelValue,
    noScriptAction,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet (..),
    FeatureSupport (..),
    IfSupportsMint (..),
    IfSupportsPlutus (..),
    IfSupportsScript (..),
    IfSupportsTimelock (..),
    KnownRequiredFeatures,
    TyScriptFeature (..),
    TyValueExpected (..),
  )
import Test.Cardano.Ledger.Model.PParams (ModelPParams, ModelPParamsF (..))
import Test.Cardano.Ledger.Model.Rules (ModelPredicateFailure (..))
import Test.Cardano.Ledger.Model.Value
  ( evalModelValue,
  )

instance
  ( PraosCrypto crypto,
    KES.Signable (KES crypto) ~ SignableRepresentation,
    DSIGN.Signable (DSIGN crypto) ~ SignableRepresentation
  ) =>
  ElaborateEraModel (ShelleyEra crypto)
  where
  type EraFeatureSet (ShelleyEra crypto) = 'FeatureSet 'ExpectAdaOnly ('TyScriptFeature 'False 'False)

  reifyValueConstraint = ExpectedValueTypeC_Simple

  makeInitialState globals mpp genDelegs utxo0 = nes
    where
      pp = elaborateShelleyPParams mpp
      additionalGenesesConfig = ()
      sg = fromShelleyGlobals globals pp genDelegs utxo0
      nes = initialState sg additionalGenesesConfig

  makeTimelockScript _ (NoTimelockSupport x) = absurd x
  makePlutusScript _ (NoPlutusSupport x) = absurd x
  makeExtendedTxOut _ _ (NoPlutusSupport x) = absurd x

  toEraPredicateFailure = \case
    ModelValueNotConservedUTxO (ModelValue x) (ModelValue y) -> State.evalState $
      do
        x' <- evalModelValue (lookupModelValue noScriptAction) x
        y' <- evalModelValue (lookupModelValue noScriptAction) y
        pure $
          ApplyBlockTransitionError_Tx $
            ApplyTxError
              [UtxowFailure (UtxoFailure (ValueNotConservedUTxO x' y'))]

  makeTxBody _ (TxBodyArguments maxTTL fee ins outs dcerts wdrl (NoMintSupport ()) (NoPlutusSupport ()) (NoPlutusSupport ()) (NoPlutusSupport ())) =
    Shelley.TxBody
      { Shelley._inputs = ins,
        Shelley._outputs = outs,
        Shelley._certs = dcerts,
        Shelley._wdrls = wdrl,
        Shelley._txfee = fee,
        Shelley._ttl = maxTTL,
        Shelley._txUpdate = SNothing,
        Shelley._mdHash = SNothing
      }

  makeTx _ realTxBody (TxWitnessArguments wits (NoScriptSupport ()) (NoPlutusSupport ()) (NoPlutusSupport ())) =
    Shelley.Tx realTxBody (mempty {Shelley.addrWits = wits}) SNothing

fromShelleyGlobals ::
  Globals ->
  Shelley.PParams era ->
  Map.Map (KeyHash 'Genesis (Crypto era)) (GenDelegPair (Crypto era)) ->
  Map.Map (Addr (Crypto era)) Coin ->
  ShelleyGenesis era
fromShelleyGlobals globals pp genDelegs initialFunds =
  ShelleyGenesis
    { sgSystemStart = (posixSecondsToUTCTime 0),
      sgNetworkMagic = 1, -- genNetworkMagic
      sgNetworkId = networkId globals,
      sgActiveSlotsCoeff = activeSlotVal $ activeSlotCoeff globals,
      sgSecurityParam = securityParameter globals,
      sgEpochLength = runIdentity $ flip epochInfoSize (EpochNo 1) $ epochInfo globals,
      sgSlotsPerKESPeriod = slotsPerKESPeriod globals,
      sgMaxKESEvolutions = maxKESEvo globals,
      sgSlotLength = (secondsToNominalDiffTime 1),
      sgUpdateQuorum = quorum globals,
      sgMaxLovelaceSupply = maxLovelaceSupply globals,
      sgProtocolParams = pp,
      sgGenDelegs = genDelegs, --  genGenesisDelegationList
      sgInitialFunds = initialFunds, -- genFundsList
      sgStaking = emptyGenesisStaking -- genStaking
    }

elaborateShelleyPParams ::
  KnownRequiredFeatures (EraFeatureSet era) =>
  ModelPParams (EraFeatureSet era) ->
  Shelley.PParams era
elaborateShelleyPParams mpp =
  let minUTxOValue =
        bifoldMapSupportsFeature id (`pow` (29 :: Integer)) $ runIdentity $ _modelPParams_coinsPerUTxOWord mpp
   in Shelley.emptyPParams
        { Shelley._nOpt = GHC.getField @"_nOpt" mpp,
          Shelley._a0 = GHC.getField @"_a0" mpp,
          Shelley._rho = GHC.getField @"_rho" mpp,
          Shelley._tau = GHC.getField @"_tau" mpp,
          Shelley._d = GHC.getField @"_d" mpp,
          Shelley._minfeeA = GHC.getField @"_minfeeA" mpp,
          Shelley._minfeeB = GHC.getField @"_minfeeB" mpp,
          -- , Shelley._maxBBSize = GHC.getField @"_maxBBSize" mpp
          Shelley._maxTxSize = GHC.getField @"_maxTxSize" mpp,
          -- , Shelley._maxBHSize = GHC.getField @"_maxBHSize" mpp
          Shelley._keyDeposit = GHC.getField @"_keyDeposit" mpp,
          Shelley._poolDeposit = GHC.getField @"_poolDeposit" mpp,
          -- , Shelley._eMax = GHC.getField @"_eMax" mpp
          -- , Shelley._extraEntropy = GHC.getField @"_extraEntropy" mpp
          Shelley._protocolVersion = GHC.getField @"_protocolVersion" mpp,
          Shelley._minUTxOValue = minUTxOValue,
          Shelley._minPoolCost = GHC.getField @"_minPoolCost" mpp
        }
