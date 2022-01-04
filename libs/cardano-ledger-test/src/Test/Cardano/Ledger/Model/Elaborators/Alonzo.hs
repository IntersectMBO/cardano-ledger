{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Model.Elaborators.Alonzo where

import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Genesis as Alonzo (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Rules.Utxo
  ( UtxoPredicateFailure (..),
  )
import Cardano.Ledger.Alonzo.Rules.Utxow
  ( AlonzoPredFail (..),
  )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.Alonzo.Translation ()
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import Cardano.Ledger.Crypto (DSIGN, KES)
import Cardano.Ledger.Shelley.API.Genesis (initialState)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTxError (..))
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.Rules.Ledger
  ( LedgerPredicateFailure (..),
  )
import Cardano.Ledger.Shelley.Rules.Utxow
  ( UtxowPredicateFailure (..),
  )
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Protocol.TPraos.API (PraosCrypto)
import qualified Control.Monad.Trans.State as State hiding (state)
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import GHC.Records as GHC
import Test.Cardano.Ledger.Examples.TwoPhaseValidation (freeCostModel)
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelValue (..),
  )
import Test.Cardano.Ledger.Model.Elaborators
  ( ApplyBlockTransitionError (..),
    ElaborateEraModel (..),
    TxBodyArguments (..),
    TxWitnessArguments (..),
    lookupModelValue,
    noScriptAction,
  )
import Test.Cardano.Ledger.Model.Elaborators.Shelley
  ( elaborateShelleyPParams,
    fromShelleyGlobals,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet (..),
    IfSupportsMint (..),
    IfSupportsPlutus (..),
    IfSupportsScript (..),
    IfSupportsTimelock (..),
    ScriptFeatureTag (..),
    TyScriptFeature (..),
    TyValueExpected (..),
  )
import Test.Cardano.Ledger.Model.Rules (ModelPredicateFailure (..))
import Test.Cardano.Ledger.Model.Value (evalModelValue)

instance
  ( PraosCrypto crypto,
    KES.Signable (KES crypto) ~ SignableRepresentation,
    DSIGN.Signable (DSIGN crypto) ~ SignableRepresentation
  ) =>
  ElaborateEraModel (AlonzoEra crypto)
  where
  type EraFeatureSet (AlonzoEra crypto) = 'FeatureSet 'ExpectAnyOutput ('TyScriptFeature 'True 'True)

  toEraPredicateFailure = \case
    ModelValueNotConservedUTxO (ModelValue x) (ModelValue y) -> State.evalState $
      do
        x' <- evalModelValue (lookupModelValue noScriptAction) x
        y' <- evalModelValue (lookupModelValue noScriptAction) y
        pure $
          ApplyBlockTransitionError_Tx $
            ApplyTxError
              [ UtxowFailure
                  ( WrappedShelleyEraFailure
                      (UtxoFailure (ValueNotConservedUTxO x' y'))
                  )
              ]

  makeInitialState globals mpp genDelegs utxo0 = nes
    where
      pp = elaborateShelleyPParams mpp
      additionalGenesesConfig =
        AlonzoGenesis
          { coinsPerUTxOWord = GHC.getField @"_coinsPerUTxOWord" mpp,
            costmdls = Map.fromSet (const freeCostModel) $ Set.fromList [minBound ..],
            prices = GHC.getField @"_prices" mpp,
            maxTxExUnits = ExUnits 1_000 1_000,
            maxBlockExUnits = ExUnits 10_000 10_000,
            maxValSize = 100_000,
            collateralPercentage = 0,
            Alonzo.maxCollateralInputs = GHC.getField @"_maxCollateralInputs" mpp
          }
      sg = fromShelleyGlobals globals pp genDelegs utxo0
      nes = initialState sg additionalGenesesConfig

  makeTimelockScript _ (SupportsTimelock x) = Alonzo.TimelockScript x
  makePlutusScript _ (SupportsPlutus x) = x -- Alonzo.PlutusScript
  makeExtendedTxOut _ (Alonzo.TxOut a v _) (SupportsPlutus dh) = Alonzo.TxOut a v (SJust dh)

  makeTxBody nes (TxBodyArguments maxTTL fee ins outs dcerts wdrl (SupportsMint mint) (SupportsPlutus redeemers) (SupportsPlutus cins) (SupportsPlutus _)) =
    Alonzo.TxBody
      { Alonzo.inputs = ins,
        Alonzo.collateral = cins,
        Alonzo.outputs = outs,
        Alonzo.txcerts = dcerts,
        Alonzo.txwdrls = wdrl,
        Alonzo.txfee = fee,
        Alonzo.txvldt = ValidityInterval SNothing $ SJust (1 + maxTTL),
        Alonzo.txUpdates = SNothing,
        Alonzo.reqSignerHashes = Set.empty,
        Alonzo.mint = mint,
        Alonzo.scriptIntegrityHash =
          redeemers
            >>= uncurry
              ( Alonzo.hashScriptIntegrity
                  (LedgerState.esPp . LedgerState.nesEs $ nes)
                  (Set.singleton PlutusV1)
              ),
        Alonzo.adHash = SNothing,
        Alonzo.txnetworkid = SNothing -- SJust Testnet
      }

  makeTx _ realTxBody (TxWitnessArguments wits (SupportsScript ScriptFeatureTag_PlutusV1 scripts) (SupportsPlutus (rdmr, dats)) (SupportsPlutus isValid)) =
    let witSet =
          Alonzo.TxWitness
            { Alonzo.txwitsVKey = wits,
              Alonzo.txwitsBoot = Set.empty,
              Alonzo.txscripts = scripts,
              Alonzo.txdats = dats,
              Alonzo.txrdmrs = rdmr
            }
     in (Alonzo.ValidatedTx realTxBody witSet isValid SNothing)
