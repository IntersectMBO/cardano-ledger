{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fmax-relevant-binds=0 #-}

module Test.Cardano.Ledger.Elaborators.Alonzo where

import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Utxo
import Cardano.Ledger.Alonzo.Rules.Utxow
import Cardano.Ledger.Alonzo.Scripts (Prices(..), ExUnits(..), CostModel(..))
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis(..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto (KES, DSIGN)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval(..))
import Data.Default.Class
import Data.Maybe.Strict (StrictMaybe(..))
import Shelley.Spec.Ledger.API.Mempool (ApplyTxError(..))
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.STS.Ledger
import Shelley.Spec.Ledger.STS.Utxow
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.KES.Class as KES
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.Val as Val
import qualified Data.Map as Map
import qualified Data.Set as Set
import Cardano.Ledger.Alonzo.Translation ()

import Test.Cardano.Ledger.ModelChain

instance Default AlonzoGenesis where
  def = AlonzoGenesis
    { adaPerUTxOWord = Coin 10000
    , costmdls = Map.fromSet (const $ CostModel Map.empty) $ Set.fromList [minBound..]
    , prices = Prices (Coin 1000) (Coin 1000)
    , maxTxExUnits = ExUnits 100 100
    , maxBlockExUnits = ExUnits 100 100
    , maxValSize = 100000
    , collateralPercentage = 100
    , maxCollateralInputs = 100
    }

instance
    ( PraosCrypto crypto
    , KES.Signable (KES crypto) ~ SignableRepresentation
    , DSIGN.Signable (DSIGN crypto) ~ SignableRepresentation
    ) => ElaborateEraModel (AlonzoEra crypto) where

  toEraPredicateFailure = \case
    ModelValueNotConservedUTxO x y -> ApplyBlockTransitionError_Tx $ ApplyTxError
      [UtxowFailure (WrappedShelleyEraFailure
        (UtxoFailure (ValueNotConservedUTxO (Val.inject $ Coin $ unModelValue x) (Val.inject $ Coin $ unModelValue y)))
        )]

  makeTxBody _ maxTTL fee ins outs dcerts wdrl = Alonzo.TxBody
    { Alonzo.inputs = ins
    , Alonzo.collateral = ins
    , Alonzo.outputs = outs
    , Alonzo.txcerts = dcerts
    , Alonzo.txwdrls = wdrl
    , Alonzo.txfee = fee
    , Alonzo.txvldt = ValidityInterval SNothing $ SJust (1+maxTTL)
    , Alonzo.txUpdates = SNothing
    , Alonzo.reqSignerHashes = Set.empty
    , Alonzo.mint = Val.zero
    , Alonzo.wppHash = SNothing
    , Alonzo.adHash = SNothing
    , Alonzo.txnetworkid = SNothing -- SJust Testnet
    }

  makeTx _ realTxBody wits =
    let
      witSet = Alonzo.TxWitness
        { Alonzo.txwitsVKey = wits
        , Alonzo.txwitsBoot = Set.empty
        , Alonzo.txscripts = Map.empty
        , Alonzo.txdats = Map.empty
        , Alonzo.txrdmrs = Alonzo.Redeemers Map.empty
        }
    in (Alonzo.ValidatedTx realTxBody witSet (Alonzo.IsValidating True) SNothing)

