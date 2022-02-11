{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Abstracting over operations (especially involving PParams) over all concrete eras.
--   These are the operations we need to generate arbitrary transactions in every era.
module Test.Cardano.Ledger.Generic.Parametric where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.Scripts (CostModel (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Tx
  ( ScriptIntegrityHash,
    ValidatedTx (..),
    hashScriptIntegrity,
    minfee,
  )
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.Babbage.PParams as Babbage (PParams, PParams' (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.EpochBoundary (obligation)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley (minfee)
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams, PParams' (..))
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.UnifiedMap (ViewMap)
import Data.Default.Class (Default (..))
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import Numeric.Natural (Natural)

-- ================================================================

data Change
  = MaxCollateralInputs Natural
  | MaxTxExUnits ExUnits
  | CollateralPercentage Natural
  | MinfeeA Natural
  | MinfeeB Natural
  | Costmdls (Map Language CostModel)
  | MaxValSize Natural
  | MaxTxSize Natural
  | PoolDeposit Coin
  | KeyDeposit Coin

class (Default (Core.PParams era)) => Parametric era where
  maxCollateralInputs' :: Core.PParams era -> Natural
  maxTxExUnits' :: Core.PParams era -> ExUnits
  collateralPercentage' :: Core.PParams era -> Natural
  minfeeA' :: Core.PParams era -> Natural
  minfeeB' :: Core.PParams era -> Natural
  costmdls' :: Core.PParams era -> Map Language CostModel
  maxValSize' :: Core.PParams era -> Natural
  maxTxSize' :: Core.PParams era -> Natural
  keyDeposit' :: Core.PParams era -> Coin
  poolDeposit' :: Core.PParams era -> Coin

  makeChange :: Core.PParams era -> Change -> Core.PParams era
  obligation' ::
    Core.PParams era ->
    ViewMap (Crypto era) (Credential 'Staking (Crypto era)) Coin ->
    Map (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era)) ->
    Coin
  minfee' :: Core.PParams era -> Core.Tx era -> Coin
  hashScriptIntegrity' ::
    Core.PParams era ->
    Set Language ->
    Redeemers era ->
    TxDats era -> -- (Map.Map (DataHash c) (Data era))
    StrictMaybe (ScriptIntegrityHash (Crypto era))
  hashScriptIntegrity' _pp _l _r _d = SNothing

makeChanges :: Parametric era => [Change] -> Core.PParams era -> Core.PParams era
makeChanges cs pp = foldl' makeChange pp cs

-- ====================================

instance CC.Crypto c => Parametric (AlonzoEra c) where
  maxCollateralInputs' x = _maxCollateralInputs x
  maxTxExUnits' x = _maxTxExUnits x
  collateralPercentage' x = _collateralPercentage x
  minfeeA' x = _minfeeA x
  minfeeB' x = _minfeeB x
  costmdls' x = _costmdls x
  maxValSize' x = _maxValSize x
  maxTxSize' x = _maxTxSize x
  poolDeposit' x = _poolDeposit x
  keyDeposit' x = _keyDeposit x

  makeChange x (MaxCollateralInputs n) = x {_maxCollateralInputs = n}
  makeChange x (MaxTxExUnits n) = x {_maxTxExUnits = n}
  makeChange x (CollateralPercentage n) = x {_collateralPercentage = n}
  makeChange x (MinfeeA n) = x {_minfeeA = n}
  makeChange x (MinfeeB n) = x {_minfeeB = n}
  makeChange x (Costmdls n) = x {_costmdls = n}
  makeChange x (MaxValSize n) = x {_maxValSize = n}
  makeChange x (MaxTxSize n) = x {_maxTxSize = n}
  makeChange x (PoolDeposit n) = x {_poolDeposit = n}
  makeChange x (KeyDeposit n) = x {_keyDeposit = n}

  obligation' = obligation @c @(PParams (AlonzoEra c)) @(ViewMap c)
  minfee' = minfee @(AlonzoEra c) @(ValidatedTx)
  hashScriptIntegrity' = hashScriptIntegrity

-- ================================================================

instance CC.Crypto c => Parametric (BabbageEra c) where
  maxCollateralInputs' x = Babbage._maxCollateralInputs x
  maxTxExUnits' x = Babbage._maxTxExUnits x
  collateralPercentage' x = Babbage._collateralPercentage x
  minfeeA' x = Babbage._minfeeA x
  minfeeB' x = Babbage._minfeeB x
  costmdls' x = Babbage._costmdls x
  maxValSize' x = Babbage._maxValSize x
  maxTxSize' x = Babbage._maxTxSize x
  poolDeposit' x = Babbage._poolDeposit x
  keyDeposit' x = Babbage._keyDeposit x

  makeChange x (MaxCollateralInputs n) = x {Babbage._maxCollateralInputs = n}
  makeChange x (MaxTxExUnits n) = x {Babbage._maxTxExUnits = n}
  makeChange x (CollateralPercentage n) = x {Babbage._collateralPercentage = n}
  makeChange x (MinfeeA n) = x {Babbage._minfeeA = n}
  makeChange x (MinfeeB n) = x {Babbage._minfeeB = n}
  makeChange x (Costmdls n) = x {Babbage._costmdls = n}
  makeChange x (MaxValSize n) = x {Babbage._maxValSize = n}
  makeChange x (MaxTxSize n) = x {Babbage._maxTxSize = n}
  makeChange x (PoolDeposit n) = x {Babbage._poolDeposit = n}
  makeChange x (KeyDeposit n) = x {Babbage._keyDeposit = n}

  obligation' = obligation @c @(Babbage.PParams (BabbageEra c)) @(ViewMap c)
  minfee' = minfee @(BabbageEra c) @(ValidatedTx)
  hashScriptIntegrity' = hashScriptIntegrity

-- ================================================================

makeChangeS :: Shelley.PParams era -> Change -> Shelley.PParams era
makeChangeS x (MaxCollateralInputs _) = x
makeChangeS x (MaxTxExUnits _) = x
makeChangeS x (CollateralPercentage _) = x
makeChangeS x (MinfeeA n) = x {Shelley._minfeeA = n}
makeChangeS x (MinfeeB n) = x {Shelley._minfeeB = n}
makeChangeS x (Costmdls _) = x
makeChangeS x (MaxValSize _) = x
makeChangeS x (MaxTxSize n) = x {Shelley._maxTxSize = n}
makeChangeS x (PoolDeposit n) = x {Shelley._poolDeposit = n}
makeChangeS x (KeyDeposit n) = x {Shelley._keyDeposit = n}

instance CC.Crypto c => Parametric (ShelleyEra c) where
  maxCollateralInputs' _x = 0
  maxTxExUnits' _x = mempty
  collateralPercentage' _x = 0
  minfeeA' x = Shelley._minfeeA x
  minfeeB' x = Shelley._minfeeB x
  costmdls' _x = Map.empty
  maxValSize' _x = 0
  maxTxSize' x = Shelley._maxTxSize x
  poolDeposit' x = Shelley._poolDeposit x
  keyDeposit' x = Shelley._keyDeposit x

  makeChange = makeChangeS
  obligation' = obligation @c @(Shelley.PParams (ShelleyEra c)) @(ViewMap c)
  minfee' = Shelley.minfee
  hashScriptIntegrity' _pp _l _r _d = SNothing

-- ================================================================

instance CC.Crypto c => Parametric (MaryEra c) where
  maxCollateralInputs' _x = 0
  maxTxExUnits' _x = mempty
  collateralPercentage' _x = 0
  minfeeA' x = Shelley._minfeeA x
  minfeeB' x = Shelley._minfeeB x
  costmdls' _x = Map.empty
  maxValSize' _x = 0
  maxTxSize' x = Shelley._maxTxSize x
  poolDeposit' x = Shelley._poolDeposit x
  keyDeposit' x = Shelley._keyDeposit x

  makeChange = makeChangeS
  obligation' = obligation @c @(Shelley.PParams (MaryEra c)) @(ViewMap c)
  minfee' = Shelley.minfee
  hashScriptIntegrity' _pp _l _r _d = SNothing

-- ================================================================

instance CC.Crypto c => Parametric (AllegraEra c) where
  maxCollateralInputs' _x = 0
  maxTxExUnits' _x = mempty
  collateralPercentage' _x = 0
  minfeeA' x = Shelley._minfeeA x
  minfeeB' x = Shelley._minfeeB x
  costmdls' _x = Map.empty
  maxValSize' _x = 0
  maxTxSize' x = Shelley._maxTxSize x
  poolDeposit' x = Shelley._poolDeposit x
  keyDeposit' x = Shelley._keyDeposit x

  makeChange = makeChangeS
  obligation' = obligation @c @(Shelley.PParams (AllegraEra c)) @(ViewMap c)
  minfee' = Shelley.minfee
  hashScriptIntegrity' _pp _l _r _d = SNothing
