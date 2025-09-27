{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Transaction building and inspecting relies heavily on lenses (`microlens`). Therefore, some
-- familiarity with those is necessary. However, you can probably go a long way by simply
-- looking at the examples and try to go from there.
--
-- Let's start by defining the GHC extensions and imports.
--
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> import qualified Data.Sequence.Strict as StrictSeq
-- >>> import Cardano.Ledger.Api.Era (BabbageEra)
-- >>> import Lens.Micro
-- >>> import Test.Cardano.Ledger.Babbage.Arbitrary ()
--
-- Here's an example on how to build a Babbage era unbalanced transaction containing a single
-- transaction output using the provided interface.
--
-- >>> :{
-- quickCheck $ \(txOut :: TxOut BabbageEra) ->
--     let
--         -- Defining a Babbage era transaction body with a single random transaction output
--         txBody = mkBasicTxBody
--                & outputsTxBodyL <>~ StrictSeq.singleton txOut
--         -- Defining a basic transaction with our transaction body
--         tx = mkBasicTx txBody
--      in
--         -- We verify that the transaction's outputs contains our single random output
--         tx ^. bodyTxL . outputsTxBodyL == StrictSeq.singleton txOut
-- :}
-- +++ OK, passed 100 tests.
module Cardano.Ledger.Api.Tx (
  -- | Building and inspecting transaction bodies
  module Cardano.Ledger.Api.Tx.Body,
  module Cardano.Ledger.Api.Tx.Cert,
  module Cardano.Ledger.Api.Tx.AuxData,
  module Cardano.Ledger.Api.Tx.Wits,

  -- * Any era
  AnyEraTx (isValidTxG),
  producedTxOuts,

  -- * Shelley onwards
  EraTx (Tx),
  mkBasicTx,
  bodyTxL,
  witsTxL,
  auxDataTxL,
  sizeTxF,
  getMinFeeTx,
  setMinFeeTx,
  setMinFeeTxUtxo,
  calcMinFeeTx,
  estimateMinFeeTx,
  txIdTx,

  -- * Alonzo onwards
  AlonzoEraTx,
  isValidTxL,
  IsValid (..),

  -- ** Execution units
  evalTxExUnits,
  RedeemerReport,
  evalTxExUnitsWithLogs,
  RedeemerReportWithLogs,
  TransactionScriptFailure (..),

  -- * Upgrade
  binaryUpgradeTx,
  upgradeTx,
) where

import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..))
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Scripts.ExUnits (
  RedeemerReport,
  RedeemerReportWithLogs,
  TransactionScriptFailure (..),
  evalTxExUnits,
  evalTxExUnitsWithLogs,
 )
import Cardano.Ledger.Api.Tx.AuxData
import Cardano.Ledger.Api.Tx.Body
import Cardano.Ledger.Api.Tx.Cert
import Cardano.Ledger.Api.Tx.Wits
import Cardano.Ledger.Babbage.Collateral (mkCollateralTxIn)
import Cardano.Ledger.Core (EraTx (..), binaryUpgradeTx, txIdTx)
import Cardano.Ledger.State (UTxO (..), txouts)
import Cardano.Ledger.Tools (calcMinFeeTx, estimateMinFeeTx, setMinFeeTx, setMinFeeTxUtxo)
import Control.Monad (join)
import qualified Data.Map as Map
import Lens.Micro

class (EraTx era, AnyEraTxBody era, AnyEraTxWits era, AnyEraTxAuxData era) => AnyEraTx era where
  isValidTxG :: SimpleGetter (Tx era) (Maybe IsValid)
  default isValidTxG :: AlonzoEraTx era => SimpleGetter (Tx era) (Maybe IsValid)
  isValidTxG = isValidTxL . to Just

instance AnyEraTx ShelleyEra where
  isValidTxG = to (const Nothing)

instance AnyEraTx AllegraEra where
  isValidTxG = to (const Nothing)

instance AnyEraTx MaryEra where
  isValidTxG = to (const Nothing)

instance AnyEraTx AlonzoEra

instance AnyEraTx BabbageEra

instance AnyEraTx ConwayEra

instance AnyEraTx DijkstraEra

-- | Construct all of the unspent outputs that will be produced by this transaction
producedTxOuts :: AnyEraTx era => Tx era -> UTxO era
producedTxOuts tx =
  case tx ^. isValidTxG of
    Just (IsValid False) ->
      UTxO $
        case join (txBody ^. collateralReturnTxBodyG) of
          Nothing -> mempty
          Just txOut -> Map.singleton (mkCollateralTxIn txBody) txOut
    _ -> txouts txBody
  where
    txBody = tx ^. bodyTxL
