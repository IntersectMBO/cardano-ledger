{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Transaction building and inspecting relies heavily on lenses (`microlens`). Therefore, some
-- familiarity with those is necessary. However, you can probably go a long way by simply
-- looking at the examples and try to go from there.
--
-- Let's start by defining the GHC extensions and imports.
--
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> import qualified Data.Sequence.Strict as StrictSeq
-- >>> import Cardano.Ledger.Api.Era (Babbage)
-- >>> import Lens.Micro
-- >>> import Test.Cardano.Ledger.Babbage.Arbitrary ()
--
-- Here's an example on how to build a Babbage era unbalanced transaction containing a single
-- transaction output using the provided interface.
--
-- >>> :{
-- quickCheck $ \(txOut :: TxOut Babbage) ->
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

  -- * Shelley onwards
  EraTx (Tx),
  mkBasicTx,
  bodyTxL,
  witsTxL,
  auxDataTxL,
  sizeTxF,
  getMinFeeTx,
  setMinFeeTx,
  estimateMinFeeTx,

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
)
where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN.Class (sizeSigDSIGN, sizeVerKeyDSIGN)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..))
import Cardano.Ledger.Api.Era ()
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
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (byronProtVer, decodeFull', serialize')
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (EraCrypto, EraTx (..), PParams, ppProtocolVersionL, setMinFeeTx)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (BootstrapWitness (..), ChainCode (..))
import Data.Bits (shiftR)
import qualified Data.ByteString as BS
import Data.Proxy
import qualified Data.Set as Set
import Lens.Micro

-- | Estimate a minimum transaction fee for a transaction that does not yet have all of
-- the `VKey` witnesses. This calculation is not very accurate in estimating Byron
-- witnesses, but it should work for the most part.
--
-- @since 1.8.0
estimateMinFeeTx ::
  forall era.
  EraTx era =>
  -- | The current protocol parameters.
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | The number of key witnesses still to be added to the transaction.
  Int ->
  -- | The number of Byron key witnesses still to be added to the transaction.
  Int ->
  -- | The required minimum fee.
  Coin
estimateMinFeeTx pp tx numKeyWits numByronKeyWits = setMinFeeTx pp tx' ^. bodyTxL . feeTxBodyL
  where
    tx' =
      tx
        & (witsTxL . addrTxWitsL <>~ dummyKeyWits)
        & (witsTxL . bootAddrTxWitsL <>~ dummyByronKeyWits)

    dsign :: Proxy (DSIGN (EraCrypto era))
    dsign = Proxy
    version = pvMajor (pp ^. ppProtocolVersionL)
    -- We need to make sure that dummies are unique, since they'll be placed into a Set
    mkDummy name n =
      either
        (\err -> error ("Corrupt Dummy " ++ name ++ ": " ++ show err))
        id
        . decodeFull' version
        . serialize' version
        . fst
        . BS.unfoldrN n (\b -> Just (fromIntegral b, b `shiftR` 8))
    vKeySize = fromIntegral $ sizeVerKeyDSIGN dsign
    dummyKeys = map (mkDummy "VKey" vKeySize) [0 :: Int ..]

    sigSize = fromIntegral $ sizeSigDSIGN dsign
    dummySig = mkDummy "Signature" sigSize (0 :: Int)
    dummyKeyWits =
      Set.fromList [WitVKey key dummySig | key <- take numKeyWits dummyKeys]

    -- ChainCode is always 32 bytes long.
    chainCode = ChainCode $ BS.replicate 32 0
    -- We assume testnet network magic here to avoid having to thread the actual network
    -- ID into this function merely to calculate the fees of byron witnesses more
    -- accurately. This will over-estimate min fees for byron witnesses in mainnet
    -- transaction by 7 bytes per witness.
    dummyByronAttributes =
      serialize' byronProtVer $
        Byron.mkAttributes
          Byron.AddrAttributes
            { Byron.aaVKDerivationPath = Nothing
            , Byron.aaNetworkMagic = Byron.NetworkTestnet maxBound
            }
    mkDummyByronKeyWit key = BootstrapWitness key dummySig chainCode dummyByronAttributes
    dummyByronKeyWits =
      Set.fromList [mkDummyByronKeyWit key | key <- take numByronKeyWits dummyKeys]
