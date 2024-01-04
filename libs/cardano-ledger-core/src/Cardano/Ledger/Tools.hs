{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This is a module that contains functionality that is not necessary for ledger
-- operation, but is usefuk for testing as well as for downstream users of ledger
module Cardano.Ledger.Tools (
  setMinFeeTx,
  estimateMinFeeTx,
  addDummyWitsTx,
)
where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN.Class (sizeSigDSIGN, sizeVerKeyDSIGN)
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (byronProtVer, decodeFull', serialize')
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  BootstrapWitness (..),
  ChainCode (..),
  KeyRole (Witness),
  VKey,
  WitVKey (..),
 )
import Data.Bits (shiftR)
import qualified Data.ByteString as BS
import Data.Proxy
import qualified Data.Set as Set
import Lens.Micro

-- | Calculate and update the fee in the transaction until it has the smallest possible
-- value according to the settings in the protocol parameters.
setMinFeeTx :: EraTx era => PParams era -> Tx era -> Tx era
setMinFeeTx pp tx =
  let curMinFee = getMinFeeTx pp tx
      curFee = tx ^. bodyTxL . feeTxBodyL
      modifiedTx = tx & bodyTxL . feeTxBodyL .~ curMinFee
   in if curFee == curMinFee
        then tx
        else setMinFeeTx pp modifiedTx

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
    tx' = addDummyWitsTx pp tx numKeyWits $ replicate numByronKeyWits dummyByronAttributes
    -- We assume testnet network magic here to avoid having to thread the actual network
    -- ID into this function merely to calculate the fees of byron witnesses more
    -- accurately. This will over-estimate min fees for byron witnesses in mainnet
    -- transaction by 7 bytes per witness.
    dummyByronAttributes =
      Byron.AddrAttributes
        { Byron.aaVKDerivationPath = Nothing
        , Byron.aaNetworkMagic = Byron.NetworkTestnet maxBound
        }

-- | Create dummy witnesses and add them to the transaction
addDummyWitsTx ::
  forall era.
  EraTx era =>
  -- | The current protocol parameters.
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | The number of key witnesses still to be added to the transaction.
  Int ->
  -- | List of attributes from TxOuts with Byron addresses that are being spent
  [Byron.AddrAttributes] ->
  -- | The required minimum fee.
  Tx era
addDummyWitsTx pp tx numKeyWits byronAttrs =
  tx
    & (witsTxL . addrTxWitsL <>~ dummyKeyWits)
    & (witsTxL . bootAddrTxWitsL <>~ dummyByronKeyWits)
  where
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

    mkDummyByronKeyWit ::
      VKey 'Witness (EraCrypto era) -> Byron.AddrAttributes -> BootstrapWitness (EraCrypto era)
    mkDummyByronKeyWit key =
      BootstrapWitness key dummySig chainCode . serialize' byronProtVer . Byron.mkAttributes
    dummyByronKeyWits =
      Set.fromList $ zipWith mkDummyByronKeyWit dummyKeys byronAttrs
