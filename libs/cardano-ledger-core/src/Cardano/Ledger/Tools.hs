{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This is a module that contains functionality that is not necessary for ledger
-- operation, but is useful for testing as well as for downstream users of ledger
module Cardano.Ledger.Tools (
  setMinFeeTx,
  calcMinFeeTx,
  estimateMinFeeTx,
  addDummyWitsTx,
)
where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN.Class (sizeSigDSIGN, sizeVerKeyDSIGN)
import Cardano.Ledger.Address (BootstrapAddress (..), bootstrapKeyHash)
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
  asWitness,
 )
import Cardano.Ledger.UTxO (EraUTxO (getWitsVKeyNeeded), UTxO (..))
import Data.Bits (shiftR)
import qualified Data.ByteString as BS
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy
import qualified Data.Set as Set
import Lens.Micro

-- | Calculate and update the fee in the transaction until it has the smallest possible
-- value according to the settings in the protocol parameters.
--
-- This function potentially changes the `feeTxBodyL` field of the `TxBody`, as such it
-- affects the hash of the body, which consequently invalidates all of the signature in
-- the attached witnesses.
setMinFeeTx :: EraTx era => PParams era -> Tx era -> Tx era
setMinFeeTx pp tx =
  let curMinFee = getMinFeeTx pp tx
      curFee = tx ^. bodyTxL . feeTxBodyL
      modifiedTx = tx & bodyTxL . feeTxBodyL .~ curMinFee
   in if curFee == curMinFee
        then tx
        else setMinFeeTx pp modifiedTx

calcMinFeeTx ::
  forall era.
  EraUTxO era =>
  -- | All relevant TxOuts for this transaction. In other words `TxIn`s produced by
  -- `allInputsTxBodyF` should be present in this `UTxO` map, however this is not
  -- checked.
  UTxO era ->
  -- | The current protocol parameters.
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | Number of extra KeyHash witnesses that will be supplied for satisfying native
  -- scripts. It is impossible to know how many of these is required without knowing the
  -- actual witnesses supplied and the time when the transaction will be
  -- submitted. Therefore we put this burden on the user.
  --
  -- This number can also be used to specify all of the redundant extra key witnesses that
  -- will be supplied.
  Int ->
  -- | The required minimum fee.
  Coin
calcMinFeeTx utxo pp tx extraKeyWitsCount =
  setMinFeeTx pp tx' ^. bodyTxL . feeTxBodyL
  where
    tx' = addDummyWitsTx pp tx numKeyWitsRequired $ Map.elems byronAttributes
    inputs = Set.toList $ tx ^. bodyTxL . spendableInputsTxBodyF
    getByronAttrs txIn = do
      txOut <- Map.lookup txIn $ unUTxO utxo
      ba@(BootstrapAddress bootAddr) <- txOut ^. bootAddrTxOutF
      pure (asWitness (bootstrapKeyHash ba), Byron.addrAttributes bootAddr)
    byronAttributes = Map.fromList $ mapMaybe getByronAttrs inputs
    requiredKeyHashes = getWitsVKeyNeeded def utxo (tx ^. bodyTxL)
    numKeyWitsRequired =
      extraKeyWitsCount + Set.size (requiredKeyHashes Set.\\ Map.keysSet byronAttributes)

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
      Byron.mkAttributes
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
  [Byron.Attributes Byron.AddrAttributes] ->
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
      VKey 'Witness (EraCrypto era) ->
      Byron.Attributes Byron.AddrAttributes ->
      BootstrapWitness (EraCrypto era)
    mkDummyByronKeyWit key =
      BootstrapWitness key dummySig chainCode . serialize' byronProtVer
    dummyByronKeyWits =
      Set.fromList $ zipWith mkDummyByronKeyWit dummyKeys byronAttrs
