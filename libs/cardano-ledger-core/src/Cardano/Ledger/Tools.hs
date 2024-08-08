{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a module that contains functionality that is not necessary for ledger
-- operation, but is useful for testing as well as for downstream users of ledger
module Cardano.Ledger.Tools (
  -- * Tx
  setMinFeeTx,
  setMinFeeTxUtxo,
  calcMinFeeTx,
  calcMinFeeTxNativeScriptWits,
  estimateMinFeeTx,
  addDummyWitsTx,

  -- * TxOut
  setMinCoinTxOut,
  ensureMinCoinTxOut,
  setMinCoinTxOutWith,

  -- * General tools
  boom,
  integralToByteStringN,
  byteStringToNum,
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
  KeyHash,
  KeyRole (Witness),
  VKey,
  WitVKey (..),
  asWitness,
 )
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..))
import Data.Bits (Bits (..), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Lens.Micro

-- | Calculate and update the fee in the transaction until it has the smallest possible
-- value according to the settings in the protocol parameters.
--
-- This function potentially changes the `feeTxBodyL` field of the `TxBody`, as such it
-- affects the hash of the body, which consequently invalidates all of the signature in
-- the attached witnesses.
setMinFeeTx ::
  EraTx era =>
  PParams era ->
  Tx era ->
  -- | Size in bytes of reference scripts present in this transaction
  Int ->
  Tx era
setMinFeeTx pp tx refScriptsSize =
  setMinFeeTxInternal (\t -> getMinFeeTx pp t refScriptsSize) tx

setMinFeeTxUtxo :: EraUTxO era => PParams era -> Tx era -> UTxO era -> Tx era
setMinFeeTxUtxo pp tx utxo =
  setMinFeeTxInternal (\t -> getMinFeeTxUtxo pp t utxo) tx

-- | Similar to `setMinCoinTxOut` it will guarantee that the minimum requirement for the
-- output amount is satisified, however it makes it possible to set a higher amount than
-- the minimaly required.
--
-- @
-- > ensureMinCoinTxOut pp (txOut & coinTxOutL .~ zero) == setMinCoinTxOut pp (txOut & coinTxOutL .~ zero)
-- > (ensureMinCoinTxOut pp txOut ^. coinTxOutL) >= (setMinCoinTxOut pp txOut ^. coinTxOutL)
-- @
ensureMinCoinTxOut :: EraTxOut era => PParams era -> TxOut era -> TxOut era
ensureMinCoinTxOut = setMinCoinTxOutWith (>=)

setMinFeeTxInternal ::
  EraTx era =>
  (Tx era -> Coin) ->
  Tx era ->
  Tx era
setMinFeeTxInternal f tx =
  let curMinFee = f tx
      curFee = tx ^. bodyTxL . feeTxBodyL
      modifiedTx = tx & bodyTxL . feeTxBodyL .~ curMinFee
   in if curFee == curMinFee
        then tx
        else setMinFeeTxInternal f modifiedTx

-- | Same as `calcMinFeeTx`, except this function allows to specify hashes of key witnesses
-- that will be supplied, instead of their count. That is only useful whenever there is a
-- chance of some of the required witnesses being the same as the witnesses that will be
-- supplied for native scripts.
calcMinFeeTxNativeScriptWits ::
  forall era.
  EraUTxO era =>
  -- | All TxOuts available for this transaction. In other words `TxIn`s produced by
  -- `allInputsTxBodyF` should be present in this `UTxO` map, however this is not checked.
  UTxO era ->
  -- | The current protocol parameters.
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | KeyHash witnesses that will be supplied for satisfying native scripts. It is
  -- impossible to know how many of these is required without knowing the actual witnesses
  -- supplied and the time when the transaction will be submitted. Therefore we put this
  -- burden on the user.
  Set.Set (KeyHash 'Witness (EraCrypto era)) ->
  -- | The required minimum fee.
  Coin
calcMinFeeTxNativeScriptWits utxo pp tx nativeScriptsKeyWitsHashes =
  calcMinFeeTxInternal utxo pp tx (Set.size nativeScriptsKeyWitsHashes) nativeScriptsKeyWitsHashes

-- | This is a more accurate version `estimateMinFeeTx` that looks into transaction and
-- figures out how many and what kind of key witnesses this transaction needs. It requires
-- access to the portion of the `UTxO` that is relevant for this transaction. The only
-- type of witnesses that it cannot figure out reliably is the witnesses needed for
-- satisfying native scripts included in the transaction. For this reason number of
-- witnesses needed for native scripts must be supplied as an extra argument.
--
-- ====__Example__
--
-- >>> let relevantUtxo = txInsFilter utxo (tx ^. bodyTxL . allInputsTxBodyF)
-- >>> calcMinFeeTx relevantUtxo pp tx 5
calcMinFeeTx ::
  forall era.
  EraUTxO era =>
  -- | All TxOuts available for this transaction. In other words `TxIn`s produced by
  -- `allInputsTxBodyF` should be present in this `UTxO` map, however this is not checked.
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
  calcMinFeeTxInternal utxo pp tx extraKeyWitsCount Set.empty

calcMinFeeTxInternal ::
  forall era.
  EraUTxO era =>
  -- | All TxOuts available for this transaction. In other words `TxIn`s produced by
  -- `allInputsTxBodyF` should be present in this `UTxO` map, however this is not checked.
  UTxO era ->
  -- | The current protocol parameters.
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | Number of KeyHash witnesses that will be supplied for native scripts
  Int ->
  -- | KeyHash witnesses that will be supplied for native scripts
  Set.Set (KeyHash 'Witness (EraCrypto era)) ->
  Coin
calcMinFeeTxInternal utxo pp tx extraKeyWitsCount nativeScriptsKeyWitsHashes =
  setMinFeeTxUtxo pp tx' utxo ^. bodyTxL . feeTxBodyL
  where
    txBody = tx ^. bodyTxL
    tx' = addDummyWitsTx pp tx numKeyWitsRequired $ Map.elems byronAttributes
    inputs = Set.toList $ txBody ^. spendableInputsTxBodyF
    getByronAttrs txIn = do
      txOut <- Map.lookup txIn $ unUTxO utxo
      ba@(BootstrapAddress bootAddr) <- txOut ^. bootAddrTxOutF
      pure (asWitness (bootstrapKeyHash ba), Byron.addrAttributes bootAddr)
    byronAttributes = Map.fromList $ mapMaybe getByronAttrs inputs
    requiredKeyHashes =
      getWitsVKeyNeeded def utxo txBody Set.\\ nativeScriptsKeyWitsHashes
    numKeyWitsRequired =
      getGenesisKeyHashCountTxBody txBody
        + extraKeyWitsCount
        + Set.size (requiredKeyHashes Set.\\ Map.keysSet byronAttributes)

-- | Estimate a minimum transaction fee for a transaction that does not yet have all of
-- the `VKey` witnesses. This calculation is not very accurate in estimating Byron
-- witnesses, but it should work for the most part. If you have access to UTxO necessary
-- for the transaction that it is better and easier to use `calcMinFeeTx` instead.
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
  -- | The total size in bytes of reference scripts
  Int ->
  -- | The required minimum fee.
  Coin
estimateMinFeeTx pp tx numKeyWits numByronKeyWits refScriptsSize =
  setMinFeeTx pp tx' refScriptsSize ^. bodyTxL . feeTxBodyL
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

integralToByteStringN :: (Integral i, Bits i) => Int -> i -> ByteString
integralToByteStringN len = fst . BS.unfoldrN len (\n -> Just (fromIntegral n, n `shiftR` 8))

byteStringToNum :: (Bits i, Num i) => ByteString -> i
byteStringToNum = BS.foldr (\w i -> i `shiftL` 8 + fromIntegral w) 0

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
        . integralToByteStringN n
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

-- | Same as `setMinCoinSizedTxOut`, except it doesn't require the size of the
-- TxOut and will recompute it if needed. Initial amount is not important.
setMinCoinTxOut :: EraTxOut era => PParams era -> TxOut era -> TxOut era
setMinCoinTxOut = setMinCoinTxOutWith (==)

setMinCoinTxOutWith ::
  EraTxOut era =>
  (Coin -> Coin -> Bool) ->
  PParams era ->
  TxOut era ->
  TxOut era
setMinCoinTxOutWith f pp = go
  where
    go !txOut =
      let curMinCoin = getMinCoinTxOut pp txOut
          curCoin = txOut ^. coinTxOutL
       in if curCoin `f` curMinCoin
            then txOut
            else go (txOut & coinTxOutL .~ curMinCoin)

-- | A helpful placeholder to use during development.
boom :: HasCallStack => a
boom = error "Unimplemented"
{-# WARNING boom "BOOM!" #-}
