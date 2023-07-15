{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.AgdaSpec () where

import qualified HSLedgerTest as Agda
import Cardano.Ledger.Conway.Core
import Lens.Micro ((^.), (&), (.~))
import Cardano.Ledger.Coin (Coin(..))
import Data.Foldable (Foldable(..))
import Cardano.Ledger.TxIn (TxIn (..), TxId (..))
import Cardano.Ledger.BaseTypes (txIxToInt, SlotNo (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Crypto.Hash.Class (hashToBytes)
import PlutusLedgerApi.V1 ()
import Cardano.Ledger.Address (serialiseAddr)
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), LedgerState, NewEpochState)
import Cardano.Ledger.UTxO (UTxO(..))
import qualified Data.Map.Strict as Map
import Data.Bifunctor (Bifunctor(..))
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import qualified Data.Set as Set
import qualified Data.Sequence.Strict as StrictSeq
import Cardano.Ledger.Conway (Conway)
import qualified Data.ByteString.Lazy as BSL
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Shelley.API.Mempool
import Cardano.Ledger.Shelley.Genesis
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Monad (void)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import Data.List (sort)

example :: EraTx era => Tx era
example = mkBasicTx $
  mkBasicTxBody
    & inputsTxBodyL .~ Set.fromList []
    & outputsTxBodyL .~ StrictSeq.fromList []

byteStringToInteger :: ByteString -> Integer
byteStringToInteger = BS.foldr' (\x y -> y * 256 + toInteger x) 0

toAgdaTxIn :: TxIn era -> Agda.TxIn
toAgdaTxIn (TxIn txId txix) =
  ( byteStringToInteger . hashToBytes . extractHash $ unTxId txId
  , toInteger $ txIxToInt txix
  )

toAgdaTxOut :: EraTxOut era => TxOut era -> Agda.TxOut
toAgdaTxOut txout =
  ( byteStringToInteger . serialiseAddr $ txout ^. addrTxOutL
  , unCoin $ txout ^. coinTxOutL
  )

toAgdaTxId :: TxId era -> Agda.TxId
toAgdaTxId = byteStringToInteger . hashToBytes . extractHash . unTxId

toAgdaTxBody ::
  ( EraTx era
  ) => Tx era -> Agda.TxBody
toAgdaTxBody tx = Agda.MkTxBody
  { txins = fmap toAgdaTxIn . toList $ tx ^. bodyTxL . inputsTxBodyL
  , txouts =
      zip
        [0..]
        (fmap toAgdaTxOut . toList $ tx ^. bodyTxL . outputsTxBodyL)
  , txfee = unCoin $ tx ^. bodyTxL . feeTxBodyL
  , txvldt = (Nothing, Nothing)
  , txsize = tx ^. sizeTxF
  , txid = toAgdaTxId . txid $ tx ^. bodyTxL
  }

toAgdaUTxOState :: EraTxOut era => UTxOState era -> Agda.UTxOState
toAgdaUTxOState utxoSt = Agda.MkUTxOState
  { utxo =
      fmap (bimap toAgdaTxIn toAgdaTxOut) .
      Map.toList .
      unUTxO $
      utxosUtxo utxoSt
  , fees = unCoin $ utxosFees utxoSt
  }

toAgdaPParams :: EraPParams era => PParams era -> Agda.PParams
toAgdaPParams pp = Agda.MkPParams
  { a                = unCoin $ pp ^. ppMinFeeAL
  , b                = unCoin $ pp ^. ppMinFeeBL
  , maxBlockSize     = undefined
  , maxTxSize        = toInteger $ pp ^. ppMaxTxSizeL
  , maxHeaderSize    = undefined
  , maxValSize       = undefined
  , minUTxOValue     = undefined
  , poolDeposit      = undefined
  , emax             = undefined
  , pv               = undefined
  , votingThresholds = undefined
  , minCCSize        = undefined
  , ccTermLimit      = undefined
  , govExpiration    = undefined
  , govDeposit       = undefined
  , drepDeposit      = undefined
  , drepActivity     = undefined
  }

toAgdaUTxOEnv :: EraPParams era => UtxoEnv era -> Agda.UTxOEnv
toAgdaUTxOEnv (UtxoEnv slot pp _ _) = Agda.MkUTxOEnv
  { slot = toInteger $ unSlotNo slot
  , pparams = toAgdaPParams pp
  }

readNewEpochState :: IO (NewEpochState Conway)
readNewEpochState = do
  ls <- BSL.readFile "/home/work/Projects/cardano-ledger/CBOR/ledger-state.42.cbor"
  either (error . show) pure $ decodeFull (natVersion @9) ls :: IO (NewEpochState Conway)

tTx :: IO (Tx Conway)
tTx = do
  tx <- either error (pure . BSL.fromStrict) $ B16.decode "84a300818258207c7e5fc419c2fd1a33d8f4761dd6e4cb6c43554c55378c7fccdf65f22c1c946b000181a200581d60ed0998e484ef9c9088c43ee7169cf0811cea2e52c07a3bf7b5b0cb63011b00000045d962375b021a000280a5a10081825820190e437455e5909c2af62f5e61e316324dccca282a4a6fa8c47b851e1d4137b05840f687759143c365eb459597dca30c46fdda72a37cada18da040813624a59ae6da1eed3db6292470ea40c1a55ccc228be39791d6ae8a1cb06ddbc371ee52a37803f5f6"
  either (error . show) pure $ decodeFullAnnotator (natVersion @9) "Tx" decCBOR tx :: IO (Tx Conway)

mkEnv :: NewEpochState Conway -> LedgerEnv Conway
mkEnv nes =
  LedgerEnv
    { ledgerSlotNo = SlotNo 123456
    , ledgerIx = mkTxIxPartial 0
    , ledgerPp = pp
    , ledgerAccount = as
    }
  where
    pp = esPp (nesEs nes)
    as = esAccountState (nesEs nes)

test :: IO ()
test = do
  nes <- readNewEpochState
  tx <- tTx
  let env@LedgerEnv {..} = mkEnv nes
      st = esLState $ nesEs nes
  (ls, _) <- either (error . show) pure $
    applyTx
      @Conway
      testGlobals
      env
      st
      tx
  let implAgda = Just . toAgdaUTxOState $ ls ^. lsUTxOStateL
      agdaUtxoState = toAgdaUTxOState $ st ^. lsUTxOStateL
      agdaTx = toAgdaTxBody tx
      agdaUtxoEnv =
        Agda.MkUTxOEnv
          (toInteger $ unSlotNo ledgerSlotNo)
          (toAgdaPParams ledgerPp)
      agdaRes = Agda.utxoStep agdaUtxoEnv agdaUtxoState agdaTx
  putStrLn $ "Agda result: " <> show agdaRes
  putStrLn $ "Impl result: " <> show implAgda
  print $ sort (show agdaRes) == sort (show implAgda)



