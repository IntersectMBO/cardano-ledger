{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Ledger.Alonzo.FakePlutus where

-- import Plutus.V1.Ledger.Address(Address (..))

import qualified Data.ByteString as BS (ByteString)
import Language.PlutusTx (Data (..))
import Language.PlutusTx.IsData.Class (IsData (..))
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Contexts (TxOutInfo)
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import Plutus.V1.Ledger.Interval
  ( Extended (..),
    Interval (..),
    LowerBound (..),
    UpperBound (..),
  )
import Plutus.V1.Ledger.Scripts (Datum (..), DatumHash (..), MonetaryPolicyHash (..), ValidatorHash (..))
import Plutus.V1.Ledger.Slot (SlotRange)
import Plutus.V1.Ledger.Tx (TxOutRef (..), TxOutType (..))
import Plutus.V1.Ledger.TxId (TxId (..))
import Plutus.V1.Ledger.Value (CurrencySymbol (..), TokenName (..), Value (..), singleton, unionWith)

-- ========================================================

-- | Legal addresses may have Staking credential, used to assign rewards
newtype StakingHash = StakingHash {getStakingHash :: BS.ByteString}

-- | This is isomorphic to the old Plutus stype Address
data Credential
  = PubKeyCredential !PubKeyHash
  | ScriptCredential !ValidatorHash

-- | The new style Plutus Address has two kinds of credentials, normal and staking
data Address = Address !Credential !(Maybe StakingHash)

-- | The TxInIfo 'resolves' the TxIn from the Tx, using the UTxO and the PtrMap
data TxInInfo = TxInInfo
  { txInInfoOutRef :: !TxOutRef,
    txInAddr :: !Address,
    txInValue :: !Value,
    txInDataHash :: !(Maybe DatumHash)
  }

-- | Newstyle TxOut uses the new style Address
data TxOut = TxOut
  { txOutAddr :: !Address,
    txOutValue :: !Value,
    txOutDataHash :: !(Maybe DatumHash)
  }

data TxInfo = TxInfo
  { -- | Transaction inputs
    txInfoInputs :: [TxInInfo],
    -- | Transaction outputs
    txInfoOutputs :: [TxOut],
    -- | The fee paid by this transaction.
    txInfoFee :: Value,
    -- | The 'Value' forged by this transaction.
    txInfoForge :: Value,
    -- | The valid range for the transaction.
    txInfoValidRange :: SlotRange,
    -- | Signatures provided with the transaction
    txInfoSignatories :: [PubKeyHash],
    txInfoData :: [(DatumHash, Datum)],
    -- | Hash of the pending transaction (excluding witnesses)
    txInfoId :: TxId
  }

instance IsData TxInfo where
  toData _txinfo = undefined
  fromData _dat = Nothing
