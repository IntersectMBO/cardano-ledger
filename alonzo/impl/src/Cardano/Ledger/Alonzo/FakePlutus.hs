{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Ledger.Alonzo.FakePlutus where

-- import Plutus.V1.Ledger.Address(Address (..))

import qualified Data.ByteString as BS (ByteString)
import Data.Map (Map)
import Data.Word (Word64)
import Language.PlutusTx (Data (..))
import Language.PlutusTx.IsData.Class (IsData (..))
import Numeric.Natural (Natural)
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
data StakingCredential
  = StakingHash BS.ByteString
  | StakingPtr Word64 Natural Natural
  deriving (Eq, Ord)

-- | This is isomorphic to the old Plutus stype Address
data Credential
  = PubKeyCredential !PubKeyHash
  | ScriptCredential !ValidatorHash

-- | The new style Plutus Address has two kinds of credentials, normal and staking
data Address = Address !Credential !(Maybe StakingCredential)

-- | The TxInIfo 'resolves' the TxIn from the Tx, using the UTxO and the PtrMap
data TxInInfo = TxInInfo
  { txInInfoOutRef :: !TxOutRef,
    txInInfoResolved :: !TxOut
  }

-- | Newstyle TxOut uses the new style Address
data TxOut = TxOut
  { txOutAddr :: !Address,
    txOutValue :: !Value,
    txOutDataHash :: !(Maybe DatumHash)
  }

data TxInfo = TxInfo
  { -- | Transaction inputs NOT used to pay fees
    txInfoInputs :: [TxInInfo],
    -- | Transaction inputs designated to pay fees
    txInfoInputsFees :: [TxInInfo],
    -- | Transaction outputs
    txInfoOutputs :: [TxOut],
    -- | The fee paid by this transaction.
    txInfoFee :: Value,
    -- | The 'Value' forged by this transaction
    txInfoForge :: Value,
    -- | Digests of Certificates included in this transaction
    txInfoDCert :: [DCert],
    -- | Withdrawals
    txInfoWdrl :: (Map StakingCredential Integer),
    -- | The valid range for the transaction.
    txInfoValidRange :: SlotRange,
    -- | Signatures provided with the transaction, attested that they all signed the Tx
    txInfoSignatories :: [PubKeyHash],
    txInfoData :: [(DatumHash, Datum)],
    -- | Hash of the pending transaction (excluding witnesses)
    txInfoId :: TxId
  }

-- | A representation of the Ledger DCert, Some information is digested, and not included
data DCert
  = DCertDelegRegKey StakingCredential
  | DCertDelegDeRegKey StakingCredential
  | DCertDelegDelegate
      StakingCredential
      -- ^ delegator
      PubKeyHash
      -- ^ delegatee
  | -- | A digest of the PoolParams
    DCertPoolRegister
      PubKeyHash
      -- ^ poolId
      PubKeyHash
      -- ^ pool VFR
  | -- | The retiremant certificate and the Epoch N
    DCertPoolRetire PubKeyHash Word64
  | -- | A really terse Digest
    DCertGenesis
  | -- | Another really terse Digest
    DCertMir

data ScriptPurpose
  = Minting CurrencySymbol
  | Spending TxOutRef
  | Rewarding StakingCredential
  | Certifying DCert

data Context = Context TxInfo ScriptPurpose

instance IsData Context where
  toData (Context _ _) = undefined
  fromData _ctxdata = Nothing
