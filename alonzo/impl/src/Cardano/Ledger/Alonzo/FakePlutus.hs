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
  { -- | Transaction inputs
    txInfoInputs :: [TxInInfo],
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

{-
transDCert (DCertDeleg (RegKey stkcred)) = P.DCertDelegRegKey  undefined
transDCert (DCertDeleg (DeRegKey stkcred)) = P.DCertDelegRegDeKey  undefined
transDCert (DCertDeleg (Delegate stkcred keyhash)) =  P.DCertDelegDelegate undefined undefined
transDCert (DCertPool (RegPool pp)) = P.DCertPoolRegister undefined undefined
transDCert (DCertPool (RetirePool keyhash epochni)) = P.DCertPoolRetire undefined undefined
transDCert (DCertGenesis _) = P.DCertGenesis
transDCert (DCertMir _) = P.DCertMir

 Missing
_certs :: !(StrictSeq (DCert (Crypto era))),
    _wdrls :: !(Wdrl (Crypto era)),

data DCert crypto
  = DCertDeleg !(DelegCert crypto)
  | DCertPool !(PoolCert crypto)
  | DCertGenesis !(GenesisDelegCert crypto)
  | DCertMir !(MIRCert crypto)

data DelegCert crypto
  = -- | A stake key registration certificate.
    RegKey !(StakeCredential crypto)
  | -- | A stake key deregistration certificate.
    DeRegKey !(StakeCredential crypto)
  | -- | A stake delegation certificate.
    Delegate !(Delegation crypto)

data PoolCert crypto
  = -- | A stake pool registration certificate.
    RegPool !(PoolParams crypto)
  | -- | A stake pool retirement certificate.
    RetirePool !(KeyHash 'StakePool crypto) !EpochNo
  deriving (Show, Generic, Eq, NFData)

data PoolParams crypto = PoolParams
  { _poolId :: !(KeyHash 'StakePool crypto),
    _poolVrf :: !(Hash crypto (VerKeyVRF crypto)),

data MIRCert crypto = MIRCert
  { mirPot :: MIRPot,
    mirRewards :: MIRTarget crypto

data MIRCert crypto = MIRCert
  { mirPot :: MIRPot,
    mirRewards :: MIRTarget crypto

data MIRPot = ReservesMIR | TreasuryMIR
  deriving (Show, Generic, Eq, NFData)

-- | The delegation of one stake key to another.
data Delegation crypto = Delegation
  { _delegator :: !(StakeCredential crypto),
    _delegatee :: !(KeyHash 'StakePool crypto)
  }

data RewardAcnt crypto = RewardAcnt
  { getRwdNetwork :: !Network,
    getRwdCred :: !(Credential 'Staking crypto)
  }
-}

instance IsData TxInfo where
  toData _txinfo = undefined
  fromData _dat = Nothing
