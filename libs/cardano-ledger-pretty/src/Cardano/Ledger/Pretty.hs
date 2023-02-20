{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Pretty (
  module Cardano.Ledger.Pretty.Core,
) where

import Cardano.Chain.Common (
  AddrAttributes (..),
  Address (..),
  Attributes (..),
  HDAddressPayload (..),
  NetworkMagic (..),
  UnparsedFields (..),
 )
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address (
  Addr (..),
  BootstrapAddress (..),
  CompactAddr,
  RewardAcnt (..),
  decompactAddr,
 )
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  BlocksMade (..),
  BoundedRational (..),
  DnsName,
  FixedPoint,
  Globals (..),
  Network (..),
  NonNegativeInterval,
  Nonce (..),
  Port (..),
  ProtVer (..),
  StrictMaybe (..),
  UnitInterval,
  Url (..),
  Version,
  activeSlotLog,
  activeSlotVal,
  certIxToInt,
  dnsToText,
  getVersion64,
  txIxToInt,
 )
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (
  Credential (KeyHashObj, ScriptHashObj),
  GenesisCredential (..),
  Ptr (..),
  StakeReference (..),
 )
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.EpochBoundary (
  SnapShot (..),
  SnapShots (..),
  Stake (..),
 )
import qualified Cardano.Ledger.Era as Era (TxSeq)
import Cardano.Ledger.Keys (
  GKeys (..),
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash (..),
  VKey (..),
  VerKeyKES,
  hashKey,
 )
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness (..), ChainCode (..))
import Cardano.Ledger.MemoBytes (MemoBytes (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Pretty.Core
import Cardano.Ledger.Pretty.Generic ()
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  DPState (..),
  DState (..),
  EpochState (..),
  FutureGenDeleg (..),
  IncrementalStake (..),
  InstantaneousRewards (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.PParams (
  PPUpdateEnv (..),
  ProposedPPUpdates (..),
  Update (..),
 )
import Cardano.Ledger.Shelley.PoolRank (
  Histogram (..),
  Likelihood (..),
  LogWeight (..),
  NonMyopic (..),
  PerformanceEstimate (..),
 )
import Cardano.Ledger.Shelley.RewardUpdate (
  FreeVars (..),
  Pulser,
  PulsingRewUpdate (..),
  RewardAns (..),
  RewardPulser (..),
  RewardSnapShot (..),
  RewardUpdate (..),
 )
import Cardano.Ledger.Shelley.Rewards (
  LeaderOnlyReward (..),
  PoolRewardInfo (..),
  StakeShare (..),
 )
import Cardano.Ledger.Shelley.Scripts (MultiSig (..))
import Cardano.Ledger.Shelley.Tx (
  ShelleyTx (..),
 )
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..), ShelleyTxAuxData (..))
import Cardano.Ledger.Shelley.TxBody (
  ConstitutionalDelegCert (..),
  DCert (..),
  DelegCert (..),
  Delegation (..),
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
  PoolCert (..),
  PoolMetadata (..),
  PoolParams (..),
  ShelleyTxBody (..),
  ShelleyTxBodyRaw (..),
  ShelleyTxOut (..),
  StakePoolRelay (..),
  WitVKey (..),
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.TxWits (
  ShelleyTxWits,
 )
import Cardano.Ledger.Slot (
  BlockNo (..),
  Duration (..),
  EpochNo (..),
  EpochSize (..),
  SlotNo (..),
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMapCompact (RDPair (..), Trip, UMap (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Protocol.TPraos.BHeader (
  BHBody (..),
  BHeader (BHeader),
  HashHeader (..),
  LastAppliedBlock (..),
  PrevHash (..),
 )
import Cardano.Protocol.TPraos.OCert (
  KESPeriod (..),
  OCert (..),
  OCertEnv (..),
  OCertSignable (..),
 )
import Cardano.Slotting.Slot (WithOrigin (..))
import Cardano.Slotting.Time (SystemStart (SystemStart))
import Codec.Binary.Bech32
import qualified Data.ByteString as Long (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString, toStrict)
import Data.ByteString.Short (ShortByteString)
import Data.IP (IPv4, IPv6)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio, denominator, numerator)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set, toList)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.VMap (VMap)
import qualified Data.VMap as VMap
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Lens.Micro ((^.))
import Prettyprinter

instance PrettyA Long.ByteString where
  prettyA = ppLong

instance PrettyA Lazy.ByteString where
  prettyA = ppLazy

instance PrettyA () where
  prettyA = ppString . show

instance PrettyA Void where
  prettyA = absurd

instance PrettyA (LastAppliedBlock c) where
  prettyA = ppLastAppliedBlock

instance PrettyA (HashHeader c) where
  prettyA = ppHashHeader

instance PrettyA t => PrettyA (WithOrigin t) where
  prettyA = ppWithOrigin prettyA

-- =====================================================================================================
-- HELPER FUNCTIONS
-- =====================================================================================================

-- ======================
-- Named pretty printers for some simpe types

ppString :: String -> Doc a
ppString = pretty

ppDouble :: Double -> Doc a
ppDouble = viaShow

ppInteger :: Integer -> Doc a
ppInteger = viaShow

ppRational :: Rational -> Doc a
ppRational = viaShow

ppFloat :: Float -> Doc a
ppFloat = viaShow

ppNatural :: Natural -> Doc a
ppNatural = viaShow

ppWord64 :: Word64 -> Doc a
ppWord64 = viaShow

ppWord32 :: Word32 -> Doc a
ppWord32 = viaShow

ppWord8 :: Word8 -> Doc a
ppWord8 = viaShow

ppWord16 :: Word16 -> Doc a
ppWord16 = viaShow

ppFixedPoint :: FixedPoint -> Doc a
ppFixedPoint = viaShow

-- ppSignedDSIGN :: SignedDSIGN a b -> Doc ann
ppSignedDSIGN :: Show a => a -> PDoc
ppSignedDSIGN x = reAnnotate (Width 5 :) (viaShow x)

ppBool :: Bool -> Doc a
ppBool = viaShow

ppInt :: Int -> Doc a
ppInt = viaShow

-- =========================
-- operations for pretty printing

-- ======================
-- Byte Strings in Bech32 format

long_bech32 :: Long.ByteString -> Text
long_bech32 x =
  case humanReadablePartFromText "*" of
    Right human ->
      case encode human (dataPartFromBytes x) of
        Right ans -> ans
        Left _ -> "bech32Error"
    Left _ -> "bech32Error"

lazy_bech32 :: Lazy.ByteString -> Text
lazy_bech32 x =
  case humanReadablePartFromText "*" of
    Right human ->
      case encode human (dataPartFromBytes (Lazy.toStrict x)) of
        Right ans -> ans
        Left _ -> "bech32Error"
    Left _ -> "bech32Error"

ppLong :: Long.ByteString -> PDoc
ppLong x = text (long_bech32 x)

ppLazy :: Lazy.ByteString -> PDoc
ppLazy x = text (lazy_bech32 x)

ppSet :: (x -> Doc ann) -> Set x -> Doc ann
ppSet p xs = encloseSep lbrace rbrace comma (map p (toList xs))

instance PrettyA a => PrettyA (StrictSeq a) where
  prettyA xs = prettyA (foldr (:) [] xs)

ppStrictMaybe :: (x -> Doc ann) -> StrictMaybe x -> Doc ann
ppStrictMaybe _ SNothing = text "?-"
ppStrictMaybe p (SJust x) = text "?" <> p x

ppMaybe :: (x -> Doc ann) -> Maybe x -> Doc ann
ppMaybe _ Nothing = text "?-"
ppMaybe p (Just x) = text "?" <> p x

ppVMap ::
  ( PrettyA k
  , PrettyA v
  , VMap.Vector kv k
  , VMap.Vector vv v
  ) =>
  VMap kv vv k v ->
  PDoc
ppVMap = ppAssocList (text "VMap") prettyA prettyA . VMap.toList

instance
  ( PrettyA k
  , PrettyA v
  , VMap.Vector kv k
  , VMap.Vector vv v
  ) =>
  PrettyA (VMap kv vv k v)
  where
  prettyA = ppVMap

-- =============================================================================
-- END HELPER FUNCTIONS
-- =============================================================================

-- ======================================================

ppLastAppliedBlock :: LastAppliedBlock c -> PDoc
ppLastAppliedBlock (LastAppliedBlock blkNo slotNo hh) =
  ppRecord
    "LastAppliedBlock"
    [ ("blockNo", ppBlockNo blkNo)
    , ("slotNo", ppSlotNo slotNo)
    , ("hash", ppHashHeader hh)
    ]

ppHashHeader :: HashHeader c -> PDoc
ppHashHeader (HashHeader x) = ppHash x

ppWithOrigin :: (t -> PDoc) -> WithOrigin t -> PDoc
ppWithOrigin _ Origin = ppString "Origin"
ppWithOrigin pp (At t) = ppSexp "At" [pp t]

ppBHBody :: Crypto c => BHBody c -> PDoc
ppBHBody (BHBody bn sn prev vk vrfvk eta l size hash ocert protver) =
  ppRecord
    "BHBody"
    [ ("BlockNo", ppBlockNo bn)
    , ("SlotNo", ppSlotNo sn)
    , ("Prev", ppPrevHash prev)
    , ("VKey", prettyA vk)
    , ("VerKeyVRF", viaShow vrfvk) -- The next 3 are type families
    , ("Eta", viaShow eta)
    , ("L", viaShow l)
    , ("size", ppNatural size)
    , ("Hash", ppHash hash)
    , ("OCert", ppOCert ocert)
    , ("ProtVersion", prettyA protver)
    ]

ppPrevHash :: PrevHash c -> PDoc
ppPrevHash GenesisHash = ppString "GenesisHash"
ppPrevHash (BlockHash x) = ppSexp "BlockHashppHashHeader" [ppHashHeader x]

ppBHeader :: Crypto c => BHeader c -> PDoc
ppBHeader (BHeader bh sig) =
  ppRecord
    "BHeader"
    [ ("Body", ppBHBody bh)
    , ("Sig", viaShow sig)
    ]

ppBlock :: (PrettyA (Era.TxSeq era), PrettyA (h)) => Block h era -> PDoc
ppBlock (UnserialisedBlock bh seqx) =
  ppRecord
    "Block"
    [ ("Header", prettyA bh)
    , ("TxSeq", prettyA seqx)
    ]

instance Crypto c => PrettyA (BHBody c) where
  prettyA = ppBHBody

instance Crypto c => PrettyA (BHeader c) where
  prettyA = ppBHeader

instance PrettyA (PrevHash c) where
  prettyA = ppPrevHash

instance (Era era, PrettyA (Era.TxSeq era), PrettyA h) => PrettyA (Block h era) where
  prettyA = ppBlock

-- =================================
-- Cardano.Ledger.Shelley.LedgerState.Delegation.Certificates

ppPoolDistr :: PoolDistr c -> PDoc
ppPoolDistr (PoolDistr mp) = ppSexp "PoolDistr" [ppMap ppKeyHash ppIndividualPoolStake mp]

ppIndividualPoolStake :: IndividualPoolStake c -> PDoc
ppIndividualPoolStake (IndividualPoolStake r1 h) =
  ppRecord
    "IndividualPoolStake"
    [ ("stake", ppRational r1)
    , ("stakeVrf", ppHash h)
    ]

instance PrettyA (PoolDistr c) where
  prettyA = ppPoolDistr

instance PrettyA (IndividualPoolStake c) where
  prettyA = ppIndividualPoolStake

-- ================================
-- Cardano.Ledger.Shelley.RewardUpdate

instance PrettyA (NonMyopic c)

instance PrettyA RewardType

instance PrettyA (Reward c)

instance PrettyA (RewardSnapShot c)

instance PrettyA (LeaderOnlyReward c)

instance PrettyA (PoolRewardInfo c)

instance PrettyA (FreeVars c)

instance PrettyA (RewardAns c) where
  prettyA (RewardAns x y) =
    ppRecord
      "RewardAns"
      [ ("allEvents", prettyA x)
      , ("recentEvents", prettyA y)
      ]

instance PrettyA (Pulser c) where
  prettyA (RSLP n free items ans) =
    ppSexp
      "RewardPulser"
      [ prettyA n
      , prettyA free
      , prettyA items
      , prettyA ans
      ]

instance PrettyA (PulsingRewUpdate c)

instance PrettyA (RewardUpdate c)

-- instance PrettyA (RewardAccounts era) where
-- prettyA = ppRewardAcnt

-- =================================
-- Cardano.Ledger.Shelley.LedgerState

-- | Constraints needed to ensure that the ledger state can be pretty printed.
type CanPrettyPrintLedgerState era =
  ( PrettyA (TxOut era)
  , PrettyA (PParams era)
  )

instance PrettyA AccountState

instance PrettyA (DPState c)

instance PrettyA RDPair where
  prettyA (RDPair rew dep) =
    ppRecord
      "RDPair"
      [ ("reward", prettyA rew)
      , ("deposit", prettyA dep)
      ]

instance PrettyA (Trip c)

instance PrettyA (UMap c) where
  prettyA (UMap tripmap ptrmap) =
    ppRecord
      "UMap"
      [ ("combined", prettyA tripmap)
      , ("ptrs", prettyA ptrmap)
      ]

instance PrettyA (DState c)

instance
  ( Era era
  , CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  PrettyA (EpochState era)

instance
  ( Era era
  , CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  PrettyA (NewEpochState era)
  where
  prettyA (NewEpochState enum prevB curB es rewup pool _) =
    ppRecord
      "NewEpochState"
      [ ("epochnum", prettyA enum)
      , ("prevBlock", prettyA prevB)
      , ("currBlock", prettyA curB)
      , ("epochState", prettyA es)
      , ("rewUpdate", prettyA rewup)
      , ("poolDist", prettyA pool)
      ]

instance PrettyA (FutureGenDeleg c)

instance PrettyA (InstantaneousRewards c)

instance
  ( Era era
  , CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  PrettyA (LedgerState era)

instance
  PrettyA (PParamsUpdate era) =>
  PrettyA (ShelleyPPUPState era)

instance PrettyA (PState c)

instance
  ( Era era
  , CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  PrettyA (UTxOState era)

instance PrettyA (IncrementalStake c)

instance PrettyA PerformanceEstimate

instance PrettyA StakeShare

instance PrettyA Histogram

instance PrettyA LogWeight

instance PrettyA Likelihood

instance PrettyA (Stake c)

instance PrettyA (BlocksMade c)

instance PrettyA (SnapShot c)

instance PrettyA (SnapShots c)

instance PrettyA (TxOut era) => PrettyA (UTxO era)

instance PrettyA a => PrettyA [a] where
  prettyA xs =
    group $
      flatAlt
        (puncLeft lbracket (map prettyA xs) comma rbracket)
        (encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (map prettyA xs))

instance PrettyA Metadatum where
  prettyA (Map m) =
    let pairs = fmap (\(k, v) -> arrow (prettyA k, prettyA v)) m
     in ppSexp
          "Map"
          [ group $
              flatAlt
                (hang 1 (puncLeft lbrace pairs comma rbrace))
                (encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) pairs)
          ]
  prettyA (List ds) = ppSexp "List" [prettyA ds]
  prettyA (I n) = ppSexp "I" [ppInteger n]
  prettyA (B bs) = ppSexp "B" [ppLong bs]
  prettyA (S txt) = ppSexp "S" [text txt]

instance Era era => PrettyA (ShelleyTxAuxData era)

instance
  ( PrettyA (TxBody era)
  , PrettyA (TxAuxData era)
  , PrettyA (TxWits era)
  , EraTx era
  , Tx era ~ ShelleyTx era
  ) =>
  PrettyA (ShelleyTx era)
  where
  prettyA tx =
    ppRecord
      "Tx"
      [ ("body", prettyA $ tx ^. bodyTxL)
      , ("witnessSet", prettyA $ tx ^. witsTxL)
      , ("metadata", ppStrictMaybe prettyA $ tx ^. auxDataTxL)
      ]

instance Crypto c => PrettyA (BootstrapWitness c) where
  prettyA (BootstrapWitness key sig (ChainCode code) attr) =
    ppRecord
      "BootstrapWitness"
      [ ("key", ppVKey key)
      , ("signature", ppSignedDSIGN sig)
      , ("chaincode", ppLong code)
      , ("attributes", ppLong attr)
      ]

instance (Era era, PrettyA (Script era)) => PrettyA (ShelleyTxWits era)

-- ============================
--  Cardano.Ledger.AuxiliaryData

instance PrettyA (AuxiliaryDataHash c)

instance PrettyA (SafeHash c i) where
  prettyA = ppHash . extractHash

-- ============================
--  Cardano.Ledger.Compactible

instance (Compactible a, PrettyA a) => PrettyA (CompactForm a) where
  prettyA = prettyA . fromCompact

instance PrettyA DeltaCoin

instance PrettyA (Delegation c)

instance PrettyA PoolMetadata

instance PrettyA StakePoolRelay

instance PrettyA (PoolParams c)

instance PrettyA (Withdrawals c)

instance PrettyA (TxId c)

instance PrettyA (TxIn c) where
  prettyA (TxIn txid index) =
    ppSexp
      "TxIn"
      [prettyA txid, pretty (txIxToInt index)]

instance (EraTxOut era, PrettyA (Value era)) => PrettyA (ShelleyTxOut era)

instance PrettyA (DelegCert c)

instance PrettyA (PoolCert c)

instance PrettyA (ConstitutionalDelegCert c)

instance PrettyA MIRPot

instance PrettyA (MIRTarget c) where
  prettyA (StakeAddressesMIR rews) = prettyA rews
  prettyA (SendToOppositePotMIR c) = prettyA c

instance PrettyA (MIRCert c)

instance PrettyA (DCert c)

instance
  ( EraTxOut era
  , PrettyA (PParamsUpdate era)
  , PrettyA (TxOut era)
  , Era era
  ) =>
  PrettyA (ShelleyTxBody era)
  where
  prettyA (TxBodyConstr (Memo (ShelleyTxBodyRaw ins outs cs withdrawals fee ttl upd mdh) _)) =
    ppRecord
      "TxBody"
      [ ("inputs", prettyA ins)
      , ("outputs", prettyA outs)
      , ("cert", prettyA cs)
      , ("withdrawals", prettyA withdrawals)
      , ("fee", prettyA fee)
      , ("timetolive", prettyA ttl)
      , ("update", prettyA upd)
      , ("metadatahash", prettyA mdh)
      ]

instance (Typeable kr, Crypto c) => PrettyA (WitVKey kr c) where
  prettyA (WitVKey key sig) =
    ppRecord
      "WitVKey"
      [("key", ppVKey key), ("signature", ppSignedDSIGN sig)]

instance PrettyA IPv4 where
  prettyA = viaShow

instance PrettyA IPv6 where
  prettyA = viaShow

instance Crypto c => PrettyA (CompactAddr c) where
  prettyA x = ppAddr (decompactAddr x)

-- ================================================
-- Cardano.Ledger.Shelley.PParams

instance PrettyA Version where
  prettyA = ppWord64 . getVersion64

ppProposedPPUpdates :: PrettyA (PParamsUpdate era) => ProposedPPUpdates era -> PDoc
ppProposedPPUpdates (ProposedPPUpdates m) = ppMap ppKeyHash prettyA m

instance PrettyA (PPUpdateEnv c)

instance PrettyA (PParamsUpdate e) => PrettyA (ProposedPPUpdates e) where
  prettyA = ppProposedPPUpdates

instance PrettyA (PParamsUpdate e) => PrettyA (Update e)

instance PrettyA ProtVer

instance PrettyA Coin

-- ===========================================
-- Cardano.Chain.Common

ppBootstrapAddress :: BootstrapAddress c -> PDoc
ppBootstrapAddress (BootstrapAddress (Address root (Attributes (AddrAttributes path magic) y) typ)) =
  ppRecord
    "BootstrapAddress"
    [ ("root", viaShow root) -- Cardano.Crypto.Hashing.AbstractHash
    , ("derivationpath", ppMaybe ppHDAddressPayload path)
    , ("networkmagic", ppNetworkMagic magic)
    , ("remain", ppUnparsedFields y)
    , ("type", viaShow typ) -- Cardano.Chain.Common.AddrSpendingData.AddrType
    ]

ppNetworkMagic :: NetworkMagic -> PDoc
ppNetworkMagic NetworkMainOrStage = text "MainOrStage"
ppNetworkMagic (NetworkTestnet n) = text "Testnet" <+> ppWord32 n

ppUnparsedFields :: UnparsedFields -> PDoc
ppUnparsedFields (UnparsedFields m) = ppMap' mempty ppWord8 ppLazy m

instance PrettyA NetworkMagic

instance PrettyA (BootstrapAddress c) where
  prettyA (BootstrapAddress (Address root (Attributes (AddrAttributes path magic) y) typ)) =
    ppRecord
      "BootstrapAddress"
      [ ("root", viaShow root) -- Cardano.Crypto.Hashing.AbstractHash
      , ("derivationpath", ppMaybe ppHDAddressPayload path)
      , ("networkmagic", ppNetworkMagic magic)
      , ("remain", ppUnparsedFields y)
      , ("type", viaShow typ) -- Cardano.Chain.Common.AddrSpendingData.AddrType
      ]

instance PrettyA UnparsedFields

-- ===========================================
-- Cardano.Ledger.Shelley.Address

ppAddr :: Addr c -> PDoc
ppAddr (Addr net cred ref) = ppSexp "Addr" [ppNetwork net, ppCredential cred, ppStakeReference ref]
ppAddr (AddrBootstrap x) = ppBootstrapAddress x

ppHDAddressPayload :: HDAddressPayload -> PDoc
ppHDAddressPayload (HDAddressPayload x) = ppLong x

ppRewardAcnt' :: RewardAcnt c -> PDoc
ppRewardAcnt' (RewardAcnt net cred) = ppRecord "RewardAcnt" [("network", ppNetwork net), ("credential", ppCredential cred)]

instance PrettyA (Addr c) where
  prettyA = ppAddr

instance PrettyA (RewardAcnt c) where
  prettyA = ppRewardAcnt'

-- ===========================================
-- Cardano.Ledger.Shelley.Credential

ppCredential :: Credential keyrole c -> PDoc
ppCredential (ScriptHashObj (ScriptHash x)) = ppSexp "ScriptCred" [ppHash x]
ppCredential (KeyHashObj (KeyHash x)) = ppSexp "KeyCred" [ppHash x]

ppPtr :: Ptr -> PDoc
ppPtr (Ptr slot txIx certIx) =
  ppSexp "Ptr" [ppSlotNo slot, pretty (txIxToInt txIx), pretty (certIxToInt certIx)]

ppStakeReference :: StakeReference c -> PDoc
ppStakeReference (StakeRefBase x) = ppSexp "BaseRef" [ppCredential x]
ppStakeReference (StakeRefPtr x) = ppSexp "PtrRef" [ppPtr x]
ppStakeReference StakeRefNull = text "NullRef"

ppGenesisCredential :: GenesisCredential c -> PDoc
ppGenesisCredential (GenesisCredential (KeyHash x)) = ppSexp "GenesisCredential" [ppHash x]

instance PrettyA (Credential r c) where
  prettyA = ppCredential

instance PrettyA (StakeReference c) where
  prettyA = ppStakeReference

instance PrettyA (GenesisCredential c) where
  prettyA = ppGenesisCredential

instance PrettyA Ptr where
  prettyA = ppPtr

-- ===========================================
-- Cardano.Ledger.Shelley.Scripts

ppMultiSig :: Era era => MultiSig era -> PDoc
ppMultiSig (RequireSignature hk) = ppSexp "Require" [ppKeyHash hk]
ppMultiSig (RequireAllOf ps) = ppSexp "AllOf" (map ppMultiSig ps)
ppMultiSig (RequireAnyOf ps) = ppSexp "AnyOf" (map ppMultiSig ps)
ppMultiSig (RequireMOf m ps) = ppSexp "MOf" (pretty m : map ppMultiSig ps)

ppScriptHash :: ScriptHash c -> PDoc
ppScriptHash (ScriptHash h) = ppSexp "ScriptHash" [ppHash h]

instance PrettyA (ScriptHash c) where
  prettyA = ppScriptHash

instance Era era => PrettyA (MultiSig era) where
  prettyA = ppMultiSig

-- ====================================================
-- Cardano.Ledger.Shelley.Slot

ppSlotNo :: SlotNo -> Doc ann
ppSlotNo (SlotNo x) = text "SlotNo" <+> pretty x

ppDuration :: Duration -> Doc ann
ppDuration (Duration x) = text "Duration" <+> pretty x

ppEpochNo :: EpochNo -> Doc ann
ppEpochNo (EpochNo x) = text "EpochNo" <+> pretty x

ppEpochSize :: EpochSize -> Doc ann
ppEpochSize (EpochSize x) = text "EpochSize" <+> pretty x

ppBlockNo :: BlockNo -> Doc ann
ppBlockNo (BlockNo x) = text "BlockNo" <+> pretty x

instance PrettyA SlotNo where
  prettyA = ppSlotNo

instance PrettyA Duration where
  prettyA = ppDuration

instance PrettyA EpochNo where
  prettyA = ppEpochNo

instance PrettyA EpochSize where
  prettyA = ppEpochSize

instance PrettyA BlockNo where
  prettyA = ppBlockNo

-- ===================================================
-- Cardano.Ledger.Shelley.OCert

ppKESPeriod :: KESPeriod -> PDoc
ppKESPeriod (KESPeriod x) = text "KESPeriod" <+> pretty x

ppOCertEnv :: OCertEnv c -> PDoc
ppOCertEnv (OCertEnv ps ds) =
  ppRecord
    "OCertEnv"
    [ ("ocertEnvStPools", ppSet ppKeyHash ps)
    , ("ocertEnvGenDelegs", ppSet ppKeyHash ds)
    ]

ppOCert :: forall c. Crypto c => OCert c -> PDoc
ppOCert (OCert vk n per sig) =
  ppRecord
    "OCert"
    [ ("ocertVkKot", ppVerKeyKES (Proxy @c) vk)
    , ("ocertN", pretty n)
    , ("ocertKESPeriod", ppKESPeriod per)
    , ("ocertSigma", ppSignedDSIGN sig)
    ]

ppOCertSignable :: forall c. Crypto c => OCertSignable c -> PDoc
ppOCertSignable (OCertSignable verkes w per) =
  ppSexp "OCertSignable" [ppVerKeyKES (Proxy @c) verkes, pretty w, ppKESPeriod per]

instance PrettyA KESPeriod where
  prettyA = ppKESPeriod

instance PrettyA (OCertEnv c) where
  prettyA = ppOCertEnv

instance Crypto c => PrettyA (OCert c) where
  prettyA = ppOCert

instance Crypto c => PrettyA (OCertSignable c) where
  prettyA = ppOCertSignable

-- ==========================================
--  Cardano.Crypto.Hash

ppHash :: Hash.Hash a b -> PDoc
ppHash x = text "#" <> reAnnotate (Width 5 :) (viaShow x)

instance PrettyA (Hash.Hash a b) where
  prettyA = ppHash

-- ==========================================
-- Cardano.Ledger.Shelley.BaseTypes

ppUnitInterval :: UnitInterval -> PDoc
ppUnitInterval = viaShow

ppNonce :: Nonce -> PDoc
ppNonce (Nonce h) = text "Nonce" <+> ppHash h
ppNonce NeutralNonce = text "NeutralNonce"

ppActiveSlotCoeff :: ActiveSlotCoeff -> PDoc
ppActiveSlotCoeff x =
  ppRecord
    "ActiveSlotCoeff"
    [ ("activeSlotVal", viaShow (activeSlotVal x))
    , ("ActiveSlotLog", ppFixedPoint (activeSlotLog x))
    ]

ppGlobals :: Globals -> PDoc
ppGlobals
  ( Globals
      _e
      slot
      stab
      ran
      sec
      maxkes
      quor
      maxmaj
      maxlove
      active
      net
      start
    ) =
    ppRecord
      "Globals"
      [ ("epochInfo", text "?")
      , ("slotsPerKESPeriod", pretty slot)
      , ("stabilityWindow", pretty stab)
      , ("randomnessStabilisationWindow", pretty ran)
      , ("securityParameter", pretty sec)
      , ("maxKESEvo", pretty maxkes)
      , ("quorum", pretty quor)
      , ("maxMajorPV", prettyA maxmaj)
      , ("maxLovelaceSupply", pretty maxlove)
      , ("activeSlotCoeff", ppActiveSlotCoeff active)
      , ("networkId", ppNetwork net)
      , ("systemStart", ppSystemStart start)
      ]

ppNetwork :: Network -> PDoc
ppNetwork Testnet = text "Testnet"
ppNetwork Mainnet = text "Mainnet"

ppSystemStart :: SystemStart -> PDoc
ppSystemStart (SystemStart time) = viaShow time

ppUrl :: Url -> PDoc
ppUrl x = text (urlToText x)

ppPort :: Port -> PDoc
ppPort (Port n) = ppSexp "Port" [ppWord16 n]

ppDnsName :: DnsName -> PDoc
ppDnsName x = ppSexp "DnsName" [text (dnsToText x)]

instance PrettyA Url where
  prettyA = ppUrl

instance PrettyA Network where
  prettyA = ppNetwork

instance PrettyA Globals where
  prettyA = ppGlobals

instance PrettyA ActiveSlotCoeff where
  prettyA = ppActiveSlotCoeff

instance PrettyA Nonce where
  prettyA = ppNonce

instance PrettyA UnitInterval where
  prettyA = ppUnitInterval

instance PrettyA Port where
  prettyA = ppPort

instance PrettyA DnsName where
  prettyA = ppDnsName

-- ===========================================
-- Cardano.Ledger.Shelley.Keys

ppVKey :: Crypto c => VKey r c -> PDoc
ppVKey vk@(VKey x) = vsep [reAnnotate (Width 5 :) (viaShow x), "hash = " <+> ppKeyHash (hashKey vk)]

ppKeyHash :: KeyHash x c -> PDoc
ppKeyHash (KeyHash x) = ppSexp "KeyHash" [ppHash x]

ppGenDelegPair :: GenDelegPair c -> PDoc
ppGenDelegPair (GenDelegPair (KeyHash x) y) =
  ppRecord "GenDelegPair" [("KeyHash", ppHash x), ("VrfHash", ppHash y)]

ppGKeys :: Crypto c => GKeys c -> PDoc
ppGKeys (GKeys x) = ppSexp "GKeys" [ppSet ppVKey x]

ppVerKeyKES :: forall c. Crypto c => Proxy c -> VerKeyKES c -> PDoc
ppVerKeyKES Proxy x = reAnnotate (Width 5 :) (viaShow x)

ppGenDelegs :: GenDelegs c -> PDoc
ppGenDelegs (GenDelegs m) = ppSexp "GenDelegs" [ppMap ppKeyHash ppGenDelegPair m]

instance Crypto c => PrettyA (VKey r c) where
  prettyA = ppVKey

instance PrettyA (KeyHash x c) where
  prettyA = ppKeyHash

instance Crypto c => PrettyA (GKeys c) where
  prettyA = ppGKeys

instance PrettyA (GenDelegPair c) where
  prettyA = ppGenDelegPair

instance PrettyA (GenDelegs c) where
  prettyA = ppGenDelegs

-- =========================================================

-- | Wrap a type: t (that has a PrettyA instance) with a newtype
--   cnstructor, so that the prettyA instance is used to Show (Nice t).
--   Can also be used as a DerivingVia helper.
newtype Nice t = Nice {unNice :: t}

instance PrettyA t => Show (Nice t) where
  show (Nice x) = show (prettyA x)
  showList xs more = show (prettyA $ unNice <$> xs) ++ more

instance Eq t => Eq (Nice t) where
  (Nice x) == (Nice y) = x == y

instance PrettyA String where
  prettyA = ppString

instance PrettyA Double where
  prettyA = ppDouble

instance PrettyA Integer where
  prettyA = ppInteger

instance PrettyA Float where
  prettyA = ppFloat

instance PrettyA Natural where
  prettyA = ppNatural

instance PrettyA Word64 where
  prettyA = ppWord64

instance PrettyA Word32 where
  prettyA = ppWord32

instance PrettyA Word16 where
  prettyA = ppWord16

instance PrettyA Word8 where
  prettyA = ppWord8

instance PrettyA FixedPoint where
  prettyA = ppFixedPoint

instance PrettyA Bool where
  prettyA = ppBool

instance PrettyA Int where
  prettyA = ppInt

instance PrettyA a => PrettyA (Ratio a) where
  prettyA x = prettyA (numerator x) <> " % " <> prettyA (denominator x)

instance PrettyA a => PrettyA (Set a) where
  prettyA = ppSet prettyA

instance (PrettyA x, PrettyA y) => PrettyA (x, y) where
  prettyA (x, y) = "(" <> prettyA x <> ", " <> prettyA y <> ")"

instance PrettyA a => PrettyA (StrictMaybe a) where
  prettyA = ppStrictMaybe prettyA

instance PrettyA ShortByteString where
  prettyA = viaShow

instance (PrettyA (a era), Era era) => PrettyA (MemoBytes a era) where
  prettyA (Memo v h) = prettyA v <> " (hash " <> prettyA h <> ")"

instance (PrettyA x, PrettyA y) => PrettyA (Map.Map x y) where
  prettyA m = ppMap prettyA prettyA m

instance PrettyA NonNegativeInterval where
  prettyA = prettyA . unboundRational

instance PrettyA (PParams (ShelleyEra c)) where
  prettyA = undefined

instance PrettyA (PParamsUpdate (ShelleyEra c)) where
  prettyA = undefined
