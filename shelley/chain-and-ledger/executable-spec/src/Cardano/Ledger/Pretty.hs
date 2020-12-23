{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Pretty where

import Cardano.Binary (ToCBOR)
import Cardano.Chain.Common
  ( AddrAttributes (..),
    Address (..),
    Attributes (..),
    HDAddressPayload (..),
    NetworkMagic (..),
    UnparsedFields (..),
  )
import qualified Cardano.Crypto.Hash as Hash
-- import Cardano.Crypto.Hashing(abstractHashToShort)

import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Compactible (Compactible (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era (Era)
import Control.Monad.Identity (Identity)
import Control.SetAlgebra (forwards)
import Data.IP (IPv4, IPv6)
import qualified Data.Map.Strict as Map (Map, fromList, toList)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set, toList)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Prettyprinter
import Prettyprinter.Internal (Doc (Empty))
import Prettyprinter.Util (putDocW)
import Shelley.Spec.Ledger.Address
  ( Addr (..),
    BootstrapAddress (..),
    RewardAcnt (..),
  )
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness (..), ChainCode (..))
import Shelley.Spec.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    DnsName,
    Globals (..),
    Network (..),
    Nonce (..),
    Port (..),
    StrictMaybe (..),
    UnitInterval,
    Url (..),
    activeSlotLog,
    activeSlotVal,
    dnsToText,
  )
import Shelley.Spec.Ledger.Coin (Coin (..), DeltaCoin (..))
import Shelley.Spec.Ledger.CompactAddr (CompactAddr (..), decompactAddr)
import Shelley.Spec.Ledger.Credential
  ( Credential (KeyHashObj, ScriptHashObj),
    GenesisCredential (..),
    Ptr (..),
    StakeReference (..),
  )
import Shelley.Spec.Ledger.EpochBoundary
  ( BlocksMade (..),
    SnapShot (..),
    SnapShots (..),
    Stake (..),
  )
import Shelley.Spec.Ledger.Keys
  ( GKeys (..),
    GenDelegPair (..),
    GenDelegs (..),
    KeyHash (..),
    KeyPair (..),
    KeyRole (Staking),
    VKey (..),
    VerKeyKES,
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    FutureGenDeleg (..),
    InstantaneousRewards (..),
    Ix,
    LedgerState (..),
    PPUPState (..),
    PState (..),
    RewardUpdate (..),
    UTxOState (..),
  )
import Shelley.Spec.Ledger.Metadata (Metadata (..), Metadatum (..))
import Shelley.Spec.Ledger.OCert
  ( KESPeriod (..),
    OCert (..),
    OCertEnv (..),
    OCertSignable (..),
  )
import Shelley.Spec.Ledger.PParams
  ( PPUpdateEnv (..),
    PParams' (..),
    ProposedPPUpdates (..),
    ProtVer (..),
    Update (..),
  )
import Shelley.Spec.Ledger.Rewards
  ( Histogram (..),
    Likelihood (..),
    LogWeight (..),
    NonMyopic (..),
    PerformanceEstimate (..),
    StakeShare (..),
  )
import Shelley.Spec.Ledger.Scripts (MultiSig (..), ScriptHash (..))
import Shelley.Spec.Ledger.Slot
  ( BlockNo (..),
    Duration (..),
    EpochNo (..),
    EpochSize (..),
    SlotNo (..),
  )
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD,
    prettyWitnessSetParts,
  )
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    PoolCert (..),
    PoolMetadata (..),
    PoolParams (..),
    StakeCreds (..),
    StakePoolRelay (..),
    TxBody (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
    WitVKey (..),
  )
import Shelley.Spec.Ledger.UTxO (UTxO (..))

-- =====================================================================================================
-- HELPER FUNCTIONS
-- =====================================================================================================

isEmpty :: Doc ann -> Bool
isEmpty Empty = True
isEmpty _ = False

putDoc :: Doc ann -> IO ()
putDoc = putDocW 80

data PrettyAnn = Width Int

type Ann = [PrettyAnn]

type PDoc = Doc Ann

text :: Text -> Doc ann
text x = pretty x

equate :: Doc a -> Doc a -> Doc a
equate x y = group (flatAlt (hang 2 (sep [x <+> text "=", y])) (hsep [x, text "=", y]))

arrow :: (Doc a, Doc a) -> Doc a
arrow (x, y) = group (flatAlt (hang 2 (sep [x <+> text "->", y])) (hsep [x, text "->", y]))

ppSexp :: Text -> [PDoc] -> PDoc
ppSexp con fields = ppSexp' (text con) fields

ppSexp' :: PDoc -> [PDoc] -> PDoc
ppSexp' con fields =
  group $
    flatAlt
      (hang 2 (encloseSep lparen rparen space docs))
      (encloseSep lparen rparen space docs)
  where
    docs = if isEmpty con then fields else (con : fields)

ppRecord :: Text -> [(Text, PDoc)] -> PDoc
ppRecord con fields = ppRecord' (text con) fields

ppRecord' :: PDoc -> [(Text, PDoc)] -> PDoc
ppRecord' con fields =
  group $
    flatAlt
      (hang 1 (vcat [con, puncLeft lbrace (map (\(x, y) -> equate (text x) y) fields) comma rbrace]))
      (con <> (encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) (map (\(x, y) -> equate (text x) y) fields)))

puncLeft :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann -> Doc ann
puncLeft open [] _ close = hsep [open, close]
puncLeft open [x] _ close = hsep [open, x, close]
puncLeft open (x : xs) coma close = align (sep ((open <+> x) : help xs))
  where
    help [] = mempty
    help [y] = [hsep [coma, y, close]]
    help (y : ys) = ((coma <+> y) : help ys)

ppSet :: (x -> Doc ann) -> Set x -> Doc ann
ppSet p xs = encloseSep lbrace rbrace comma (map p (toList xs))

ppList :: (x -> Doc ann) -> [x] -> Doc ann
ppList p xs =
  group $
    flatAlt
      (puncLeft lbracket (map p xs) comma rbracket)
      (encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (map p xs))

ppStrictSeq :: (a -> Doc ann) -> StrictSeq a -> Doc ann
ppStrictSeq p xs = ppList p (foldr (:) [] xs)

ppStrictMaybe :: (x -> Doc ann) -> StrictMaybe x -> Doc ann
ppStrictMaybe _ SNothing = text "?-"
ppStrictMaybe p (SJust x) = text "?" <> p x

ppMaybe :: (x -> Doc ann) -> Maybe x -> Doc ann
ppMaybe _ Nothing = text "?-"
ppMaybe p (Just x) = text "?" <> p x

ppMap' :: PDoc -> (k -> PDoc) -> (v -> PDoc) -> Map.Map k v -> PDoc
ppMap' name kf vf m =
  let docs = fmap (\(k, v) -> arrow (kf k, vf v)) (Map.toList m)
      vertical =
        if isEmpty name
          then (hang 1 (puncLeft lbrace docs comma rbrace))
          else (hang 1 (vcat [name, puncLeft lbrace docs comma rbrace]))
   in group $
        flatAlt
          vertical
          (name <> (encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) docs))

ppMap :: (k -> PDoc) -> (v -> PDoc) -> Map.Map k v -> PDoc
ppMap kf vf m = ppMap' (text "Map") kf vf m

class PrettyA t where
  prettyA :: t -> PDoc

-- =====================================================================================================
-- END HELPER FUNCTIONS
-- ================================= ====================================================================

-- =================================
-- Shelley.Spec.Ledger.LedgerState

ppAccountState :: AccountState -> PDoc
ppAccountState (AccountState tr re) =
  ppRecord
    "AccountState"
    [ ("treasury", ppCoin tr),
      ("reserves", ppCoin re)
    ]

ppDPState :: DPState crypto -> PDoc
ppDPState (DPState d p) = ppRecord "DPState" [("dstate", ppDState d), ("pstate", ppPState p)]

ppDState :: DState crypto -> PDoc
ppDState (DState r ds ptrs future gen irwd) =
  ppRecord
    "DState"
    [ ("rewards", ppRewardAccounts r),
      ("delegations", ppMap' mempty ppCredential ppKeyHash ds),
      ("ptrs", ppMap ppPtr ppCredential (forwards ptrs)),
      ("futuregendelegs", ppMap ppFutureGenDeleg ppGenDelegPair future),
      ("gendelegs", ppGenDelegs gen),
      ("instantaeousrewards", ppInstantaneousRewards irwd)
    ]

ppFutureGenDeleg :: FutureGenDeleg crypto -> PDoc
ppFutureGenDeleg (FutureGenDeleg sl kh) =
  ppRecord
    "FutureGenDeleg"
    [ ("delegSlot", ppSlotNo sl),
      ("keyhash", ppKeyHash kh)
    ]

ppInstantaneousRewards :: InstantaneousRewards crypto -> PDoc
ppInstantaneousRewards (InstantaneousRewards res treas) =
  ppRecord
    "InstantaneousRewards"
    [ ("reserves", ppMap' mempty ppCredential ppCoin res),
      ("treasury", ppMap' mempty ppCredential ppCoin treas)
    ]

ppIx :: Ix -> PDoc
ppIx x = viaShow x

ppPPUPState :: PPUPState era -> PDoc
ppPPUPState (PPUPState p fp) =
  ppRecord
    "Proposed PPUPState"
    [ ("proposals", ppProposedPPUpdates p),
      ("futureProposals", ppProposedPPUpdates fp)
    ]

ppPState :: PState crypto -> PDoc
ppPState (PState par fpar ret) =
  ppRecord
    "PState"
    [ ("poolparams", ppMap' mempty ppKeyHash ppPoolParams par),
      ("futurepoolparams", ppMap' mempty ppKeyHash ppPoolParams fpar),
      ("retiring", ppMap' mempty ppKeyHash ppEpochNo ret)
    ]

ppRewardAccounts :: Map.Map (Credential 'Staking crypto) Coin -> PDoc
ppRewardAccounts m = ppMap' (text "RewardAccounts") ppCredential ppCoin m

ppRewardUpdate :: RewardUpdate crypto -> PDoc
ppRewardUpdate (RewardUpdate dt dr rss df nonmyop) =
  ppRecord
    "RewardUpdate"
    [ ("deltaT", ppDeltaCoin dt),
      ("deltaR", ppDeltaCoin dr),
      ("rs", ppMap' mempty ppCredential ppCoin rss),
      ("deltaF", ppDeltaCoin df),
      ("nonMyopic", ppNonMyopic nonmyop)
    ]

ppUTxOState ::
  ( Era era,
    PrettyA (Core.Value era),
    Compactible (Core.Value era)
  ) =>
  UTxOState era ->
  PDoc
ppUTxOState (UTxOState u dep fee ppup) =
  ppRecord
    "UTxOState"
    [ ("utxo", ppUTxO u),
      ("deposited", ppCoin dep),
      ("fees", ppCoin fee),
      ("ppups", ppPPUPState ppup)
    ]

ppEpochState :: (Era era, PrettyA (Core.Value era), Compactible (Core.Value era)) => EpochState era -> PDoc
ppEpochState (EpochState acnt snap ls prev pp non) =
  ppRecord
    "EpochState"
    [ ("accountState", ppAccountState acnt),
      ("snapShots", ppSnapShots snap),
      ("ledgerState", ppLedgerState ls),
      ("prevPoolParams", ppPParams prev),
      ("pooParams", ppPParams pp),
      ("nonMyopic", ppNonMyopic non)
    ]

ppLedgerState :: (Era era, PrettyA (Core.Value era), Compactible (Core.Value era)) => LedgerState era -> PDoc
ppLedgerState (LedgerState u d) =
  ppRecord
    "LedgerState"
    [ ("utxoState", ppUTxOState u),
      ("delegationState", ppDPState d)
    ]

instance PrettyA (AccountState) where prettyA = ppAccountState

instance PrettyA (DPState crypto) where prettyA = ppDPState

instance PrettyA (DState crypto) where prettyA = ppDState

instance
  (Era era, PrettyA (Core.Value era), Compactible (Core.Value era)) =>
  PrettyA (EpochState era)
  where
  prettyA = ppEpochState

instance PrettyA (FutureGenDeleg crypto) where prettyA = ppFutureGenDeleg

instance PrettyA (InstantaneousRewards crypto) where prettyA = ppInstantaneousRewards

instance
  (Era era, PrettyA (Core.Value era), Compactible (Core.Value era)) =>
  PrettyA (LedgerState era)
  where
  prettyA = ppLedgerState

instance PrettyA (PPUPState era) where prettyA = ppPPUPState

instance PrettyA (PState crypto) where prettyA = ppPState

instance PrettyA (RewardUpdate crypto) where prettyA = ppRewardUpdate

instance
  (Era era, PrettyA (Core.Value era), Compactible (Core.Value era)) =>
  PrettyA (UTxOState era)
  where
  prettyA = ppUTxOState

-- =================================
-- Shelley.Spec.Ledger.Rewards

ppPerformanceEstimate :: PerformanceEstimate -> PDoc
ppPerformanceEstimate (PerformanceEstimate n) = ppSexp "PerformanceEstimate" [viaShow n]

ppNonMyopic :: NonMyopic crypto -> PDoc
ppNonMyopic (NonMyopic m c) =
  ppRecord
    "NonMyopic"
    [ ("likelihood", ppMap' "" ppKeyHash ppLikelihood m),
      ("rewardPot", ppCoin c)
    ]

ppStakeShare :: StakeShare -> PDoc
ppStakeShare (StakeShare n) = ppSexp "StakeShare" [viaShow n]

ppHistogram :: Histogram -> PDoc
ppHistogram (Histogram ss) = ppSexp "Histogram" [ppStrictSeq ppLogWeight ss]

ppLogWeight :: LogWeight -> PDoc
ppLogWeight (LogWeight n) = ppSexp "LogWeight" [viaShow n]

ppLikelihood :: Likelihood -> PDoc
ppLikelihood (Likelihood ns) = ppSexp "Likelihood" [ppStrictSeq ppLogWeight ns]

instance PrettyA PerformanceEstimate where prettyA = ppPerformanceEstimate

instance PrettyA StakeShare where prettyA = ppStakeShare

instance PrettyA Histogram where prettyA = ppHistogram

instance PrettyA LogWeight where prettyA = ppLogWeight

instance PrettyA Likelihood where prettyA = ppLikelihood

-- =================================
-- Shelley.Spec.Ledger.EpochBoundary

ppStake :: Stake crypto -> PDoc
ppStake (Stake m) = ppMap' (text "Stake") ppCredential ppCoin m

ppBlocksMade :: BlocksMade crypto -> PDoc
ppBlocksMade (BlocksMade m) = ppMap' (text "BlocksMade") ppKeyHash viaShow m

ppSnapShot :: SnapShot crypto -> PDoc
ppSnapShot (SnapShot st deleg params) =
  ppRecord
    "SnapShot"
    [ ("stake", ppStake st),
      ("delegations", ppMap ppCredential ppKeyHash deleg),
      ("poolParams", ppMap ppKeyHash ppPoolParams params)
    ]

ppSnapShots :: SnapShots crypto -> PDoc
ppSnapShots (SnapShots mark set go fees) =
  ppRecord
    "SnapShots"
    [ ("pstakeMark", ppSnapShot mark),
      ("pstakeSet", ppSnapShot set),
      ("pstakeGo", ppSnapShot go),
      ("fee", ppCoin fees)
    ]

instance PrettyA (Stake crypto) where prettyA = ppStake

instance PrettyA (BlocksMade crypto) where prettyA = ppBlocksMade

instance PrettyA (SnapShot crypto) where prettyA = ppSnapShot

instance PrettyA (SnapShots crypto) where prettyA = ppSnapShots

-- ============================
-- Shelley.Spec.Ledger.UTxO

ppUTxO ::
  ( Era era,
    PrettyA (Core.Value era),
    Compactible (Core.Value era)
  ) =>
  UTxO era ->
  PDoc
ppUTxO (UTxO m) = ppMap' (text "UTxO") ppTxIn ppTxOut m

instance
  ( Era era,
    PrettyA (Core.Value era),
    Compactible (Core.Value era)
  ) =>
  PrettyA (UTxO era)
  where
  prettyA = ppUTxO

-- ============================
-- Sheley.Spec.Ledger.Metadata

ppMetadatum :: Metadatum -> PDoc
ppMetadatum (Map m) =
  let pairs = fmap (\(k, v) -> arrow (ppMetadatum k, ppMetadatum v)) m
   in ppSexp
        "Map"
        [ group $
            flatAlt
              (hang 1 (puncLeft lbrace pairs comma rbrace))
              (encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) pairs)
        ]
ppMetadatum (List ds) = ppSexp "List" [ppList ppMetadatum ds]
ppMetadatum (I n) = ppSexp "I" [viaShow n]
ppMetadatum (B bs) = ppSexp "B" [viaShow bs]
ppMetadatum (S txt) = ppSexp "S" [text txt]

ppMetadata :: Metadata -> PDoc
ppMetadata (Metadata m) = ppMap' (text "Metadata") viaShow ppMetadatum m

instance PrettyA Metadatum where prettyA = ppMetadatum

instance PrettyA Metadata where prettyA = ppMetadata

-- ============================
-- Shelley.Spec.Ledger.Tx

ppTx ::
  ( PrettyA (Core.TxBody era),
    PrettyA (Core.AuxiliaryData era),
    PrettyA (Core.Script era),
    Era era
  ) =>
  Tx era ->
  PDoc
ppTx (Tx' body witset meta _) =
  ppRecord
    "Tx"
    [ ("body", prettyA body),
      ("witnessSet", ppWitnessSetHKD witset),
      ("metadata", ppStrictMaybe prettyA meta)
    ]

ppBootstrapWitness :: Crypto crypto => BootstrapWitness crypto -> PDoc
ppBootstrapWitness (BootstrapWitness key sig (ChainCode code) attr) =
  ppRecord
    "BootstrapWitness"
    [ ("key", ppVKey key),
      ("signature", viaShow sig),
      ("chaincode", viaShow code),
      ("attributes", viaShow attr)
    ]

ppWitnessSetHKD :: (Era era, PrettyA (Core.Script era)) => WitnessSetHKD Identity era -> PDoc
ppWitnessSetHKD x =
  let (addr, scr, boot) = prettyWitnessSetParts x
   in ppRecord
        "WitnessSet"
        [ ("addrWits", ppSet ppWitVKey addr),
          ("scriptWits", ppMap ppScriptHash prettyA scr),
          ("bootWits", ppSet ppBootstrapWitness boot)
        ]

instance
  ( PrettyA (Core.TxBody era),
    PrettyA (Core.AuxiliaryData era),
    PrettyA (Core.Script era),
    Era era
  ) =>
  PrettyA (Tx era)
  where
  prettyA = ppTx

instance Crypto crypto => PrettyA (BootstrapWitness crypto) where prettyA = ppBootstrapWitness

instance (Era era, PrettyA (Core.Script era)) => PrettyA (WitnessSetHKD Identity era) where prettyA = ppWitnessSetHKD

-- ============================
--  Cardano.Ledger.AuxiliaryData

ppAuxiliaryDataHash :: AuxiliaryDataHash c -> PDoc
ppAuxiliaryDataHash (AuxiliaryDataHash h) = ppSexp "AuxiliaryDataHash" [ppHash h]

instance PrettyA (AuxiliaryDataHash c) where prettyA = ppAuxiliaryDataHash

-- ============================
--  Cardano.Ledger.Compactible

ppCompactForm :: (Compactible a) => (a -> PDoc) -> CompactForm a -> PDoc
ppCompactForm cf x = cf (fromCompact x)

instance (Compactible a, PrettyA a) => PrettyA (CompactForm a) where prettyA = ppCompactForm prettyA

-- ============================
-- Shelley.Spec.Ledger.TxBody

ppDelegation :: Delegation c -> PDoc
ppDelegation (Delegation orx ee) = ppRecord "Delegation" [("delegator", ppCredential orx), ("delegatee", ppKeyHash ee)]

ppPoolMetadata :: PoolMetadata -> PDoc
ppPoolMetadata (PoolMetadata url hsh) =
  ppRecord
    "PoolMetadata"
    [ ("url", ppUrl url),
      ("hash", (text "#" <> reAnnotate (Width 5 :) (viaShow hsh)))
    ]

ppStakePoolRelay :: StakePoolRelay -> PDoc
ppStakePoolRelay (SingleHostAddr port ip4 ip6) = ppSexp "SingleHostAddr" [ppStrictMaybe ppPort port, ppStrictMaybe ppIPv4 ip4, ppStrictMaybe ppIPv6 ip6]
ppStakePoolRelay (SingleHostName port dns) = ppSexp "SingleHostName" [ppStrictMaybe ppPort port, ppDnsName dns]
ppStakePoolRelay (MultiHostName dns) = ppSexp "MultiHostName" [ppDnsName dns]

ppPoolParams :: PoolParams c -> PDoc
ppPoolParams (PoolParams idx vrf pledge cost margin acnt owners relays md) =
  ppRecord
    "PoolParams"
    [ ("Id", ppKeyHash idx),
      ("Vrf", ppHash vrf),
      ("Pledge", ppCoin pledge),
      ("Cost", ppCoin cost),
      ("Margin", ppUnitInterval margin),
      ("RAcnt", ppRewardAcnt acnt),
      ("Owners", ppSet ppKeyHash owners),
      ("Relays", ppStrictSeq ppStakePoolRelay relays),
      ("Metadata", ppStrictMaybe ppPoolMetadata md)
    ]

ppWdrl :: Wdrl c -> PDoc
ppWdrl (Wdrl m) = ppSexp "" [ppMap' (text "Wdr") ppRewardAcnt ppCoin m]

ppTxId :: TxId c -> PDoc
ppTxId (TxId x) = ppSexp "TxId" [ppHash x]

ppTxIn :: TxIn c -> PDoc
ppTxIn (TxInCompact txid word) = ppSexp "TxIn" [ppTxId txid, viaShow word]

ppTxOut :: (Era era, PrettyA (Core.Value era), Compactible (Core.Value era)) => TxOut era -> PDoc
ppTxOut (TxOutCompact caddr cval) = ppSexp "TxOut" [ppCompactAddr caddr, ppCompactForm prettyA cval]

ppDelegCert :: DelegCert c -> PDoc
ppDelegCert (RegKey x) = ppSexp "RegKey" [ppCredential x]
ppDelegCert (DeRegKey x) = ppSexp "DeRegKey" [ppCredential x]
ppDelegCert (Delegate x) = ppSexp "Delegate" [ppDelegation x]

ppPoolCert :: PoolCert c -> PDoc
ppPoolCert (RegPool x) = ppSexp "RegPool" [ppPoolParams x]
ppPoolCert (RetirePool x y) = ppSexp "RetirePool" [ppKeyHash x, ppEpochNo y]

ppGenesisDelegCert :: GenesisDelegCert c -> PDoc
ppGenesisDelegCert (GenesisDelegCert a b c) = ppSexp "GenesisDelgCert" [ppKeyHash a, ppKeyHash b, ppHash c]

ppMIRPot :: MIRPot -> PDoc
ppMIRPot x = viaShow x

ppMIRCert :: MIRCert c -> PDoc
ppMIRCert (MIRCert pot rew) = ppSexp "MirCert" [ppMIRPot pot, ppMap ppCredential ppCoin rew]

ppDCert :: DCert c -> PDoc
ppDCert (DCertDeleg x) = ppSexp "DCertDeleg" [ppDelegCert x]
ppDCert (DCertPool x) = ppSexp "DCertPool" [ppPoolCert x]
ppDCert (DCertGenesis x) = ppSexp "DCertGenesis" [ppGenesisDelegCert x]
ppDCert (DCertMir x) = ppSexp "DCertMir" [ppMIRCert x]

ppTxBody ::
  ( Era era,
    ToCBOR (Core.Value era),
    PrettyA (Core.Value era),
    ToCBOR (CompactForm (Core.Value era)),
    Compactible (Core.Value era)
  ) =>
  TxBody era ->
  PDoc
ppTxBody (TxBody ins outs cs wdrls fee ttl upd mdh) =
  ppRecord
    "TxBody"
    [ ("inputs", ppSet ppTxIn ins),
      ("outputs", ppStrictSeq ppTxOut outs),
      ("cert", ppStrictSeq ppDCert cs),
      ("wdrls", ppWdrl wdrls),
      ("fee", ppCoin fee),
      ("timetolive", ppSlotNo ttl),
      ("update", ppStrictMaybe ppUpdate upd),
      ("metadatahash", ppStrictMaybe ppAuxiliaryDataHash mdh)
    ]

ppWitVKey :: (Typeable kr, Crypto c) => WitVKey kr c -> PDoc
ppWitVKey (WitVKey key sig) = ppRecord "WitVKey" [("key", ppVKey key), ("signature", viaShow sig)]

ppStakeCreds :: StakeCreds c -> PDoc
ppStakeCreds (StakeCreds m) = ppSexp "" [ppMap' (text "StakeCreds") ppCredential ppSlotNo m]

instance PrettyA (Delegation c) where prettyA = ppDelegation

instance PrettyA PoolMetadata where prettyA = ppPoolMetadata

instance PrettyA StakePoolRelay where prettyA = ppStakePoolRelay

instance PrettyA (PoolParams c) where prettyA = ppPoolParams

instance PrettyA (Wdrl c) where prettyA = ppWdrl

instance PrettyA (TxId c) where prettyA = ppTxId

instance PrettyA (TxIn c) where prettyA = ppTxIn

instance (Era era, PrettyA (Core.Value era), Compactible (Core.Value era)) => PrettyA (TxOut era) where prettyA = ppTxOut

instance PrettyA (DelegCert c) where prettyA = ppDelegCert

instance PrettyA (PoolCert c) where prettyA = ppPoolCert

instance PrettyA (GenesisDelegCert c) where prettyA = ppGenesisDelegCert

instance PrettyA MIRPot where prettyA = ppMIRPot

instance PrettyA (MIRCert c) where prettyA = ppMIRCert

instance PrettyA (DCert c) where prettyA = ppDCert

instance
  ( Era era,
    ToCBOR (Core.Value era),
    PrettyA (Core.Value era),
    ToCBOR (CompactForm (Core.Value era)),
    Compactible (Core.Value era)
  ) =>
  PrettyA (TxBody era)
  where
  prettyA = ppTxBody

instance (Typeable kr, Crypto c) => PrettyA (WitVKey kr c) where prettyA = ppWitVKey

instance Crypto c => PrettyA (StakeCreds c) where prettyA = ppStakeCreds

-- ===========================================
-- Data.IP
ppIPv4 :: IPv4 -> PDoc
ppIPv4 x = viaShow x

ppIPv6 :: IPv6 -> PDoc
ppIPv6 x = viaShow x

instance PrettyA IPv4 where prettyA = ppIPv4

instance PrettyA IPv6 where prettyA = ppIPv6

-- ====================================================
-- Shelley.Spec.Ledger.CompactAddr

ppCompactAddr :: Crypto c => CompactAddr c -> PDoc
ppCompactAddr x = ppAddr (decompactAddr x)

instance Crypto c => PrettyA (CompactAddr c) where prettyA = ppCompactAddr

-- ================================================
-- Shelley.Spec.Ledger.PParams

ppProtVer :: ProtVer -> PDoc
ppProtVer (ProtVer maj mi) = ppRecord "Version" [("major", viaShow maj), ("minor", viaShow mi)]

ppPParams :: PParams' Identity era -> PDoc
ppPParams (PParams feeA feeB mbb mtx mbh kd pd em no a0 rho tau d ex pv mutxo mpool) =
  ppRecord
    "PParams"
    [ ("minfeeA", viaShow feeA),
      ("minfeeB", viaShow feeB),
      ("maxBBSize", viaShow mbb),
      ("maxTxSize", viaShow mtx),
      ("maxBHSize", viaShow mbh),
      ("keyDeposit", ppCoin kd),
      ("poolDeposit", ppCoin pd),
      ("eMax", ppEpochNo em),
      ("nOpt", viaShow no),
      ("a0", viaShow a0),
      ("rho", ppUnitInterval rho),
      ("tau", ppUnitInterval tau),
      ("d", ppUnitInterval d),
      ("extraEntropy", ppNonce ex),
      ("protocolVersion", ppProtVer pv),
      ("minUTxOValue", ppCoin mutxo),
      ("minPoolCost", ppCoin mpool)
    ]

ppPParamsUpdate :: PParams' StrictMaybe era -> PDoc
ppPParamsUpdate (PParams feeA feeB mbb mtx mbh kd pd em no a0 rho tau d ex pv mutxo mpool) =
  ppRecord
    "PParams"
    [ ("minfeeA", lift viaShow feeA),
      ("minfeeB", lift viaShow feeB),
      ("maxBBSize", lift viaShow mbb),
      ("maxTxSize", lift viaShow mtx),
      ("maxBHSize", lift viaShow mbh),
      ("keyDeposit", lift ppCoin kd),
      ("poolDeposit", lift ppCoin pd),
      ("eMax", lift ppEpochNo em),
      ("nOpt", lift viaShow no),
      ("a0", lift viaShow a0),
      ("rho", lift ppUnitInterval rho),
      ("tau", lift ppUnitInterval tau),
      ("d", lift ppUnitInterval d),
      ("extraEntropy", lift ppNonce ex),
      ("protocolVersion", lift ppProtVer pv),
      ("minUTxOValue", lift ppCoin mutxo),
      ("minPoolCost", lift ppCoin mpool)
    ]
  where
    lift pp x = ppStrictMaybe pp x

ppUpdate :: Update era -> PDoc
ppUpdate (Update prop epn) = ppSexp "Update" [ppProposedPPUpdates prop, ppEpochNo epn]

ppProposedPPUpdates :: ProposedPPUpdates era -> PDoc
ppProposedPPUpdates (ProposedPPUpdates m) = ppMap ppKeyHash ppPParamsUpdate m

ppPPUpdateEnv :: PPUpdateEnv c -> PDoc
ppPPUpdateEnv (PPUpdateEnv slot gd) = ppSexp "PPUpdateEnv" [ppSlotNo slot, ppGenDelegs gd]

instance PrettyA (PPUpdateEnv c) where prettyA = ppPPUpdateEnv

instance PrettyA (ProposedPPUpdates e) where prettyA = ppProposedPPUpdates

instance PrettyA (Update e) where prettyA = ppUpdate

instance PrettyA (PParams' StrictMaybe e) where prettyA = ppPParamsUpdate

instance PrettyA (PParams' Identity e) where prettyA = ppPParams

instance PrettyA ProtVer where prettyA = ppProtVer

-- ============================================
-- Cardano.Ledger.Coin
ppCoin :: Coin -> PDoc
ppCoin (Coin n) = ppSexp "Coin" [pretty n]

ppDeltaCoin :: DeltaCoin -> PDoc
ppDeltaCoin (DeltaCoin n) = ppSexp "DeltaCoin" [pretty n]

instance PrettyA Coin where prettyA = ppCoin

-- ===========================================
-- Cardano.Chain.Common

ppBootstrapAddress :: BootstrapAddress c -> PDoc
ppBootstrapAddress (BootstrapAddress (Address root (Attributes (AddrAttributes path magic) y) typ)) =
  ppRecord
    "BootstrapAddress"
    [ ("root", viaShow root),
      ("derivationpath", ppMaybe ppHDAddressPayload path),
      ("networkmagic", ppNetworkMagic magic),
      ("remain", ppUnparsedFields y),
      ("type", viaShow typ)
    ]

ppNetworkMagic :: NetworkMagic -> PDoc
ppNetworkMagic NetworkMainOrStage = text "MainOrStage"
ppNetworkMagic (NetworkTestnet n) = text "Testnet" <+> viaShow n

ppUnparsedFields :: UnparsedFields -> PDoc
ppUnparsedFields (UnparsedFields m) = ppMap' mempty viaShow viaShow m

instance PrettyA NetworkMagic where prettyA = ppNetworkMagic

instance PrettyA (BootstrapAddress c) where prettyA = ppBootstrapAddress

instance PrettyA UnparsedFields where prettyA = ppUnparsedFields

-- ===========================================
-- Shelley.Spec.Ledger.Address

ppAddr :: Addr c -> PDoc
ppAddr (Addr net cred ref) = ppSexp "Addr" [ppNetwork net, ppCredential cred, ppStakeReference ref]
ppAddr (AddrBootstrap x) = ppSexp' mempty [ppBootstrapAddress x]

ppHDAddressPayload :: HDAddressPayload -> PDoc
ppHDAddressPayload (HDAddressPayload x) = viaShow x

ppRewardAcnt :: RewardAcnt c -> PDoc
ppRewardAcnt (RewardAcnt net cred) = ppRecord "RewardAcnt" [("network", ppNetwork net), ("credential", ppCredential cred)]

instance PrettyA (Addr c) where prettyA = ppAddr

instance PrettyA (RewardAcnt c) where prettyA = ppRewardAcnt

-- ===========================================
-- Shelley.Spec.Ledger.Credential

ppCredential :: Credential keyrole c -> PDoc
ppCredential (ScriptHashObj (ScriptHash x)) = ppSexp "ScriptCred" [ppHash x]
ppCredential (KeyHashObj (KeyHash x)) = ppSexp "KeyCred" [ppHash x]

ppPtr :: Ptr -> PDoc
ppPtr (Ptr slot n m) = ppSexp "Ptr" [ppSlotNo slot, pretty n, pretty m]

ppStakeReference :: StakeReference c -> PDoc
ppStakeReference (StakeRefBase x) = ppSexp "BaseRef" [ppCredential x]
ppStakeReference (StakeRefPtr x) = ppSexp "PtrRef" [ppPtr x]
ppStakeReference (StakeRefNull) = text "NullRef"

ppGenesisCredential :: GenesisCredential c -> PDoc
ppGenesisCredential (GenesisCredential (KeyHash x)) = ppSexp "GenesisCredential" [ppHash x]

instance PrettyA (Credential r c) where prettyA = ppCredential

instance PrettyA (StakeReference c) where prettyA = ppStakeReference

instance PrettyA (GenesisCredential c) where prettyA = ppGenesisCredential

instance PrettyA Ptr where prettyA = ppPtr

-- ===========================================
-- Shelley.Spec.Ledger.Scripts

ppMultiSig :: Crypto crypto => MultiSig crypto -> PDoc
ppMultiSig (RequireSignature hk) = ppSexp "Require" [ppKeyHash hk]
ppMultiSig (RequireAllOf ps) = ppSexp "AllOf" (map ppMultiSig ps)
ppMultiSig (RequireAnyOf ps) = ppSexp "AnyOf" (map ppMultiSig ps)
ppMultiSig (RequireMOf m ps) = ppSexp "MOf" (pretty m : map ppMultiSig ps)

ppScriptHash :: ScriptHash crypto -> PDoc
ppScriptHash (ScriptHash h) = ppSexp "ScriptHash" [ppHash h]

instance PrettyA (ScriptHash c) where prettyA = ppScriptHash

instance Crypto c => PrettyA (MultiSig c) where prettyA = ppMultiSig

-- ====================================================
-- Shelley.Spec.Ledger.Slot

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

instance PrettyA SlotNo where prettyA = ppSlotNo

instance PrettyA Duration where prettyA = ppDuration

instance PrettyA EpochNo where prettyA = ppEpochNo

instance PrettyA EpochSize where prettyA = ppEpochSize

instance PrettyA BlockNo where prettyA = ppBlockNo

-- ===================================================
-- Shelley.Spec.Ledger.OCert

ppKESPeriod :: KESPeriod -> PDoc
ppKESPeriod (KESPeriod x) = text "KESPeriod" <+> pretty x

ppOCertEnv :: OCertEnv crypto -> PDoc
ppOCertEnv (OCertEnv ps ds) =
  ppRecord
    "OCertEnv"
    [ ("ocertEnvStPools", ppSet ppKeyHash ps),
      ("ocertEnvGenDelegs", ppSet ppKeyHash ds)
    ]

ppOCert :: forall crypto. Crypto crypto => OCert crypto -> PDoc
ppOCert (OCert vk n per sig) =
  ppRecord
    "OCert"
    [ ("ocertVkKot", ppVerKeyKES (Proxy @crypto) vk),
      ("ocertN", pretty n),
      ("ocertKESPeriod", ppKESPeriod per),
      ("ocertSigma", reAnnotate (Width 5 :) (viaShow sig))
    ]

ppOCertSignable :: forall crypto. Crypto crypto => OCertSignable crypto -> PDoc
ppOCertSignable (OCertSignable verkes w per) = ppSexp "OCertSignable" [ppVerKeyKES (Proxy @crypto) verkes, pretty w, ppKESPeriod per]

instance PrettyA KESPeriod where prettyA = ppKESPeriod

instance PrettyA (OCertEnv c) where prettyA = ppOCertEnv

instance Crypto c => PrettyA (OCert c) where prettyA = ppOCert

instance Crypto c => PrettyA (OCertSignable c) where prettyA = ppOCertSignable

-- ==========================================
--  Cardano.Crypto.Hash

ppHash :: Hash.Hash a b -> PDoc
ppHash x = text "#" <> reAnnotate (Width 5 :) (viaShow x)

instance PrettyA (Hash.Hash a b) where prettyA = ppHash

-- ==========================================
-- Shelley.Spec.Ledger.BaseTypes

ppUnitInterval :: UnitInterval -> PDoc
ppUnitInterval z = viaShow z

ppNonce :: Nonce -> PDoc
ppNonce (Nonce h) = text "Nonce" <+> ppHash h
ppNonce (NeutralNonce) = text "NeutralNonce"

ppActiveSlotCoeff :: ActiveSlotCoeff -> PDoc
ppActiveSlotCoeff x =
  ppRecord
    "ActiveSlotCoeff"
    [ ("activeSlotVal", ppUnitInterval (activeSlotVal x)),
      ("ActiveSlotLog", viaShow (activeSlotLog x))
    ]

ppGlobals :: Globals -> PDoc
ppGlobals (Globals _e slot stab ran sec maxkes quor maxmaj maxlove active net) =
  ppRecord
    "Globals"
    [ ("epochInfo", text "?"),
      ("slotsPerKESPeriod", pretty slot),
      ("stabilityWindow", pretty stab),
      ("randomnessStabilisationWindow", pretty ran),
      ("securityParameter", pretty sec),
      ("maxKESEvo", pretty maxkes),
      ("quorum", pretty quor),
      ("maxMajorPV", pretty maxmaj),
      ("maxLovelaceSupply", pretty maxlove),
      ("activeSlotCoeff", ppActiveSlotCoeff active),
      ("networkId", ppNetwork net)
    ]

ppNetwork :: Network -> PDoc
ppNetwork Testnet = text "Testnet"
ppNetwork Mainnet = text "Mainnet"

ppUrl :: Url -> PDoc
ppUrl x = viaShow x

ppPort :: Port -> PDoc
ppPort (Port n) = ppSexp "Port" [viaShow n]

ppDnsName :: DnsName -> PDoc
ppDnsName x = ppSexp "DnsName" [text (dnsToText x)]

instance PrettyA Url where prettyA = ppUrl

instance PrettyA Network where prettyA = ppNetwork

instance PrettyA Globals where prettyA = ppGlobals

instance PrettyA ActiveSlotCoeff where prettyA = ppActiveSlotCoeff

instance PrettyA Nonce where prettyA = ppNonce

instance PrettyA UnitInterval where prettyA = ppUnitInterval

instance PrettyA Port where prettyA = ppPort

instance PrettyA DnsName where prettyA = ppDnsName

-- ===========================================
-- Shelley.Spec.Ledger.Keys

ppVKey :: Crypto c => (VKey r c) -> PDoc
ppVKey (VKey x) = reAnnotate (Width 5 :) (viaShow x)

ppKeyPair :: Crypto c => (KeyPair r c) -> PDoc
ppKeyPair (KeyPair x y) =
  ppRecord "KeyPair" [("vKey", ppVKey x), ("sKey", reAnnotate (Width 5 :) (viaShow y))]

ppKeyHash :: KeyHash x c -> PDoc
ppKeyHash (KeyHash x) = text "KeyHash" <+> ppHash x

ppGenDelegPair :: GenDelegPair c -> PDoc
ppGenDelegPair (GenDelegPair (KeyHash x) y) =
  ppRecord "GenDelegPair" [("KeyHash", ppHash x), ("VrfHash", ppHash y)]

ppGKeys :: Crypto c => GKeys c -> PDoc
ppGKeys (GKeys x) = ppSexp "GKeys" [ppSet ppVKey x]

ppVerKeyKES :: forall crypto. Crypto crypto => Proxy crypto -> VerKeyKES crypto -> PDoc
ppVerKeyKES Proxy x = reAnnotate (Width 5 :) (viaShow x)

ppGenDelegs :: GenDelegs c -> PDoc
ppGenDelegs (GenDelegs m) = ppSexp "GenDelegs" [ppMap ppKeyHash ppGenDelegPair m]

instance Crypto c => PrettyA (VKey r c) where prettyA = ppVKey

instance Crypto c => PrettyA (KeyPair r c) where prettyA = ppKeyPair

instance PrettyA (KeyHash x c) where prettyA = ppKeyHash

instance Crypto c => PrettyA (GKeys c) where prettyA = ppGKeys

instance PrettyA (GenDelegPair c) where prettyA = ppGenDelegPair

instance PrettyA (GenDelegs c) where prettyA = ppGenDelegs

-- ======================================================

main :: Int -> IO ()
main n = do
  putDocW n (ppSexp "BIGname" ([(pretty True), (pretty [1 :: Int, 3, 5]), (pretty ("agh" :: String))]))
  putStrLn ""

main1 :: Int -> IO ()
main1 n = do
  putDocW n (ppRecord "BIGname" ([("abc", pretty True), ("ljklkjfdjlfd", pretty [1 :: Int, 3, 5]), ("last", pretty ("agh" :: String))]))
  putStrLn ""

main2 :: Int -> IO ()
main2 n = do
  putDocW n (ppMap viaShow viaShow (Map.fromList [(x, x) | x <- [1 .. (6 :: Int)]]))
  putStrLn ""
