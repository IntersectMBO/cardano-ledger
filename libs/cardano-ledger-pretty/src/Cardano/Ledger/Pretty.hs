{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Due to usage of Delegation
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Ledger.Pretty where

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
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  BlocksMade (..),
  BoundedRational (..),
  DnsName,
  FixedPoint,
  Globals (..),
  Network (..),
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
import Cardano.Ledger.CertState (VState (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (
  Credential (KeyHashObj, ScriptHashObj),
  GenesisCredential (..),
  Ptr (..),
  StakeReference (..),
 )
import Cardano.Ledger.Crypto (Crypto)
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
  KeyRole (Staking),
  VKey (..),
  VerKeyKES,
  hashKey,
 )
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness (..), ChainCode (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.MemoBytes (MemoBytes (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  CertState (..),
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
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
  PoolMetadata (..),
  PoolParams (..),
  ShelleyTxBody (..),
  ShelleyTxBodyRaw (..),
  ShelleyTxOut (..),
  StakePoolRelay (..),
  WitVKey (..),
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.TxCert (
  GenesisDelegCert (..),
  ShelleyDelegCert (..),
  ShelleyTxCert (..),
 )
import Cardano.Ledger.Shelley.TxWits (
  ShelleyTxWits,
  prettyWitnessSetParts,
 )
import Cardano.Ledger.Slot (
  BlockNo (..),
  Duration (..),
  EpochNo (..),
  EpochSize (..),
  SlotNo (..),
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap (
  RDPair (..),
  UMElem (UMElem),
  UMap (..),
 )
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
import qualified Data.Hashable as Hashable
import Data.IP (IPv4, IPv6)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio, denominator, numerator)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set, toList)
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.VMap as VMap
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import Debug.Trace (trace)
import GHC.Natural (Natural)
import Lens.Micro ((^.))
import Prettyprinter
import Prettyprinter.Internal (Doc (Empty))
import Prettyprinter.Util (putDocW)

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

ppPair :: (t1 -> PDoc) -> (t2 -> PDoc) -> (t1, t2) -> PDoc
ppPair pp1 pp2 (x, y) = ppSexp' mempty [pp1 x, pp2 y]

-- ppSignedDSIGN :: SignedDSIGN a b -> Doc ann
ppSignedDSIGN :: Show a => a -> PDoc
ppSignedDSIGN x = reAnnotate (Width 5 :) (viaShow x)

ppBool :: Bool -> Doc a
ppBool = viaShow

ppInt :: Int -> Doc a
ppInt = viaShow

-- =========================
-- operations for pretty printing

isEmpty :: Doc ann -> Bool
isEmpty Empty = True
isEmpty _ = False

putDoc :: Doc ann -> IO ()
putDoc = putDocW 80

newtype PrettyAnn = Width Int

type Ann = [PrettyAnn]

type PDoc = Doc Ann

text :: Text -> Doc ann
text = pretty

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

instance PrettyA Long.ByteString where
  prettyA = ppLong

instance PrettyA Lazy.ByteString where
  prettyA = ppLazy

-- ================================
-- Combinators for common patterns of layout

-- | x == y
equate :: Doc a -> Doc a -> Doc a
equate x y = group (flatAlt (hang 2 (sep [x <+> text "=", y])) (hsep [x, text "=", y]))

-- | x -> y
arrow :: (Doc a, Doc a) -> Doc a
arrow (x, y) = group (flatAlt (hang 2 (sep [x <+> text "->", y])) (hsep [x, text "->", y]))

-- | ppSexp x [w,y,z] --> (x w y z)
ppSexp :: Text -> [PDoc] -> PDoc
ppSexp con = ppSexp' (text con)

ppSexp' :: PDoc -> [PDoc] -> PDoc
ppSexp' con fields =
  group $
    flatAlt
      (hang 2 (encloseSep lparen rparen space docs))
      (encloseSep lparen rparen space docs)
  where
    docs = if isEmpty con then fields else con : fields

-- | ppRecord name [("a",x),("b",y),("c",z)] --> name { a = x, b = y, c = z }
ppRecord :: Text -> [(Text, PDoc)] -> PDoc
ppRecord con = ppRecord' (text con)

ppRecord' :: PDoc -> [(Text, PDoc)] -> PDoc
ppRecord' con fields =
  group $
    flatAlt
      (hang 1 (vcat [con, puncLeft lbrace (map (\(x, y) -> equate (text x) y) fields) comma rbrace]))
      (con <> encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) (map (\(x, y) -> equate (text x) y) fields))

-- | Vertical layout with commas aligned on the left hand side
puncLeft :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann -> Doc ann
puncLeft open [] _ close = hsep [open, close]
puncLeft open [x] _ close = hsep [open, x, close]
puncLeft open (x : xs) coma close = align (sep ((open <+> x) : help xs))
  where
    help [] = mempty
    help [y] = [hsep [coma, y, close]]
    help (y : ys) = (coma <+> y) : help ys

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

ppAssocList :: PDoc -> (k -> PDoc) -> (v -> PDoc) -> [(k, v)] -> PDoc
ppAssocList name kf vf xs =
  let docs = fmap (\(k, v) -> arrow (kf k, vf v)) xs
      vertical =
        if isEmpty name
          then hang 1 (puncLeft lbrace docs comma rbrace)
          else hang 1 (vcat [name, puncLeft lbrace docs comma rbrace])
   in group $
        flatAlt
          vertical
          (name <> encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) docs)

ppMap' :: PDoc -> (k -> PDoc) -> (v -> PDoc) -> Map.Map k v -> PDoc
ppMap' name kf vf = ppAssocList name kf vf . Map.toList

ppMap :: (k -> PDoc) -> (v -> PDoc) -> Map.Map k v -> PDoc
ppMap = ppMap' (text "Map")

ppVMap ::
  (VMap.Vector kv k, VMap.Vector vv v) =>
  (k -> PDoc) ->
  (v -> PDoc) ->
  VMap.VMap kv vv k v ->
  PDoc
ppVMap pk pv = ppAssocList (text "VMap") pk pv . VMap.toList

class PrettyA t where
  prettyA :: t -> PDoc

instance PrettyA () where
  prettyA = ppString . show

instance PrettyA Void where
  prettyA = absurd

-- =============================================================================
-- END HELPER FUNCTIONS
-- =============================================================================

-- =====================================================
-- Data.UMap

ppRDPair :: RDPair -> PDoc
ppRDPair (RDPair rew dep) =
  ppRecord
    "RDPair"
    [ ("reward", ppCoin (fromCompact rew))
    , ("deposit", ppCoin (fromCompact dep))
    ]

ppUMElem :: UMElem c -> PDoc
ppUMElem (UMElem rd ptr spool drep) =
  ppSexp
    "UMElem"
    [ ppStrictMaybe ppRDPair rd
    , ppSet ppPtr ptr
    , ppStrictMaybe ppKeyHash spool
    , ppStrictMaybe prettyA drep
    ]

ppUnifiedMap :: UMap c -> PDoc
ppUnifiedMap UMap {..} =
  ppRecord
    "UMap"
    [ ("combined", ppMap ppCredential ppUMElem umElems)
    , ("ptrs", ppMap ppPtr ppCredential umPtrs)
    ]

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

instance PrettyA (LastAppliedBlock c) where
  prettyA = ppLastAppliedBlock

instance PrettyA (HashHeader c) where
  prettyA = ppHashHeader

instance PrettyA t => PrettyA (WithOrigin t) where
  prettyA = ppWithOrigin prettyA

ppBHBody :: Crypto c => BHBody c -> PDoc
ppBHBody (BHBody bn sn prev vk vrfvk eta l size hash ocert protver) =
  ppRecord
    "BHBody"
    [ ("BlockNo", ppBlockNo bn)
    , ("SlotNo", ppSlotNo sn)
    , ("Prev", ppPrevHash prev)
    , ("VKey", ppVKey vk)
    , ("VerKeyVRF", viaShow vrfvk) -- The next 3 are type families
    , ("Eta", viaShow eta)
    , ("L", viaShow l)
    , ("size", ppNatural size)
    , ("Hash", ppHash hash)
    , ("OCert", ppOCert ocert)
    , ("ProtVersion", ppProtVer protver)
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
-- Cardano.Ledger.Shelley.LedgerState.TxCert.Certificates

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

ppRewardUpdate :: RewardUpdate c -> PDoc
ppRewardUpdate (RewardUpdate dt dr rss df nonmyop) =
  ppRecord
    "RewardUpdate"
    [ ("deltaT", ppDeltaCoin dt)
    , ("deltaR", ppDeltaCoin dr)
    , ("rs", ppMap' mempty ppCredential (ppSet ppReward) rss)
    , ("deltaF", ppDeltaCoin df)
    , ("nonMyopic", ppNonMyopic nonmyop)
    ]

ppRewardSnapShot :: RewardSnapShot c -> PDoc
ppRewardSnapShot (RewardSnapShot fee ver non deltaR1 rR deltaT1 ls lrews) =
  ppRecord
    "RewardSnapShot"
    [ ("fees", ppCoin fee)
    , ("version", ppProtVer ver)
    , ("nonmyopic", ppNonMyopic non)
    , ("deltaR1", ppCoin deltaR1)
    , ("R", ppCoin rR)
    , ("deltaT1", ppCoin deltaT1)
    , ("likelihoods", ppMap ppKeyHash ppLikelihood ls)
    , ("leaderRewards", ppMap ppCredential (ppSet ppReward) lrews)
    ]

ppPoolRewardInfo :: PoolRewardInfo c -> PDoc
ppPoolRewardInfo (PoolRewardInfo relStake pot params blocks lreward) =
  ppRecord
    "PoolRewardInfo"
    [ ("poolRelativeStake", ppStakeShare relStake)
    , ("poolPot", ppCoin pot)
    , ("poolPs", ppPoolParams params)
    , ("poolBlocks", ppNatural blocks)
    , ("leaderReward", ppLeaderOnlyReward lreward)
    ]

ppFreeVars :: FreeVars c -> PDoc
ppFreeVars (FreeVars ds addrs total pv pri) =
  ppRecord
    "FreeVars"
    [ ("delegs", ppVMap ppCredential ppKeyHash ds)
    , ("addrsRew", ppSet ppCredential addrs)
    , ("totalStake", ppCoin total)
    , ("pv", ppProtVer pv)
    , ("poolRewardInfo", ppMap ppKeyHash ppPoolRewardInfo pri)
    ]

ppAns :: RewardAns c -> PDoc
ppAns (RewardAns x y) =
  ppRecord "RewardAns" [("allEvents", ppMap ppCredential ppReward x), ("recentEvents", ppMap ppCredential (ppSet ppReward) y)]

ppRewardPulser :: Pulser c -> PDoc
ppRewardPulser (RSLP n free items ans) =
  ppSexp
    "RewardPulser"
    [ ppInt n
    , ppFreeVars free
    , ppVMap ppCredential (ppCompactForm ppCoin) items
    , ppAns ans
    ]

ppPulsingRewUpdate :: PulsingRewUpdate c -> PDoc
ppPulsingRewUpdate (Pulsing snap pulser) =
  ppSexp "Pulsing" [ppRewardSnapShot snap, ppRewardPulser pulser]
ppPulsingRewUpdate (Complete rewup) =
  ppSexp "Complete" [ppRewardUpdate rewup]

instance PrettyA (RewardSnapShot c) where
  prettyA = ppRewardSnapShot

instance PrettyA (FreeVars c) where
  prettyA = ppFreeVars

instance PrettyA (Pulser c) where
  prettyA = ppRewardPulser

instance PrettyA (PulsingRewUpdate c) where
  prettyA = ppPulsingRewUpdate

instance PrettyA (RewardUpdate c) where
  prettyA = ppRewardUpdate

-- =================================
-- Cardano.Ledger.Shelley.LedgerState

-- | Constraints needed to ensure that the ledger state can be pretty printed.
type CanPrettyPrintLedgerState era =
  ( PrettyA (TxOut era)
  , PrettyA (PParams era)
  )

ppAccountState :: AccountState -> PDoc
ppAccountState (AccountState tr re) =
  ppRecord
    "AccountState"
    [ ("treasury", ppCoin tr)
    , ("reserves", ppCoin re)
    ]

ppCertState :: CertState era -> PDoc
ppCertState (CertState v p d) =
  ppRecord
    "CertState"
    [ ("vstate", prettyA v)
    , ("pstate", ppPState p)
    , ("dstate", ppDState d)
    ]

ppDState :: DState era -> PDoc
ppDState (DState unified future gen irwd) =
  ppRecord
    "DState"
    [ ("unifiedMap", ppUnifiedMap unified)
    , ("futuregendelegs", ppMap ppFutureGenDeleg ppGenDelegPair future)
    , ("gendelegs", ppGenDelegs gen)
    , ("instantaeousrewards", ppInstantaneousRewards irwd)
    ]

ppFutureGenDeleg :: FutureGenDeleg c -> PDoc
ppFutureGenDeleg (FutureGenDeleg sl kh) =
  ppRecord
    "FutureGenDeleg"
    [ ("delegSlot", ppSlotNo sl)
    , ("keyhash", ppKeyHash kh)
    ]

ppInstantaneousRewards :: InstantaneousRewards c -> PDoc
ppInstantaneousRewards (InstantaneousRewards res treas dR dT) =
  ppRecord
    "InstantaneousRewards"
    [ ("reserves", ppMap' mempty ppCredential ppCoin res)
    , ("treasury", ppMap' mempty ppCredential ppCoin treas)
    , ("deltaReserves", ppDeltaCoin dR)
    , ("deltaTreasury", ppDeltaCoin dT)
    ]

ppShelleyGovState ::
  ( PrettyA (PParamsUpdate era)
  , PrettyA (PParams era)
  ) =>
  ShelleyGovState era ->
  PDoc
ppShelleyGovState (ShelleyGovState p fp pp prev) =
  ppRecord
    "Proposed ShelleyGovState"
    [ ("proposals", ppProposedPPUpdates p)
    , ("futureProposals", ppProposedPPUpdates fp)
    , ("curPParams", prettyA pp)
    , ("prevPParams", prettyA prev)
    ]

ppPState :: PState c -> PDoc
ppPState (PState par fpar ret deposits) =
  ppRecord
    "PState"
    [ ("poolparams", ppMap' mempty ppKeyHash ppPoolParams par)
    , ("futurepoolparams", ppMap' mempty ppKeyHash ppPoolParams fpar)
    , ("retiring", ppMap' mempty ppKeyHash ppEpochNo ret)
    , ("deposits", ppMap ppKeyHash ppCoin deposits)
    ]

ppRewardAccounts :: Map.Map (Credential 'Staking c) Coin -> PDoc
ppRewardAccounts = ppMap' (text "RewardAccounts") ppCredential ppCoin

ppRewardType :: RewardType -> PDoc
ppRewardType MemberReward = text "MemberReward"
ppRewardType LeaderReward = text "LeaderReward"

ppReward :: Reward c -> PDoc
ppReward (Reward rt pool amt) =
  ppRecord
    "Reward"
    [ ("rewardType", ppRewardType rt)
    , ("poolId", ppKeyHash pool)
    , ("rewardAmount", ppCoin amt)
    ]

ppLeaderOnlyReward :: LeaderOnlyReward c -> PDoc
ppLeaderOnlyReward (LeaderOnlyReward pool amt) =
  ppRecord
    "LeaderOnlyReward"
    [ ("poolId", ppKeyHash pool)
    , ("rewardAmount", ppCoin amt)
    ]

ppIncrementalStake :: IncrementalStake c -> PDoc
ppIncrementalStake (IStake st dangle) =
  ppRecord
    "IncrementalStake"
    [ ("credMap", ppMap ppCredential ppCoin st)
    , ("ptrMap", ppMap ppPtr ppCoin dangle)
    ]

ppUTxOState ::
  ( CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  UTxOState era ->
  PDoc
ppUTxOState (UTxOState u dep fee ppup sd) =
  ppRecord
    "UTxOState"
    [ ("utxo", ppUTxO u)
    , ("deposited", ppCoin dep)
    , ("fees", ppCoin fee)
    , ("ppups", prettyA ppup)
    , ("stakeDistro", ppIncrementalStake sd)
    ]

ppEpochState ::
  ( CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  EpochState era ->
  PDoc
ppEpochState (EpochState acnt snap ls non) =
  ppRecord
    "EpochState"
    [ ("accountState", ppAccountState acnt)
    , ("snapShots", ppSnapShots snap)
    , ("ledgerState", ppLedgerState ls)
    , ("nonMyopic", ppNonMyopic non)
    ]

ppNewEpochState ::
  ( CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  NewEpochState era ->
  PDoc
ppNewEpochState (NewEpochState enum prevB curB es rewup pool _) =
  ppRecord
    "NewEpochState"
    [ ("epochnum", ppEpochNo enum)
    , ("prevBlock", ppBlocksMade prevB)
    , ("currBlock", ppBlocksMade curB)
    , ("epochState", ppEpochState es)
    , ("rewUpdate", ppStrictMaybe ppPulsingRewUpdate rewup)
    , ("poolDist", ppPoolDistr pool)
    ]

ppLedgerState ::
  ( CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  LedgerState era ->
  PDoc
ppLedgerState (LedgerState u d) =
  ppRecord
    "LedgerState"
    [ ("utxoState", ppUTxOState u)
    , ("delegationState", ppCertState d)
    ]

instance PrettyA AccountState where
  prettyA = ppAccountState

instance PrettyA (CertState c) where
  prettyA = ppCertState

instance PrettyA (DState c) where
  prettyA = ppDState

instance
  ( Era era
  , CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  PrettyA (EpochState era)
  where
  prettyA = ppEpochState

instance
  ( Era era
  , CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  PrettyA (NewEpochState era)
  where
  prettyA x = ppNewEpochState x

instance PrettyA (FutureGenDeleg c) where
  prettyA = ppFutureGenDeleg

instance PrettyA (InstantaneousRewards c) where
  prettyA = ppInstantaneousRewards

instance
  ( Era era
  , CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  PrettyA (LedgerState era)
  where
  prettyA = ppLedgerState

instance
  ( PrettyA (PParamsUpdate era)
  , PrettyA (PParams era)
  ) =>
  PrettyA (ShelleyGovState era)
  where
  prettyA = ppShelleyGovState

instance PrettyA (PState c) where
  prettyA = ppPState

instance
  ( Era era
  , CanPrettyPrintLedgerState era
  , PrettyA (GovernanceState era)
  ) =>
  PrettyA (UTxOState era)
  where
  prettyA = ppUTxOState

instance PrettyA (IncrementalStake c) where
  prettyA = ppIncrementalStake

-- =================================
-- Cardano.Ledger.Shelley.Rewards

ppPerformanceEstimate :: PerformanceEstimate -> PDoc
ppPerformanceEstimate (PerformanceEstimate n) = ppSexp "PerformanceEstimate" [ppDouble n]

ppNonMyopic :: NonMyopic c -> PDoc
ppNonMyopic (NonMyopic m c) =
  ppRecord
    "NonMyopic"
    [ ("likelihood", ppMap' "" ppKeyHash ppLikelihood m)
    , ("rewardPot", ppCoin c)
    ]

ppStakeShare :: StakeShare -> PDoc
ppStakeShare (StakeShare n) = ppSexp "StakeShare" [ppRational n]

ppHistogram :: Histogram -> PDoc
ppHistogram (Histogram ss) = ppSexp "Histogram" [ppStrictSeq ppLogWeight ss]

ppLogWeight :: LogWeight -> PDoc
ppLogWeight (LogWeight n) = ppSexp "LogWeight" [ppFloat n]

ppLikelihood :: Likelihood -> PDoc
ppLikelihood (Likelihood ns) = ppSexp "Likelihood" [ppStrictSeq ppLogWeight ns]

instance PrettyA PerformanceEstimate where
  prettyA = ppPerformanceEstimate

instance PrettyA StakeShare where
  prettyA = ppStakeShare

instance PrettyA Histogram where
  prettyA = ppHistogram

instance PrettyA LogWeight where
  prettyA = ppLogWeight

instance PrettyA Likelihood where
  prettyA = ppLikelihood

-- =================================
-- Cardano.Ledger.EpochBoundary

ppStake :: Stake c -> PDoc
ppStake (Stake m) =
  ppMap' (text "Stake") ppCredential (ppCoin . fromCompact) (VMap.toMap m)

ppBlocksMade :: BlocksMade c -> PDoc
ppBlocksMade (BlocksMade m) = ppMap' (text "BlocksMade") ppKeyHash ppNatural m

ppSnapShot :: SnapShot c -> PDoc
ppSnapShot (SnapShot st deleg params) =
  ppRecord
    "SnapShot"
    [ ("stake", ppStake st)
    , ("delegations", ppVMap ppCredential ppKeyHash deleg)
    , ("poolParams", ppVMap ppKeyHash ppPoolParams params)
    ]

ppSnapShots :: SnapShots c -> PDoc
ppSnapShots (SnapShots mark _markPD set go fees) =
  ppRecord
    "SnapShots"
    [ ("pstakeMark", ppSnapShot mark)
    , ("pstakeSet", ppSnapShot set)
    , ("pstakeGo", ppSnapShot go)
    , ("fee", ppCoin fees)
    ]

instance PrettyA (Stake c) where
  prettyA = ppStake

instance PrettyA (BlocksMade c) where
  prettyA = ppBlocksMade

instance PrettyA (SnapShot c) where
  prettyA = ppSnapShot

instance PrettyA (SnapShots c) where
  prettyA = ppSnapShots

-- ============================
-- Cardano.Ledger.UTxO

ppUTxO ::
  PrettyA (TxOut era) =>
  UTxO era ->
  PDoc
ppUTxO = ppAssocList (text "UTxO") ppTxIn prettyA . Map.toList . unUTxO

instance
  PrettyA (TxOut era) =>
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
ppMetadatum (I n) = ppSexp "I" [ppInteger n]
ppMetadatum (B bs) = ppSexp "B" [ppLong bs]
ppMetadatum (S txt) = ppSexp "S" [text txt]

ppShelleyTxAuxData :: Era era => ShelleyTxAuxData era -> PDoc
ppShelleyTxAuxData (ShelleyTxAuxData m) = ppMap' (text "ShelleyTxAuxData") ppWord64 ppMetadatum m

instance PrettyA Metadatum where
  prettyA = ppMetadatum

instance Era era => PrettyA (ShelleyTxAuxData era) where
  prettyA = ppShelleyTxAuxData

-- ============================
-- Cardano.Ledger.Shelley.Tx

ppTx ::
  ( PrettyA (TxBody era)
  , PrettyA (TxAuxData era)
  , PrettyA (TxWits era)
  , EraTx era
  ) =>
  Tx era ->
  PDoc
ppTx tx =
  ppRecord
    "Tx"
    [ ("body", prettyA $ tx ^. bodyTxL)
    , ("witnessSet", prettyA $ tx ^. witsTxL)
    , ("metadata", ppStrictMaybe prettyA $ tx ^. auxDataTxL)
    ]

ppBootstrapWitness :: Crypto c => BootstrapWitness c -> PDoc
ppBootstrapWitness (BootstrapWitness key sig (ChainCode code) attr) =
  ppRecord
    "BootstrapWitness"
    [ ("key", ppVKey key)
    , ("signature", ppSignedDSIGN sig)
    , ("chaincode", ppLong code)
    , ("attributes", ppLong attr)
    ]

ppWitnessSetHKD :: (Era era, PrettyA (Script era)) => ShelleyTxWits era -> PDoc
ppWitnessSetHKD x =
  let (addr, scr, boot) = prettyWitnessSetParts x
   in ppRecord
        "ShelleyTxWits"
        [ ("addrWits", ppSet ppWitVKey addr)
        , ("scriptWits", ppMap ppScriptHash prettyA scr)
        , ("bootWits", ppSet ppBootstrapWitness boot)
        ]

instance
  ( PrettyA (TxBody era)
  , PrettyA (TxAuxData era)
  , PrettyA (TxWits era)
  , EraTx era
  , Tx era ~ ShelleyTx era
  ) =>
  PrettyA (ShelleyTx era)
  where
  prettyA = ppTx

instance Crypto c => PrettyA (BootstrapWitness c) where
  prettyA = ppBootstrapWitness

instance (Era era, PrettyA (Script era)) => PrettyA (ShelleyTxWits era) where
  prettyA = ppWitnessSetHKD

-- ============================
--  Cardano.Ledger.AuxiliaryData

ppSafeHash :: SafeHash c index -> PDoc
ppSafeHash x = ppHash (extractHash x)

ppAuxiliaryDataHash :: AuxiliaryDataHash c -> PDoc
ppAuxiliaryDataHash (AuxiliaryDataHash h) = ppSexp "AuxiliaryDataHash" [ppSafeHash h]

instance PrettyA (AuxiliaryDataHash c) where
  prettyA = ppAuxiliaryDataHash

instance PrettyA (SafeHash c i) where
  prettyA = ppSafeHash

-- ============================
--  Cardano.Ledger.Compactible

ppCompactForm :: Compactible a => (a -> PDoc) -> CompactForm a -> PDoc
ppCompactForm cf x = cf (fromCompact x)

instance (Compactible a, PrettyA a) => PrettyA (CompactForm a) where
  prettyA = ppCompactForm prettyA

-- ============================
-- Cardano.Ledger.Shelley.TxBody

ppDelegation :: Delegation c -> PDoc
ppDelegation (Delegation orx ee) =
  ppRecord "Delegation" [("delegator", ppCredential orx), ("delegatee", ppKeyHash ee)]

ppPoolMetadata :: PoolMetadata -> PDoc
ppPoolMetadata (PoolMetadata url hsh) =
  ppRecord
    "PoolMetadata"
    [ ("url", ppUrl url)
    , ("hash", text "#" <> reAnnotate (Width 5 :) (ppLong hsh))
    ]

ppStakePoolRelay :: StakePoolRelay -> PDoc
ppStakePoolRelay (SingleHostAddr port ip4 ip6) = ppSexp "SingleHostAddr" [ppStrictMaybe ppPort port, ppStrictMaybe ppIPv4 ip4, ppStrictMaybe ppIPv6 ip6]
ppStakePoolRelay (SingleHostName port dns) = ppSexp "SingleHostName" [ppStrictMaybe ppPort port, ppDnsName dns]
ppStakePoolRelay (MultiHostName dns) = ppSexp "MultiHostName" [ppDnsName dns]

ppPoolParams :: PoolParams c -> PDoc
ppPoolParams (PoolParams idx vrf pledge cost margin acnt owners relays md) =
  ppRecord
    "PoolParams"
    [ ("Id", ppKeyHash idx)
    , ("Vrf", ppHash vrf)
    , ("Pledge", ppCoin pledge)
    , ("Cost", ppCoin cost)
    , ("Margin", ppUnitInterval margin)
    , ("RAcnt", ppRewardAcnt' acnt)
    , ("Owners", ppSet ppKeyHash owners)
    , ("Relays", ppStrictSeq ppStakePoolRelay relays)
    , ("Metadata", ppStrictMaybe ppPoolMetadata md)
    ]

ppWithdrawals :: Withdrawals c -> PDoc
ppWithdrawals (Withdrawals m) = ppSexp "" [ppMap' (text "Wdr") ppRewardAcnt' ppCoin m]

ppTxId :: TxId c -> PDoc
ppTxId (TxId x) = ppSexp "TxId" [ppSafeHash x]

ppTxIn :: TxIn c -> PDoc
ppTxIn (TxIn txid index) = ppSexp "TxIn" [ppTxId txid, pretty (txIxToInt index)]

ppTxOut :: (EraTxOut era, PrettyA (Value era)) => ShelleyTxOut era -> PDoc
ppTxOut (ShelleyTxOut addr val) =
  ppSexp
    "TxOut"
    [ ppAddr addr
    , prettyA val
    ]

ppShelleyDelegCert :: ShelleyDelegCert c -> PDoc
ppShelleyDelegCert (ShelleyRegCert cred) = ppSexp "ShelleyRegCert" [ppCredential cred]
ppShelleyDelegCert (ShelleyUnRegCert cred) = ppSexp "ShelleyUnRegCert" [ppCredential cred]
ppShelleyDelegCert (ShelleyDelegCert cred poolId) =
  ppSexp "ShelleyDelegCert" [ppCredential cred, ppKeyHash poolId]

ppPoolCert :: PoolCert c -> PDoc
ppPoolCert (RegPool x) = ppSexp "RegPool" [ppPoolParams x]
ppPoolCert (RetirePool x y) = ppSexp "RetirePool" [ppKeyHash x, ppEpochNo y]

ppGenesisDelegCert :: GenesisDelegCert c -> PDoc
ppGenesisDelegCert (GenesisDelegCert a b1 c) = ppSexp "GenesisDelgCert" [ppKeyHash a, ppKeyHash b1, ppHash c]

ppMIRPot :: MIRPot -> PDoc
ppMIRPot ReservesMIR = text "Reserves"
ppMIRPot TreasuryMIR = text "Treasury"

ppMIRTarget :: MIRTarget c -> PDoc
ppMIRTarget (StakeAddressesMIR rews) = ppMap ppCredential ppDeltaCoin rews
ppMIRTarget (SendToOppositePotMIR c) = ppCoin c

ppMIRCert :: MIRCert c -> PDoc
ppMIRCert (MIRCert pot vs) = ppSexp "MirCert" [ppMIRPot pot, ppMIRTarget vs]

ppShelleyTxCert :: ShelleyTxCert c -> PDoc
ppShelleyTxCert = \case
  ShelleyTxCertDelegCert x -> ppSexp "ShelleyTxCertDeleg" [ppShelleyDelegCert x]
  ShelleyTxCertPool x -> ppSexp "TxCertPool" [ppPoolCert x]
  ShelleyTxCertGenesisDeleg x -> ppSexp "ShelleyTxCertGenesisDeleg" [ppGenesisDelegCert x]
  ShelleyTxCertMir x -> ppSexp "TxCertMir" [ppMIRCert x]

ppTxBody ::
  ( EraTxOut era
  , PrettyA (PParamsUpdate era)
  , PrettyA (TxOut era)
  , PrettyA (TxCert era)
  ) =>
  ShelleyTxBody era ->
  PDoc
ppTxBody (TxBodyConstr (Memo (ShelleyTxBodyRaw ins outs cs withdrawals fee ttl upd mdh) _)) =
  ppRecord
    "TxBody"
    [ ("inputs", ppSet ppTxIn ins)
    , ("outputs", ppStrictSeq prettyA outs)
    , ("cert", ppStrictSeq prettyA cs)
    , ("withdrawals", ppWithdrawals withdrawals)
    , ("fee", ppCoin fee)
    , ("timetolive", ppSlotNo ttl)
    , ("update", ppStrictMaybe ppUpdate upd)
    , ("metadatahash", ppStrictMaybe ppAuxiliaryDataHash mdh)
    ]

ppWitVKey :: (Typeable kr, Crypto c) => WitVKey kr c -> PDoc
ppWitVKey (WitVKey key sig) = ppRecord "WitVKey" [("key", ppVKey key), ("signature", ppSignedDSIGN sig)]

instance PrettyA (Delegation c) where
  prettyA = ppDelegation

instance PrettyA PoolMetadata where
  prettyA = ppPoolMetadata

instance PrettyA StakePoolRelay where
  prettyA = ppStakePoolRelay

instance PrettyA (PoolParams c) where
  prettyA = ppPoolParams

instance PrettyA (Withdrawals c) where
  prettyA = ppWithdrawals

instance PrettyA (TxId c) where
  prettyA = ppTxId

instance PrettyA (TxIn c) where
  prettyA = ppTxIn

instance
  ( EraTxOut era
  , PrettyA (Value era)
  ) =>
  PrettyA (ShelleyTxOut era)
  where
  prettyA = ppTxOut

instance PrettyA (ShelleyDelegCert c) where
  prettyA = ppShelleyDelegCert

instance PrettyA (PoolCert c) where
  prettyA = ppPoolCert

instance PrettyA (GenesisDelegCert c) where
  prettyA = ppGenesisDelegCert

instance PrettyA MIRPot where
  prettyA = ppMIRPot

instance PrettyA (MIRCert c) where
  prettyA = ppMIRCert

instance PrettyA (ShelleyTxCert c) where
  prettyA = ppShelleyTxCert

instance
  ( EraTxOut era
  , PrettyA (PParamsUpdate era)
  , PrettyA (TxOut era)
  , PrettyA (TxCert era)
  , Era era
  ) =>
  PrettyA (ShelleyTxBody era)
  where
  prettyA = ppTxBody

instance (Typeable kr, Crypto c) => PrettyA (WitVKey kr c) where
  prettyA = ppWitVKey

-- ===========================================
-- Data.IP
ppIPv4 :: IPv4 -> PDoc
ppIPv4 = viaShow

ppIPv6 :: IPv6 -> PDoc
ppIPv6 = viaShow

instance PrettyA IPv4 where
  prettyA = ppIPv4

instance PrettyA IPv6 where
  prettyA = ppIPv6

-- ====================================================
-- Cardano.Ledger.Shelley.CompactAddr

ppCompactAddr :: Crypto c => CompactAddr c -> PDoc
ppCompactAddr x = ppAddr (decompactAddr x)

instance Crypto c => PrettyA (CompactAddr c) where
  prettyA = ppCompactAddr

-- ================================================
-- Cardano.Ledger.Shelley.PParams

ppVersion :: Version -> PDoc
ppVersion = ppWord64 . getVersion64

instance PrettyA Version where
  prettyA = ppVersion

ppProtVer :: ProtVer -> PDoc
ppProtVer (ProtVer maj mi) = ppRecord "ProtVer" [("major", ppVersion maj), ("minor", ppNatural mi)]

ppPParams :: (ProtVerAtMost era 4, ProtVerAtMost era 6, EraPParams era) => PParams era -> PDoc
ppPParams pp =
  ppRecord
    "PParams"
    [ ("minfeeA", ppCoin $ pp ^. ppMinFeeAL)
    , ("minfeeB", ppCoin $ pp ^. ppMinFeeBL)
    , ("maxBBSize", ppNatural $ pp ^. ppMaxBBSizeL)
    , ("maxTxSize", ppNatural $ pp ^. ppMaxTxSizeL)
    , ("maxBHSize", ppNatural $ pp ^. ppMaxBHSizeL)
    , ("keyDeposit", ppCoin $ pp ^. ppKeyDepositL)
    , ("poolDeposit", ppCoin $ pp ^. ppPoolDepositL)
    , ("eMax", ppEpochNo $ pp ^. ppEMaxL)
    , ("nOpt", ppNatural $ pp ^. ppNOptL)
    , ("a0", ppRational $ unboundRational $ pp ^. ppA0L)
    , ("rho", ppUnitInterval $ pp ^. ppRhoL)
    , ("tau", ppUnitInterval $ pp ^. ppTauL)
    , ("d", ppUnitInterval $ pp ^. ppDL)
    , ("extraEntropy", ppNonce $ pp ^. ppExtraEntropyL)
    , ("protocolVersion", ppProtVer $ pp ^. ppProtocolVersionL)
    , ("minUTxOValue", ppCoin $ pp ^. ppMinUTxOValueL)
    , ("minPoolCost", ppCoin $ pp ^. ppMinPoolCostL)
    ]

ppPParamsUpdate ::
  (ProtVerAtMost era 4, ProtVerAtMost era 6, EraPParams era) =>
  PParamsUpdate era ->
  PDoc
ppPParamsUpdate pp =
  ppRecord
    "PParamsUdate"
    [ ("minfeeA", ppStrictMaybe ppCoin $ pp ^. ppuMinFeeAL)
    , ("minfeeB", ppStrictMaybe ppCoin $ pp ^. ppuMinFeeBL)
    , ("maxBBSize", ppStrictMaybe ppNatural $ pp ^. ppuMaxBBSizeL)
    , ("maxTxSize", ppStrictMaybe ppNatural $ pp ^. ppuMaxTxSizeL)
    , ("maxBHSize", ppStrictMaybe ppNatural $ pp ^. ppuMaxBHSizeL)
    , ("keyDeposit", ppStrictMaybe ppCoin $ pp ^. ppuKeyDepositL)
    , ("poolDeposit", ppStrictMaybe ppCoin $ pp ^. ppuPoolDepositL)
    , ("eMax", ppStrictMaybe ppEpochNo $ pp ^. ppuEMaxL)
    , ("nOpt", ppStrictMaybe ppNatural $ pp ^. ppuNOptL)
    , ("a0", ppStrictMaybe (ppRational . unboundRational) $ pp ^. ppuA0L)
    , ("rho", ppStrictMaybe ppUnitInterval $ pp ^. ppuRhoL)
    , ("tau", ppStrictMaybe ppUnitInterval $ pp ^. ppuTauL)
    , ("d", ppStrictMaybe ppUnitInterval $ pp ^. ppuDL)
    , ("extraEntropy", ppStrictMaybe ppNonce $ pp ^. ppuExtraEntropyL)
    , ("protocolVersion", ppStrictMaybe ppProtVer $ pp ^. ppuProtocolVersionL)
    , ("minUTxOValue", ppStrictMaybe ppCoin $ pp ^. ppuMinUTxOValueL)
    , ("minPoolCost", ppStrictMaybe ppCoin $ pp ^. ppuMinPoolCostL)
    ]

ppUpdate :: PrettyA (PParamsUpdate era) => Update era -> PDoc
ppUpdate (Update prop epn) = ppSexp "Update" [ppProposedPPUpdates prop, ppEpochNo epn]

ppProposedPPUpdates :: PrettyA (PParamsUpdate era) => ProposedPPUpdates era -> PDoc
ppProposedPPUpdates (ProposedPPUpdates m) = ppMap ppKeyHash prettyA m

ppPPUpdateEnv :: PPUpdateEnv c -> PDoc
ppPPUpdateEnv (PPUpdateEnv slot gd) =
  ppSexp
    "PPUpdateEnv"
    [ppSlotNo slot, ppGenDelegs gd]

instance PrettyA (PPUpdateEnv c) where
  prettyA = ppPPUpdateEnv

instance
  PrettyA (PParamsUpdate e) =>
  PrettyA (ProposedPPUpdates e)
  where
  prettyA = ppProposedPPUpdates

instance PrettyA (PParamsUpdate e) => PrettyA (Update e) where
  prettyA = ppUpdate

instance Crypto c => PrettyA (PParamsUpdate (ShelleyEra c)) where
  prettyA = ppPParamsUpdate

instance Crypto c => PrettyA (PParams (ShelleyEra c)) where
  prettyA = ppPParams

instance Crypto c => PrettyA (PParamsUpdate (AllegraEra c)) where
  prettyA = ppPParamsUpdate

instance Crypto c => PrettyA (PParams (AllegraEra c)) where
  prettyA = ppPParams

instance Crypto c => PrettyA (PParamsUpdate (MaryEra c)) where
  prettyA = ppPParamsUpdate

instance Crypto c => PrettyA (PParams (MaryEra c)) where
  prettyA = ppPParams

instance PrettyA ProtVer where
  prettyA = ppProtVer

-- ============================================
-- Cardano.Ledger.Coin
ppCoin :: Coin -> PDoc
ppCoin (Coin n) = ppSexp "Coin" [pretty n]

ppDeltaCoin :: DeltaCoin -> PDoc
ppDeltaCoin (DeltaCoin n) = ppSexp "DeltaCoin" [pretty n]

instance PrettyA Coin where
  prettyA = ppCoin

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

instance PrettyA NetworkMagic where
  prettyA = ppNetworkMagic

instance PrettyA (BootstrapAddress c) where
  prettyA = ppBootstrapAddress

instance PrettyA UnparsedFields where
  prettyA = ppUnparsedFields

-- ===========================================
-- Cardano.Ledger.Shelley.Address

ppAddr :: Addr c -> PDoc
ppAddr (Addr net cred ref) = ppSexp "Addr" [ppNetwork net, ppCredential cred, ppStakeReference ref]
ppAddr (AddrBootstrap x) = ppSexp' mempty [ppBootstrapAddress x]

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

instance PrettyA (VState era) where
  prettyA (VState vsDReps vsCommitteeHotKeys) =
    ppRecord
      "VState"
      [ ("DReps", prettyA vsDReps)
      , ("CC Hot Keys", prettyA vsCommitteeHotKeys)
      ]

-- ======================================================

-- | Used to test pretty printing things with different widths
--   for example: testwidth 120 ls ppLedgerState
--   prints LedgerState, ls, with a max width of 120 columns
--   one can use this to observe the how "pretty" a printer is at different widths
atWidth :: Int -> a -> (a -> PDoc) -> IO ()
atWidth n a pp = do
  let doc = pp a
  putDocW n doc
  putStrLn ""

-- =========================================================

-- | Wrap a type: t (that has a PrettyA instance) with a newtype
--   cnstructor, so that the prettyA instance is used to Show (Nice t).
--   Can also be used as a DerivingVia helper.
newtype Nice t = Nice {unNice :: t}

instance PrettyA t => Show (Nice t) where
  show (Nice x) = show (prettyA x)
  showList xs more = show (ppList (prettyA . unNice) xs) ++ more

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
  prettyA = ppPair prettyA prettyA

instance PrettyA a => PrettyA (StrictSeq a) where
  prettyA = ppStrictSeq prettyA

instance PrettyA a => PrettyA (StrictMaybe a) where
  prettyA = ppStrictMaybe prettyA

instance PrettyA a => PrettyA (Maybe a) where
  prettyA = ppMaybe prettyA

ptrace :: PrettyA t => String -> t -> a -> a
ptrace x y z = trace ("\n" ++ show (prettyA y) ++ "\n" ++ show x) z

instance (PrettyA x, PrettyA y) => PrettyA (Map.Map x y) where
  prettyA m = ppMap prettyA prettyA m

-- | turn on trace appromimately 1 in 'n' times it is called.
occaisionally :: Hashable.Hashable a => a -> Int -> String -> String
occaisionally x n s = if mod (Hashable.hash x) n == 0 then trace s s else s

instance PrettyA (DRep c) where
  prettyA (DRepCredential c) = ppSexp "DRepCredential" [prettyA c]
  prettyA DRepAlwaysAbstain = "DRepAlwaysAbstain"
  prettyA DRepAlwaysNoConfidence = "DRepAlwaysNoConfidence"
