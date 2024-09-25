{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Generic.PrettyCore where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.Allegra.Rules as Allegra (AllegraUtxoPredFailure (..))
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript (..),
  Timelock,
  ValidityInterval (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody (..))
import Cardano.Ledger.Alonzo.Core (CoinPerWord (..))
import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules as Alonzo (
  AlonzoBbodyPredFailure (..),
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure (..),
  FailureDescription (..),
  TagMismatchDescription (..),
 )
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AlonzoPlutusPurpose (..),
  AlonzoScript (..),
  AsItem (..),
  AsIx (..),
  AsIxItem (..),
  ExUnits (..),
  PlutusPurpose,
  plutusScriptLanguage,
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData,
  atadMetadata',
  getAlonzoTxAuxDataScripts,
 )
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (..), AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), TxDats (..), unRedeemers, unTxDats)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Babbage.Core (CoinPerByte (..))
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (
  Anchor (..),
  BlocksMade (..),
  EpochInterval (..),
  EpochNo (..),
  FixedPoint,
  Network (..),
  Nonce (..),
  ProtVer (..),
  SlotNo (..),
  TxIx (..),
  UnitInterval,
  Version,
  certIxToInt,
  getVersion64,
  txIxToInt,
  unboundRational,
 )
import Cardano.Ledger.CertState (CommitteeAuthorization (..), CommitteeState (..))
import qualified Cardano.Ledger.CertState as DP
import Cardano.Ledger.Coin (Coin (..), CompactForm (..), DeltaCoin (..))
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  Constitution (..),
  ConwayGovState (..),
  DRepPulser (..),
  DRepPulsingState (..),
  EnactState (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionState (..),
  GovPurposeId (..),
  GovRelation (..),
  PEdges (..),
  PGraph (..),
  ProposalProcedure (..),
  Proposals,
  PulsingSnapshot (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  pGraphL,
  pRootsL,
  proposalsActionsMap,
  toPrevGovActionIds,
  unPGraph,
 )
import Cardano.Ledger.Conway.Rules (
  CertEnv (..),
  ConwayBbodyPredFailure,
  ConwayCertsPredFailure (..),
  ConwayDelegEnv (..),
  ConwayDelegPredFailure (..),
  ConwayGovCertEnv (..),
  ConwayGovCertPredFailure (..),
  ConwayGovPredFailure (..),
  ConwayLedgerPredFailure (..),
  ConwayNewEpochPredFailure,
  ConwayUtxosPredFailure,
  EnactSignal (..),
  GovEnv (..),
  GovSignal (..),
 )
import qualified Cardano.Ledger.Conway.Rules as ConwayRules
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  ConwayGovCert (..),
  ConwayTxCert (..),
  Delegatee (..),
 )
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (
  Credential (
    KeyHashObj,
    ScriptHashObj
  ),
  Ptr (..),
  StakeReference (..),
 )
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.EpochBoundary (
  SnapShot (..),
  SnapShots (..),
  Stake (..),
 )
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  GenDelegs (..),
  HasKeyRole (coerceKeyRole),
  KeyHash (..),
  KeyRole (..),
  VKey (..),
  WitVKey (..),
  hashKey,
 )
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness (..), ChainCode (..))
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..))
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
  flattenMultiAsset,
 )
import Cardano.Ledger.MemoBytes (MemoBytes (..))
import Cardano.Ledger.Plutus.Data (
  Data (..),
  Datum (..),
  binaryDataToData,
  hashData,
 )
import qualified Cardano.Ledger.Plutus.ExUnits as ExUnits (Prices (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash, hashAnnotated)
import Cardano.Ledger.Shelley.AdaPots (
  AdaPots (..),
  totalAdaES,
  totalAdaPotsES,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  CertState (..),
  DState (..),
  EpochState (..),
  FutureGenDeleg (..),
  IncrementalStake (IStake),
  InstantaneousRewards (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  RewardUpdate (..),
  UTxOState (..),
  VState (..),
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import qualified Cardano.Ledger.Shelley.PParams as PParams (Update (..))
import Cardano.Ledger.Shelley.PoolRank (Likelihood (..), LogWeight (..), NonMyopic (..))
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  PoolEnv (..),
  ShelleyLedgerPredFailure (..),
  UpecPredFailure,
  UtxoEnv (..),
 )
import Cardano.Ledger.Shelley.Rules as Shelley (
  ShelleyBbodyPredFailure (..),
  ShelleyBbodyState (..),
  ShelleyDelegPredFailure (..),
  ShelleyDelegsPredFailure (..),
  ShelleyDelplPredFailure (..),
  ShelleyEpochPredFailure (..),
  ShelleyLedgersPredFailure (..),
  ShelleyNewEpochPredFailure (..),
  ShelleyNewppPredFailure (..),
  ShelleyPoolPredFailure (..),
  ShelleyPoolreapPredFailure,
  ShelleyPpupPredFailure (..),
  ShelleySnapPredFailure,
  ShelleyTickPredFailure (..),
  ShelleyUpecPredFailure (..),
  ShelleyUtxoPredFailure (..),
  ShelleyUtxowPredFailure (..),
 )
import Cardano.Ledger.Shelley.Scripts (
  MultiSig,
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..), ShelleyTxAuxData (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..), ShelleyTxBodyRaw (..))
import Cardano.Ledger.Shelley.TxCert (ShelleyDelegCert (..), ShelleyTxCert (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap (
  RDPair (..),
  dRepMap,
  depositMap,
  fromCompact,
  ptrMap,
  rewardMap,
  sPoolMap,
 )
import qualified Cardano.Ledger.UMap as UM (UMap, UView (..), size)
import Cardano.Ledger.UTxO (ScriptsNeeded, UTxO (..))
import qualified Cardano.Ledger.Val as Val
import Codec.Binary.Bech32
import Control.Monad.Identity (Identity)
import Control.State.Transition.Extended (STS (..))
import qualified Data.ByteString as Long (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString, toStrict)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.OSet.Strict (OSet)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import qualified Data.VMap as VMap
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Lens.Micro ((^.))
import qualified PlutusLedgerApi.V1 as PV1 (Data (..))
import Prettyprinter (
  Pretty (pretty),
  align,
  brackets,
  comma,
  encloseSep,
  fillSep,
  flatAlt,
  group,
  hang,
  hsep,
  lbrace,
  lparen,
  parens,
  punctuate,
  rbrace,
  reAnnotate,
  rparen,
  sep,
  space,
  vcat,
  viaShow,
  vsep,
  (<+>),
 )
import Prettyprinter.Internal (Doc (Empty))
import Prettyprinter.Util (putDocW)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Fields (
  PParamsField (..),
  TxBodyField (..),
  TxField (..),
  WitnessesField (..),
  abstractPParams,
  abstractTx,
  abstractTxBody,
  abstractWitnesses,
 )
import qualified Test.Cardano.Ledger.Generic.Fields as Fields
import Test.Cardano.Ledger.Generic.Proof (
  AllegraEra,
  AlonzoEra,
  BabbageEra,
  ConwayEra,
  GovStateWit (..),
  MaryEra,
  Proof (..),
  Reflect (..),
  ShelleyEra,
  unReflect,
  whichGovState,
 )

-- ================================================

class PrettyA t where
  prettyA :: t -> PDoc

instance PrettyA () where
  prettyA = ppString . show

instance PrettyA Void where
  prettyA = absurd

instance PrettyA Bool where
  prettyA = ppBool

instance PrettyA Int where
  prettyA = ppInt

instance PrettyA x => PrettyA [x] where
  prettyA = ppList prettyA

instance PrettyA x => PrettyA (NonEmpty x) where
  prettyA = prettyA . toList

instance (PrettyA a, PrettyA b) => PrettyA (Map a b) where
  prettyA = ppMap prettyA prettyA

instance (PrettyA a, PrettyA b) => PrettyA (a, b) where
  prettyA (x, y) = encloseSep lparen rparen comma [prettyA x, prettyA y]

instance PrettyA Word32 where
  prettyA = ppString . show

instance PrettyA Word64 where
  prettyA = ppString . show

-- =====================================
-- Operations for pretty printing
-- =====================================

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

trim :: PDoc -> PDoc
trim x = ppString (take 10 (show x))

-- ================================================
-- Named pretty printers for simple Haskell types
-- ================================================

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

-- ============================================
-- Combinators for common patterns of layout
-- ============================================

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
      ( con
          <> encloseSep
            (lbrace <> space)
            (space <> rbrace)
            (comma <> space)
            (map (\(x, y) -> equate (text x) y) fields)
      )

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

ppSeq :: (a -> Doc ann) -> Seq a -> Doc ann
ppSeq p xs = ppList p (foldr (:) [] xs)

ppList :: (x -> Doc ann) -> [x] -> Doc ann
ppList p xs = brackets $ fillSep $ punctuate comma $ map p xs

ppStrictSeq :: (a -> Doc ann) -> StrictSeq a -> Doc ann
ppStrictSeq p xs = ppList p (foldr (:) [] xs)

ppOSet :: (a -> Doc ann) -> OSet a -> Doc ann
ppOSet p xs = ppList p (foldr (:) [] xs)

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

-- =====================================================================================================
-- Pretty printers Cardano.Ledger types
-- =====================================================================================================

ppPolicyID :: PolicyID c -> PDoc
ppPolicyID (PolicyID sh) = pcScriptHash sh

ppLogWeight :: LogWeight -> PDoc
ppLogWeight (LogWeight n) = ppSexp "LogWeight" [ppFloat n]

instance PrettyA LogWeight where
  prettyA = ppLogWeight

ppPrices :: ExUnits.Prices -> PDoc
ppPrices (ExUnits.Prices prMem prSteps) =
  ppRecord
    "Prices"
    [ ("prMem", ppRational $ unboundRational prMem)
    , ("prSteps", ppRational $ unboundRational prSteps)
    ]

instance PrettyA ExUnits.Prices where
  prettyA = ppPrices

ppRewardType :: RewardType -> PDoc
ppRewardType MemberReward = text "MemberReward"
ppRewardType LeaderReward = text "LeaderReward"

ppLikelihood :: Likelihood -> PDoc
ppLikelihood (Likelihood ns) = ppSexp "Likelihood" [ppStrictSeq ppLogWeight ns]

ppNonMyopic :: NonMyopic c -> PDoc
ppNonMyopic (NonMyopic m c) =
  ppRecord
    "NonMyopic"
    [ ("likelihood", ppMap' "" pcKeyHash ppLikelihood m)
    , ("rewardPot", pcCoin c)
    ]

ppRewardUpdate :: RewardUpdate c -> PDoc
ppRewardUpdate (RewardUpdate dt dr rss df nonmyop) =
  ppRecord
    "RewardUpdate"
    [ ("deltaT", pcDeltaCoin dt)
    , ("deltaR", pcDeltaCoin dr)
    , ("rs", ppMap' mempty pcCredential (ppSet pcReward) rss)
    , ("deltaF", pcDeltaCoin df)
    , ("nonMyopic", ppNonMyopic nonmyop)
    ]

instance PrettyA (RewardUpdate c) where
  prettyA = ppRewardUpdate

ppLanguage :: Language -> PDoc
ppLanguage = ppString . show

ppTxWitness ::
  forall era.
  Reflect era =>
  AlonzoTxWits era ->
  PDoc
ppTxWitness (AlonzoTxWits' vk wb sc da rd) =
  ppRecord
    "AlonzoTxWits"
    [ ("keys", ppSet (pcWitVKey @era reify) vk)
    , ("bootstrap witnesses", ppSet ppBootstrapWitness wb)
    , ("scripts map", ppMap pcScriptHash (pcScript reify) sc)
    , ("Data map", ppMap ppSafeHash pcData (unTxDats da))
    , ("Redeemer map", ppMap ppPlutusPurposeAsIx (ppPair pcData pcExUnits) (unRedeemers rd))
    ]

instance Reflect era => PrettyA (AlonzoTxWits era) where
  prettyA = ppTxWitness

ppIsValid :: IsValid -> PDoc
ppIsValid (IsValid True) = ppString "True"
ppIsValid (IsValid False) = ppString "False"

ppVersion :: Version -> PDoc
ppVersion = ppWord64 . getVersion64

ppNetwork :: Network -> PDoc
ppNetwork Testnet = text "Testnet"
ppNetwork Mainnet = text "Mainnet"

ppValidityInterval :: ValidityInterval -> PDoc
ppValidityInterval (ValidityInterval b a) =
  ppRecord
    "ValidityInterval"
    [ ("invalidBefore", ppStrictMaybe pcSlotNo b)
    , ("invalidHereafter", ppStrictMaybe pcSlotNo a)
    ]

instance PrettyA ValidityInterval where
  prettyA = ppValidityInterval

ppAuxiliaryDataHash :: AuxiliaryDataHash c -> PDoc
ppAuxiliaryDataHash (AuxiliaryDataHash h) = ppSexp "AuxiliaryDataHash" [ppSafeHash h]

instance PrettyA (AuxiliaryDataHash c) where
  prettyA = ppAuxiliaryDataHash

ppSafeHash :: SafeHash c index -> PDoc
ppSafeHash x = ppHash (extractHash x)

instance PrettyA (SafeHash x y) where
  prettyA = ppSafeHash

ppEpochNo :: EpochNo -> Doc ann
ppEpochNo (EpochNo x) = text "EpochNo" <+> pretty x

instance PrettyA EpochNo where
  prettyA = ppEpochNo

ppEpochInterval :: EpochInterval -> Doc ann
ppEpochInterval (EpochInterval x) = text "EpochInterval" <+> pretty x

instance PrettyA EpochInterval where
  prettyA = ppEpochInterval

ppHash :: Hash.Hash a b -> PDoc
ppHash x = text "#" <> reAnnotate (Width 5 :) (viaShow x)

ppUnitInterval :: UnitInterval -> PDoc
ppUnitInterval = viaShow

instance PrettyA UnitInterval where
  prettyA = ppUnitInterval

ppNonce :: Nonce -> PDoc
ppNonce (Nonce h) = text "Nonce" <+> ppHash h
ppNonce NeutralNonce = text "NeutralNonce"

ppPtr :: Ptr -> PDoc
ppPtr (Ptr slot txIx certIx) =
  ppSexp "Ptr" [pcSlotNo slot, pretty (txIxToInt txIx), pretty (certIxToInt certIx)]

instance PrettyA Ptr where
  prettyA = ppPtr

ppProtVer :: ProtVer -> PDoc
ppProtVer (ProtVer maj mi) = ppRecord "ProtVer" [("major", ppVersion maj), ("minor", ppNatural mi)]

instance PrettyA ProtVer where
  prettyA = ppProtVer

ppVKey :: Crypto c => VKey r c -> PDoc
ppVKey vk@(VKey x) = vsep [reAnnotate (Width 5 :) (viaShow x), "hash = " <+> pcKeyHash (hashKey vk)]

instance Crypto c => PrettyA (VKey r c) where
  prettyA = ppVKey

ppBootstrapWitness :: Crypto c => BootstrapWitness c -> PDoc
ppBootstrapWitness (BootstrapWitness key sig (ChainCode code) attr) =
  ppRecord
    "BootstrapWitness"
    [ ("key", ppVKey key)
    , ("signature", ppSignedDSIGN sig)
    , ("chaincode", ppLong code)
    , ("attributes", ppLong attr)
    ]

instance Crypto c => PrettyA (BootstrapWitness c) where
  prettyA = ppBootstrapWitness

ppWitnessSetHKD :: forall era. Reflect era => ShelleyTxWits era -> PDoc
ppWitnessSetHKD (ShelleyTxWits addr scr boot) =
  ppRecord
    "ShelleyTxWits"
    [ ("addrWits", ppSet (pcWitVKey @era reify) addr)
    , ("scriptWits", ppMap pcScriptHash (pcScript reify) scr)
    , ("bootWits", ppSet ppBootstrapWitness boot)
    ]

instance Reflect era => PrettyA (ShelleyTxWits era) where
  prettyA = ppWitnessSetHKD

-- TODO Redo this with Fields?
ppPParamsUpdate ::
  (ProtVerAtMost era 4, ProtVerAtMost era 6, ProtVerAtMost era 8, EraPParams era) =>
  PParamsUpdate era ->
  PDoc
ppPParamsUpdate pp =
  ppRecord
    "PParamsUdate"
    [ ("minfeeA", ppStrictMaybe pcCoin $ pp ^. ppuMinFeeAL)
    , ("minfeeB", ppStrictMaybe pcCoin $ pp ^. ppuMinFeeBL)
    , ("maxBBSize", ppStrictMaybe ppWord32 $ pp ^. ppuMaxBBSizeL)
    , ("maxTxSize", ppStrictMaybe ppWord32 $ pp ^. ppuMaxTxSizeL)
    , ("maxBHSize", ppStrictMaybe ppWord16 $ pp ^. ppuMaxBHSizeL)
    , ("keyDeposit", ppStrictMaybe pcCoin $ pp ^. ppuKeyDepositL)
    , ("poolDeposit", ppStrictMaybe pcCoin $ pp ^. ppuPoolDepositL)
    , ("eMax", ppStrictMaybe ppEpochInterval $ pp ^. ppuEMaxL)
    , ("nOpt", ppStrictMaybe ppNatural $ pp ^. ppuNOptL)
    , ("a0", ppStrictMaybe (ppRational . unboundRational) $ pp ^. ppuA0L)
    , ("rho", ppStrictMaybe ppUnitInterval $ pp ^. ppuRhoL)
    , ("tau", ppStrictMaybe ppUnitInterval $ pp ^. ppuTauL)
    , ("d", ppStrictMaybe ppUnitInterval $ pp ^. ppuDL)
    , ("extraEntropy", ppStrictMaybe ppNonce $ pp ^. ppuExtraEntropyL)
    , ("protocolVersion", ppStrictMaybe ppProtVer $ pp ^. ppuProtocolVersionL)
    , ("minUTxOValue", ppStrictMaybe pcCoin $ pp ^. ppuMinUTxOValueL)
    , ("minPoolCost", ppStrictMaybe pcCoin $ pp ^. ppuMinPoolCostL)
    ]

-- TODO Do we need all these instances?
-- Does this resolve the constraints  (ProtVerAtMost era 4, ProtVerAtMost era 6, ProtVerAtMost era 8)
instance Crypto c => PrettyA (PParamsUpdate (ShelleyEra c)) where
  prettyA = ppPParamsUpdate

instance Crypto c => PrettyA (PParamsUpdate (AllegraEra c)) where
  prettyA = ppPParamsUpdate

instance Crypto c => PrettyA (PParamsUpdate (MaryEra c)) where
  prettyA = ppPParamsUpdate

instance Crypto c => PrettyA (PParamsUpdate (AlonzoEra c)) where
  prettyA _ = ppString ("PParamsUpdate (AlonzoEra c)")

instance Crypto c => PrettyA (PParamsUpdate (BabbageEra c)) where
  prettyA _ = ppString ("PParamsUpdate (BabbageEra c)")

instance Crypto c => PrettyA (PParamsUpdate (ConwayEra c)) where
  prettyA _ = ppString ("PParamsUpdate (ConwayEra c)")

ppUpdate :: PrettyA (PParamsUpdate era) => PParams.Update era -> PDoc
ppUpdate (PParams.Update prop epn) = ppSexp "Update" [ppProposedPPUpdates prop, ppEpochNo epn]

instance PrettyA (PParamsUpdate e) => PrettyA (PParams.Update e) where
  prettyA = ppUpdate

ppProposedPPUpdates :: PrettyA (PParamsUpdate era) => ProposedPPUpdates era -> PDoc
ppProposedPPUpdates (ProposedPPUpdates m) = ppMap pcKeyHash prettyA m

instance PrettyA (PParamsUpdate e) => PrettyA (ProposedPPUpdates e) where
  prettyA = ppProposedPPUpdates

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

instance PrettyA Metadatum where
  prettyA = ppMetadatum

ppShelleyTxAuxData :: Era era => ShelleyTxAuxData era -> PDoc
ppShelleyTxAuxData (ShelleyTxAuxData m) = ppMap' (text "ShelleyTxAuxData") ppWord64 ppMetadatum m

instance Era era => PrettyA (ShelleyTxAuxData era) where
  prettyA = ppShelleyTxAuxData

ppAllegraTxAuxData ::
  (AllegraEraScript era, Reflect era, NativeScript era ~ Timelock era) => AllegraTxAuxData era -> PDoc
ppAllegraTxAuxData (AllegraTxAuxData m sp) =
  ppRecord
    "AllegraTxAuxData"
    [ ("metadata", ppMap' (text "Metadata") ppWord64 ppMetadatum m)
    , ("auxiliaryScripts", ppStrictSeq prettyA sp)
    ]

instance
  (AllegraEraScript era, Reflect era, NativeScript era ~ Timelock era) =>
  PrettyA (AllegraTxAuxData era)
  where
  prettyA = ppAllegraTxAuxData

ppAlonzoTxAuxData ::
  Reflect era =>
  AlonzoTxAuxData era ->
  PDoc
ppAlonzoTxAuxData auxData =
  ppSexp
    "AuxiliaryData"
    [ ppMap ppWord64 ppMetadatum (atadMetadata' auxData)
    , ppStrictSeq (pcScript reify) (extractAlonzoTxAuxDataScripts auxData)
    ]

extractAlonzoTxAuxDataScripts ::
  forall era.
  Reflect era =>
  AlonzoTxAuxData era ->
  StrictSeq (Script era)
extractAlonzoTxAuxDataScripts auxData =
  case reify @era of
    Shelley -> error "Unsuported"
    Allegra -> error "Unsuported"
    Mary -> error "Unsuported"
    Alonzo -> getAlonzoTxAuxDataScripts auxData
    Babbage -> getAlonzoTxAuxDataScripts auxData
    Conway -> getAlonzoTxAuxDataScripts auxData

instance
  (Reflect era, Script era ~ AlonzoScript era) =>
  PrettyA (AlonzoTxAuxData era)
  where
  prettyA = ppAlonzoTxAuxData

pcAuxData :: Proof era -> TxAuxData era -> PDoc
pcAuxData Shelley x = ppShelleyTxAuxData x
pcAuxData Allegra x = ppAllegraTxAuxData x
pcAuxData Mary x = ppAllegraTxAuxData x
pcAuxData Alonzo x = ppAlonzoTxAuxData x
pcAuxData Babbage x = ppAlonzoTxAuxData x
pcAuxData Conway x = ppAlonzoTxAuxData x

ppWithdrawals :: Withdrawals c -> PDoc
ppWithdrawals (Withdrawals m) = ppSexp "Withdrawals" [ppMap' (text "Wdr") pcRewardAccount pcCoin m]

instance PrettyA (Withdrawals c) where
  prettyA = ppWithdrawals

ppWitHashes :: Set (KeyHash 'Witness c) -> PDoc
ppWitHashes hs = ppSexp "WitHashes" [ppSet pcKeyHash hs]

-- ================================================
-- Pretty printers for Tx and TxBody
-- ================================================

ppShelleyTx ::
  Reflect era =>
  Tx era ->
  PDoc
ppShelleyTx tx =
  ppRecord
    "Tx"
    [ ("body", pcTxBody reify $ tx ^. bodyTxL)
    , ("witnessSet", ppCoreWitnesses reify $ tx ^. witsTxL)
    , ("metadata", ppStrictMaybe (pcAuxData reify) $ tx ^. auxDataTxL)
    ]

instance
  ( Reflect era
  , Tx era ~ ShelleyTx era
  ) =>
  PrettyA (ShelleyTx era)
  where
  prettyA = ppShelleyTx

ppAlonzoTx ::
  forall era.
  (Reflect era, Tx era ~ AlonzoTx era) =>
  AlonzoTx era ->
  PDoc
ppAlonzoTx tx = ppRecord "AlonzoTx" pairs
  where
    fields = abstractTx reify tx
    pairs = concatMap (pcTxField @era (reify @era)) fields

instance
  (Reflect era, Tx era ~ AlonzoTx era) =>
  PrettyA (AlonzoTx era)
  where
  prettyA = ppAlonzoTx

ppShelleyTxBody ::
  ( Reflect era
  , PrettyA (PParamsUpdate era)
  ) =>
  ShelleyTxBody era ->
  PDoc
ppShelleyTxBody (TxBodyConstr (Memo (ShelleyTxBodyRaw ins outs cs withdrawals fee ttl upd mdh) _)) =
  ppRecord
    "TxBody"
    [ ("inputs", ppSet pcTxIn ins)
    , ("outputs", ppStrictSeq (pcTxOut reify) outs)
    , ("cert", ppStrictSeq (pcTxCert reify) cs)
    , ("withdrawals", ppWithdrawals withdrawals)
    , ("fee", pcCoin fee)
    , ("timetolive", pcSlotNo ttl)
    , ("update", ppStrictMaybe ppUpdate upd)
    , ("metadatahash", ppStrictMaybe ppAuxiliaryDataHash mdh)
    ]

instance
  ( EraTxOut era
  , PrettyA (PParamsUpdate era)
  , Reflect era
  ) =>
  PrettyA (ShelleyTxBody era)
  where
  prettyA = ppShelleyTxBody

ppAllegraTxBody ::
  forall era. (TxBody era ~ AllegraTxBody era, Reflect era) => AllegraTxBody era -> PDoc
ppAllegraTxBody txbody = ppRecord ("TxBody " <> pack (show (reify @era))) pairs
  where
    fields = abstractTxBody reify txbody
    pairs = concatMap (pcTxBodyField reify) fields

instance (TxBody era ~ AllegraTxBody era, Reflect era) => PrettyA (AllegraTxBody era) where
  prettyA = ppAllegraTxBody

ppAlonzoTxBody ::
  forall era. (TxBody era ~ AlonzoTxBody era, Reflect era) => AlonzoTxBody era -> PDoc
ppAlonzoTxBody txbody = ppRecord ("TxBody " <> pack (show (reify @era))) pairs
  where
    fields = abstractTxBody reify txbody
    pairs = concatMap (pcTxBodyField reify) fields

instance (TxBody era ~ AlonzoTxBody era, Reflect era) => PrettyA (AlonzoTxBody era) where
  prettyA = ppAlonzoTxBody

ppMaryTxBody :: forall era. (TxBody era ~ MaryTxBody era, Reflect era) => MaryTxBody era -> PDoc
ppMaryTxBody txbody = ppRecord ("TxBody " <> pack (show (reify @era))) pairs
  where
    fields = abstractTxBody reify txbody
    pairs = concatMap (pcTxBodyField reify) fields

instance (TxBody era ~ MaryTxBody era, Reflect era) => PrettyA (MaryTxBody era) where
  prettyA = ppMaryTxBody

-- =============================================================
-- Pretty Printers for Type families must have a (Proof era) as
-- an argument. Because they are type families, they cannot
-- have a PrettyA instance. As one cannot write an instance for
-- any type family.
-- ==============================================================

ppCoreWitnesses :: Proof era -> TxWits era -> PDoc
ppCoreWitnesses Conway x = ppTxWitness x
ppCoreWitnesses Babbage x = ppTxWitness x
ppCoreWitnesses Alonzo x = ppTxWitness x
ppCoreWitnesses Mary x = ppWitnessSetHKD x
ppCoreWitnesses Allegra x = ppWitnessSetHKD x
ppCoreWitnesses Shelley x = ppWitnessSetHKD x

pcTxOut :: Reflect era => Proof era -> TxOut era -> PDoc
pcTxOut p@Conway (BabbageTxOut addr v d s) =
  ppSexp "TxOut" [pcAddr addr, pcValue v, pcDatum d, ppStrictMaybe (pcScript p) s]
pcTxOut p@Babbage (BabbageTxOut addr v d s) =
  ppSexp "TxOut" [pcAddr addr, pcValue v, pcDatum d, ppStrictMaybe (pcScript p) s]
pcTxOut Alonzo (AlonzoTxOut addr v md) =
  ppSexp "TxOut" [pcAddr addr, pcValue v, ppStrictMaybe pcDataHash md]
pcTxOut p@Mary (ShelleyTxOut addr v) =
  ppSexp "TxOut" [pcAddr addr, pcCoreValue p v]
pcTxOut p@Allegra (ShelleyTxOut addr v) =
  ppSexp "TxOut" [pcAddr addr, pcCoreValue p v]
pcTxOut p@Shelley (ShelleyTxOut addr v) =
  ppSexp "TxOut" [pcAddr addr, pcCoreValue p v]

pcScript :: forall era. Reflect era => Proof era -> Script era -> PDoc
pcScript Conway (TimelockScript t) = pcTimelock @era t
pcScript p@Conway s@(PlutusScript v) =
  parens (hsep [ppString ("PlutusScript " <> show (plutusScriptLanguage v) <> " "), pcHashScript p s])
pcScript Babbage (TimelockScript t) = pcTimelock @era t
pcScript p@Babbage s@(PlutusScript v) =
  parens (hsep [ppString ("PlutusScript " <> show (plutusScriptLanguage v) <> " "), pcHashScript p s])
pcScript Alonzo (TimelockScript t) = pcTimelock @era t
pcScript p@Alonzo s@(PlutusScript v) =
  parens (hsep [ppString ("PlutusScript " <> show (plutusScriptLanguage v) <> " "), pcHashScript p s])
pcScript Mary s = pcTimelock @era s
pcScript Allegra s = pcTimelock @era s
pcScript p@Shelley s = pcMultiSig @era (pcHashScript @era p s) s

pcWitnesses ::
  Reflect era =>
  Proof era ->
  TxWits era ->
  PDoc
pcWitnesses proof txwits = ppRecord "Witnesses" pairs
  where
    fields = abstractWitnesses proof txwits
    pairs = concat (map (pcWitnessesField proof) fields)

-- | Pretty print a Tx.
--   Also see Test.Cardano.Ledger.Constrained.Preds.Tx(pcTxWithUTxO)
--   which expands the inputs (given the UTxO)
pcTx :: Proof era -> Tx era -> PDoc
pcTx proof tx = ppRecord "Tx" pairs
  where
    fields = abstractTx proof tx
    pairs = concatMap (unReflect pcTxField proof) fields

pcTxBody :: Proof era -> TxBody era -> PDoc
pcTxBody proof txbody = ppRecord ("TxBody " <> pack (show proof)) pairs
  where
    fields = abstractTxBody proof txbody
    pairs = concatMap (pcTxBodyField proof) fields

pcPParams :: Proof era -> PParams era -> PDoc
pcPParams proof pp = ppRecord ("PParams " <> pack (show proof)) pairs
  where
    fields = abstractPParams proof pp
    pairs = concatMap pcPParamsField fields

-- | Unike other type families, PParams CAN have a PrettyA instance, as they
--   are a newtype wrapped around a type family.
instance Reflect era => PrettyA (PParams era) where
  prettyA = pcPParams reify

-- =================================
-- Type families that have Fields, can be pretty printed (for all eras) by using
-- function that can pretty print each field.
-- =================================

pcTxBodyField ::
  Proof era ->
  TxBodyField era ->
  [(Text, PDoc)]
pcTxBodyField proof x = case x of
  Inputs s -> [("spend inputs", ppSet pcTxIn s)]
  Collateral s -> [("coll inputs", ppSet pcTxIn s)]
  RefInputs s -> [("ref inputs", ppSet pcTxIn s)]
  Outputs s -> [("outputs", ppList (unReflect pcTxOut proof) (toList s))]
  CollateralReturn SNothing -> []
  CollateralReturn (SJust txout) -> [("coll return", unReflect pcTxOut proof txout)]
  TotalCol SNothing -> []
  TotalCol (SJust c) -> [("total coll", pcCoin c)]
  Certs xs -> [("certs", ppList (pcTxCert proof) (toList xs))]
  Withdrawals' (Withdrawals m) -> [("withdrawal", ppMap pcRewardAccount pcCoin m)]
  Txfee c -> [("fee", pcCoin c)]
  Vldt v -> [("validity interval", ppValidityInterval v)]
  TTL slot -> [("time to live", pcSlotNo slot)]
  Update SNothing -> []
  Update (SJust _) -> [("update", ppString "UPDATE")]
  ReqSignerHashes s -> [("required hashes", ppSet pcKeyHash s)]
  Fields.Mint (MultiAsset m) -> [("minted", ppSet pcPolicyID (Map.keysSet m))]
  WppHash SNothing -> []
  WppHash (SJust h) -> [("integrity hash", trim (ppSafeHash h))]
  AdHash SNothing -> []
  AdHash (SJust (AuxiliaryDataHash h)) -> [("aux data hash", trim (ppSafeHash h))]
  Txnetworkid SNothing -> [("network id", ppString "Nothing")]
  Txnetworkid (SJust nid) -> [("network id", pcNetwork nid)]
  ProposalProc props -> [("proposing procedure", ppList pcProposalProcedure (toList props))]
  VotingProc votes -> [("voting procedure", pcVotingProcedures votes)]
  CurrentTreasuryValue ctv -> [("current treasury value", ppStrictMaybe pcCoin ctv)]
  TreasuryDonation td -> [("treasury donation", pcCoin td)]

pcTxField ::
  forall era.
  Reflect era =>
  Proof era ->
  TxField era ->
  [(Text, PDoc)]
pcTxField proof x = case x of
  Body b -> [("txbody hash", ppSafeHash (hashAnnotated b)), ("body", pcTxBody proof b)]
  BodyI xs -> [("body", ppRecord "TxBody" (concat (map (pcTxBodyField proof) xs)))]
  TxWits w -> [("witnesses", pcWitnesses proof w)]
  WitnessesI ws -> [("witnesses", ppRecord "Witnesses" (concat (map (pcWitnessesField proof) ws)))]
  AuxData SNothing -> []
  AuxData (SJust auxdata) -> [("aux data", pcAuxData proof auxdata)]
  Valid (IsValid v) -> [("is valid", ppString (show v))]

pcWitnessesField ::
  forall era.
  Reflect era =>
  Proof era ->
  WitnessesField era ->
  [(Text, PDoc)]
pcWitnessesField proof x = case x of
  AddrWits set -> [("key wits", ppSet (pcWitVKey proof) set)]
  BootWits bwits -> [("boot wits", ppSet (\z -> ppVKey (bwKey z)) bwits)]
  ScriptWits mp -> [("script wits", ppMap pcScriptHash (pcScript proof) mp)]
  DataWits (TxDats m) -> [("data wits", ppMap pcDataHash pcData m)]
  RdmrWits m ->
    [("redeemer wits", ppMap ppPlutusPurposeAsIx (pcPair pcData pcExUnits) (unRedeemers m))]

pcPParamsField ::
  PParamsField era ->
  [(Text, PDoc)]
pcPParamsField x = case x of
  MinfeeA coin -> [("minfeeA", pcCoin coin)]
  MinfeeB coin -> [("minfeeB", pcCoin coin)]
  MaxBBSize natural -> [("maxBBsize", ppWord32 natural)]
  MaxTxSize natural -> [("maxTxsize", ppWord32 natural)]
  MaxBHSize natural -> [("maxBHsize", ppWord16 natural)]
  KeyDeposit coin -> [("keydeposit", pcCoin coin)]
  PoolDeposit coin -> [("pooldeposit", pcCoin coin)]
  EMax n -> [("emax", ppEpochInterval n)]
  NOpt natural -> [("NOpt", ppNatural natural)]
  A0 i -> [("A0", viaShow i)]
  Rho u -> [("Rho", ppUnitInterval u)]
  Tau u -> [("Tau", ppUnitInterval u)]
  D u -> [("D", ppUnitInterval u)]
  ExtraEntropy n -> [("extraEntropy", ppNonce n)]
  ProtocolVersion protVer -> [("ProtocolVersion", ppProtVer protVer)]
  MinPoolCost coin -> [("minPoolCost", pcCoin coin)]
  MinUTxOValue coin -> [("minUTxOValue", pcCoin coin)]
  CoinPerUTxOWord (CoinPerWord c) -> [("coinPerUTxOWord", pcCoin c)]
  CoinPerUTxOByte (CoinPerByte c) -> [("coinPerUTxOByte", pcCoin c)]
  Costmdls _ -> [("costmodels", ppString "?")]
  Fields.Prices prices -> [("prices", ppPrices prices)]
  MaxTxExUnits e -> [("maxTxExUnits", pcExUnits e)]
  MaxBlockExUnits e -> [("maxBlockExUnits", pcExUnits e)]
  MaxValSize n -> [("maxValSize", ppNatural n)]
  CollateralPercentage n -> [("Collateral%", ppNatural n)]
  MaxCollateralInputs n -> [("maxCollateralInputs", ppNatural n)]
  PoolVotingThreshold _ -> [("PoolVotingThresholds", ppString "?")]
  DRepVotingThreshold _ -> [("DRepVotingThresholds", ppString "?")]
  MinCommitteeSize n -> [("minCommitteeSize", ppNatural n)]
  CommitteeTermLimit n -> [("committeeTermLimit", ppEpochInterval n)]
  GovActionExpiration epochNo -> [("govActionExpire", ppEpochInterval epochNo)]
  GovActionDeposit coin -> [("govActiondDeposit", pcCoin coin)]
  DRepDeposit coin -> [("drepdeposit", pcCoin coin)]
  DRepActivity epochNo -> [("drepActivity", ppEpochInterval epochNo)]

-- =======================================================
-- @@
-- Pretty printers for PredicateFailures can be difficult because they often have componenents like
-- PredicateFailure (EraRule "LEDGER" era)
-- where both PredicateFailure and (EraRule "LEDGER" era) are type families.
-- Note that field can be different for every era. For example look at the type of the constructor
-- LedgerFailure :: PredicateFailure (EraRule "LEDGER" era) -> ShelleyLedgersPredFailure era
-- So we must resort to case analysis over Proofs
-- So for some EraRule "XXX", we write ppXXX :: Proof era -> PredicateFailure (EraRule "XXX") -> PDoc
-- Complicated because some Eras do NOT have type instances for all (EraRule "XXX")

ppUTXOW :: Reflect era => Proof era -> PredicateFailure (EraRule "UTXOW" era) -> PDoc
ppUTXOW Shelley x = ppShelleyUtxowPredFailure x
ppUTXOW Allegra x = ppShelleyUtxowPredFailure x
ppUTXOW Mary x = ppShelleyUtxowPredFailure x
ppUTXOW Alonzo x = ppAlonzoUtxowPredFailure x
ppUTXOW p@Babbage x = ppBabbageUtxowPredFailure p x
ppUTXOW p@Conway x = ppConwayUtxowPredFailure p x

ppUTXOS :: Proof era -> PredicateFailure (EraRule "UTXOS" era) -> PDoc
ppUTXOS Alonzo x = ppUtxosPredicateFailure x
ppUTXOS Babbage x = ppUtxosPredicateFailure x
ppUTXOS Conway x = prettyA x
ppUTXOS proof _ =
  error
    ( "Only the AlonzoEra, BabbageEra, and ConwayEra have a (PredicateFailure (EraRule \"UTXOS\" era))."
        ++ "This Era is "
        ++ show proof
    )

ppDELEGS :: Proof era -> PredicateFailure (EraRule "DELEGS" era) -> PDoc
ppDELEGS p@Shelley x = ppShelleyDelegsPredFailure p x
ppDELEGS p@Allegra x = ppShelleyDelegsPredFailure p x
ppDELEGS p@Mary x = ppShelleyDelegsPredFailure p x
ppDELEGS p@Alonzo x = ppShelleyDelegsPredFailure p x
ppDELEGS p@Babbage x = ppShelleyDelegsPredFailure p x
ppDELEGS p@Conway _ =
  error
    ( "Only the Shelley, Allegra, Mary, Alonzo, and Babbage era have a (PredicateFailure (EraRule \"DELEGS\" era))."
        ++ "This Era is "
        ++ show p
    )

ppDELEG :: Proof era -> PredicateFailure (EraRule "DELEG" era) -> PDoc
ppDELEG Shelley x = ppShelleyDelegPredFailure x
ppDELEG Allegra x = ppShelleyDelegPredFailure x
ppDELEG Mary x = ppShelleyDelegPredFailure x
ppDELEG Alonzo x = ppShelleyDelegPredFailure x
ppDELEG Babbage x = ppShelleyDelegPredFailure x
ppDELEG Conway x = ppConwayDelegPredFailure x

ppPOOL :: Proof era -> PredicateFailure (EraRule "POOL" era) -> PDoc
ppPOOL Shelley x = ppShelleyPoolPredFailure x
ppPOOL Allegra x = ppShelleyPoolPredFailure x
ppPOOL Mary x = ppShelleyPoolPredFailure x
ppPOOL Alonzo x = ppShelleyPoolPredFailure x
ppPOOL Babbage x = ppShelleyPoolPredFailure x
ppPOOL Conway x = ppShelleyPoolPredFailure x

ppLEDGER :: Reflect era => Proof era -> PredicateFailure (EraRule "LEDGER" era) -> PDoc
ppLEDGER p@Shelley x = ppShelleyLedgerPredFailure p x
ppLEDGER p@Allegra x = ppShelleyLedgerPredFailure p x
ppLEDGER p@Mary x = ppShelleyLedgerPredFailure p x
ppLEDGER p@Alonzo x = ppShelleyLedgerPredFailure p x
ppLEDGER p@Babbage x = ppShelleyLedgerPredFailure p x
ppLEDGER p@Conway x = ppConwayLedgerPredFailure p x

ppUTXO :: Proof era -> PredicateFailure (EraRule "UTXO" era) -> PDoc
ppUTXO Shelley x = ppShelleyUtxoPredFailure x
ppUTXO Allegra x = ppAllegraUtxoPredFailure x
ppUTXO Mary x = ppAllegraUtxoPredFailure x
ppUTXO Alonzo x = ppAlonzoUtxoPredFailure x
ppUTXO Babbage x = ppBabbageUtxoPredFailure x
ppUTXO Conway x = ppConwayUtxoPredFailure x

ppLEDGERS :: Proof era -> PredicateFailure (EraRule "LEDGERS" era) -> PDoc
ppLEDGERS p@Shelley x = unReflect ppShelleyLedgersPredFailure p x
ppLEDGERS p@Allegra x = unReflect ppShelleyLedgersPredFailure p x
ppLEDGERS p@Mary x = unReflect ppShelleyLedgersPredFailure p x
ppLEDGERS p@Alonzo x = unReflect ppShelleyLedgersPredFailure p x
ppLEDGERS p@Babbage x = unReflect ppShelleyLedgersPredFailure p x
ppLEDGERS p@Conway x = unReflect ppShelleyLedgersPredFailure p x

ppDELPL :: Proof era -> PredicateFailure (EraRule "DELPL" era) -> PDoc
ppDELPL p@Shelley x = ppShelleyDelplPredFailure p x
ppDELPL p@Allegra x = ppShelleyDelplPredFailure p x
ppDELPL p@Mary x = ppShelleyDelplPredFailure p x
ppDELPL p@Alonzo x = ppShelleyDelplPredFailure p x
ppDELPL p@Babbage x = ppShelleyDelplPredFailure p x
ppDELPL p@Conway _ =
  error
    ( "Only the Shelley, Allegra, Mary, Alonzo, and Babbage era have a (PredicateFailure (EraRule \"DELPL\" era))."
        ++ "This Era is "
        ++ show p
    )

ppNEWEPOCH :: Proof era -> PredicateFailure (EraRule "NEWEPOCH" era) -> PDoc
ppNEWEPOCH Shelley x = ppShelleyNewEpochPredicateFailure x
ppNEWEPOCH Allegra x = ppShelleyNewEpochPredicateFailure x
ppNEWEPOCH Mary x = ppShelleyNewEpochPredicateFailure x
ppNEWEPOCH Alonzo x = ppShelleyNewEpochPredicateFailure x
ppNEWEPOCH Babbage x = ppShelleyNewEpochPredicateFailure x
ppNEWEPOCH Conway x = ppConwayNewEpochPredFailure x

ppEPOCH :: Proof era -> PredicateFailure (EraRule "EPOCH" era) -> PDoc
ppEPOCH Shelley x = ppShelleyEpochPredFailure x
ppEPOCH Allegra x = ppShelleyEpochPredFailure x
ppEPOCH Mary x = ppShelleyEpochPredFailure x
ppEPOCH Alonzo x = ppShelleyEpochPredFailure x
ppEPOCH Babbage x = ppShelleyEpochPredFailure x
ppEPOCH Conway _ = ppString "PredicateFailure (ConwayEPOCH era) = Void, and can never Fail"

-- | A bit different since it is NOT of the form: PredicateFailure (EraRule "UPEC" era)
--   but instead, the type family UpecPredFailurePV pv era
--   where, type UpecPredFailure era = UpecPredFailurePV (ProtVerLow era) era
--   But the effect is still the same. The Proof era, fixes the type family result.
ppUPEC :: Proof era -> UpecPredFailure era -> PDoc
ppUPEC Shelley x = ppUpecPredicateFailure x
ppUPEC Mary x = ppUpecPredicateFailure x
ppUPEC Alonzo x = ppUpecPredicateFailure x
ppUPEC Allegra x = ppUpecPredicateFailure x
ppUPEC Babbage x = ppUpecPredicateFailure x
ppUPEC Conway x = absurd x

-- | A bit different since it is NOT of the form: PredicateFailure (EraRule "LEDGERS" era)
--   but instead:  State (EraRule "LEDGERS" era)
--   But the effect is still the same. The Proof era, fixes the type family result.
ppStateLEDGERS :: Proof era -> State (EraRule "LEDGERS" era) -> PDoc
ppStateLEDGERS p@Shelley = pcLedgerState p
ppStateLEDGERS p@Allegra = pcLedgerState p
ppStateLEDGERS p@Mary = pcLedgerState p
ppStateLEDGERS p@Alonzo = pcLedgerState p
ppStateLEDGERS p@Babbage = pcLedgerState p
ppStateLEDGERS p@Conway = pcLedgerState p

-- ============================================================================
-- pretty printers for concrete types that are the target of PredicateFailure type instances

ppShelleyDelegsPredFailure :: forall era. Proof era -> ShelleyDelegsPredFailure era -> PDoc
ppShelleyDelegsPredFailure _ (Shelley.DelegateeNotRegisteredDELEG x) = pcKeyHash x
ppShelleyDelegsPredFailure _ (WithdrawalsNotInRewardsDELEGS x) = ppMap pcRewardAccount pcCoin x
ppShelleyDelegsPredFailure p (DelplFailure x) = ppDELPL p x

instance Reflect era => PrettyA (ShelleyDelegsPredFailure era) where
  prettyA = ppShelleyDelegsPredFailure reify

ppConwayUtxoPredFailure ::
  forall era.
  Reflect era =>
  ConwayRules.ConwayUtxoPredFailure era ->
  PDoc
ppConwayUtxoPredFailure = \case
  ConwayRules.UtxosFailure yy -> ppSexp "UtxosFailure" [ppUTXOS @era reify yy]
  ConwayRules.BadInputsUTxO txins -> ppSexp "BadInputsUTxO" [ppSet pcTxIn txins]
  ConwayRules.OutsideValidityIntervalUTxO vi slot ->
    ppRecord
      "OutsideValidityIntervalUTxO"
      [ ("provided interval", ppValidityInterval vi)
      , ("current slot", pcSlotNo slot)
      ]
  ConwayRules.MaxTxSizeUTxO actual maxs ->
    ppRecord "MaxTxSizeUTxO" [("Actual", ppInteger actual), ("max transaction size", ppInteger maxs)]
  ConwayRules.InputSetEmptyUTxO -> ppString "InputSetEmptyUTxO"
  ConwayRules.FeeTooSmallUTxO computed supplied ->
    ppRecord
      "FeeTooSmallUTxO"
      [ ("min fee for this transaction", pcCoin computed)
      , ("fee supplied by this transaction", pcCoin supplied)
      ]
  ConwayRules.ValueNotConservedUTxO consumed produced ->
    ppRecord
      "ValueNotConservedUTxO"
      [("coin consumed", pcVal @era reify consumed), ("coin produced", pcVal @era reify produced)]
  ConwayRules.WrongNetwork n add ->
    ppRecord
      "WrongNetwork"
      [ ("expected network id", ppNetwork n)
      , ("set of addresses with wrong network id", ppSet pcAddr add)
      ]
  ConwayRules.WrongNetworkWithdrawal n accnt ->
    ppRecord
      "WrongNetworkWithdrawal"
      [ ("expected network id", ppNetwork n)
      , ("set reward address with wrong network id", ppSet pcRewardAccount accnt)
      ]
  ConwayRules.OutputTooSmallUTxO xs ->
    ppRecord
      "OutputTooSmallUTxO"
      [("list of supplied transaction outputs that are too small", ppList (pcTxOut reify) xs)]
  ConwayRules.OutputBootAddrAttrsTooBig xs ->
    ppRecord
      "OutputBootAddrAttrsTooBig"
      [("list of supplied bad transaction outputs", ppList (pcTxOut reify) xs)]
  ConwayRules.OutputTooBigUTxO xs ->
    ppSexp
      "OutputTooBigUTxO"
      [ ppList
          ( \(a, b, c) ->
              ppRecord'
                ""
                [("actual size", ppInt a), ("PParam max value", ppInt b), ("TxOut", pcTxOut reify c)]
          )
          xs
      ]
  ConwayRules.InsufficientCollateral c1 c2 ->
    ppRecord
      "InsufficientCollateral"
      [ ("balance computed", prettyA c1)
      , ("the required collateral for the given fee", pcCoin c2)
      ]
  ConwayRules.ScriptsNotPaidUTxO u -> ppSexp "ScriptsNotPaidUTxO" [pcUTxO reify u]
  ConwayRules.ExUnitsTooBigUTxO e1 e2 ->
    ppRecord
      "ExUnitsTooBigUTxO"
      [ ("Max EXUnits from the protocol parameters", pcExUnits e1)
      , ("EXUnits supplied", pcExUnits e2)
      ]
  ConwayRules.CollateralContainsNonADA v -> ppSexp "CollateralContainsNonADA" [pcVal (reify @era) v]
  ConwayRules.WrongNetworkInTxBody n1 n2 ->
    ppRecord
      "WrongNetworkInTxBody"
      [ ("Actual Network ID", ppNetwork n1)
      , ("Network ID in transaction body", ppNetwork n2)
      ]
  ConwayRules.OutsideForecast slot -> ppRecord "OutsideForecast" [("slot number outside consensus forecast range", pcSlotNo slot)]
  ConwayRules.TooManyCollateralInputs n1 n2 ->
    ppRecord
      "TooManyCollateralInputs"
      [ ("Max allowed collateral inputs", ppNatural n1)
      , ("Number of collateral inputs", ppNatural n2)
      ]
  ConwayRules.NoCollateralInputs -> ppSexp " NoCollateralInputs" []
  ConwayRules.IncorrectTotalCollateralField c1 c2 ->
    ppRecord
      "IncorrectTotalCollateralField"
      [("collateral provided", prettyA c1), ("collateral declared", pcCoin c2)]
  ConwayRules.BabbageOutputTooSmallUTxO xs ->
    ppSexp "BabbageOutputTooSmallUTxO" [ppList (ppPair (pcTxOut reify) pcCoin) xs]
  ConwayRules.BabbageNonDisjointRefInputs xs ->
    ppSexp "BabbageNonDisjointRefInputs" [ppList pcTxIn (toList xs)]

instance
  Reflect era =>
  PrettyA (ConwayRules.ConwayUtxoPredFailure era)
  where
  prettyA = ppConwayUtxoPredFailure

ppBabbageUtxoPredFailure :: Reflect era => BabbageUtxoPredFailure era -> PDoc
ppBabbageUtxoPredFailure (AlonzoInBabbageUtxoPredFailure x) = ppAlonzoUtxoPredFailure x
ppBabbageUtxoPredFailure (IncorrectTotalCollateralField c1 c2) =
  ppRecord
    "IncorrectTotalCollateralField"
    [("collateral provided", prettyA c1), ("collateral declared", prettyA c2)]
ppBabbageUtxoPredFailure (BabbageOutputTooSmallUTxO xs) =
  ppSexp "BabbageOutputTooSmallUTxO" [ppList (ppPair (pcTxOut reify) pcCoin) xs]
ppBabbageUtxoPredFailure (BabbageNonDisjointRefInputs xs) =
  ppSexp "BabbageNonDisjointRefInputs" [ppList pcTxIn (toList xs)]

instance Reflect era => PrettyA (BabbageUtxoPredFailure era) where
  prettyA = ppBabbageUtxoPredFailure

ppShelleyLedgersPredFailure :: Reflect era => Proof era -> ShelleyLedgersPredFailure era -> PDoc
ppShelleyLedgersPredFailure p (LedgerFailure x) = ppLEDGER p x

instance Reflect era => PrettyA (ShelleyLedgersPredFailure era) where
  prettyA = ppShelleyLedgersPredFailure reify

ppConwayLedgerPredFailure :: Reflect era => Proof era -> ConwayLedgerPredFailure era -> PDoc
ppConwayLedgerPredFailure proof x = case x of
  ConwayWdrlNotDelegatedToDRep s -> ppSexp "ConwayWdrlNotDelegatedToDRep" [prettyA s]
  ConwayTreasuryValueMismatch c1 c2 -> ppSexp "ConwayTreasuryValueMismatch" [pcCoin c1, pcCoin c2]
  ConwayGovFailure y -> case proof of
    Conway -> ppSexp "ConwayGovFailure" [ppConwayGovPredFailure y]
    _ ->
      error
        ("Only the ConwayEra has a (PredicateFailure (EraRule \"GOV\" era)). This Era is " ++ show proof)
  ConwayUtxowFailure y -> ppUTXOW proof y
  ConwayCertsFailure pf -> case proof of
    Conway -> ppConwayCertsPredFailure proof pf
    _ ->
      error
        ("Only the ConwayEra has a (PredicateFailure (EraRule \"CERTS\" era)). This Era is " ++ show proof)
  ConwayTxRefScriptsSizeTooBig s1 s2 ->
    ppRecord
      "ConwayTxRefScriptsSizeTooBig"
      [ ("Computed sum of reference script size", ppInt s1)
      , ("Maximum allowed total reference script size", ppInt s2)
      ]
  ConwayMempoolFailure t ->
    ppSexp "ConwayMempoolFailure" [text t]

instance Reflect era => PrettyA (ConwayLedgerPredFailure era) where
  prettyA = ppConwayLedgerPredFailure reify

ppConwayCertPredFailure :: Proof era -> ConwayRules.ConwayCertPredFailure era -> PDoc
ppConwayCertPredFailure proof x = case x of
  ConwayRules.DelegFailure pf -> ppSexp "DelegFailure" [ppDELEG proof pf] -- (PredicateFailure (EraRule "DELEG" era))
  ConwayRules.PoolFailure pf -> ppSexp ".PoolFailure" [ppPOOL proof pf] -- (PredicateFailure (EraRule "POOL" era))
  ConwayRules.GovCertFailure pf -> case proof of
    Conway -> ppSexp "GovCertFailure" [ppConwayGovCertPredFailure pf] -- (PredicateFailure (EraRule "GOVCERT" era))
    _ ->
      error
        ("Only the ConwayEra has a (PredicateFailure (EraRule \"GOVCERT\" era)). This Era is " ++ show proof)

instance Reflect era => PrettyA (ConwayRules.ConwayCertPredFailure era) where
  prettyA = ppConwayCertPredFailure reify

ppConwayGovCertPredFailure :: ConwayGovCertPredFailure era -> PDoc
ppConwayGovCertPredFailure z = case z of
  ConwayDRepAlreadyRegistered x -> ppSexp "ConwayDRepAlreadyRegistered" [pcCredential x]
  ConwayDRepNotRegistered x -> ppSexp "ConwayDRepNotRegistered" [pcCredential x]
  ConwayDRepIncorrectDeposit c1 c2 -> ppSexp "ConwayDRepIncorrectDeposit" [pcCoin c1, pcCoin c2]
  ConwayCommitteeHasPreviouslyResigned x -> ppSexp "ConwayCommitteeHasPreviouslyResigned" [pcCredential x]
  ConwayDRepIncorrectRefund c1 c2 -> ppSexp "ConwayDRepIncorrectRefund" [pcCoin c1, pcCoin c2]
  ConwayCommitteeIsUnknown c -> ppSexp "ConwayCommitteeIsUnknown" [pcCredential c]

instance PrettyA (ConwayGovCertPredFailure era) where
  prettyA = ppConwayGovCertPredFailure

ppConwayCertsPredFailure :: Proof era -> ConwayCertsPredFailure era -> PDoc
ppConwayCertsPredFailure proof x = case x of
  WithdrawalsNotInRewardsCERTS m -> ppSexp "WithdrawalsNotInRewardsCERTS" [ppMap pcRewardAccount pcCoin m]
  CertFailure pf -> case proof of
    Conway -> ppSexp " CertFailure" [ppConwayCertPredFailure proof pf] -- !(PredicateFailure (EraRule "CERT" era))
    _ ->
      error
        ("Only the ConwayEra has a (PredicateFailure (EraRule \"CERT\" era)). This Era is " ++ show proof)

instance Reflect era => PrettyA (ConwayCertsPredFailure era) where
  prettyA = ppConwayCertsPredFailure reify

ppConwayGovPredFailure :: ConwayGovPredFailure era -> PDoc
ppConwayGovPredFailure x = case x of
  GovActionsDoNotExist c -> ppSexp "GovActionsDoNotExist" [prettyA c]
  MalformedProposal ga -> ppSexp "MalformedProposal" [pcGovAction ga]
  ProposalProcedureNetworkIdMismatch racnt nw ->
    ppSexp "ProposalProcedureNetworkIdMismatch" [pcRewardAccount racnt, pcNetwork nw]
  TreasuryWithdrawalsNetworkIdMismatch sr nw ->
    ppSexp "TreasuryWithdrawalsNetworkIdMismatch" [ppSet pcRewardAccount sr, pcNetwork nw]
  ProposalDepositIncorrect c1 c2 -> ppSexp "ProposalDepositIncorrect" [pcCoin c1, pcCoin c2]
  DisallowedVoters m -> ppSexp "DisallowedVoters" [prettyA m]
  ConflictingCommitteeUpdate s ->
    ppSexp "ConflictingCommitteeUpdate" [ppSet pcCredential s]
  ExpirationEpochTooSmall m -> ppSexp "ExpirationEpochTooSmall" [ppMap pcCredential ppEpochNo m]
  InvalidPrevGovActionId p -> ppSexp "InvalidPrevGovActionId" [pcProposalProcedure p]
  VotingOnExpiredGovAction m ->
    ppSexp "VotingOnExpiredGovAction" [prettyA m]
  ProposalCantFollow s1 p1 p2 ->
    ppSexp "ProposalCantFollow" [ppStrictMaybe pcGovPurposeId s1, ppProtVer p1, ppProtVer p2]
  InvalidPolicyHash a b ->
    ppSexp "InvalidPolicyHash" [ppStrictMaybe prettyA a, ppStrictMaybe prettyA b]
  DisallowedProposalDuringBootstrap p ->
    ppSexp "DisallowedProposalDuringBootstrap" [pcProposalProcedure p]
  DisallowedVotesDuringBootstrap m -> ppSexp "DisallowedVotesDuringBootstrap" [prettyA m]
  VotersDoNotExist m -> ppSexp "VotersDoNotExist" [prettyA m]
  ZeroTreasuryWithdrawals ga -> ppSexp "ZeroTreasuryWithdrawals" [pcGovAction ga]

instance PrettyA (ConwayGovPredFailure era) where
  prettyA = ppConwayGovPredFailure

-- Note there is both: ppShelleyLedgerPredFailure and ppShelleyLedgersPredFailure (Ledger vs Ledgers)
ppShelleyLedgerPredFailure :: Reflect era => Proof era -> ShelleyLedgerPredFailure era -> PDoc
ppShelleyLedgerPredFailure proof (UtxowFailure x) = ppUTXOW proof x
ppShelleyLedgerPredFailure proof (DelegsFailure x) = ppDELEGS proof x

instance Reflect era => PrettyA (ShelleyLedgerPredFailure era) where
  prettyA = ppShelleyLedgerPredFailure reify

ppConwayUtxowPredFailure ::
  Reflect era =>
  Proof era ->
  ConwayRules.ConwayUtxowPredFailure era ->
  PDoc
ppConwayUtxowPredFailure proof = \case
  ConwayRules.UtxoFailure x -> ppSexp "UtxoFailure" [ppUTXO proof x]
  ConwayRules.InvalidWitnessesUTXOW vkeyws ->
    ppSexp "InvalidWitnessesUTXOW" [ppList ppVKey vkeyws]
  ConwayRules.MissingVKeyWitnessesUTXOW whs ->
    ppSexp "MissingVKeyWitnessesUTXOW" [ppWitHashes whs]
  ConwayRules.MissingScriptWitnessesUTXOW m ->
    ppSexp "MissingScriptWitnessesUTXOW" [ppSet pcScriptHash m]
  ConwayRules.ScriptWitnessNotValidatingUTXOW m ->
    ppSexp "ScriptWitnessNotValidatingUTXOW" [ppSet pcScriptHash m]
  ConwayRules.MissingTxBodyMetadataHash m ->
    ppSexp " MissingTxMetadata" [ppAuxiliaryDataHash m]
  ConwayRules.MissingTxMetadata m ->
    ppSexp " MissingTxMetadata" [ppAuxiliaryDataHash m]
  ConwayRules.ConflictingMetadataHash h1 h2 ->
    ppRecord
      "ConflictingMetadataHash"
      [("Hash in the body", ppAuxiliaryDataHash h1), ("Hash of full metadata", ppAuxiliaryDataHash h2)]
  ConwayRules.InvalidMetadata ->
    ppSexp "InvalidMetadata" []
  ConwayRules.ExtraneousScriptWitnessesUTXOW m ->
    ppSexp "ExtraneousScriptWitnessesUTXOW" [ppSet pcScriptHash m]
  ConwayRules.MissingRedeemers xs ->
    ppSexp "MissingRedeemers" [ppList (ppPair ppPlutusPurposeAsItem prettyA) xs]
  ConwayRules.MissingRequiredDatums s1 s2 ->
    ppRecord
      "MissingRequiredDatums"
      [ ("missing data hashes", ppSet ppSafeHash s1)
      , ("received data hashes", ppSet ppSafeHash s2)
      ]
  ConwayRules.NotAllowedSupplementalDatums s1 s2 ->
    ppRecord
      "NotAllowedSupplementalDatums"
      [ ("unallowed data hashes", ppSet ppSafeHash s1)
      , ("acceptable data hashes", ppSet ppSafeHash s2)
      ]
  ConwayRules.PPViewHashesDontMatch h1 h2 ->
    ppRecord
      "PPViewHashesDontMatch"
      [ ("PPHash in the TxBody", ppStrictMaybe ppSafeHash h1)
      , ("PPHash Computed from the current Protocol Parameters", ppStrictMaybe ppSafeHash h2)
      ]
  ConwayRules.UnspendableUTxONoDatumHash x ->
    ppSexp "UnspendableUTxONoDatumHash" [ppSet pcTxIn x]
  ConwayRules.ExtraRedeemers x ->
    ppSexp "ExtraRedeemers" [ppList ppPlutusPurposeAsIx x]
  ConwayRules.MalformedScriptWitnesses x -> ppSexp "MalformedScriptWitnesses" [ppSet pcScriptHash x]
  ConwayRules.MalformedReferenceScripts x -> ppSexp "MalformedReferenceScripts" [ppSet pcScriptHash x] -- !(Set (ScriptHash (EraCrypto era)))

instance
  Reflect era =>
  PrettyA (ConwayRules.ConwayUtxowPredFailure era)
  where
  prettyA = ppConwayUtxowPredFailure reify

ppBabbageUtxowPredFailure ::
  Reflect era =>
  Proof era ->
  BabbageUtxowPredFailure era ->
  PDoc
ppBabbageUtxowPredFailure proof failure = case failure of
  AlonzoInBabbageUtxowPredFailure x -> ppSexp "AlonzoInBabbageUtxowPredFailure" [ppAlonzoUtxowPredFailure x]
  Cardano.Ledger.Babbage.Rules.UtxoFailure x -> ppSexp "UtxoFailure" [ppUTXO proof x]
  MalformedScriptWitnesses x -> ppSexp "MalformedScriptWitnesses" [ppSet pcScriptHash x]
  MalformedReferenceScripts x -> ppSexp "MalformedReferenceScripts" [ppSet pcScriptHash x] -- !(Set (ScriptHash (EraCrypto era)))

instance
  Reflect era =>
  PrettyA (BabbageUtxowPredFailure era)
  where
  prettyA = ppBabbageUtxowPredFailure reify

-- ================

ppBbodyPredicateFailure :: forall era. Reflect era => ShelleyBbodyPredFailure era -> PDoc
ppBbodyPredicateFailure (WrongBlockBodySizeBBODY x y) =
  ppRecord
    "WrongBlockBodySizeBBODY"
    [ ("actual computed BBody size", ppInt x)
    , ("claimed BBody Size in Header", ppInt y)
    ]
ppBbodyPredicateFailure (InvalidBodyHashBBODY h1 h2) =
  ppRecord
    "(InvalidBodyHashBBODY"
    [ ("actual hash", ppHash h1)
    , ("claimed hash", ppHash h2)
    ]
ppBbodyPredicateFailure (LedgersFailure x) =
  ppSexp "LedgersFailure" [ppLEDGERS @era reify x]

instance Reflect era => PrettyA (ShelleyBbodyPredFailure era) where
  prettyA = ppBbodyPredicateFailure

-- ================
ppConwayBbodyPredFail :: forall era. Reflect era => ConwayBbodyPredFailure era -> PDoc
ppConwayBbodyPredFail (ConwayRules.BodyRefScriptsSizeTooBig s1 s2) =
  ppRecord
    "BodyRefScriptsSizeTooBig"
    [ ("Computed sum of reference script size", ppInt s1)
    , ("Maximum allowed total reference script size", ppInt s2)
    ]
ppConwayBbodyPredFail (ConwayRules.TooManyExUnits e1 e2) =
  ppRecord
    "TooManyExUnits"
    [ ("Computed Sum of ExUnits for all plutus scripts", pcExUnits e1)
    , ("Maximum allowed by protocal parameters", pcExUnits e2)
    ]
ppConwayBbodyPredFail (ConwayRules.WrongBlockBodySizeBBODY x y) =
  ppRecord
    "WrongBlockBodySizeBBODY"
    [ ("actual computed BBody size", ppInt x)
    , ("claimed BBody Size in Header", ppInt y)
    ]
ppConwayBbodyPredFail (ConwayRules.InvalidBodyHashBBODY h1 h2) =
  ppRecord
    "(InvalidBodyHashBBODY"
    [ ("actual hash", ppHash h1)
    , ("claimed hash", ppHash h2)
    ]
ppConwayBbodyPredFail (ConwayRules.LedgersFailure x) =
  ppSexp "LedgersFailure" [ppLEDGERS @era reify x]

instance Reflect era => PrettyA (ConwayBbodyPredFailure era) where
  prettyA = ppConwayBbodyPredFail

ppAlonzoBbodyPredFail :: Reflect era => AlonzoBbodyPredFailure era -> PDoc
ppAlonzoBbodyPredFail (ShelleyInAlonzoBbodyPredFailure x) =
  ppSexp "ShelleyInAlonzoPredFail" [ppBbodyPredicateFailure x]
ppAlonzoBbodyPredFail (TooManyExUnits e1 e2) =
  ppRecord
    "TooManyExUnits"
    [ ("Computed Sum of ExUnits for all plutus scripts", pcExUnits e1)
    , ("Maximum allowed by protocal parameters", pcExUnits e2)
    ]

instance Reflect era => PrettyA (AlonzoBbodyPredFailure era) where
  prettyA = ppAlonzoBbodyPredFail

-- ===============
ppTickPredicateFailure ::
  forall era.
  Reflect era =>
  ShelleyTickPredFailure era ->
  PDoc
ppTickPredicateFailure (NewEpochFailure x) = ppNEWEPOCH @era reify x
ppTickPredicateFailure (RupdFailure _) =
  ppString "RupdPredicateFailure has no constructors"

instance Reflect era => PrettyA (ShelleyTickPredFailure era) where
  prettyA = ppTickPredicateFailure

ppShelleyNewEpochPredicateFailure ::
  forall era. Reflect era => ShelleyNewEpochPredFailure era -> PDoc
ppShelleyNewEpochPredicateFailure (EpochFailure x) = ppEPOCH @era reify x
ppShelleyNewEpochPredicateFailure (CorruptRewardUpdate x) =
  ppSexp "CorruptRewardUpdate" [ppRewardUpdate x]
ppShelleyNewEpochPredicateFailure (MirFailure _) =
  error "In the Conway era, there is no (EraRule MIR) type instance."

instance Reflect era => PrettyA (ShelleyNewEpochPredFailure era) where
  prettyA = ppShelleyNewEpochPredicateFailure

ppConwayNewEpochPredFailure :: ConwayNewEpochPredFailure era -> PDoc
ppConwayNewEpochPredFailure (ConwayRules.CorruptRewardUpdate x) =
  ppSexp "CorruptRewardUpdate" [ppRewardUpdate x]

instance PrettyA (ConwayNewEpochPredFailure era) where
  prettyA = ppConwayNewEpochPredFailure

-- ===============

ppShelleyEpochPredFailure :: forall era. Reflect era => ShelleyEpochPredFailure era -> PDoc
ppShelleyEpochPredFailure (PoolReapFailure _) =
  ppString "PoolreapPredicateFailure has no constructors"
ppShelleyEpochPredFailure (SnapFailure _) =
  ppString "SnapPredicateFailure has no constructors"
ppShelleyEpochPredFailure (UpecFailure x) = ppUPEC @era reify x

instance Reflect era => PrettyA (ShelleyEpochPredFailure era) where
  prettyA = ppShelleyEpochPredFailure

-- This type has no constructors, so the show instance is fine.
instance PrettyA (ShelleyPoolreapPredFailure era) where
  prettyA = viaShow

-- This type has no constructors, so the show instance is fine.
instance PrettyA (ShelleySnapPredFailure era) where
  prettyA = viaShow

-- ===============

ppUpecPredicateFailure :: ShelleyUpecPredFailure era -> PDoc
ppUpecPredicateFailure (NewPpFailure x) = ppNewppPredicateFailure x

instance PrettyA (ShelleyUpecPredFailure era) where
  prettyA = ppUpecPredicateFailure

-- ===============
ppNewppPredicateFailure :: ShelleyNewppPredFailure era -> PDoc
ppNewppPredicateFailure (UnexpectedDepositPot c1 c2) =
  ppRecord
    "UnexpectedDepositPot"
    [ ("The total outstanding deposits", pcCoin c1)
    , ("The deposit pot", pcCoin c2)
    ]

instance PrettyA (ShelleyNewppPredFailure era) where prettyA = ppNewppPredicateFailure

-- =========================================
-- Predicate Failure for Alonzo UTXOW

ppAlonzoUtxowPredFailure ::
  Reflect era =>
  AlonzoUtxowPredFailure era ->
  PDoc
ppAlonzoUtxowPredFailure (ShelleyInAlonzoUtxowPredFailure x) = prettyA x
ppAlonzoUtxowPredFailure (MissingRedeemers xs) =
  ppSexp "MissingRedeemers" [ppList (ppPair ppPlutusPurposeAsItem prettyA) xs]
ppAlonzoUtxowPredFailure (MissingRequiredDatums s1 s2) =
  ppRecord
    "MissingRequiredDatums"
    [ ("missing data hashes", ppSet ppSafeHash s1)
    , ("received data hashes", ppSet ppSafeHash s2)
    ]
ppAlonzoUtxowPredFailure (NotAllowedSupplementalDatums s1 s2) =
  ppRecord
    "NotAllowedSupplementalDatums"
    [ ("unallowed data hashes", ppSet ppSafeHash s1)
    , ("acceptable data hashes", ppSet ppSafeHash s2)
    ]
ppAlonzoUtxowPredFailure (PPViewHashesDontMatch h1 h2) =
  ppRecord
    "PPViewHashesDontMatch"
    [ ("PPHash in the TxBody", ppStrictMaybe ppSafeHash h1)
    , ("PPHash Computed from the current Protocol Parameters", ppStrictMaybe ppSafeHash h2)
    ]
ppAlonzoUtxowPredFailure (MissingRequiredSigners x) =
  ppSexp "MissingRequiredSigners" [ppSet pcKeyHash x]
ppAlonzoUtxowPredFailure (UnspendableUTxONoDatumHash x) =
  ppSexp "UnspendableUTxONoDatumHash" [ppSet pcTxIn x]
ppAlonzoUtxowPredFailure (ExtraRedeemers x) =
  ppSexp "ExtraRedeemers" [ppList ppPlutusPurposeAsIx x]

instance
  Reflect era =>
  PrettyA (AlonzoUtxowPredFailure era)
  where
  prettyA = ppAlonzoUtxowPredFailure

-- ====================================================
-- Predicate Failure for Shelley UTXOW

ppShelleyUtxowPredFailure :: forall era. Reflect era => ShelleyUtxowPredFailure era -> PDoc
ppShelleyUtxowPredFailure (InvalidWitnessesUTXOW vkeyws) =
  ppSexp "InvalidWitnessesUTXOW" [ppList ppVKey vkeyws]
ppShelleyUtxowPredFailure (MissingVKeyWitnessesUTXOW whs) =
  ppSexp "MissingVKeyWitnessesUTXOW" [ppWitHashes whs]
ppShelleyUtxowPredFailure (MissingScriptWitnessesUTXOW m) =
  ppSexp "MissingScriptWitnessesUTXOW" [ppSet pcScriptHash m]
ppShelleyUtxowPredFailure (ScriptWitnessNotValidatingUTXOW m) =
  ppSexp "ScriptWitnessNotValidatingUTXOW" [ppSet pcScriptHash m]
ppShelleyUtxowPredFailure (Shelley.UtxoFailure m) = ppSexp "UtxoFailure" [ppUTXO (reify @era) m]
ppShelleyUtxowPredFailure (MIRInsufficientGenesisSigsUTXOW m) =
  ppSexp "MIRInsufficientGenesisSigsUTXOW" [ppSet pcKeyHash m]
ppShelleyUtxowPredFailure (MissingTxBodyMetadataHash m) =
  ppSexp " MissingTxMetadata" [ppAuxiliaryDataHash m]
ppShelleyUtxowPredFailure (MissingTxMetadata m) =
  ppSexp " MissingTxMetadata" [ppAuxiliaryDataHash m]
ppShelleyUtxowPredFailure (ConflictingMetadataHash h1 h2) =
  ppRecord
    "ConflictingMetadataHash"
    [("Hash in the body", ppAuxiliaryDataHash h1), ("Hash of full metadata", ppAuxiliaryDataHash h2)]
ppShelleyUtxowPredFailure InvalidMetadata =
  ppSexp "InvalidMetadata" []
ppShelleyUtxowPredFailure (ExtraneousScriptWitnessesUTXOW m) =
  ppSexp "ExtraneousScriptWitnessesUTXOW" [ppSet pcScriptHash m]

instance Reflect era => PrettyA (ShelleyUtxowPredFailure era) where
  prettyA = ppShelleyUtxowPredFailure

-- ========================================================
-- Predicate Failure for Alonzo UTXO

ppAlonzoUtxoPredFailure :: forall era. Reflect era => AlonzoUtxoPredFailure era -> PDoc
ppAlonzoUtxoPredFailure x = case x of
  Alonzo.BadInputsUTxO txins -> ppSexp "BadInputsUTxO" [ppSet pcTxIn txins]
  Alonzo.OutsideValidityIntervalUTxO vi slot ->
    ppRecord
      "OutsideValidityIntervalUTxO"
      [ ("provided interval", ppValidityInterval vi)
      , ("current slot", pcSlotNo slot)
      ]
  Alonzo.MaxTxSizeUTxO actual maxs ->
    ppRecord "MaxTxSizeUTxO" [("Actual", ppInteger actual), ("max transaction size", ppInteger maxs)]
  Alonzo.InputSetEmptyUTxO -> ppString "InputSetEmptyUTxO"
  Alonzo.FeeTooSmallUTxO computed supplied ->
    ppRecord
      "FeeTooSmallUTxO"
      [ ("min fee for this transaction", pcCoin computed)
      , ("fee supplied by this transaction", pcCoin supplied)
      ]
  Alonzo.ValueNotConservedUTxO consumed produced ->
    ppRecord
      "ValueNotConservedUTxO"
      [("coin consumed", pcVal @era reify consumed), ("coin produced", pcVal @era reify produced)]
  Alonzo.WrongNetwork n add ->
    ppRecord
      "WrongNetwork"
      [ ("expected network id", ppNetwork n)
      , ("set of addresses with wrong network id", ppSet pcAddr add)
      ]
  Alonzo.WrongNetworkWithdrawal n accnt ->
    ppRecord
      "WrongNetworkWithdrawal"
      [ ("expected network id", ppNetwork n)
      , ("set reward address with wrong network id", ppSet pcRewardAccount accnt)
      ]
  Alonzo.OutputTooSmallUTxO xs ->
    ppRecord
      "OutputTooSmallUTxO"
      [("list of supplied transaction outputs that are too small", ppList (pcTxOut reify) xs)]
  Alonzo.UtxosFailure yy -> ppSexp "UtxosFailure" [ppUTXOS @era reify yy]
  Alonzo.OutputBootAddrAttrsTooBig xs ->
    ppRecord
      "OutputBootAddrAttrsTooBig"
      [("list of supplied bad transaction outputs", ppList (pcTxOut reify) xs)]
  Alonzo.TriesToForgeADA -> ppString "TriesToForgeADA"
  Alonzo.OutputTooBigUTxO xs ->
    ppSexp
      "OutputTooBigUTxO"
      [ ppList
          ( \(a, b, c) ->
              ppRecord'
                ""
                [("actual size", ppInteger a), ("PParam max value", ppInteger b), ("TxOut", pcTxOut reify c)]
          )
          xs
      ]
  InsufficientCollateral c1 c2 ->
    ppRecord
      "InsufficientCollateral"
      [ ("balance computed", prettyA c1)
      , ("the required collateral for the given fee", pcCoin c2)
      ]
  ScriptsNotPaidUTxO u -> ppSexp "ScriptsNotPaidUTxO" [pcUTxO reify u]
  ExUnitsTooBigUTxO e1 e2 ->
    ppRecord
      "ExUnitsTooBigUTxO"
      [ ("Max EXUnits from the protocol parameters", pcExUnits e1)
      , ("EXUnits supplied", pcExUnits e2)
      ]
  CollateralContainsNonADA v -> ppSexp "CollateralContainsNonADA" [pcVal (reify @era) v]
  WrongNetworkInTxBody n1 n2 ->
    ppRecord
      "WrongNetworkInTxBody"
      [ ("Actual Network ID", ppNetwork n1)
      , ("Network ID in transaction body", ppNetwork n2)
      ]
  OutsideForecast slot -> ppRecord "OutsideForecast" [("slot number outside consensus forecast range", pcSlotNo slot)]
  TooManyCollateralInputs n1 n2 ->
    ppRecord
      "TooManyCollateralInputs"
      [ ("Max allowed collateral inputs", ppNatural n1)
      , ("Number of collateral inputs", ppNatural n2)
      ]
  NoCollateralInputs -> ppSexp " NoCollateralInputs" []

instance Reflect era => PrettyA (AlonzoUtxoPredFailure era) where
  prettyA = ppAlonzoUtxoPredFailure

-- =================================

ppShelleyDelegPredFailure :: ShelleyDelegPredFailure era -> PDoc
ppShelleyDelegPredFailure x = case x of
  StakeKeyAlreadyRegisteredDELEG cred -> ppSexp "StakeKeyAlreadyRegisteredDELEG" [pcCredential cred]
  StakeKeyInRewardsDELEG cred -> ppSexp "StakeKeyInRewardsDELEG" [pcCredential cred]
  Shelley.StakeKeyNotRegisteredDELEG cred -> ppSexp "StakeKeyNotRegisteredDELEG" [pcCredential cred]
  StakeKeyNonZeroAccountBalanceDELEG mcoin -> ppSexp " StakeKeyNonZeroAccountBalanceDELEG" [ppMaybe pcCoin mcoin]
  StakeDelegationImpossibleDELEG cred -> ppSexp "StakeDelegationImpossibleDELEG" [pcCredential cred]
  WrongCertificateTypeDELEG -> ppSexp "WrongCertificateTypeDELEG" []
  GenesisKeyNotInMappingDELEG kh -> ppSexp "GenesisKeyNotInMappingDELEG" [pcKeyHash kh]
  DuplicateGenesisDelegateDELEG kh -> ppSexp "DuplicateGenesisDelegateDELEG" [pcKeyHash kh]
  InsufficientForInstantaneousRewardsDELEG pot c1 c2 ->
    ppSexp
      "InsufficientForInstantaneousRewardsDELEG"
      [ppString (show pot), pcCoin c1, pcCoin c2]
  MIRCertificateTooLateinEpochDELEG s1 s2 ->
    ppSexp "MIRCertificateTooLateinEpochDELEG" [pcSlotNo s1, pcSlotNo s2]
  DuplicateGenesisVRFDELEG hash -> ppSexp "DuplicateGenesisVRFDELEG" [ppHash hash]
  MIRTransferNotCurrentlyAllowed -> ppString "MIRTransferNotCurrentlyAllowed"
  MIRNegativesNotCurrentlyAllowed -> ppString " MIRNegativesNotCurrentlyAllowed"
  InsufficientForTransferDELEG pot c1 c2 ->
    ppSexp "InsufficientForTransferDELEG" [ppString (show pot), pcCoin c1, pcCoin c2]
  MIRProducesNegativeUpdate -> ppString "MIRProducesNegativeUpdate"
  MIRNegativeTransfer pot c1 -> ppSexp " MIRNegativeTransfer" [ppString (show pot), pcCoin c1]

instance PrettyA (ShelleyDelegPredFailure era) where
  prettyA = ppShelleyDelegPredFailure

ppShelleyDelplPredFailure :: Proof era -> ShelleyDelplPredFailure era -> PDoc
ppShelleyDelplPredFailure p (PoolFailure x) = ppPOOL p x
ppShelleyDelplPredFailure p (DelegFailure x) = ppDELEG p x

instance Reflect era => PrettyA (ShelleyDelplPredFailure era) where
  prettyA = ppShelleyDelplPredFailure reify

ppConwayDelegPredFailure :: ConwayDelegPredFailure era -> PDoc
ppConwayDelegPredFailure x = case x of
  IncorrectDepositDELEG c -> ppSexp "IncorrectDepositDELEG" [pcCoin c]
  StakeKeyRegisteredDELEG cred -> ppSexp "StakeKeyRegisteredDELEG" [pcCredential cred]
  ConwayRules.StakeKeyNotRegisteredDELEG cred ->
    ppSexp "StakeKeyNotRegisteredDELEG" [pcCredential cred]
  StakeKeyHasNonZeroRewardAccountBalanceDELEG c ->
    ppSexp "StakeKeyHasNonZeroRewardAccountBalanceDELEG" [pcCoin c]
  ConwayRules.DelegateeNotRegisteredDELEG kh -> ppSexp "DelegateeNotRegisteredDELEG" [pcKeyHash kh]

instance PrettyA (ConwayDelegPredFailure era) where
  prettyA = ppConwayDelegPredFailure

ppShelleyPoolPredFailure :: ShelleyPoolPredFailure era -> PDoc
ppShelleyPoolPredFailure (StakePoolNotRegisteredOnKeyPOOL kh) =
  ppRecord
    "StakePoolNotRegisteredOnKeyPOOL"
    [ ("KeyHash", pcKeyHash kh)
    ]
ppShelleyPoolPredFailure
  ( StakePoolRetirementWrongEpochPOOL
      curEpoch
      poolRetEpoch
      firstTooFarEpoch
    ) =
    ppRecord
      "StakePoolRetirementWrongEpochPOOL"
      [ ("Current Epoch", ppEpochNo curEpoch)
      , ("Pool Retirement Epoch", ppEpochNo poolRetEpoch)
      , ("First Epoch Too Far", ppEpochNo firstTooFarEpoch)
      ]
ppShelleyPoolPredFailure
  ( StakePoolCostTooLowPOOL
      prcStakePoolCost
      ppStakePoolCost
    ) =
    ppRecord
      "StakePoolCostTooLowPOOL"
      [ ("PRC Stake Pool Cost", pcCoin prcStakePoolCost)
      , ("PP Stake Pool Cost", pcCoin ppStakePoolCost)
      ]
ppShelleyPoolPredFailure
  ( WrongNetworkPOOL
      nwId
      regCertNwId
      stakePoolId
    ) =
    ppRecord
      "WrongNetworkPOOL"
      [ ("Network ID", ppNetwork nwId)
      , ("Registration Certificate Network ID", ppNetwork regCertNwId)
      , ("Stake Pool ID", pcKeyHash stakePoolId)
      ]
ppShelleyPoolPredFailure
  ( PoolMedataHashTooBig
      stakePoolId
      metadataHashSize
    ) =
    ppRecord
      "PoolMedataHashTooBig"
      [ ("Stake Pool ID", pcKeyHash stakePoolId)
      , ("Metadata Hash Size", ppInt metadataHashSize)
      ]

instance PrettyA (ShelleyPoolPredFailure era) where
  prettyA = ppShelleyPoolPredFailure

-- =========================================
-- Predicate Failure for Alonzo UTXOS

ppUtxosPredicateFailure ::
  forall era.
  Reflect era =>
  AlonzoUtxosPredFailure era ->
  PDoc
ppUtxosPredicateFailure (ValidationTagMismatch isvalid tag) =
  ppRecord
    "ValidationTagMismatch"
    [ ("isValid tag", ppIsValid isvalid)
    , ("mismatch description", ppTagMismatchDescription tag)
    ]
ppUtxosPredicateFailure (CollectErrors es) =
  ppRecord'
    mempty
    [
      ( "When collecting inputs for twophase scripts, these went wrong."
      , ppList ppCollectError es
      )
    ]
ppUtxosPredicateFailure (Alonzo.UpdateFailure p) = ppPPUPPredFailure p

instance Reflect era => PrettyA (AlonzoUtxosPredFailure era) where
  prettyA = ppUtxosPredicateFailure

instance Reflect era => PrettyA (ConwayUtxosPredFailure era) where
  prettyA = \case
    ConwayRules.ValidationTagMismatch isvalid tag ->
      ppRecord
        "ValidationTagMismatch"
        [ ("isValid tag", ppIsValid isvalid)
        , ("mismatch description", ppTagMismatchDescription tag)
        ]
    ConwayRules.CollectErrors es ->
      ppRecord'
        mempty
        [
          ( "When collecting inputs for twophase scripts, these went wrong."
          , ppList ppCollectError es
          )
        ]

ppCollectError ::
  Reflect era =>
  CollectError era ->
  PDoc
ppCollectError (NoRedeemer sp) = ppSexp "NoRedeemer" [ppPlutusPurposeAsItem sp]
ppCollectError (NoWitness sh) = ppSexp "NoWitness" [prettyA sh]
ppCollectError (NoCostModel l) = ppSexp "NoCostModel" [ppLanguage l]
ppCollectError (BadTranslation x) = ppSexp "BadTranslation" [ppContextError x]

ppContextError :: forall era. Reflect era => ContextError era -> PDoc
ppContextError e =
  case reify @era of
    Shelley -> error "Unsuported"
    Allegra -> error "Unsuported"
    Mary -> error "Unsuported"
    Alonzo -> ppString (show e)
    Babbage -> ppString (show e)
    Conway -> ppString (show e)

ppPlutusPurposeAsIx :: Reflect era => PlutusPurpose AsIx era -> PDoc
ppPlutusPurposeAsIx = ppPlutusPurpose @AsIx

ppPlutusPurposeAsItem :: Reflect era => PlutusPurpose AsItem era -> PDoc
ppPlutusPurposeAsItem = ppPlutusPurpose @AsItem

ppPlutusPurposeAsIxItem :: Reflect era => PlutusPurpose AsIxItem era -> PDoc
ppPlutusPurposeAsIxItem = ppPlutusPurpose @AsIxItem

ppPlutusPurpose ::
  forall f era.
  (Reflect era, forall ix it. (PrettyA ix, PrettyA it) => PrettyA (f ix it)) =>
  PlutusPurpose f era ->
  PDoc
ppPlutusPurpose p =
  case reify @era of
    Shelley -> error "Unsuported"
    Allegra -> error "Unsuported"
    Mary -> error "Unsuported"
    Alonzo -> prettyA p
    Babbage -> prettyA p
    Conway -> prettyA p

instance Reflect era => PrettyA (CollectError era) where
  prettyA = ppCollectError

ppTagMismatchDescription :: TagMismatchDescription -> PDoc
ppTagMismatchDescription PassedUnexpectedly = ppSexp "PassedUnexpectedly" []
ppTagMismatchDescription (FailedUnexpectedly xs) =
  ppSexp "FailedUnexpectedly" [ppList ppFailureDescription (toList xs)]

instance PrettyA TagMismatchDescription where
  prettyA = ppTagMismatchDescription

ppFailureDescription :: FailureDescription -> PDoc
ppFailureDescription (PlutusFailure txt bytes) =
  ppRecord "PlutusFailure" [("reason", text txt), ("script", ppLong bytes)]

instance PrettyA FailureDescription where
  prettyA = ppFailureDescription

-- =======================================
-- Predicate Failure for Shelley UTxO

ppShelleyUtxoPredFailure :: forall era. Reflect era => ShelleyUtxoPredFailure era -> PDoc
ppShelleyUtxoPredFailure (Shelley.BadInputsUTxO x) =
  ppSexp "BadInputsUTxO" [ppSet pcTxIn x]
ppShelleyUtxoPredFailure (Shelley.ExpiredUTxO ttl slot) =
  ppRecord "ExpiredUTxO" [("transaction time to live", pcSlotNo ttl), ("current slot", pcSlotNo slot)]
ppShelleyUtxoPredFailure (Shelley.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", ppInteger actual)
    , ("max transaction size", ppInteger maxs)
    ]
ppShelleyUtxoPredFailure (Shelley.InputSetEmptyUTxO) =
  ppSexp "InputSetEmptyUTxO" []
ppShelleyUtxoPredFailure (Shelley.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for this transaction", pcCoin computed)
    , ("fee supplied by this transaction", pcCoin supplied)
    ]
ppShelleyUtxoPredFailure (Shelley.ValueNotConservedUTxO consumed produced) =
  ppRecord
    "ValueNotConservedUTxO"
    [ ("coin consumed", pcVal @era reify consumed)
    , ("coin produced", pcVal @era reify produced)
    ]
ppShelleyUtxoPredFailure (Shelley.WrongNetwork n add) =
  ppRecord
    "WrongNetwork"
    [ ("expected network id", ppNetwork n)
    , ("set of addresses with wrong network id", ppSet pcAddr add)
    ]
ppShelleyUtxoPredFailure (Shelley.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", ppNetwork n)
    , ("set of reward address with wrong network id", ppSet pcRewardAccount accnt)
    ]
ppShelleyUtxoPredFailure (Shelley.OutputTooSmallUTxO xs) =
  ppRecord
    "OutputTooSmallUTxO"
    [("list of supplied transaction outputs that are too small", ppList (pcTxOut reify) xs)]
ppShelleyUtxoPredFailure (Shelley.UpdateFailure x) =
  ppSexp "UpdateFailure" [ppPPUPPredFailure x]
ppShelleyUtxoPredFailure (Shelley.OutputBootAddrAttrsTooBig xs) =
  ppRecord
    "OutputBootAddrAttrsTooBig"
    [("list of supplied bad transaction outputs", ppList (pcTxOut reify) xs)]

instance Reflect era => PrettyA (ShelleyUtxoPredFailure era) where
  prettyA = ppShelleyUtxoPredFailure

-- =======================================
-- Predicate Failure for Shelley PPUP

ppPpupPredicateFailure :: ShelleyPpupPredFailure era -> PDoc
ppPpupPredicateFailure (NonGenesisUpdatePPUP x y) =
  ppRecord
    "NonGenesisUpdatePPUP"
    [ ("KeyHashes which are voting", ppSet pcKeyHash x)
    , ("KeyHashes which should be voting", ppSet pcKeyHash y)
    ]
ppPpupPredicateFailure (PPUpdateWrongEpoch x y z) =
  ppRecord
    "PPUpdateWrongEpoch"
    [ ("current epoch", ppEpochNo x)
    , ("intended epoch of update", ppEpochNo y)
    , ("voting period within the epoch", ppString (show z))
    ]
ppPpupPredicateFailure (PVCannotFollowPPUP x) =
  ppRecord "PVCannotFollowPPUP" [("the first bad protocol version", ppProtVer x)]

instance PrettyA (ShelleyPpupPredFailure era) where
  prettyA = ppPpupPredicateFailure

-- =====================================================
-- Predicate failure for Mary UTXO

ppPPUPPredFailure :: a -> PDoc
ppPPUPPredFailure _ = ppString "PPUPPredFailure" -- TODO FIXME

ppAllegraUtxoPredFailure ::
  forall era.
  Reflect era =>
  AllegraUtxoPredFailure era ->
  PDoc
ppAllegraUtxoPredFailure (Allegra.BadInputsUTxO txins) =
  ppSexp "BadInputsUTxO" [ppSet pcTxIn txins]
ppAllegraUtxoPredFailure (Allegra.OutsideValidityIntervalUTxO vi slot) =
  ppRecord
    "OutsideValidityIntervalUTxO"
    [ ("provided interval", ppValidityInterval vi)
    , ("current slot", pcSlotNo slot)
    ]
ppAllegraUtxoPredFailure (Allegra.MaxTxSizeUTxO actual maxs) =
  ppRecord
    "MaxTxSizeUTxO"
    [ ("Actual", ppInteger actual)
    , ("max transaction size", ppInteger maxs)
    ]
ppAllegraUtxoPredFailure (Allegra.InputSetEmptyUTxO) = ppSexp "InputSetEmptyUTxO" []
ppAllegraUtxoPredFailure (Allegra.FeeTooSmallUTxO computed supplied) =
  ppRecord
    "FeeTooSmallUTxO"
    [ ("min fee for this transaction", pcCoin computed)
    , ("fee supplied by this transaction", pcCoin supplied)
    ]
ppAllegraUtxoPredFailure (Allegra.ValueNotConservedUTxO consumed produced) =
  ppRecord
    "ValueNotConservedUTxO"
    [ ("coin consumed", pcVal @era reify consumed)
    , ("coin produced", pcVal @era reify produced)
    ]
ppAllegraUtxoPredFailure (Allegra.WrongNetwork n add) =
  ppRecord
    "WrongNetwork"
    [ ("expected network id", ppNetwork n)
    , ("set of addresses with wrong network id", ppSet pcAddr add)
    ]
ppAllegraUtxoPredFailure (Allegra.WrongNetworkWithdrawal n accnt) =
  ppRecord
    "WrongNetworkWithdrawal"
    [ ("expected network id", ppNetwork n)
    , ("set reward address with wrong network id", ppSet pcRewardAccount accnt)
    ]
ppAllegraUtxoPredFailure (Allegra.OutputTooSmallUTxO xs) =
  ppRecord
    "OutputTooSmallUTxO"
    [("list of supplied transaction outputs that are too small", ppList (pcTxOut reify) xs)]
ppAllegraUtxoPredFailure (Allegra.UpdateFailure x) =
  ppSexp "UpdateFailure" [ppPPUPPredFailure x]
ppAllegraUtxoPredFailure (Allegra.OutputBootAddrAttrsTooBig xs) =
  ppRecord
    "OutputBootAddrAttrsTooBig"
    [("list of supplied bad transaction outputs", ppList (pcTxOut reify) xs)]
ppAllegraUtxoPredFailure (Allegra.TriesToForgeADA) = ppSexp "TriesToForgeADA" []
ppAllegraUtxoPredFailure (Allegra.OutputTooBigUTxO outs) =
  ppRecord "OutputTooBigUTxO" [("list of TxOuts which are too big", ppList (pcTxOut reify) outs)]

instance Reflect era => PrettyA (AllegraUtxoPredFailure era) where
  prettyA = ppAllegraUtxoPredFailure

-- ==========================================
-- LedgerState objects
-- ==========================================

ppBbodyState :: forall era. Reflect era => ShelleyBbodyState era -> PDoc
ppBbodyState (BbodyState ls (BlocksMade mp)) =
  ppRecord
    "BbodyState"
    [ ("ledger state", ppStateLEDGERS @era reify ls)
    , ("blocks made", ppMap pcKeyHash ppNatural mp)
    ]

instance Reflect era => PrettyA (ShelleyBbodyState era) where
  prettyA = ppBbodyState

-- =======================================================
-- Summaries. A Summary prints just some information
-- For examplle, just the size of a Map or List.

txBodyFieldSummary :: EraTxBody era => TxBodyField era -> [(Text, PDoc)]
txBodyFieldSummary txb = case txb of
  (Inputs s) -> [("Inputs", ppInt (Set.size s))]
  (Collateral s) -> [("Collateral", ppInt (Set.size s))]
  (RefInputs s) -> [("RefInputs", ppInt (Set.size s))]
  (Outputs xs) -> [("Outputs", ppInt (length xs))]
  (CollateralReturn (SJust _)) -> [("Collateral Return", ppString "?")]
  (TotalCol (SJust c)) -> [("TotalCollateral", pcCoin c)]
  (Certs xs) -> [("Certs", ppInt (length xs))]
  (Withdrawals' x) -> [("Withdrawals", ppInt (Map.size (unWithdrawals x)))]
  (Vldt x) -> [("Validity interval", ppValidityInterval x)]
  (Txfee c) -> [("Fee", pcCoin c)]
  (Update (SJust _)) -> [("Collateral Return", ppString "?")]
  (ReqSignerHashes x) -> [("Required Signer hashes", ppInt (Set.size x))]
  (Fields.Mint ma) -> [("Mint", ppInteger (Val.size (MaryValue mempty ma)) <> ppString " bytes")]
  (WppHash (SJust _)) -> [("WppHash", ppString "?")]
  (AdHash (SJust _)) -> [("AdHash", ppString "?")]
  (Txnetworkid (SJust x)) -> [("Network id", ppNetwork x)]
  _ -> []

bodySummary :: EraTxBody era => Proof era -> TxBody era -> PDoc
bodySummary proof txbody =
  ppRecord
    "TxBody"
    (concat (map txBodyFieldSummary (abstractTxBody proof txbody)))

witnessFieldSummary :: WitnessesField era -> (Text, PDoc)
witnessFieldSummary wit = case wit of
  (AddrWits s) -> ("Address Witnesses", ppInt (Set.size s))
  (BootWits s) -> ("BootStrap Witnesses", ppInt (Set.size s))
  (ScriptWits s) -> ("Script Witnesses", ppInt (Map.size s))
  (DataWits m) -> ("Data Witnesses", ppInt (Map.size (unTxDats m)))
  (RdmrWits redeemers) -> ("Redeemer Witnesses", ppInt (Map.size (unRedeemers redeemers)))

witnessSummary :: Proof era -> TxWits era -> PDoc
witnessSummary proof txwits =
  ppRecord
    "Witnesses"
    (map witnessFieldSummary (abstractWitnesses proof txwits))

txFieldSummary :: EraTxBody era => Proof era -> TxField era -> [PDoc]
txFieldSummary proof tx = case tx of
  (Body b) -> [bodySummary proof b]
  (BodyI xs) -> [ppRecord "TxBody" (concat (map txBodyFieldSummary xs))]
  (TxWits ws) -> [witnessSummary proof ws]
  (WitnessesI ws) -> [ppRecord "Witnesses" (map witnessFieldSummary ws)]
  (AuxData (SJust _)) -> [ppSexp "AuxData" [ppString "?"]]
  (Valid (IsValid b)) -> [ppSexp "IsValid" [ppBool b]]
  _ -> []

txSummary :: EraTx era => Proof era -> Tx era -> PDoc
txSummary proof tx =
  ppSexp "Tx" (concat (map (txFieldSummary proof) (abstractTx proof tx)))

-- =================================
-- Summary version of UTxO

txInSummary :: TxIn era -> PDoc
txInSummary (TxIn (TxId h) n) = ppSexp "TxIn" [trim (ppSafeHash h), ppInt (txIxToInt n)]

txOutSummary :: Proof era -> TxOut era -> PDoc
txOutSummary p@Conway (BabbageTxOut addr v d s) =
  ppSexp
    "TxOut"
    [addrSummary addr, pcCoreValue p v, datumSummary d, ppStrictMaybe (scriptSummary p) s]
txOutSummary p@Babbage (BabbageTxOut addr v d s) =
  ppSexp
    "TxOut"
    [addrSummary addr, pcCoreValue p v, datumSummary d, ppStrictMaybe (scriptSummary p) s]
txOutSummary p@Alonzo (AlonzoTxOut addr v md) =
  ppSexp "TxOut" [addrSummary addr, pcCoreValue p v, ppStrictMaybe dataHashSummary md]
txOutSummary p@Mary (ShelleyTxOut addr v) = ppSexp "TxOut" [addrSummary addr, pcCoreValue p v]
txOutSummary p@Allegra (ShelleyTxOut addr v) = ppSexp "TxOut" [addrSummary addr, pcCoreValue p v]
txOutSummary p@Shelley (ShelleyTxOut addr v) = ppSexp "TxOut" [addrSummary addr, pcCoreValue p v]

datumSummary :: Era era => Datum era -> PDoc
datumSummary NoDatum = ppString "NoDatum"
datumSummary (DatumHash h) = ppSexp "DHash" [trim (ppSafeHash h)]
datumSummary (Datum b) = dataSummary (binaryDataToData b)

dataSummary :: Era era => Data era -> PDoc
dataSummary (Data x) = plutusDataSummary x

plutusDataSummary :: PV1.Data -> PDoc
plutusDataSummary (PV1.Constr n ds) = (ppString (show n)) <> ppList plutusDataSummary ds
plutusDataSummary (PV1.Map ds) = ppString "Map" <> ppList (ppPair plutusDataSummary plutusDataSummary) ds
plutusDataSummary (PV1.List xs) = ppList plutusDataSummary xs
plutusDataSummary (PV1.I n) = ppInteger n
plutusDataSummary (PV1.B bs) = trim (ppLong bs)

multiAssetSummary :: MultiAsset c -> PDoc
multiAssetSummary (MultiAsset m) = ppString ("num tokens = " ++ show (Map.size m))

vSummary :: MaryValue c -> PDoc
vSummary (MaryValue n ma) =
  ppSexp "Value" [pcCoin n, multiAssetSummary ma]

scriptSummary :: forall era. Proof era -> Script era -> PDoc
scriptSummary p@Conway script = plutusSummary p script
scriptSummary p@Babbage script = plutusSummary p script
scriptSummary p@Alonzo script = plutusSummary p script
scriptSummary Mary script = timelockSummary script
scriptSummary Allegra script = timelockSummary script
scriptSummary Shelley script = multiSigSummary script

networkSummary :: Network -> PDoc
networkSummary Testnet = ppString "Test"
networkSummary Mainnet = ppString "Main"

addrSummary :: Addr c -> PDoc
addrSummary (Addr nw pay stk) =
  ppSexp "Addr" [networkSummary nw, credSummary pay, stakeSummary stk]
addrSummary (AddrBootstrap _) = ppString "Bootstrap"

credSummary :: Credential keyrole c -> PDoc
credSummary (ScriptHashObj (ScriptHash h)) = ppSexp "Script" [trim (ppHash h)]
credSummary (KeyHashObj (KeyHash kh)) = ppSexp "Key" [trim (ppHash kh)]

stakeSummary :: StakeReference c -> PDoc
stakeSummary StakeRefNull = ppString "Null"
stakeSummary (StakeRefPtr _) = ppString "Ptr"
stakeSummary (StakeRefBase x) = ppSexp "Stake" [credSummary (coerceKeyRole x)]

utxoSummary :: Proof era -> UTxO era -> PDoc
utxoSummary proof = ppMap txInSummary (txOutSummary proof) . unUTxO

utxoString :: Proof era -> UTxO era -> String
utxoString proof = show . ppMap txInSummary (txOutSummary proof) . unUTxO

scriptHashSummary :: ScriptHash c -> PDoc
scriptHashSummary (ScriptHash h) = trim (ppHash h)

keyHashSummary :: KeyHash keyrole c -> PDoc
keyHashSummary (KeyHash h) = trim (ppHash h)

dataHashSummary :: DataHash era -> PDoc
dataHashSummary dh = trim (ppSafeHash dh)

keyPairSummary :: Crypto c => KeyPair r c -> PDoc
keyPairSummary (KeyPair x y) =
  ppRecord "KeyPair" [("vKey", vKeySummary x), ("sKey", viaShow y)]

vKeySummary :: Crypto c => VKey r c -> PDoc
vKeySummary vk@(VKey x) = viaShow x <> " (hash " <> keyHashSummary (hashKey vk) <> ")"

timelockSummary :: (AllegraEraScript era, NativeScript era ~ Timelock era) => Timelock era -> PDoc
timelockSummary (RequireSignature akh) =
  ppSexp "Signature" [keyHashSummary akh]
timelockSummary (RequireAllOf ms) =
  ppSexp "AllOf" (foldr (:) [] (timelockSummary <$> ms))
timelockSummary (RequireAnyOf ms) =
  ppSexp "AnyOf" (foldr (:) [] (timelockSummary <$> ms))
timelockSummary (RequireMOf m ms) =
  ppSexp "MOfN" (ppInteger (fromIntegral m) : foldr (:) [] (timelockSummary <$> ms))
timelockSummary (RequireTimeExpire mslot) =
  ppSexp "Expires" [pcSlotNo mslot]
timelockSummary (RequireTimeStart mslot) =
  ppSexp "Starts" [pcSlotNo mslot]

multiSigSummary ::
  (ShelleyEraScript era, NativeScript era ~ MultiSig era) => MultiSig era -> PDoc
multiSigSummary (RequireSignature hk) = ppSexp "ReqSig" [keyHashSummary hk]
multiSigSummary (RequireAllOf ps) = ppSexp "AllOf" (map multiSigSummary (toList ps))
multiSigSummary (RequireAnyOf ps) = ppSexp "AnyOf" (map multiSigSummary (toList ps))
multiSigSummary (RequireMOf m ps) = ppSexp "MOf" (ppInt m : map multiSigSummary (toList ps))
multiSigSummary _ = error "Impossible: All NativeScripts should have been accounted for"

plutusSummary :: forall era. Proof era -> AlonzoScript era -> PDoc
plutusSummary Conway s@(PlutusScript plutusScript) =
  ppString (show (plutusScriptLanguage plutusScript) ++ " ") <> scriptHashSummary (hashScript @era s)
plutusSummary Conway (TimelockScript x) = timelockSummary x
plutusSummary Babbage s@(PlutusScript plutusScript) =
  ppString (show (plutusScriptLanguage plutusScript) ++ " ") <> scriptHashSummary (hashScript @era s)
plutusSummary Babbage (TimelockScript x) = timelockSummary x
plutusSummary Alonzo s@(PlutusScript plutusScript) =
  ppString (show (plutusScriptLanguage plutusScript) ++ " ") <> scriptHashSummary (hashScript @era s)
plutusSummary Alonzo (TimelockScript x) = timelockSummary x
plutusSummary other _ = ppString ("Plutus script in era " ++ show other ++ "???")

dStateSummary :: DState c -> PDoc
dStateSummary (DState umap future (GenDelegs current) irwd) =
  ppRecord
    "DState"
    [ ("Unified Reward Map", uMapSummary umap)
    , ("Future genesis key delegations", ppInt (Map.size future))
    , ("Genesis key delegations", ppInt (Map.size current))
    , ("Instantaneous Rewards", instantSummary irwd)
    ]

instantSummary :: InstantaneousRewards c -> PDoc
instantSummary (InstantaneousRewards reserves treasury dreserves dtreasury) =
  ppRecord
    "InstantaneousRewards"
    [ ("Rewards from reserves", ppInt (Map.size reserves))
    , ("Rewards from treasury", ppInt (Map.size treasury))
    , ("Treasury to reserves", pcDeltaCoin dreserves)
    , ("Reserves to treasury", pcDeltaCoin dtreasury)
    ]

uMapSummary :: UM.UMap c -> PDoc
uMapSummary umap =
  ppRecord
    "UMap"
    [ ("Reward-Deposit Map", ppInt (UM.size (UM.RewDepUView umap)))
    , ("Ptrs Map", ppInt (UM.size (UM.PtrUView umap)))
    , ("SPoolUView Map", ppInt (UM.size (UM.SPoolUView umap)))
    , ("DRepUView Map", ppInt (UM.size (UM.DRepUView umap)))
    ]

pStateSummary :: PState c -> PDoc
pStateSummary (PState pp fpp retire deposit) =
  ppRecord
    "PState"
    [ ("Pool parameters", ppInt (Map.size pp))
    , ("Future pool parameters", ppInt (Map.size fpp))
    , ("Retiring stake pools", ppInt (Map.size retire))
    , ("Deposits", ppInt (Map.size deposit))
    ]

dpStateSummary :: CertState era -> PDoc
dpStateSummary (CertState v p d) = vsep [pcVState v, pStateSummary p, dStateSummary d]

-- =============================================
-- Pretty printers for more Ledger specific types

pcTxId :: TxId c -> PDoc
pcTxId (TxId safehash) = trim (ppSafeHash safehash)

instance PrettyA (TxId c) where prettyA = pcTxId

pcTxIn :: TxIn c -> PDoc
pcTxIn (TxIn (TxId h) (TxIx i)) = parens (hsep [ppString "TxIn", trim (ppSafeHash h), ppWord64 i])

instance PrettyA (TxIn c) where prettyA = pcTxIn

pcNetwork :: Network -> PDoc
pcNetwork Testnet = ppString "TestNet"
pcNetwork Mainnet = ppString "Mainnet"

instance PrettyA Network where prettyA = pcNetwork

pcKeyHash :: KeyHash discriminator c -> PDoc
pcKeyHash (KeyHash h) = trim (ppHash h)

instance PrettyA (KeyHash d c) where prettyA = pcKeyHash

pcCredential :: Credential keyrole c -> PDoc
pcCredential (ScriptHashObj (ScriptHash h)) = hsep [ppString "(Script", trim (ppHash h) <> ppString ")"]
pcCredential (KeyHashObj (KeyHash h)) = hsep [ppString "(Key", trim (ppHash h) <> ppString ")"]

instance PrettyA (Credential keyrole c) where prettyA = pcCredential

pcStakeReference :: StakeReference c -> PDoc
pcStakeReference StakeRefNull = ppString "Null"
pcStakeReference (StakeRefBase cred) = pcCredential cred
pcStakeReference (StakeRefPtr _) = ppString "Ptr"

instance PrettyA (StakeReference c) where prettyA = pcStakeReference

pcAddr :: Addr c -> PDoc
pcAddr (Addr nw pay stk) =
  parens $
    hsep
      [ ppString "Addr"
      , pcNetwork nw
      , pcCredential pay
      , pcStakeReference stk
      ]
pcAddr (AddrBootstrap _) = ppString "Bootstrap"

instance PrettyA (Addr c) where prettyA = pcAddr

-- | Value is a type family, so it has no PrettyA instance.
pcCoreValue :: Proof era -> Value era -> PDoc
pcCoreValue Conway v = vSummary v
pcCoreValue Babbage v = vSummary v
pcCoreValue Alonzo v = vSummary v
pcCoreValue Mary v = vSummary v
pcCoreValue Allegra (Coin n) = hsep [ppString "₳", ppInteger n]
pcCoreValue Shelley (Coin n) = hsep [ppString "₳", ppInteger n]

pcCoin :: Coin -> PDoc
pcCoin (Coin n) = hsep [ppString "₳", ppInteger n]

instance PrettyA Coin where prettyA = pcCoin

pcValue :: MaryValue c -> PDoc
pcValue (MaryValue n (MultiAsset m)) =
  ppSexp
    "Value"
    [ pcCoin n
    , -- , ppString ("num tokens = " ++ show (Map.size m))
      ppSet pcPolicyID (Map.keysSet m)
    ]

instance PrettyA (MaryValue c) where
  prettyA = pcValue

pcVal :: Proof era -> Value era -> PDoc
pcVal Shelley v = pcCoin v
pcVal Allegra v = pcCoin v
pcVal Mary v = pcValue v
pcVal Alonzo v = pcValue v
pcVal Babbage v = pcValue v
pcVal Conway v = pcValue v

pcDatum :: Era era => Datum era -> PDoc
pcDatum NoDatum = ppString "NoDatum"
pcDatum (DatumHash h) = ppSexp "DHash" [trim (ppSafeHash h)]
pcDatum (Datum b) = pcData (binaryDataToData b)

instance Era era => PrettyA (Datum era) where prettyA = pcDatum

pcData :: forall era. Era era => Data era -> PDoc
pcData d@(Data (PV1.Constr n _)) =
  ppSexp (pack ("Constr" ++ show n)) [ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (PV1.Map _)) =
  ppSexp "Map" [ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (PV1.List _)) =
  ppSexp "List" [ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (PV1.I n)) =
  ppSexp "I" [ppInteger n, ppString "Hash", trim $ ppSafeHash (hashData d)]
pcData d@(Data (PV1.B bytes)) =
  ppSexp "B" [trim (viaShow bytes), ppString "Hash", trim $ ppSafeHash (hashData d)]

instance Era era => PrettyA (Data era) where prettyA = pcData

pcTimelock ::
  forall era.
  (AllegraEraScript era, Reflect era, NativeScript era ~ Timelock era) =>
  Timelock era ->
  PDoc
pcTimelock (RequireSignature akh) = ppSexp "Sign" [pcKeyHash akh]
pcTimelock (RequireAllOf ts) = ppSexp "AllOf" [ppList pcTimelock (toList ts)]
pcTimelock (RequireAnyOf ts) = ppSexp "AnyOf" [ppList pcTimelock (toList ts)]
pcTimelock (RequireMOf m ts) = ppSexp "MOfN" (ppInteger (fromIntegral m) : [ppList pcTimelock (toList ts)])
pcTimelock (RequireTimeExpire mslot) = ppSexp "Expires" [pcSlotNo mslot]
pcTimelock (RequireTimeStart mslot) = ppSexp "Starts" [pcSlotNo mslot]

instance
  ( AllegraEraScript era
  , Reflect era
  , NativeScript era ~ Timelock era
  ) =>
  PrettyA (Timelock era)
  where
  prettyA = pcTimelock

pcMultiSig ::
  (ShelleyEraScript era, Reflect era, NativeScript era ~ MultiSig era) =>
  PDoc ->
  MultiSig era ->
  PDoc
pcMultiSig h (RequireSignature hk) = ppSexp "ReqSig" [keyHashSummary hk, h]
pcMultiSig h (RequireAllOf _) = ppSexp "AllOf" [h]
pcMultiSig h (RequireAnyOf _) = ppSexp "AnyOf" [h]
pcMultiSig h (RequireMOf m _) = ppSexp "MOf" [ppInt m, h]
pcMultiSig _ _ = error "Impossible: All NativeScripts should have been accounted for"

instance
  ( AllegraEraScript era
  , Reflect era
  , NativeScript era ~ MultiSig era
  ) =>
  PrettyA (MultiSig era)
  where
  prettyA = pcMultiSig mempty

pcScriptHash :: ScriptHash era -> PDoc
pcScriptHash (ScriptHash h) = trim (ppHash h)

instance PrettyA (ScriptHash era) where
  prettyA = pcScriptHash

pcHashScript :: forall era. Reflect era => Proof era -> Script era -> PDoc
pcHashScript Conway s = ppString "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript Babbage s = ppString "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript Alonzo s = ppString "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript Mary s = ppString "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript Allegra s = ppString "Hash " <> pcScriptHash (hashScript @era s)
pcHashScript Shelley s = ppString "Hash " <> pcScriptHash (hashScript @era s)

instance (Script era ~ AlonzoScript era, Reflect era) => PrettyA (AlonzoScript era) where
  prettyA = pcScript reify

pcDataHash :: DataHash era -> PDoc
pcDataHash dh = trim (ppSafeHash dh)

instance PrettyA (DataHash era) where
  prettyA = pcDataHash

pcUTxO :: Proof era -> UTxO era -> PDoc
pcUTxO proof = ppMap pcTxIn (unReflect pcTxOut proof) . unUTxO

instance Reflect era => PrettyA (UTxO era) where prettyA = pcUTxO reify

pcPoolParams :: PoolParams era -> PDoc
pcPoolParams x =
  ppRecord
    "PoolParams"
    [ ("Id", keyHashSummary (ppId x))
    , ("reward accnt", pcCredential (raCredential (ppRewardAccount x)))
    ]

instance PrettyA (PoolParams era) where prettyA = pcPoolParams

pcDelegCert :: ShelleyDelegCert c -> PDoc
pcDelegCert (ShelleyRegCert cred) = ppSexp "ShelleyRegCert" [pcCredential cred]
pcDelegCert (ShelleyUnRegCert cred) = ppSexp "ShelleyUnRegCert" [pcCredential cred]
pcDelegCert (ShelleyDelegCert x y) = ppSexp "ShelleyDelegCert" [pcCredential x, pcKeyHash y]

instance PrettyA (ShelleyDelegCert c) where prettyA = pcDelegCert

pcPoolCert :: PoolCert c -> PDoc
pcPoolCert (RegPool poolp) = ppSexp "RegPool" [pcPoolParams poolp]
pcPoolCert (RetirePool keyhash epoch) = ppSexp "RetirePool" [pcKeyHash keyhash, ppEpochNo epoch]

instance PrettyA (PoolCert c) where
  prettyA = pcPoolCert

pcShelleyTxCert :: ShelleyTxCert c -> PDoc
pcShelleyTxCert (ShelleyTxCertDelegCert x) = pcDelegCert x
pcShelleyTxCert (ShelleyTxCertPool x) = pcPoolCert x
pcShelleyTxCert (ShelleyTxCertGenesisDeleg _) = ppString "GenesisCert"
pcShelleyTxCert (ShelleyTxCertMir (MIRCert x (StakeAddressesMIR m))) =
  ppRecord
    "MIRStakeAdresses"
    [ ("pot", ppString (show x))
    , ("Addresses", ppMap pcCredential pcDeltaCoin m)
    ]
pcShelleyTxCert (ShelleyTxCertMir (MIRCert x (SendToOppositePotMIR c))) =
  ppRecord
    "MIROppositePot"
    [ ("pot", ppString (show x))
    , ("Amount", pcCoin c)
    ]

instance PrettyA (ShelleyTxCert c) where
  prettyA = pcShelleyTxCert

pcConwayTxCert :: ConwayTxCert c -> PDoc
pcConwayTxCert (ConwayTxCertDeleg dc) = pcConwayDelegCert dc
pcConwayTxCert (ConwayTxCertPool poolc) = pcPoolCert poolc
pcConwayTxCert (ConwayTxCertGov x) = pcConwayGovCert x

instance PrettyA (ConwayTxCert c) where
  prettyA = pcConwayTxCert

pcConwayGovCert :: ConwayGovCert c -> PDoc
pcConwayGovCert (ConwayRegDRep cred c smA) =
  ppSexp "ConwayRegDRep" [pcCredential cred, pcCoin c, ppStrictMaybe pcAnchor smA]
pcConwayGovCert (ConwayUnRegDRep cred c) =
  ppSexp "ConwayUnRegDRep" [pcCredential cred, pcCoin c]
pcConwayGovCert (ConwayUpdateDRep cred smA) =
  ppSexp "ConwayUpdateDRep" [pcCredential cred, ppStrictMaybe pcAnchor smA]
pcConwayGovCert (ConwayAuthCommitteeHotKey cred1 cred2) =
  ppSexp "ConwayAuthCommitteeHotKey" [pcCredential cred1, pcCredential cred2]
pcConwayGovCert (ConwayResignCommitteeColdKey cred anch) =
  ppRecord
    "ConwayResignCommitteeColdKey"
    [("cred", pcCredential cred), ("anchor", ppStrictMaybe pcAnchor anch)]

instance PrettyA (ConwayGovCert c) where
  prettyA = pcConwayGovCert

pcConwayDelegCert :: ConwayDelegCert c -> PDoc
pcConwayDelegCert (ConwayRegCert cred mcoin) =
  ppSexp "RegCert" [pcCredential cred, ppStrictMaybe pcCoin mcoin]
pcConwayDelegCert (ConwayUnRegCert cred mcoin) =
  ppSexp "UnRegCert" [pcCredential cred, ppStrictMaybe pcCoin mcoin]
pcConwayDelegCert (ConwayDelegCert cred d) =
  ppSexp "DelegCert" [pcCredential cred, pcDelegatee d]
pcConwayDelegCert (ConwayRegDelegCert cred d c) =
  ppSexp "RegDelegCert" [pcCredential cred, pcDelegatee d, pcCoin c]

instance PrettyA (ConwayDelegCert c) where
  prettyA = pcConwayDelegCert

instance Reflect era => PrettyA (ConwayDelegEnv era) where
  prettyA ConwayDelegEnv {..} =
    ppRecord
      "ConwayDelegEnv"
      [ ("cdePParams", prettyA cdePParams)
      , ("cdePools", prettyA cdePools)
      ]

pcDelegatee :: Delegatee c -> PDoc
pcDelegatee (DelegStake kh) = ppSexp "DelegStake" [pcKeyHash kh]
pcDelegatee (DelegVote cred) = ppSexp "DelegVote" [pcDRep cred]
pcDelegatee (DelegStakeVote kh cred) = ppSexp "DelegStakeVote" [pcKeyHash kh, pcDRep cred]

instance PrettyA (Delegatee c) where
  prettyA = pcDelegatee

pcTxCert :: Proof era -> TxCert era -> PDoc
pcTxCert Shelley x = pcShelleyTxCert x
pcTxCert Allegra x = pcShelleyTxCert x
pcTxCert Mary x = pcShelleyTxCert x
pcTxCert Alonzo x = pcShelleyTxCert x
pcTxCert Babbage x = pcShelleyTxCert x
pcTxCert Conway x = pcConwayTxCert x

pcGovSignal :: forall era. Reflect era => GovSignal era -> PDoc
pcGovSignal (GovSignal vote proposal certs) =
  ppRecord
    "GovProcedure"
    [ ("voting", pcVotingProcedures vote)
    , ("proposal", ppList pcProposalProcedure (toList proposal))
    , ("certificates", ppList (pcTxCert (reify @era)) (toList certs))
    ]

instance Reflect era => PrettyA (GovSignal era) where
  prettyA = pcGovSignal

pcVotingProcedures :: VotingProcedures era -> PDoc
pcVotingProcedures (VotingProcedures m) =
  ppSexp "VotingProcedures" [ppMap pcVoter (ppMap pcGovActionId pcVotingProcedure) m]

instance PrettyA (VotingProcedures era) where
  prettyA = pcVotingProcedures

pcProposalProcedure :: ProposalProcedure era -> PDoc
pcProposalProcedure (ProposalProcedure c rewacnt govact anch) =
  ppRecord
    "ProposalProcedure"
    [ ("Deposit", pcCoin c)
    , ("ReturnAddr", pcRewardAccount rewacnt)
    , ("GovAction", pcGovAction govact)
    , ("Anchor", pcAnchor anch)
    ]

instance PrettyA (ProposalProcedure era) where
  prettyA = pcProposalProcedure

pcVoter :: Voter c -> PDoc
pcVoter (CommitteeVoter cred) = ppSexp "CommitteeVoter" [pcCredential cred]
pcVoter (DRepVoter cred) = ppSexp "DRepVoter" [pcCredential cred]
pcVoter (StakePoolVoter keyhash) = ppSexp "StakePoolVoter" [pcKeyHash keyhash]

instance PrettyA (Voter c) where
  prettyA = pcVoter

pcVotingProcedure :: VotingProcedure era -> PDoc
pcVotingProcedure (VotingProcedure v smA) =
  ppRecord "VotingProcedure" [("vote", pcVote v), ("anchor", ppStrictMaybe pcAnchor smA)]

instance PrettyA (VotingProcedure era) where
  prettyA = pcVotingProcedure

-- ============================================================

pcRewardAccount :: RewardAccount c -> PDoc
pcRewardAccount (RewardAccount net cred) = ppSexp "RewardAccount" [pcNetwork net, pcCredential cred]

instance PrettyA (RewardAccount c) where prettyA = pcRewardAccount

pcExUnits :: ExUnits -> PDoc
pcExUnits (ExUnits mem step) =
  ppSexp "ExUnits" [ppNatural mem, ppNatural step]

instance PrettyA ExUnits where prettyA = pcExUnits

pcPair :: (t1 -> PDoc) -> (t2 -> PDoc) -> (t1, t2) -> PDoc
pcPair pp1 pp2 (x, y) = parens (hsep [pp1 x, ppString ",", pp2 y])

pcWitVKey ::
  forall era keyrole.
  (Reflect era, Typeable keyrole) =>
  Proof era ->
  WitVKey keyrole (EraCrypto era) ->
  PDoc
pcWitVKey _p (WitVKey vk@(VKey x) sig) =
  ppSexp
    "WitVKey"
    [ ppString (" VerKey=" ++ (take 10 (drop 19 keystring)))
    , ppString (" SignKey=" ++ (take 10 (drop 29 sigstring)))
    , " VerKeyHash=" <> hash
    ]
  where
    keystring = show x
    hash = pcKeyHash (hashKey vk)
    sigstring = show sig

instance
  forall era c keyrole.
  ( Reflect era
  , c ~ EraCrypto era
  , Typeable keyrole
  ) =>
  PrettyA (WitVKey keyrole c)
  where
  prettyA = pcWitVKey @era reify

-- =====================================
-- Governance Actions etc

-- | GovState is a type family, No PrettyA instance
pcGovState :: Proof era -> GovState era -> PDoc
pcGovState p x = case whichGovState p of
  (GovStateShelleyToBabbage) -> pcShelleyGovState p x
  (GovStateConwayToConway) -> unReflect pcConwayGovState p x

pcShelleyGovState :: Proof era -> ShelleyGovState era -> PDoc
pcShelleyGovState p (ShelleyGovState _proposal _futproposal pp prevpp futurepp) =
  ppRecord
    "ShelleyGovState"
    $ [ ("proposals", ppString "(Proposals ...)")
      , ("futureProposals", ppString "(Proposals ...)")
      , ("pparams", pcPParamsSynopsis p pp)
      , ("prevParams", pcPParamsSynopsis p prevpp)
      , ("futureParams", pcFuturePParams p futurepp)
      ]

pcFuturePParams :: Proof era -> FuturePParams era -> PDoc
pcFuturePParams p = \case
  NoPParamsUpdate -> ppSexp "NoPParamsUpdate" []
  PotentialPParamsUpdate mpp -> ppSexp "PotentialPParamsUpdate" [ppMaybe (pcPParamsSynopsis p) mpp]
  DefinitePParamsUpdate pp -> ppSexp "DefinitePParamsUpdate" [pcPParamsSynopsis p pp]

instance Reflect era => PrettyA (ShelleyGovState era) where
  prettyA = pcShelleyGovState reify

pcEnactState :: Proof era -> EnactState era -> PDoc
pcEnactState p ens@(EnactState _ _ _ _ _ _ _) =
  let EnactState {..} = ens
   in ppRecord
        "EnactState"
        [ ("Constitutional Committee", ppStrictMaybe pcCommittee ensCommittee)
        , ("Constitution", pcConstitution ensConstitution)
        , ("CurPParams", pcPParamsSynopsis p ensCurPParams)
        , ("PrevPParams", pcPParamsSynopsis p ensPrevPParams)
        , ("Treasury", pcCoin ensTreasury)
        , ("Withdrawals", ppMap pcCredential pcCoin ensWithdrawals)
        , ("PrevGovActionIds", pcPrevGovActionIds ensPrevGovActionIds)
        ]

instance Reflect era => PrettyA (EnactState era) where
  prettyA = pcEnactState reify

pcGovActionId :: GovActionId c -> PDoc
pcGovActionId (GovActionId txid (GovActionIx a)) = ppSexp "GovActId" [pcTxId txid, ppWord16 a]

instance PrettyA (GovActionId c) where
  prettyA = pcGovActionId

pcGovPurposeId :: GovPurposeId p era -> PDoc
pcGovPurposeId (GovPurposeId x) = pcGovActionId x

pcPrevGovActionIds :: GovRelation StrictMaybe era -> PDoc
pcPrevGovActionIds GovRelation {grPParamUpdate, grHardFork, grCommittee, grConstitution} =
  ppRecord
    "PrevGovActionIds"
    [ ("LastPParamUpdate", ppStrictMaybe pcGovPurposeId grPParamUpdate)
    , ("LastHardFork", ppStrictMaybe pcGovPurposeId grHardFork)
    , ("LastCommittee", ppStrictMaybe pcGovPurposeId grCommittee)
    , ("LastConstitution", ppStrictMaybe pcGovPurposeId grConstitution)
    ]

instance PrettyA (GovRelation StrictMaybe era) where
  prettyA = pcPrevGovActionIds

pcConwayGovState :: Reflect era => Proof era -> ConwayGovState era -> PDoc
pcConwayGovState p (ConwayGovState ss cmt con cpp ppp fpp dr) =
  ppRecord
    "ConwayGovState"
    [ ("proposals", pcProposals ss)
    , ("committee", ppStrictMaybe prettyA cmt)
    , ("constitution", prettyA con)
    , ("currentPParams", prettyA cpp)
    , ("prevPParams", prettyA ppp)
    , ("futurePParams", pcFuturePParams p fpp)
    , ("drepPulsingState", pcDRepPulsingState p dr)
    ]

instance Reflect era => PrettyA (ConwayGovState era) where
  prettyA = pcConwayGovState reify

pcPulsingSnapshot :: PulsingSnapshot era -> PDoc
pcPulsingSnapshot (PulsingSnapshot ps dd ds pd) =
  ppRecord
    "Snapshot"
    [ ("proposals", ppStrictSeq pcGovActionState ps)
    , ("drepDistr", ppMap pcDRep (pcCoin . fromCompact) dd)
    , ("drepState", ppMap pcCredential pcDRepState ds)
    , ("poolDistr", ppMap pcKeyHash (pcCoin . fromCompact) pd)
    ]

instance PrettyA (PulsingSnapshot era) where
  prettyA = pcPulsingSnapshot

pcDRepPulsingState :: Proof era -> DRepPulsingState era -> PDoc
pcDRepPulsingState p (DRComplete x y) =
  ppRecord
    "DRComplete"
    [ ("pulsingSnapshot", pcPulsingSnapshot x)
    , ("ratifyState", pcRatifyState p y)
    ]
pcDRepPulsingState _ (DRPulsing x) = ppSexp "DRPulsing" [pcDRepPulser x]

instance Reflect era => PrettyA (DRepPulsingState era) where
  prettyA = pcDRepPulsingState reify

pcRatifyState :: Proof era -> RatifyState era -> PDoc
pcRatifyState p (RatifyState enactedState enactedPs expiredPs delayedPs) =
  ppRecord
    "RatifyState"
    [ ("enactstate", pcEnactState p enactedState)
    , ("enacted", ppSeq pcGovActionState enactedPs)
    , ("expired", ppSet pcGovActionId expiredPs)
    , ("delayed", ppBool delayedPs)
    ]

instance Reflect era => PrettyA (RatifyState era) where
  prettyA = pcRatifyState reify

pcProposals :: Proposals era -> PDoc
pcProposals p =
  ppRecord
    "Proposals"
    [ ("pProps", ppMap pcGovActionId pcGovActionState (proposalsActionsMap p))
    , ("pRoots", pcPrevGovActionIds (toPrevGovActionIds (p ^. pRootsL)))
    , ("pGraph", pcForest (p ^. pGraphL))
    ]

pcPEdges :: PEdges (GovPurposeId purpose era) -> PDoc
pcPEdges (PEdges x y) =
  ppRecord
    "PEdges"
    [ ("parent edge", ppStrictMaybe pcGovPurposeId x)
    , ("children", ppSet pcGovPurposeId y)
    ]

pcForest :: GovRelation PGraph era -> PDoc
pcForest (GovRelation a b c d) =
  ppRecord
    "Forest PGraph"
    [ ("pparamupdate", ppMap pcGovPurposeId pcPEdges (unPGraph a))
    , ("hardfork", ppMap pcGovPurposeId pcPEdges (unPGraph b))
    , ("committee", ppMap pcGovPurposeId pcPEdges (unPGraph c))
    , ("constitution", ppMap pcGovPurposeId pcPEdges (unPGraph d))
    ]

instance Reflect era => PrettyA (Proposals era) where
  prettyA = pcProposals

pcGovActionState :: GovActionState era -> PDoc
pcGovActionState gas@(GovActionState _ _ _ _ _ _ _) =
  let GovActionState {..} = gas
   in ppRecord
        "GovActionState"
        [ ("Id", pcGovActionId gasId)
        , ("CommitteVotes", ppMap pcCredential pcVote gasCommitteeVotes)
        , ("DRepVotes", ppMap pcCredential pcVote gasDRepVotes)
        , ("StakePoolVotes", ppMap pcKeyHash pcVote gasStakePoolVotes)
        , ("Procedure", pcProposalProcedure gasProposalProcedure)
        , ("Proposed In", ppEpochNo gasProposedIn)
        , ("Expires After", ppEpochNo gasExpiresAfter)
        ]

instance PrettyA (GovActionState era) where
  prettyA = pcGovActionState

pcVote :: Vote -> PDoc
pcVote x = ppString (show x)

instance PrettyA Vote where
  prettyA = pcVote

pcCommittee :: Committee era -> PDoc
pcCommittee (Committee mem quor) =
  ppRecord
    "Committee"
    [ ("members", ppMap pcCredential ppEpochNo mem)
    , ("quorum", ppUnitInterval quor)
    ]

instance PrettyA (Committee era) where
  prettyA = pcCommittee

pcGovAction :: GovAction era -> PDoc
pcGovAction x = case x of
  (ParameterChange pgaid _ppup policy) ->
    ppRecord
      "ParameterChange"
      [ ("PrevGovActId", ppStrictMaybe pcGovPurposeId pgaid)
      , ("PPUpdate", ppString "(PParamsUpdate ...)")
      , ("Policy", ppStrictMaybe prettyA policy)
      ]
  (HardForkInitiation pgaid pv) ->
    ppRecord
      "HardForkInitiation"
      [ ("PrevGovActId", ppStrictMaybe pcGovPurposeId pgaid)
      , ("ProtVer", ppString (showProtver pv))
      ]
  (TreasuryWithdrawals ws policy) ->
    ppRecord
      "TreasuryWithdrawals"
      [ ("Withdrawals", ppMap pcRewardAccount pcCoin ws)
      , ("Policy", ppStrictMaybe prettyA policy)
      ]
  (NoConfidence pgaid) ->
    ppRecord "NoConfidence" [("PrevGovActId", ppStrictMaybe pcGovPurposeId pgaid)]
  (UpdateCommittee pgaid toRemove toAdd quor) ->
    ppRecord
      "NewCommittee"
      [ ("PrevGovActId", ppStrictMaybe pcGovPurposeId pgaid)
      , ("membersToRemove", ppSet pcCredential toRemove)
      , ("membersToAdd", ppMap pcCredential ppEpochNo toAdd)
      , ("quorum", ppUnitInterval quor)
      ]
  (NewConstitution pgaid c) ->
    ppRecord
      "NewConstitution"
      [ ("PrevGovActId", ppStrictMaybe pcGovPurposeId pgaid)
      , ("Constitution", pcConstitution c)
      ]
  InfoAction -> ppString "InfoAction"

instance PrettyA (GovAction era) where
  prettyA = pcGovAction

pcConstitution :: Constitution c -> PDoc
pcConstitution (Constitution x y) =
  ppRecord
    "Constitution"
    [("anchor", pcAnchor x), ("scripthash", ppStrictMaybe pcScriptHash y)]

instance PrettyA (Constitution c) where
  prettyA = pcConstitution

ppCommitteeAuthorization :: CommitteeAuthorization c -> PDoc
ppCommitteeAuthorization =
  \case
    CommitteeHotCredential hk -> ppSexp "CommitteeHotCredential" [pcCredential hk]
    CommitteeMemberResigned anchor -> ppSexp "CommitteeMemberResigned" [ppStrictMaybe pcAnchor anchor]

instance PrettyA (CommitteeAuthorization c) where
  prettyA = ppCommitteeAuthorization

pcCommitteeState :: CommitteeState era -> PDoc
pcCommitteeState x = ppMap pcCredential ppCommitteeAuthorization (csCommitteeCreds x)

instance (PrettyA (CommitteeState era)) where
  prettyA = pcCommitteeState

-- ===================================================

pcReward :: Reward c -> PDoc
pcReward (Reward ty pl c) =
  ppRecord
    "Reward"
    [ ("type", ppRewardType ty)
    , ("pool", pcKeyHash pl)
    , ("amount", pcCoin c)
    ]

instance PrettyA (Reward c) where
  prettyA = pcReward

pcFutureGenDeleg :: FutureGenDeleg c -> PDoc
pcFutureGenDeleg (FutureGenDeleg (SlotNo x) y) =
  ppRecord
    "FutGenDeleg"
    [ ("slot", ppWord64 x)
    , ("keyHash", pcKeyHash y)
    ]

instance PrettyA (FutureGenDeleg c) where
  prettyA = pcFutureGenDeleg

instance PrettyA (GenDelegPair c) where
  prettyA = pcGenDelegPair

pcCertState :: CertState era -> PDoc
pcCertState (CertState vst pst dst) =
  ppRecord
    "CertState"
    [ ("pstate", pcPState pst)
    , ("vstate", pcVState vst)
    , ("dstate", pcDState dst)
    ]

pcVState :: VState era -> PDoc
pcVState (VState dreps committeeState numDormantEpochs) =
  ppRecord
    "VState"
    [ ("DReps", ppMap pcCredential pcDRepState dreps)
    , ("CC Hot Keys", pcCommitteeState committeeState)
    , ("Number of dormant epochs", ppEpochNo numDormantEpochs)
    ]

instance PrettyA (VState era) where
  prettyA st = pcVState st

pcAnchor :: Anchor c -> PDoc
pcAnchor (Anchor u h) =
  ppRecord
    "Anchor"
    [ ("url", ppString (show u))
    , ("datahash", trim $ ppSafeHash h)
    ]

instance PrettyA (Anchor c) where
  prettyA = pcAnchor

pcDRepState :: DRepState c -> PDoc
pcDRepState (DRepState expire anchor deposit) =
  ppRecord
    "DRepState"
    [ ("expire", ppEpochNo expire)
    , ("anchor", ppStrictMaybe pcAnchor anchor)
    , ("deposit", pcCoin deposit)
    ]

instance PrettyA (DRepState c) where
  prettyA = pcDRepState

pcDRep :: DRep c -> PDoc
pcDRep (DRepCredential cred) = ppSexp "DRepCred" [pcCredential cred]
pcDRep DRepAlwaysAbstain = ppSexp "DRep" [ppString "Abstain"]
pcDRep DRepAlwaysNoConfidence = ppSexp "DRep" [ppString "NoConfidence"]

instance PrettyA (DRep c) where
  prettyA = pcDRep

pcSnapShotL :: Text -> SnapShot c -> [(Text, PDoc)]
pcSnapShotL prefix ss =
  [ (prefix <> "Stake", ppMap pcCredential (pcCoin . fromCompact) (VMap.toMap (unStake (ssStake ss))))
  , (prefix <> "Delegs", ppMap pcCredential pcKeyHash (VMap.toMap (ssDelegations ss)))
  , (prefix <> "Pools", ppMap pcKeyHash pcPoolParams (VMap.toMap (ssPoolParams ss)))
  ]

pcIndividualPoolStake :: IndividualPoolStake c -> PDoc
pcIndividualPoolStake x =
  ppRecord
    "IPS"
    [ ("ratio", ppRational (individualPoolStake x))
    , ("vrf", trim (ppHash (individualPoolStakeVrf x)))
    ]

instance PrettyA (IndividualPoolStake c) where prettyA = pcIndividualPoolStake

pcSnapShots :: SnapShots c -> PDoc
pcSnapShots sss =
  ppRecord' "" $
    pcSnapShotL "mark" (ssStakeMark sss)
      ++ [("markPoolDistr", pcPoolDistr (ssStakeMarkPoolDistr sss))]
      ++ pcSnapShotL "set" (ssStakeSet sss)
      ++ pcSnapShotL "go" (ssStakeGo sss)
      ++ [("fee", pcCoin (ssFee sss))]

instance PrettyA (SnapShots c) where prettyA = pcSnapShots

pcPoolDistr :: PoolDistr c -> PDoc
pcPoolDistr (PoolDistr pdistr tot) =
  ppMap pcKeyHash pcIndividualPoolStake pdistr
    <> ppString " total = "
    <> ppRational (Map.foldl' (+) 0 (fmap individualPoolStake pdistr))
    <> ppString " actualTotal = "
    <> pcCoin (fromCompact tot)

instance PrettyA (PoolDistr c) where prettyA = pcPoolDistr

withEraPParams :: forall era a. Proof era -> (Core.EraPParams era => a) -> a
withEraPParams Shelley x = x
withEraPParams Mary x = x
withEraPParams Allegra x = x
withEraPParams Alonzo x = x
withEraPParams Babbage x = x
withEraPParams Conway x = x

-- | Print just a few of the PParams fields
pcPParamsSynopsis :: forall era. Proof era -> Core.PParams era -> PDoc
pcPParamsSynopsis p x = withEraPParams p help
  where
    help :: Core.EraPParams era => PDoc
    help =
      ppRecord
        "PParams (synopsis)"
        [ ("maxBBSize", ppWord32 (x ^. Core.ppMaxBBSizeL))
        , ("maxBHSize", ppWord16 (x ^. Core.ppMaxBHSizeL))
        , ("maxTxSize", ppWord32 (x ^. Core.ppMaxTxSizeL))
        , ("poolDeposit", pcCoin (x ^. Core.ppPoolDepositL))
        , ("keyDeposit", pcCoin (x ^. Core.ppKeyDepositL))
        , ("protVer", ppString (showProtver (x ^. Core.ppProtocolVersionL)))
        ]

showProtver :: ProtVer -> String
showProtver (ProtVer x y) = "(" ++ show x ++ " " ++ show y ++ ")"

pcEpochState :: Reflect era => Proof era -> EpochState era -> PDoc
pcEpochState proof es@(EpochState (AccountState tre res) ls sss nonmy) =
  ppRecord
    "EpochState"
    [ ("AccountState", ppRecord' "" [("treasury", pcCoin tre), ("reserves", pcCoin res)])
    , ("LedgerState", pcLedgerState proof ls)
    , ("SnapShots", pcSnapShots sss)
    , ("NonMyopic", ppNonMyopic nonmy)
    , ("AdaPots", pcAdaPot es)
    ]

instance Reflect era => PrettyA (EpochState era) where
  prettyA = pcEpochState reify

pcAccountState :: AccountState -> PDoc
pcAccountState (AccountState tr re) = ppRecord' "" [("treasury", pcCoin tr), ("reserves", pcCoin re)]

instance PrettyA AccountState where
  prettyA = pcAccountState

-- | Like pcEpochState.but it only prints a summary of the UTxO
psEpochState :: Reflect era => Proof era -> EpochState era -> PDoc
psEpochState proof es@(EpochState (AccountState tre res) ls sss _) =
  ppRecord
    "EpochState"
    [ ("AccountState", ppRecord' "" [("treasury", pcCoin tre), ("reserves", pcCoin res)])
    , ("LedgerState", psLedgerState proof ls)
    , ("SnapShots", pcSnapShots sss)
    , ("AdaPots", pcAdaPot es)
    ]

pcNewEpochState :: Reflect era => Proof era -> NewEpochState era -> PDoc
pcNewEpochState proof (NewEpochState en (BlocksMade pbm) (BlocksMade cbm) es _ (PoolDistr pd _) _) =
  ppRecord
    "NewEpochState"
    [ ("EpochState", pcEpochState proof es)
    , ("PoolDistr", ppMap pcKeyHash pcIndividualPoolStake pd)
    , ("Prev Blocks", ppMap pcKeyHash ppNatural pbm)
    , ("Current Blocks", ppMap pcKeyHash ppNatural cbm)
    , ("EpochNo", ppEpochNo en)
    ]

instance Reflect era => PrettyA (NewEpochState era) where prettyA = pcNewEpochState reify

-- | Like pcEpochState.but it only prints a summary of the UTxO
psNewEpochState :: Reflect era => Proof era -> NewEpochState era -> PDoc
psNewEpochState proof (NewEpochState en (BlocksMade pbm) (BlocksMade cbm) es _ (PoolDistr pd _) _) =
  ppRecord
    "NewEpochState"
    [ ("EpochState", psEpochState proof es)
    , ("PoolDistr", ppMap pcKeyHash pcIndividualPoolStake pd)
    , ("Prev Blocks", ppMap pcKeyHash ppNatural pbm)
    , ("Current Blocks", ppMap pcKeyHash ppNatural cbm)
    , ("EpochNo", ppEpochNo en)
    ]

pcUTxOState :: Proof era -> UTxOState era -> PDoc
pcUTxOState proof (UTxOState u dep fs gs (IStake m _) don) =
  ppRecord
    "UTxOState"
    [ ("utxo", pcUTxO proof u)
    , ("deposited", pcCoin dep)
    , ("fees", pcCoin fs)
    , ("govState", pcGovState proof gs)
    , ("incremental stake distr", ppString ("size = " ++ show (Map.size m))) -- This is not part of the model
    , ("donation", pcCoin don)
    ]

instance Reflect era => PrettyA (UTxOState era) where
  prettyA = pcUTxOState reify

-- | Like pcUTxOState, except it prints only a summary of the UTxO
psUTxOState :: forall era. Reflect era => Proof era -> UTxOState era -> PDoc
psUTxOState proof (UTxOState (UTxO u) dep fs gs (IStake m _) don) =
  ppRecord
    "UTxOState"
    [ ("utxo", summaryMapCompact (Map.map (\x -> x ^. compactCoinTxOutL) u))
    , ("deposited", pcCoin dep)
    , ("fees", pcCoin fs)
    , ("govState", pcGovState proof gs)
    , ("incremental stake distr", ppString ("size = " ++ show (Map.size m))) -- This is not part of the model
    , ("donation", pcCoin don)
    ]

pcLedgerState :: Proof era -> LedgerState era -> PDoc
pcLedgerState proof ls =
  ppRecord
    "LedgerState"
    [ ("utxoState", pcUTxOState proof (lsUTxOState ls))
    , ("certState", pcCertState (lsCertState ls))
    ]

instance Reflect era => PrettyA (LedgerState era) where
  prettyA = pcLedgerState reify

-- | Like pcLedgerState, except it prints only a summary of the UTxO
psLedgerState :: Reflect era => Proof era -> LedgerState era -> PDoc
psLedgerState proof ls =
  ppRecord
    "LedgerState"
    [ ("utxoState", psUTxOState proof (lsUTxOState ls))
    , ("certState", pcCertState (lsCertState ls))
    ]

pcPState :: PState era -> PDoc
pcPState (PState regP fregP ret dep) =
  ppRecord
    "PState"
    [ ("regPools", ppMap pcKeyHash pcPoolParams regP)
    , ("futureRegPools", ppMap pcKeyHash pcPoolParams fregP)
    , ("retiring", ppMap pcKeyHash ppEpochNo ret)
    , ("poolDeposits", ppMap pcKeyHash pcCoin dep)
    ]

instance PrettyA (PState era) where
  prettyA = pcPState

pcDState :: DState c -> PDoc
pcDState ds =
  ppRecord
    "DState"
    [ ("rewards", ppMap pcCredential pcCoin (rewardMap (dsUnified ds)))
    , ("deposits", ppMap pcCredential pcCoin (depositMap (dsUnified ds)))
    , ("delegate", ppMap pcCredential pcKeyHash (sPoolMap (dsUnified ds)))
    , ("drepDeleg", ppMap pcCredential pcDRep (dRepMap (dsUnified ds)))
    , ("ptrs", ppMap ppPtr pcCredential (ptrMap (dsUnified ds)))
    , ("fGenDel", ppMap pcFutureGenDeleg pcGenDelegPair (dsFutureGenDelegs ds))
    , ("GenDel", ppMap pcKeyHash pcGenDelegPair (unGenDelegs (dsGenDelegs ds)))
    , ("iRewards", pcIRewards (dsIRewards ds))
    ]

instance PrettyA (DState era) where
  prettyA = pcDState

pcGenDelegPair :: GenDelegPair c -> PDoc
pcGenDelegPair x =
  ppRecord
    "GDPair"
    [ ("keyhash", pcKeyHash (genDelegKeyHash x))
    , ("vrfhash", trim (ppHash (genDelegVrfHash x)))
    ]

pcIRewards :: InstantaneousRewards c -> PDoc
pcIRewards xs =
  ppRecord
    "IReward"
    [ ("reserves", ppMap pcCredential pcCoin (DP.iRReserves xs))
    , ("treasury", ppMap pcCredential pcCoin (DP.iRTreasury xs))
    , ("deltaR", pcDeltaCoin (DP.deltaReserves xs))
    , ("deltaT", pcDeltaCoin (DP.deltaTreasury xs))
    ]

instance PrettyA (InstantaneousRewards c) where
  prettyA = pcIRewards

pcDeltaCoin :: DeltaCoin -> PDoc
pcDeltaCoin (DeltaCoin n) = hsep [ppString "▵₳", ppInteger n]

instance PrettyA DeltaCoin where
  prettyA = pcDeltaCoin

pcSlotNo :: SlotNo -> PDoc
pcSlotNo (SlotNo n) = ppWord64 n

instance PrettyA SlotNo where
  prettyA = pcSlotNo

instance PrettyA DP.Obligations where
  prettyA = viaShow

pcAdaPot ::
  ( EraTxOut era
  , EraGov era
  ) =>
  EpochState era ->
  PDoc
pcAdaPot es =
  let x = totalAdaPotsES es
   in ppRecord
        "AdaPot"
        [ ("treasury", pcCoin (treasuryAdaPot x))
        , ("rewards", pcCoin (rewardsAdaPot x))
        , ("utxo", pcCoin (utxoAdaPot x))
        , ("fees", pcCoin (feesAdaPot x))
        , ("obligations", prettyA (obligationsPot x))
        , ("totalAda", pcCoin (totalAdaES es))
        ]

-- ========================

pcPolicyID :: PolicyID c -> PDoc
pcPolicyID (PolicyID sh) = pcScriptHash sh

instance PrettyA (PolicyID c) where
  prettyA = pcPolicyID

pcAssetName :: AssetName -> PDoc
pcAssetName x = trim (viaShow x)

instance PrettyA AssetName where
  prettyA = pcAssetName

pcMultiAsset :: MultiAsset c -> PDoc
pcMultiAsset m = ppList pptriple (flattenMultiAsset m)
  where
    pptriple (i, asset, num) = hsep ["(", pcPolicyID i, pcAssetName asset, ppInteger num, ")"]

instance PrettyA (MultiAsset c) where
  prettyA = pcMultiAsset

instance PrettyA ix => PrettyA (AsIx ix it) where
  prettyA (AsIx ix) = ppSexp "AsIx" [prettyA ix]

instance PrettyA it => PrettyA (AsItem ix it) where
  prettyA (AsItem it) = ppSexp "AsItem" [prettyA it]

instance (PrettyA ix, PrettyA it) => PrettyA (AsIxItem ix it) where
  prettyA (AsIxItem ix it) = ppSexp "AsIxItem" [prettyA ix, prettyA it]

instance
  ( forall ix it. (PrettyA ix, PrettyA it) => PrettyA (f ix it)
  , Reflect era
  , PrettyA (TxCert era)
  ) =>
  PrettyA (AlonzoPlutusPurpose f era)
  where
  prettyA = \case
    AlonzoMinting i -> ppSexp "AlonzoMinting" [prettyA i]
    AlonzoSpending i -> ppSexp "AlonzoSpending" [prettyA i]
    AlonzoRewarding i -> ppSexp "AlonzoRewarding" [prettyA i]
    AlonzoCertifying i -> ppSexp "AlonzoCertifying" [prettyA i]

instance
  ( forall ix it. (PrettyA ix, PrettyA it) => PrettyA (f ix it)
  , Reflect era
  , PrettyA (TxCert era)
  ) =>
  PrettyA (ConwayPlutusPurpose f era)
  where
  prettyA = \case
    ConwayMinting i -> ppSexp "ConwayMinting" [prettyA i]
    ConwaySpending i -> ppSexp "ConwaySpending" [prettyA i]
    ConwayRewarding i -> ppSexp "ConwayRewarding" [prettyA i]
    ConwayCertifying i -> ppSexp "ConwayCertifying" [prettyA i]
    ConwayVoting i -> ppSexp "ConwayVoting" [prettyA i]
    ConwayProposing i -> ppSexp "ConwayProposing" [prettyA i]

-- ScriptsNeeded is a type family so it doesn't have PrettyA instance, use pcScriptsNeeded
-- instead of prettyA
pcScriptsNeeded :: Reflect era => Proof era -> ScriptsNeeded era -> PDoc
pcScriptsNeeded Shelley (ShelleyScriptsNeeded ss) = ppSexp "ScriptsNeeded" [ppSet pcScriptHash ss]
pcScriptsNeeded Allegra (ShelleyScriptsNeeded ss) = ppSexp "ScriptsNeeded" [ppSet pcScriptHash ss]
pcScriptsNeeded Mary (ShelleyScriptsNeeded ss) = ppSexp "ScriptsNeeded" [ppSet pcScriptHash ss]
pcScriptsNeeded Alonzo (AlonzoScriptsNeeded pl) =
  ppSexp "ScriptsNeeded" [ppList (ppPair prettyA pcScriptHash) pl]
pcScriptsNeeded Babbage (AlonzoScriptsNeeded pl) =
  ppSexp "ScriptsNeeded" [ppList (ppPair prettyA pcScriptHash) pl]
pcScriptsNeeded Conway (AlonzoScriptsNeeded pl) =
  ppSexp "ScriptsNeeded" [ppList (ppPair prettyA pcScriptHash) pl]

pcDRepPulser :: DRepPulser era Identity (RatifyState era) -> PDoc
pcDRepPulser x =
  ppRecord
    "DRepPulser"
    [ ("pulseSize", ppInt (dpPulseSize x))
    , ("DRepUView Map", ppMap pcCredential pcDRep (dRepMap (dpUMap x)))
    , ("index", ppInt (dpIndex x))
    , ("stakeDistr", summaryMapCompact (dpStakeDistr x))
    , ("poolDistr", pcPoolDistr (dpStakePoolDistr x))
    , ("partialDrepDistr", summaryMapCompact (dpDRepDistr x))
    , ("drepState", ppMap pcCredential pcDRepState (dpDRepState x))
    , ("epoch", ppEpochNo (dpCurrentEpoch x))
    , ("committeeState", pcCommitteeState (dpCommitteeState x))
    , ("proposals", ppStrictSeq pcGovActionState (dpProposals x))
    , ("globals", ppString "...")
    ]

summaryMapCompact :: Map a (CompactForm Coin) -> PDoc
summaryMapCompact x = ppString ("Count " ++ show (Map.size x) ++ ", Total " ++ show (Map.foldl' (<>) mempty x))

-- ========================

pcConwayGovCertEnv :: forall era. Reflect era => ConwayGovCertEnv era -> PDoc
pcConwayGovCertEnv (ConwayGovCertEnv pp ce cc cp) =
  ppSexp
    "ConwayGovCertEnv"
    [ pcPParams @era reify pp
    , ppEpochNo ce
    , ppStrictMaybe pcCommittee cc
    , ppMap pcGovActionId pcGovActionState $ Map.mapKeys unGovPurposeId cp
    ]

instance Reflect era => PrettyA (ConwayGovCertEnv era) where
  prettyA = pcConwayGovCertEnv

pcPoolEnv :: Reflect era => PoolEnv era -> PDoc
pcPoolEnv (PoolEnv sn pp) = ppSexp "PoolEnv" [pcSlotNo sn, pcPParams reify pp]

instance forall era. Reflect era => PrettyA (PoolEnv era) where
  prettyA = pcPoolEnv

pcEnactSignal :: EnactSignal era -> PDoc
pcEnactSignal EnactSignal {..} =
  ppRecord
    "EnactSignal"
    [ ("Gov Action Id", pcGovActionId esGovActionId)
    , ("Gov Action", pcGovAction esGovAction)
    ]

instance PrettyA (EnactSignal era) where
  prettyA = pcEnactSignal

pcRatifySignal :: RatifySignal era -> PDoc
pcRatifySignal (RatifySignal s) = ppStrictSeq pcGovActionState s

instance PrettyA (RatifySignal era) where
  prettyA = pcRatifySignal

pcRatifyEnv :: RatifyEnv era -> PDoc
pcRatifyEnv rs@(RatifyEnv {}) =
  let RatifyEnv {..} = rs
   in ppRecord
        "RatifyEnv"
        [ ("StakeDistr", ppMap pcCredential (pcCoin . fromCompact) reStakeDistr)
        , ("StakePoolDistr", pcPoolDistr reStakePoolDistr)
        , ("DRepDistr", ppMap pcDRep (pcCoin . fromCompact) reDRepDistr)
        , ("DRepState", ppMap pcCredential pcDRepState reDRepState)
        , ("CurrentEpoch", ppEpochNo reCurrentEpoch)
        , ("CommitteeState", pcCommitteeState reCommitteeState)
        ]

instance PrettyA (RatifyEnv era) where
  prettyA = pcRatifyEnv

pcGovEnv :: Reflect era => GovEnv era -> PDoc
pcGovEnv GovEnv {..} =
  ppRecord
    "GovEnv"
    [ ("TxId", pcTxId geTxId)
    , ("Epoch", ppEpochNo geEpoch)
    , ("PParams", pcPParams reify gePParams)
    ]

instance Reflect era => PrettyA (GovEnv era) where
  prettyA = pcGovEnv

instance Reflect era => PrettyA (LedgerEnv era) where
  prettyA LedgerEnv {..} =
    ppRecord
      "LedgerEnv"
      [ ("slot no", prettyA ledgerSlotNo)
      , ("ix", prettyA (txIxToInt ledgerIx))
      , ("pparams", prettyA ledgerPp)
      , ("account", prettyA ledgerAccount)
      ]

instance Reflect era => PrettyA (UtxoEnv era) where
  prettyA UtxoEnv {..} =
    ppRecord
      "UtxoEnv"
      [ ("slot no", prettyA ueSlot)
      , ("pparams", prettyA uePParams)
      , ("certState", prettyA ueCertState)
      ]

instance Reflect era => PrettyA (CertEnv era) where
  prettyA CertEnv {..} =
    ppRecord
      "CertEnv"
      [ ("slot no", prettyA ceSlotNo)
      , ("pparams", prettyA cePParams)
      , ("currentEpoch", prettyA ceCurrentEpoch)
      ]

instance Reflect era => PrettyA (CertState era) where
  prettyA CertState {..} =
    ppRecord
      "CertState"
      [ ("vState", prettyA certVState)
      , ("pState", prettyA certPState)
      , ("dState", prettyA certDState)
      ]

instance PrettyA x => PrettyA (Seq x) where
  prettyA x = prettyA (toList x)

instance PrettyA (ConwayRules.CertsEnv era) where
  prettyA (ConwayRules.CertsEnv _ _ slot epoch com prop) =
    ppRecord
      "CertsEnv"
      [ ("Tx", ppString "Tx")
      , ("pparams", ppString "PParams")
      , ("slot", pcSlotNo slot)
      , ("epoch", ppEpochNo epoch)
      , ("committee", ppStrictMaybe pcCommittee com)
      , ("proposals", ppMap pcGovPurposeId prettyA prop)
      ]

instance PrettyA RDPair where
  prettyA (RDPair x y) = prettyA (fromCompact x, fromCompact y)

pcStake :: Stake c -> PDoc
pcStake (Stake m) = ppMap pcCredential (pcCoin . fromCompact) (VMap.toMap m)

instance PrettyA (Stake c) where
  prettyA s = pcStake s

pcSnapShot :: SnapShot c -> PDoc
pcSnapShot x = ppRecord "SnapShot" (pcSnapShotL "" x)

instance PrettyA (SnapShot c) where
  prettyA s = pcSnapShot s
