{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto (SigningKey)
import Cardano.Crypto.Signing (toVerification)
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Wallet as Byron (generate)
import Cardano.Ledger.Address (BootstrapAddress (..), RewardAccount (..))
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (..), AsIx (..), PlutusPurpose)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (Network (..), SlotNo (..))
import Cardano.Ledger.Binary (EncCBOR (encCBOR))
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State (
  ConwayAccountState (..),
  ConwayAccounts (..),
  ShelleyAccountState,
 )
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys (
  BootstrapWitness,
  GenDelegPair (..),
  KeyHash (..),
  KeyRole (..),
  VKey,
  WitVKey,
  hashKey,
  makeBootstrapWitness,
 )
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value ()
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.State (PoolParams (..))
import Constrained.API
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.TreeDiff as Tree (Expr (..))
import Data.Typeable
import GHC.Generics
import Lens.Micro
import System.IO.Unsafe (unsafePerformIO)
import Test.Cardano.Ledger.Binary.Arbitrary (genByteString)
import Test.Cardano.Ledger.Common (ToExpr (..))
import Test.Cardano.Ledger.Constrained.Conway.Instances.Basic (cSJust_, prettyE)
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Conway.Instances.PParams ()
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.QuickCheck hiding (forAll, witness)
import Text.PrettyPrint.HughesPJ (Doc)

-- ======================================================
-- BootstrapAddress are specific to the Byron Era.

-- | Turn a random bytestring into a SigningKey
genSigningKey :: Gen SigningKey
genSigningKey = do
  seed <- genByteString 32
  pure (Byron.SigningKey $ Byron.generate seed (mempty :: ByteString))

-- | Generate a pair, A Byron address, and the key that can sign it.
genAddrPair :: Network -> Gen (BootstrapAddress, Byron.SigningKey)
genAddrPair netwrk = do
  signkey <- genSigningKey
  let verificationKey = Byron.toVerification signkey
      asd = Byron.VerKeyASD verificationKey
      byronNetwork = case netwrk of
        Mainnet -> Byron.NetworkMainOrStage
        Testnet -> Byron.NetworkTestnet 0
      attrs =
        Byron.AddrAttributes
          (Just (Byron.HDAddressPayload "a compressed lenna.png"))
          byronNetwork
  pure (BootstrapAddress (Byron.makeAddress asd attrs), signkey)

-- ===============================================
-- Move these somewhere else

instance Era era => HasSimpleRep (TxDats era) where
  type SimpleRep (TxDats era) = Map DataHash (Data era)
  toSimpleRep (TxDats m) = m
  fromSimpleRep m = TxDats m

instance Era era => HasSpec (TxDats era)

instance AlonzoEraScript era => HasSimpleRep (Redeemers era) where
  type SimpleRep (Redeemers era) = Map (PlutusPurpose AsIx era) (Data era, ExUnits)
  toSimpleRep (Redeemers m) = m
  fromSimpleRep m = Redeemers m

instance
  ( ShelleyEraScript era
  , NativeScript era ~ MultiSig era
  ) =>
  HasSpec (MultiSig era)
  where
  type TypeSpec (MultiSig era) = ()
  emptySpec = ()
  combineSpec _ _ = trueSpec
  genFromTypeSpec _ = pureGen $ genNestedMultiSig 2
  cardinalTypeSpec _ = trueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

-- ========================================================================
-- We need witnesses when the Tx has values of type 'hashtype' inside, because
-- the witnesses allows one to recover proof (ProofType hashtype era) that
-- the Tx author both knows, and controls, the thing that was Hashed,
-- i.e. (TypeHashed hashtype era). The witness (WitnessType hashtype era)
-- is NOT ALWAYS the same as the (ProofType hashtype era), since it can depend on
-- the Tx. All the strange class preconditions are required by conformance testing.

class
  ( Eq (ProofType hashtype era)
  , EncCBOR (ProofType hashtype era)
  , ToExpr (ProofType hashtype era)
  , NFData (ProofType hashtype era)
  , NFData hashtype
  , Eq hashtype
  ) =>
  HasWitness hashtype era
  where
  type ProofType hashtype era
  type WitnessType hashtype era
  type TypeHashed hashtype era
  hash :: TypeHashed hashtype era -> hashtype
  mkWitness :: ProofType hashtype era -> WitnessType hashtype era
  getTypeHashed :: ProofType hashtype era -> TypeHashed hashtype era
  prettyHash :: hashtype -> Doc

-- ============================================================

-- | A WitBlock is designed to have five purposes
--   1) To efficiently constrain objects to be witnessed when using constraint generators
--      the (Set hashtype) allows efficient constraints like :: (member_ t) (lit (wbHash witblock))
--   2) To efficiently compute witnesses from a 'hashtype'
--      the (Map hashtype (ProofType hashtype era)) can be used like this
--      case Map.lookup hash (wbMap witblock) of
--        Just base -> mkWitness base
--        Nothing -> error "Missing hash. perhaps generator did not constrain the hash to be witnessed?"
--   3) When (HasWitness hashtype era) holds, the WitBlock can be computed from only [ProofType hashtype era]
--      using 'getTypeHashed'. This makes Gen and CBOR instances, especially easy. We compute only with
--      [ProofType hashtype era] and then reconstruct the rest
--   4) WitBlock is a Monoid, so we can combine them easily
--   5) We can easily make (Gen (WitBlock t era)), so we can make them for testing.
data WitBlock t era where
  WitBlock :: (Era era, HasWitness t era) => Set t -> Map t (ProofType t era) -> WitBlock t era

instance ToExpr t => ToExpr (WitBlock t era) where
  toExpr (WitBlock x y) = Tree.App "WitBlock" [toExpr x, toExpr y]

wbHash :: WitBlock t era -> Set t
wbHash (WitBlock x _) = x

wbMap :: WitBlock t era -> Map t (ProofType t era)
wbMap (WitBlock _ y) = y

-- | when we print a WitBlock, we are only interested in the hashes, not the witnesses
instance (Show t, ToExpr t) => Show (WitBlock t era) where
  show (WitBlock hashset _) = unlines (map show (Set.toList hashset))

instance NFData (WitBlock t era) where
  rnf (WitBlock x y) = deepseq (rnf x) (deepseq (rnf y) ())

instance Eq (WitBlock t era) where
  (WitBlock x y) == (WitBlock a b) = x == a && y == b

-- ==========================================================================
-- (HasWitness t era) Instances for several different types of hash and hashed types

-- A short descriptive name for a long complicated thing
type BodyHash = SafeHash EraIndependentTxBody

-- ========
-- KeyHash and VKey
-- KeyPair seems to be missing a lot of instances, so we define them here

instance Era era => HasWitness (KeyHash 'Witness) era where
  type ProofType (KeyHash 'Witness) era = KeyPair 'Witness
  type WitnessType (KeyHash 'Witness) era = (BodyHash -> WitVKey 'Witness)
  type TypeHashed (KeyHash 'Witness) era = VKey 'Witness
  hash x = hashKey x
  mkWitness keypair safehash = mkWitnessVKey safehash keypair
  getTypeHashed (KeyPair x _) = x
  prettyHash x = prettyE x

-- ========
-- ScriptHash and Scripts

instance
  GenScript era =>
  HasWitness ScriptHash era
  where
  -- type ProofType ScriptHash era = NewScript era
  type ProofType ScriptHash era = Script era
  type WitnessType ScriptHash era = Script era
  type TypeHashed ScriptHash era = ProofType ScriptHash era
  hash (x) = hashScript x
  mkWitness (script) = script
  getTypeHashed x = x
  prettyHash x = prettyE x

-- ========
-- BootstrapAddress and SigningKey
-- BootstrapAddress is a convoluted hash of the SigningKey,
-- understood only by the Byron programmers (don't ask)

instance ToExpr SigningKey where toExpr sk = Tree.App (show sk) []

instance Era era => HasWitness BootstrapAddress era where
  type ProofType BootstrapAddress era = SigningKey
  type WitnessType BootstrapAddress era = (BodyHash -> BootstrapWitness)
  type TypeHashed BootstrapAddress era = SigningKey

  -- \| Don't ask about this, its related to the magic genAddrPair, which, like
  --   all things Byron, I don't understand. It generates a Byron address and
  --   its signing key for test purposes, and the signing key, uniquely determines
  --   the Byron address (for Testnet) as defined in method 'hash' below. I am certain, this
  --   is not how Byron address work in real life, but it works for test purposes.
  hash signkey =
    let verificationKey = toVerification signkey
        asd = Byron.VerKeyASD verificationKey
        attrs =
          Byron.AddrAttributes
            (Just (Byron.HDAddressPayload "a compressed lenna.png"))
            (Byron.NetworkTestnet 0)
     in BootstrapAddress (Byron.makeAddress asd attrs)
  mkWitness signkey bodyhash =
    let bootAddr = hash @BootstrapAddress @era signkey
     in makeBootstrapWitness
          (extractHash bodyhash)
          signkey
          (Byron.addrAttributes (unBootstrapAddress bootAddr))
  getTypeHashed signkey = signkey
  prettyHash x = prettyE x

-- ========
-- DataHash and Data
-- note the type synonym
-- type DataHash = SafeHash c EraIndependentData

instance EraScript era => HasWitness DataHash era where
  type ProofType DataHash era = Data era
  type WitnessType DataHash era = Data era
  type TypeHashed DataHash era = Data era
  hash x = hashData x
  mkWitness script = script
  getTypeHashed x = x
  prettyHash x = prettyE x

-- ==============================================
-- The WitUniv type is 4 WitBlocks, there are some missing instances

data WitUniv era
  = WitUniv
  { wvSize :: Int
  , wvVKey :: WitBlock (KeyHash 'Witness) era
  , wvBoot :: WitBlock (BootstrapAddress) era
  , wvScript :: WitBlock (ScriptHash) era
  , wvDats :: WitBlock (DataHash) era
  }
  deriving (Eq, NFData, ToExpr, Generic)

instance Show (WitUniv era) where
  show (WitUniv n k _b s d) =
    "WitUniv\n"
      ++ show n
      ++ "\n"
      ++ show k
      ++ "\n"
      ++ show s
      ++ "\n"
      ++ show d
      ++ "\n"

instance Era era => EncCBOR (WitUniv era) where
  encCBOR (WitUniv n w x y z) = encode $ Rec WitUniv !> To n !> To w !> To x !> To y !> To z

-- ====================================================================
-- Purpose 1) To efficiently constrain objects to be witnessed when using constraint generators
-- the (Set hashtype) allows efficient constraints like :: (member_ t) (lit (wbHash witblock))

explainWit :: String -> WitUniv era -> Specification t -> Specification t
explainWit str (WitUniv n _ _ _ _) =
  explainSpec
    ["While witnessing " ++ str ++ " with WitUniv of size " ++ show n]

witKeyHashSpec ::
  forall era krole.
  Typeable krole =>
  WitUniv era -> Specification (KeyHash krole)
witKeyHashSpec univ =
  explainWit "keyhash :: (KeyHash r c)" univ $
    constrained $
      \ [var|keyhash|] ->
        explanation (pure ("witnessing " ++ show keyhash)) $
          assert $
            member_ (coerce_ keyhash) (lit (wbHash (wvVKey univ)))

witScriptHashSpec ::
  forall era.
  WitUniv era -> Specification (ScriptHash)
witScriptHashSpec univ =
  explainWit "scripthash :: ScriptHash" univ $
    constrained $
      \ [var|scripthash|] -> member_ scripthash (lit (wbHash (wvScript univ)))

witBootstrapAddress ::
  forall era.
  WitUniv era -> Specification (BootstrapAddress)
witBootstrapAddress univ =
  explainWit "bootAddr :: BootstrapAddress" univ $
    constrained $
      \ [var|bootAddr|] -> member_ bootAddr (lit (wbHash (wvBoot univ)))

witCredSpec ::
  forall era krole.
  Typeable krole =>
  WitUniv era -> Specification (Credential krole)
witCredSpec univ =
  explainWit "cred :: (Credential c)" univ $
    constrained $ \ [var|cred|] ->
      [ (caseOn cred)
          -- ScriptHash c -> Credential
          (branchW 1 $ \ [var|scripthash|] -> satisfies scripthash (witScriptHashSpec univ))
          -- KeyHash kr c -> Credential
          (branchW 1 $ \ [var|keyhash|] -> satisfies keyhash (witKeyHashSpec univ))
      ]

witDRepSpec ::
  forall era.
  WitUniv era -> Specification (DRep)
witDRepSpec univ =
  explainWit "drep :: (DRep c)" univ $
    constrained $ \ [var|drep|] ->
      [ (caseOn drep)
          -- KeyHash kr c -> Drep
          (branchW 3 $ \ [var|keyhash|] -> satisfies keyhash (witKeyHashSpec univ))
          -- ScriptHash c -> DRep
          (branchW 3 $ \ [var|scripthash|] -> satisfies scripthash (witScriptHashSpec univ))
          -- DRepAlwaysObstain
          (branchW 1 $ \_ -> True)
          -- DRepAlwaysNoConfidence
          (branchW 1 $ \_ -> True)
      ]

-- | Used only in Withdrawals, other RewardAccounts, not being withdrawn do not need witnessing
witRewardAccountSpec ::
  forall era.
  WitUniv era -> Specification (RewardAccount)
witRewardAccountSpec univ =
  explainWit "rewaccount :: (RewardAccount c)" univ $
    constrained $ \ [var|rewaccount|] ->
      match rewaccount $ \ [var|_network|] [var|raCred|] ->
        satisfies raCred (witCredSpec @era univ)

owners_ ::
  Term PoolParams -> Term (Set (KeyHash 'Staking))
owners_ = sel @6

witPoolParamsSpec ::
  forall era.
  WitUniv era -> Specification PoolParams
witPoolParamsSpec univ =
  explainWit "poolparams :: (PoolParams c)" univ $
    constrained $ \ [var|poolparams|] ->
      [ forAll (owners_ poolparams) $ \ [var|ownerKeyHash|] -> satisfies ownerKeyHash (witKeyHashSpec univ)
      , satisfies (owners_ poolparams) (hasSize (rangeSize 1 3))
      ]

witGenDelegPairSpec ::
  forall era.
  WitUniv era -> Specification (GenDelegPair)
witGenDelegPairSpec univ =
  explainWit "gdpair :: (GenDelegPair  c)" univ $
    constrained $ \ [var|gdpair|] ->
      match gdpair $ \ [var|keyhash|] [var|_hash|] -> satisfies keyhash (witKeyHashSpec univ)

-- | Constrains all the Certificate Authors. Sometimes thay are keyHashes, and sometimes Credentials
witShelleyTxCert ::
  forall era.
  Era era =>
  WitUniv era -> Specification (ShelleyTxCert era)
witShelleyTxCert univ =
  explainWit ("txcert :: (ShelleyTxCert " ++ "typeRep (Proxy @era)" ++ ")") univ $
    constrained $ \ [var|txcert|] ->
      (caseOn txcert)
        ( branchW 5 $ \delegcert ->
            (caseOn delegcert)
              (branch $ \_register -> True)
              (branch $ \unregisterAuthor -> satisfies unregisterAuthor (witCredSpec univ))
              (branch $ \delegateAuthor _ -> satisfies delegateAuthor (witCredSpec univ))
        )
        ( branchW 3 $ \poolcert ->
            (caseOn poolcert)
              (branch $ \registerPoolParams -> satisfies registerPoolParams (witPoolParamsSpec univ))
              (branch $ \retirePoolAuthor _ -> satisfies retirePoolAuthor (witKeyHashSpec univ))
        )
        ( branchW 1 $ \genesiscert -> match genesiscert $ \authorkey _ _ -> satisfies authorkey (witKeyHashSpec univ)
        )
        (branchW 1 $ \_mircert -> assertExplain (pure "NO MIR") False)

-- | Constrains all the Certificate Authors. Sometimes thay are keyHashes, and sometimes Credentials
witConwayTxCert ::
  forall era.
  Era era =>
  WitUniv era -> Specification (ConwayTxCert era)
witConwayTxCert univ =
  explainWit ("txcert :: (ConwayTxCert " ++ "typeRep (Proxy @era)" ++ ")") univ $
    constrained $ \ [var|txcert|] ->
      (caseOn txcert)
        ( branch $ \delegcert ->
            (caseOn delegcert)
              ( branch $ \registerAuthor deposit ->
                  (caseOn deposit)
                    (branch $ \_ -> True)
                    (branch $ \_ -> satisfies registerAuthor (witCredSpec univ))
              )
              (branch $ \unregisterAuthor _ -> satisfies unregisterAuthor (witCredSpec univ))
              (branch $ \delegateAuthor _ -> satisfies delegateAuthor (witCredSpec univ))
              (branch $ \registerdelegateAuthor _ _ -> satisfies registerdelegateAuthor (witCredSpec univ))
        )
        ( branch $ \poolcert ->
            (caseOn poolcert)
              (branch $ \registerPoolParams -> satisfies registerPoolParams (witPoolParamsSpec univ))
              (branch $ \retirePoolAuthor _ -> satisfies retirePoolAuthor (witKeyHashSpec univ))
        )
        ( branch $ \govcert ->
            (caseOn govcert)
              (branch $ \regdrepAuthor _ _ -> satisfies regdrepAuthor (witCredSpec univ))
              (branch $ \unregdrepAuthor _ -> satisfies unregdrepAuthor (witCredSpec univ))
              (branch $ \updatedrepAuthor _ -> satisfies updatedrepAuthor (witCredSpec univ))
              (branch $ \authorizeAuthor _ -> satisfies authorizeAuthor (witCredSpec univ))
              (branch $ \resignAuthor _ -> satisfies resignAuthor (witCredSpec univ))
        )

-- =====================================================================
-- Purpose 2) To efficiently compute witnesses from a 'hashtype'
-- the (Map hashtype (ProofType hashtype era)) can be used like this
--     case Map.lookup hash (wbMap witblock) of
--        Just base -> mkWitness base
--        Nothing -> error "Missing hash. perhaps generator did not constrain the hash to be witnessed?"

witnessBootAddr ::
  forall era.
  EraTxWits era =>
  BodyHash -> BootstrapAddress -> WitUniv era -> TxWits era
witnessBootAddr bodyhash bootaddr wu = case Map.lookup bootaddr (wbMap (wvBoot wu)) of
  Just x ->
    (mkBasicTxWits @era)
      & bootAddrTxWitsL
        .~ (Set.singleton (mkWitness @(BootstrapAddress) @era x bodyhash))
  Nothing -> error ("missing BootstrapAddress in WitUnv " ++ show bootaddr)

witnessKeyHash ::
  forall era.
  EraTxWits era =>
  BodyHash -> KeyHash 'Witness -> WitUniv era -> TxWits era
witnessKeyHash bodyhash keyhash wu = case Map.lookup keyhash (wbMap (wvVKey wu)) of
  Just x ->
    (mkBasicTxWits @era)
      & addrTxWitsL
        .~ (Set.singleton (mkWitness @(KeyHash 'Witness) @era x bodyhash))
  Nothing -> error ("missing key hash in WitUnv " ++ show keyhash)

witnessScriptHash ::
  forall era. EraTxWits era => ScriptHash -> WitUniv era -> TxWits era
witnessScriptHash scripthash wu = case Map.lookup scripthash (wbMap (wvScript wu)) of
  Just script -> (mkBasicTxWits @era) & scriptTxWitsL .~ (Map.insert scripthash script Map.empty)
  Nothing -> error ("missing script hash in WitUnv " ++ show scripthash)

witnessDataHash ::
  forall era. AlonzoEraTxWits era => DataHash -> WitUniv era -> TxWits era
witnessDataHash datahash wu = case Map.lookup datahash (wbMap (wvDats wu)) of
  Just d -> (mkBasicTxWits @era) & datsTxWitsL .~ TxDats (Map.insert datahash d Map.empty)
  Nothing -> error ("missing data hash in WitUnv " ++ show datahash)

-- =========================================================
-- Purpose 3) When (HasWitness hashtype era) holds, the WitBlock can be computed from
-- only [ProofType hashtype era] using 'getTypeHashed'. This makes Gen and CBOR instances,
-- especially easy. We compute only with [ProofType hashtype era] and then reconstruct the rest

-- | Reconstruct a (WitBlock t era) from only a [ProofType t era]
blockFromProofList ::
  forall t era. (Era era, Ord t, HasWitness t era) => [ProofType t era] -> WitBlock t era
blockFromProofList baselist =
  WitBlock
    (Set.fromList hashlist)
    (Map.fromList (zip hashlist baselist))
  where
    hashlist = map (hash @t @era . getTypeHashed @t @era) baselist

-- ===============================================================
-- Purpose 4) WitBlock is a Monoid, so we can combine them easily

instance Ord t => Semigroup (WitBlock t era) where
  (WitBlock x y) <> (WitBlock a b) = WitBlock (x <> a) (y <> b)

instance (Era era, Ord t, HasWitness t era) => Monoid (WitBlock t era) where
  mempty = (WitBlock mempty mempty)

-- | Easy to extend Monoid from WitBlock to WitUniv
instance Era era => Semigroup (WitUniv era) where
  x <> y =
    WitUniv
      { wvSize = min (wvSize x) (wvSize y)
      , wvVKey = wvVKey x <> wvVKey y
      , wvBoot = wvBoot x <> wvBoot y
      , wvScript = wvScript x <> wvScript y
      , wvDats = wvDats x <> wvDats y
      }

instance (EraScript era, HasWitness (ScriptHash) era) => Monoid (WitUniv era) where
  mempty = WitUniv {wvSize = 0, wvVKey = mempty, wvBoot = mempty, wvScript = mempty, wvDats = mempty}

-- =======================================================================
-- Purpose 5) We can easily to make (Gen (WitBlock t era)), so we can make them for testing.
-- this is facilitated by the methods of the (HasWitness t era) instances
-- and the special property that WitBlock can be computed from [ProofList]

genWitBlock ::
  forall t era.
  (Era era, Ord t, HasWitness t era) => Int -> Gen (ProofType t era) -> Gen (WitBlock t era)
genWitBlock n g = blockFromProofList <$> vectorOf n g

instance (Era era, Typeable t) => EncCBOR (WitBlock t era) where
  encCBOR (WitBlock _ m) = encCBOR (Map.elems m)

genWitUniv ::
  forall era.
  (GenScript era, HasWitness (ScriptHash) era) =>
  Int -> Gen (WitUniv era)
genWitUniv n =
  WitUniv n
    <$> genWitBlock n (arbitrary @(KeyPair 'Witness))
    <*> genWitBlock n (snd <$> genAddrPair Testnet)
    <*> genWitBlock n (genScript @era)
    <*> genWitBlock n (arbitrary @(Data era))

-- Use this for small tests only (otherwise increase from 100 to something larger)
-- or use (genWitUniv n) instead of arbitrary for some bigger 'n'
instance
  (HasWitness (ScriptHash) era, GenScript era) =>
  Arbitrary (WitUniv era)
  where
  arbitrary = genWitUniv 100

-- ==================================================================================
-- Generating random native-scripts in every era. Needed to generate a random WitUniv)

genNestedMultiSig :: forall era. ShelleyEraScript era => Int -> Gen (NativeScript era)
genNestedMultiSig depth
  | depth > 0 =
      oneof $
        nonRecTimelocks ++ [requireAllOf depth, requireAnyOf depth, requireMOf depth]
  | otherwise = oneof nonRecTimelocks
  where
    nonRecTimelocks = [requireSignature]
    requireSignature = RequireSignature @era <$> genKeyHash
    requireAllOf k = do
      n <- choose (0, 4)
      RequireAllOf . Seq.fromList <$> replicateM n (genNestedMultiSig @era (k - 1))
    requireAnyOf k = do
      n <- choose (1, 4)
      RequireAnyOf . Seq.fromList <$> replicateM n (genNestedMultiSig @era (k - 1))
    requireMOf k = do
      n <- choose (0, 4)
      m <- choose (0, n)
      RequireMOf m . Seq.fromList <$> replicateM n (genNestedMultiSig @era (k - 1))
    genKeyHash :: Gen (KeyHash 'Witness)
    genKeyHash = arbitrary

genNestedTimelock :: forall era. AllegraEraScript era => Int -> Gen (NativeScript era)
genNestedTimelock depth
  | depth > 0 =
      oneof $
        nonRecTimelocks ++ [requireAllOf depth, requireAnyOf depth, requireMOf depth]
  | otherwise = oneof nonRecTimelocks
  where
    nonRecTimelocks =
      [ requireSignature
      , requireTimeStart (SlotNo minBound)
      , requireTimeExpire (SlotNo maxBound)
      ]
    requireSignature = RequireSignature <$> genKeyHash
    requireAllOf k = do
      n <- choose (2, 3) -- lift nonNegativeSingleDigitInt
      RequireAllOf . Seq.fromList <$> replicateM n (genNestedTimelock @era (k - 1))
    requireAnyOf k = do
      n <- choose (2, 3) -- lift positiveSingleDigitInt
      RequireAnyOf . Seq.fromList <$> replicateM n (genNestedTimelock @era (k - 1))
    requireMOf k = do
      n <- choose (2, 3) -- lift nonNegativeSingleDigitInt
      m <- choose (0, n)
      RequireMOf m . Seq.fromList <$> replicateM n (genNestedTimelock @era (k - 1))
    requireTimeStart (SlotNo validFrom) = do
      minSlotNo <- choose (minBound, validFrom)
      pure $ RequireTimeStart (SlotNo minSlotNo)
    requireTimeExpire (SlotNo validTill) = do
      maxSlotNo <- choose (validTill, maxBound)
      pure $ RequireTimeExpire (SlotNo maxSlotNo)
    genKeyHash :: Gen (KeyHash 'Witness)
    genKeyHash = arbitrary

-- =======================================================================
-- Tools for Parametric Witnessing
-- ==============================================================

-- | The class of things we know how to witness. This way you don't
--   have to remember long complicated names.
class Witnessed era t where
  witness :: WitUniv era -> Term t -> Pred

instance (Era era, Typeable r) => Witnessed era (KeyHash r) where
  witness univ t = satisfies t (witKeyHashSpec univ)

instance Era era => Witnessed era ScriptHash where
  witness univ t = satisfies t (witScriptHashSpec univ)

instance (Era era, Typeable r) => Witnessed era (Credential r) where
  witness univ t = satisfies t (witCredSpec univ)

instance Era era => Witnessed era BootstrapAddress where
  witness univ t = satisfies t (witBootstrapAddress univ)

instance Era era => Witnessed era DRep where
  witness univ t = satisfies t (witDRepSpec univ)

instance Era era => Witnessed era RewardAccount where
  witness univ t = satisfies t (witRewardAccountSpec univ)

instance Era era => Witnessed era PoolParams where
  witness univ t = satisfies t (witPoolParamsSpec univ)

instance Era era => Witnessed era GenDelegPair where
  witness univ t = satisfies t (witGenDelegPairSpec univ)

instance Era era => Witnessed era (ShelleyTxCert era) where
  witness univ t = satisfies t (witShelleyTxCert univ)

instance Era era => Witnessed era (ConwayTxCert era) where
  witness univ t = satisfies t (witConwayTxCert univ)

instance EraSpecPParams era => Witnessed era (Committee era) where
  witness univ t = satisfies t (committeeWitness univ)

instance (Era era, HasSpec t, Ord t, Witnessed era t) => Witnessed era (Set t) where
  witness univ t =
    forAll
      t
      ( \x ->
          assertExplain (pure ("In the " ++ show (typeRep (Proxy @(Set t))) ++ " instance of Witnesssed")) $
            witness univ x
      )

instance (Era era, HasSpec t, Witnessed era t, IsNormalType t) => Witnessed era (StrictMaybe t) where
  witness univ t =
    (caseOn t)
      -- SNothing
      (branch $ \_ -> True)
      -- SJust
      ( branch $ \x ->
          assertExplain
            (pure ("In the " ++ show (typeRep (Proxy @(StrictMaybe t))) ++ " instance of Witnesssed"))
            (witness univ x)
      )

instance (Era era, HasSpec t, Witnessed era t, IsNormalType t) => Witnessed era (Maybe t) where
  witness univ t =
    (caseOn t)
      -- Nothing
      (branch $ \_ -> False)
      -- Just
      ( branch $ \x ->
          explanation
            (pure ("In the " ++ show (typeRep (Proxy @(Maybe t))) ++ " instance of Witnesssed"))
            (witness univ x)
      )

instance
  (Era era, HasSpec t, HasSpec v, IsNormalType t, IsNormalType v, Ord t, Witnessed era t) =>
  Witnessed era (Map t v)
  where
  witness univ t =
    assertExplain (pure ("In the " ++ show (typeRep (Proxy @(Map t v))) ++ " instance of Witnesssed")) $
      forAll (dom_ t) (witness univ)

instance (Era era, HasSpec t, Witnessed era t) => Witnessed era [t] where
  witness univ t =
    assertExplain (pure ("In the " ++ show (typeRep (Proxy @([t]))) ++ " instance of Witnessed")) $
      forAll t (witness univ)

instance Era era => Witnessed era (ConwayAccounts era) where
  witness univ t = match t $ \ [var|conwayAccountsMap|] ->
    assertExplain
      (pure ("In the ConwayAccounts instance of Witnessed"))
      [ witness univ (dom_ conwayAccountsMap)
      , witness univ (rng_ conwayAccountsMap)
      ]

instance Era era => Witnessed era (ConwayAccountState era) where
  witness univ t = match t $ \_bal _dep [var|strictmaybeKeyhash|] [var|strictmaybeDrep|] ->
    assertExplain
      (pure ("In the ConwayAccountState instance of Witnessed"))
      [ witness univ strictmaybeKeyhash
      , witness univ strictmaybeDrep
      ]

instance Era era => Witnessed era (ShelleyAccountState era) where
  witness univ t = match t $ \_ptr _bal _dep [var|strictmaybeKeyhash|] ->
    assertExplain
      (pure ("In the ShelleyAccountState instance of Witnessed"))
      [ witness univ strictmaybeKeyhash
      ]

-- ===================================================================

-- | Parametric TxCert
class (EraTxCert era, HasSpec (TxCert era)) => EraSpecTxCert era where
  witTxCert :: WitUniv era -> Specification (TxCert era)

instance EraSpecTxCert ShelleyEra where
  witTxCert = witShelleyTxCert

instance EraSpecTxCert AllegraEra where
  witTxCert = witShelleyTxCert

instance EraSpecTxCert MaryEra where
  witTxCert = witShelleyTxCert

instance EraSpecTxCert AlonzoEra where
  witTxCert = witShelleyTxCert

instance EraSpecTxCert BabbageEra where
  witTxCert = witShelleyTxCert

instance EraSpecTxCert ConwayEra where
  witTxCert = witConwayTxCert

-- | Parametric Script
class
  ( EraScript era
  , ToExpr (NativeScript era)
  , NFData (Script era)
  , ToExpr (Script era)
  ) =>
  GenScript era
  where
  genScript :: Gen (Script era)

instance GenScript ShelleyEra where genScript = genNestedMultiSig 2

instance GenScript AllegraEra where genScript = genNestedTimelock @AllegraEra 2

instance GenScript MaryEra where genScript = genNestedTimelock @MaryEra 2

instance GenScript AlonzoEra where genScript = fromNativeScript <$> genNestedTimelock @AlonzoEra 2

instance GenScript BabbageEra where genScript = fromNativeScript <$> genNestedTimelock @BabbageEra 2

instance GenScript ConwayEra where genScript = fromNativeScript <$> genNestedTimelock @ConwayEra 2

-- ===============================================================
-- examples
spec1 :: WitUniv ShelleyEra -> Specification (Set ScriptHash)
spec1 univ =
  constrained $ \ [var|setHash|] -> [assert $ sizeOf_ setHash ==. 11, witness univ setHash]

go1 :: IO ()
go1 = do
  univ <- generate $ genWitUniv @ShelleyEra 5
  ans <- generate $ genFromSpec (spec1 univ)
  putStrLn (show (prettyE ans))

spec2 ::
  WitUniv ShelleyEra ->
  Set ScriptHash ->
  Specification (Set ScriptHash)
spec2 univ big =
  constrained $ \ [var|setHash|] ->
    [ assert $ subset_ (lit big) setHash
    , witness univ setHash
    ]

go2 :: IO ()
go2 = do
  univ <- generate $ genWitUniv @ShelleyEra 5
  big <- generate arbitrary
  ans <- generate $ genFromSpec (spec2 univ big)
  putStrLn (show (prettyE ans))

-- ======================================================================

conwayWitUniv :: Int -> WitUniv ConwayEra
conwayWitUniv n = unsafePerformIO $ generate $ genWitUniv @ConwayEra n

babbageWitUniv :: Int -> WitUniv BabbageEra
babbageWitUniv n = unsafePerformIO $ generate $ genWitUniv @BabbageEra n

alonzoWitUniv :: Int -> WitUniv AlonzoEra
alonzoWitUniv n = unsafePerformIO $ generate $ genWitUniv @AlonzoEra n

maryWitUniv :: Int -> WitUniv MaryEra
maryWitUniv n = unsafePerformIO $ generate $ genWitUniv @MaryEra n

allegraWitUniv :: Int -> WitUniv AllegraEra
allegraWitUniv n = unsafePerformIO $ generate $ genWitUniv @AllegraEra n

shelleyWitUniv :: Int -> WitUniv ShelleyEra
shelleyWitUniv n = unsafePerformIO $ generate $ genWitUniv @ShelleyEra n

class EraUniverse era where
  eraWitUniv :: Int -> WitUniv era

instance EraUniverse ConwayEra where eraWitUniv = conwayWitUniv

instance EraUniverse BabbageEra where eraWitUniv = babbageWitUniv

instance EraUniverse AlonzoEra where eraWitUniv = alonzoWitUniv

instance EraUniverse MaryEra where eraWitUniv = maryWitUniv

instance EraUniverse AllegraEra where eraWitUniv = allegraWitUniv

instance EraUniverse ShelleyEra where eraWitUniv = shelleyWitUniv

-- =======================================================================

-- | Constrains just the parts that need witnessing in GovActionState
govActionStateWitness ::
  forall era.
  EraSpecPParams era =>
  WitUniv era -> Specification (GovActionState era)
govActionStateWitness univ = explainSpec ["Witnessing GovActionState"] $
  constrained $ \ [var|govactstate|] ->
    match govactstate $
      \_gaid [var|comVotemap|] [var|drepVotemap|] [var|poolVotemap|] [var|proposalProc|] _proposed _expires ->
        [ witness univ (dom_ comVotemap)
        , assert $ sizeOf_ comVotemap ==. lit 3
        , witness univ (dom_ drepVotemap)
        , assert $ sizeOf_ drepVotemap ==. lit 2
        , witness univ (dom_ poolVotemap)
        , assert $ sizeOf_ poolVotemap ==. lit 2
        , satisfies proposalProc (proposalProcedureWitness univ)
        ]

-- | Constrains just the parts that need witnessing in GovAction
govActionWitness ::
  forall era.
  EraSpecPParams era =>
  WitUniv era -> Specification (GovAction era)
govActionWitness univ = explainSpec ["Witnessing GovAction"] $
  constrained $ \ [var|govaction|] ->
    (caseOn govaction)
      -- ParameterChange
      (branch $ \_ _ mhash -> witness univ mhash)
      -- HardFork
      (branch $ \_ _ -> True)
      -- TreasuryWithdrawals
      (branch $ \rewacctmap mhash -> [witness univ (dom_ rewacctmap), witness univ mhash])
      -- NoConfidence
      (branch $ \_ -> True)
      -- UpdateCommitee
      (branch $ \_ credSet credMap _ -> [witness univ credSet, witness univ (dom_ credMap)])
      -- NewConstituion
      (branch $ \_ _ -> True)
      -- InfoAction
      (branch $ \_ -> True)

-- | Constrains just the parts that need witnessing in ProposalProcedure
proposalProcedureWitness ::
  forall era.
  EraSpecPParams era =>
  WitUniv era -> Specification (ProposalProcedure era)
proposalProcedureWitness univ =
  constrained $ \ [var|proposalProc|] ->
    match proposalProc $ \_dep [var|returnAddr|] [var|govAction|] _anchor ->
      [witness univ returnAddr, satisfies govAction (govActionWitness univ)]

-- | Constrains just the parts that need witnessing in Committee
committeeWitness ::
  EraSpecPParams era =>
  WitUniv era -> Specification (Committee era)
committeeWitness univ =
  constrained $ \ [var|committee|] ->
    match committee $ \ [var|epochMap|] _threshold ->
      [witness univ (dom_ epochMap), assert $ sizeOf_ epochMap ==. lit 3]

go9 :: IO ()
go9 = do
  univ <- generate $ genWitUniv @ConwayEra 5
  ans <- generate $ genFromSpec (committeeWitness @ConwayEra univ)
  putStrLn (show (prettyE ans))
  putStrLn (show (prettyE univ))
