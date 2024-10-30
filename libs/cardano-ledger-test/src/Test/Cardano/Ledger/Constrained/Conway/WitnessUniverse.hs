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
import Cardano.Ledger.Address (BootstrapAddress (..), RewardAccount (..))
import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (..), AsIx (..), PlutusPurpose)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.BaseTypes (Network (..), SlotNo (..))
import Cardano.Ledger.Binary (EncCBOR (encCBOR))
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
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
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Mary.Value ()
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.TxCert
import Constrained hiding (Value)
import Constrained.Base (Pred (..), addToErrorSpec, hasSize, rangeSize)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (replicateM)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.TreeDiff as Tree (Expr (..))
import Data.Typeable
import GHC.Generics
import Lens.Micro
import System.IO.Unsafe (unsafePerformIO)
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Common (ToExpr (..))
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Preds.Universes (genAddrPair)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.Cardano.Ledger.Generic.PrettyCore
import Test.Cardano.Ledger.Generic.Proof (Reflect)
import qualified Test.Cardano.Ledger.Generic.Proof as Proof
import Test.QuickCheck hiding (forAll, witness)

-- import Cardano.Ledger.Conway.Governance(GovActionId(..)) TxId and GovActionIX, no need to witness
-- import Cardano.Ledger.TxIn (TxId (..)) Hash of a TxBody, no need to witness
-- import Test.Cardano.Ledger.Constrained.Conway.Utxo(DepositPurpose(..))

{-
Will need a winessing Spec for this
data ConwayCertExecContext era = ConwayCertExecContext
  { ccecWithdrawals :: !(Map (RewardAccount (EraCrypto era)) Coin)
  , ccecDeposits :: !(Map (DepositPurpose (EraCrypto era)) Coin)
  , ccecVotes :: !(VotingProcedures era)
  }
  deriving (Generic, Eq, Show)
-}

-- ===============================================
-- Move these somewhere else?

instance Era era => HasSimpleRep (TxDats era) where
  type SimpleRep (TxDats era) = Map (DataHash (EraCrypto era)) (Data era)
  toSimpleRep (TxDats m) = m
  fromSimpleRep m = TxDats m

instance (IsConwayUniv fn, Era era) => HasSpec fn (TxDats era)

instance AlonzoEraScript era => HasSimpleRep (Redeemers era) where
  type SimpleRep (Redeemers era) = Map (PlutusPurpose AsIx era) (Data era, ExUnits)
  toSimpleRep (Redeemers m) = m
  fromSimpleRep m = Redeemers m

instance
  ( IsConwayUniv fn
  , Crypto (EraCrypto era)
  , ShelleyEraScript era
  , NativeScript era ~ MultiSig era
  ) =>
  HasSpec fn (MultiSig era)
  where
  type TypeSpec fn (MultiSig era) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen $ genNestedMultiSig 2
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

-- ========================================================================
-- We need witnesses when the Tx has values of type 'hashtype' inside, because
-- the witnesses allows one to recover proof (ProofType hashtype era) that
-- the Tx author both knows, and controls, the thing that was Hashed,
-- i.e. (TypeHashed hashtype era). The witness (WitnessType hashtype era)
-- is NOT ALWAYS the same as the (ProofType hashtype era), since it can depend on
-- the Tx. All the strange class preconditions are required by conformance testing.

-- deriving instance Typeable r => Generic (KeyHash r c)

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
  prettyHash :: hashtype -> PDoc

-- ============================================================

-- | A WitBlock is desined to have five purposes
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
--   5) We can easily to make (Gen (WitBlock t era)), so we can make them for testing.
data WitBlock t era where
  WitBlock :: (Era era, HasWitness t era) => Set t -> Map t (ProofType t era) -> WitBlock t era

instance ToExpr t => ToExpr (WitBlock t era) where
  toExpr (WitBlock x y) = Tree.App "WitBlock" [toExpr x, toExpr y]

wbHash :: WitBlock t era -> Set t
wbHash (WitBlock x _) = x
wbMap :: WitBlock t era -> Map t (ProofType t era)
wbMap (WitBlock _ y) = y

-- | when we print a WitBlock, we are only interested in the hashes, not the witnesses
instance PrettyA (WitBlock t era) where
  prettyA (WitBlock hashset _) = ppSet (prettyHash @t @era) hashset

instance Show (WitBlock t era) where
  show x = show (prettyA x)

instance NFData (WitBlock t era) where
  rnf (WitBlock x y) = deepseq (rnf x) (deepseq (rnf y) ())

instance Eq (WitBlock t era) where
  (WitBlock x y) == (WitBlock a b) = x == a && y == b

-- ==========================================================================
-- (HasWitness t era) Instances for several different types of hash and hashed types

-- A short descriptive name for a long complicated thing
type BodyHash era = SafeHash (EraCrypto era) EraIndependentTxBody

-- ========
-- KeyHash and VKey
-- KeyPair seems to be missing a lot of instances, so we define them here

instance (Typeable r, Crypto c) => EncCBOR (KeyPair r c) where
  encCBOR (KeyPair x y) = encode $ Rec KeyPair !> To x !> To y

deriving instance Typeable r => Eq (KeyPair r StandardCrypto)

instance Crypto c => ToExpr (KeyPair r c) where
  toExpr (KeyPair x y) = Tree.App "KeyPair" [toExpr x, Tree.App (take 10 (show y)) []]

instance (Reflect era, EraCrypto era ~ c, Crypto c) => HasWitness (KeyHash 'Witness c) era where
  type ProofType (KeyHash 'Witness c) era = KeyPair 'Witness c
  type WitnessType (KeyHash 'Witness c) era = (BodyHash era -> WitVKey 'Witness c)
  type TypeHashed (KeyHash 'Witness c) era = VKey 'Witness c
  hash x = hashKey x
  mkWitness keypair safehash = mkWitnessVKey safehash keypair
  getTypeHashed (KeyPair x _) = x
  prettyHash x = prettyA x

-- ========
-- ScriptHash and Scripts
-- NewScript needed because (Script era) is a type family, and hence can't have instances

newtype NewScript era = NewScript {unNew :: Script era}
deriving newtype instance NFData (Script era) => NFData (NewScript era)
deriving newtype instance EraScript era => Eq (NewScript era)
deriving newtype instance EraScript era => EncCBOR (NewScript era)
instance (EraScript era, ToExpr (NativeScript era)) => ToExpr (NewScript era) where
  toExpr (NewScript script) = case getNativeScript script of
    Just s -> toExpr s
    Nothing -> Tree.App "PlutusScript" []

instance
  ( GenScript era
  , EraCrypto era ~ c
  ) =>
  HasWitness (ScriptHash c) era
  where
  type ProofType (ScriptHash c) era = NewScript era
  type WitnessType (ScriptHash c) era = Script era
  type TypeHashed (ScriptHash c) era = ProofType (ScriptHash c) era
  hash (NewScript x) = hashScript x
  mkWitness (NewScript script) = script
  getTypeHashed x = x
  prettyHash x = prettyA x

-- ========
-- BootstrapAddress and SigningKey
-- BootstrapAddress is a convoluted hash of the SigningKey,
-- understood only by the Byron programmers (don't ask)

instance ToExpr SigningKey where toExpr sk = Tree.App (show sk) []

instance (Reflect era, EraCrypto era ~ c) => HasWitness (BootstrapAddress c) era where
  type ProofType (BootstrapAddress c) era = SigningKey
  type WitnessType (BootstrapAddress c) era = (BodyHash era -> BootstrapWitness (EraCrypto era))
  type TypeHashed (BootstrapAddress c) era = SigningKey

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
    let bootAddr = hash @(BootstrapAddress c) @era signkey
     in makeBootstrapWitness
          (extractHash bodyhash)
          signkey
          (Byron.addrAttributes (unBootstrapAddress bootAddr))
  getTypeHashed signkey = signkey
  prettyHash x = prettyA x

-- ========
-- DataHash and Data
-- note the type synonym
-- type (DataHash c) = SafeHash c EraIndependentData

instance (EraScript era, EraCrypto era ~ c) => HasWitness (DataHash c) era where
  type ProofType (DataHash c) era = Data era
  type WitnessType (DataHash c) era = Data era
  type TypeHashed (DataHash c) era = Data era
  hash x = hashData x
  mkWitness script = script
  getTypeHashed x = x
  prettyHash x = pcDataHash x

-- ==============================================
-- The WitUniv type is 4 WitBlocks, there are some missing instances

data WitUniv era
  = WitUniv
  { wvVKey :: WitBlock (KeyHash 'Witness (EraCrypto era)) era
  , wvBoot :: WitBlock (BootstrapAddress (EraCrypto era)) era
  , wvScript :: WitBlock (ScriptHash (EraCrypto era)) era
  , wvDats :: WitBlock (DataHash (EraCrypto era)) era
  }
  deriving (Eq, NFData, ToExpr, Generic)

-- Non deriveable instances for WitUniv

instance Proof.Reflect era => PrettyA (WitUniv era) where
  prettyA (WitUniv keys boot script dats) =
    ppRecord
      "WitnessUniverse"
      [ ("keys", ppSet pcKeyHash (wbHash keys))
      , ("boot", ppSet pcByronAddress (wbHash boot))
      , ("scripts", ppSet pcScriptHash (wbHash script))
      , ("dats", ppSet ppSafeHash (wbHash dats))
      ]

instance Proof.Reflect era => Show (WitUniv era) where show x = show (prettyA x)

instance Era era => EncCBOR (WitUniv era) where
  encCBOR (WitUniv w x y z) = encode $ Rec WitUniv !> To w !> To x !> To y !> To z

-- ====================================================================
-- Purpose 1) To efficiently constrain objects to be witnessed when using constraint generators
-- the (Set hashtype) allows efficient constraints like :: (member_ t) (lit (wbHash witblock))

explainWit :: String -> WitUniv era -> Specification fn t -> Specification fn t
explainWit str (WitUniv (WitBlock s _) _ _ _) spec =
  addToErrorSpec
    (pure ("While witnessing " ++ str ++ " with WitUniv of size " ++ show (Set.size s)))
    spec

witKeyHashSpec ::
  forall fn era krole.
  (Era era, IsConwayUniv fn, Typeable krole) =>
  WitUniv era -> Specification fn (KeyHash krole (EraCrypto era))
witKeyHashSpec univ =
  explainWit "(KeyHash r c)" univ $
    constrained $
      \ [var|keyhash|] ->
        Explain (pure "witnessing (KeyHash r c)") $
          Assert $
            member_ (coerce_ keyhash) (lit (wbHash (wvVKey univ)))

witScriptHashSpec ::
  forall fn era.
  (Era era, IsConwayUniv fn) =>
  WitUniv era -> Specification fn (ScriptHash (EraCrypto era))
witScriptHashSpec univ =
  explainWit "(ScriptHash c)" univ $
    constrained $
      \ [var|scripthash|] -> member_ scripthash (lit (wbHash (wvScript univ)))

witBootstrapAddress ::
  forall fn era.
  (Era era, IsConwayUniv fn) =>
  WitUniv era -> Specification fn (BootstrapAddress (EraCrypto era))
witBootstrapAddress univ =
  explainWit "(BootstrapAddress c)" univ $
    constrained $
      \ [var|bootAddr|] -> member_ bootAddr (lit (wbHash (wvBoot univ)))

witCredSpec ::
  forall fn era krole.
  (IsConwayUniv fn, Era era, Typeable krole) =>
  WitUniv era -> Specification fn (Credential krole (EraCrypto era))
witCredSpec univ =
  explainWit "(Credential c)" univ $
    constrained $ \ [var|cred|] ->
      [ (caseOn cred)
          -- ScriptHash c -> Credential
          (branchW 1 $ \ [var|scripthash|] -> satisfies scripthash (witScriptHashSpec univ))
          -- KeyHash kr c -> Credential
          (branchW 1 $ \ [var|keyhash|] -> satisfies keyhash (witKeyHashSpec univ))
      ]

witDRepSpec ::
  forall fn era.
  (IsConwayUniv fn, Era era) =>
  WitUniv era -> Specification fn (DRep (EraCrypto era))
witDRepSpec univ =
  explainWit "(DRep c)" univ $
    constrained $ \ [var|drep|] ->
      [ (caseOn drep)
          -- KeyHash kr c -> Drep
          (branchW 3 $ \ [var|keyhash|] -> satisfies keyhash (witKeyHashSpec univ))
          -- ScriptHash c -> DRep
          (branchW 3 $ \ [var|scripthash|] -> satisfies scripthash (witScriptHashSpec univ))
          -- DRepAlwaysObstain
          (branchW 1 $ \_ -> assert True)
          -- DRepAlwaysNoConfidence
          (branchW 1 $ \_ -> assert True)
      ]

-- | Used only in Withdrawals, other RewardAccounts, not being withdrawn do not need witnessing
witRewardAccountSpec ::
  forall fn era.
  (Era era, IsConwayUniv fn) =>
  WitUniv era -> Specification fn (RewardAccount (EraCrypto era))
witRewardAccountSpec univ =
  explainWit "(RewardAccount c)" univ $
    constrained $ \ [var|rewaccount|] ->
      match rewaccount $ \ [var|network|] [var|raCred|] ->
        [ assert $ network ==. lit (Testnet)
        , satisfies raCred (witCredSpec @fn @era univ)
        ]

owners_ ::
  (Crypto c, IsConwayUniv fn) => Term fn (PoolParams c) -> Term fn (Set (KeyHash 'Staking c))
owners_ = sel @6

witPoolParamsSpec ::
  forall fn era.
  (Era era, IsConwayUniv fn) =>
  WitUniv era -> Specification fn (PoolParams (EraCrypto era))
witPoolParamsSpec univ =
  explainWit "(PoolParams c)" univ $
    constrained $ \ [var|poolparams|] ->
      [ forAll (owners_ poolparams) $ \ [var|ownerKeyHash|] -> satisfies ownerKeyHash (witKeyHashSpec univ)
      , satisfies (owners_ poolparams) (hasSize (rangeSize 1 3))
      ]

witGenDelegPairSpec ::
  forall fn era.
  (Era era, IsConwayUniv fn) =>
  WitUniv era -> Specification fn (GenDelegPair (EraCrypto era))
witGenDelegPairSpec univ =
  explainWit "(GenDelegPair  c)" univ $
    constrained $ \ [var|gdpair|] ->
      match gdpair $ \ [var|keyhash|] [var|_hash|] -> satisfies keyhash (witKeyHashSpec univ)

-- | Constrains all the Certificate Authors. Sometimes thay are keyHashes, and sometimes Credentials
witShelleyTxCert ::
  forall fn era.
  (Era era, IsConwayUniv fn) =>
  WitUniv era -> Specification fn (ShelleyTxCert era)
witShelleyTxCert univ =
  explainWit ("(ShelleyTxCert " ++ "typeRep (Proxy @era)" ++ ")") univ $
    constrained $ \ [var|txcert|] ->
      (caseOn txcert)
        ( branchW 5 $ \delegcert ->
            (caseOn delegcert)
              (branch $ \_register -> TruePred)
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
        (branchW 1 $ \_mircert -> FalsePred (pure "NO MIR"))

-- | Constrains all the Certificate Authors. Sometimes thay are keyHashes, and sometimes Credentials
witConwayTxCert ::
  forall fn era.
  (Era era, IsConwayUniv fn) =>
  WitUniv era -> Specification fn (ConwayTxCert era)
witConwayTxCert univ =
  explainWit ("(ConwayTxCert " ++ "typeRep (Proxy @era)" ++ ")") univ $
    constrained $ \ [var|txcert|] ->
      (caseOn txcert)
        ( branch $ \delegcert ->
            (caseOn delegcert)
              ( branch $ \registerAuthor deposit ->
                  (caseOn deposit)
                    (branch $ \_ -> TruePred)
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
  Proof.Reflect era =>
  BodyHash era -> BootstrapAddress (EraCrypto era) -> WitUniv era -> TxWits era
witnessBootAddr bodyhash bootaddr wu = case Map.lookup bootaddr (wbMap (wvBoot wu)) of
  Just x ->
    (mkBasicTxWits @era)
      & bootAddrTxWitsL
        .~ (Set.singleton (mkWitness @(BootstrapAddress (EraCrypto era)) @era x bodyhash))
  Nothing -> error ("missing BootstrapAddress in WitUnv " ++ show bootaddr)

witnessKeyHash ::
  forall era.
  Reflect era =>
  BodyHash era -> KeyHash 'Witness (EraCrypto era) -> WitUniv era -> TxWits era
witnessKeyHash bodyhash keyhash wu = case Map.lookup keyhash (wbMap (wvVKey wu)) of
  Just x ->
    (mkBasicTxWits @era)
      & addrTxWitsL
        .~ (Set.singleton (mkWitness @(KeyHash 'Witness (EraCrypto era)) @era x bodyhash))
  Nothing -> error ("missing key hash in WitUnv " ++ show keyhash)

witnessScriptHash ::
  forall era. EraTxWits era => ScriptHash (EraCrypto era) -> WitUniv era -> TxWits era
witnessScriptHash scripthash wu = case Map.lookup scripthash (wbMap (wvScript wu)) of
  Just (NewScript script) -> (mkBasicTxWits @era) & scriptTxWitsL .~ (Map.insert scripthash script Map.empty)
  Nothing -> error ("missing script hash in WitUnv " ++ show scripthash)

witnessDataHash ::
  forall era. AlonzoEraTxWits era => DataHash (EraCrypto era) -> WitUniv era -> TxWits era
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
      { wvVKey = wvVKey x <> wvVKey y
      , wvBoot = wvBoot x <> wvBoot y
      , wvScript = wvScript x <> wvScript y
      , wvDats = wvDats x <> wvDats y
      }

instance (Reflect era, HasWitness (ScriptHash (EraCrypto era)) era) => Monoid (WitUniv era) where
  mempty = WitUniv {wvVKey = mempty, wvBoot = mempty, wvScript = mempty, wvDats = mempty}

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
  (GenScript era, HasWitness (ScriptHash (EraCrypto era)) era) =>
  Int -> Gen (WitUniv era)
genWitUniv n =
  WitUniv
    <$> genWitBlock n (arbitrary @(KeyPair 'Witness (EraCrypto era)))
    <*> genWitBlock n (snd <$> genAddrPair Testnet)
    <*> genWitBlock n (NewScript <$> genScript @era)
    <*> genWitBlock n (arbitrary @(Data era))

-- Use this for small tests only (otherwise increase from 100 to something larger)
-- or use (genWitUniv n) instead of arbitrary for some bigger 'n'
instance
  (HasWitness (ScriptHash (EraCrypto era)) era, GenScript era) =>
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
    genKeyHash :: Gen (KeyHash 'Witness (EraCrypto era))
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
    genKeyHash :: Gen (KeyHash 'Witness (EraCrypto era))
    genKeyHash = arbitrary

-- =======================================================================
-- Tools for Parametric Witnessing
-- ==============================================================

-- | The class of things we know how to witness. This way you don't
--   have to remember long complicated names.
class Witnessed fn era t where
  witness :: WitUniv era -> Term fn t -> Pred fn

instance (IsConwayUniv fn, Era era, Typeable r, EraCrypto era ~ c) => Witnessed fn era (KeyHash r c) where
  witness univ t = satisfies t (witKeyHashSpec univ)

instance (IsConwayUniv fn, Era era, EraCrypto era ~ c) => Witnessed fn era (ScriptHash c) where
  witness univ t = satisfies2 (pure "BAD-SCRIPT-HASH") t (witScriptHashSpec univ)

instance (IsConwayUniv fn, Era era, Typeable r, EraCrypto era ~ c) => Witnessed fn era (Credential r c) where
  witness univ t = satisfies t (witCredSpec univ)

instance (IsConwayUniv fn, Era era, EraCrypto era ~ c) => Witnessed fn era (BootstrapAddress c) where
  witness univ t = satisfies t (witBootstrapAddress univ)

instance (IsConwayUniv fn, Era era, EraCrypto era ~ c) => Witnessed fn era (DRep c) where
  witness univ t = satisfies t (witDRepSpec univ)

instance (IsConwayUniv fn, Era era, EraCrypto era ~ c) => Witnessed fn era (RewardAccount c) where
  witness univ t = satisfies t (witRewardAccountSpec univ)

instance (IsConwayUniv fn, Era era, EraCrypto era ~ c) => Witnessed fn era (PoolParams c) where
  witness univ t = satisfies t (witPoolParamsSpec univ)

instance (IsConwayUniv fn, Era era, EraCrypto era ~ c) => Witnessed fn era (GenDelegPair c) where
  witness univ t = satisfies t (witGenDelegPairSpec univ)

instance (IsConwayUniv fn, Era era) => Witnessed fn era (ShelleyTxCert era) where
  witness univ t = satisfies t (witShelleyTxCert univ)

instance (IsConwayUniv fn, Era era) => Witnessed fn era (ConwayTxCert era) where
  witness univ t = satisfies t (witConwayTxCert univ)

instance (Era era, HasSpec fn t, Ord t, Witnessed fn era t) => Witnessed fn era (Set t) where
  witness univ t =
    forAll
      t
      ( \x ->
          assertExplain (pure ("While witnessing " ++ show (typeRep (Proxy @(Set t))))) $
            witness univ x
      )

instance (Era era, HasSpec fn t, HasSpec fn v, Ord t, Witnessed fn era t) => Witnessed fn era (Map t v) where
  witness univ t =
    assertExplain (pure ("While witnessing " ++ show (typeRep (Proxy @(Map t v))))) $
      forAll (dom_ t) (witness univ)

instance (Era era, HasSpec fn t, Witnessed fn era t) => Witnessed fn era [t] where
  witness univ t =
    assertExplain (pure ("While witnessing " ++ show (typeRep (Proxy @([t]))))) $
      forAll t (witness univ)

-- ===================================================================

-- | Parametric TxCert
class (EraTxCert era, HasSpec fn (TxCert era)) => EraSpecTxCert fn era where
  witTxCert :: WitUniv era -> Specification fn (TxCert era)

instance IsConwayUniv fn => EraSpecTxCert fn Shelley where
  witTxCert = witShelleyTxCert
instance IsConwayUniv fn => EraSpecTxCert fn Allegra where
  witTxCert = witShelleyTxCert
instance IsConwayUniv fn => EraSpecTxCert fn Mary where
  witTxCert = witShelleyTxCert
instance IsConwayUniv fn => EraSpecTxCert fn Alonzo where
  witTxCert = witShelleyTxCert
instance IsConwayUniv fn => EraSpecTxCert fn Babbage where
  witTxCert = witShelleyTxCert
instance IsConwayUniv fn => EraSpecTxCert fn Conway where
  witTxCert = witConwayTxCert

-- | Parametric Script
class (Reflect era, ToExpr (NativeScript era), NFData (Script era)) => GenScript era where
  genScript :: Gen (Script era)

instance GenScript Shelley where genScript = genNestedMultiSig 2
instance GenScript Allegra where genScript = genNestedTimelock @Allegra 2
instance GenScript Mary where genScript = genNestedTimelock @Mary 2
instance GenScript Alonzo where genScript = fromNativeScript <$> genNestedTimelock @Alonzo 2
instance GenScript Babbage where genScript = fromNativeScript <$> genNestedTimelock @Babbage 2
instance GenScript Conway where genScript = fromNativeScript <$> genNestedTimelock @Conway 2

-- ===============================================================
-- examples
spec1 :: WitUniv Shelley -> Specification ConwayFn (Set (ScriptHash StandardCrypto))
spec1 univ =
  constrained $ \ [var|setHash|] -> [assert $ sizeOf_ setHash ==. 11, witness univ setHash]

go1 :: IO ()
go1 = do
  univ <- generate $ genWitUniv @Shelley 5
  {-
  -- print (witScriptHashSpec @ConwayFn univ)
  print (spec1 univ)
  case (spec1 univ) of
    SuspendedSpec {} -> putStrLn "YES"
    _ -> putStrLn "NO"

  putStrLn "\n\n(simplifySpec (spec1 univ))"
  print (simplifySpec (spec1 univ))

  putStrLn "\n\n(explainSpec (pure VVV) (simplifySpec(spec1 univ)))"
  print (explainSpec (pure "VVV") (simplifySpec(spec1 univ))) -}
  ans <- generate $ genFromSpec (spec1 univ)
  putStrLn (show (prettyA ans))

spec2 ::
  WitUniv Shelley ->
  Set (ScriptHash StandardCrypto) ->
  Specification ConwayFn (Set (ScriptHash StandardCrypto))
spec2 univ big =
  constrained $ \ [var|setHash|] ->
    [ assert $ subset_ (lit big) setHash
    , witness univ setHash
    ]
go2 :: IO ()
go2 = do
  univ <- generate $ genWitUniv @Shelley 5
  big <- generate arbitrary
  ans <- generate $ genFromSpec (spec2 univ big)
  putStrLn (show (prettyA ans))

-- =================================================================
explainSpec2 :: HasSpec fn a => NE.NonEmpty String -> Specification fn a -> Specification fn a
explainSpec2 es spec = case simplifySpec spec of
  ErrorSpec es' -> ErrorSpec (es <> es')
  TypeSpec tyspec cs1 -> case guardTypeSpec (NE.toList es) tyspec of
    TypeSpec tyspec2 [] -> TypeSpec tyspec2 cs1
    other -> other
  SuspendedSpec v p -> SuspendedSpec v (assertExplain es p)
  MemberSpec xs -> constrained $ \x -> [assertExplain es $ satisfies x (MemberSpec xs)]
  s -> s

satisfies2 ::
  forall fn a. HasSpec fn a => NE.NonEmpty String -> Term fn a -> Specification fn a -> Pred fn
satisfies2 nes e (ExplainSpec [] x) = satisfies2 nes e x
satisfies2 nes e (ExplainSpec (w : ws) x) = satisfies2 (nes <> (w NE.:| ws)) e x
satisfies2 _ _ TrueSpec = TruePred
satisfies2 nes e (MemberSpec nonempty) = assertExplain nes $ elem_ e (lit (NE.toList nonempty))
satisfies2 nes t (SuspendedSpec x p) = assertExplain nes $ Subst x t p
satisfies2 nes e (TypeSpec s cant)
  | null cant = toPreds e s
  | otherwise =
      Explain (pure (show e ++ " `notElem` " ++ show cant) <> nes) $
        Assert (not_ (elem_ e $ lit cant))
          <> toPreds e s
satisfies2 nes _ (ErrorSpec e) = FalsePred (nes <> e)

-- ======================================================================

conwayWitUniv :: Int -> WitUniv Conway
conwayWitUniv n = unsafePerformIO $ generate $ genWitUniv @Conway n

babbageWitUniv :: Int -> WitUniv Babbage
babbageWitUniv n = unsafePerformIO $ generate $ genWitUniv @Babbage n

alonzoWitUniv :: Int -> WitUniv Alonzo
alonzoWitUniv n = unsafePerformIO $ generate $ genWitUniv @Alonzo n

maryWitUniv :: Int -> WitUniv Mary
maryWitUniv n = unsafePerformIO $ generate $ genWitUniv @Mary n

allegraWitUniv :: Int -> WitUniv Allegra
allegraWitUniv n = unsafePerformIO $ generate $ genWitUniv @Allegra n

shelleyWitUniv :: Int -> WitUniv Shelley
shelleyWitUniv n = unsafePerformIO $ generate $ genWitUniv @Shelley n

class EraUniverse era where
  eraWitUniv :: Int -> WitUniv era

instance EraUniverse Conway where eraWitUniv = conwayWitUniv
instance EraUniverse Babbage where eraWitUniv = babbageWitUniv
instance EraUniverse Alonzo where eraWitUniv = alonzoWitUniv
instance EraUniverse Mary where eraWitUniv = maryWitUniv
instance EraUniverse Allegra where eraWitUniv = allegraWitUniv
instance EraUniverse Shelley where eraWitUniv = shelleyWitUniv
