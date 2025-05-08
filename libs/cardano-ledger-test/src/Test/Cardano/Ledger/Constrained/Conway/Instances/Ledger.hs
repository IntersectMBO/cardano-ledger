{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides the necessary instances of `HasSpec`
-- and `HasSimpleRep` to write specs for the environments,
-- states, and signals in the STS rules of the Ledger. Note some simple
-- types used in the PParams (Coin, EpochInterval, etc.) have their
-- instances defined in Test.Cardano.Ledger.Constrained.Conway.Instances.Basic
-- and they are reexported here.
module Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger (
  StringW,
  ProposalTree,
  onJust',
  onSized,
  cKeyHashObj,
  cScriptHashObj,
  maryValueCoin_,
  strLen_,
  sizedValue_,
  sizedSize_,
  txOutVal_,
  pProcDeposit_,
  pProcGovAction_,
  gasId_,
  gasCommitteeVotes_,
  gasDRepVotes_,
  gasProposalProcedure_,
  psPParamUpdate_,
  ProposalsSplit (..),
  genProposalsSplit,
  proposalSplitSum,
  coerce_,
  toDelta_,
  module Test.Cardano.Ledger.Constrained.Conway.Instances.Basic,
) where

import Cardano.Chain.Common (
  AddrAttributes (..),
  AddrType (..),
  Address (..),
  Address',
  Attributes (..),
  NetworkMagic (..),
  UnparsedFields (..),
 )
import Cardano.Crypto.Hash hiding (Blake2b_224)
import Cardano.Crypto.Hashing (AbstractHash, abstractHashFromBytes)
import Cardano.Ledger.Address
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Alonzo.TxOut
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), Sized (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (ConwayEra, Tx (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import Cardano.Ledger.HKD
import Cardano.Ledger.Hashes (GenDelegPair (..), GenDelegs (..))
import Cardano.Ledger.Keys (BootstrapWitness, WitVKey, coerceKeyRole)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.MemoBytes
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PoolRank
import Cardano.Ledger.Shelley.RewardUpdate (FreeVars, Pulser, RewardAns, RewardPulser (RSLP))
import Cardano.Ledger.Shelley.Rewards (LeaderOnlyReward, PoolRewardInfo, StakeShare)
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxAuxData (Metadatum, ShelleyTxAuxData (..))
import Cardano.Ledger.Shelley.TxCert (
  GenesisDelegCert (..),
  ShelleyDelegCert (..),
  ShelleyTxCert (..),
 )
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap
import Cardano.Ledger.Val (Val)
import Constrained.API.Extend hiding (Sized)
import Constrained.API.Extend qualified as C
import Constrained.GenT (pureGen, vectorOfT)
import Constrained.Generic
import Control.DeepSeq (NFData)
import Crypto.Hash (Blake2b_224)
import Data.ByteString qualified as BS
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Coerce
import Data.Foldable
import Data.Int
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.OMap.Strict qualified as OMap
import Data.OSet.Strict qualified as SOS
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Tree
import Data.Typeable
import Data.VMap (VMap)
import Data.VMap qualified as VMap
import Data.Word
import GHC.Generics (Generic)
import PlutusLedgerApi.V1 qualified as PV1
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Constrained.Conway.Instances.Basic
import Test.Cardano.Ledger.Constrained.Conway.Instances.PParams ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Utils
import Test.Cardano.Ledger.Shelley.Utils
import Test.Cardano.Ledger.TreeDiff (ToExpr)
import Test.Cardano.Slotting.Numeric ()
import Test.QuickCheck hiding (Args, Fun, NonZero, forAll)

-- ==========================================================
-- TxBody HasSpec instance ------------------------------------------------

-- NOTE: this is a representation of the `ConwayTxBody` type. You can't
-- simply use the generics to derive the `SimpleRep` for `ConwayTxBody`
-- because the type is memoized. So instead we say that the representation
-- is the same as what you would get from using the `ConwayTxBody` pattern.
type ConwayTxBodyTypes =
  '[ Set TxIn
   , Set TxIn
   , Set TxIn
   , StrictSeq (Sized (TxOut ConwayEra))
   , StrictMaybe (Sized (TxOut ConwayEra))
   , StrictMaybe Coin
   , SOS.OSet (ConwayTxCert ConwayEra)
   , Withdrawals
   , Coin
   , ValidityInterval
   , Set (KeyHash 'Witness)
   , MultiAsset
   , StrictMaybe ScriptIntegrityHash
   , StrictMaybe TxAuxDataHash
   , StrictMaybe Network
   , VotingProcedures ConwayEra
   , SOS.OSet (ProposalProcedure ConwayEra)
   , StrictMaybe Coin
   , Coin
   ]

instance HasSpec (TxBody ConwayEra)

instance HasSimpleRep (TxBody ConwayEra) where
  type TheSop (TxBody ConwayEra) = '["ConwayTxBody" ::: ConwayTxBodyTypes]
  toSimpleRep ConwayTxBody {..} =
    inject @"ConwayTxBody" @'["ConwayTxBody" ::: ConwayTxBodyTypes]
      ctbSpendInputs
      ctbCollateralInputs
      ctbReferenceInputs
      ctbOutputs
      ctbCollateralReturn
      ctbTotalCollateral
      ctbCerts
      ctbWithdrawals
      ctbTxfee
      ctbVldt
      ctbReqSignerHashes
      ctbMint
      ctbScriptIntegrityHash
      ctbAdHash
      ctbTxNetworkId
      ctbVotingProcedures
      ctbProposalProcedures
      ctbCurrentTreasuryValue
      ctbTreasuryDonation
  fromSimpleRep rep =
    algebra @'["ConwayTxBody" ::: ConwayTxBodyTypes] rep ConwayTxBody

instance HasSimpleRep DeltaCoin where
  type SimpleRep DeltaCoin = Integer
  fromSimpleRep = DeltaCoin
  toSimpleRep (DeltaCoin c) = c

instance HasSpec DeltaCoin

instance OrdLike DeltaCoin

instance NumLike DeltaCoin

instance Foldy DeltaCoin

deriving via Integer instance Num DeltaCoin

instance (Typeable (TxCert era), Typeable era) => HasSimpleRep (GovSignal era)

instance HasSpec (GovSignal ConwayEra)

instance HasSimpleRep SlotNo

instance OrdLike SlotNo

instance HasSpec SlotNo

instance HasSimpleRep EpochNo

instance OrdLike EpochNo

instance HasSpec EpochNo

instance NumLike EpochNo

instance HasSimpleRep TxIx

instance HasSpec TxIx

instance Typeable index => HasSpec (SafeHash index) where
  type TypeSpec (SafeHash index) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSimpleRep TxId

instance HasSpec TxId

instance HasSimpleRep TxIn

instance HasSpec TxIn

instance Typeable a => HasSimpleRep (StrictSeq a) where
  type SimpleRep (StrictSeq a) = [a]
  toSimpleRep = toList
  fromSimpleRep = StrictSeq.fromList

instance HasSpec a => HasSpec (StrictSeq a)

instance Typeable a => Forallable (StrictSeq a) a

instance Typeable a => HasSimpleRep (Seq a) where
  type SimpleRep (Seq a) = [a]
  toSimpleRep = toList
  fromSimpleRep = Seq.fromList

instance HasSpec a => HasSpec (Seq a)

instance Typeable a => Forallable (Seq a) a

instance HasSpec a => C.Sized (Seq a)

instance Typeable a => HasSimpleRep (Sized a)

instance HasSpec a => HasSpec (Sized a)

sizedValue_ :: (HasSpec (Sized a), HasSpec a) => Term (Sized a) -> Term a
sizedValue_ = sel @0

sizedSize_ :: (HasSpec (Sized a), HasSpec a) => Term (Sized a) -> Term Int64
sizedSize_ = sel @1

instance HasSimpleRep Addr28Extra

instance HasSpec Addr28Extra

instance HasSimpleRep DataHash32

instance HasSpec DataHash32

type ShelleyTxOutTypes era =
  '[ Addr
   , Value era
   ]

instance (Era era, Val (Value era)) => HasSimpleRep (ShelleyTxOut era) where
  type TheSop (ShelleyTxOut era) = '["ShelleyTxOut" ::: ShelleyTxOutTypes era]
  toSimpleRep (ShelleyTxOut addr val) =
    inject @"ShelleyTxOut" @'["ShelleyTxOut" ::: ShelleyTxOutTypes era]
      addr
      val
  fromSimpleRep rep =
    algebra @'["ShelleyTxOut" ::: ShelleyTxOutTypes era] rep ShelleyTxOut

instance (EraTxOut era, HasSpec (Value era)) => HasSpec (ShelleyTxOut era)

type AlonzoTxOutTypes era =
  '[ Addr
   , Value era
   , StrictMaybe DataHash
   ]

instance (Era era, Val (Value era)) => HasSimpleRep (AlonzoTxOut era) where
  type TheSop (AlonzoTxOut era) = '["AlonzoTxOut" ::: AlonzoTxOutTypes era]
  toSimpleRep (AlonzoTxOut addr val mdat) =
    inject @"AlonzoTxOut" @'["AlonzoTxOut" ::: AlonzoTxOutTypes era]
      addr
      val
      mdat
  fromSimpleRep rep =
    algebra @'["AlonzoTxOut" ::: AlonzoTxOutTypes era] rep AlonzoTxOut

instance (EraTxOut era, HasSpec (Value era)) => HasSpec (AlonzoTxOut era)

type BabbageTxOutTypes era =
  '[ Addr
   , Value era
   , Datum era
   , StrictMaybe (Script era)
   ]

instance (Typeable (Script era), Era era, Val (Value era)) => HasSimpleRep (BabbageTxOut era) where
  type TheSop (BabbageTxOut era) = '["BabbageTxOut" ::: BabbageTxOutTypes era]
  toSimpleRep (BabbageTxOut addr val dat msc) =
    inject @"BabbageTxOut" @'["BabbageTxOut" ::: BabbageTxOutTypes era]
      addr
      val
      dat
      msc
  fromSimpleRep rep =
    algebra @'["BabbageTxOut" ::: BabbageTxOutTypes era] rep BabbageTxOut

instance
  ( HasSpec (Value era)
  , Era era
  , HasSpec (Data era)
  , Val (Value era)
  , HasSpec (Script era)
  , IsNormalType (Script era)
  ) =>
  HasSpec (BabbageTxOut era)

txOutVal_ ::
  ( HasSpec (Value era)
  , Era era
  , HasSpec (Data era)
  , Val (Value era)
  , HasSpec (Script era)
  , HasSpec (BabbageTxOut era)
  , IsNormalType (Script era)
  ) =>
  Term (BabbageTxOut era) ->
  Term (Value era)
txOutVal_ = sel @1

instance HasSimpleRep MaryValue where
  type TheSop MaryValue = '["MaryValue" ::: '[Coin]]
  toSimpleRep (MaryValue c _) = c
  fromSimpleRep c = MaryValue c mempty

instance HasSpec MaryValue

maryValueCoin_ :: Term MaryValue -> Term Coin
maryValueCoin_ = sel @0

instance HasSimpleRep PV1.Data

instance HasSpec PV1.Data where
  type TypeSpec PV1.Data = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance Era era => HasSimpleRep (Data era) where
  type SimpleRep (Data era) = PV1.Data
  toSimpleRep = getPlutusData
  fromSimpleRep = mkMemoizedEra @era . PlutusData

instance Era era => HasSpec (Data era)

instance Era era => HasSimpleRep (BinaryData era) where
  type SimpleRep (BinaryData era) = Data era
  toSimpleRep = binaryDataToData
  fromSimpleRep = dataToBinaryData

instance
  (Era era, HasSpec (Data era)) =>
  HasSpec (BinaryData era)

instance Typeable era => HasSimpleRep (Datum era)

instance (Era era, HasSpec (Data era)) => HasSpec (Datum era)

-- TODO: here we are cheating to get out of having to deal with Plutus scripts
instance Typeable era => HasSimpleRep (AlonzoScript era) where
  type SimpleRep (AlonzoScript era) = Timelock era
  toSimpleRep (TimelockScript tl) = tl
  toSimpleRep (PlutusScript _) = error "toSimpleRep for AlonzoScript on a PlutusScript"
  fromSimpleRep = TimelockScript

instance
  ( AlonzoEraScript era
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  HasSpec (AlonzoScript era)

{-
NOTE:
You might think that you could do something like this for `Timelock`.
However, when you do that some questions arise:
  (1) How are you going to write constraints over recursive types
      that don't blow up to infinity?
  (2) How are you going to generate recursive values?

(2) you could imagine solving with some tricks for controlling how we generate
Sum and Prod things (with some global index of sizes: `TypeRep -> Int`). Potentially
you could solve this by having size constraints in the language. There the question is
how you design those constraints - their semantics could be `const True` while still
changing the `Specification` - thus giving you the ability to provide a generation time hint!

Solving (1) is more tricky however. The best guess I have is that you would need
to push any constraint you have into functions `MyConstraint :: MyUniv '[Timelock era] Bool`
and implement everything "offline". This is highly non-satisfactory - but it's hard to see
how else you would do it.

type TimelockTypes era =
  '[ -- RequireSignature
     '[KeyHash 'Witness ]
     -- RequireAllOf
   , '[StrictSeq (Timelock era)]
     -- RequireAnyOf
   , '[StrictSeq (Timelock era)]
     -- RequireMOf
   , '[Int, StrictSeq (Timelock era)]
     -- RequireTimeExpire
   , '[SlotNo]
     -- RequireTimeStart
   , '[SlotNo]
   ]

instance Era era => HasSimpleRep (Timelock era) where
  type SimpleRep (Timelock era) = SOP (TimelockTypes era)

  toSimpleRep (RequireSignature h)  = inject @0 @(TimelockTypes era) h
  toSimpleRep (RequireAllOf ts)     = inject @1 @(TimelockTypes era) ts
  toSimpleRep (RequireAnyOf ts)     = inject @2 @(TimelockTypes era) ts
  toSimpleRep (RequireMOf m ts)     = inject @3 @(TimelockTypes era) m ts
  toSimpleRep (RequireTimeExpire s) = inject @4 @(TimelockTypes era) s
  toSimpleRep (RequireTimeStart s)  = inject @5 @(TimelockTypes era) s

  fromSimpleRep rep =
    algebra @(TimelockTypes era) rep
      RequireSignature
      RequireAllOf
      RequireAnyOf
      RequireMOf
      RequireTimeExpire
      RequireTimeStart
-}

instance
  ( AllegraEraScript era
  , NativeScript era ~ Timelock era
  ) =>
  HasSpec (Timelock era)
  where
  type TypeSpec (Timelock era) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSimpleRep CompactAddr where
  type SimpleRep CompactAddr = SimpleRep Addr
  toSimpleRep = toSimpleRep . decompactAddr
  fromSimpleRep = compactAddr . fromSimpleRep

instance HasSpec CompactAddr

instance HasSimpleRep Addr

instance HasSpec Addr

instance HasSimpleRep BootstrapAddress where
  type
    TheSop BootstrapAddress =
      '[ "BootstrapAddress"
           ::: '[ AbstractHash Blake2b_224 Address'
                , NetworkMagic
                , AddrType
                ]
       ]
  toSimpleRep (BootstrapAddress (Address root (Attributes (AddrAttributes _ magic) _) typ)) =
    inject @"BootstrapAddress" @(TheSop BootstrapAddress)
      root
      magic
      typ
  fromSimpleRep rep =
    algebra @(TheSop BootstrapAddress) rep $
      \root magic typ ->
        BootstrapAddress
          (Address root (Attributes (AddrAttributes Nothing magic) (UnparsedFields mempty)) typ)

instance HasSpec BootstrapAddress

instance HasSimpleRep NetworkMagic

instance HasSpec NetworkMagic

instance HasSimpleRep AddrType

instance HasSpec AddrType

instance Typeable b => HasSpec (AbstractHash Blake2b_224 b) where
  type TypeSpec (AbstractHash Blake2b_224 b) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = do
    bytes <- pureGen $ vectorOf 28 arbitrary
    pure $ fromJust $ abstractHashFromBytes (BS.pack bytes)
  shrinkWithTypeSpec _ _ = []
  cardinalTypeSpec _ = TrueSpec
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSimpleRep StakeReference

instance HasSpec StakeReference

instance HasSimpleRep SlotNo32

instance HasSpec SlotNo32

instance HasSimpleRep Ptr

instance HasSpec Ptr

instance HasSimpleRep CertIx where
  type SimpleRep CertIx = Word16
  toSimpleRep = unCertIx
  fromSimpleRep = CertIx

instance HasSpec CertIx

instance Typeable r => HasSimpleRep (Credential r)

instance Typeable r => HasSpec (Credential r)

cKeyHashObj ::
  Typeable r => Term (KeyHash r) -> Term (Credential r)
cKeyHashObj = con @"KeyHashObj"

cScriptHashObj ::
  Typeable r => Term ScriptHash -> Term (Credential r)
cScriptHashObj = con @"ScriptHashObj"

instance HasSimpleRep ScriptHash

instance HasSpec ScriptHash

pickFromFixedPool :: Arbitrary a => Int -> Gen a
pickFromFixedPool n = do
  seed <- chooseInt (0, n)
  variant seed arbitrary

genHashWithDuplicates :: HashAlgorithm h => Gen (Hash h b)
genHashWithDuplicates =
  oneof
    [ pickFromFixedPool 20
    , arbitrary
    ]

instance Typeable r => HasSpec (VRFVerKeyHash r) where
  type TypeSpec (VRFVerKeyHash r) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen $ VRFVerKeyHash <$> genHashWithDuplicates
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance (HashAlgorithm a, Typeable b) => HasSpec (Hash a b) where
  type TypeSpec (Hash a b) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen genHashWithDuplicates
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSimpleRep (ConwayTxCert era)

instance Era era => HasSpec (ConwayTxCert era)

instance HasSimpleRep ConwayDelegCert

instance HasSpec ConwayDelegCert

instance HasSimpleRep PoolCert

instance HasSpec PoolCert

instance HasSimpleRep PoolParams

instance HasSpec PoolParams

instance HasSimpleRep PoolMetadata

instance HasSpec PoolMetadata

instance HasSpec StakePoolRelay where
  type TypeSpec StakePoolRelay = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSimpleRep Port

instance HasSpec Port

instance HasSimpleRep ConwayGovCert

instance HasSpec ConwayGovCert

instance HasSimpleRep Anchor

instance HasSpec Anchor

instance HasSimpleRep Url

instance HasSpec Url where
  type TypeSpec Url = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSpec Text where
  type TypeSpec Text = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

newtype StringSpec = StringSpec {strSpecLen :: Specification Int}

deriving instance Show StringSpec

instance Semigroup StringSpec where
  StringSpec len <> StringSpec len' = StringSpec (len <> len')

instance Monoid StringSpec where
  mempty = StringSpec TrueSpec

instance HasSpec ByteString where
  type TypeSpec ByteString = StringSpec
  emptySpec = mempty
  combineSpec s s' = typeSpec $ s <> s'
  genFromTypeSpec (StringSpec ls) = do
    len <- genFromSpecT ls
    BS.pack <$> vectorOfT len (pureGen arbitrary)
  shrinkWithTypeSpec _ = shrink
  cardinalTypeSpec _ = TrueSpec
  conformsTo bs (StringSpec ls) = BS.length bs `conformsToSpec` ls
  toPreds str (StringSpec len) = satisfies (strLen_ str) len

instance HasSpec ShortByteString where
  type TypeSpec ShortByteString = StringSpec
  emptySpec = mempty
  combineSpec s s' = typeSpec $ s <> s'
  genFromTypeSpec (StringSpec ls) = do
    len <- genFromSpecT ls
    SBS.pack <$> vectorOfT len (pureGen arbitrary)
  shrinkWithTypeSpec _ = shrink
  cardinalTypeSpec _ = TrueSpec
  conformsTo bs (StringSpec ls) = SBS.length bs `conformsToSpec` ls
  toPreds str (StringSpec len) = satisfies (strLen_ str) len

instance StringLike ByteString where
  lengthSpec = StringSpec
  getLengthSpec (StringSpec len) = len
  getLength = BS.length

instance StringLike ShortByteString where
  lengthSpec = StringSpec
  getLengthSpec (StringSpec len) = len
  getLength = SBS.length

data StringW :: [Type] -> Type -> Type where
  StrLenW :: StringLike s => StringW '[s] Int

deriving instance Show (StringW as b)

deriving instance Eq (StringW as b)

strLen_ :: (HasSpec s, StringLike s) => Term s -> Term Int
strLen_ = appTerm StrLenW

instance Syntax StringW

instance Semantics StringW where
  semantics StrLenW = getLength

-- | In this instance there is no way to bring the type variable `s` into scope
--   so we introduce some local functions that have a signature that bring it into scope.
instance Logic StringW where
  propagateTypeSpec StrLenW (Unary HOLE) ts cant = foo ts cant
    where
      foo :: forall s. (HasSpec s, StringLike s) => NumSpec Int -> [Int] -> Specification s
      foo t c = typeSpec $ lengthSpec @s (TypeSpec t c)
  propagateMemberSpec StrLenW (Unary HOLE) xs = bar xs
    where
      bar :: forall s. (HasSpec s, StringLike s) => NonEmpty Int -> Specification s
      bar ys = typeSpec $ lengthSpec @s (MemberSpec ys)

  mapTypeSpec :: forall a b. StringW '[a] b -> TypeSpec a -> Specification b
  mapTypeSpec StrLenW ss = getLengthSpec @a ss

class StringLike s where
  lengthSpec :: Specification Int -> TypeSpec s
  getLengthSpec :: TypeSpec s -> Specification Int
  getLength :: s -> Int

instance HasSimpleRep Delegatee

instance HasSpec Delegatee

instance HasSimpleRep DRep

instance HasSpec DRep

instance HasSimpleRep Withdrawals

instance HasSpec Withdrawals

instance HasSimpleRep RewardAccount

instance HasSpec RewardAccount

instance HasSimpleRep Network

instance HasSpec Network

instance HasSimpleRep MultiAsset

instance HasSpec MultiAsset where
  emptySpec =
    defaultMapSpec
      { mapSpecElem = constrained' $ \_ innerMap ->
          forAll innerMap $ \kv' ->
            lit 0 <=. snd_ kv'
      }

instance HasSimpleRep AssetName where
  type SimpleRep AssetName = ShortByteString
  toSimpleRep (AssetName sbs) = sbs
  fromSimpleRep sbs = AssetName sbs

instance HasSpec AssetName

instance HasSimpleRep PolicyID

instance HasSpec PolicyID

instance HasSimpleRep TxAuxDataHash

instance HasSpec TxAuxDataHash

instance Typeable era => HasSimpleRep (VotingProcedures era)

instance Typeable era => HasSpec (VotingProcedures era)

instance HasSimpleRep (VotingProcedure era)

instance Typeable era => HasSpec (VotingProcedure era)

instance HasSimpleRep Vote

instance HasSpec Vote

instance HasSimpleRep GovActionId

instance HasSpec GovActionId where
  shrinkWithTypeSpec _ _ = []

instance HasSimpleRep GovActionIx

instance HasSpec GovActionIx

instance HasSimpleRep (GovPurposeId p)

instance Typeable p => HasSpec (GovPurposeId p)

instance Typeable era => HasSimpleRep (GovAction era)

instance EraSpecPParams era => HasSpec (GovAction era)

instance HasSimpleRep (Constitution era)

instance EraPParams era => HasSpec (Constitution era)

instance HasSimpleRep (ConwayPParams StrictMaybe c)

instance Typeable c => HasSpec (ConwayPParams StrictMaybe c)

instance HasSimpleRep (ConwayPParams Identity era)

instance Era era => HasSpec (ConwayPParams Identity era)

instance HasSimpleRep CoinPerByte where
  -- TODO: consider `SimpleRep Coin` instead if this is annoying
  type SimpleRep CoinPerByte = Coin
  fromSimpleRep = CoinPerByte
  toSimpleRep = unCoinPerByte

instance HasSpec CoinPerByte

instance HasSpec Char where
  type TypeSpec Char = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSpec CostModel where
  type TypeSpec CostModel = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSimpleRep Language

instance HasSpec Language

instance HasSimpleRep (NoUpdate a)

instance Typeable a => HasSpec (NoUpdate a)

instance Typeable a => HasSimpleRep (THKD tag StrictMaybe a) where
  type SimpleRep (THKD tag StrictMaybe a) = SOP (TheSop (StrictMaybe a))
  fromSimpleRep = THKD . fromSimpleRep
  toSimpleRep (THKD sm) = toSimpleRep sm

instance (IsNormalType a, Typeable tag, HasSpec a) => HasSpec (THKD tag StrictMaybe a)

instance Typeable a => HasSimpleRep (THKD tag Identity a) where
  type SimpleRep (THKD tag Identity a) = a
  fromSimpleRep = THKD
  toSimpleRep (THKD a) = a

instance
  ( IsNormalType a
  , Typeable tag
  , HasSpec a
  , GenericallyInstantiated (THKD tag Identity a)
  ) =>
  HasSpec (THKD tag Identity a)

instance HasSimpleRep GovActionPurpose

instance HasSpec GovActionPurpose

instance HasSimpleRep Voter

instance HasSpec Voter

-- TODO: this might be a problem considering duplicates in the list! This
-- type might require having its own `HasSpec` at some point
instance (Typeable a, Ord a) => HasSimpleRep (SOS.OSet a) where
  type SimpleRep (SOS.OSet a) = [a]
  fromSimpleRep = SOS.fromStrictSeq . StrictSeq.fromList
  toSimpleRep = toList . SOS.toStrictSeq

instance (Ord a, HasSpec a) => HasSpec (SOS.OSet a)

instance (Typeable a, Ord a) => Forallable (SOS.OSet a) a

instance Typeable era => HasSimpleRep (ProposalProcedure era)

instance EraSpecPParams era => HasSpec (ProposalProcedure era)

pProcDeposit_ ::
  Term (ProposalProcedure ConwayEra) ->
  Term Coin
pProcDeposit_ = sel @0

pProcGovAction_ ::
  Term (ProposalProcedure ConwayEra) ->
  Term (GovAction ConwayEra)
pProcGovAction_ = sel @2

instance HasSimpleRep ValidityInterval

instance HasSpec ValidityInterval

instance HasSimpleRep DRepState

instance HasSpec DRepState

instance HasSimpleRep CommitteeAuthorization

instance HasSpec CommitteeAuthorization

instance HasSimpleRep (CommitteeState era)

instance Era era => HasSpec (CommitteeState era)

instance Typeable era => HasSimpleRep (VState era)

instance Era era => HasSpec (VState era)

instance HasSimpleRep (PState era)

instance Era era => HasSpec (PState era)

instance HasSimpleRep (DState era)

instance Era era => HasSpec (DState era)

instance HasSimpleRep FutureGenDeleg

instance HasSpec FutureGenDeleg

instance HasSimpleRep GenDelegPair

instance HasSpec GenDelegPair

instance HasSimpleRep GenDelegs

instance HasSpec GenDelegs

instance HasSimpleRep InstantaneousRewards

instance HasSpec InstantaneousRewards

type UMapTypes =
  '[ Map (Credential 'Staking) RDPair
   , Map Ptr (Credential 'Staking)
   , Map (Credential 'Staking) (KeyHash 'StakePool)
   , Map (Credential 'Staking) DRep
   ]

instance HasSimpleRep UMap where
  type TheSop UMap = '["UMap" ::: UMapTypes]
  toSimpleRep um = inject @"UMap" @'["UMap" ::: UMapTypes] (rdPairMap um) (ptrMap um) (sPoolMap um) (dRepMap um)
  fromSimpleRep rep = algebra @'["UMap" ::: UMapTypes] rep unify

instance HasSpec UMap

instance HasSimpleRep RDPair where
  type TheSop RDPair = '["RDPair" ::: '[SimpleRep Coin, SimpleRep Coin]]
  toSimpleRep (RDPair rew dep) =
    inject
      @"RDPair"
      @'["RDPair" ::: '[SimpleRep Coin, SimpleRep Coin]]
      (toSimpleRep rew)
      (toSimpleRep dep)
  fromSimpleRep rep =
    algebra @'["RDPair" ::: '[SimpleRep Coin, SimpleRep Coin]]
      rep
      ( \rew dep ->
          RDPair
            (fromSimpleRep rew)
            (fromSimpleRep dep)
      )

instance HasSpec RDPair

instance Typeable era => HasSimpleRep (ShelleyCertState era)

instance EraCertState era => HasSpec (ShelleyCertState era)

instance Typeable era => HasSimpleRep (ConwayCertState era)

instance ConwayEraCertState era => HasSpec (ConwayCertState era)

instance HasSimpleRep (GovRelation StrictMaybe)

instance HasSpec (GovRelation StrictMaybe)

instance (Typeable (CertState era), Era era) => HasSimpleRep (GovEnv era)

instance
  (EraSpecPParams era, EraTxOut era, EraCertState era, EraGov era, HasSpec (CertState era)) =>
  HasSpec (GovEnv era)

instance Typeable era => HasSimpleRep (GovActionState era)

instance (Era era, EraSpecPParams era) => HasSpec (GovActionState era)

gasId_ ::
  Term (GovActionState ConwayEra) ->
  Term GovActionId
gasId_ = sel @0

gasCommitteeVotes_ ::
  Term (GovActionState ConwayEra) ->
  Term (Map (Credential 'HotCommitteeRole) Vote)
gasCommitteeVotes_ = sel @1

gasDRepVotes_ ::
  Term (GovActionState ConwayEra) ->
  Term (Map (Credential 'DRepRole) Vote)
gasDRepVotes_ = sel @2

gasProposalProcedure_ ::
  Term (GovActionState ConwayEra) ->
  Term (ProposalProcedure ConwayEra)
gasProposalProcedure_ = sel @4

-- =====================================================================
-- Proposals from Cardano.Ledger.Conway.Governance.Proposals
-- =====================================================================
-- The correct way to think of Proposals (definition for reference below)
--
-- data Proposals era = Proposals
--  { pProps :: !(OMap.OMap (GovActionId ) (GovActionState era))
--  , pRoots :: !(GovRelation PRoot era)
--  , pGraph :: !(GovRelation PGraph era)
--  }
--  is four copies of the following abstract type: ProposalType
--  one for each @`GovActionPurpose`@ (PParamUpdate,HardFork,Committee,Constitution)
--  See the extensive notes in Cardano.Ledger.Conway.Governance.Proposals
--
--  data ProposalTree a = Node (StrictMaybe a) [ProposalTree a]
--
--  In Haskell this abstration of Proposals would look something like
--
--  data ProposalsType = ProposalsType ProposalTree ProposalTree ProposalTree ProposalTree  [GAS]
--
--  Thus the SimpleRep for Proposals is a Sum type with 5 different cases, thus we need to provde
--  toSimpleRep and fromSimpleRep methods to make the HasSimpleRep instance.

type GAS era = GovActionState era

type ProposalTree era = (StrictMaybe GovActionId, [Tree (GAS era)])

type ProposalsType era =
  '[ ProposalTree era -- PParamUpdate
   , ProposalTree era -- HardFork
   , ProposalTree era -- Committee
   , ProposalTree era -- Constitution
   , [GAS era] -- Everything else (TreasuryWithdrawals, Info) which can't be grouped into one of the 4 purposes.
   -- TODO - in order to improve the distribution of orders in the OMap
   -- one could try doing something like this as well to materialize the order:
   -- , TotalOrder (GovActionId )
   -- However, (1) I have no clue how this would work in detail and (2) the approach
   -- of DFS gives us a lot of testing already, and there are bigger fish to fry than
   -- this right now.
   ]

instance EraPParams era => HasSimpleRep (Proposals era) where
  type TheSop (Proposals era) = '["Proposals" ::: ProposalsType era]
  toSimpleRep props =
    inject @"Proposals" @'["Proposals" ::: ProposalsType era]
      (buildProposalTree $ coerce grPParamUpdate)
      (buildProposalTree $ coerce grHardFork)
      (buildProposalTree $ coerce grCommittee)
      (buildProposalTree $ coerce grConstitution)
      (Map.elems $ Map.withoutKeys idMap treeKeys)
    where
      GovRelation {..} = toGovRelationTree props
      idMap = proposalsActionsMap props

      treeKeys =
        foldMap
          keys
          [ coerce grPParamUpdate
          , coerce grHardFork
          , coerce grCommittee
          , coerce grConstitution
          ]

      buildProposalTree :: TreeMaybe GovActionId -> ProposalTree era
      buildProposalTree (TreeMaybe (Node mId cs)) = (mId, map buildTree cs)

      buildTree :: Tree (StrictMaybe GovActionId) -> Tree (GAS era)
      buildTree (Node (SJust gid) cs) | Just gas <- Map.lookup gid idMap = Node gas (map buildTree cs)
      buildTree _ =
        error "toSimpleRep @Proposals: toGovRelationTree returned trees with Nothing nodes below the root"

      keys :: TreeMaybe GovActionId -> Set GovActionId
      keys (TreeMaybe t) = foldMap (strictMaybe mempty Set.singleton) t

  fromSimpleRep rep =
    algebra @'["Proposals" ::: ProposalsType era]
      rep
      $ \(rPPUp, ppupTree) (rHF, hfTree) (rCom, comTree) (rCon, conTree) others ->
        let root = GovRelation (coerce rPPUp) (coerce rHF) (coerce rCom) (coerce rCon)
            -- TODO: note, this doesn't roundtrip and the distribution is a bit iffy. See the TODO
            -- above for ideas on how to deal with this.
            oMap = foldMap (foldMap mkOMap) [ppupTree, hfTree, comTree, conTree] <> OMap.fromFoldable others
         in unsafeMkProposals root oMap
    where
      mkOMap (Node a ts) = a OMap.<| foldMap mkOMap ts

instance
  ( EraSpecPParams era
  , Arbitrary (Proposals era)
  , HasSpec (Tree (GAS era))
  ) =>
  HasSpec (Proposals era)
  where
  shrinkWithTypeSpec _ props = shrink props

psPParamUpdate_ ::
  (EraSpecPParams era, Arbitrary (Proposals era)) =>
  Term (Proposals era) -> Term (ProposalTree era)
psPParamUpdate_ = sel @0

data ProposalsSplit = ProposalsSplit
  { psPPChange :: Integer
  , psHFInitiation :: Integer
  , psUpdateCommittee :: Integer
  , psNewConstitution :: Integer
  , psOthers :: Integer
  }
  deriving (Show, Eq, Generic)

instance EncCBOR ProposalsSplit where
  encCBOR x@(ProposalsSplit _ _ _ _ _) =
    let ProposalsSplit {..} = x
     in encode $
          Rec ProposalsSplit
            !> To psPPChange
            !> To psHFInitiation
            !> To psUpdateCommittee
            !> To psNewConstitution
            !> To psOthers

instance DecCBOR ProposalsSplit where
  decCBOR =
    decode $
      RecD ProposalsSplit
        <! From
        <! From
        <! From
        <! From
        <! From

instance ToExpr ProposalsSplit

instance NFData ProposalsSplit

proposalSplitSum :: ProposalsSplit -> Integer
proposalSplitSum ProposalsSplit {..} =
  sum
    [ psPPChange
    , psHFInitiation
    , psUpdateCommittee
    , psNewConstitution
    , psOthers
    ]

-- | Randomly splits a number into the given number of terms. Might undershoot
-- due to rounding
splitInto :: Integer -> Int -> Gen [Integer]
splitInto budget numSplits = do
  splits <- vectorOf numSplits $ arbitrary @(NonNegative Int)
  let unwrappedSplits = fmap getNonNegative splits
  let splitsTotal = toInteger $ sum unwrappedSplits
  pure $
    if splitsTotal == 0 || budget == 0
      then replicate numSplits 0
      else (* (budget `div` splitsTotal)) . toInteger <$> unwrappedSplits

genProposalsSplit :: Integer -> Gen ProposalsSplit
genProposalsSplit maxTotal = do
  actualMaxTotal <- choose (0, maxTotal)
  splits <- actualMaxTotal `splitInto` 5
  case splits of
    [ psPPChange
      , psHFInitiation
      , psUpdateCommittee
      , psNewConstitution
      , psOthers
      ] -> pure ProposalsSplit {..}
    l ->
      error $
        "impossible: should have exactly 5 values, but has "
          <> show (length l)

instance
  ( HasSpec (SimpleRep (Proposals era))
  , HasSpec (Proposals era)
  , HasSimpleRep (Proposals era)
  , era ~ ConwayEra
  , EraSpecPParams era
  ) =>
  HasGenHint (Proposals era)
  where
  type Hint (Proposals era) = ProposalsSplit
  giveHint ProposalsSplit {..} = constrained' $ \ppuTree hfTree comTree conTree others ->
    [ limitForest psPPChange ppuTree
    , limitForest psHFInitiation hfTree
    , limitForest psUpdateCommittee comTree
    , limitForest psNewConstitution conTree
    , [genHint psOthers others]
    ]
    where
      limitForest limit forest =
        [ genHint limit (snd_ forest)
        , forAll (snd_ forest) $ genHint (Just 2, limit)
        ]

instance HasSimpleRep (EnactSignal ConwayEra)

instance HasSpec (EnactSignal ConwayEra)

instance Typeable era => HasSimpleRep (EnactState era)

instance (EraGov era, EraTxOut era, EraSpecPParams era) => HasSpec (EnactState era)

instance HasSimpleRep (Committee era)

instance Era era => HasSpec (Committee era)

instance
  ( HasSpec (InstantStake era)
  , Typeable era
  ) =>
  HasSimpleRep (RatifyEnv era)

instance
  ( HasSpec (InstantStake era)
  , Era era
  ) =>
  HasSpec (RatifyEnv era)

instance HasSimpleRep (RatifyState ConwayEra)

instance HasSpec (RatifyState ConwayEra)

instance HasSimpleRep (RatifySignal ConwayEra)

instance HasSpec (RatifySignal ConwayEra)

instance HasSimpleRep PoolDistr

instance HasSpec PoolDistr

instance HasSimpleRep IndividualPoolStake

instance HasSpec IndividualPoolStake

instance HasSimpleRep (ConwayGovCertEnv ConwayEra)

instance HasSpec (ConwayGovCertEnv ConwayEra)

instance Typeable era => HasSimpleRep (PoolEnv era)

instance (EraGov era, EraTxOut era, EraSpecPParams era) => HasSpec (PoolEnv era)

instance Era era => HasSimpleRep (CertEnv era)

instance (EraGov era, EraTxOut era, EraSpecPParams era) => HasSpec (CertEnv era)

instance HasSimpleRep NonMyopic

instance HasSpec NonMyopic

instance HasSimpleRep Likelihood

instance HasSpec Likelihood

instance HasSimpleRep LogWeight

instance HasSpec LogWeight

instance HasSimpleRep ChainAccountState

instance HasSpec ChainAccountState

instance HasSimpleRep SnapShot

instance HasSpec SnapShot

instance HasSimpleRep Stake

instance HasSpec Stake

instance (Typeable k, Typeable v, VMap.Vector vk k, VMap.Vector vv v) => HasSimpleRep (VMap vk vv k v) where
  type SimpleRep (VMap vk vv k v) = Map k v
  toSimpleRep = VMap.toMap
  fromSimpleRep = VMap.fromMap

instance
  ( VMap.Vector vk k
  , VMap.Vector vv v
  , Typeable vk
  , Typeable vv
  , Ord k
  , Eq (vv v)
  , Eq (vk k)
  , HasSpec k
  , HasSpec v
  , IsNormalType v
  , IsNormalType k
  ) =>
  HasSpec (VMap vk vv k v)

instance HasSimpleRep SnapShots

instance HasSpec SnapShots

instance (Typeable (CertState era), EraTxOut era) => HasSimpleRep (LedgerState era)

instance
  ( EraTxOut era
  , HasSpec (TxOut era)
  , IsNormalType (TxOut era)
  , HasSpec (GovState era)
  , EraStake era
  , EraCertState era
  , IsNormalType (CertState era)
  , HasSpec (InstantStake era)
  , HasSpec (CertState era)
  ) =>
  HasSpec (LedgerState era)

instance (Typeable (InstantStake era), Typeable (GovState era), Typeable era) => HasSimpleRep (UTxOState era)

instance
  ( EraTxOut era
  , HasSpec (TxOut era)
  , IsNormalType (TxOut era)
  , HasSpec (GovState era)
  , HasSpec (InstantStake era)
  ) =>
  HasSpec (UTxOState era)

instance HasSimpleRep (ShelleyInstantStake era)

instance Typeable era => HasSpec (ShelleyInstantStake era)

instance HasSimpleRep (ConwayInstantStake era)

instance Typeable era => HasSpec (ConwayInstantStake era)

instance Typeable (TxOut era) => HasSimpleRep (UTxO era)

instance
  (Era era, HasSpec (TxOut era), IsNormalType (TxOut era)) =>
  HasSpec (UTxO era)

instance HasSimpleRep (ConwayGovState ConwayEra)

instance HasSpec (ConwayGovState ConwayEra)

instance HasSimpleRep (DRepPulsingState ConwayEra)

instance HasSpec (DRepPulsingState ConwayEra)

instance HasSimpleRep (PulsingSnapshot ConwayEra)

instance HasSpec (PulsingSnapshot ConwayEra)

type DRepPulserTypes =
  '[ Int
   , UMap
   , Int
   , InstantStake ConwayEra
   , PoolDistr
   , Map DRep (CompactForm Coin)
   , Map (Credential 'DRepRole) DRepState
   , EpochNo
   , CommitteeState ConwayEra
   , EnactState ConwayEra
   , StrictSeq (GovActionState ConwayEra)
   , Map (Credential 'Staking) (CompactForm Coin)
   , Map (KeyHash 'StakePool) PoolParams
   ]

instance
  HasSimpleRep
    (DRepPulser ConwayEra Identity (RatifyState ConwayEra))
  where
  type
    TheSop (DRepPulser ConwayEra Identity (RatifyState ConwayEra)) =
      '["DRepPulser" ::: DRepPulserTypes]
  toSimpleRep DRepPulser {..} =
    inject @"DRepPulser" @'["DRepPulser" ::: DRepPulserTypes]
      dpPulseSize
      dpUMap
      dpIndex
      dpInstantStake
      dpStakePoolDistr
      dpDRepDistr
      dpDRepState
      dpCurrentEpoch
      dpCommitteeState
      dpEnactState
      dpProposals
      dpProposalDeposits
      dpPoolParams
  fromSimpleRep rep =
    algebra @'["DRepPulser" ::: DRepPulserTypes]
      rep
      $ \ps um b sd spd dd ds ce cs es p pds poolps ->
        DRepPulser ps um b sd spd dd ds ce cs es p pds testGlobals poolps

instance HasSpec (DRepPulser ConwayEra Identity (RatifyState ConwayEra))

instance (Typeable (CertState era), Era era) => HasSimpleRep (UtxoEnv era)

instance
  (EraGov era, EraTxOut era, EraSpecPParams era, EraCertState era, HasSpec (CertState era)) =>
  HasSpec (UtxoEnv era)

-- ================================================================
-- All the Tx instances

-- Unlike ShelleyTx, AlonzoTx is just a data type, and the generic instances work fine
-- BUT, all the type families inside need constraints

instance
  ( Typeable (TxAuxData era)
  , Typeable (TxBody era)
  , Typeable (TxWits era)
  , Era era
  ) =>
  HasSimpleRep (AlonzoTx era)

instance
  ( EraSpecPParams era
  , HasSpec (TxBody era)
  , HasSpec (TxWits era)
  , HasSpec (TxAuxData era)
  , IsNormalType (TxAuxData era)
  ) =>
  HasSpec (AlonzoTx era)

-- NOTE: this is a representation of the `ShelleyTx` type. You can't
-- simply use the generics to derive the `SimpleRep` for `ShelleyTx`
-- because the type is memoized. So instead we say that the representation
-- is the same as what you would get from using the `ShelleyTx` pattern.
type ShelleyTxTypes era =
  '[ TxBody era
   , TxWits era
   , Maybe (TxAuxData era)
   ]

instance
  ( EraTxOut era
  , EraTx era
  , EraSpecPParams era
  , HasSpec (TxBody era)
  , HasSpec (TxWits era)
  , HasSpec (TxAuxData era)
  , IsNormalType (TxAuxData era)
  ) =>
  HasSpec (ShelleyTx era)

instance HasSimpleRep (Tx ConwayEra) where
  type TheSop (Tx ConwayEra) = TheSop (AlonzoTx ConwayEra)
  toSimpleRep = toSimpleRep . unConwayTx
  fromSimpleRep = MkConwayTx . fromSimpleRep

instance HasSpec (Tx ConwayEra)

instance (EraTx era, EraTxOut era, EraSpecPParams era) => HasSimpleRep (ShelleyTx era) where
  type TheSop (ShelleyTx era) = '["ShelleyTx" ::: ShelleyTxTypes era]
  toSimpleRep (ShelleyTx body wits auxdata) =
    inject @"ShelleyTx" @'["ShelleyTx" ::: ShelleyTxTypes era]
      body
      wits
      (strictMaybeToMaybe auxdata)
  fromSimpleRep rep =
    algebra @'["ShelleyTx" ::: ShelleyTxTypes era]
      rep
      (\body wits aux -> ShelleyTx body wits (maybeToStrictMaybe aux))

instance HasSimpleRep IsValid

instance HasSpec IsValid

-- ===============================================================
-- All the TxAuxData instances

-- NOTE: we don't generate or talk about plutus scripts (yet!)
type AlonzoTxAuxDataTypes era =
  '[ Map Word64 Metadatum
   , StrictSeq (Timelock era)
   ]

instance AlonzoEraScript era => HasSimpleRep (AlonzoTxAuxData era) where
  type
    TheSop (AlonzoTxAuxData era) =
      '["AlonzoTxOutData" ::: AlonzoTxAuxDataTypes era]
  toSimpleRep (AlonzoTxAuxData metaMap tsSeq _) =
    inject @"AlonzoTxAuxData" @'["AlonzoTxAuxData" ::: AlonzoTxAuxDataTypes era]
      metaMap
      tsSeq
  fromSimpleRep rep =
    algebra @'["AlonzoTxAuxData" ::: AlonzoTxAuxDataTypes era] rep $
      \metaMap tsSeq -> AlonzoTxAuxData metaMap tsSeq mempty

instance
  ( Era era
  , AlonzoEraScript era
  , NativeScript era ~ Timelock era
  ) =>
  HasSpec (AlonzoTxAuxData era)

-- NOTE: we don't generate or talk about plutus scripts (yet!)
type AllegraTxAuxDataTypes era =
  '[ Map Word64 Metadatum
   , StrictSeq (Timelock era)
   ]

instance Era era => HasSimpleRep (AllegraTxAuxData era) where
  type
    TheSop (AllegraTxAuxData era) =
      '["AllegraTxOutData" ::: AllegraTxAuxDataTypes era]
  toSimpleRep (AllegraTxAuxData metaMap tsSeq) =
    inject @"AllegraTxAuxData" @'["AllegraTxAuxData" ::: AllegraTxAuxDataTypes era]
      metaMap
      tsSeq
  fromSimpleRep rep =
    algebra @'["AllegraTxAuxData" ::: AllegraTxAuxDataTypes era] rep $
      \metaMap tsSeq -> AllegraTxAuxData metaMap tsSeq

instance
  ( Era era
  , AllegraEraScript era
  , NativeScript era ~ Timelock era
  ) =>
  HasSpec (AllegraTxAuxData era)

type ShelleyTxAuxDataTypes era =
  '[ Map Word64 Metadatum
   ]

instance Era era => HasSimpleRep (ShelleyTxAuxData era) where
  type
    TheSop (ShelleyTxAuxData era) =
      '["ShelleyTxAuxData" ::: ShelleyTxAuxDataTypes era]
  toSimpleRep (ShelleyTxAuxData metaMap) =
    inject @"ShelleyTxAuxData" @'["ShelleyTxAuxData" ::: ShelleyTxAuxDataTypes era]
      metaMap
  fromSimpleRep rep =
    algebra @'["ShelleyTxAuxData" ::: ShelleyTxAuxDataTypes era] rep $
      \metaMap -> ShelleyTxAuxData metaMap

instance
  ( Era era
  , AllegraEraScript era
  , NativeScript era ~ Timelock era
  ) =>
  HasSpec (ShelleyTxAuxData era)

instance HasSimpleRep Metadatum

instance HasSpec Metadatum

-- ===============================================================
-- All the TxWits instances

type AlonzoTxWitsTypes =
  '[ Set (WitVKey 'Witness)
   , Set BootstrapWitness
   ]

instance AlonzoEraScript era => HasSimpleRep (AlonzoTxWits era) where
  type
    TheSop (AlonzoTxWits era) =
      '["AlonzoTxWits" ::: AlonzoTxWitsTypes]
  toSimpleRep (AlonzoTxWits vkeyWits bootstrapWits _ _ _) =
    inject @"AlonzoTxWits" @'["AlonzoTxWits" ::: AlonzoTxWitsTypes]
      vkeyWits
      bootstrapWits
  fromSimpleRep rep =
    algebra @'["AlonzoTxWits" ::: AlonzoTxWitsTypes] rep $
      \vkeyWits bootstrapWits -> AlonzoTxWits vkeyWits bootstrapWits mempty (TxDats mempty) (Redeemers mempty)

instance AlonzoEraScript era => HasSpec (AlonzoTxWits era)

type ShelleyTxWitsTypes era =
  '[ Set (WitVKey 'Witness)
   , Set BootstrapWitness
   ]

instance EraScript era => HasSimpleRep (ShelleyTxWits era) where
  type
    TheSop (ShelleyTxWits era) =
      '["ShelleyTxWits" ::: ShelleyTxWitsTypes era]
  toSimpleRep (ShelleyTxWits vkeyWits _ bootstrapWits) =
    inject @"ShelleyTxWits" @'["ShelleyTxWits" ::: ShelleyTxWitsTypes era]
      vkeyWits
      bootstrapWits
  fromSimpleRep rep =
    algebra @'["ShelleyTxWits" ::: ShelleyTxWitsTypes era] rep $
      \vkeyWits bootstrapWits -> ShelleyTxWits vkeyWits mempty bootstrapWits

instance EraScript era => HasSpec (ShelleyTxWits era)

instance Typeable r => HasSpec (WitVKey r) where
  type TypeSpec (WitVKey r) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance HasSpec BootstrapWitness where
  type TypeSpec BootstrapWitness = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = assert True

instance Era era => HasSimpleRep (LedgerEnv era)

instance (HasSpec (PParams era), Era era) => HasSpec (LedgerEnv era)

onJust' ::
  ( HasSpec a
  , IsNormalType a
  , IsPred p
  ) =>
  Term (StrictMaybe a) ->
  (Term a -> p) ->
  Pred
onJust' tm p = caseOn tm (branch $ const True) (branch p)

onSized ::
  (HasSpec a, IsPred p) =>
  Term (Sized a) ->
  (Term a -> p) ->
  Pred
onSized sz p = match sz $ \a _ -> p a

instance Typeable era => HasSimpleRep (ConwayDelegEnv era)

instance (HasSpec (PParams era), Era era) => HasSpec (ConwayDelegEnv era)

instance Era era => HasSimpleRep (EpochState era)

instance
  ( EraTxOut era
  , HasSpec (TxOut era)
  , IsNormalType (TxOut era)
  , HasSpec (GovState era)
  , EraStake era
  , EraCertState era
  , IsNormalType (CertState era)
  , HasSpec (InstantStake era)
  , HasSpec (CertState era)
  ) =>
  HasSpec (EpochState era)

instance HasSimpleRep FreeVars

instance HasSpec FreeVars

instance HasSimpleRep PoolRewardInfo

instance HasSpec PoolRewardInfo

instance HasSimpleRep LeaderOnlyReward

instance HasSpec LeaderOnlyReward

instance HasSimpleRep StakeShare

instance HasSpec StakeShare

instance HasSimpleRep BlocksMade

instance HasSpec BlocksMade

instance HasSimpleRep RewardType

instance HasSpec RewardType

instance HasSimpleRep RewardAns

instance HasSpec RewardAns

instance HasSimpleRep PulsingRewUpdate where
  type SimpleRep PulsingRewUpdate = SimpleRep RewardUpdate
  toSimpleRep (Complete x) = toSimpleRep x
  toSimpleRep x@(Pulsing _ _) = toSimpleRep (runShelleyBase (fst <$> completeRupd x))
  fromSimpleRep x = Complete (fromSimpleRep x)

instance HasSpec PulsingRewUpdate

instance (Typeable (StashedAVVMAddresses era), Era era) => HasSimpleRep (NewEpochState era)

instance
  ( EraTxOut era
  , HasSpec (TxOut era)
  , IsNormalType (TxOut era)
  , HasSpec (GovState era)
  , HasSpec (StashedAVVMAddresses era)
  , EraStake era
  , EraCertState era
  , IsNormalType (CertState era)
  , HasSpec (CertState era)
  , HasSpec (InstantStake era)
  ) =>
  HasSpec (NewEpochState era)

instance HasSimpleRep Reward

instance HasSpec Reward

instance HasSimpleRep RewardSnapShot

instance HasSpec RewardSnapShot

instance HasSimpleRep RewardUpdate

instance HasSpec RewardUpdate

type PulserTypes =
  '[ Int
   , FreeVars
   , VMap VMap.VB VMap.VP (Credential 'Staking) (CompactForm Coin)
   , RewardAns
   ]

instance HasSimpleRep Pulser where
  type TheSop Pulser = '["Pulser" ::: PulserTypes]
  toSimpleRep (RSLP n free bal ans) =
    inject @"Pulser" @'["Pulser" ::: PulserTypes]
      n
      free
      bal
      ans
  fromSimpleRep rep =
    algebra @'["Pulser" ::: PulserTypes]
      rep
      RSLP

instance HasSpec Pulser

instance (Typeable (Tx era), Typeable era) => HasSimpleRep (CertsEnv era)

instance (EraGov era, EraTx era, EraSpecPParams era, HasSpec (Tx era)) => HasSpec (CertsEnv era)

-- CompactForm

class Coercible a b => CoercibleLike a b where
  coerceSpec ::
    Specification b ->
    Specification a
  getCoerceSpec ::
    TypeSpec a ->
    Specification b

instance Typeable krole => CoercibleLike (KeyHash krole) (KeyHash 'Witness) where
  coerceSpec (ExplainSpec es x) = explainSpec es (coerceSpec x)
  coerceSpec (TypeSpec z excl) = TypeSpec z $ coerceKeyRole <$> excl
  coerceSpec (MemberSpec s) = MemberSpec $ coerceKeyRole <$> s
  coerceSpec (ErrorSpec e) = ErrorSpec e
  coerceSpec (SuspendedSpec x p) = constrained $ \x' ->
    [ p
    , reify x' coerceKeyRole (==. V x)
    ]
  coerceSpec TrueSpec = TrueSpec

  getCoerceSpec ::
    TypeSpec (KeyHash krole) ->
    Specification (KeyHash 'Witness)
  getCoerceSpec x = TypeSpec x mempty

instance CoercibleLike (CompactForm Coin) Word64 where
  coerceSpec (TypeSpec (NumSpecInterval lo hi) excl) =
    TypeSpec (NumSpecInterval lo hi) $ CompactCoin <$> excl
  coerceSpec (MemberSpec s) = MemberSpec $ CompactCoin <$> s
  coerceSpec (ErrorSpec e) = ErrorSpec e
  coerceSpec (SuspendedSpec x p) = constrained $ \x' ->
    [ p
    , reify x' unCompactCoin (==. V x)
    ]
  coerceSpec TrueSpec = TrueSpec
  coerceSpec (ExplainSpec es x) = ExplainSpec es (coerceSpec x)

  getCoerceSpec ::
    TypeSpec (CompactForm Coin) ->
    Specification Word64
  getCoerceSpec (NumSpecInterval a b) = TypeSpec (NumSpecInterval a b) mempty

data CoercibleW (args :: [Type]) (res :: Type) where
  CoerceW :: (CoercibleLike a b, Coercible a b) => CoercibleW '[a] b

deriving instance Show (CoercibleW args res)

deriving instance Eq (CoercibleW args res)

instance Syntax CoercibleW

instance Semantics CoercibleW where
  semantics = \case
    CoerceW -> coerce

instance Logic CoercibleW where
  propagateMemberSpec CoerceW (Unary HOLE) xs = coerceSpec $ MemberSpec xs
  propagateTypeSpec CoerceW (Unary HOLE) ts cant = coerceSpec $ TypeSpec ts cant

  mapTypeSpec :: forall a b. CoercibleW '[a] b -> TypeSpec a -> Specification b
  mapTypeSpec CoerceW = getCoerceSpec @a

coerce_ ::
  forall a b.
  ( HasSpec a
  , HasSpec b
  , CoercibleLike a b
  ) =>
  Term a ->
  Term b
coerce_ = appTerm CoerceW

-- ==============================================================

data CoinW (ds :: [Type]) (res :: Type) where
  ToDeltaW :: CoinW '[Coin] DeltaCoin

deriving instance Show (CoinW args res)

deriving instance Eq (CoinW args res)

instance Syntax CoinW

instance Semantics CoinW where
  semantics = \case
    ToDeltaW -> DeltaCoin . unCoin

toDelta_ ::
  Term Coin ->
  Term DeltaCoin
toDelta_ = appTerm ToDeltaW

instance Logic CoinW where
  propagateMemberSpec ToDeltaW (Unary HOLE) xs = MemberSpec (NE.map deltaToCoin xs)

  propagateTypeSpec ToDeltaW (Unary HOLE) (NumSpecInterval l h) cant =
    TypeSpec
      (NumSpecInterval (fromIntegral <$> l) (fromIntegral <$> h))
      (map deltaToCoin cant)

  mapTypeSpec ToDeltaW (NumSpecInterval l h) = typeSpec (NumSpecInterval (fromIntegral <$> l) (fromIntegral <$> h))

deltaToCoin :: DeltaCoin -> Coin
deltaToCoin (DeltaCoin i) = Coin i

instance Typeable era => HasSimpleRep (ShelleyGovState era)

instance (EraTxOut era, EraGov era, EraSpecPParams era) => HasSpec (ShelleyGovState era)

instance HasSimpleRep ShelleyDelegCert

instance HasSpec ShelleyDelegCert

instance HasSimpleRep MIRCert

instance HasSpec MIRCert

instance HasSimpleRep MIRTarget

instance HasSpec MIRTarget

instance HasSimpleRep MIRPot

instance HasSpec MIRPot

instance HasSimpleRep (ShelleyTxCert era)

instance Era era => HasSpec (ShelleyTxCert era)

instance HasSimpleRep GenesisDelegCert

instance HasSpec GenesisDelegCert
