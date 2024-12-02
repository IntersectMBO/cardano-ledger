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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- RecordWildCards cause name shadowing warnings in ghc-8.10.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -O0 #-}
#endif

-- | This module provides the necessary instances of `HasSpec`
-- and `HasSimpleRep` to write specs for the environments,
-- states, and signals in the STS rules of the Ledger. Note some simple
-- types used in the PParams (Coin, EpochInterval, etc.) have their
-- instances defined in Test.Cardano.Ledger.Constrained.Conway.Instances.Basic
-- and they are reexported here.
module Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger (
  ConwayFn,
  StringFn,
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
  IsConwayUniv,
  gasId_,
  gasCommitteeVotes_,
  gasDRepVotes_,
  gasProposalProcedure_,
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
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..), AuxiliaryDataHash)
import Cardano.Ledger.Alonzo.TxOut
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), Sized (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.HKD
import Cardano.Ledger.Keys (
  BootstrapWitness,
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash,
  KeyRole (..),
  VRFVerKeyHash (..),
  WitVKey,
 )
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.MemoBytes
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.PoolParams
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley.LedgerState hiding (ptrMap)
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
import Cardano.Ledger.UTxO
import Cardano.Ledger.Val (Val)
import Constrained hiding (Value)
import Constrained qualified as C
import Constrained.Base (Binder (..), HasGenHint (..), Pred (..), Term (..))
import Constrained.Spec.Map
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
import Test.Cardano.Ledger.Core.Utils
import Test.Cardano.Ledger.Shelley.Utils
import Test.Cardano.Ledger.TreeDiff (ToExpr)
import Test.Cardano.Slotting.Numeric ()
import Test.QuickCheck hiding (Args, Fun, forAll)

-- ==========================================================

type ConwayUnivFns = CoinFn : CoerceFn : StringFn : MapFn : FunFn : TreeFn : BaseFns
type ConwayFn = Fix (OneofL ConwayUnivFns)

type IsConwayUniv fn =
  ( BaseUniverse fn
  , Member (CoinFn fn) fn
  , Member (CoerceFn fn) fn
  , Member (StringFn fn) fn
  , Member (MapFn fn) fn
  , Member (FunFn fn) fn
  , Member (TreeFn fn) fn
  )

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
   , StrictMaybe AuxiliaryDataHash
   , StrictMaybe Network
   , VotingProcedures ConwayEra
   , SOS.OSet (ProposalProcedure ConwayEra)
   , StrictMaybe Coin
   , Coin
   ]
instance (EraSpecPParams ConwayEra, IsConwayUniv fn) => HasSpec fn (ConwayTxBody ConwayEra)

instance HasSimpleRep (ConwayTxBody ConwayEra) where
  type SimpleRep (ConwayTxBody ConwayEra) = SOP '["ConwayTxBody" ::: ConwayTxBodyTypes]
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
instance IsConwayUniv fn => HasSpec fn DeltaCoin
instance IsConwayUniv fn => OrdLike fn DeltaCoin
instance IsConwayUniv fn => NumLike fn DeltaCoin
instance IsConwayUniv fn => Foldy fn DeltaCoin where
  genList s s' = map fromSimpleRep <$> genList @fn @Integer (toSimpleRepSpec s) (toSimpleRepSpec s')
  theAddFn = addFn
  theZero = DeltaCoin 0

deriving via Integer instance Num DeltaCoin

instance HasSimpleRep (GovSignal era)
instance (EraTxCert ConwayEra, EraSpecPParams ConwayEra, IsConwayUniv fn) => HasSpec fn (GovSignal ConwayEra)

instance HasSimpleRep SlotNo
instance IsConwayUniv fn => OrdLike fn SlotNo
instance IsConwayUniv fn => HasSpec fn SlotNo

instance HasSimpleRep EpochNo
instance IsConwayUniv fn => OrdLike fn EpochNo
instance IsConwayUniv fn => HasSpec fn EpochNo
instance IsConwayUniv fn => NumLike fn EpochNo

instance HasSimpleRep TxIx
instance IsConwayUniv fn => HasSpec fn TxIx

instance (IsConwayUniv fn, Typeable index) => HasSpec fn (SafeHash index) where
  type TypeSpec fn (SafeHash index) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep TxId
instance IsConwayUniv fn => HasSpec fn TxId

instance HasSimpleRep TxIn
instance IsConwayUniv fn => HasSpec fn TxIn

instance HasSimpleRep (StrictSeq a) where
  type SimpleRep (StrictSeq a) = [a]
  toSimpleRep = toList
  fromSimpleRep = StrictSeq.fromList
instance (IsConwayUniv fn, HasSpec fn a) => HasSpec fn (StrictSeq a)

instance HasSimpleRep (Seq a) where
  type SimpleRep (Seq a) = [a]
  toSimpleRep = toList
  fromSimpleRep = Seq.fromList
instance (IsConwayUniv fn, HasSpec fn a) => HasSpec fn (Seq a)

instance HasSimpleRep (Sized a)
instance (IsConwayUniv fn, HasSpec fn a) => HasSpec fn (Sized a)

sizedValue_ :: (HasSpec fn (Sized a), HasSpec fn a) => Term fn (Sized a) -> Term fn a
sizedValue_ = sel @0

sizedSize_ :: (HasSpec fn (Sized a), HasSpec fn a) => Term fn (Sized a) -> Term fn Int64
sizedSize_ = sel @1

instance HasSimpleRep Addr28Extra
instance IsConwayUniv fn => HasSpec fn Addr28Extra

instance HasSimpleRep DataHash32
instance IsConwayUniv fn => HasSpec fn DataHash32

type ShelleyTxOutTypes era =
  '[ Addr
   , Value era
   ]
instance (Era era, Val (Value era)) => HasSimpleRep (ShelleyTxOut era) where
  -- type SimpleRep (ShelleyTxOut era) = SOP '["ShelleyTxOut" ::: ShelleyTxOutTypes era]
  type TheSop (ShelleyTxOut era) = '["ShelleyTxOut" ::: ShelleyTxOutTypes era]
  toSimpleRep (ShelleyTxOut addr val) =
    inject @"ShelleyTxOut" @'["ShelleyTxOut" ::: ShelleyTxOutTypes era]
      addr
      val
  fromSimpleRep rep =
    algebra @'["ShelleyTxOut" ::: ShelleyTxOutTypes era] rep ShelleyTxOut

instance (EraTxOut era, HasSpec fn (Value era), IsConwayUniv fn) => HasSpec fn (ShelleyTxOut era)

type AlonzoTxOutTypes era =
  '[ Addr
   , Value era
   , StrictMaybe DataHash
   ]
instance (Era era, Val (Value era)) => HasSimpleRep (AlonzoTxOut era) where
  -- type SimpleRep (AlonzoTxOut era) = SOP '["AlonzoTxOut" ::: AlonzoTxOutTypes era]
  type TheSop (AlonzoTxOut era) = '["AlonzoTxOut" ::: AlonzoTxOutTypes era]
  toSimpleRep (AlonzoTxOut addr val mdat) =
    inject @"AlonzoTxOut" @'["AlonzoTxOut" ::: AlonzoTxOutTypes era]
      addr
      val
      mdat
  fromSimpleRep rep =
    algebra @'["AlonzoTxOut" ::: AlonzoTxOutTypes era] rep AlonzoTxOut

instance (EraTxOut era, HasSpec fn (Value era), IsConwayUniv fn) => HasSpec fn (AlonzoTxOut era)

type BabbageTxOutTypes era =
  '[ Addr
   , Value era
   , Datum era
   , StrictMaybe (Script era)
   ]
instance (Era era, Val (Value era)) => HasSimpleRep (BabbageTxOut era) where
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
  ( IsConwayUniv fn
  , HasSpec fn (Value era)
  , Era era
  , HasSpec fn (Data era)
  , Val (Value era)
  , HasSpec fn (Script era)
  , IsNormalType (Script era)
  ) =>
  HasSpec fn (BabbageTxOut era)

txOutVal_ ::
  ( HasSpec fn (Value era)
  , Era era
  , HasSpec fn (Data era)
  , Val (Value era)
  , HasSpec fn (Script era)
  , IsConwayUniv fn
  , HasSpec fn (BabbageTxOut era)
  , IsNormalType (Script era)
  ) =>
  Term fn (BabbageTxOut era) ->
  Term fn (Value era)
txOutVal_ = sel @1

instance
  ( Compactible a
  , HasSimpleRep a
  , Typeable (SimpleRep a)
  , Show (SimpleRep a)
  ) =>
  HasSimpleRep (CompactForm a)
  where
  type SimpleRep (CompactForm a) = SimpleRep a
  toSimpleRep = toSimpleRep . fromCompact
  fromSimpleRep x = fromMaybe err . toCompact $ fromSimpleRep x
    where
      err = error $ "toCompact @" ++ show (typeOf x) ++ " " ++ show x
instance
  ( IsConwayUniv fn
  , Compactible a
  , HasSpec fn a
  , HasSimpleRep a
  , HasSpec fn (SimpleRep a)
  , Show (TypeSpec fn (SimpleRep a))
  ) =>
  HasSpec fn (CompactForm a)

instance HasSimpleRep MaryValue where
  type TheSop MaryValue = '["MaryValue" ::: '[Coin]]
  toSimpleRep (MaryValue c _) = c
  fromSimpleRep c = MaryValue c mempty
instance IsConwayUniv fn => HasSpec fn MaryValue

maryValueCoin_ :: IsConwayUniv fn => Term fn MaryValue -> Term fn Coin
maryValueCoin_ = sel @0

instance HasSimpleRep PV1.Data
instance IsConwayUniv fn => HasSpec fn PV1.Data where
  type TypeSpec fn PV1.Data = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance Era era => HasSimpleRep (Data era) where
  type SimpleRep (Data era) = PV1.Data
  toSimpleRep = getPlutusData
  fromSimpleRep = mkMemoized . PlutusData
instance (IsConwayUniv fn, Era era) => HasSpec fn (Data era)

instance Era era => HasSimpleRep (BinaryData era) where
  type SimpleRep (BinaryData era) = Data era
  toSimpleRep = binaryDataToData
  fromSimpleRep = dataToBinaryData
instance
  (IsConwayUniv fn, Era era, HasSpec fn (Data era)) =>
  HasSpec fn (BinaryData era)

instance HasSimpleRep (Datum era)
instance (IsConwayUniv fn, Era era, HasSpec fn (Data era)) => HasSpec fn (Datum era)

-- TODO: here we are cheating to get out of having to deal with Plutus scripts
instance HasSimpleRep (AlonzoScript era) where
  type SimpleRep (AlonzoScript era) = Timelock era
  toSimpleRep (TimelockScript tl) = tl
  toSimpleRep (PlutusScript _) = error "toSimpleRep for AlonzoScript on a PlutusScript"
  fromSimpleRep = TimelockScript
instance
  ( IsConwayUniv fn
  , AlonzoEraScript era
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  HasSpec fn (AlonzoScript era)

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
to push any constraint you have into functions `MyConstraint :: MyUniv fn '[Timelock era] Bool`
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
  ( IsConwayUniv fn
  , AllegraEraScript era
  , NativeScript era ~ Timelock era
  ) =>
  HasSpec fn (Timelock era)
  where
  type TypeSpec fn (Timelock era) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep CompactAddr where
  type SimpleRep CompactAddr = SimpleRep Addr
  toSimpleRep = toSimpleRep . decompactAddr
  fromSimpleRep = compactAddr . fromSimpleRep
instance IsConwayUniv fn => HasSpec fn CompactAddr

instance HasSimpleRep Addr
instance IsConwayUniv fn => HasSpec fn Addr

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
instance IsConwayUniv fn => HasSpec fn BootstrapAddress

instance HasSimpleRep NetworkMagic
instance IsConwayUniv fn => HasSpec fn NetworkMagic

instance HasSimpleRep AddrType
instance IsConwayUniv fn => HasSpec fn AddrType

instance (IsConwayUniv fn, Typeable b) => HasSpec fn (AbstractHash Blake2b_224 b) where
  type TypeSpec fn (AbstractHash Blake2b_224 b) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = do
    bytes <- pureGen $ vectorOf 28 arbitrary
    pure $ fromJust $ abstractHashFromBytes (BS.pack bytes)
  shrinkWithTypeSpec _ _ = []
  cardinalTypeSpec _ = TrueSpec
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep StakeReference
instance IsConwayUniv fn => HasSpec fn StakeReference

instance HasSimpleRep Ptr
instance IsConwayUniv fn => HasSpec fn Ptr

instance HasSimpleRep CertIx where
  type SimpleRep CertIx = Word16
  toSimpleRep (CertIx w) = fromIntegral w
  fromSimpleRep = mkCertIx
instance IsConwayUniv fn => HasSpec fn CertIx

instance HasSimpleRep (Credential r)
instance (IsConwayUniv fn, Typeable r) => HasSpec fn (Credential r)

cKeyHashObj ::
  (IsConwayUniv fn, Typeable r) => Term fn (KeyHash r) -> Term fn (Credential r)
cKeyHashObj = con @"KeyHashObj"

cScriptHashObj ::
  (IsConwayUniv fn, Typeable r) => Term fn ScriptHash -> Term fn (Credential r)
cScriptHashObj = con @"ScriptHashObj"

instance HasSimpleRep ScriptHash
instance IsConwayUniv fn => HasSpec fn ScriptHash

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

instance (IsConwayUniv fn, Typeable r) => HasSpec fn (VRFVerKeyHash r) where
  type TypeSpec fn (VRFVerKeyHash r) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen $ VRFVerKeyHash <$> genHashWithDuplicates
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance (IsConwayUniv fn, HashAlgorithm a, Typeable b) => HasSpec fn (Hash a b) where
  type TypeSpec fn (Hash a b) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen genHashWithDuplicates
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep (ConwayTxCert era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (ConwayTxCert era)

instance HasSimpleRep ConwayDelegCert
instance IsConwayUniv fn => HasSpec fn ConwayDelegCert

instance HasSimpleRep PoolCert
instance IsConwayUniv fn => HasSpec fn PoolCert

instance HasSimpleRep PoolParams
instance IsConwayUniv fn => HasSpec fn PoolParams

instance HasSimpleRep PoolMetadata
instance IsConwayUniv fn => HasSpec fn PoolMetadata

instance IsConwayUniv fn => HasSpec fn StakePoolRelay where
  type TypeSpec fn StakePoolRelay = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep Port
instance IsConwayUniv fn => HasSpec fn Port

instance HasSimpleRep ConwayGovCert
instance IsConwayUniv fn => HasSpec fn ConwayGovCert

instance HasSimpleRep Anchor
instance IsConwayUniv fn => HasSpec fn Anchor

instance HasSimpleRep Url
instance IsConwayUniv fn => HasSpec fn Url where
  type TypeSpec fn Url = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance IsConwayUniv fn => HasSpec fn Text where
  type TypeSpec fn Text = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

newtype StringSpec fn = StringSpec {strSpecLen :: Specification fn Int}

deriving instance IsConwayUniv fn => Show (StringSpec fn)

instance HasSpec fn Int => Semigroup (StringSpec fn) where
  StringSpec len <> StringSpec len' = StringSpec (len <> len')

instance HasSpec fn Int => Monoid (StringSpec fn) where
  mempty = StringSpec TrueSpec

instance IsConwayUniv fn => HasSpec fn ByteString where
  type TypeSpec fn ByteString = StringSpec fn
  emptySpec = mempty
  combineSpec s s' = typeSpec $ s <> s'
  genFromTypeSpec (StringSpec ls) = do
    len <- genFromSpecT ls
    BS.pack <$> vectorOfT len (pureGen arbitrary)
  shrinkWithTypeSpec _ = shrink
  cardinalTypeSpec _ = TrueSpec
  conformsTo bs (StringSpec ls) = BS.length bs `conformsToSpec` ls
  toPreds str (StringSpec len) = satisfies (strLen_ str) len

instance IsConwayUniv fn => HasSpec fn ShortByteString where
  type TypeSpec fn ShortByteString = StringSpec fn
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

data StringFn (fn :: [Type] -> Type -> Type) as b where
  LengthFn :: StringLike s => StringFn fn '[s] Int

deriving instance IsConwayUniv fn => Show (StringFn fn as b)
deriving instance IsConwayUniv fn => Eq (StringFn fn as b)

strLen_ ::
  forall fn s.
  (Member (StringFn fn) fn, StringLike s, HasSpec fn s) =>
  Term fn s ->
  Term fn Int
strLen_ = app (injectFn $ LengthFn @_ @fn)

instance FunctionLike (StringFn fn) where
  sem LengthFn = getLength

instance IsConwayUniv fn => Functions (StringFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn ctx spec = case fn of
    _
      | SuspendedSpec {} <- spec
      , ListCtx pre HOLE suf <- ctx ->
          constrained $ \x' ->
            let args =
                  appendList
                    (mapList (\(C.Value a) -> lit a) pre)
                    (x' :> mapList (\(C.Value a) -> lit a) suf)
             in uncurryList (app @fn $ injectFn fn) args `satisfies` spec
    LengthFn ->
      -- No TypeAbstractions in ghc-8.10
      case fn of
        (_ :: StringFn fn '[s] Int)
          | NilCtx HOLE <- ctx -> typeSpec $ lengthSpec @s spec

  mapTypeSpec f@LengthFn ss =
    -- No TypeAbstractions in ghc-8.10
    case f of
      (_ :: StringFn fn '[s] Int) -> getLengthSpec @s ss

class StringLike s where
  lengthSpec :: IsConwayUniv fn => Specification fn Int -> TypeSpec fn s
  getLengthSpec :: TypeSpec fn s -> Specification fn Int
  getLength :: s -> Int

instance HasSimpleRep Delegatee
instance IsConwayUniv fn => HasSpec fn Delegatee

instance HasSimpleRep DRep
instance IsConwayUniv fn => HasSpec fn DRep

instance HasSimpleRep Withdrawals
instance IsConwayUniv fn => HasSpec fn Withdrawals

instance HasSimpleRep RewardAccount
instance IsConwayUniv fn => HasSpec fn RewardAccount

instance HasSimpleRep Network
instance IsConwayUniv fn => HasSpec fn Network

instance HasSimpleRep MultiAsset
instance IsConwayUniv fn => HasSpec fn MultiAsset where
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
instance IsConwayUniv fn => HasSpec fn AssetName

instance HasSimpleRep PolicyID
instance IsConwayUniv fn => HasSpec fn PolicyID

instance HasSimpleRep AuxiliaryDataHash
instance IsConwayUniv fn => HasSpec fn AuxiliaryDataHash

instance HasSimpleRep (VotingProcedures era)
instance (IsConwayUniv fn, Typeable era) => HasSpec fn (VotingProcedures era)

instance HasSimpleRep (VotingProcedure era)
instance (IsConwayUniv fn, Typeable era) => HasSpec fn (VotingProcedure era)

instance HasSimpleRep Vote
instance IsConwayUniv fn => HasSpec fn Vote

instance HasSimpleRep GovActionId
instance IsConwayUniv fn => HasSpec fn GovActionId where
  shrinkWithTypeSpec _ _ = []

instance HasSimpleRep GovActionIx
instance IsConwayUniv fn => HasSpec fn GovActionIx

instance HasSimpleRep (GovPurposeId p era)
instance (Typeable p, IsConwayUniv fn, Era era) => HasSpec fn (GovPurposeId p era)

instance HasSimpleRep (GovAction era)
instance (IsConwayUniv fn, EraSpecPParams era) => HasSpec fn (GovAction era)

instance HasSimpleRep (Constitution era)
instance (IsConwayUniv fn, EraPParams era) => HasSpec fn (Constitution era)

instance HasSimpleRep (ConwayPParams StrictMaybe c)
instance
  ( IsConwayUniv fn
  , Typeable c
  ) =>
  HasSpec fn (ConwayPParams StrictMaybe c)

instance HasSimpleRep (ConwayPParams Identity era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (ConwayPParams Identity era)

instance HasSimpleRep CoinPerByte where
  -- TODO: consider `SimpleRep Coin` instead if this is annoying
  type SimpleRep CoinPerByte = Coin
  fromSimpleRep = CoinPerByte
  toSimpleRep = unCoinPerByte
instance IsConwayUniv fn => HasSpec fn CoinPerByte

instance IsConwayUniv fn => HasSpec fn Char where
  type TypeSpec fn Char = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance IsConwayUniv fn => HasSpec fn CostModel where
  type TypeSpec fn CostModel = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep Language
instance IsConwayUniv fn => HasSpec fn Language

instance HasSimpleRep (NoUpdate a)
instance (IsConwayUniv fn, Typeable a) => HasSpec fn (NoUpdate a)

instance HasSimpleRep (THKD tag StrictMaybe a) where
  type SimpleRep (THKD tag StrictMaybe a) = SOP (TheSop (StrictMaybe a))
  fromSimpleRep = THKD . fromSimpleRep
  toSimpleRep (THKD sm) = toSimpleRep sm
instance (IsConwayUniv fn, IsNormalType a, Typeable tag, HasSpec fn a) => HasSpec fn (THKD tag StrictMaybe a)

instance HasSimpleRep (THKD tag Identity a) where
  type SimpleRep (THKD tag Identity a) = a
  fromSimpleRep = THKD
  toSimpleRep (THKD a) = a
instance (IsConwayUniv fn, IsNormalType a, Typeable tag, HasSpec fn a) => HasSpec fn (THKD tag Identity a)

instance HasSimpleRep GovActionPurpose
instance IsConwayUniv fn => HasSpec fn GovActionPurpose

instance HasSimpleRep Voter
instance IsConwayUniv fn => HasSpec fn Voter

-- TODO: this might be a problem considering duplicates in the list! This
-- type might require having its own `HasSpec` at some point
instance Ord a => HasSimpleRep (SOS.OSet a) where
  type SimpleRep (SOS.OSet a) = [a]
  fromSimpleRep = SOS.fromStrictSeq . StrictSeq.fromList
  toSimpleRep = toList . SOS.toStrictSeq
instance (IsConwayUniv fn, Ord a, HasSpec fn a) => HasSpec fn (SOS.OSet a)
instance Ord a => Forallable (SOS.OSet a) a

instance HasSimpleRep (ProposalProcedure era)
instance
  (IsConwayUniv fn, EraSpecPParams era) =>
  HasSpec fn (ProposalProcedure era)

pProcDeposit_ ::
  (EraSpecPParams ConwayEra, IsConwayUniv fn) =>
  Term fn (ProposalProcedure ConwayEra) ->
  Term fn Coin
pProcDeposit_ = sel @0

pProcGovAction_ ::
  (EraSpecPParams ConwayEra, IsConwayUniv fn) =>
  Term fn (ProposalProcedure ConwayEra) ->
  Term fn (GovAction ConwayEra)
pProcGovAction_ = sel @2

instance HasSimpleRep ValidityInterval
instance IsConwayUniv fn => HasSpec fn ValidityInterval

instance HasSimpleRep DRepState
instance IsConwayUniv fn => HasSpec fn DRepState

instance HasSimpleRep CommitteeAuthorization
instance IsConwayUniv fn => HasSpec fn CommitteeAuthorization

instance HasSimpleRep (CommitteeState era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (CommitteeState era)

instance HasSimpleRep (VState era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (VState era)

instance HasSimpleRep (PState era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (PState era)

instance HasSimpleRep (DState era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (DState era)

instance HasSimpleRep FutureGenDeleg
instance IsConwayUniv fn => HasSpec fn FutureGenDeleg

instance HasSimpleRep GenDelegPair
instance IsConwayUniv fn => HasSpec fn GenDelegPair

instance HasSimpleRep GenDelegs
instance IsConwayUniv fn => HasSpec fn GenDelegs

instance HasSimpleRep InstantaneousRewards
instance IsConwayUniv fn => HasSpec fn InstantaneousRewards

type UMapTypes =
  '[ Map (Credential 'Staking) RDPair
   , Map Ptr (Credential 'Staking)
   , Map (Credential 'Staking) (KeyHash 'StakePool)
   , Map (Credential 'Staking) DRep
   ]
instance HasSimpleRep UMap where
  type SimpleRep UMap = SOP '["UMap" ::: UMapTypes]
  toSimpleRep um = inject @"UMap" @'["UMap" ::: UMapTypes] (rdPairMap um) (ptrMap um) (sPoolMap um) (dRepMap um)
  fromSimpleRep rep = algebra @'["UMap" ::: UMapTypes] rep unify
instance IsConwayUniv fn => HasSpec fn UMap

instance HasSimpleRep RDPair where
  type SimpleRep RDPair = SOP '["RDPair" ::: '[SimpleRep Coin, SimpleRep Coin]]
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
instance IsConwayUniv fn => HasSpec fn RDPair

instance HasSimpleRep (CertState era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (CertState era)

instance HasSimpleRep (GovRelation StrictMaybe era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (GovRelation StrictMaybe era)

instance Era era => HasSimpleRep (GovEnv era)
instance (EraSpecPParams era, IsConwayUniv fn) => HasSpec fn (GovEnv era)

instance HasSimpleRep (GovActionState era)
instance (IsConwayUniv fn, Era era, EraSpecPParams era) => HasSpec fn (GovActionState era)

gasId_ ::
  (EraSpecPParams ConwayEra, IsConwayUniv fn) =>
  Term fn (GovActionState ConwayEra) ->
  Term fn (GovActionId)
gasId_ = sel @0

gasCommitteeVotes_ ::
  (EraSpecPParams ConwayEra, IsConwayUniv fn) =>
  Term fn (GovActionState ConwayEra) ->
  Term fn (Map (Credential 'HotCommitteeRole) Vote)
gasCommitteeVotes_ = sel @1

gasDRepVotes_ ::
  (EraSpecPParams ConwayEra, IsConwayUniv fn) =>
  Term fn (GovActionState ConwayEra) ->
  Term fn (Map (Credential 'DRepRole) Vote)
gasDRepVotes_ = sel @2

gasProposalProcedure_ ::
  (EraSpecPParams ConwayEra, IsConwayUniv fn) =>
  Term fn (GovActionState ConwayEra) ->
  Term fn (ProposalProcedure ConwayEra)
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
type ProposalTree era = (StrictMaybe (GovActionId), [Tree (GAS era)])
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
  type SimpleRep (Proposals era) = SOP '["Proposals" ::: ProposalsType era]
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

      buildProposalTree :: TreeMaybe (GovActionId) -> ProposalTree era
      buildProposalTree (TreeMaybe (Node mId cs)) = (mId, map buildTree cs)

      buildTree :: Tree (StrictMaybe (GovActionId)) -> Tree (GAS era)
      buildTree (Node (SJust gid) cs) | Just gas <- Map.lookup gid idMap = Node gas (map buildTree cs)
      buildTree _ =
        error "toSimpleRep @Proposals: toGovRelationTree returned trees with Nothing nodes below the root"

      keys :: TreeMaybe (GovActionId) -> Set (GovActionId)
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

instance (EraSpecPParams era, IsConwayUniv fn) => HasSpec fn (Proposals era)

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
  ( HasSpec fn (SimpleRep (Proposals era))
  , HasSpec fn (Proposals era)
  , HasSimpleRep (Proposals era)
  , IsConwayUniv fn
  , era ~ ConwayEra
  , EraSpecPParams ConwayEra
  ) =>
  HasGenHint fn (Proposals era)
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
instance (IsConwayUniv fn, EraSpecPParams ConwayEra) => HasSpec fn (EnactSignal ConwayEra)

instance HasSimpleRep (EnactState era)
instance (EraSpecPParams era, IsConwayUniv fn) => HasSpec fn (EnactState era)

instance HasSimpleRep (Committee era)
instance (Era era, IsConwayUniv fn) => HasSpec fn (Committee era)

instance HasSimpleRep (RatifyEnv era)
instance (Era era, IsConwayUniv fn) => HasSpec fn (RatifyEnv era)

instance HasSimpleRep (RatifyState ConwayEra)
instance (EraSpecPParams ConwayEra, IsConwayUniv fn) => HasSpec fn (RatifyState ConwayEra)

instance HasSimpleRep (RatifySignal ConwayEra)
instance (EraSpecPParams ConwayEra, IsConwayUniv fn) => HasSpec fn (RatifySignal ConwayEra)

instance HasSimpleRep PoolDistr
instance IsConwayUniv fn => HasSpec fn PoolDistr

instance HasSimpleRep IndividualPoolStake
instance IsConwayUniv fn => HasSpec fn IndividualPoolStake

instance HasSimpleRep (ConwayGovCertEnv ConwayEra)
instance (EraSpecPParams ConwayEra, IsConwayUniv fn) => HasSpec fn (ConwayGovCertEnv ConwayEra)

instance HasSimpleRep (PoolEnv era)
instance (EraSpecPParams era, IsConwayUniv fn) => HasSpec fn (PoolEnv era)

instance Era era => HasSimpleRep (CertEnv era)
instance (EraSpecPParams era, IsConwayUniv fn) => HasSpec fn (CertEnv era)

instance HasSimpleRep NonMyopic
instance IsConwayUniv fn => HasSpec fn NonMyopic

instance HasSimpleRep Likelihood
instance IsConwayUniv fn => HasSpec fn Likelihood

instance HasSimpleRep LogWeight
instance IsConwayUniv fn => HasSpec fn LogWeight

instance HasSimpleRep AccountState
instance IsConwayUniv fn => HasSpec fn AccountState

instance HasSimpleRep SnapShot
instance IsConwayUniv fn => HasSpec fn SnapShot

instance HasSimpleRep Stake
instance IsConwayUniv fn => HasSpec fn Stake

instance (VMap.Vector vk k, VMap.Vector vv v) => HasSimpleRep (VMap vk vv k v) where
  type SimpleRep (VMap vk vv k v) = Map k v
  toSimpleRep = VMap.toMap
  fromSimpleRep = VMap.fromMap
instance
  ( IsConwayUniv fn
  , VMap.Vector vk k
  , VMap.Vector vv v
  , Typeable vk
  , Typeable vv
  , Ord k
  , Eq (vv v)
  , Eq (vk k)
  , HasSpec fn k
  , HasSpec fn v
  ) =>
  HasSpec fn (VMap vk vv k v)

instance HasSimpleRep SnapShots
instance IsConwayUniv fn => HasSpec fn SnapShots

instance EraTxOut era => HasSimpleRep (LedgerState era)
instance
  ( EraTxOut era
  , IsConwayUniv fn
  , HasSpec fn (TxOut era)
  , IsNormalType (TxOut era)
  , HasSpec fn (GovState era)
  ) =>
  HasSpec fn (LedgerState era)

instance HasSimpleRep (UTxOState era)
instance
  ( EraTxOut era
  , HasSpec fn (TxOut era)
  , IsNormalType (TxOut era)
  , HasSpec fn (GovState era)
  , IsConwayUniv fn
  ) =>
  HasSpec fn (UTxOState era)

instance HasSimpleRep IncrementalStake
instance IsConwayUniv fn => HasSpec fn IncrementalStake

instance HasSimpleRep (UTxO era)
instance
  (Era era, HasSpec fn (TxOut era), IsNormalType (TxOut era), IsConwayUniv fn) =>
  HasSpec fn (UTxO era)

instance HasSimpleRep (ConwayGovState ConwayEra)
instance (EraSpecPParams ConwayEra, IsConwayUniv fn) => HasSpec fn (ConwayGovState ConwayEra)

instance HasSimpleRep (DRepPulsingState ConwayEra)
instance (EraSpecPParams ConwayEra, IsConwayUniv fn) => HasSpec fn (DRepPulsingState ConwayEra)

instance HasSimpleRep (PulsingSnapshot ConwayEra)
instance (EraSpecPParams ConwayEra, IsConwayUniv fn) => HasSpec fn (PulsingSnapshot ConwayEra)

type DRepPulserTypes =
  '[ Int
   , UMap
   , Int
   , Map (Credential 'Staking) (CompactForm Coin)
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
    SimpleRep (DRepPulser ConwayEra Identity (RatifyState ConwayEra)) =
      SOP '["DRepPulser" ::: DRepPulserTypes]
  toSimpleRep DRepPulser {..} =
    inject @"DRepPulser" @'["DRepPulser" ::: DRepPulserTypes]
      dpPulseSize
      dpUMap
      dpIndex
      dpStakeDistr
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
instance
  (EraSpecPParams ConwayEra, IsConwayUniv fn) =>
  HasSpec fn (DRepPulser ConwayEra Identity (RatifyState ConwayEra))

instance Era era => HasSimpleRep (UtxoEnv era)
instance (EraSpecPParams era, IsConwayUniv fn) => HasSpec fn (UtxoEnv era)

-- ================================================================
-- All the Tx instances

-- Unlike ShelleyTx, AlonzoTx is just a data type, and the generic instances work fine

instance Era era => HasSimpleRep (AlonzoTx era)
instance
  ( EraSpecPParams era
  , IsConwayUniv fn
  , HasSpec fn (TxBody era)
  , HasSpec fn (TxWits era)
  , HasSpec fn (TxAuxData era)
  , IsNormalType (TxAuxData era)
  ) =>
  HasSpec fn (AlonzoTx era)

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
  ( EraSpecPParams era
  , IsConwayUniv fn
  , HasSpec fn (TxBody era)
  , HasSpec fn (TxWits era)
  , HasSpec fn (TxAuxData era)
  , IsNormalType (TxAuxData era)
  ) =>
  HasSpec fn (ShelleyTx era)

instance EraSpecPParams era => HasSimpleRep (ShelleyTx era) where
  type SimpleRep (ShelleyTx era) = SOP '["ShelleyTx" ::: ShelleyTxTypes era]
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
instance IsConwayUniv fn => HasSpec fn IsValid

-- ===============================================================
-- All the TxAuxData instances

-- NOTE: we don't generate or talk about plutus scripts (yet!)
type AlonzoTxAuxDataTypes era =
  '[ Map Word64 Metadatum
   , StrictSeq (Timelock era)
   ]
instance AlonzoEraScript era => HasSimpleRep (AlonzoTxAuxData era) where
  type
    SimpleRep (AlonzoTxAuxData era) =
      SOP '["AlonzoTxOutData" ::: AlonzoTxAuxDataTypes era]
  toSimpleRep (AlonzoTxAuxData metaMap tsSeq _) =
    inject @"AlonzoTxAuxData" @'["AlonzoTxAuxData" ::: AlonzoTxAuxDataTypes era]
      metaMap
      tsSeq
  fromSimpleRep rep =
    algebra @'["AlonzoTxAuxData" ::: AlonzoTxAuxDataTypes era] rep $
      \metaMap tsSeq -> AlonzoTxAuxData metaMap tsSeq mempty
instance
  ( Era era
  , IsConwayUniv fn
  , AlonzoEraScript era
  , NativeScript era ~ Timelock era
  ) =>
  HasSpec fn (AlonzoTxAuxData era)

-- NOTE: we don't generate or talk about plutus scripts (yet!)
type AllegraTxAuxDataTypes era =
  '[ Map Word64 Metadatum
   , StrictSeq (Timelock era)
   ]
instance Era era => HasSimpleRep (AllegraTxAuxData era) where
  type
    SimpleRep (AllegraTxAuxData era) =
      SOP '["AllegraTxOutData" ::: AllegraTxAuxDataTypes era]
  toSimpleRep (AllegraTxAuxData metaMap tsSeq) =
    inject @"AllegraTxAuxData" @'["AllegraTxAuxData" ::: AllegraTxAuxDataTypes era]
      metaMap
      tsSeq
  fromSimpleRep rep =
    algebra @'["AllegraTxAuxData" ::: AllegraTxAuxDataTypes era] rep $
      \metaMap tsSeq -> AllegraTxAuxData metaMap tsSeq

instance
  ( Era era
  , IsConwayUniv fn
  , AllegraEraScript era
  , NativeScript era ~ Timelock era
  ) =>
  HasSpec fn (AllegraTxAuxData era)

type ShelleyTxAuxDataTypes era =
  '[ Map Word64 Metadatum
   ]
instance Era era => HasSimpleRep (ShelleyTxAuxData era) where
  type
    SimpleRep (ShelleyTxAuxData era) =
      SOP '["ShelleyTxAuxData" ::: ShelleyTxAuxDataTypes era]
  toSimpleRep (ShelleyTxAuxData metaMap) =
    inject @"ShelleyTxAuxData" @'["ShelleyTxAuxData" ::: ShelleyTxAuxDataTypes era]
      metaMap
  fromSimpleRep rep =
    algebra @'["ShelleyTxAuxData" ::: ShelleyTxAuxDataTypes era] rep $
      \metaMap -> ShelleyTxAuxData metaMap

instance
  ( Era era
  , IsConwayUniv fn
  , AllegraEraScript era
  , NativeScript era ~ Timelock era
  ) =>
  HasSpec fn (ShelleyTxAuxData era)

instance HasSimpleRep Metadatum
instance IsConwayUniv fn => HasSpec fn Metadatum

-- ===============================================================
-- All the TxWits instances

type AlonzoTxWitsTypes =
  '[ Set (WitVKey 'Witness)
   , Set BootstrapWitness
   ]
instance AlonzoEraScript era => HasSimpleRep (AlonzoTxWits era) where
  type
    SimpleRep (AlonzoTxWits era) =
      SOP '["AlonzoTxWits" ::: AlonzoTxWitsTypes]
  toSimpleRep (AlonzoTxWits vkeyWits bootstrapWits _ _ _) =
    inject @"AlonzoTxWits" @'["AlonzoTxWits" ::: AlonzoTxWitsTypes]
      vkeyWits
      bootstrapWits
  fromSimpleRep rep =
    algebra @'["AlonzoTxWits" ::: AlonzoTxWitsTypes] rep $
      \vkeyWits bootstrapWits -> AlonzoTxWits vkeyWits bootstrapWits mempty (TxDats mempty) (Redeemers mempty)
instance (AlonzoEraScript era, IsConwayUniv fn) => HasSpec fn (AlonzoTxWits era)

type ShelleyTxWitsTypes era =
  '[ Set (WitVKey 'Witness)
   , Set BootstrapWitness
   ]
instance EraScript era => HasSimpleRep (ShelleyTxWits era) where
  type
    SimpleRep (ShelleyTxWits era) =
      SOP '["ShelleyTxWits" ::: ShelleyTxWitsTypes era]
  toSimpleRep (ShelleyTxWits vkeyWits _ bootstrapWits) =
    inject @"ShelleyTxWits" @'["ShelleyTxWits" ::: ShelleyTxWitsTypes era]
      vkeyWits
      bootstrapWits
  fromSimpleRep rep =
    algebra @'["ShelleyTxWits" ::: ShelleyTxWitsTypes era] rep $
      \vkeyWits bootstrapWits -> ShelleyTxWits vkeyWits mempty bootstrapWits
instance (EraScript era, IsConwayUniv fn) => HasSpec fn (ShelleyTxWits era)

instance (IsConwayUniv fn, Typeable r) => HasSpec fn (WitVKey r) where
  type TypeSpec fn (WitVKey r) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance IsConwayUniv fn => HasSpec fn BootstrapWitness where
  type TypeSpec fn BootstrapWitness = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  cardinalTypeSpec _ = TrueSpec
  shrinkWithTypeSpec _ = shrink
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance Era era => HasSimpleRep (LedgerEnv era)
instance (IsConwayUniv fn, HasSpec fn (PParams era), Era era) => HasSpec fn (LedgerEnv era)

onJust' ::
  ( HasSpec fn a
  , IsNormalType a
  , IsPred p fn
  ) =>
  Term fn (StrictMaybe a) ->
  (Term fn a -> p) ->
  Pred fn
onJust' tm p = caseOn tm (branch $ const True) (branch p)

onSized ::
  (IsConwayUniv fn, HasSpec fn a, IsPred p fn) =>
  Term fn (Sized a) ->
  (Term fn a -> p) ->
  Pred fn
onSized sz p = match sz $ \a _ -> p a

instance HasSimpleRep (ConwayDelegEnv era)
instance (IsConwayUniv fn, HasSpec fn (PParams era), Era era) => HasSpec fn (ConwayDelegEnv era)

instance Era era => HasSimpleRep (EpochState era)
instance
  ( EraTxOut era
  , IsConwayUniv fn
  , HasSpec fn (TxOut era)
  , IsNormalType (TxOut era)
  , HasSpec fn (GovState era)
  ) =>
  HasSpec fn (EpochState era)

instance HasSimpleRep FreeVars
instance IsConwayUniv fn => HasSpec fn FreeVars

instance HasSimpleRep (PoolRewardInfo)
instance IsConwayUniv fn => HasSpec fn (PoolRewardInfo)

instance HasSimpleRep (LeaderOnlyReward)
instance IsConwayUniv fn => HasSpec fn (LeaderOnlyReward)

instance HasSimpleRep StakeShare
instance IsConwayUniv fn => HasSpec fn StakeShare

instance HasSimpleRep BlocksMade
instance IsConwayUniv fn => HasSpec fn BlocksMade

instance HasSimpleRep RewardType
instance IsConwayUniv fn => HasSpec fn RewardType

instance HasSimpleRep RewardAns
instance IsConwayUniv fn => HasSpec fn RewardAns

instance HasSimpleRep PulsingRewUpdate where
  type SimpleRep PulsingRewUpdate = SimpleRep RewardUpdate
  toSimpleRep (Complete x) = toSimpleRep x
  toSimpleRep x@(Pulsing _ _) = toSimpleRep (runShelleyBase (fst <$> (completeRupd x)))
  fromSimpleRep x = Complete (fromSimpleRep x)
instance IsConwayUniv fn => HasSpec fn PulsingRewUpdate

instance Era era => HasSimpleRep (NewEpochState era)
instance
  ( EraTxOut era
  , IsConwayUniv fn
  , HasSpec fn (TxOut era)
  , IsNormalType (TxOut era)
  , HasSpec fn (GovState era)
  , HasSpec fn (StashedAVVMAddresses era)
  ) =>
  HasSpec fn (NewEpochState era)

instance HasSimpleRep Reward
instance IsConwayUniv fn => HasSpec fn Reward

instance HasSimpleRep (RewardSnapShot)
instance IsConwayUniv fn => HasSpec fn (RewardSnapShot)

instance HasSimpleRep RewardUpdate
instance IsConwayUniv fn => HasSpec fn RewardUpdate

type PulserTypes =
  '[ Int
   , FreeVars
   , VMap VMap.VB VMap.VP (Credential 'Staking) (CompactForm Coin)
   , RewardAns
   ]
instance HasSimpleRep Pulser where
  type SimpleRep Pulser = SOP '["Pulser" ::: PulserTypes]
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

instance IsConwayUniv fn => HasSpec fn (Pulser)

instance HasSimpleRep (CertsEnv era)
instance (IsConwayUniv fn, EraSpecPParams era, HasSpec fn (Tx era)) => HasSpec fn (CertsEnv era)

-- CompactForm

class Coercible a b => CoercibleLike a b where
  coerceSpec ::
    IsConwayUniv fn =>
    Specification fn b ->
    Specification fn a
  getCoerceSpec ::
    IsConwayUniv fn =>
    TypeSpec fn a ->
    Specification fn b

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
    forall (fn :: [Type] -> Type -> Type).
    IsConwayUniv fn =>
    TypeSpec fn (CompactForm Coin) ->
    Specification fn Word64
  getCoerceSpec (NumSpecInterval a b) = TypeSpec @fn (NumSpecInterval a b) mempty

data CoerceFn (fn :: [Type] -> Type -> Type) args res where
  Coerce :: (CoercibleLike a b, Coercible a b) => CoerceFn fn '[a] b

deriving instance Show (CoerceFn fn args res)
deriving instance Eq (CoerceFn fn args res)

instance FunctionLike (CoerceFn fn) where
  sem = \case
    Coerce -> coerce

instance IsConwayUniv fn => Functions (CoerceFn fn) fn where
  propagateSpecFun _ _ (ErrorSpec e) = ErrorSpec e
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun fn ctx spec =
    case fn of
      _
        | SuspendedSpec {} <- spec
        , ListCtx pre HOLE suf <- ctx ->
            constrained $ \x' ->
              let args =
                    appendList
                      (mapList (\(C.Value a) -> lit a) pre)
                      (x' :> mapList (\(C.Value a) -> lit a) suf)
               in uncurryList (app @fn $ injectFn fn) args `satisfies` spec
      Coerce ->
        case fn of
          (_ :: CoerceFn fn '[a] b)
            | NilCtx HOLE <- ctx -> coerceSpec @a @b spec
  mapTypeSpec fn ss =
    case fn of
      Coerce ->
        case fn of
          (_ :: CoerceFn fn '[a] b) -> getCoerceSpec @a ss

coerce_ ::
  forall a b fn.
  ( Member (CoerceFn fn) fn
  , HasSpec fn a
  , HasSpec fn b
  , CoercibleLike a b
  ) =>
  Term fn a ->
  Term fn b
coerce_ = app (injectFn $ Coerce @a @b @fn)

-- ==============================================================

data CoinFn (fn :: [Type] -> Type -> Type) args res where
  ToDelta :: CoinFn fn '[Coin] DeltaCoin

deriving instance Show (CoinFn fn args res)
deriving instance Eq (CoinFn fn args res)

instance FunctionLike (CoinFn fn) where
  sem = \case
    ToDelta -> DeltaCoin . unCoin

toDeltaFn :: forall fn. Member (CoinFn fn) fn => fn '[Coin] DeltaCoin
toDeltaFn = injectFn $ ToDelta @fn

toDelta_ ::
  (HasSpec fn Coin, HasSpec fn DeltaCoin, Member (CoinFn fn) fn) =>
  Term fn Coin ->
  Term fn DeltaCoin
toDelta_ = app toDeltaFn

instance (Typeable fn, Member (CoinFn fn) fn) => Functions (CoinFn fn) fn where
  propagateSpecFun fn ctx (ExplainSpec [] s) = propagateSpecFun fn ctx s
  propagateSpecFun fn ctx (ExplainSpec es s) = ExplainSpec es $ propagateSpecFun fn ctx s
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn (ListCtx pre HOLE suf) (SuspendedSpec x p) =
    constrained $ \x' ->
      let args =
            appendList
              (mapList (\(C.Value a) -> Lit a) pre)
              (x' :> mapList (\(C.Value a) -> Lit a) suf)
       in Let (App (injectFn fn) args) (x :-> p)
  propagateSpecFun ToDelta (NilCtx HOLE) (MemberSpec xs) = MemberSpec (NE.map deltaToCoin xs)
  propagateSpecFun ToDelta (NilCtx HOLE) (TypeSpec (NumSpecInterval l h) cant) =
    ( TypeSpec
        (NumSpecInterval (fromIntegral <$> l) (fromIntegral <$> h))
        (map deltaToCoin cant)
    )

  mapTypeSpec ToDelta (NumSpecInterval l h) = typeSpec (NumSpecInterval (fromIntegral <$> l) (fromIntegral <$> h))

deltaToCoin :: DeltaCoin -> Coin
deltaToCoin (DeltaCoin i) = Coin i

instance HasSimpleRep (ShelleyGovState era)
instance (IsConwayUniv fn, EraSpecPParams era) => HasSpec fn (ShelleyGovState era)

instance HasSimpleRep ShelleyDelegCert
instance IsConwayUniv fn => HasSpec fn ShelleyDelegCert

instance HasSimpleRep MIRCert
instance IsConwayUniv fn => HasSpec fn MIRCert

instance HasSimpleRep MIRTarget
instance IsConwayUniv fn => HasSpec fn MIRTarget

instance HasSimpleRep MIRPot
instance IsConwayUniv fn => HasSpec fn MIRPot

instance HasSimpleRep (ShelleyTxCert era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (ShelleyTxCert era)

instance HasSimpleRep GenesisDelegCert
instance IsConwayUniv fn => HasSpec fn GenesisDelegCert
