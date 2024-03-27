{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
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

-- RecordWildCards cause name shadowing warnings in ghc-8.10.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#endif

-- | This module provides the necessary instances of `HasSpec`
-- and `HasSimpleRep` to write specs for the environments,
-- states, and signals in the conway STS rules.
module Test.Cardano.Ledger.Constrained.V2.Conway where

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
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxOut
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Api
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
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
import Cardano.Ledger.Keys (GenDelegPair (..), GenDelegs (..), KeyHash, KeyRole (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.MemoBytes
import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.PoolParams
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley.LedgerState hiding (ptrMap)
import Cardano.Ledger.Shelley.PoolRank
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap
import Cardano.Ledger.UTxO
import Cardano.Ledger.Val (Val)
import Control.Monad.Trans.Fail.String
import Crypto.Hash (Blake2b_224)
import Data.ByteString qualified as BS
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Coerce
import Data.Foldable
import Data.Kind
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
import Numeric.Natural (Natural)
import PlutusLedgerApi.V1 qualified as PV1
import System.Random
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Core.Utils
import Test.Cardano.Ledger.Generic.Fields ()
import Test.QuickCheck hiding (Args, Fun, forAll)

import Constrained hiding (Value)
import Constrained qualified as C

type ConwayUnivFns = StringFn : RoseTreeFn : BaseFns
type ConwayFn = Fix (OneofL ConwayUnivFns)

type IsConwayUniv fn =
  ( BaseUniverse fn
  , Member (StringFn fn) fn
  , Member (MapFn fn) fn
  , Member (FunFn fn) fn
  , Member (RoseTreeFn fn) fn
  )

-- TxBody HasSpec instance ------------------------------------------------

-- NOTE: this is a representation of the `ConwayTxBody` type. You can't
-- simply use the generics to derive the `SimpleRep` for `ConwayTxBody`
-- because the type is memoized. So instead we say that the representation
-- is the same as what you would get from using the `ConwayTxBody` pattern.
type ConwayTxBodyTypes c =
  '[ Set (TxIn (EraCrypto (ConwayEra c)))
   , Set (TxIn (EraCrypto (ConwayEra c)))
   , Set (TxIn (EraCrypto (ConwayEra c)))
   , StrictSeq (Sized (TxOut (ConwayEra c)))
   , StrictMaybe (Sized (TxOut (ConwayEra c)))
   , StrictMaybe Coin
   , SOS.OSet (ConwayTxCert (ConwayEra c))
   , Withdrawals (EraCrypto (ConwayEra c))
   , Coin
   , ValidityInterval
   , Set (KeyHash 'Witness (EraCrypto (ConwayEra c)))
   , MultiAsset (EraCrypto (ConwayEra c))
   , StrictMaybe (ScriptIntegrityHash (EraCrypto (ConwayEra c)))
   , StrictMaybe (AuxiliaryDataHash (EraCrypto (ConwayEra c)))
   , StrictMaybe Network
   , VotingProcedures (ConwayEra c)
   , SOS.OSet (ProposalProcedure (ConwayEra c))
   , StrictMaybe Coin
   , Coin
   ]
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (ConwayTxBody (ConwayEra c))

instance Crypto c => HasSimpleRep (ConwayTxBody (ConwayEra c)) where
  type SimpleRep (ConwayTxBody (ConwayEra c)) = SOP '["ConwayTxBody" ::: ConwayTxBodyTypes c]
  toSimpleRep ConwayTxBody {..} =
    inject @"ConwayTxBody" @'["ConwayTxBody" ::: ConwayTxBodyTypes c]
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
    algebra @'["ConwayTxBody" ::: ConwayTxBodyTypes c] rep ConwayTxBody

instance HasSimpleRep Coin where
  type SimpleRep Coin = Word64
  toSimpleRep (Coin i) = case integerToWord64 i of
    Nothing -> error "The impossible happened in toSimpleRep for `Coin`"
    Just w -> w
  fromSimpleRep = word64ToCoin
instance IsConwayUniv fn => HasSpec fn Coin
instance IsConwayUniv fn => OrdLike fn Coin
instance IsConwayUniv fn => NumLike fn Coin
instance IsConwayUniv fn => Foldy fn Coin where
  genList s s' = map fromSimpleRep <$> genList @fn @Word64 (toSimpleRepSpec s) (toSimpleRepSpec s')
  theAddFn = addFn
  theZero = Coin 0

-- TODO: This is hack to get around the need for `Num` in `NumLike`. We should possibly split
-- this up so that `NumLike` has its own addition etc. instead?
deriving via Integer instance Num Coin

instance HasSimpleRep DeltaCoin where
  type SimpleRep DeltaCoin = Integer
  fromSimpleRep = DeltaCoin
  toSimpleRep (DeltaCoin c) = c
instance IsConwayUniv fn => HasSpec fn DeltaCoin

instance HasSimpleRep (GovProcedures era)
instance
  ( Era era
  , EraPParams era
  , IsConwayUniv fn
  , HasSimpleRep (PParamsHKD StrictMaybe era)
  , TypeSpec fn (SimpleRep (PParamsHKD StrictMaybe era)) ~ TypeSpec fn (PParamsHKD StrictMaybe era)
  , HasSpec fn (SimpleRep (PParamsHKD StrictMaybe era))
  ) =>
  HasSpec fn (GovProcedures era)

instance HasSimpleRep SlotNo
instance IsConwayUniv fn => OrdLike fn SlotNo
instance IsConwayUniv fn => HasSpec fn SlotNo

instance HasSimpleRep EpochNo
instance IsConwayUniv fn => OrdLike fn EpochNo
instance IsConwayUniv fn => HasSpec fn EpochNo

instance HasSimpleRep TxIx
instance IsConwayUniv fn => HasSpec fn TxIx

instance (IsConwayUniv fn, Crypto c, Typeable index) => HasSpec fn (SafeHash c index) where
  type TypeSpec fn (SafeHash c index) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep (TxId c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (TxId c)

instance HasSimpleRep (TxIn c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (TxIn c)

instance (IsConwayUniv fn, Typeable r, Crypto c) => HasSpec fn (KeyHash r c) where
  type TypeSpec fn (KeyHash r c) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

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

instance HasSimpleRep (StrictMaybe a)
instance HasSpec fn a => HasSpec fn (StrictMaybe a)

cSNothing_ :: HasSpec fn a => Term fn (StrictMaybe a)
cSNothing_ = con @"SNothing" (lit ())

cSJust_ :: HasSpec fn a => Term fn a -> Term fn (StrictMaybe a)
cSJust_ = con @"SJust"

instance HasSimpleRep (Sized a)
instance (IsConwayUniv fn, HasSpec fn a) => HasSpec fn (Sized a)

instance HasSimpleRep Addr28Extra
instance IsConwayUniv fn => HasSpec fn Addr28Extra

instance HasSimpleRep DataHash32
instance IsConwayUniv fn => HasSpec fn DataHash32

type BabbageTxOutTypes era =
  '[ Addr (EraCrypto era)
   , Value era
   , Datum era
   , StrictMaybe (Script era)
   ]
instance (Era era, Val (Value era)) => HasSimpleRep (BabbageTxOut era) where
  type SimpleRep (BabbageTxOut era) = SOP '["BabbageTxOut" ::: BabbageTxOutTypes era]
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
  , Crypto (EraCrypto era)
  , HasSpec fn (Script era)
  ) =>
  HasSpec fn (BabbageTxOut era)

instance (Typeable a, Show a, Compactible a) => HasSimpleRep (CompactForm a) where
  type SimpleRep (CompactForm a) = a
  toSimpleRep = fromCompact
  fromSimpleRep rep = case toCompact rep of
    Nothing -> error $ "toCompact @" ++ show (typeOf rep) ++ " " ++ show rep
    Just c -> c
instance (IsConwayUniv fn, Compactible a, HasSpec fn a) => HasSpec fn (CompactForm a)

instance HasSimpleRep (MaryValue c) where
  type SimpleRep (MaryValue c) = SOP '["MaryValue" ::: '[Coin]]
  toSimpleRep (MaryValue c _) = c
  fromSimpleRep c = MaryValue c mempty
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (MaryValue c)

instance HasSimpleRep PV1.Data
instance IsConwayUniv fn => HasSpec fn PV1.Data where
  type TypeSpec fn PV1.Data = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
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
  (IsConwayUniv fn, Era era, Crypto (EraCrypto era), HasSpec fn (Data era)) =>
  HasSpec fn (BinaryData era)

instance HasSimpleRep (Datum era)
instance (IsConwayUniv fn, Era era, HasSpec fn (Data era), Crypto (EraCrypto era)) => HasSpec fn (Datum era)

-- TODO: here we are cheating to get out of having to deal with Plutus scripts
instance HasSimpleRep (AlonzoScript era) where
  type SimpleRep (AlonzoScript era) = Timelock era
  toSimpleRep (TimelockScript tl) = tl
  toSimpleRep (PlutusScript _) = error "toSimpleRep for AlonzoScript on a PlutusScript"
  fromSimpleRep = TimelockScript
instance (IsConwayUniv fn, EraScript era, Script era ~ AlonzoScript era) => HasSpec fn (AlonzoScript era)

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
changing the `Spec` - thus giving you the ability to provide a generation time hint!

Solving (1) is more tricky however. The best guess I have is that you would need
to push any constraint you have into functions `MyConstraint :: MyUniv fn '[Timelock era] Bool`
and implement everything "offline". This is highly non-satisfactory - but it's hard to see
how else you would do it.

type TimelockTypes era =
  '[ -- RequireSignature
     '[KeyHash 'Witness (EraCrypto era)]
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

instance (IsConwayUniv fn, Crypto (EraCrypto era), Era era) => HasSpec fn (Timelock era) where
  type TypeSpec fn (Timelock era) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance Crypto c => HasSimpleRep (CompactAddr c) where
  type SimpleRep (CompactAddr c) = SimpleRep (Addr c)
  toSimpleRep = toSimpleRep . decompactAddr
  fromSimpleRep = compactAddr . fromSimpleRep
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (CompactAddr c)

instance HasSimpleRep (Addr c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (Addr c)

instance HasSimpleRep (BootstrapAddress c) where
  type
    TheSop (BootstrapAddress c) =
      '[ "BootstrapAddress"
          ::: '[ AbstractHash Blake2b_224 Address'
               , NetworkMagic
               , AddrType
               ]
       ]
  toSimpleRep (BootstrapAddress (Address root (Attributes (AddrAttributes _ magic) _) typ)) =
    inject @"BootstrapAddress" @(TheSop (BootstrapAddress c))
      root
      magic
      typ
  fromSimpleRep rep =
    algebra @(TheSop (BootstrapAddress c)) rep $
      \root magic typ ->
        BootstrapAddress
          (Address root (Attributes (AddrAttributes Nothing magic) (UnparsedFields mempty)) typ)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (BootstrapAddress c)

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
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep (StakeReference c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (StakeReference c)

instance HasSimpleRep Ptr
instance IsConwayUniv fn => HasSpec fn Ptr

instance HasSimpleRep CertIx where
  type SimpleRep CertIx = Word16
  toSimpleRep (CertIx w) = fromIntegral w
  fromSimpleRep = mkCertIx
instance IsConwayUniv fn => HasSpec fn CertIx

instance HasSimpleRep (Credential r c)
instance (IsConwayUniv fn, Typeable r, Crypto c) => HasSpec fn (Credential r c)

cKeyHashObj ::
  (IsConwayUniv fn, Typeable r, Crypto c) => Term fn (KeyHash r c) -> Term fn (Credential r c)
cKeyHashObj = con @"KeyHashObj"

cScriptHashObj ::
  (IsConwayUniv fn, Typeable r, Crypto c) => Term fn (ScriptHash c) -> Term fn (Credential r c)
cScriptHashObj = con @"ScriptHashObj"

instance HasSimpleRep (ScriptHash c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (ScriptHash c)

instance (IsConwayUniv fn, HashAlgorithm a, Typeable b) => HasSpec fn (Hash a b) where
  type TypeSpec fn (Hash a b) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep (ConwayTxCert era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (ConwayTxCert era)

instance HasSimpleRep (ConwayDelegCert c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (ConwayDelegCert c)

instance HasSimpleRep (PoolCert c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (PoolCert c)

instance HasSimpleRep (PoolParams c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (PoolParams c)

instance HasSimpleRep PoolMetadata
instance IsConwayUniv fn => HasSpec fn PoolMetadata

instance IsConwayUniv fn => HasSpec fn StakePoolRelay where
  type TypeSpec fn StakePoolRelay = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep Port
instance IsConwayUniv fn => HasSpec fn Port

-- TODO: once you start adding interesting functions on these to the
-- function universe this instance has to become more complicated
instance IsConwayUniv fn => HasSpec fn UnitInterval where
  type TypeSpec fn UnitInterval = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

-- TODO: once you start adding interesting functions on these to the
-- function universe this instance has to become more complicated
instance IsConwayUniv fn => HasSpec fn NonNegativeInterval where
  type TypeSpec fn NonNegativeInterval = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep (ConwayGovCert c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (ConwayGovCert c)

instance HasSimpleRep (Anchor c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (Anchor c)

instance HasSimpleRep Url
instance IsConwayUniv fn => HasSpec fn Url

instance IsConwayUniv fn => HasSpec fn Text where
  type TypeSpec fn Text = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

data StringSpec fn = StringSpec {strSpecLen :: Spec fn Int}

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
    len <- genFromSpec ls
    BS.pack <$> vectorOfT len (pureGen arbitrary)
  conformsTo bs (StringSpec ls) = BS.length bs `conformsToSpec` ls
  toPreds str (StringSpec len) = satisfies (strLen_ str) len

instance IsConwayUniv fn => HasSpec fn ShortByteString where
  type TypeSpec fn ShortByteString = StringSpec fn
  emptySpec = mempty
  combineSpec s s' = typeSpec $ s <> s'
  genFromTypeSpec (StringSpec ls) = do
    len <- genFromSpec ls
    SBS.pack <$> vectorOfT len (pureGen arbitrary)
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
  lengthSpec :: IsConwayUniv fn => Spec fn Int -> TypeSpec fn s
  getLengthSpec :: TypeSpec fn s -> Spec fn Int
  getLength :: s -> Int

instance HasSimpleRep (Delegatee c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (Delegatee c)

instance HasSimpleRep (DRep c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (DRep c)

instance HasSimpleRep (Withdrawals c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (Withdrawals c)

instance HasSimpleRep (RewardAccount c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (RewardAccount c)

instance HasSimpleRep Network
instance IsConwayUniv fn => HasSpec fn Network

instance HasSimpleRep (MultiAsset c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (MultiAsset c) where
  emptySpec = flip (MapSpec mempty [] mempty) NoFold $
    constrained' $ \_ innerMap ->
      forAll innerMap $ \kv' ->
        lit 0 <=. snd_ kv'

instance HasSimpleRep AssetName where
  type SimpleRep AssetName = ShortByteString
  toSimpleRep (AssetName sbs) = sbs
  fromSimpleRep sbs = AssetName sbs
instance IsConwayUniv fn => HasSpec fn AssetName

instance HasSimpleRep (PolicyID c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (PolicyID c)

instance HasSimpleRep (AuxiliaryDataHash c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (AuxiliaryDataHash c)

instance HasSimpleRep (VotingProcedures era)
instance (IsConwayUniv fn, Typeable era, Crypto (EraCrypto era)) => HasSpec fn (VotingProcedures era)

instance HasSimpleRep (VotingProcedure era)
instance (IsConwayUniv fn, Typeable era, Crypto (EraCrypto era)) => HasSpec fn (VotingProcedure era)

instance HasSimpleRep Vote
instance IsConwayUniv fn => HasSpec fn Vote

instance HasSimpleRep (GovActionId c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (GovActionId c)

instance HasSimpleRep GovActionIx
instance IsConwayUniv fn => HasSpec fn GovActionIx

instance HasSimpleRep ProtVer
instance IsConwayUniv fn => HasSpec fn ProtVer

-- We do this like this to get the right bounds for `VersionRep`
-- while ensuring that we don't have to add instances for e.g. `Num`
-- to version.
newtype VersionRep = VersionRep Word8
  deriving (Show, Eq, Ord, Num, Random) via Word8
instance BaseUniverse fn => HasSpec fn VersionRep where
  type TypeSpec fn VersionRep = NumSpec VersionRep
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
instance Bounded VersionRep where
  minBound = VersionRep $ getVersion minBound
  maxBound = VersionRep $ getVersion maxBound
instance MaybeBounded VersionRep

instance HasSimpleRep Version where
  type SimpleRep Version = VersionRep
  fromSimpleRep (VersionRep rep) = case runFail $ mkVersion rep of
    Left err ->
      error $
        unlines
          [ "fromSimpleRep @Version:"
          , show rep
          , err
          ]
    Right a -> a
  toSimpleRep = VersionRep . getVersion
instance BaseUniverse fn => HasSpec fn Version
instance BaseUniverse fn => OrdLike fn Version

succV_ :: BaseUniverse fn => Term fn Version -> Term fn Version
succV_ = fromGeneric_ . (+ 1) . toGeneric_

instance HasSimpleRep (GovPurposeId p era)
instance (Typeable p, IsConwayUniv fn, Era era) => HasSpec fn (GovPurposeId p era)

instance HasSimpleRep (GovAction era)
instance
  ( IsConwayUniv fn
  , EraPParams era
  , HasSimpleRep (PParamsHKD StrictMaybe era)
  , TypeSpec fn (SimpleRep (PParamsHKD StrictMaybe era)) ~ TypeSpec fn (PParamsHKD StrictMaybe era)
  , HasSpec fn (SimpleRep (PParamsHKD StrictMaybe era))
  ) =>
  HasSpec fn (GovAction era)

instance HasSimpleRep (Constitution era)
instance (IsConwayUniv fn, EraPParams era) => HasSpec fn (Constitution era)

instance HasSimpleRep (PParamsHKD StrictMaybe era) => HasSimpleRep (PParamsUpdate era) where
  type SimpleRep (PParamsUpdate era) = SimpleRep (PParamsHKD StrictMaybe era)
  toSimpleRep (PParamsUpdate hkd) = toSimpleRep hkd
  fromSimpleRep = PParamsUpdate . fromSimpleRep
instance
  ( IsConwayUniv fn
  , EraPParams era
  , HasSimpleRep (PParamsHKD StrictMaybe era)
  , TypeSpec fn (SimpleRep (PParamsHKD StrictMaybe era)) ~ TypeSpec fn (PParamsHKD StrictMaybe era)
  , HasSpec fn (SimpleRep (PParamsHKD StrictMaybe era))
  , Show (TypeSpec fn (PParamsHKD StrictMaybe era))
  ) =>
  HasSpec fn (PParamsUpdate era)

instance HasSimpleRep EpochInterval
instance IsConwayUniv fn => OrdLike fn EpochInterval
instance IsConwayUniv fn => HasSpec fn EpochInterval

instance HasSimpleRep (ConwayPParams StrictMaybe c)
instance
  ( IsConwayUniv fn
  , Typeable c
  ) =>
  HasSpec fn (ConwayPParams StrictMaybe c)

instance HasSimpleRep (ConwayPParams Identity era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (ConwayPParams Identity era)

instance HasSimpleRep (PParams (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (PParams (ConwayEra StandardCrypto))

instance HasSimpleRep ExUnits where
  type SimpleRep ExUnits = SimpleRep (Natural, Natural)
  fromSimpleRep = uncurry ExUnits . fromSimpleRep
  toSimpleRep (ExUnits a b) = toSimpleRep (a, b)
instance IsConwayUniv fn => HasSpec fn ExUnits

instance HasSimpleRep OrdExUnits where
  type SimpleRep OrdExUnits = SimpleRep ExUnits
  fromSimpleRep = OrdExUnits . fromSimpleRep
  toSimpleRep = toSimpleRep . unOrdExUnits
instance IsConwayUniv fn => HasSpec fn OrdExUnits

instance HasSimpleRep CoinPerByte where
  -- TODO: consider `SimpleRep Coin` instead if this is annoying
  type SimpleRep CoinPerByte = Coin
  fromSimpleRep = CoinPerByte
  toSimpleRep = unCoinPerByte
instance IsConwayUniv fn => HasSpec fn CoinPerByte

instance HasSimpleRep CostModels
instance IsConwayUniv fn => HasSpec fn CostModels where
  emptySpec =
    Cartesian
      (constrained $ \m -> size_ (dom_ m) <=. 3)
      (constrained $ \p -> size_ (dom_ (fst_ $ fromGeneric_ p)) <=. 3)

instance HasSimpleRep PoolVotingThresholds
instance IsConwayUniv fn => HasSpec fn PoolVotingThresholds

instance HasSimpleRep DRepVotingThresholds
instance IsConwayUniv fn => HasSpec fn DRepVotingThresholds

instance HasSimpleRep CostModelError
instance IsConwayUniv fn => HasSpec fn CostModelError

instance HasSimpleRep CostModelApplyError
instance IsConwayUniv fn => HasSpec fn CostModelApplyError

instance IsConwayUniv fn => HasSpec fn Char where
  type TypeSpec fn Char = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance IsConwayUniv fn => HasSpec fn CostModel where
  type TypeSpec fn CostModel = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance HasSimpleRep Language
instance IsConwayUniv fn => HasSpec fn Language

instance HasSimpleRep Prices
instance IsConwayUniv fn => HasSpec fn Prices

instance HasSimpleRep (NoUpdate a)
instance (IsConwayUniv fn, Typeable a) => HasSpec fn (NoUpdate a)

instance HasSimpleRep (THKD tag StrictMaybe a) where
  type SimpleRep (THKD tag StrictMaybe a) = SimpleRep (StrictMaybe a)
  fromSimpleRep = THKD . fromSimpleRep
  toSimpleRep (THKD sm) = toSimpleRep sm
instance (IsConwayUniv fn, Typeable tag, HasSpec fn a) => HasSpec fn (THKD tag StrictMaybe a)

instance HasSimpleRep (THKD tag Identity a) where
  type SimpleRep (THKD tag Identity a) = a
  fromSimpleRep = THKD
  toSimpleRep (THKD a) = a
instance (IsConwayUniv fn, Typeable tag, HasSpec fn a) => HasSpec fn (THKD tag Identity a)

instance HasSimpleRep GovActionPurpose
instance IsConwayUniv fn => HasSpec fn GovActionPurpose

instance HasSimpleRep (Voter c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (Voter c)

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
  ( IsConwayUniv fn
  , EraPParams era
  , HasSimpleRep (PParamsHKD StrictMaybe era)
  , TypeSpec fn (SimpleRep (PParamsHKD StrictMaybe era)) ~ TypeSpec fn (PParamsHKD StrictMaybe era)
  , HasSpec fn (SimpleRep (PParamsHKD StrictMaybe era))
  ) =>
  HasSpec fn (ProposalProcedure era)

pProcGovAction_ ::
  IsConwayUniv fn =>
  Term fn (ProposalProcedure (ConwayEra StandardCrypto)) ->
  Term fn (GovAction (ConwayEra StandardCrypto))
pProcGovAction_ = sel @2

instance HasSimpleRep ValidityInterval
instance IsConwayUniv fn => HasSpec fn ValidityInterval

instance HasSimpleRep (DRepState c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (DRepState c)

instance HasSimpleRep (CommitteeAuthorization c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (CommitteeAuthorization c)

instance HasSimpleRep (CommitteeState era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (CommitteeState era)

instance HasSimpleRep (VState era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (VState era)

instance HasSimpleRep (PState era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (PState era)

instance HasSimpleRep (DState era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (DState era)

instance HasSimpleRep (FutureGenDeleg c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (FutureGenDeleg c)

instance HasSimpleRep (GenDelegPair c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (GenDelegPair c)

instance HasSimpleRep (GenDelegs c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (GenDelegs c)

instance HasSimpleRep (InstantaneousRewards c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (InstantaneousRewards c)

type UMapTypes c =
  '[ Map (Credential 'Staking c) RDPair
   , Map Ptr (Credential 'Staking c)
   , Map (Credential 'Staking c) (KeyHash 'StakePool c)
   , Map (Credential 'Staking c) (DRep c)
   ]
instance Crypto c => HasSimpleRep (UMap c) where
  type SimpleRep (UMap c) = SOP '["UMap" ::: UMapTypes c]
  toSimpleRep um = inject @"UMap" @'["UMap" ::: UMapTypes c] (rdPairMap um) (ptrMap um) (sPoolMap um) (dRepMap um)
  fromSimpleRep rep = algebra @'["UMap" ::: UMapTypes c] rep unify
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (UMap c)

instance HasSimpleRep RDPair where
  type SimpleRep RDPair = SOP '["RDPair" ::: '[Coin, Coin]]
  toSimpleRep (RDPair rew dep) = inject @"RDPair" @'["RDPair" ::: '[Coin, Coin]] (toSimpleRep rew) (toSimpleRep dep)
  fromSimpleRep rep =
    algebra @'["RDPair" ::: '[Coin, Coin]]
      rep
      (\rew dep -> RDPair (fromSimpleRep rew) (fromSimpleRep dep))
instance IsConwayUniv fn => HasSpec fn RDPair

instance HasSimpleRep (CertState era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (CertState era)

instance HasSimpleRep (GovRelation StrictMaybe era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (GovRelation StrictMaybe era)

instance HasSimpleRep (GovEnv (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (GovEnv (ConwayEra StandardCrypto))

instance HasSimpleRep (GovActionState (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (GovActionState (ConwayEra StandardCrypto))

gasId_ ::
  IsConwayUniv fn =>
  Term fn (GovActionState (ConwayEra StandardCrypto)) ->
  Term fn (GovActionId StandardCrypto)
gasId_ = sel @0

gasCommitteeVotes_ ::
  IsConwayUniv fn =>
  Term fn (GovActionState (ConwayEra StandardCrypto)) ->
  Term fn (Map (Credential 'HotCommitteeRole StandardCrypto) Vote)
gasCommitteeVotes_ = sel @1

gasDRepVotes_ ::
  IsConwayUniv fn =>
  Term fn (GovActionState (ConwayEra StandardCrypto)) ->
  Term fn (Map (Credential 'DRepRole StandardCrypto) Vote)
gasDRepVotes_ = sel @2

gasProposalProcedure_ ::
  IsConwayUniv fn =>
  Term fn (GovActionState (ConwayEra StandardCrypto)) ->
  Term fn (ProposalProcedure (ConwayEra StandardCrypto))
gasProposalProcedure_ = sel @4

type GAS = GovActionState (ConwayEra StandardCrypto)
type ProposalTree = (StrictMaybe (GovActionId StandardCrypto), [RoseTree GAS])
type ProposalsType =
  '[ ProposalTree -- PParamUpdate
   , ProposalTree -- HardFork
   , ProposalTree -- Committee
   , ProposalTree -- Constitution
   , [GAS] -- Everything else (TreasuryWithdrawals, Info)
   -- TODO - in order to improve the distribution of orders in the OMap
   -- one could try doing something like this as well to materialize the order:
   -- , TotalOrder (GovActionId StandardCrypto)
   -- However, (1) I have no clue how this would work in detail and (2) the approach
   -- of DFS gives us a lot of testing already, and there are bigger fish to fry than
   -- this right now.
   ]
instance HasSimpleRep (Proposals (ConwayEra StandardCrypto)) where
  type SimpleRep (Proposals (ConwayEra StandardCrypto)) = SOP '["Proposals" ::: ProposalsType]
  toSimpleRep props =
    inject @"Proposals" @'["Proposals" ::: ProposalsType]
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

      buildProposalTree :: TreeMaybe (GovActionId StandardCrypto) -> ProposalTree
      buildProposalTree (TreeMaybe (Node mId cs)) = (mId, map buildTree cs)

      buildTree :: Tree (StrictMaybe (GovActionId StandardCrypto)) -> RoseTree GAS
      buildTree (Node (SJust gid) cs) | Just gas <- Map.lookup gid idMap = RoseNode gas (map buildTree cs)
      buildTree _ =
        error "toSimpleRep @Proposals: toGovRelationTree returned trees with Nothing nodes below the root"

      keys :: TreeMaybe (GovActionId StandardCrypto) -> Set (GovActionId StandardCrypto)
      keys (TreeMaybe t) = foldMap (strictMaybe mempty Set.singleton) t

  fromSimpleRep rep =
    algebra @'["Proposals" ::: ProposalsType]
      rep
      $ \(rPPUp, ppupTree) (rHF, hfTree) (rCom, comTree) (rCon, conTree) others ->
        let root = GovRelation (coerce rPPUp) (coerce rHF) (coerce rCom) (coerce rCon)
            -- TODO: note, this doesn't roundtrip and the distribution is a bit iffy. See the TODO
            -- above for ideas on how to deal with this.
            oMap = foldMap (foldMap mkOMap) [ppupTree, hfTree, comTree, conTree] <> OMap.fromFoldable others
         in unsafeMkProposals root oMap
    where
      mkOMap (RoseNode a ts) = a OMap.<| foldMap mkOMap ts

instance IsConwayUniv fn => HasSpec fn (Proposals (ConwayEra StandardCrypto))

instance HasSimpleRep (EnactSignal (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (EnactSignal (ConwayEra StandardCrypto))

instance HasSimpleRep (EnactState (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (EnactState (ConwayEra StandardCrypto))

instance HasSimpleRep (Committee (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (Committee (ConwayEra StandardCrypto))

instance HasSimpleRep (RatifyEnv (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (RatifyEnv (ConwayEra StandardCrypto))

instance HasSimpleRep (RatifyState (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (RatifyState (ConwayEra StandardCrypto))

instance HasSimpleRep (RatifySignal (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (RatifySignal (ConwayEra StandardCrypto))

instance Crypto c => HasSimpleRep (PoolDistr c)
instance (Crypto c, IsConwayUniv fn) => HasSpec fn (PoolDistr c)

instance Crypto c => HasSimpleRep (IndividualPoolStake c)
instance (Crypto c, IsConwayUniv fn) => HasSpec fn (IndividualPoolStake c)

instance HasSimpleRep (ConwayGovCertEnv (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (ConwayGovCertEnv (ConwayEra StandardCrypto))

instance HasSimpleRep (PoolEnv (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (PoolEnv (ConwayEra StandardCrypto))

instance HasSimpleRep (CertEnv (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (CertEnv (ConwayEra StandardCrypto))

instance HasSimpleRep (EpochState (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (EpochState (ConwayEra StandardCrypto))

instance HasSimpleRep (NonMyopic c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (NonMyopic c)

instance HasSimpleRep Likelihood
instance IsConwayUniv fn => HasSpec fn Likelihood

instance HasSimpleRep LogWeight
instance IsConwayUniv fn => HasSpec fn LogWeight

instance HasSimpleRep AccountState
instance IsConwayUniv fn => HasSpec fn AccountState

instance HasSimpleRep (SnapShot c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (SnapShot c)

instance HasSimpleRep (Stake c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (Stake c)

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

instance HasSimpleRep (SnapShots c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (SnapShots c)

instance HasSimpleRep (LedgerState (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (LedgerState (ConwayEra StandardCrypto))

instance HasSimpleRep (UTxOState (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (UTxOState (ConwayEra StandardCrypto))

instance HasSimpleRep (IncrementalStake c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (IncrementalStake c)

instance HasSimpleRep (UTxO (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (UTxO (ConwayEra StandardCrypto))

instance HasSimpleRep (ConwayGovState (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (ConwayGovState (ConwayEra StandardCrypto))

instance HasSimpleRep (DRepPulsingState (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (DRepPulsingState (ConwayEra StandardCrypto))

instance HasSimpleRep (PulsingSnapshot (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (PulsingSnapshot (ConwayEra StandardCrypto))

type DRepPulserTypes =
  '[ Int
   , UMap StandardCrypto
   , Map (Credential 'Staking StandardCrypto) (CompactForm Coin)
   , Map (Credential 'Staking StandardCrypto) (CompactForm Coin)
   , PoolDistr StandardCrypto
   , Map (DRep StandardCrypto) (CompactForm Coin)
   , Map (Credential 'DRepRole StandardCrypto) (DRepState StandardCrypto)
   , EpochNo
   , CommitteeState (ConwayEra StandardCrypto)
   , EnactState (ConwayEra StandardCrypto)
   , StrictSeq (GovActionState (ConwayEra StandardCrypto))
   ]
instance
  HasSimpleRep
    (DRepPulser (ConwayEra StandardCrypto) Identity (RatifyState (ConwayEra StandardCrypto)))
  where
  type
    SimpleRep (DRepPulser (ConwayEra StandardCrypto) Identity (RatifyState (ConwayEra StandardCrypto))) =
      SOP '["DRepPulser" ::: DRepPulserTypes]
  toSimpleRep DRepPulser {..} =
    inject @"DRepPulser" @'["DRepPulser" ::: DRepPulserTypes]
      dpPulseSize
      dpUMap
      dpBalance
      dpStakeDistr
      dpStakePoolDistr
      dpDRepDistr
      dpDRepState
      dpCurrentEpoch
      dpCommitteeState
      dpEnactState
      dpProposals
  fromSimpleRep rep =
    algebra @'["DRepPulser" ::: DRepPulserTypes]
      rep
      $ \ps um b sd spd dd ds ce cs es p ->
        DRepPulser ps um b sd spd dd ds ce cs es p testGlobals
instance
  IsConwayUniv fn =>
  HasSpec fn (DRepPulser (ConwayEra StandardCrypto) Identity (RatifyState (ConwayEra StandardCrypto)))

instance HasSimpleRep (UtxoEnv (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (UtxoEnv (ConwayEra StandardCrypto))

instance HasSimpleRep (AlonzoTx (ConwayEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (AlonzoTx (ConwayEra StandardCrypto))

instance HasSimpleRep IsValid
instance IsConwayUniv fn => HasSpec fn IsValid

-- NOTE: we don't generate or talk about plutus scripts (yet!)
type AlonzoTxAuxDataTypes =
  '[ Map Word64 Metadatum
   , StrictSeq (Timelock (ConwayEra StandardCrypto))
   ]
instance HasSimpleRep (AlonzoTxAuxData (ConwayEra StandardCrypto)) where
  type
    SimpleRep (AlonzoTxAuxData (ConwayEra StandardCrypto)) =
      SOP '["AlonzoTxOutData" ::: AlonzoTxAuxDataTypes]
  toSimpleRep (AlonzoTxAuxData metaMap tsSeq _) =
    inject @"AlonzoTxAuxData" @'["AlonzoTxAuxData" ::: AlonzoTxAuxDataTypes]
      metaMap
      tsSeq
  fromSimpleRep rep =
    algebra @'["AlonzoTxAuxData" ::: AlonzoTxAuxDataTypes] rep $
      \metaMap tsSeq -> AlonzoTxAuxData metaMap tsSeq mempty
instance IsConwayUniv fn => HasSpec fn (AlonzoTxAuxData (ConwayEra StandardCrypto))

instance HasSimpleRep Metadatum
instance IsConwayUniv fn => HasSpec fn Metadatum

type AlonzoTxWitsTypes =
  '[ Set (WitVKey 'Witness StandardCrypto)
   , Set (BootstrapWitness StandardCrypto)
   ]
instance HasSimpleRep (AlonzoTxWits (ConwayEra StandardCrypto)) where
  type
    SimpleRep (AlonzoTxWits (ConwayEra StandardCrypto)) =
      SOP '["AlonzoTxWits" ::: AlonzoTxWitsTypes]
  toSimpleRep (AlonzoTxWits vkeyWits bootstrapWits _ _ _) =
    inject @"AlonzoTxWits" @'["AlonzoTxWits" ::: AlonzoTxWitsTypes]
      vkeyWits
      bootstrapWits
  fromSimpleRep rep =
    algebra @'["AlonzoTxWits" ::: AlonzoTxWitsTypes] rep $
      \vkeyWits bootstrapWits -> AlonzoTxWits vkeyWits bootstrapWits mempty (TxDats mempty) (Redeemers mempty)
instance IsConwayUniv fn => HasSpec fn (AlonzoTxWits (ConwayEra StandardCrypto))

instance (IsConwayUniv fn, Crypto c, Typeable r) => HasSpec fn (WitVKey r c) where
  type TypeSpec fn (WitVKey r c) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
  conformsTo _ _ = True
  toPreds _ _ = toPred True

instance (IsConwayUniv fn, Crypto c) => HasSpec fn (BootstrapWitness c) where
  type TypeSpec fn (BootstrapWitness c) = ()
  emptySpec = ()
  combineSpec _ _ = TrueSpec
  genFromTypeSpec _ = pureGen arbitrary
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
