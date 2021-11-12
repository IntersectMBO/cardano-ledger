{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Model.Script where

import Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import qualified Cardano.Ledger.Crypto as C
import Cardano.Ledger.Keys
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Slotting.Slot hiding (at)
import Control.DeepSeq
import Control.Lens
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified GHC.Exts as GHC
import GHC.Generics
import Numeric.Natural (Natural)
import qualified PlutusTx (Data (..))
import Quiet (Quiet (..))
import Test.Cardano.Ledger.Alonzo.PlutusScripts as TestScripts
import qualified Test.Cardano.Ledger.Alonzo.Scripts as AlonzoTest
import Test.Cardano.Ledger.Model.FeatureSet

data ModelAddress (k :: TyScriptFeature) = ModelAddress
  { _modelAddress_pmt :: ModelCredential 'Payment k,
    _modelAddress_stk :: ModelCredential 'Staking k
  }
  deriving (Generic)

modelAddress_pmt :: Lens' (ModelAddress k) (ModelCredential 'Payment k)
modelAddress_pmt a2fb s = (\b -> s {_modelAddress_pmt = b}) <$> a2fb (_modelAddress_pmt s)
{-# INLINE modelAddress_pmt #-}

modelAddress_stk :: Lens' (ModelAddress k) (ModelCredential 'Staking k)
modelAddress_stk a2fb s = (\b -> s {_modelAddress_stk = b}) <$> a2fb (_modelAddress_stk s)
{-# INLINE modelAddress_stk #-}

deriving instance Eq (ModelAddress k)

deriving instance Ord (ModelAddress k)

deriving via (Quiet (ModelAddress k)) instance Show (ModelAddress k)

instance GHC.IsString (ModelAddress k) where
  fromString s = ModelAddress (GHC.fromString s) (GHC.fromString s)

instance NFData (ModelAddress k)

filterModelAddress ::
  FeatureTag b ->
  ModelAddress a ->
  Maybe (ModelAddress (ScriptFeature b))
filterModelAddress tag (ModelAddress pmt stk) =
  ModelAddress
    <$> filterModelCredential tag pmt
    <*> filterModelCredential tag stk

-- | Polykinded HasKeyRole
class HasKeyRole' (a :: KeyRole -> k -> Type) where
  -- | General coercion of key roles.
  --
  --   The presence of this function is mostly to help the user realise where they
  --   are converting key roles.
  coerceKeyRole' ::
    a r x ->
    a r' x
  default coerceKeyRole' ::
    Coercible (a r x) (a r' x) =>
    a r x ->
    a r' x
  coerceKeyRole' = coerce

data ModelCredential (r :: KeyRole) (k :: TyScriptFeature) where
  ModelKeyHashObj :: String -> ModelCredential r k
  -- TODO: this should be:
  -- ModelScriptHashObj :: ModelScript s -> ModelCredential r s
  ModelScriptHashObj :: ModelPlutusScript -> ModelCredential r ('TyScriptFeature x 'True)

_ModelKeyHashObj :: Prism' (ModelCredential r k) String
_ModelKeyHashObj = prism ModelKeyHashObj $ \case
  ModelKeyHashObj x -> Right x
  x -> Left x
{-# INLINE _ModelKeyHashObj #-}

_ModelScriptHashObj :: Prism (ModelCredential r k) (ModelCredential r' ('TyScriptFeature k' 'True)) ModelPlutusScript ModelPlutusScript
_ModelScriptHashObj = prism ModelScriptHashObj $ \case
  ModelScriptHashObj x -> Right x
  ModelKeyHashObj x -> Left (ModelKeyHashObj x)
{-# INLINE _ModelScriptHashObj #-}

filterModelCredential ::
  FeatureTag b ->
  ModelCredential r a ->
  Maybe (ModelCredential r' (ScriptFeature b))
filterModelCredential (FeatureTag _ s) = \case
  ModelKeyHashObj a -> Just (ModelKeyHashObj a)
  ModelScriptHashObj a -> case s of
    ScriptFeatureTag_None -> Nothing
    ScriptFeatureTag_Simple -> Nothing
    ScriptFeatureTag_PlutusV1 -> Just (ModelScriptHashObj a)

-- slightly reduced strength _ModelScriptHashObj because gadt's are not very
-- strong.
traverseModelScriptHashObj :: Traversal' (ModelCredential r k) ModelPlutusScript
traverseModelScriptHashObj f = \case
  ModelScriptHashObj x -> ModelScriptHashObj <$> f x
  ModelKeyHashObj x -> pure $ ModelKeyHashObj x
{-# INLINE traverseModelScriptHashObj #-}

deriving instance Eq (ModelCredential r k)

deriving instance Ord (ModelCredential r k)

instance Show (ModelCredential r k) where
  showsPrec n (ModelKeyHashObj x) = showsPrec n x
  showsPrec n (ModelScriptHashObj x) =
    showParen (n >= 11) $
      showString "ModelScriptHashObj " . showsPrec 11 x

instance GHC.IsString (ModelCredential r k) where
  fromString = ModelKeyHashObj

instance HasKeyRole' ModelCredential

instance NFData (ModelCredential r k) where
  rnf = \case
    ModelKeyHashObj x -> rnf x
    ModelScriptHashObj x -> rnf x

liftModelAddress ::
  ModelAddress ('TyScriptFeature 'False 'False) ->
  ModelAddress a
liftModelAddress (ModelAddress a b) = ModelAddress (liftModelCredential a) (liftModelCredential b)

liftModelCredential ::
  ModelCredential r ('TyScriptFeature 'False 'False) ->
  ModelCredential r a
liftModelCredential (ModelKeyHashObj a) = ModelKeyHashObj a

liftModelAddress' ::
  ModelAddress a ->
  ModelAddress ('TyScriptFeature 'True 'True)
liftModelAddress' (ModelAddress a b) = ModelAddress (liftModelCredential' a) (liftModelCredential' b)

liftModelCredential' ::
  ModelCredential r a ->
  ModelCredential r ('TyScriptFeature 'True 'True)
liftModelCredential' (ModelKeyHashObj a) = ModelKeyHashObj a
liftModelCredential' (ModelScriptHashObj a) = ModelScriptHashObj a

data ModelScript (k :: TyScriptFeature) where
  ModelScript_Timelock :: ModelTimelock -> ModelScript ('TyScriptFeature 'True x)
  ModelScript_PlutusV1 :: ModelPlutusScript -> ModelScript ('TyScriptFeature x 'True)

instance NFData (ModelScript k) where
  rnf = \case
    ModelScript_Timelock a -> rnf a
    ModelScript_PlutusV1 a -> rnf a

deriving instance Eq (ModelScript k)

deriving instance Ord (ModelScript k)

deriving instance Show (ModelScript k)

filterModelScript ::
  forall b a.
  KnownScriptFeature b =>
  ModelScript a ->
  Maybe (ModelScript b)
filterModelScript = \case
  ModelScript_Timelock t -> case reifyScriptFeature (Proxy @b) of
    ScriptFeatureTag_None -> Nothing
    ScriptFeatureTag_Simple -> Just $ ModelScript_Timelock t
    ScriptFeatureTag_PlutusV1 -> Just $ ModelScript_Timelock t
  ModelScript_PlutusV1 t -> case reifyScriptFeature (Proxy @b) of
    ScriptFeatureTag_None -> Nothing
    ScriptFeatureTag_Simple -> Nothing
    ScriptFeatureTag_PlutusV1 -> Just $ ModelScript_PlutusV1 t

-- | enum listing some preprocessed test scripts.
data PreprocessedPlutusScript
  = GuessTheNumber3
  | Evendata3
  | Odddata3
  | EvenRedeemer3
  | OddRedeemer3
  | SumsTo103
  | OddRedeemer2
  | EvenRedeemer2
  | RedeemerIs102
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance NFData PreprocessedPlutusScript

data ModelPlutusScript
  = ModelPlutusScript_AlwaysSucceeds Natural
  | ModelPlutusScript_AlwaysFails Natural
  | ModelPlutusScript_Preprocessed PreprocessedPlutusScript
  | ModelPlutusScript_Salt Integer ModelPlutusScript
  deriving (Eq, Ord, Show, Generic)

instance NFData ModelPlutusScript

modelScriptNeededSigs :: ModelTimelock -> [ModelCredential 'Witness ('TyScriptFeature 'False 'False)]
modelScriptNeededSigs = go
  where
    go = \case
      ModelTimelock_Signature ma -> [ma]
      ModelTimelock_AllOf xs -> go =<< xs
      ModelTimelock_AnyOf xs -> go =<< take 1 xs
      ModelTimelock_MOfN n xs -> go =<< take n xs
      ModelTimelock_TimeStart _ -> []
      ModelTimelock_TimeExpire _ -> []

-- modelScriptNeededSigs (ModelScript_PlutusV1 {}) = []
-- TODO: start/expire are somewhat irritating since absolute slot numbers aren't
-- visible in the model; it should probably be refactored to use epochs + slot
-- in epoch
data ModelTimelock
  = ModelTimelock_Signature (ModelCredential 'Witness ('TyScriptFeature 'False 'False))
  | ModelTimelock_AllOf [ModelTimelock]
  | ModelTimelock_AnyOf [ModelTimelock]
  | ModelTimelock_MOfN Int [ModelTimelock] -- Note that the Int may be negative in which case (MOfN -2 [..]) is always True
  | ModelTimelock_TimeStart SlotNo -- The start time
  | ModelTimelock_TimeExpire SlotNo -- The time it expires
  deriving (Eq, Ord, Show, Generic)

instance NFData ModelTimelock

elaborateModelTimelock ::
  forall crypto m.
  (C.Crypto crypto, Applicative m) =>
  (ModelCredential 'Witness ('TyScriptFeature 'False 'False) -> m (KeyHash 'Witness crypto)) ->
  ModelTimelock ->
  m (Timelock crypto)
elaborateModelTimelock f = go
  where
    go :: ModelTimelock -> m (Timelock crypto)
    go = \case
      ModelTimelock_Signature maddr -> RequireSignature <$> f maddr
      ModelTimelock_AllOf xs -> RequireAllOf . StrictSeq.fromList <$> traverse go xs
      ModelTimelock_AnyOf xs -> RequireAnyOf . StrictSeq.fromList <$> traverse go xs
      ModelTimelock_MOfN m xs -> RequireMOf m . StrictSeq.fromList <$> traverse go xs
      ModelTimelock_TimeStart slotNo -> pure $ RequireTimeStart slotNo
      ModelTimelock_TimeExpire slotNo -> pure $ RequireTimeExpire slotNo

elaboratePreprocessedPlutusScript :: PreprocessedPlutusScript -> Alonzo.Script x
elaboratePreprocessedPlutusScript = \case
  GuessTheNumber3 -> TestScripts.guessTheNumber3
  Evendata3 -> TestScripts.evendata3
  Odddata3 -> TestScripts.odddata3
  EvenRedeemer3 -> TestScripts.evenRedeemer3
  OddRedeemer3 -> TestScripts.oddRedeemer3
  SumsTo103 -> TestScripts.sumsTo103
  OddRedeemer2 -> TestScripts.oddRedeemer2
  EvenRedeemer2 -> TestScripts.evenRedeemer2
  RedeemerIs102 -> TestScripts.redeemerIs102

elaborateModelScript ::
  ModelPlutusScript ->
  Alonzo.Script era
elaborateModelScript = \case
  ModelPlutusScript_AlwaysSucceeds n -> AlonzoTest.alwaysSucceeds PlutusV1 n
  ModelPlutusScript_AlwaysFails n -> AlonzoTest.alwaysFails PlutusV1 n
  ModelPlutusScript_Preprocessed p -> elaboratePreprocessedPlutusScript p
  ModelPlutusScript_Salt n ps -> case elaborateModelScript ps of
    Alonzo.TimelockScript {} -> error $ "not supposted to be a timelock script: " <> show ps
    Alonzo.PlutusScript lang ps' -> AlonzoTest.saltFunction lang n ps'

evalModelPlutusScript ::
  Maybe PlutusTx.Data ->
  PlutusTx.Data ->
  ModelPlutusScript ->
  IsValid
evalModelPlutusScript dh rdmr = IsValid . go
  where
    nargs = maybe 2 (const 3) dh
    reqArgs n k
      | n < nargs = pure True
      | n == nargs = k
      | otherwise = pure False
    fromI = \case
      PlutusTx.I i -> Just i
      _ -> Nothing
    go = \case
      ModelPlutusScript_AlwaysSucceeds n -> n >= nargs
      ModelPlutusScript_AlwaysFails n -> n > nargs
      ModelPlutusScript_Salt _ ps -> go ps
      ModelPlutusScript_Preprocessed p -> fromMaybe False $ case p of
        GuessTheNumber3 -> reqArgs 3 $ (==) <$> (dh >>= fromI) <*> fromI rdmr
        Evendata3 -> reqArgs 3 $ dh >>= (fmap even . fromI)
        Odddata3 -> reqArgs 3 $ dh >>= (fmap odd . fromI)
        EvenRedeemer3 -> reqArgs 3 $ even <$> fromI rdmr
        OddRedeemer3 -> reqArgs 3 $ odd <$> fromI rdmr
        SumsTo103 -> reqArgs 3 $ do
          x <- (+) <$> (dh >>= fromI) <*> fromI rdmr
          pure $ x == 10
        OddRedeemer2 -> reqArgs 2 $ odd <$> fromI rdmr
        EvenRedeemer2 -> reqArgs 2 $ even <$> fromI rdmr
        RedeemerIs102 -> reqArgs 2 $ (10 ==) <$> fromI rdmr
