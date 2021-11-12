{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.ModelChain.FeatureSet where

import Control.DeepSeq
import Data.Kind (Type)
import Data.Proxy
import Data.Type.Bool
import Data.Type.Equality ((:~:) (..))

data TyValueExpected
  = ExpectAdaOnly
  | ExpectAnyOutput

data TyScriptFeature = TyScriptFeature
  { _tyScript_timelock :: !Bool,
    _tyScript_plutus :: !Bool
  }

type family PlutusScriptFeature (a :: TyScriptFeature) where
  PlutusScriptFeature ('TyScriptFeature _ x) = x

type ShelleyScriptFeatures = 'TyScriptFeature 'False 'False

data ScriptFeatureTag (s :: TyScriptFeature) where
  ScriptFeatureTag_None :: ScriptFeatureTag ('TyScriptFeature 'False 'False)
  ScriptFeatureTag_Simple :: ScriptFeatureTag ('TyScriptFeature 'True 'False)
  ScriptFeatureTag_PlutusV1 :: ScriptFeatureTag ('TyScriptFeature 'True 'True)

deriving instance Show (ScriptFeatureTag s)

class KnownScriptFeature (s :: TyScriptFeature) where reifyScriptFeature :: proxy s -> ScriptFeatureTag s

instance KnownScriptFeature ('TyScriptFeature 'False 'False) where reifyScriptFeature _ = ScriptFeatureTag_None

instance KnownScriptFeature ('TyScriptFeature 'True 'False) where reifyScriptFeature _ = ScriptFeatureTag_Simple

instance KnownScriptFeature ('TyScriptFeature 'True 'True) where reifyScriptFeature _ = ScriptFeatureTag_PlutusV1

hasKnownScriptFeature :: ScriptFeatureTag s -> (KnownScriptFeature s => c) -> c
hasKnownScriptFeature = \case
  ScriptFeatureTag_None -> \x -> x
  ScriptFeatureTag_Simple -> \x -> x
  ScriptFeatureTag_PlutusV1 -> \x -> x

data IfSupportsTimelock a b (k :: TyScriptFeature) where
  NoTimelockSupport :: a -> IfSupportsTimelock a b ('TyScriptFeature 'False x)
  SupportsTimelock :: b -> IfSupportsTimelock a b ('TyScriptFeature 'True x)

data IfSupportsPlutus a b (k :: TyScriptFeature) where
  NoPlutusSupport :: a -> IfSupportsPlutus a b ('TyScriptFeature x 'False)
  SupportsPlutus :: b -> IfSupportsPlutus a b ('TyScriptFeature x 'True)

instance (NFData a, NFData b) => NFData (IfSupportsPlutus a b k) where
  rnf = \case
    NoPlutusSupport x -> rnf x
    SupportsPlutus y -> rnf y

deriving instance (Show a, Show b) => Show (IfSupportsPlutus a b k)

deriving instance (Eq a, Eq b) => Eq (IfSupportsPlutus a b k)

deriving instance (Ord a, Ord b) => Ord (IfSupportsPlutus a b k)

fromSupportsPlutus :: (a -> c) -> (b -> c) -> IfSupportsPlutus a b k -> c
fromSupportsPlutus f g = \case
  NoPlutusSupport x -> f x
  SupportsPlutus y -> g y

ifSupportsPlutus ::
  KnownScriptFeature s =>
  proxy s ->
  a ->
  b ->
  IfSupportsPlutus a b s
ifSupportsPlutus proxy x y = case reifySupportsPlutus proxy of
  NoPlutusSupport () -> NoPlutusSupport x
  SupportsPlutus () -> SupportsPlutus y

mapSupportsPlutus ::
  (a -> b) ->
  IfSupportsPlutus x a s ->
  IfSupportsPlutus x b s
mapSupportsPlutus f = \case
  NoPlutusSupport x -> NoPlutusSupport x
  SupportsPlutus x -> SupportsPlutus (f x)

traverseSupportsPlutus ::
  Applicative m =>
  (a -> m b) ->
  IfSupportsPlutus x a s ->
  m (IfSupportsPlutus x b s)
traverseSupportsPlutus f = \case
  NoPlutusSupport x -> pure $ NoPlutusSupport x
  SupportsPlutus x -> SupportsPlutus <$> f x

traverseSupportsPlutus_ ::
  Applicative m =>
  (a -> m b) ->
  IfSupportsPlutus x a s ->
  m ()
traverseSupportsPlutus_ f = \case
  NoPlutusSupport _ -> pure ()
  SupportsPlutus x -> () <$ f x

zipSupportsPlutusWith ::
  (a -> b -> c) ->
  IfSupportsPlutus () a s ->
  IfSupportsPlutus () b s ->
  IfSupportsPlutus () c s
zipSupportsPlutusWith f = \case
  NoPlutusSupport () -> \case
    NoPlutusSupport () -> NoPlutusSupport ()
  SupportsPlutus x -> \case
    SupportsPlutus y -> SupportsPlutus (f x y)

bitraverseSupportsPlutus ::
  Applicative m =>
  (a -> m c) ->
  (b -> m d) ->
  IfSupportsPlutus a b s ->
  m (IfSupportsPlutus c d s)
bitraverseSupportsPlutus f g = \case
  NoPlutusSupport x -> NoPlutusSupport <$> f x
  SupportsPlutus y -> SupportsPlutus <$> g y

reifySupportsPlutus ::
  KnownScriptFeature s => proxy s -> IfSupportsPlutus () () s
reifySupportsPlutus proxy = case reifyScriptFeature proxy of
  ScriptFeatureTag_None -> NoPlutusSupport ()
  ScriptFeatureTag_Simple -> NoPlutusSupport ()
  ScriptFeatureTag_PlutusV1 -> SupportsPlutus ()

data IfSupportsScript a b (k :: TyScriptFeature) where
  NoScriptSupport ::
    a ->
    IfSupportsScript a b ('TyScriptFeature 'False 'False)
  SupportsScript ::
    ((t || p) ~ 'True) =>
    ScriptFeatureTag ('TyScriptFeature t p) ->
    b ->
    IfSupportsScript a b ('TyScriptFeature t p)

-- same convention as Data.Bool.bool;  True case comes last
data IfSupportsMint a b (valF :: TyValueExpected) where
  NoMintSupport :: a -> IfSupportsMint a b 'ExpectAdaOnly
  SupportsMint :: b -> IfSupportsMint a b 'ExpectAnyOutput

instance (NFData a, NFData b) => NFData (IfSupportsMint a b k) where
  rnf = \case
    NoMintSupport x -> rnf x
    SupportsMint y -> rnf y

deriving instance (Show a, Show b) => Show (IfSupportsMint a b k)

deriving instance (Eq a, Eq b) => Eq (IfSupportsMint a b k)

instance (Semigroup a, Semigroup b) => Semigroup (IfSupportsMint a b k) where
  NoMintSupport x <> NoMintSupport y = NoMintSupport (x <> y)
  SupportsMint x <> SupportsMint y = SupportsMint (x <> y)

instance (Monoid a, Monoid b, KnownValueFeature k) => Monoid (IfSupportsMint a b k) where
  mempty = case reifyValueFeature (Proxy :: Proxy k) of
    ValueFeatureTag_AdaOnly -> NoMintSupport mempty
    ValueFeatureTag_AnyOutput -> SupportsMint mempty

ifSupportsMint ::
  KnownValueFeature s =>
  proxy s ->
  a ->
  b ->
  IfSupportsMint a b s
ifSupportsMint proxy x y = case reifyValueFeature proxy of
  ValueFeatureTag_AdaOnly -> NoMintSupport x
  ValueFeatureTag_AnyOutput -> SupportsMint y

fromSupportsMint :: (a -> c) -> (b -> c) -> IfSupportsMint a b s -> c
fromSupportsMint f g = \case
  NoMintSupport x -> f x
  SupportsMint y -> g y

mapSupportsMint ::
  (a -> b) ->
  IfSupportsMint x a s ->
  IfSupportsMint x b s
mapSupportsMint f = \case
  NoMintSupport x -> NoMintSupport x
  SupportsMint x -> SupportsMint (f x)

traverseSupportsMint ::
  Applicative m =>
  (a -> m b) ->
  IfSupportsMint x a s ->
  m (IfSupportsMint x b s)
traverseSupportsMint f = \case
  NoMintSupport x -> pure $ NoMintSupport x
  SupportsMint x -> SupportsMint <$> f x

reifySupportsMint ::
  KnownValueFeature s => proxy s -> IfSupportsMint () () s
reifySupportsMint proxy = case reifyValueFeature proxy of
  ValueFeatureTag_AdaOnly -> NoMintSupport ()
  ValueFeatureTag_AnyOutput -> SupportsMint ()

class v ~ 'ExpectAnyOutput => MintSupported v where
  supportsMint :: IfSupportsMint a b v -> b
  supportsMint (SupportsMint x) = x

instance v ~ 'ExpectAnyOutput => MintSupported v

type family ValueFeature (a :: FeatureSet) where
  ValueFeature ('FeatureSet v _) = v

type family ScriptFeature (a :: FeatureSet) where
  ScriptFeature ('FeatureSet _ s) = s

data FeatureSet = FeatureSet TyValueExpected TyScriptFeature

data FeatureTag (tag :: FeatureSet) where
  FeatureTag :: ValueFeatureTag v -> ScriptFeatureTag s -> FeatureTag ('FeatureSet v s)

deriving instance Show (FeatureTag tag)

data ValueFeatureTag (v :: TyValueExpected) where
  ValueFeatureTag_AdaOnly :: ValueFeatureTag 'ExpectAdaOnly
  ValueFeatureTag_AnyOutput :: ValueFeatureTag 'ExpectAnyOutput

deriving instance Show (ValueFeatureTag s)

type family MaxValueFeature (a :: TyValueExpected) (b :: TyValueExpected) :: TyValueExpected where
  MaxValueFeature 'ExpectAdaOnly b = b
  MaxValueFeature a 'ExpectAdaOnly = a
  MaxValueFeature 'ExpectAnyOutput b = b
  MaxValueFeature a 'ExpectAnyOutput = a

-- law: () <$ filterFeatures (minFeatures x) y == () <$ filterFeatures x y
class RequiredFeatures (f :: FeatureSet -> Type) where
  -- minFeatures :: f a -> FeatureTag a
  filterFeatures ::
    KnownRequiredFeatures a =>
    FeatureTag b ->
    f a ->
    Maybe (f b)

data GPartialOrdering (a :: k) (b :: k) where
  GIncomparable :: GPartialOrdering a b
  GPartialLT :: GPartialOrdering a b
  GPartialEQ :: GPartialOrdering a a
  GPartialGT :: GPartialOrdering a b

compareScriptFeatureTag :: ScriptFeatureTag a -> ScriptFeatureTag b -> GPartialOrdering a b
compareScriptFeatureTag = \case
  ScriptFeatureTag_None -> \case
    ScriptFeatureTag_None -> GPartialEQ
    ScriptFeatureTag_Simple -> GPartialLT
    ScriptFeatureTag_PlutusV1 -> GPartialLT
  ScriptFeatureTag_Simple -> \case
    ScriptFeatureTag_None -> GPartialLT
    ScriptFeatureTag_Simple -> GPartialEQ
    ScriptFeatureTag_PlutusV1 -> GPartialGT
  ScriptFeatureTag_PlutusV1 -> \case
    ScriptFeatureTag_None -> GPartialGT
    ScriptFeatureTag_Simple -> GPartialGT
    ScriptFeatureTag_PlutusV1 -> GPartialEQ

compareValueFeatureTag :: ValueFeatureTag a -> ValueFeatureTag b -> GPartialOrdering a b
compareValueFeatureTag = \case
  ValueFeatureTag_AdaOnly -> \case
    ValueFeatureTag_AdaOnly -> GPartialEQ
    ValueFeatureTag_AnyOutput -> GPartialLT
  ValueFeatureTag_AnyOutput -> \case
    ValueFeatureTag_AdaOnly -> GPartialGT
    ValueFeatureTag_AnyOutput -> GPartialEQ

compareFeatureTag :: FeatureTag a -> FeatureTag b -> GPartialOrdering a b
compareFeatureTag (FeatureTag v s) (FeatureTag v' s') =
  combineGPartialOrdering
    (compareValueFeatureTag v v')
    (compareScriptFeatureTag s s')
  where
    combineGPartialOrdering ::
      GPartialOrdering v v' -> GPartialOrdering s s' -> GPartialOrdering ('FeatureSet v s) ('FeatureSet v' s')
    combineGPartialOrdering GIncomparable _ = GIncomparable
    combineGPartialOrdering _ GIncomparable = GIncomparable
    combineGPartialOrdering GPartialEQ GPartialEQ = GPartialEQ
    combineGPartialOrdering GPartialEQ GPartialGT = GPartialGT
    combineGPartialOrdering GPartialEQ GPartialLT = GPartialLT
    combineGPartialOrdering GPartialGT GPartialEQ = GPartialGT
    combineGPartialOrdering GPartialGT GPartialGT = GPartialGT
    combineGPartialOrdering GPartialGT GPartialLT = GIncomparable
    combineGPartialOrdering GPartialLT GPartialEQ = GPartialLT
    combineGPartialOrdering GPartialLT GPartialGT = GIncomparable
    combineGPartialOrdering GPartialLT GPartialLT = GPartialLT

preceedsModelEra :: FeatureTag a -> FeatureTag b -> Bool
preceedsModelEra x y = case compareFeatureTag x y of
  GPartialLT -> True
  GPartialEQ -> True
  GPartialGT -> False
  GIncomparable -> False

class KnownValueFeature (v :: TyValueExpected) where reifyValueFeature :: proxy v -> ValueFeatureTag v

instance KnownValueFeature 'ExpectAdaOnly where reifyValueFeature _ = ValueFeatureTag_AdaOnly

instance KnownValueFeature 'ExpectAnyOutput where reifyValueFeature _ = ValueFeatureTag_AnyOutput

hasKnownValueFeature :: ValueFeatureTag v -> (KnownValueFeature v => c) -> c
hasKnownValueFeature = \case
  ValueFeatureTag_AdaOnly -> \x -> x
  ValueFeatureTag_AnyOutput -> \x -> x

class
  ( KnownValueFeature (ValueFeature a),
    KnownScriptFeature (ScriptFeature a),
    a ~ 'FeatureSet (ValueFeature a) (ScriptFeature a)
  ) =>
  KnownRequiredFeatures (a :: FeatureSet)
  where
  reifyRequiredFeatures :: proxy a -> FeatureTag a
  reifyRequiredFeatures _ =
    FeatureTag
      (reifyValueFeature (Proxy :: Proxy (ValueFeature a)))
      (reifyScriptFeature (Proxy :: Proxy (ScriptFeature a)))

hasKnownRequiredFeatures :: FeatureTag a -> (KnownRequiredFeatures a => c) -> c
hasKnownRequiredFeatures (FeatureTag v s) =
  \x -> hasKnownValueFeature v (hasKnownScriptFeature s x)

instance
  ( KnownValueFeature v,
    KnownScriptFeature s
  ) =>
  KnownRequiredFeatures ('FeatureSet v s)

instance RequiredFeatures FeatureTag where
  filterFeatures (FeatureTag v0 s0) (FeatureTag v1 s1) = FeatureTag <$> v <*> s
    where
      v = case (v0, v1) of
        (ValueFeatureTag_AdaOnly, ValueFeatureTag_AdaOnly) -> Just v0
        (ValueFeatureTag_AdaOnly, ValueFeatureTag_AnyOutput) -> Just v0
        (ValueFeatureTag_AnyOutput, ValueFeatureTag_AdaOnly) -> Nothing
        (ValueFeatureTag_AnyOutput, ValueFeatureTag_AnyOutput) -> Just v0

      s = case (s0, s1) of
        (ScriptFeatureTag_None, ScriptFeatureTag_None) -> Just s0
        (ScriptFeatureTag_None, ScriptFeatureTag_Simple) -> Just s0
        (ScriptFeatureTag_None, ScriptFeatureTag_PlutusV1) -> Just s0
        (ScriptFeatureTag_Simple, ScriptFeatureTag_None) -> Nothing
        (ScriptFeatureTag_Simple, ScriptFeatureTag_Simple) -> Just s0
        (ScriptFeatureTag_Simple, ScriptFeatureTag_PlutusV1) -> Just s0
        (ScriptFeatureTag_PlutusV1, ScriptFeatureTag_None) -> Nothing
        (ScriptFeatureTag_PlutusV1, ScriptFeatureTag_Simple) -> Nothing
        (ScriptFeatureTag_PlutusV1, ScriptFeatureTag_PlutusV1) -> Just s0

filterSupportsPlutus ::
  FeatureTag k ->
  IfSupportsPlutus () (Maybe x) s' ->
  Maybe (IfSupportsPlutus () (Maybe x) (ScriptFeature k))
filterSupportsPlutus (FeatureTag _ s) = case s of
  ScriptFeatureTag_None -> \case
    NoPlutusSupport () -> Just (NoPlutusSupport ())
    SupportsPlutus Nothing -> Just (NoPlutusSupport ())
    SupportsPlutus (Just _) -> Nothing
  ScriptFeatureTag_Simple -> \case
    NoPlutusSupport () -> Just (NoPlutusSupport ())
    SupportsPlutus Nothing -> Just (NoPlutusSupport ())
    SupportsPlutus (Just _) -> Nothing
  ScriptFeatureTag_PlutusV1 -> \case
    NoPlutusSupport () -> Just (SupportsPlutus Nothing)
    SupportsPlutus x -> Just (SupportsPlutus x)

reifyExpectAnyOutput :: ValueFeatureTag a -> Maybe (a :~: 'ExpectAnyOutput)
reifyExpectAnyOutput = \case
  ValueFeatureTag_AnyOutput -> Just Refl
  ValueFeatureTag_AdaOnly -> Nothing
