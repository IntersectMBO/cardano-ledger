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

-- | = Type level representation of features which are present, conditionally on
-- the era/hard fork.
--
-- Many of the helper types (those which start with \"@If@\") have a signature
-- which resembles 'Data.Bool.bool'.
module Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet (..),
    TyValueExpected (..),
    TyScriptFeature (..),
    ValueFeature,
    ScriptFeature,
    ShelleyScriptFeatures,
    reifySupportsMint',
    reifySupportsMint,
    KnownRequiredFeatures (..),
    KnownScriptFeature (..),
    KnownValueFeature (..),
    hasKnownRequiredFeatures,
    hasKnownValueFeature,
    reifyExpectAnyOutput,
    RequiredFeatures (..),
    preceedsModelEra,
    FeatureTag (..),
    ScriptFeatureTag (..),
    ValueFeatureTag (..),
    IfSupportsMint (..),
    MintSupported (..),
    FeatureSupport (..),
    ifSupportsMint,
    IfSupportsPlutus (..),
    filterSupportsPlutus,
    ifSupportsPlutus,
    reifySupportsPlutus,
    IfSupportsScript (..),
    hasKnownScriptFeature,
    IfSupportsTimelock (..),
  )
where

import Control.DeepSeq (NFData (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Type.Bool (type (||))
import Data.Type.Equality ((:~:) (..))

-- | Type level indication of what kind of value is allowed.
data TyValueExpected
  = -- | Only ada is permitted in this position.
    ExpectAdaOnly
  | -- | ada or multi-asset values are permitted.
    ExpectAnyOutput

-- | type level indication of what kind of script is allowed
data TyScriptFeature = TyScriptFeature
  { -- | true when multi-sig credentials are permitted.
    _tyScript_timelock :: !Bool,
    -- | true when plutus scripts are permitted.
    _tyScript_plutus :: !Bool
  }

type family PlutusScriptFeature (a :: TyScriptFeature) where
  PlutusScriptFeature ('TyScriptFeature _ x) = x

type ShelleyScriptFeatures = 'TyScriptFeature 'False 'False

-- | GADT to reify into a value the type level information in 'TyScriptFeature'
data ScriptFeatureTag (s :: TyScriptFeature) where
  ScriptFeatureTag_None :: ScriptFeatureTag ('TyScriptFeature 'False 'False)
  ScriptFeatureTag_Simple :: ScriptFeatureTag ('TyScriptFeature 'True 'False)
  ScriptFeatureTag_PlutusV1 :: ScriptFeatureTag ('TyScriptFeature 'True 'True)

deriving instance Show (ScriptFeatureTag s)

class KnownScriptFeature (s :: TyScriptFeature) where
  -- | Reflect 'TyScriptFeature' into a value.
  reifyScriptFeature :: proxy s -> ScriptFeatureTag s

instance KnownScriptFeature ('TyScriptFeature 'False 'False) where reifyScriptFeature _ = ScriptFeatureTag_None

instance KnownScriptFeature ('TyScriptFeature 'True 'False) where reifyScriptFeature _ = ScriptFeatureTag_Simple

instance KnownScriptFeature ('TyScriptFeature 'True 'True) where reifyScriptFeature _ = ScriptFeatureTag_PlutusV1

hasKnownScriptFeature :: ScriptFeatureTag s -> (KnownScriptFeature s => c) -> c
hasKnownScriptFeature = \case
  ScriptFeatureTag_None -> \x -> x
  ScriptFeatureTag_Simple -> \x -> x
  ScriptFeatureTag_PlutusV1 -> \x -> x

class FeatureSupport (f :: Type -> Type -> k -> Type) where
  bitraverseSupportsFeature :: Functor m => (a -> m c) -> (b -> m d) -> f a b x -> m (f c d x)

  bisequenceSupportsFeature :: Functor m => f (m a) (m b) x -> m (f a b x)
  bisequenceSupportsFeature = bitraverseSupportsFeature id id
  {-# INLINE bisequenceSupportsFeature #-}

  bitraverseSupportsFeature_ :: Functor m => (a -> m c) -> (b -> m d) -> f a b x -> m ()
  bitraverseSupportsFeature_ f g = (<$) () . bitraverseSupportsFeature f g
  {-# INLINE bitraverseSupportsFeature_ #-}

  traverseSupportsFeature :: Applicative m => (b -> m d) -> f a b x -> m (f a d x)
  traverseSupportsFeature = bitraverseSupportsFeature pure
  {-# INLINE traverseSupportsFeature #-}

  sequenceSupportsFeature :: Applicative m => f a (m b) x -> m (f a b x)
  sequenceSupportsFeature = traverseSupportsFeature id
  {-# INLINE sequenceSupportsFeature #-}

  traverseSupportsFeature_ :: Applicative m => (b -> m d) -> f a b x -> m ()
  traverseSupportsFeature_ f = (<$) () . bitraverseSupportsFeature pure f
  {-# INLINE traverseSupportsFeature_ #-}

  bimapSupportsFeature :: (a -> c) -> (b -> d) -> f a b x -> f c d x
  bimapSupportsFeature f g = runIdentity . bitraverseSupportsFeature (Identity . f) (Identity . g)
  {-# INLINE bimapSupportsFeature #-}

  mapSupportsFeature :: (b -> d) -> f a b x -> f a d x
  mapSupportsFeature = bimapSupportsFeature id
  {-# INLINE mapSupportsFeature #-}

  bifoldMapSupportsFeature :: (a -> m) -> (b -> m) -> f a b x -> m
  bifoldMapSupportsFeature f g = getConst . bitraverseSupportsFeature (Const . f) (Const . g)
  {-# INLINE bifoldMapSupportsFeature #-}

  foldMapSupportsFeature :: Monoid m => (b -> m) -> f a b x -> m
  foldMapSupportsFeature = bifoldMapSupportsFeature mempty
  {-# INLINE foldMapSupportsFeature #-}

  bifoldSupportsFeature :: f m m x -> m
  bifoldSupportsFeature = getConst . bitraverseSupportsFeature Const Const
  {-# INLINE bifoldSupportsFeature #-}

  foldSupportsFeature :: Monoid m => f a m x -> m
  foldSupportsFeature = getConst . bitraverseSupportsFeature mempty Const
  {-# INLINE foldSupportsFeature #-}

-- | Select a type if a model has support for multi-sig contracts.
-- same convention as Data.Bool.bool;  True case comes last
data IfSupportsTimelock a b (k :: TyScriptFeature) where
  NoTimelockSupport :: a -> IfSupportsTimelock a b ('TyScriptFeature 'False x)
  SupportsTimelock :: b -> IfSupportsTimelock a b ('TyScriptFeature 'True x)

instance FeatureSupport IfSupportsTimelock where
  bitraverseSupportsFeature f g = \case
    NoTimelockSupport x -> NoTimelockSupport <$> f x
    SupportsTimelock y -> SupportsTimelock <$> g y
  {-# INLINE bitraverseSupportsFeature #-}

-- | Select a type if a model has support for Plutus Scripts.
-- same convention as Data.Bool.bool;  True case comes last
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

ifSupportsPlutus ::
  forall s a b proxy.
  KnownScriptFeature s =>
  proxy s ->
  a ->
  b ->
  IfSupportsPlutus a b s
ifSupportsPlutus proxy x y = case reifySupportsPlutus proxy of
  NoPlutusSupport () -> NoPlutusSupport x
  SupportsPlutus () -> SupportsPlutus y

instance FeatureSupport IfSupportsPlutus where
  bitraverseSupportsFeature f g = \case
    NoPlutusSupport x -> NoPlutusSupport <$> f x
    SupportsPlutus y -> SupportsPlutus <$> g y
  {-# INLINE bitraverseSupportsFeature #-}

reifySupportsPlutus ::
  KnownScriptFeature s => proxy s -> IfSupportsPlutus () () s
reifySupportsPlutus proxy = case reifyScriptFeature proxy of
  ScriptFeatureTag_None -> NoPlutusSupport ()
  ScriptFeatureTag_Simple -> NoPlutusSupport ()
  ScriptFeatureTag_PlutusV1 -> SupportsPlutus ()

-- | Select a type if a model has support for any type of contract.
-- same convention as Data.Bool.bool;  True case comes last
data IfSupportsScript a b (k :: TyScriptFeature) where
  NoScriptSupport ::
    a ->
    IfSupportsScript a b ('TyScriptFeature 'False 'False)
  SupportsScript ::
    ((t || p) ~ 'True) =>
    ScriptFeatureTag ('TyScriptFeature t p) ->
    b ->
    IfSupportsScript a b ('TyScriptFeature t p)

-- | Select a type if a model has support for Multi-Asset values.
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
  (s ~ 'ExpectAdaOnly => a) ->
  (s ~ 'ExpectAnyOutput => b) ->
  IfSupportsMint a b s
ifSupportsMint proxy x y = case reifyValueFeature proxy of
  ValueFeatureTag_AdaOnly -> NoMintSupport x
  ValueFeatureTag_AnyOutput -> SupportsMint y

instance FeatureSupport IfSupportsMint where
  bitraverseSupportsFeature f g = \case
    NoMintSupport x -> NoMintSupport <$> f x
    SupportsMint y -> SupportsMint <$> g y
  {-# INLINE bitraverseSupportsFeature #-}

reifySupportsMint ::
  KnownValueFeature s => proxy s -> IfSupportsMint () () s
reifySupportsMint proxy = case reifyValueFeature proxy of
  ValueFeatureTag_AdaOnly -> NoMintSupport ()
  ValueFeatureTag_AnyOutput -> SupportsMint ()

reifySupportsMint' ::
  KnownValueFeature s => proxy s -> Bool
reifySupportsMint' = bifoldMapSupportsFeature (const False) (const True) . reifySupportsMint

class v ~ 'ExpectAnyOutput => MintSupported v where
  supportsMint :: IfSupportsMint a b v -> b
  supportsMint (SupportsMint x) = x

instance v ~ 'ExpectAnyOutput => MintSupported v

-- | project the 'TyValueExpected' out of a 'FeatureSet'
type family ValueFeature (a :: FeatureSet) where
  ValueFeature ('FeatureSet v _) = v

-- | project the 'TyScriptFeature' out of a 'FeatureSet'
type family ScriptFeature (a :: FeatureSet) where
  ScriptFeature ('FeatureSet _ s) = s

-- | A type level representation if which features are allowed in a ledger
-- model.  Rather than specifying a particular hard fork, this data only
-- expresses the presence or absence of particular features,
data FeatureSet = FeatureSet TyValueExpected TyScriptFeature

-- | GADT to reify the type level information in 'FeatureSet'
data FeatureTag (tag :: FeatureSet) where
  FeatureTag :: ValueFeatureTag v -> ScriptFeatureTag s -> FeatureTag ('FeatureSet v s)

deriving instance Show (FeatureTag tag)

-- | GADT to reify the type level information in 'TyValueExpected'
data ValueFeatureTag (v :: TyValueExpected) where
  ValueFeatureTag_AdaOnly :: ValueFeatureTag 'ExpectAdaOnly
  ValueFeatureTag_AnyOutput :: ValueFeatureTag 'ExpectAnyOutput

deriving instance Show (ValueFeatureTag s)

type family MaxValueFeature (a :: TyValueExpected) (b :: TyValueExpected) :: TyValueExpected where
  MaxValueFeature 'ExpectAdaOnly b = b
  MaxValueFeature a 'ExpectAdaOnly = a
  MaxValueFeature 'ExpectAnyOutput b = b
  MaxValueFeature a 'ExpectAnyOutput = a

class RequiredFeatures (f :: FeatureSet -> Type) where
  -- | restrict a model to a subset of ledger features.
  -- Returns 'Nothing' if the given model uses features not present in the
  -- requested era, or the same model with the right tag if the model is
  -- compatible with that model.
  -- law: () <$ filterFeatures (minFeatures x) y == () <$ filterFeatures x y
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

-- | test if one set of features is contained in another.
preceedsModelEra :: FeatureTag a -> FeatureTag b -> Bool
preceedsModelEra x y = case compareFeatureTag x y of
  GPartialLT -> True
  GPartialEQ -> True
  GPartialGT -> False
  GIncomparable -> False

class KnownValueFeature (v :: TyValueExpected) where
  -- | Reflect 'TyValueExpected' into a value.
  reifyValueFeature :: proxy v -> ValueFeatureTag v

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
  -- | Reflect 'FeatureSet' into a value.
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
