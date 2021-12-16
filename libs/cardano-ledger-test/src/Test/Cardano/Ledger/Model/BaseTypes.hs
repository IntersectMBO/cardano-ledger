{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Model.BaseTypes
  ( ModelValue (..),
    ModelValueVars (..),
    filterModelValue,
    liftModelValue,
    PreservedAda (..),
    ModelPoolId (..),
    ModelBlocksMade (..),
    _ModelBlocksMade,
    HasGlobals (..),
    PositiveRational,
    boundPositiveRational,
    ModelMA,
    unboundPositiveRational,
  )
where

import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Mary.Value (AssetName)
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData (..))
import Control.Lens
  ( Iso',
    iso,
    _1,
  )
import Data.Group (Abelian, Group)
import Data.Group.GrpMap (GrpMap (..))
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.Typeable ((:~:) (Refl))
import qualified GHC.Exts as GHC (IsString)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Quiet (Quiet (..))
import Test.Cardano.Ledger.Model.FeatureSet
  ( KnownRequiredFeatures,
    KnownValueFeature,
    RequiredFeatures (..),
    ScriptFeature,
    TyScriptFeature (..),
    TyValueExpected (..),
    ValueFeature,
    hasKnownRequiredFeatures,
    reifyExpectAnyOutput,
    reifyValueFeature,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelCredential,
    ModelScript,
    filterModelScript,
  )
import Test.Cardano.Ledger.Model.Value
  ( ModelValueF,
    mapModelValueF,
    traverseModelValueF,
  )

class HasGlobals a where
  getGlobals :: a -> Globals

instance HasGlobals Globals where
  getGlobals = id

-- | Things that have a pot of ADA in them.
--
-- This should only count coins that contribute toward the conserved quantity
-- 'Cardano.Ledger.BaseTypes.maxLovelaceSupply'
class PreservedAda a where
  -- | Get the amount of ADA in a value.
  totalPreservedAda :: a -> Coin

newtype ModelBlocksMade = ModelBlocksMade {unModelBlocksMade :: Map.Map ModelPoolId Natural}
  deriving (Generic, NFData, Eq)
  deriving (Show) via Quiet ModelBlocksMade
  deriving (Semigroup) via GrpMap ModelPoolId (Sum Natural)
  deriving (Monoid) via GrpMap ModelPoolId (Sum Natural)

_ModelBlocksMade :: Iso' ModelBlocksMade (Map.Map ModelPoolId Natural)
_ModelBlocksMade = iso unModelBlocksMade ModelBlocksMade

newtype ModelPoolId = ModelPoolId {unModelPoolId :: ModelCredential 'StakePool ('TyScriptFeature 'False 'False)}
  deriving (Eq, Ord, GHC.IsString, NFData)

-- always works because the wrapped credential is always KeyHashObj, and
-- consequently always like a string.
deriving newtype instance Show ModelPoolId

type ModelMA era = (ModelScript era, AssetName)

-- | key for a single non-ada asset
data ModelValueVars era (k :: TyValueExpected) where
  ModelValue_MA ::
    ('ExpectAnyOutput ~ ValueFeature era) =>
    ModelMA (ScriptFeature era) ->
    ModelValueVars era 'ExpectAnyOutput

instance NFData (ModelValueVars era k) where
  rnf = \case
    ModelValue_MA a -> rnf a

deriving instance Show (ModelValueVars era valF)

deriving instance Eq (ModelValueVars era valF)

deriving instance Ord (ModelValueVars era valF)

liftModelValueVars :: ModelValueVars era 'ExpectAdaOnly -> ModelValueVars era k
liftModelValueVars = \case {}

filterModelValueVars ::
  forall a b c d.
  (KnownRequiredFeatures c, KnownValueFeature d) =>
  ModelValueVars a b ->
  Maybe (ModelValueVars c d)
filterModelValueVars (ModelValue_MA ys) = do
  Refl <- reifyExpectAnyOutput (reifyValueFeature (Proxy @d))
  Refl <- reifyExpectAnyOutput (reifyValueFeature (Proxy @(ValueFeature c)))
  ModelValue_MA <$> _1 filterModelScript ys

newtype ModelValue k era = ModelValue {unModelValue :: ModelValueF (ModelValueVars era k)}
  deriving (Eq, Ord, Generic, NFData, Semigroup, Monoid, Group, Abelian, Val.Val)
  deriving (Show) via Quiet (ModelValue k era)

liftModelValue :: ModelValue 'ExpectAdaOnly era -> ModelValue k era
liftModelValue = ModelValue . mapModelValueF liftModelValueVars . unModelValue

-- | change the "expected return type" of a ModelValue
filterModelValue ::
  forall a b c.
  (KnownValueFeature b, KnownRequiredFeatures c) =>
  ModelValue a c ->
  Maybe (ModelValue b c)
filterModelValue = \case
  ModelValue x -> ModelValue <$> traverseModelValueF filterModelValueVars x

instance KnownValueFeature v => RequiredFeatures (ModelValue v) where
  filterFeatures tag (ModelValue val) = ModelValue <$> traverseModelValueF (hasKnownRequiredFeatures tag filterModelValueVars) val

-- | The spec stipulates that certain values particularly, the sigma parameter
-- used in rewards calculations) to be within [0,1], but which never-the-less
-- can be above 1 due to the
newtype PositiveRational = PositiveRational {unboundPositiveRational :: Rational}
  deriving (Eq, Ord, Show)
  deriving (Semigroup) via (Sum Rational)

boundPositiveRational :: Rational -> Maybe PositiveRational
boundPositiveRational x
  | x > 0 = Just $ PositiveRational x
  | otherwise = Nothing
