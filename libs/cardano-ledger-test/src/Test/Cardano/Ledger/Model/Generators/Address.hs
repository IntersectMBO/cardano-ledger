{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Model.Generators.Address where

import Cardano.Ledger.Keys (KeyRole (..))
import Control.Monad.Supply (MonadSupply, supply)
import Data.Proxy (Proxy (..))
import QuickCheck.GenT (MonadGen, oneof)
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelPoolId (..),
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( IfSupportsPlutus (..),
    KnownScriptFeature,
    ScriptFeatureTag (..),
    TyScriptFeature (..),
    reifyScriptFeature,
  )
import Test.Cardano.Ledger.Model.Generators
  ( AllowScripts,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelAddress (..),
    ModelCredential (..),
    ModelPlutusScript (..),
    PreprocessedPlutusScript (..),
  )

freshPaymentAddress :: (Show n, MonadSupply n m) => String -> m (ModelAddress era)
freshPaymentAddress clue =
  ModelAddress
    <$> freshCredential ("pmt:" <> clue)
    <*> freshCredential ("stk:" <> clue)

freshPaymentScript ::
  MonadSupply Integer m =>
  m (ModelCredential 'Payment ('TyScriptFeature x 'True))
freshPaymentScript = do
  x <- supply
  pure $ ModelScriptHashObj $ ModelPlutusScript_Salt x $ ModelPlutusScript_Preprocessed SumsTo103

genPaymentCredential ::
  forall sf m.
  (KnownScriptFeature sf, MonadSupply Integer m, MonadGen m) =>
  AllowScripts sf ->
  String ->
  m (ModelCredential 'Payment sf)
genPaymentCredential haveCollateral clue =
  oneof $
    [freshCredential clue] <> case reifyScriptFeature (Proxy :: Proxy sf) of
      ScriptFeatureTag_None -> []
      ScriptFeatureTag_Simple -> []
      ScriptFeatureTag_PlutusV1 -> case haveCollateral of
        SupportsPlutus True -> [freshPaymentScript]
        _ -> []

freshStakeScript ::
  MonadSupply Integer m =>
  m (ModelCredential 'Staking ('TyScriptFeature x 'True))
freshStakeScript = do
  x <- supply
  pure $ ModelScriptHashObj $ ModelPlutusScript_Salt x $ ModelPlutusScript_Preprocessed RedeemerIs102

genStakingCredential ::
  forall sf m.
  (KnownScriptFeature sf, MonadSupply Integer m, MonadGen m) =>
  String ->
  m (ModelCredential 'Staking sf)
genStakingCredential clue =
  oneof $
    [freshCredential clue] <> case reifyScriptFeature (Proxy :: Proxy sf) of
      ScriptFeatureTag_None -> []
      ScriptFeatureTag_Simple -> []
      ScriptFeatureTag_PlutusV1 -> [freshStakeScript]

genAddr ::
  ( MonadGen m,
    MonadSupply Integer m,
    KnownScriptFeature sf
  ) =>
  AllowScripts sf ->
  String ->
  m (ModelAddress sf)
genAddr haveCollateral clue =
  ModelAddress
    <$> genPaymentCredential haveCollateral clue
    <*> genStakingCredential clue

freshCredential :: (Show n, MonadSupply n m) => String -> m (ModelCredential r era)
freshCredential clue = ModelKeyHashObj . (clue <>) . show <$> supply

freshRewardAddress :: (Show n, MonadSupply n m) => m (ModelCredential 'Staking era)
freshRewardAddress = ModelKeyHashObj . ("reward_" <>) . show <$> supply

freshPoolAddress :: (Show n, MonadSupply n m) => m ModelPoolId
freshPoolAddress = ModelPoolId <$> freshCredential "pool_"

freshWdrlAddress :: (Show n, MonadSupply n m) => ModelCredential 'Staking era -> m (ModelAddress era)
freshWdrlAddress c = do
  c' <- case c of
    ModelKeyHashObj x -> freshCredential ("wdrl-" <> x)
    ModelScriptHashObj _ -> freshCredential "wdrl"
  pure $ ModelAddress c' c
