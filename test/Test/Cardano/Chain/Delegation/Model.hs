{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.Delegation.Model
  ( prop_ScheduleDelegationModel
  )
where

import Cardano.Prelude

import Data.Coerce
import Data.IORef

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Cardano.Chain.Delegation.Validation as Concrete
import qualified Control.State.Transition as Abstract
import qualified Control.State.Transition.Generator as Abstract
import qualified Ledger.Delegation as Abstract

import Test.Cardano.Chain.Elaboration.Delegation
import Test.Cardano.Crypto.Dummy (dummyProtocolMagicId)


prop_ScheduleDelegationModel :: Property
prop_ScheduleDelegationModel = withTests 25 . property $ do
  concreteRef <- liftIO $ newIORef initialConcreteState

  abstractEnv <- forAll $ Abstract.initEnvGen @Abstract.DELEG

  actions <- forAll $ Gen.sequential
    (Range.linear 1 20)
    initialState
    [commandScheduleDelegation concreteRef abstractEnv]

  liftIO (cleanup concreteRef) >> executeSequential initialState actions
 where
  initialConcreteState = Concrete.SchedulingState mempty mempty

  cleanup = flip writeIORef initialConcreteState


newtype ScheduleDelegationState (v :: Type -> Type)
  = ScheduleDelegationState Abstract.DSState

initialState :: ScheduleDelegationState v
initialState = ScheduleDelegationState $ Abstract.DSState
  []
  mempty


data ScheduleDelegation (v :: Type -> Type)
  = ScheduleDelegation Abstract.DCert
  deriving Show

instance HTraversable ScheduleDelegation where
  htraverse _ s = pure (coerce s)


-- TODO: Change dcertGen to use 'MonadGen'
commandScheduleDelegation
  :: forall m
   . MonadIO m
  => IORef Concrete.SchedulingState
  -> Abstract.DSEnv
  -> Command Gen m ScheduleDelegationState
commandScheduleDelegation concreteRef abstractEnv = Command gen execute callbacks
 where
  gen :: ScheduleDelegationState v -> Maybe (Gen (ScheduleDelegation v))
  gen _ = Just $ ScheduleDelegation <$> Abstract.dcertGen abstractEnv

  execute
    :: ScheduleDelegation v
    -> m (Either Concrete.SchedulingError Concrete.SchedulingState)
  execute (ScheduleDelegation cert) = do
    concreteState <- liftIO $ readIORef concreteRef

    let
      result :: Either Concrete.SchedulingError Concrete.SchedulingState
      result = Concrete.scheduleCertificate
          dummyProtocolMagicId
          (elaborateDSEnv abstractEnv)
          concreteState
          (elaborateDCertAnnotated dummyProtocolMagicId cert)

    either (const $ pure ()) (liftIO . writeIORef concreteRef) result

    pure result

  callbacks :: [Callback ScheduleDelegation (Either a b) ScheduleDelegationState]
  callbacks =
    [ Update $ \(ScheduleDelegationState s) (ScheduleDelegation cert) _ ->
        ScheduleDelegationState $
          case Abstract.applySTS @Abstract.SDELEG (Abstract.TRC (abstractEnv, s, cert)) of
            Left _ -> s
            Right s' -> s'
    , Ensure $ \(ScheduleDelegationState s) _ (ScheduleDelegation cert) result ->
        let abstractResult = Abstract.applySTS
              @Abstract.SDELEG
              (Abstract.TRC (abstractEnv, s, cert))
        in isRight result === isRight abstractResult
    ]
