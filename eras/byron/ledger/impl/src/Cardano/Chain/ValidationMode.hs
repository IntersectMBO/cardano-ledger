{-# LANGUAGE FlexibleContexts #-}

module Cardano.Chain.ValidationMode
  ( ValidationMode (..),
    fromBlockValidationMode,
    orThrowErrorInBlockValidationMode,
    askBlockValidationMode,
    askTxValidationMode,
    whenBlockValidation,
    whenTxValidation,
    unlessNoTxValidation,
    wrapErrorWithValidationMode,
  )
where

import Cardano.Chain.Block.ValidationMode
  ( BlockValidationMode (..),
    toTxValidationMode,
  )
import Cardano.Chain.UTxO.ValidationMode (TxValidationMode (..))
import Cardano.Prelude

--------------------------------------------------------------------------------
-- ValidationMode
--------------------------------------------------------------------------------

data ValidationMode = ValidationMode
  { blockValidationMode :: !BlockValidationMode,
    txValidationMode :: !TxValidationMode
  }
  deriving (Show)

-- | Helper function which accepts a 'BlockValidationMode', constructs a
-- sensible 'TxValidationMode' based on that, and constructs a
-- 'ValidationMode'
fromBlockValidationMode :: BlockValidationMode -> ValidationMode
fromBlockValidationMode bvm =
  ValidationMode
    { blockValidationMode = bvm,
      txValidationMode = toTxValidationMode bvm
    }

orThrowErrorInBlockValidationMode ::
  (MonadError e m, MonadReader ValidationMode m) =>
  Bool ->
  e ->
  m ()
orThrowErrorInBlockValidationMode condition err = do
  bvm <- askBlockValidationMode
  unless (bvm == NoBlockValidation || condition) (throwError err)

infix 1 `orThrowErrorInBlockValidationMode`

--------------------------------------------------------------------------------
-- ValidationMode Helpers
--------------------------------------------------------------------------------

askBlockValidationMode ::
  MonadReader ValidationMode m =>
  m BlockValidationMode
askBlockValidationMode = blockValidationMode <$> ask

askTxValidationMode ::
  MonadReader ValidationMode m =>
  m TxValidationMode
askTxValidationMode = txValidationMode <$> ask

-- | Perform an action only when in the 'BlockValidation' mode. Otherwise, do
-- nothing.
whenBlockValidation ::
  (MonadError err m, MonadReader ValidationMode m) =>
  m () ->
  m ()
whenBlockValidation action = do
  bvmode <- askBlockValidationMode
  when (bvmode == BlockValidation) action

-- | Perform an action only when in the 'TxValidation' mode. Otherwise, do
-- nothing.
whenTxValidation ::
  (MonadError err m, MonadReader ValidationMode m) =>
  m () ->
  m ()
whenTxValidation action = do
  tvmode <- askTxValidationMode
  when (tvmode == TxValidation) action

-- | Perform an action unless in the 'NoTxValidation' mode.
unlessNoTxValidation ::
  (MonadError err m, MonadReader ValidationMode m) =>
  m () ->
  m ()
unlessNoTxValidation action = do
  tvmode <- askTxValidationMode
  unless (tvmode == NoTxValidation) action

wrapErrorWithValidationMode ::
  (MonadError e' m, MonadReader ValidationMode m) =>
  ReaderT ValidationMode (Either e) a ->
  (e -> e') ->
  m a
wrapErrorWithValidationMode rt wrapper = do
  vMode <- ask
  case runReaderT rt vMode of
    Left err -> throwError $ wrapper err
    Right x -> pure x

infix 1 `wrapErrorWithValidationMode`
