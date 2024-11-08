{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.ImpSpec.Random where

import Control.Monad (replicateM)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import qualified System.Random.Stateful as R
import qualified Test.QuickCheck as QC (Arbitrary (arbitrary))
import Test.QuickCheck.GenT (MonadGen (liftGen))

class R.StatefulGen g m => HasStatefulGen g m | m -> g where
  askStatefulGen :: m g
  default askStatefulGen :: MonadReader g m => m g
  askStatefulGen = ask

class HasGenEnv env g | env -> g where
  getGenEnv :: env -> g

instance HasGenEnv g g where
  getGenEnv = id

instance
  (HasGenEnv env g, R.StatefulGen g (ReaderT env m), Monad m) =>
  HasStatefulGen g (ReaderT env m)
  where
  askStatefulGen = ReaderT (pure . getGenEnv)

uniformM ::
  ( HasStatefulGen g m
  , R.Uniform a
  ) =>
  m a
uniformM = askStatefulGen >>= R.uniformM
{-# INLINE uniformM #-}

uniformRM ::
  ( HasStatefulGen g m
  , R.UniformRange a
  ) =>
  (a, a) ->
  m a
uniformRM r = askStatefulGen >>= R.uniformRM r
{-# INLINE uniformRM #-}

uniformListM ::
  ( HasStatefulGen g m
  , R.Uniform a
  ) =>
  Int ->
  m [a]
uniformListM n = askStatefulGen >>= R.uniformListM n
{-# INLINE uniformListM #-}

uniformListRM ::
  (HasStatefulGen g m, R.UniformRange a) =>
  (a, a) ->
  Int ->
  m [a]
uniformListRM r n = askStatefulGen >>= replicateM n . R.uniformRM r
{-# INLINE uniformListRM #-}

uniformByteStringM :: HasStatefulGen a m => Int -> m ByteString
uniformByteStringM n = askStatefulGen >>= R.uniformByteStringM n
{-# INLINE uniformByteStringM #-}

uniformShortByteStringM :: HasStatefulGen a m => Int -> m ShortByteString
uniformShortByteStringM n = askStatefulGen >>= R.uniformShortByteString n
{-# INLINE uniformShortByteStringM #-}

-- | Lifted version of `QC.arbitrary`.
arbitrary :: (QC.Arbitrary a, MonadGen m) => m a
arbitrary = liftGen QC.arbitrary
