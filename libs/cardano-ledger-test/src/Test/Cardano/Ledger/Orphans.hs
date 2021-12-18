{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Orphans () where

import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Slotting.Slot (EpochNo (..))
import Control.DeepSeq
import Control.Lens.Internal.Coerce ((#..))
import Control.Lens.Internal.Zoom
import Control.Lens.Zoom
import Control.Monad.RWS.CPS ()
import Control.Monad.Reader (ReaderT (..))
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.RWS.CPS as CPS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Writer.CPS ()
import Data.Monoid (All (..))
import Data.Profunctor.Unsafe ((#.))
import GHC.Generics ((:*:) (..), (:.:) (..))
import QuickCheck.GenT (MonadGen (..))
import qualified System.Random

instance NFData IsValid

deriving via All instance Semigroup IsValid

deriving via All instance Monoid IsValid

type instance Zoomed (CPS.RWST r w s z) = FocusingWith w z

instance (Monoid w, Monad z) => Zoom (CPS.RWST r w s z) (CPS.RWST r w t z) s t where
  zoom l m = CPS.rwsT $ \r -> unfocusingWith #. l (FocusingWith #. (CPS.runRWST m) r)
  {-# INLINE zoom #-}

type instance Zoomed (CPS.WriterT w m) = FocusingPlus w (Zoomed m)

instance (Monoid w, Zoom m n s t) => Zoom (CPS.WriterT w m) (CPS.WriterT w n) s t where
  zoom l = CPS.writerT . zoom (\afb -> unfocusingPlus #.. l (FocusingPlus #.. afb)) . CPS.runWriterT
  {-# INLINE zoom #-}

instance (NFData (f x), NFData (g x)) => NFData ((:*:) f g x)

deriving newtype instance NFData (f (g x)) => NFData ((:.:) f g x)

deriving newtype instance System.Random.Random EpochNo -- TODO: this can be moved closer to the package that defines EpochNo

deriving newtype instance System.Random.Random Coin -- TODO: this can be moved closer to the package that defines EpochNo

deriving newtype instance System.Random.Random DeltaCoin -- TODO: this can be moved closer to the package that defines EpochNo

instance MonadGen g => MonadGen (State.StateT s g) where
  liftGen = lift . liftGen
  variant n a = State.StateT $ \s -> variant n (State.runStateT a s)
  sized f = State.StateT $ \s -> sized (\i -> State.runStateT (f i) s)
  resize n a = State.StateT $ \s -> resize n (State.runStateT a s)
  choose = lift . choose

instance MonadGen g => MonadGen (ReaderT r g) where
  liftGen = lift . liftGen
  variant n a = ReaderT $ \s -> variant n (runReaderT a s)
  sized f = ReaderT $ \s -> sized (\i -> runReaderT (f i) s)
  resize n a = ReaderT $ \s -> resize n (runReaderT a s)
  choose = lift . choose

instance (Monoid w, MonadGen g) => MonadGen (CPS.WriterT w g) where
  liftGen = lift . liftGen
  variant n a = CPS.writerT $ variant n (CPS.runWriterT a)
  sized f = CPS.writerT $ sized (\i -> CPS.runWriterT (f i))
  resize n a = CPS.writerT $ resize n (CPS.runWriterT a)
  choose = lift . choose
