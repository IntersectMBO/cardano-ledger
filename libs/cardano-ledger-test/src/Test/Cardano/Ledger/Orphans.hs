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
import Control.DeepSeq
import Control.Lens.Internal.Coerce ((#..))
import Control.Lens.Internal.Zoom
import Control.Lens.Zoom
import Control.Monad.RWS.CPS ()
import qualified Control.Monad.Trans.RWS.CPS as CPS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Writer.CPS ()
import Data.Profunctor.Unsafe ((#.))
import GHC.Generics ((:*:) (..), (:.:) (..))

instance NFData IsValid

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
