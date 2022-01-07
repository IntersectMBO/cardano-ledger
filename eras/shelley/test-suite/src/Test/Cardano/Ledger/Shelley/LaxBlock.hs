{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.LaxBlock where

import Cardano.Binary
  ( Annotator (..),
    Decoder,
    FromCBOR (fromCBOR),
    ToCBOR (..),
    annotatorSlice,
  )
import Cardano.Ledger.Block (Block (..), BlockAnn)
import Cardano.Ledger.Era (Era, ValidateScript (..))
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Cardano.Ledger.Shelley.BlockChain (TxSeq, txSeqDecoder)
import Data.Typeable

-- | A block in which we do not validate the matched
--   encoding of parts of the segwit.
--   This is only for testing.
newtype LaxBlock h era = LaxBlock (Block h era)

blockDecoder ::
  ( BlockAnn era,
    Era.TxSeq era ~ TxSeq era,
    FromCBOR (Annotator (h))
  ) =>
  Bool ->
  forall s. Decoder s (Annotator (Block h era))
blockDecoder lax = annotatorSlice $
  decodeRecordNamed "Block" (const 4) $ do
    header <- fromCBOR
    txns <- txSeqDecoder lax
    pure $ Block' <$> header <*> txns

instance (Era era, Typeable era, Typeable h) => ToCBOR (LaxBlock h era) where
  toCBOR (LaxBlock x) = toCBOR x

deriving stock instance
  (Era era, Show (Era.TxSeq era), Show h) =>
  Show (LaxBlock h era)

instance
  ( Era era,
    Typeable h,
    BlockAnn era,
    ValidateScript era,
    Era.TxSeq era ~ TxSeq era,
    FromCBOR (Annotator h)
  ) =>
  FromCBOR (Annotator (LaxBlock h era))
  where
  fromCBOR = fmap LaxBlock <$> blockDecoder True
