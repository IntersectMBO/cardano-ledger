{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.LaxBlock where

import Cardano.Ledger.Binary
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core (Era, EraSegWits (TxSeq), EraTx)
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq, txSeqDecoder)
import Data.Typeable (Typeable)

-- | A block in which we do not validate the matched
--   encoding of parts of the segwit.
--   This is only for testing.
newtype LaxBlock h era = LaxBlock (Block h era)

deriving newtype instance
  ( Era era
  , EncCBORGroup (TxSeq era)
  , EncCBOR h
  ) =>
  ToCBOR (LaxBlock h era)

deriving newtype instance
  ( EraSegWits era
  , DecCBOR (TxSeq era)
  , DecCBOR h
  ) =>
  DecCBOR (LaxBlock h era)

blockDecoder ::
  ( EraTx era
  , TxSeq era ~ ShelleyTxSeq era
  , DecCBOR (Annotator h)
  ) =>
  Bool ->
  forall s.
  Decoder s (Annotator (Block h era))
blockDecoder lax =
  decodeRecordNamed "Block" (const 4) $ do
    header <- decCBOR
    txns <- txSeqDecoder lax
    pure $ Block <$> header <*> txns

deriving stock instance (Era era, Show (TxSeq era), Show h) => Show (LaxBlock h era)

instance
  ( EraTx era
  , Typeable h
  , TxSeq era ~ ShelleyTxSeq era
  , DecCBOR (Annotator h)
  ) =>
  DecCBOR (Annotator (LaxBlock h era))
  where
  decCBOR = fmap LaxBlock <$> blockDecoder True
