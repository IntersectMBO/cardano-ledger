{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Transactions as they are received by the mempool. See `MempoolTx` for details.
module Cardano.Ledger.MempoolTx (
  MempoolTx,
  unwrapMempoolTx,
  binaryUpgradeMempoolTx,
) where

import Cardano.Ledger.Binary (Annotator, DecCBOR, DecoderError, EncCBOR)
import Cardano.Ledger.Core (Era (..), translateEraThroughCBOR)
import Cardano.Ledger.MempoolTx.Internal (MempoolTx, unwrapMempoolTx)
import Control.Monad.Except (Except)
import qualified Data.Text as T

-- | Translate a mempool transaction through its binary representation from the previous
-- era to the current one. Block transactions are not upgraded.
binaryUpgradeMempoolTx ::
  forall era.
  ( Era era
  , Era (PreviousEra era)
  , EncCBOR (MempoolTx (PreviousEra era))
  , DecCBOR (Annotator (MempoolTx era))
  ) =>
  MempoolTx (PreviousEra era) ->
  Except DecoderError (MempoolTx era)
binaryUpgradeMempoolTx = translateEraThroughCBOR @era $ "MempoolTx " <> T.pack (eraName @era) <> "Era"
