module Cardano.Ledger.Babbage.Core
  ( BabbageEraTxBody (..),
    module Cardano.Ledger.Alonzo.Core,
  )
where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut, BabbageTxOut)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Serialization (Sized (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Lens.Micro (Lens', SimpleGetter)

class (AlonzoEraTxBody era, BabbageEraTxOut era) => BabbageEraTxBody era where
  sizedOutputsTxBodyL :: Lens' (TxBody era) (StrictSeq (Sized (BabbageTxOut era)))

  referenceInputsTxBodyL :: Lens' (TxBody era) (Set (TxIn (EraCrypto era)))

  totalCollateralTxBodyL :: Lens' (TxBody era) (StrictMaybe Coin)

  collateralReturnTxBodyL :: Lens' (TxBody era) (StrictMaybe (BabbageTxOut era))

  sizedCollateralReturnTxBodyL :: Lens' (TxBody era) (StrictMaybe (Sized (BabbageTxOut era)))

  allSizedOutputsTxBodyF :: SimpleGetter (TxBody era) (StrictSeq (Sized (BabbageTxOut era)))
