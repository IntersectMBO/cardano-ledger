{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Ledger.Shelley.Core (
  ShelleyEraTxBody (..),
  Withdrawals (..),
  EraTallyState (..),
  ShelleyTallyState (..),
  Wdrl,
  module Cardano.Ledger.Core,
  pattern Wdrl,
)
where

import Cardano.Ledger.Address
import Cardano.Ledger.Binary.Plain (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  decodeNull,
  encodeNull,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto ( Crypto )
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Default.Class (Default (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import GHC.Generics (Generic)
import Lens.Micro (Lens', SimpleGetter)
import NoThunks.Class (NoThunks)

class
  ( Era era
  , NoThunks (TallyState era)
  , NFData (TallyState era)
  , EncCBOR (TallyState era)
  , DecCBOR (TallyState era)
  ) =>
  EraTallyState era
  where
  type TallyState era = (r :: Type) | r -> era
  type TallyState era = ShelleyTallyState era

  emptyTallyState :: TallyState era
  default emptyTallyState :: Default (TallyState era) => TallyState era
  emptyTallyState = def

data ShelleyTallyState era = NoTallyState
  deriving (Show, Generic, Eq)

instance NFData (ShelleyTallyState era)

instance NoThunks (ShelleyTallyState era)

instance Default (ShelleyTallyState era) where
  def = NoTallyState

instance ToExpr (ShelleyTallyState era)

instance Era era => EncCBOR (ShelleyTallyState era) where
  encCBOR _ = encodeNull

instance Era era => DecShareCBOR (ShelleyTallyState era) where
  decShareCBOR _ = decCBOR
  decSharePlusCBOR = lift decCBOR

instance Era era => DecCBOR (ShelleyTallyState era) where
  decCBOR = NoTallyState <$ decodeNull

instance Crypto c => EraTallyState (ShelleyEra c)

class EraTxBody era => ShelleyEraTxBody era where
  ttlTxBodyL :: ExactEra ShelleyEra era => Lens' (TxBody era) SlotNo

  updateTxBodyL :: ProtVerAtMost era 8 => Lens' (TxBody era) (StrictMaybe (Update era))

  updateTxBodyG :: SimpleGetter (TxBody era) (StrictMaybe (Update era))
  default updateTxBodyG :: ProtVerAtMost era 8 => SimpleGetter (TxBody era) (StrictMaybe (Update era))
  updateTxBodyG = updateTxBodyL

  certsTxBodyL :: ProtVerAtMost era 8 => Lens' (TxBody era) (StrictSeq (DCert (EraCrypto era)))

  certsTxBodyG :: SimpleGetter (TxBody era) (StrictSeq (DCert (EraCrypto era)))
  default certsTxBodyG :: ProtVerAtMost era 8 => SimpleGetter (TxBody era) (StrictSeq (DCert (EraCrypto era)))
  certsTxBodyG = certsTxBodyL

type Wdrl c = Withdrawals c
{-# DEPRECATED Wdrl "In favor of `Cardano.Ledger.Address.Withdrawals`" #-}
pattern Wdrl :: Map (RewardAcnt c) Coin -> Withdrawals c
pattern Wdrl ws = Withdrawals ws
