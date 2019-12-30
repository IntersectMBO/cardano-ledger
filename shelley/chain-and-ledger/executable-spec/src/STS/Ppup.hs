{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Ppup
  ( PPUP
  , PPUPEnv(..)
  )
where

import           BaseTypes
import           BlockChain
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Trans.Reader (asks)
import           Control.State.Transition
import           Data.Ix (inRange)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           GHC.Generics (Generic)
import           Keys
import           Ledger.Core (dom, (⊆), (⨃))
import           Numeric.Natural (Natural)
import           PParams
import           Slot
import           Updates

data PPUP crypto

data PPUPEnv crypto
  = PPUPEnv SlotNo PParams (GenDelegs crypto)

instance STS (PPUP crypto) where
  type State (PPUP crypto) = PPUpdate crypto
  type Signal (PPUP crypto) = PPUpdate crypto
  type Environment (PPUP crypto) = PPUPEnv crypto
  type BaseM (PPUP crypto) = ShelleyBase
  data PredicateFailure (PPUP crypto)
    = NonGenesisUpdatePPUP (Set (GenKeyHash crypto)) (Set (GenKeyHash crypto))
    | PPUpdateTooLatePPUP
    | PPUpdateEmpty
    | PPUpdateNonEmpty
    | PVCannotFollowPPUP
    deriving (Show, Eq, Generic)

  initialRules = []

  transitionRules = [ppupTransitionEmpty, ppupTransitionNonEmpty]

instance NoUnexpectedThunks (PredicateFailure (PPUP crypto))

pvCanFollow :: (Natural, Natural, Natural) -> Ppm -> Bool
pvCanFollow (mjp, mip, ap) (ProtocolVersion (mjn, mn, an))
  = (mjp, mip, ap) < (mjn, mn, an)
  && inRange (0,1) (mjn - mjp)
  && ((mjp == mjn) ==> (mip + 1 == mn))
  && ((mjp + 1 == mjn) ==> (mn == 0))
pvCanFollow _ _ = True

ppupTransitionEmpty :: TransitionRule (PPUP crypto)
ppupTransitionEmpty = do
  TRC (_, pupS, PPUpdate pup) <- judgmentContext

  Map.null pup ?! PPUpdateNonEmpty

  pure pupS

ppupTransitionNonEmpty :: TransitionRule (PPUP crypto)
ppupTransitionNonEmpty = do
  TRC (PPUPEnv slot pp (GenDelegs _genDelegs), PPUpdate pupS, PPUpdate pup) <- judgmentContext

  not (Map.null pup) ?! PPUpdateEmpty

  (dom pup ⊆ dom _genDelegs) ?! NonGenesisUpdatePPUP (dom pup) (dom _genDelegs)

  all (all (pvCanFollow (_protocolVersion pp)) . ppmSet) pup ?! PVCannotFollowPPUP

  firstSlotNextEpoch <- liftSTS $ do
    ei <- asks epochInfo
    EpochNo e <- epochInfoEpoch ei slot
    epochInfoFirst ei (EpochNo $ e + 1)
  slot < firstSlotNextEpoch *- slotsPrior ?! PPUpdateTooLatePPUP

  pure $ PPUpdate (pupS ⨃  Map.toList pup)
