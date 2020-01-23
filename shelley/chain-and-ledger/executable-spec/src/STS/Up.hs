{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Up
  ( UP
  , UpdateEnv(..)
  )
where

import           BaseTypes
import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeListLen, decodeWord,
                     encodeListLen, matchSize)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.State.Transition
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Keys
import           PParams
import           Slot
import           STS.Avup
import           STS.Ppup
import           Updates (Update (..), UpdateState (..))

data UP crypto

data UpdateEnv crypto
  = UpdateEnv SlotNo PParams (GenDelegs crypto)

instance Crypto crypto => STS (UP crypto) where
  type State (UP crypto) = UpdateState crypto
  type Signal (UP crypto) = Update crypto
  type Environment (UP crypto) = UpdateEnv crypto
  type BaseM (UP crypto) = ShelleyBase
  data PredicateFailure (UP crypto)
    = NonGenesisUpdateUP
    | AvupFailure (PredicateFailure (AVUP crypto))
    | PpupFailure (PredicateFailure (PPUP crypto))
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [upTransition]

instance NoUnexpectedThunks (PredicateFailure (UP crypto))

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (PredicateFailure (UP crypto))
 where
   toCBOR = \case
     NonGenesisUpdateUP -> encodeListLen 1 <> toCBOR (0 :: Word8)
     (AvupFailure a)    -> encodeListLen 2 <> toCBOR (1 :: Word8)
                             <> toCBOR a
     (PpupFailure a)    -> encodeListLen 2 <> toCBOR (2 :: Word8)
                             <> toCBOR a

instance
  (Crypto crypto)
  => FromCBOR (PredicateFailure (UP crypto))
 where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> matchSize "NonGenesisUpdateUP" 1 n >> pure NonGenesisUpdateUP
      1 -> do
        matchSize "AvupFailure" 2 n
        a <- fromCBOR
        pure $ AvupFailure a
      2 -> do
        matchSize "PpupFailure" 2 n
        a <- fromCBOR
        pure $ PpupFailure a
      k -> invalidKey k

upTransition
  :: forall crypto
   . Crypto crypto
  => TransitionRule (UP crypto)
upTransition = do
  TRC ( UpdateEnv slot pp _genDelegs
      , UpdateState pup aup favs avs
      , Update pupU aupU e) <- judgmentContext

  pup' <- trans @(PPUP crypto) $ TRC (PPUPEnv slot pp _genDelegs, pup, (pupU, e))
  AVUPState aup' favs' avs' <-
    trans @(AVUP crypto) $ TRC (AVUPEnv slot _genDelegs, AVUPState aup favs avs, aupU)

  pure $ UpdateState pup' aup' favs' avs'

instance Crypto crypto => Embed (AVUP crypto) (UP crypto) where
  wrapFailed = AvupFailure

instance Crypto crypto => Embed (PPUP crypto) (UP crypto) where
  wrapFailed = PpupFailure
