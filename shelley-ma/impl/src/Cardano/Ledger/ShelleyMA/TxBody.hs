{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.ShelleyMA.TxBody
  ( TxBody (TxBody, STxBody),
    TxBody' (..),
  )
where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Compactible (CompactForm (..), Compactible (..))
import Cardano.Ledger.Core (Script, Value)
<<<<<<< HEAD
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.ShelleyMA (MaryOrAllegra, ShelleyMAEra)
=======
import Cardano.Ledger.Era (Era)
>>>>>>> 622da3ac... "Added the TxBody type with validity intervals and forge fields. Tied this
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..), decodeVI, encodeVI)
import Data.Coders
  ( Decode (..),
    Encode (..),
    decode,
    decodeStrictSeq,
    (!>),
    (<!),
  )
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe)
import Shelley.Spec.Ledger.Coin (Coin (..))
<<<<<<< HEAD
import Shelley.Spec.Ledger.Hashing (EraIndependentTxBody, HashAnnotated (..))
=======
>>>>>>> 622da3ac... "Added the TxBody type with validity intervals and forge fields. Tied this
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.Serialization (encodeFoldable)
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )

-- =====================================================
-- TxBody has two Era dependent type families
-- (Value era) and (Script era) (hidden in DCert) in
-- order to make CBOR instances of things we are going to
-- have to assume some properties about these.

type FamsFrom era =
  ( Era era,
    Typeable era,
    Typeable (Script era),
    FromCBOR (CompactForm (Value era)), -- Arises because TxOut uses Compact form
    FromCBOR (Value era),
    FromCBOR (Annotator (Script era)), -- Arises becaause DCert memoizes its bytes
    FromCBOR (Script era)
  )

type FamsTo era =
  ( Era era,
    ToCBOR (Value era),
    ToCBOR (CompactForm (Value era)), -- Arises because TxOut uses Compact form
    ToCBOR (Script era)
  )

-- =======================================================

data TxBody' era = TxBody'
  { inputs :: !(Set (TxIn era)),
    outputs :: !(StrictSeq (TxOut era)),
    dcerts :: !(StrictSeq (DCert era)),
    wdrls :: !(Wdrl era),
    txfee :: !Coin,
    vldt :: !ValidityInterval, -- imported from Timelocks
    txupdate :: !(StrictMaybe (Update era)),
    mdhash :: !(StrictMaybe (MetaDataHash era)),
    forge :: !(Value era)
  }
  deriving (Typeable)

-- For each instance we try and use the weakest constraint possible
-- The surprising (Compactible (Value era))) constraint comes from the fact that TxOut
-- stores a (Value era) in a compactible form.

deriving instance (Compactible (Value era), Eq (Value era)) => Eq (TxBody' era)

deriving instance (Era era, Compactible (Value era), Show (Value era)) => Show (TxBody' era)


deriving instance Generic (TxBody' era)

deriving instance NoThunks (Value era) => NoThunks (TxBody' era)

instance
  (FamsFrom era) =>
  FromCBOR (TxBody' era)
  where
  fromCBOR =
    decode $
      RecD TxBody'
        <! From
        <! D (decodeStrictSeq fromCBOR)
        <! D (decodeStrictSeq fromCBOR)
        <! From
        <! From
        <! decodeVI -- CBOR Group decoding
        <! From
        <! From
        <! From

instance
  (FamsFrom era) =>
  FromCBOR (Annotator (TxBody' era))
  where
  fromCBOR = pure <$> fromCBOR

-- ===========================================================================
-- Wrap it all up in a newtype, hiding the insides with a pattern construtor.

newtype TxBody e = STxBody (MemoBytes (TxBody' e))
  deriving (Typeable)

deriving instance (Compactible (Value era), Eq (Value era)) => Eq (TxBody era)

deriving instance (Era era, Compactible (Value era), Show (Value era)) => Show (TxBody era)

deriving instance Generic (TxBody era)

deriving newtype instance (Typeable era, NoThunks (Value era)) => NoThunks (TxBody era)

deriving newtype instance (Typeable era) => ToCBOR (TxBody era)

deriving via
  (Mem (TxBody' era))
  instance
    (FamsFrom era) =>
    FromCBOR (Annotator (TxBody era))

instance Era era => HashAnnotated (TxBody era) era where
  type HashIndex (TxBody era) = EraIndependentTxBody

-- Make a Pattern so the newtype and the MemoBytes are hidden

pattern TxBody ::
  (FamsTo era) =>
  (Set (TxIn era)) ->
  (StrictSeq (TxOut era)) ->
  (StrictSeq (DCert era)) ->
  (Wdrl era) ->
  Coin ->
  ValidityInterval ->
  (StrictMaybe (Update era)) ->
  (StrictMaybe (MetaDataHash era)) ->
  (Value era) ->
  TxBody era
pattern TxBody i o d w fee vi u m forge <-
  STxBody (Memo (TxBody' i o d w fee vi u m forge) _)
  where
    TxBody i o d w fee vi u m forge =
      STxBody $
        memoBytes $
          Rec TxBody'
            !> To i
            !> E encodeFoldable o
            !> E encodeFoldable d
            !> To w
            !> To fee
            !> (encodeVI vi) -- CBOR Group encoding
            !> To u
            !> To m
            !> To forge

{-# COMPLETE TxBody #-}

-- ==================================================================
-- Promote the fields of TxBody' to be fields of TxBody. Either
-- automatically or by hand. Both methods have drawbacks.

{-
instance HasField tag (TxBody' e) c => HasField (tag::Symbol) (TxBody e) c where
   getField (STxBody (Memo x _)) = getField @tag x

-- The method above autmatically lifts the Hasfield instances from TxBody' to TxBody
-- the problem is, if some other file imports this file, it needs to import both
-- the hidden type TxBody' and its constructors like this
-- import Cardano.Ledger.ShelleyMA.TxBody(TxBody'(..))     OR
-- import qualified Cardano.Ledger.ShelleyMA.TxBody as XXX
-- Both are very ugly, but at least in the second way, one doesn't need to know the name of TxBody'
-- So instead we tediously write by hand explicit HasField instances for TxBody
-}

instance HasField "inputs" (TxBody e) (Set (TxIn e)) where
  getField (STxBody (Memo m _)) = getField @"inputs" m

instance HasField "outputs" (TxBody e) (StrictSeq (TxOut e)) where
  getField (STxBody (Memo m _)) = getField @"outputs" m

instance HasField "dcerts" (TxBody e) (StrictSeq (DCert e)) where
  getField (STxBody (Memo m _)) = getField @"dcerts" m

instance HasField "wdrls" (TxBody e) (Wdrl e) where
  getField (STxBody (Memo m _)) = getField @"wdrls" m

instance HasField "txfee" (TxBody e) Coin where
  getField (STxBody (Memo m _)) = getField @"txfee" m

instance HasField "vldt" (TxBody e) ValidityInterval where
  getField (STxBody (Memo m _)) = getField @"vldt" m

instance HasField "txupdate" (TxBody e) (StrictMaybe (Update e)) where
  getField (STxBody (Memo m _)) = getField @"txupdate" m

instance HasField "mdhash" (TxBody e) (StrictMaybe (MetaDataHash e)) where
  getField (STxBody (Memo m _)) = getField @"mdhash" m

instance (Value e ~ vv) => HasField "forge" (TxBody e) vv where
  getField (STxBody (Memo m _)) = getField @"forge" m
