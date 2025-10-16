{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Constrained.Conway.Instances.TxBody where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.TxBody (TxBody (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.TxBody (TxBody (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.TxBody (TxBody (..))
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Mary (MaryEra, TxBody (..))
import Cardano.Ledger.Mary.Value (MultiAsset (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (Update (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Constrained.API hiding (Sized)
import Constrained.Generic
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Sequence.Strict as SS (fromList)
import Data.Set (Set)
import Data.Typeable
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger

-- ==============================================================================

instance Typeable era => HasSimpleRep (Update era)

instance EraSpecPParams era => HasSpec (Update era)

-- =========================================
-- ShelleyTxBody

-- | This is an abstraction of the Pattern ShelleyTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms bewtween the two, in the toSimpleRep
--   and fromSimpleRep methods. This makes it much easier to write Specifications, because
--   the Constrained packaage knows about Lists and Maybe.
type ShelleyTxBodyTypes =
  '[ Set TxIn
   , [TxOut ShelleyEra]
   , [TxCert ShelleyEra]
   , Map RewardAccount Coin
   , Coin
   , SlotNo
   , Maybe (Update ShelleyEra)
   , Maybe TxAuxDataHash
   ]

instance HasSimpleRep (TxBody TopTx ShelleyEra) where
  type SimpleRep (TxBody TopTx ShelleyEra) = SOP '["ShelleyTxBody" ::: ShelleyTxBodyTypes]
  toSimpleRep (ShelleyTxBody is os certs w c s up aux) =
    inject @"ShelleyTxBody" @'["ShelleyTxBody" ::: ShelleyTxBodyTypes]
      is
      (toList os)
      (toList certs)
      (unWithdrawals w)
      c
      s
      (strictMaybeToMaybe up)
      (strictMaybeToMaybe aux)

  fromSimpleRep rep =
    algebra @'["ShelleyTxBody" ::: ShelleyTxBodyTypes]
      rep
      ( \is os certs w c s up aux ->
          ShelleyTxBody
            is
            (SS.fromList os)
            (SS.fromList certs)
            (Withdrawals w)
            c
            s
            (maybeToStrictMaybe up)
            (maybeToStrictMaybe aux)
      )

instance HasSpec (TxBody TopTx ShelleyEra)

-- =======================================================
-- AllegraTxBody

-- | This is an abstraction of the Pattern AllegraTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms bewtween the two, in the toSimpleRep
--   and fromSimpleRep methods. This makes it much easier to write Specifications, because
--   the Constrained packaage knows about Lists and Maybe.
type AllegraTxBodyTypes =
  '[ Set TxIn
   , [TxOut AllegraEra]
   , [TxCert AllegraEra]
   , Map RewardAccount Coin
   , Coin
   , ValidityInterval
   , Maybe (Update AllegraEra)
   , Maybe TxAuxDataHash
   ]

instance HasSimpleRep (TxBody TopTx AllegraEra) where
  type SimpleRep (TxBody TopTx AllegraEra) = SOP '["AllegraTxBody" ::: AllegraTxBodyTypes]
  toSimpleRep (AllegraTxBody is os certs w c vi up aux) =
    inject @"AllegraTxBody" @'["AllegraTxBody" ::: AllegraTxBodyTypes]
      is
      (toList os)
      (toList certs)
      (unWithdrawals w)
      c
      vi
      (strictMaybeToMaybe up)
      (strictMaybeToMaybe aux)

  fromSimpleRep rep =
    algebra @'["AllegraTxBody" ::: AllegraTxBodyTypes]
      rep
      ( \is os certs w c vi up aux ->
          AllegraTxBody
            is
            (SS.fromList os)
            (SS.fromList certs)
            (Withdrawals w)
            c
            vi
            (maybeToStrictMaybe up)
            (maybeToStrictMaybe aux)
      )

instance HasSpec (TxBody TopTx AllegraEra)

-- =========================================================================
-- MaryTxBody

-- | This is an abstraction of the Pattern MaryTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms between the abstractions and the
--   real types in the toSimpleRep and fromSimpleRep methods. This makes it much easier to
--   write Specifications, because the Constrained packaage knows about Lists and Maybe.
type MaryTxBodyTypes =
  '[ Set TxIn
   , [TxOut MaryEra]
   , [TxCert MaryEra]
   , Map RewardAccount Coin
   , Coin
   , ValidityInterval
   , Maybe (Update MaryEra)
   , Maybe TxAuxDataHash
   , MultiAsset
   ]

instance HasSimpleRep (TxBody TopTx MaryEra) where
  type SimpleRep (TxBody TopTx MaryEra) = SOP '["MaryTxBody" ::: MaryTxBodyTypes]
  toSimpleRep (MaryTxBody is os certs w c vi up aux ma) =
    inject @"MaryTxBody" @'["MaryTxBody" ::: MaryTxBodyTypes]
      is
      (toList os)
      (toList certs)
      (unWithdrawals w)
      c
      vi
      (strictMaybeToMaybe up)
      (strictMaybeToMaybe aux)
      ma

  fromSimpleRep rep =
    algebra @'["MaryTxBody" ::: MaryTxBodyTypes]
      rep
      ( \is os certs w c vi up aux ma ->
          MaryTxBody
            is
            (SS.fromList os)
            (SS.fromList certs)
            (Withdrawals w)
            c
            vi
            (maybeToStrictMaybe up)
            (maybeToStrictMaybe aux)
            ma
      )

instance HasSpec (TxBody TopTx MaryEra)

-- =================================================================================
-- AlonzoTxBody

-- | This is an abstraction of the Pattern AlonzoTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms between the abstractions and the
--   real types in the toSimpleRep and fromSimpleRep methods. This makes it much easier to
--   write Specifications, because the Constrained packaage knows about Lists and Maybe.
type AlonzoTxBodyTypes =
  '[ Set TxIn
   , Set TxIn
   , [TxOut AlonzoEra]
   , [TxCert AlonzoEra]
   , Map RewardAccount Coin
   , Coin
   , ValidityInterval
   , Maybe (Update AlonzoEra)
   , Set (KeyHash 'Guard)
   , MultiAsset
   , Maybe ScriptIntegrityHash
   , Maybe TxAuxDataHash
   , Maybe Network
   ]

instance HasSimpleRep (TxBody TopTx AlonzoEra) where
  type SimpleRep (TxBody TopTx AlonzoEra) = SOP '["AlonzoTxBody" ::: AlonzoTxBodyTypes]
  toSimpleRep (AlonzoTxBody inputs colinputs os certs w c vi up kh ma ihash aux nw) =
    inject @"AlonzoTxBody" @'["AlonzoTxBody" ::: AlonzoTxBodyTypes]
      inputs
      colinputs
      (toList os)
      (toList certs)
      (unWithdrawals w)
      c
      vi
      (strictMaybeToMaybe up)
      kh
      ma
      (strictMaybeToMaybe ihash)
      (strictMaybeToMaybe aux)
      (strictMaybeToMaybe nw)

  fromSimpleRep rep =
    algebra @'["AlonzoTxBody" ::: AlonzoTxBodyTypes]
      rep
      ( \inputs colinputs os certs w c vi up kh ma ihash aux nw ->
          AlonzoTxBody
            inputs
            colinputs
            (SS.fromList os)
            (SS.fromList certs)
            (Withdrawals w)
            c
            vi
            (maybeToStrictMaybe up)
            kh
            ma
            (maybeToStrictMaybe ihash)
            (maybeToStrictMaybe aux)
            (maybeToStrictMaybe nw)
      )

instance HasSpec (TxBody TopTx AlonzoEra)

-- =================================================================================
-- BabbageTxBody

-- | This is an abstraction of the Pattern BabbageTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms between the abstractions and the
--   real types in the toSimpleRep and fromSimpleRep methods. This makes it much easier to
--   write Specifications, because the Constrained packaage knows about Lists and Maybe.
type BabbageTxBodyTypes =
  '[ Set TxIn
   , Set TxIn
   , Set TxIn
   , [Sized (TxOut BabbageEra)]
   , Maybe (Sized (TxOut BabbageEra))
   , Maybe Coin
   , [TxCert BabbageEra]
   , Map RewardAccount Coin -- Withdrawals without the newtype
   , Coin
   , ValidityInterval
   , Maybe (Update BabbageEra)
   , Set (KeyHash 'Guard)
   , MultiAsset
   , Maybe ScriptIntegrityHash
   , Maybe TxAuxDataHash
   , Maybe Network
   ]

instance HasSimpleRep (TxBody TopTx BabbageEra) where
  type SimpleRep (TxBody TopTx BabbageEra) = SOP '["BabbageTxBody" ::: BabbageTxBodyTypes]
  toSimpleRep (BabbageTxBody inputs colinputs refinputs os colOut coin certs w c vi up kh ma ihash aux nw) =
    inject @"BabbageTxBody" @'["BabbageTxBody" ::: BabbageTxBodyTypes]
      inputs
      colinputs
      refinputs
      (toList os)
      (strictMaybeToMaybe colOut)
      (strictMaybeToMaybe coin)
      (toList certs)
      (unWithdrawals w)
      c
      vi
      (strictMaybeToMaybe up)
      kh
      ma
      (strictMaybeToMaybe ihash)
      (strictMaybeToMaybe aux)
      (strictMaybeToMaybe nw)

  fromSimpleRep rep =
    algebra @'["BabbageTxBody" ::: BabbageTxBodyTypes]
      rep
      ( \inputs colinputs refinputs os colret totalcol certs w fee vi up kh ma ihash aux nw ->
          BabbageTxBody
            inputs
            colinputs
            refinputs
            (SS.fromList os)
            (maybeToStrictMaybe colret)
            (maybeToStrictMaybe totalcol)
            (SS.fromList certs)
            (Withdrawals w)
            fee
            vi
            (maybeToStrictMaybe up)
            kh
            ma
            (maybeToStrictMaybe ihash)
            (maybeToStrictMaybe aux)
            (maybeToStrictMaybe nw)
      )

instance HasSpec (TxBody TopTx BabbageEra)
