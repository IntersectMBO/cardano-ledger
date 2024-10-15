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

import Cardano.Ledger.Address (RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Core (AllegraEraTxBody (..))
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody (..), ValidityInterval (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..), AlonzoTxBody (..), ScriptIntegrityHash)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxBody (..), BabbageTxBody (..))
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Binary (EncCBOR (..), Sized (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Core (MaryEraTxBody (..))
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..))
import Cardano.Ledger.Mary.Value (MultiAsset (..))
import Cardano.Ledger.Shelley.PParams (Update (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Constrained hiding (Value)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Sequence.Strict as SS (fromList)
import Data.Set (Set)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger

-- ==============================================================================

instance HasSimpleRep (Update era)
instance (EraSpecPParams era, IsConwayUniv fn) => HasSpec fn (Update era)

-- =========================================
-- ShelleyTxBody

-- | This is an abstraction of the Pattern ShelleyTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms bewtween the two, in the toSimpleRep
--   and fromSimpleRep methods. This makes it much easier to write Specifications, because
--   the Constrained packaage knows about Lists and Maybe.
type ShelleyTxBodyTypes era =
  '[ Set (TxIn (EraCrypto era))
   , [TxOut era]
   , [TxCert era]
   , Map (RewardAccount (EraCrypto era)) Coin
   , Coin
   , SlotNo
   , Maybe (Update era)
   , Maybe (AuxiliaryDataHash (EraCrypto era))
   ]

instance
  ( EraTxOut era
  , EncCBOR (TxCert era)
  ) =>
  HasSimpleRep (ShelleyTxBody era)
  where
  type SimpleRep (ShelleyTxBody era) = SOP '["ShelleyTxBody" ::: ShelleyTxBodyTypes era]
  toSimpleRep (ShelleyTxBody is os certs w c s up aux) =
    inject @"ShelleyTxBody" @'["ShelleyTxBody" ::: ShelleyTxBodyTypes era]
      is
      (toList os)
      (toList certs)
      (unWithdrawals w)
      c
      s
      (strictMaybeToMaybe up)
      (strictMaybeToMaybe aux)

  fromSimpleRep rep =
    algebra @'["ShelleyTxBody" ::: ShelleyTxBodyTypes era]
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

instance
  ( EraSpecPParams era
  , IsConwayUniv fn
  , HasSpec fn (TxOut era)
  , HasSpec fn (TxCert era)
  ) =>
  HasSpec fn (ShelleyTxBody era)

fromShelleyBody :: forall era. EraTxBody era => ShelleyTxBody era -> TxBody era
fromShelleyBody (ShelleyTxBody inputs outputs certs withdrawals coin _slot _up aux) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
    & outputsTxBodyL @era .~ outputs
    & feeTxBodyL @era .~ coin
    & withdrawalsTxBodyL @era .~ withdrawals
    & certsTxBodyL @era .~ certs
    & auxDataHashTxBodyL @era .~ aux

-- =======================================================
-- AllegraTxBody

-- | This is an abstraction of the Pattern AllegraTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms bewtween the two, in the toSimpleRep
--   and fromSimpleRep methods. This makes it much easier to write Specifications, because
--   the Constrained packaage knows about Lists and Maybe.
type AllegraTxBodyTypes era =
  '[ Set (TxIn (EraCrypto era))
   , [TxOut era]
   , [TxCert era]
   , Map (RewardAccount (EraCrypto era)) Coin
   , Coin
   , ValidityInterval
   , Maybe (Update era)
   , Maybe (AuxiliaryDataHash (EraCrypto era))
   ]

instance
  ( EraTxOut era
  , EraTxCert era
  ) =>
  HasSimpleRep (AllegraTxBody era)
  where
  type SimpleRep (AllegraTxBody era) = SOP '["AllegraTxBody" ::: AllegraTxBodyTypes era]
  toSimpleRep (AllegraTxBody is os certs w c vi up aux) =
    inject @"AllegraTxBody" @'["AllegraTxBody" ::: AllegraTxBodyTypes era]
      is
      (toList os)
      (toList certs)
      (unWithdrawals w)
      c
      vi
      (strictMaybeToMaybe up)
      (strictMaybeToMaybe aux)

  fromSimpleRep rep =
    algebra @'["AllegraTxBody" ::: AllegraTxBodyTypes era]
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
instance
  ( EraSpecPParams era
  , IsConwayUniv fn
  , HasSpec fn (TxOut era)
  , HasSpec fn (TxCert era)
  ) =>
  HasSpec fn (AllegraTxBody era)

fromAllegraBody :: forall era. AllegraEraTxBody era => AllegraTxBody era -> TxBody era
fromAllegraBody (AllegraTxBody inputs outputs certs withdrawals coin vi _up aux) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
    & outputsTxBodyL @era .~ outputs
    & feeTxBodyL @era .~ coin
    & withdrawalsTxBodyL @era .~ withdrawals
    & certsTxBodyL @era .~ certs
    & auxDataHashTxBodyL @era .~ aux
    & vldtTxBodyL @era .~ vi

-- =========================================================================
-- MaryTxBody

-- | This is an abstraction of the Pattern MaryTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms between the abstractions and the
--   real types in the toSimpleRep and fromSimpleRep methods. This makes it much easier to
--   write Specifications, because the Constrained packaage knows about Lists and Maybe.
type MaryTxBodyTypes era =
  '[ Set (TxIn (EraCrypto era))
   , [TxOut era]
   , [TxCert era]
   , Map (RewardAccount (EraCrypto era)) Coin
   , Coin
   , ValidityInterval
   , Maybe (Update era)
   , Maybe (AuxiliaryDataHash (EraCrypto era))
   , MultiAsset (EraCrypto era)
   ]

instance
  ( EraTxOut era
  , EraTxCert era
  ) =>
  HasSimpleRep (MaryTxBody era)
  where
  type SimpleRep (MaryTxBody era) = SOP '["MaryTxBody" ::: MaryTxBodyTypes era]
  toSimpleRep (MaryTxBody is os certs w c vi up aux ma) =
    inject @"MaryTxBody" @'["MaryTxBody" ::: MaryTxBodyTypes era]
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
    algebra @'["MaryTxBody" ::: MaryTxBodyTypes era]
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
instance
  ( EraSpecPParams era
  , IsConwayUniv fn
  , HasSpec fn (TxOut era)
  , HasSpec fn (TxCert era)
  ) =>
  HasSpec fn (MaryTxBody era)

fromMaryBody :: forall era. MaryEraTxBody era => MaryTxBody era -> TxBody era
fromMaryBody (MaryTxBody inputs outputs certs withdrawals coin vi _up aux ma) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
    & outputsTxBodyL @era .~ outputs
    & feeTxBodyL @era .~ coin
    & withdrawalsTxBodyL @era .~ withdrawals
    & certsTxBodyL @era .~ certs
    & auxDataHashTxBodyL @era .~ aux
    & vldtTxBodyL @era .~ vi
    & mintTxBodyL @era .~ ma

-- =================================================================================
-- AlonzoTxBody

-- | This is an abstraction of the Pattern AlonzoTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms between the abstractions and the
--   real types in the toSimpleRep and fromSimpleRep methods. This makes it much easier to
--   write Specifications, because the Constrained packaage knows about Lists and Maybe.
type AlonzoTxBodyTypes era =
  '[ Set (TxIn (EraCrypto era))
   , Set (TxIn (EraCrypto era))
   , [TxOut era]
   , [TxCert era]
   , Map (RewardAccount (EraCrypto era)) Coin
   , Coin
   , ValidityInterval
   , Maybe (Update era)
   , Set (KeyHash 'Witness (EraCrypto era))
   , MultiAsset (EraCrypto era)
   , Maybe (ScriptIntegrityHash (EraCrypto era))
   , Maybe (AuxiliaryDataHash (EraCrypto era))
   , Maybe Network
   ]

instance
  ( EraTxOut era
  , EraTxCert era
  ) =>
  HasSimpleRep (AlonzoTxBody era)
  where
  type SimpleRep (AlonzoTxBody era) = SOP '["AlonzoTxBody" ::: AlonzoTxBodyTypes era]
  toSimpleRep (AlonzoTxBody inputs colinputs os certs w c vi up kh ma ihash aux nw) =
    inject @"AlonzoTxBody" @'["AlonzoTxBody" ::: AlonzoTxBodyTypes era]
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
    algebra @'["AlonzoTxBody" ::: AlonzoTxBodyTypes era]
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

instance
  ( EraSpecPParams era
  , IsConwayUniv fn
  , HasSpec fn (TxOut era)
  , HasSpec fn (TxCert era)
  ) =>
  HasSpec fn (AlonzoTxBody era)

fromAlonzoBody :: forall era. AlonzoEraTxBody era => AlonzoTxBody era -> TxBody era
fromAlonzoBody (AlonzoTxBody colinputs inputs outputs certs withdrawals coin vi _up kh ma ihash aux nw) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
    & collateralInputsTxBodyL @era .~ colinputs
    & outputsTxBodyL @era .~ outputs
    & feeTxBodyL @era .~ coin
    & withdrawalsTxBodyL @era .~ withdrawals
    & certsTxBodyL @era .~ certs
    & auxDataHashTxBodyL @era .~ aux
    & vldtTxBodyL @era .~ vi
    & mintTxBodyL @era .~ ma
    & collateralInputsTxBodyL @era .~ colinputs
    & reqSignerHashesTxBodyL @era .~ kh
    & scriptIntegrityHashTxBodyL @era .~ ihash
    & networkIdTxBodyL @era .~ nw

-- =================================================================================
-- BabbageTxBody

-- | This is an abstraction of the Pattern BabbageTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms between the abstractions and the
--   real types in the toSimpleRep and fromSimpleRep methods. This makes it much easier to
--   write Specifications, because the Constrained packaage knows about Lists and Maybe.
type BabbageTxBodyTypes era =
  '[ Set (TxIn (EraCrypto era))
   , Set (TxIn (EraCrypto era))
   , Set (TxIn (EraCrypto era))
   , [Sized (TxOut era)]
   , Maybe (Sized (TxOut era))
   , Maybe Coin
   , [TxCert era]
   , Map (RewardAccount (EraCrypto era)) Coin -- Withdrawals without the newtype
   , Coin
   , ValidityInterval
   , Maybe (Update era)
   , Set (KeyHash 'Witness (EraCrypto era))
   , MultiAsset (EraCrypto era)
   , Maybe (ScriptIntegrityHash (EraCrypto era))
   , Maybe (AuxiliaryDataHash (EraCrypto era))
   , Maybe Network
   ]

instance
  (EraTxOut era, EraTxCert era, BabbageEraTxBody era) =>
  HasSimpleRep (BabbageTxBody era)
  where
  type SimpleRep (BabbageTxBody era) = SOP '["BabbageTxBody" ::: BabbageTxBodyTypes era]
  toSimpleRep (BabbageTxBody inputs colinputs refinputs os colOut coin certs w c vi up kh ma ihash aux nw) =
    inject @"BabbageTxBody" @'["BabbageTxBody" ::: BabbageTxBodyTypes era]
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
    algebra @'["BabbageTxBody" ::: BabbageTxBodyTypes era]
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

instance
  ( EraSpecPParams era
  , BabbageEraTxBody era
  , IsConwayUniv fn
  , HasSpec fn (TxOut era)
  , HasSpec fn (TxCert era)
  ) =>
  HasSpec fn (BabbageTxBody era)

fromBabbageBody :: forall era. BabbageEraTxBody era => BabbageTxBody era -> TxBody era
fromBabbageBody (BabbageTxBody inputs colinputs refinputs os colret totalcol certs w fee vi _up kh ma ihash aux nw) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
    & collateralInputsTxBodyL @era .~ colinputs
    & referenceInputsTxBodyL @era .~ refinputs
    & sizedOutputsTxBodyL @era .~ os
    & sizedCollateralReturnTxBodyL @era .~ colret
    & totalCollateralTxBodyL @era .~ totalcol
    & certsTxBodyL @era .~ certs
    & withdrawalsTxBodyL @era .~ w
    & feeTxBodyL @era .~ fee
    & vldtTxBodyL @era .~ vi
    & reqSignerHashesTxBodyL @era .~ kh
    & mintTxBodyL @era .~ ma
    & scriptIntegrityHashTxBodyL @era .~ ihash
    & auxDataHashTxBodyL @era .~ aux
    & networkIdTxBodyL @era .~ nw
