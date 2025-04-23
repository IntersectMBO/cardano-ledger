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
import Constrained.API
import Constrained.Generic
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Sequence.Strict as SS (fromList)
import Data.Set (Set)
import Data.Typeable
import Lens.Micro
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
type ShelleyTxBodyTypes era =
  '[ Set TxIn
   , [TxOut era]
   , [TxCert era]
   , Map RewardAccount Coin
   , Coin
   , SlotNo
   , Maybe (Update era)
   , Maybe TxAuxDataHash
   ]

instance HasSimpleRep (TxBody ShelleyEra) where
  type SimpleRep (TxBody ShelleyEra) = SOP '["ShelleyTxBody" ::: ShelleyTxBodyTypes ShelleyEra]
  toSimpleRep (ShelleyTxBody is os certs w c s up aux) =
    inject @"ShelleyTxBody" @'["ShelleyTxBody" ::: ShelleyTxBodyTypes ShelleyEra]
      is
      (toList os)
      (toList certs)
      (unWithdrawals w)
      c
      s
      (strictMaybeToMaybe up)
      (strictMaybeToMaybe aux)

  fromSimpleRep rep =
    algebra @'["ShelleyTxBody" ::: ShelleyTxBodyTypes ShelleyEra]
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

instance HasSpec (TxBody ShelleyEra)

fromShelleyBody :: forall era. EraTxBody era => TxBody ShelleyEra -> TxBody era
fromShelleyBody (ShelleyTxBody inputs outputs certs withdrawals coin _slot _up aux) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
    & outputsTxBodyL @era .~ undefined outputs
    & feeTxBodyL @era .~ coin
    & withdrawalsTxBodyL @era .~ withdrawals
    & certsTxBodyL @era .~ undefined certs
    & auxDataHashTxBodyL @era .~ aux

-- =======================================================
-- AllegraTxBody

-- | This is an abstraction of the Pattern AllegraTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms bewtween the two, in the toSimpleRep
--   and fromSimpleRep methods. This makes it much easier to write Specifications, because
--   the Constrained packaage knows about Lists and Maybe.
type AllegraTxBodyTypes era =
  '[ Set TxIn
   , [TxOut era]
   , [TxCert era]
   , Map RewardAccount Coin
   , Coin
   , ValidityInterval
   , Maybe (Update era)
   , Maybe TxAuxDataHash
   ]

instance HasSimpleRep (TxBody AllegraEra) where
  type SimpleRep (TxBody AllegraEra) = SOP '["AllegraTxBody" ::: AllegraTxBodyTypes AllegraEra]
  toSimpleRep (AllegraTxBody is os certs w c vi up aux) =
    inject @"AllegraTxBody" @'["AllegraTxBody" ::: AllegraTxBodyTypes AllegraEra]
      is
      (toList os)
      (toList certs)
      (unWithdrawals w)
      c
      vi
      (strictMaybeToMaybe up)
      (strictMaybeToMaybe aux)

  fromSimpleRep rep =
    algebra @'["AllegraTxBody" ::: AllegraTxBodyTypes AllegraEra]
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
instance HasSpec (TxBody AllegraEra)

fromAllegraBody :: forall era. AllegraEraTxBody era => TxBody AllegraEra -> TxBody era
fromAllegraBody (AllegraTxBody inputs outputs certs withdrawals coin vi _up aux) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
    & outputsTxBodyL @era .~ undefined outputs
    & feeTxBodyL @era .~ coin
    & withdrawalsTxBodyL @era .~ withdrawals
    & certsTxBodyL @era .~ undefined certs
    & auxDataHashTxBodyL @era .~ aux
    & vldtTxBodyL @era .~ vi

-- =========================================================================
-- MaryTxBody

-- | This is an abstraction of the Pattern MaryTxBody, that uses [x] instead of (StrictSeq x)
--   and (Maybe x) instead of (StrictMaybe x). It transforms between the abstractions and the
--   real types in the toSimpleRep and fromSimpleRep methods. This makes it much easier to
--   write Specifications, because the Constrained packaage knows about Lists and Maybe.
type MaryTxBodyTypes era =
  '[ Set TxIn
   , [TxOut era]
   , [TxCert era]
   , Map RewardAccount Coin
   , Coin
   , ValidityInterval
   , Maybe (Update era)
   , Maybe TxAuxDataHash
   , MultiAsset
   ]

instance HasSimpleRep (TxBody MaryEra) where
  type SimpleRep (TxBody MaryEra) = SOP '["MaryTxBody" ::: MaryTxBodyTypes MaryEra]
  toSimpleRep (MaryTxBody is os certs w c vi up aux ma) =
    inject @"MaryTxBody" @'["MaryTxBody" ::: MaryTxBodyTypes MaryEra]
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
    algebra @'["MaryTxBody" ::: MaryTxBodyTypes MaryEra]
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
instance HasSpec (TxBody MaryEra)

fromMaryBody :: forall era. MaryEraTxBody era => TxBody MaryEra -> TxBody era
fromMaryBody (MaryTxBody inputs outputs certs withdrawals coin vi _up aux ma) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
    & outputsTxBodyL @era .~ undefined outputs
    & feeTxBodyL @era .~ coin
    & withdrawalsTxBodyL @era .~ withdrawals
    & certsTxBodyL @era .~ undefined certs
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
  '[ Set TxIn
   , Set TxIn
   , [TxOut era]
   , [TxCert era]
   , Map RewardAccount Coin
   , Coin
   , ValidityInterval
   , Maybe (Update era)
   , Set (KeyHash 'Witness)
   , MultiAsset
   , Maybe ScriptIntegrityHash
   , Maybe TxAuxDataHash
   , Maybe Network
   ]

instance HasSimpleRep (TxBody AlonzoEra) where
  type SimpleRep (TxBody AlonzoEra) = SOP '["AlonzoTxBody" ::: AlonzoTxBodyTypes AlonzoEra]
  toSimpleRep (AlonzoTxBody inputs colinputs os certs w c vi up kh ma ihash aux nw) =
    inject @"AlonzoTxBody" @'["AlonzoTxBody" ::: AlonzoTxBodyTypes AlonzoEra]
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
    algebra @'["AlonzoTxBody" ::: AlonzoTxBodyTypes AlonzoEra]
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

instance HasSpec (TxBody AlonzoEra)

fromAlonzoBody :: forall era. AlonzoEraTxBody era => TxBody AlonzoEra -> TxBody era
fromAlonzoBody (AlonzoTxBody colinputs inputs outputs certs withdrawals coin vi _up kh ma ihash aux nw) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
    & collateralInputsTxBodyL @era .~ colinputs
    & outputsTxBodyL @era .~ undefined outputs
    & feeTxBodyL @era .~ coin
    & withdrawalsTxBodyL @era .~ withdrawals
    & certsTxBodyL @era .~ undefined certs
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
  '[ Set TxIn
   , Set TxIn
   , Set TxIn
   , [Sized (TxOut era)]
   , Maybe (Sized (TxOut era))
   , Maybe Coin
   , [TxCert era]
   , Map RewardAccount Coin -- Withdrawals without the newtype
   , Coin
   , ValidityInterval
   , Maybe (Update era)
   , Set (KeyHash 'Witness)
   , MultiAsset
   , Maybe ScriptIntegrityHash
   , Maybe TxAuxDataHash
   , Maybe Network
   ]

instance HasSimpleRep (TxBody BabbageEra) where
  type SimpleRep (TxBody BabbageEra) = SOP '["BabbageTxBody" ::: BabbageTxBodyTypes BabbageEra]
  toSimpleRep (BabbageTxBody inputs colinputs refinputs os colOut coin certs w c vi up kh ma ihash aux nw) =
    inject @"BabbageTxBody" @'["BabbageTxBody" ::: BabbageTxBodyTypes BabbageEra]
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
    algebra @'["BabbageTxBody" ::: BabbageTxBodyTypes BabbageEra]
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

instance HasSpec (TxBody BabbageEra)

fromBabbageBody :: forall era. BabbageEraTxBody era => TxBody BabbageEra -> TxBody era
fromBabbageBody (BabbageTxBody inputs colinputs refinputs os colret totalcol certs w fee vi _up kh ma ihash aux nw) =
  mkBasicTxBody @era
    & inputsTxBodyL @era .~ inputs
    & collateralInputsTxBodyL @era .~ colinputs
    & referenceInputsTxBodyL @era .~ refinputs
    & sizedOutputsTxBodyL @era .~ undefined os
    & sizedCollateralReturnTxBodyL @era .~ undefined colret
    & totalCollateralTxBodyL @era .~ totalcol
    & certsTxBodyL @era .~ undefined certs
    & withdrawalsTxBodyL @era .~ w
    & feeTxBodyL @era .~ fee
    & vldtTxBodyL @era .~ vi
    & reqSignerHashesTxBodyL @era .~ kh
    & mintTxBodyL @era .~ ma
    & scriptIntegrityHashTxBodyL @era .~ ihash
    & auxDataHashTxBodyL @era .~ aux
    & networkIdTxBodyL @era .~ nw
